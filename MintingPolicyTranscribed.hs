import           Control.Monad          hiding (fmap)
import           Data.Aeson             (ToJSON, FromJSON)
import           Data.Text              (Text)
import           Data.Void              (Void)
import           GHC.Generics           (Generic)
import           Plutus.Contract        as Contract
import           Plutus.Trace.Emulator  as Emulator
import qualified PlutusTx
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Ledger                 hiding (mint, singleton)
import           Ledger.Constraints     as Constraints
import qualified Ledger.Typed.Scripts   as Scripts
import           Ledger.Value           as Value
import           Playground.Contract    (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
import           Playground.TH          (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types       (KnownCurrency (..))
import           Prelude                (IO, Show (..), String)
import           Text.Printf            (printf)
import           Wallet.Emulator.Wallet
import           PlutusTx.Prelude
import           Wallet.Effects         as Effects
import qualified PlutusTx.Builtins      as Builtins
import           Plutus.V1.Ledger.Ada as Ada
import           Ledger.Tx              (scriptTxOut, ChainIndexTxOut)
import           Data.Map             as Map
import           Plutus.ChainIndex.Tx 
import           Ledger.Blockchain 


minADA :: Value
minADA = Ada.lovelaceValueOf 2000000

price :: Value 
price = Ada.lovelaceValueOf 5000000

data ContractInfo = ContractInfo
    { policyID :: !CurrencySymbol
    , walletOwner :: !PubKeyHash
    , nameOfToken :: !TokenName
    } deriving (Generic, ToJSON, FromJSON)


contractInfo = ContractInfo 
    { policyID = "8b0dbdd6504ff8f129400ab3b21f12a52ffb09f0b3cff594cb5bb868"
    , walletOwner = "a2c20c77887ace1cd986193e4e75babd8993cfd56995cd5cfce609c2"
    , nameOfToken = "reward"
    }

--purchase portion of the contract 
data LootBoxData
instance Scripts.ValidatorTypes LootBoxData where
    type instance RedeemerType LootBoxData = ()
    type instance DatumType LootBoxData = ()

--Checks the validatorHash to make sure the LootBox has tokens to host the transaction
{-# INLINABLE mkValidator #-}
mkValidator :: () -> () -> ScriptContext -> Bool
mkValidator _ _ ctx = True

lootBox :: Scripts.TypedValidator LootBoxData
lootBox = Scripts.mkTypedValidator @LootBoxData
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||]) where
        wrap = Scripts.wrapValidator

valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash lootBox

validate :: Validator
validate = Scripts.validatorScript lootBox

valAddress :: Address
valAddress = Scripts.validatorAddress lootBox



--Start of minting portion of the smart contract

{-# INLINABLE mkPolicy #-}
mkPolicy :: PubKeyHash -> () -> ScriptContext -> Bool
mkPolicy pkh () ctx = txSignedBy (scriptContextTxInfo ctx) pkh

policy :: PubKeyHash -> Scripts.MintingPolicy
policy pkh = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy . mkPolicy||])
    `PlutusTx.applyCode`
    (PlutusTx.liftCode pkh)

--dont need
curSymbol :: PubKeyHash -> CurrencySymbol
curSymbol = scriptCurrencySymbol . policy

data MintParams = MintParams
    { mpTokenName :: !TokenName
    , mpAmount    :: !Integer
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

PlutusTx.makeLift ''MintParams

data PurchaseParams = PurchaseParams
    { pTokenName :: !TokenName
    , pPayment   :: !PaymentPubKeyHash
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

PlutusTx.makeLift ''PurchaseParams

--there are two error signals triped when wallet 1 isent the owner
ownPubKey :: PubKeyHash
ownPubKey = case (toPubKeyHash $ ownAddress ownWallet) of
    Nothing     ->  error ()
    Just x      ->  x

ownWallet :: WalletState
ownWallet = case (emptyWalletState $ knownWallet 1) of 
    Nothing     ->  error () 
    Just x      ->  x


--Start of endpoints
type SignedSchema = 
    Endpoint "mint" MintParams
     .\/ Endpoint "lock" MintParams
     .\/ Endpoint "purchase" ()

mint :: MintParams -> Contract w SignedSchema Text ()
mint mp = do
    let pkh = (walletOwner contractInfo)
        val     = Value.singleton (curSymbol (pkh)) (mpTokenName mp) (mpAmount mp)
        lookups = Constraints.mintingPolicy $ policy (pkh)
        tx      = Constraints.mustMintValue val
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    Contract.logInfo @String $ printf "forged %s" (show val)

lock :: AsContractError e => MintParams -> Contract w s e ()
lock mp =  do
    let tx         = Constraints.mustPayToTheScript () $ (Value.singleton (policyID contractInfo) (nameOfToken contractInfo) (mpAmount mp)) <> minADA
    void (submitTxConstraints lootBox tx)

purchaseUtxo :: ChainIndexTxOut
purchaseUtxo = case fromTxOut (scriptTxOut (Value.singleton (policyID contractInfo) (nameOfToken contractInfo) (1)) validate (Datum $ PlutusTx.toBuiltinData ()) )  of
    Nothing     ->  error ()
    Just x      ->  x

purchase ::  AsContractError e => () -> Contract w s e ()
purchase _ =  do
    utxos <- fundsAtAddressGeq valAddress (Ada.lovelaceValueOf 1)
    let redeemer = () 
        pkh = ownPubKey
        tx       = Constraints.mustPayToPubKey (paymentPubKeyHash $ ownPaymentPublicKey ownWallet) price <> collectFromScript utxos redeemer
        
    void (submitTxConstraintsSpending lootBox utxos tx)

endpoints :: Contract () SignedSchema Text ()
endpoints = awaitPromise (mint' `select` lock' `select` purchase') >> endpoints
  where
    mint' = endpoint @"mint" mint
    lock' = endpoint @"lock" lock
    purchase' = endpoint @"purchase" purchase

mkSchemaDefinitions ''SignedSchema

mkKnownCurrencies []
