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
import           Wallet.Effects (ownPaymentPubKeyHash)
import qualified PlutusTx.Builtins      as Builtins
import           Plutus.V1.Ledger.Ada as Ada

data ContractInfo = ContractInfo
    { policySpaceBudz :: !CurrencySymbol
    , policyBid :: !CurrencySymbol
    , prefixSpaceBud :: !BuiltinByteString
    , prefixSpaceBudBid :: !BuiltinByteString
    , owner1 :: !(PubKeyHash, Integer, Integer)
    , owner2 :: !(PubKeyHash, Integer)
    , extraRecipient :: !Integer
    , minPrice :: !Integer
    , bidStep :: !Integer
    } deriving (Generic, ToJSON, FromJSON)


contractInfo = ContractInfo 
    { policySpaceBudz = "d5e6bf0500378d4f0da4e8dde6becec7621cd8cbf5cbb9b87013d4cc"
    , policyBid = "800df05a0cc6b6f0d28aaa1812135bd9eebfbf5e8e80fd47da9989eb"
    , prefixSpaceBud = "SpaceBud"
    , prefixSpaceBudBid = "SpaceBudBid"
    , owner1 = ("826d9fafe1b3acf15bd250de69c04e3fc92c4493785939e069932e89", 416, 625) -- 2.4% 1.6%
    , owner2 = ("88269f8b051a739300fe743a7b315026f4614ce1216a4bb45d7fd0f5", 2500) -- 0.4%
    , extraRecipient = 2500 -- 0.4%
    , minPrice = 70000000
    , bidStep = 10000
    }



minADA :: Value
minADA = Ada.lovelaceValueOf 2000000

price :: Value 
price = Ada.lovelaceValueOf 5000000

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

curSymbol :: PubKeyHash -> CurrencySymbol
curSymbol = scriptCurrencySymbol . policy

data MintParams = MintParams
    { mpTokenName :: !TokenName
    , mpAmount    :: !Integer
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

PlutusTx.makeLift ''MintParams

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
     .\/ Endpoint "purchase" MintParams

mint :: MintParams -> Contract w SignedSchema Text ()
mint mp = do
    let pkh = ownPubKey
        val     = Value.singleton (curSymbol (pkh)) (mpTokenName mp) (mpAmount mp)
        lookups = Constraints.mintingPolicy $ policy (pkh)
        tx      = Constraints.mustMintValue val
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    Contract.logInfo @String $ printf "forged %s" (show val)

lock :: AsContractError e => MintParams -> Contract w s e ()
lock mp =  do
    let pkh = ownPubKey
        tx         = Constraints.mustPayToTheScript () $ (Value.singleton (curSymbol (pkh)) (mpTokenName mp) (mpAmount mp)) <> minADA
    void (submitTxConstraints lootBox tx)

purchase :: AsContractError e => MintParams -> Contract w s e ()
purchase mp =  do
    utxos <- fundsAtAddressGeq valAddress (Ada.lovelaceValueOf 1)

    let redeemer = ()
        pkh = ownPubKey
        tx       = Constraints.mustPayToPubKey (owner1 contractInfo) price <> collectFromScript utxos redeemer

    void (submitTxConstraintsSpending lootBox utxos tx)

endpoints :: Contract () SignedSchema Text ()
endpoints = awaitPromise (mint' `select` lock' `select` purchase') >> endpoints
  where
    mint' = endpoint @"mint" mint
    lock' = endpoint @"lock" lock
    purchase' = endpoint @"purchase" purchase

mkSchemaDefinitions ''SignedSchema

mkKnownCurrencies []
