import           Control.Monad          hiding (fmap)
import           Data.Aeson             (ToJSON, FromJSON)
import           Data.Text              (Text)
import           Data.Void              (Void)
import           GHC.Generics           (Generic)
import           Data.Map              as Map hiding (foldl)
import           Plutus.Contract        
import           Plutus.Contract        as Contract
import           Ledger.Address         as Add
import           Plutus.Trace.Emulator  as Emulator
import qualified PlutusTx
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
import           Wallet.Effects         as Effects
import qualified PlutusTx.Builtins      as Builtins
import           Plutus.V1.Ledger.Ada as Ada
import           Ledger.Tx              (scriptTxOut, ChainIndexTxOut)
import           Plutus.ChainIndex.Tx 
import           Ledger.Blockchain 
import           Playground.Contract
import           PlutusTx.Prelude     hiding (Semigroup(..), unless)
import           Prelude              (IO, Semigroup (..), String, Show(..))
import qualified Prelude              as Haskell
import           Cardano.Api hiding (Value, TxOut,Address)
import           Cardano.Api.Shelley hiding (Value, TxOut, Address)
import           Codec.Serialise hiding (encode)
import           Plutus.ChainIndex as Chain




minADA :: Value
minADA = Ada.lovelaceValueOf 2000000

price :: Value 
price = Ada.lovelaceValueOf 12000000

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
    type instance DatumType LootBoxData = Integer

--Checks the validatorHash to make sure the LootBox has tokens to host the transaction
--If never changed by the end of the project use the builtins script validator
{-# INLINABLE mkValidator #-}
mkValidator :: Integer -> () -> ScriptContext -> Bool
mkValidator i x ctx = True

lootBox :: Scripts.TypedValidator LootBoxData
lootBox = Scripts.mkTypedValidator @LootBoxData
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||]) where
        wrap = Scripts.wrapValidator @Integer @()

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

data AmountParams = AmountParams
    { apAmount    :: !Integer
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

PlutusTx.makeLift ''AmountParams

data LockParams = LockParams
    { apAmount    :: !Integer
    , apTokensPerOutput   :: !Integer
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

PlutusTx.makeLift ''AmountParams

--Start of endpoints
type SignedSchema = 
    Endpoint "mint" AmountParams
     .\/ Endpoint "lock" LockParams
     .\/ Endpoint "purchase" ()

mint :: AsContractError e => AmountParams -> Contract w s e ()
mint ap = do
    ppkh <- Contract.ownPaymentPubKeyHash
    let pkh     = unPaymentPubKeyHash ppkh
        val     = Value.singleton (curSymbol pkh) (nameOfToken contractInfo) (apAmount ap)
        lookups = Constraints.mintingPolicy $ policy (pkh)
        tx      = Constraints.mustMintValue val
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ printf "forged %s" (show val)

lock :: LockParams -> Contract w SignedSchema Text ()
lock ap =  do
-- make a recusive function that allows for the creation of a 1000 utxos of diffrent datums from a wallet (cheap if implemented correctly) 
        
    let v   = Value.singleton (policyID contractInfo) (nameOfToken contractInfo) (apTokensPerOutput ap) <> minADA
        tx  =   foldl
                (\acc i -> acc <> (Constraints.mustPayToTheScript (i) $ v))
                (TxConstraints [] [] [])
                [(apAmount ap * 1000)..((apAmount ap +1) * 1000)]
                
    void (submitTxConstraints lootBox tx)

purchase :: () -> Contract w SignedSchema Text ()
purchase _ =  do
    utxos <- utxosAt valAddress
    
    let outputs = Map.toList utxos
        r      = Redeemer $ PlutusTx.toBuiltinData ()
        (oref, o) = head outputs
        ppkh     = walletOwner contractInfo
        lookups = Constraints.unspentOutputs utxos <>
                  Constraints.otherScript validate <>
                  Constraints.typedValidatorLookups lootBox
        tx      = mustPayToPubKey (Add.PaymentPubKeyHash ppkh) price <> mconcat [Constraints.mustSpendScriptOutput oref unitRedeemer ] 
    ledgerTx <- submitTxConstraintsWith @LootBoxData lookups tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ "loot box used"

-- Sort through the Utxos so that it only collect the right one

endpoints :: Contract () SignedSchema Text ()
endpoints = awaitPromise (mint' `select` lock' `select` purchase') >> endpoints
  where
    mint' = endpoint @"mint" mint
    lock' = endpoint @"lock" lock
    purchase' = endpoint @"purchase" purchase

mkSchemaDefinitions ''SignedSchema

mkKnownCurrencies []
