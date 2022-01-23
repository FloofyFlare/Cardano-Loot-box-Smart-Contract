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
import           Data.Text
import           Wallet.Emulator.Wallet 
import           PlutusTx.Prelude
import           Wallet.Effects (ownPaymentPubKeyHash)
import           Plutus.V1.Ledger.Ada as Ada

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








{-# INLINABLE mkPolicy #-}
mkPolicy :: PubKeyHash -> () -> ScriptContext -> Bool
mkPolicy pkh () ctx@ScriptContext{scriptContextTxInfo=txInfo} = 
    when (price /= (valueSpent txInfo)) $
        False

    txSignedBy (scriptContextTxInfo ctx) pkh

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

type SignedSchema = Endpoint "mint" MintParams

--there are two error signals so be careful to make sure they dont get triped
ownPubKey :: PubKeyHash
ownPubKey = case (toPubKeyHash $ ownAddress ownWallet) of
    Nothing     ->  error ()
    Just x      ->  x

ownWallet :: WalletState
ownWallet = case (emptyWalletState $ knownWallet 1) of 
    Nothing     ->  error () 
    Just x      ->  x

mint :: MintParams -> Contract w SignedSchema Text ()
mint mp = do
    let pkh = ownPubKey
        val     = Value.singleton (curSymbol (pkh)) (mpTokenName mp) (mpAmount mp)
        lookups = Constraints.mintingPolicy $ policy (pkh)
        tx1     = Constraints.mustMintValue val 
        tx2     = Constraints.mustPayToTheScript () price
    ledgerTx <- submitTxConstraintsWith @Void lookups tx1
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    void (submitTxConstraints lootBox tx2)
    Contract.logInfo @String $ printf "forged %s" (show val)

endpoints :: Contract () SignedSchema Text ()
endpoints = awaitPromise mint' >> endpoints
  where
    mint' = endpoint @"mint" mint

mkSchemaDefinitions ''SignedSchema

mkKnownCurrencies []
