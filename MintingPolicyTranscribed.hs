import Ledger 
import Data.Aeson (ToJSON, FromJSON)
import Ledger.Constraints qualified as Constraints
import Ledger.Typed.Scripts qualified as Scripts
import Ledger.Value (Value, CurrencySymbol, singleton, valueOf)
import Plutus.V2.Ledger.Api (adaSymbol, adaToken, txOutValue, PubKeyHash, TxOut, txInfoOutputs, singleton, mkMintingPolicyScript)
import Ledger.Contexts (valueSpent, ScriptContext(..), TxInfo(..), scriptCurrencySymbol)
import Plutus.Contract
import PlutusTx qualified
import Plutus.Contract.Test
import PlutusTx.Prelude hiding (Applicative (..))
import Control.Monad        hiding (fmap)
import Data.List.NonEmpty   (NonEmpty (..))
import Data.Map             as Map
import Data.Text            (pack, Text)
import GHC.Generics         (Generic)
import Ledger.Value         as Value
import Ledger.Ada           as Ada
import Playground.Contract  (IO, ensureKnownCurrencies, printSchemas, stage, printJson)
import Playground.TH        (mkKnownCurrencies, mkSchemaDefinitions)
import Playground.Types     (KnownCurrency (..))
import qualified PlutusTx
import PlutusTx.Prelude     hiding (unless)
import qualified Prelude              as P
import Schema               (ToSchema)
import Text.Printf          (printf)

--mininmum price of Script compilation
minLovelace :: Integer
minLovelace = 2000000

nameOfToken :: TokenName
nameOfToken = TokenName "car1"

--seller Parameters
data Seller = Seller 
    { sSeller   :: !PaymentPubKeyHash
    , sPrice    :: !Integer
    , sCurrency :: !CurrencySymbol
    , sToken    :: !TokenName
    } deriving (P.Show, Generic, ToJSON, FromJSON, ToSchema)

instance Eq Seller where
    {-# INLINABLE (==) #-}
    a == b = (sSeller   a == sSeller   b) &&
             (sPrice    a == sPrice    b) &&
             (sCurrency a == sCurrency b) &&
             (sToken    a == sToken    b)

PlutusTx.unstableMakeIsData ''Seller
PlutusTx.makeLift ''Seller

--Customer Parameters
data Payment = Payment
    { cCustomer :: !PaymentPubKeyHash
    } deriving P.Show

instance Eq Payment where
    {-# INLINABLE (==) #-}
    a == b = (cCustomer   a == cCustomer   b) 

PlutusTx.unstableMakeIsData ''Payment
PlutusTx.makeLift ''Payment

{-# INLINABLE mkValidator #-}
mkValidator :: StoreFrontDatum -> Payment -> ScriptContext -> Bool
mkValidator sd redeemmer ctx = True
    
storeHash :: Ledger.ValidatorHash
storeHash = Scripts.validatorHash storeContract

storeValidator :: Validator
storeValidator = Scripts.validatorScript storeContract

data PurchaseVar
instance Scripts.ValidatorTypes PurchaseVar where
    type instance RedeemerType PurchaseVar = Payment
    type instance DatumType PurchaseVar = StoreFrontDatum

--hash of the validator script (address of the contract)
contractAddress :: Address
contractAddress = Scripts.validatorAddress storeContract

-- | The script instance is the compiled validator (ready to go onto the chain)
storeContract :: Scripts.TypedValidator PurchaseVar
storeContract = Scripts.mkTypedValidator @PurchaseVar
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||]) where
        wrap = Scripts.wrapValidator @StoreFrontDatum @Payment

-- cheack  the amount of ADA sent by the wallet to makes sure its acceptable
-- 30000000 is equal to 30 ADA
{-# INLINABLE checkForCorrectAmount #-}
checkForCorrectAmount :: TxInfo -> Value -> Bool
checkForCorrectAmount info price = ((valueSpent info) == price)

data StoreFrontDatum = StoreFrontDatum
    { sdStore    :: !Seller
    } deriving P.Show

PlutusTx.unstableMakeIsData ''StoreFrontDatum
PlutusTx.makeLift ''StoreFrontDatum

data SellerParams = SellerParams
    { apPrice    :: !Integer
    , apCurrency :: !CurrencySymbol
    , apToken    :: !TokenName
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

data MintParams = MintParams
    { mpCurrency :: !CurrencySymbol
    , mpToken    :: !TokenName
    , mpPrice      :: !Integer
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

type StoreSchema =
        Endpoint "storeFront" MintParams
    .\/ Endpoint "start" SellerParams

start :: AsContractError e => SellerParams -> Contract w s e ()
start SellerParams{..} = do
    pkh <- ownPaymentPubKeyHash
    let a = Seller
                { sSeller   = pkh
                , sPrice    = apPrice
                , sCurrency = apCurrency
                , sToken    = apToken
                }
        d = StoreFrontDatum
                { sdStore    = a
                }
        v = Ada.lovelaceValueOf minLovelace
        tx = Constraints.mustPayToTheScript d v
    ledgerTx <- submitTxConstraints storeContract tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @P.String $ printf "started storeFront %s for token %s" (P.show a) (P.show v)


-- creates a store front that a user can access using its specified store Currency symbol
storeFront :: forall w s. MintParams -> Contract w s Text ()
storeFront MintParams{..} = do
    (oref, o, d@StoreFrontDatum{..}) <- findStore mpCurrency mpToken
    logInfo @P.String $ printf "found auction utxo with datum %s" (P.show d)

    let r      = Redeemer $ PlutusTx.toBuiltinData cCustomer
        seller = sSeller sdStore
        lookups = Constraints.typedValidatorLookups storeContract P.<>
                  Constraints.otherScript storeValidator                P.<>
                  Constraints.unspentOutputs (Map.singleton oref o)

        tx    = Constraints.mustMintCurrency mintingHash mpToken 1 <> 
                Constraints.mustPayToPubKey seller (Ada.lovelaceValueOf mpPrice) <>
                Constraints.mustSpendScriptOutput oref r
    ledgerTx <- submitTxConstraintsWith lookups tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @P.String $ printf "closed sale %s for token (%s, %s)"
        (P.show sdStore)
        (P.show mpCurrency)
        (P.show mpToken)

-- you could use the mintingParams as the redeemer turst me :)
-- Allows for the minting of 1 at a time
{-# INLINABLE oneAtATimePolicy #-}
oneAtATimePolicy :: () -> ScriptContext -> Bool
oneAtATimePolicy _ ctx =
    -- 'ownCurrencySymbol' lets us get our own hash (= currency symbol)
    -- from the context
    let ownSymbol = ownCurrencySymbol ctx
        txinfo = scriptContextTxInfo ctx
        minted = txInfoMint txinfo
    -- Here we're looking at some specific token name, which we
    -- will assume we've got from elsewher e for now.
    in valueOf minted ownSymbol nameOfToken == 1

    
-- We can use 'compile' to turn a minting policy into a compiled Plutus Core program,
-- just as for validator scripts. We also provide a 'wrapMintingPolicy' function
-- to handle the boilerplate.

oneAtATimeCompiled :: Scripts.MintingPolicy
oneAtATimeCompiled = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy oneAtATimePolicy ||])

mintingHash :: MintingPolicyHash 
mintingHash = mintingPolicyHash oneAtATimeCompiled

--allows the script to find the store front referenced by a consumer        
findStore :: CurrencySymbol
            -> TokenName
            -> Contract w s Text (TxOutRef, ChainIndexTxOut, StoreFrontDatum)
findStore cs tn = do
    utxos <- utxosAt $ scriptHashAddress storeHash
    let xs = [ (oref, o)
             | (oref, o) <- Map.toList utxos
             , Value.valueOf (_ciTxOutValue o) cs tn == 1
             ]
    case xs of
        [(oref, o)] -> case _ciTxOutDatum o of
            Left _          -> throwError "datum missing"
            Right (Datum e) -> case PlutusTx.fromBuiltinData e of
                Nothing -> throwError "datum has wrong type"
                Just d@StoreFrontDatum{..}
                    | sCurrency sdStore == cs && sToken sdStore == tn -> return (oref, o, d)
                    | otherwise                                           -> throwError "store token missmatch"
        _           -> throwError "store utxo not found"

endpoints :: Contract () StoreSchema Text ()
endpoints = awaitPromise (start' `select` storeFront' `select` ) >> endpoints
  where
    start'        = endpoint @"start" start
    storeFront'   = endpoint @"storeFront" storeFront

mkSchemaDefinitions ''StoreSchema

mkKnownCurrencies []
