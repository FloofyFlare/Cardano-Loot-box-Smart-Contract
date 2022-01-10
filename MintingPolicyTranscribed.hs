import Control.Monad (void)
import Ledger (Address, ownCurrencySymbol, PaymentPubKeyHash)
import Ledger.Constraints qualified as Constraints
import Ledger.Typed.Scripts qualified as Scripts
import Ledger.Value (Value, CurrencySymbol, singleton, valueOf)
import Plutus.V2.Ledger.Api (adaSymbol, adaToken, txOutValue, PubKeyHash, TxOut, txInfoOutputs)
import Ledger.Contexts (valueSpent, ScriptContext(..), TxInfo(..))
import Playground.Contract
import Plutus.Contract
import PlutusTx (CompiledCode)
import PlutusTx qualified
import PlutusTx.Prelude hiding (Applicative (..))


data ContractInfo = ContractInfo
    { recvPkh :: PaymentPubKeyHash
    } 
    deriving (Generic, ToJSON, FromJSON, ToSchema)

contractInfo = ContractInfo
    { recvPkh = "3f7846896a48c59359746ff096d63606ceb82e65900d20a9fd2b8a93"
    }

-- makes sure that all outputs are being sent to the above address
{-# INLINABLE mkValidator #-}
mkValidator :: () -> () -> ScriptContext -> Bool
mkValidator _ _ ctx@ScriptContext{scriptContextTxInfo=txInfo} = 
    let 
        info = scriptContextTxInfo
    in checkForCorrectAmount (info ctx)
   
data PurchaseVar
instance Scripts.ValidatorTypes PurchaseVar where
    type instance RedeemerType PurchaseVar = ()
    type instance DatumType PurchaseVar = ()

--hash of the validator script (address of the contract)
contractAddress :: Address
contractAddress = Scripts.validatorAddress storeContract

-- | The script instance is the compiled validator (ready to go onto the chain)
storeContract :: Scripts.TypedValidator PurchaseVar
storeContract = Scripts.mkTypedValidator @PurchaseVar
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||]) where
        wrap = Scripts.wrapValidator

-- cheack  the amount of ADA sent by the wallet to makes sure its acceptable
-- 30000000 is equal to 30 ADA
{-# INLINABLE checkForCorrectAmount #-}
checkForCorrectAmount :: TxInfo -> Bool
checkForCorrectAmount info = ((valueSpent info) > (singleton adaSymbol adaToken 30000000))

-- Minting policy gained from https://playground.plutus.iohkdev.io/doc/plutus/tutorials/basic-minting-policies.html
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

oneAtATimeCompiled :: CompiledCode (BuiltinData -> BuiltinData -> ())
oneAtATimeCompiled = $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy oneAtATimePolicy ||])

nameOfToken :: TokenName
nameOfToken = TokenName "car1"

type Schema =
        Endpoint "storeFront" PaymentPubKeyHash

payPubHash :: PaymentPubKeyHash
payPubHash = "3f7846896a48c59359746ff096d63606ceb82e65900d20a9fd2b8a93"

storeFront :: AsContractError e => Promise () Schema e ()
storeFront = endpoint @"storeFront" $ \(payPubHash) -> do
    unspentOutputs <-  utxosAt contractAddress
    
    let 
        tx       = Constraints.mustPayToPubKey payPubHash (singleton adaSymbol adaToken 30000000) 
    void $ submitTxConstraintsSpending storeContract unspentOutputs tx
        
contract :: AsContractError e => Contract () Schema e ()
contract = storeFront

endpoints :: AsContractError e => Contract () Schema e ()
endpoints = contract

mkSchemaDefinitions ''Schema

$(mkKnownCurrencies [])
