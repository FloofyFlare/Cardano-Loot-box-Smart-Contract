import Control.Monad (void)
import Ledger (Address, ScriptContext)
import Ledger.Constraints qualified as Constraints
import Ledger.Typed.Scripts qualified as Scripts
import Ledger.Value (Value, CurrencySymbol)
import Playground.Contract
import Plutus.Contract
import PlutusTx qualified
import PlutusTx.Prelude hiding (Applicative (..))

--Splits the TxOutValue into its perspective parts so that its useable
data PaymentParams = PaymentParams 
    { pCurrencySymbol  :: CurrencySymbol
      ,pTokenName      :: TokenName
      ,pAmount         :: Integer
    } deriving (Generic, ToJSON, FromJSON, ToSchema)


-- cheack  the amount of ADA sent by the wallet to makes sure its acceptable
mkCheckForCorrectAmount :: Integer -> Bool
mkCheckForCorrectAmount amount = (amount == 30)
