import Control.Monad (void)
import Ledger (Address, ScriptContext)
import Ledger.Constraints qualified as Constraints
import Ledger.Typed.Scripts qualified as Scripts
import Ledger.Value (Value)
import Ledger.ScriptContext (TxInfo)
import Playground.Contract
import Plutus.Contract
import PlutusTx qualified
import PlutusTx.Prelude hiding (Applicative (..))

--checks the Script Context to see if the ADA inputed to the transaction is equal to 30 ADA
{-# INLINABLE mkValidatorForCorrectAmount #-}
mkValidatorForCorrectAmount :: ()
mkValidatorForCorrectAmount _ = _ == Ada.lovelaceValueOf 30

