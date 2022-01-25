mustPayToPubKey

purchase :: forall w s e. AsContractError e => MintParams -> Contract w s e ()
purchase mp = do
    utxos <- fundsAtAddressGeq valAddress (Ada.lovelaceValueOf 1)
    let orefs   = fst <$> Map.toList utxos
        lookups = Constraints.otherScript validate
        tx :: TxConstraints Void Void
        tx      = mconcat [mustSpendScriptOutput oref $ Redeemer $ PlutusTx.toBuiltinData () | oref <- orefs]
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
