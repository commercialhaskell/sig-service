((hamlet-mode
  . ((hamlet/basic-offset . 4)
     (haskell-process-use-ghci . t)))
 (haskell-mode
  . ((haskell-indent-spaces . 4)
     (hindent-style . "johan-tibell")
     (haskell-process-type . ghci)
     (haskell-process-path-ghci . "stack")
     (haskell-process-args-ghci . ("ghci"))))
 (haskell-cabal-mode
  . ((haskell-process-type . ghci)
     (haskell-process-path-ghci . "stack")
     (haskell-process-args-ghci . ("ghci")))))
