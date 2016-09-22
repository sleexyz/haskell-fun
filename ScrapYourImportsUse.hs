{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns  #-}

-- | Scrap your imports!
-- We use record datatypes as modules,
-- and abuse RecordWildCards/NamedFieldPuns for scope pollution.
--
-- Benefits:
--  - generative imports; i.e. export readers of modules
--    and discharge the reader at the import site
--
--  - first class modules; you can treat modules as terms!
--    modules can be packed into existentials
--
--  - local imports; you can import at the top-level or
--    at local scopes
--
--
-- The ability to do top-level imports this way
-- lets you keep Haskell's signature concision.

module ScrapYourImportsUse where

import qualified ScrapYourImports as M

-- `greet` enters the scope here!
M.Exports{greet} = M.makeExports "hello"

main :: IO ()
main = greet
