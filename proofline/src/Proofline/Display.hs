module Display (displayError, displayMetas, displayMeta) where

import Control.Exception()
import Data.IORef()
import System.Exit()
import qualified Data.IntMap as IM()

import Common
import Context
import Errors
import Metacontext
import qualified Pretty
import Syntax
import Value
import Evaluation

-- Display a position in the source code
showPos :: SourcePos -> String
showPos (SourcePos _ line col) =
   "line " ++ show (unPos line) ++ ", column " ++ show (unPos col)

-- Display an elaboration error
displayError :: String -> Error -> IO ()
displayError _ (Error cxt err) = do 
  let ppos = pos cxt
  putStrLn $ "Error at " ++ showPos ppos ++ ":"
  case err of
    NameNotInScope x ->
       putStrLn $ "Name not in scope: " ++ x
    CantUnify t t' -> do
      putStrLn "Cannot unify:"
      putStrLn $ "  " ++ showTm cxt t
      putStrLn "with:"
      putStrLn $ "  " ++ showTm cxt t'
    UnboundVar x ->
      putStrLn $ "Unbound variable: " ++ x

-- Display all unsolved metavariables with their contexts
displayMetas :: IO ()
displayMetas = do
  ms <- getMetas
  if null ms
    then putStrLn "No unsolved metavariables."
    else do
      putStrLn "Metavariables:"
      mapM_ displayMeta ms

displayMeta :: (Int, MetaEntry) -> IO ()
displayMeta (k, entry) = case entry of
  Solved val ->
     putStrLn $ "  ?" ++ show k ++ " : " ++ showVal0 val ++ " (solved)"
  Unsolved ctx expectedTypeVal -> do  
    let names = map fst ctx  
        -- Calculate the level based on context size
        level = Lvl (length ctx)
        -- Quote the expected type at the correct level
        quotedType = quote level expectedTypeVal 
        -- Pretty print with the context names
        typeStr = Pretty.prettyTm 0 names quotedType []
         
    putStrLn $ "  ?" ++ show k ++ " : " ++ "_" ++ " (unsolved)" ++ "\n    Type:" ++ typeStr
    if null ctx
      then putStrLn "    Context: ∅"
      else do
         putStrLn "    Context:"
         displayWithNames (reverse ctx)
         
displayWithNames :: [(Name, VTy)] -> IO ()
displayWithNames ctx = go ctx []
  where
    go [] _ = return ()
    go ((name, val):rest) names = do
      -- For each entry, use the names collected so far for display
      let depth = Lvl (length names)
          quotedVal = quote depth val
          -- Use names in the right order for pretty printing
          prettyVal = Pretty.prettyTm 0 names quotedVal []
      putStrLn $ "      " ++ name ++ " : " ++ prettyVal
      -- Add current name to the collected names for next entries
      go rest (name:names)