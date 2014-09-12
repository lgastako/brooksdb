module IO.Brooks.Csv ( relFromCsv ) where

import Debug.Trace ( trace )

import System.IO.Unsafe ( unsafePerformIO )

import Data.Relation.Types ( Relation
                           , Tuple
                           , Heading
                           , withHeading
                           , insertTuple
                           , fromList
                           , headWith
                           )

import Text.CSV ( parseCSVFromFile )

type Row = [String]


headingFromRow :: Row -> Heading
headingFromRow row = fromList augmented
  where f x = (x, "String")
        augmented = map f row


tupleFromRow :: Heading -> Row -> Row -> Tuple
tupleFromRow heading hrow row =
  let heading = trace ("heading: " ++ (show heading)) heading
      hrow = trace ("hrow: " ++ (show hrow)) hrow
      row = trace ("row: " ++ (show row)) row
  in headWith heading $ zip hrow row


relFromRows :: [Row] -> Relation
relFromRows rows =
  let hrow       = head rows
      rows'      = tail rows
      heading    = headingFromRow hrow
      relation   = withHeading heading
      tuples     = map (tupleFromRow heading hrow) rows'
  in foldl insertTuple relation tuples


relFromCsv :: FilePath -> IO (Maybe Relation)
relFromCsv fn = do
  res <- parseCSVFromFile fn
  putStrLn $ "res from csv: " ++ (show res)
  -- for some reason res ends up with an list with a single empty string at the end
  -- so we take the init of the rows

  let mr = case res of
             (Left _)     -> Nothing
             (Right rows) -> Just (relFromRows (init rows))

  putStrLn $ "mr: " ++ (show mr)
  return mr
  --return Nothing
