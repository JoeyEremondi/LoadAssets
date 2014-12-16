module LoadAssets where

import List

{-| A library providing some utilites for loading a large number of resources
of different types from a remote origin using HTTP,
and querying how many of them are loaded.

# Single assets
@docs Asset, toAsset 

# Asset groups loaded together
@docs Status, toStatus

# Unsafely extracting from Http.request
@docs fromResponseOrFail

-}

import Http

{-|
Generic type for any asset which is loaded remotely.
-}
type Asset = 
  AssetLoading
  | AssetLoaded
  | AssetFailed (Int, String)
  
{-|
Convert a response of any type to an `Asset`
-}
toAsset : Http.Response a -> Asset
toAsset resp = case resp of
  Http.Success _ -> AssetLoaded
  Http.Failure i s -> AssetFailed (i,s)
  Http.Waiting -> AssetLoading

{-|
Structure holding the load status of a number of assets
-}  
type Status = 
  InProgress Float
  | Complete
  | Failed (List (Int, String))
  

addFailString el listSoFar = case el of
  AssetFailed intStr -> listSoFar ++ [intStr]
  _ -> listSoFar  

accumLoading el numSoFar = case el of
  AssetLoading -> numSoFar + 1
  _ -> numSoFar

failStrings : List Asset -> List (Int, String)
failStrings elList = List.foldr addFailString [] elList


numLoading elList = List.foldr accumLoading 0 elList

{-|
Given a number of assets, generate their load status us a group.
Useful for progress bars and loading screens.
-}
toStatus : (List Asset) -> Status
toStatus els = let
    numEls = List.length els
    fails = failStrings els
    num = numLoading els
  in if
    | not <| List.isEmpty fails -> Failed fails
    | num > 0 -> InProgress <| (100.0 * (toFloat num)) / (toFloat numEls)    
    | otherwise -> Complete

{-|
Get a value from an HTTP request.
This is only safe to call if the asset being retrieved
is in a load group that has evaluated to `Success`
-}
fromResponseOrFail : Http.Response a -> a
fromResponseOrFail r = case r of 
  Http.Success s -> s