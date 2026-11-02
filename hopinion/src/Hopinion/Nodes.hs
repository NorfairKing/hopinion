{-# LANGUAGE ScopedTypeVariables #-}

-- | Every node of a parse tree that some rule looks for, found in one walk.
--
-- A walk over a GHC parse tree visits every location and every annotation in
-- it, so it costs about as much as parsing did.
-- A rule that walked the tree for itself would add that much per rule, and a
-- run with many rules would spend nearly all of its time walking.
-- Collecting every wanted type in one walk makes a rule's search cost only
-- what it does with the nodes it is given.
module Hopinion.Nodes
  ( Nodes,
    nodesIn,
    nodesOf,
  )
where

import Data.Data (Data, gmapQ)
import Data.Dynamic (Dynamic, fromDynamic, toDyn)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import Data.Proxy (Proxy (..))
import qualified Data.Set as S
import Type.Reflection (SomeTypeRep, Typeable, someTypeRep)

-- | The nodes of each wanted type, each list in the order a reader meets them.
newtype Nodes = Nodes (M.Map SomeTypeRep [Dynamic])

-- | Every node of each of these types, anywhere under the root, the root
-- included.
--
-- A node inside another node of a wanted type is found as well, which is what
-- a search for every expression has to mean.
nodesIn :: (Data root) => S.Set SomeTypeRep -> root -> Nodes
nodesIn wanted root =
  let visit :: forall d. (Data d) => M.Map SomeTypeRep [Dynamic] -> d -> M.Map SomeTypeRep [Dynamic]
      visit found x =
        let here = someTypeRep (Proxy :: Proxy d)
            withHere =
              if S.member here wanted
                then M.insertWith (++) here [toDyn x] found
                else found
         in foldl' (\acc step -> step acc) withHere (gmapQ (flip visit) x)
   in -- Each list was built by consing, so it is backwards until here.
      Nodes (M.map reverse (visit M.empty root))

-- | The nodes of one type, which is nothing for a type that was not wanted.
nodesOf :: forall a. (Typeable a) => Nodes -> [a]
nodesOf (Nodes found) =
  mapMaybe fromDynamic (M.findWithDefault [] (someTypeRep (Proxy :: Proxy a)) found)
