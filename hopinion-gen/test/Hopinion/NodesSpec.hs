{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Hopinion.NodesSpec (spec) where

import Data.Generics (listify)
import Data.Proxy (Proxy (..))
import qualified Data.Set as S
import qualified Data.Text as T
import GHC.Hs
import Hopinion.Nodes
import Hopinion.Parse
import Path (relfile)
import Test.Syd
import Type.Reflection (someTypeRep)

spec :: Spec
spec = do
  describe "nodesIn and nodesOf" $ do
    let source :: T.Text
        source =
          T.unlines
            [ "module Fixture where",
              "",
              "data Thing = Thing {thingName :: Either String Int}",
              "",
              "f :: Int -> Either String Int",
              "f x = case x of",
              "  0 -> Left (\"zero\" <> show x)",
              "  n -> Right (g n)",
              "  where",
              "    g y = y + 1",
              "",
              "class C a where",
              "  type T a",
              "  m :: a -> Thing"
            ]
    let input =
          ParseInput
            { parseInputRelPath = [relfile|src/Fixture.hs|],
              parseInputDefaultExtensions = [],
              parseInputSource = source
            }

    -- What a rule used to find by walking the tree for itself is the oracle:
    -- the same nodes, in the same order, nested ones included, with every type
    -- collected in the one walk.
    it "finds every node of each wanted type that a walk for that type alone finds, in the same order" $ do
      parsed <- parseHaskellModule input >>= either (fail . show) pure
      let decls = parsedModuleDecls parsed
      let nodes =
            nodesIn
              ( S.fromList
                  [ someTypeRep (Proxy @(LHsExpr GhcPs)),
                    someTypeRep (Proxy @(HsBind GhcPs)),
                    someTypeRep (Proxy @(TyClDecl GhcPs)),
                    someTypeRep (Proxy @(LHsType GhcPs))
                  ]
              )
              decls
      -- Two of each, so that agreeing on nothing cannot pass for agreement.
      (length (nodesOf @(TyClDecl GhcPs) nodes), length (nodesOf @(HsBind GhcPs) nodes)) `shouldBe` (2, 2)
      map getLocA (nodesOf @(LHsExpr GhcPs) nodes)
        `shouldBe` map getLocA (listify (const True :: LHsExpr GhcPs -> Bool) decls)
      map getLocA (nodesOf @(LHsType GhcPs) nodes)
        `shouldBe` map getLocA (listify (const True :: LHsType GhcPs -> Bool) decls)
      let bindName :: HsBind GhcPs -> Maybe (LIdP GhcPs)
          bindName = \case
            FunBind {fun_id = n} -> Just n
            PatBind {} -> Nothing
            VarBind {} -> Nothing
            PatSynBind {} -> Nothing
      map (fmap getLocA . bindName) (nodesOf @(HsBind GhcPs) nodes)
        `shouldBe` map (fmap getLocA . bindName) (listify (const True :: HsBind GhcPs -> Bool) decls)
      map (getLocA . tyClDeclLName) (nodesOf @(TyClDecl GhcPs) nodes)
        `shouldBe` map (getLocA . tyClDeclLName) (listify (const True :: TyClDecl GhcPs -> Bool) decls)

    it "finds nothing of a type nobody asked for" $ do
      parsed <- parseHaskellModule input >>= either (fail . show) pure
      let nodes = nodesIn (S.fromList [someTypeRep (Proxy @(TyClDecl GhcPs))]) (parsedModuleDecls parsed)
      map getLocA (nodesOf @(LHsExpr GhcPs) nodes) `shouldBe` []
