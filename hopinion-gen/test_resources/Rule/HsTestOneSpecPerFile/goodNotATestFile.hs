module GoodNotATestFile where

-- A module that is not a test file may export what it likes, including
-- everything, which is what having no export list says.
theAnswer :: Int
theAnswer = 42
