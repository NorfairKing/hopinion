module Thing.Jobs where

-- The same field as the dirty case, and the same type sitting in the same
-- repository, and no finding: this package does not build against the one that
-- holds InstallationId, so using it means editing a cabal file as well. That is
-- a different change from the one this rule asks for, and a rule that named a
-- type a site cannot reach would be asking for the dependency without saying
-- so.
data ActivateRepoJob = ActivateRepoJob
  { activateRepoJobInstallationId :: !Int
  }
