module Thing.Jobs where

-- The shape the rule is for, and the one the measurement found four times: a
-- job payload carrying an id as the primitive, in a different package from the
-- type that names it.
data ActivateRepoJob = ActivateRepoJob
  { activateRepoJobInstallationId :: !Int
  }
