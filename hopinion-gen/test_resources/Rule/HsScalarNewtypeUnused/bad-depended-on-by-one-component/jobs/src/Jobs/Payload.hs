module Jobs.Payload where

-- This package's library does not build against the one holding
-- InstallationId: only its test suite does. So the library cannot use the type
-- and the rule says nothing here, however much the field's name asks for it.
data ActivateRepoJob = ActivateRepoJob
  { activateRepoJobInstallationId :: !Int
  }
