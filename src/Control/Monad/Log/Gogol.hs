{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# language OverloadedStrings #-}
{-# language TypeFamilies #-}
{-# options_ghc -Wno-unused-imports #-}
module Control.Monad.Log.Gogol (gogolLogging, textLogEntry, jsonLogEntry, gceInstance, LoggingScopes, HasLoggingScopes) where

import Control.Monad.IO.Class (MonadIO(..))
import Data.Functor (void)
import qualified Data.List.NonEmpty as NE (NonEmpty, nonEmpty, toList)
import Data.Proxy (Proxy(..))

-- aeson
import qualified Data.Aeson as A (FromJSON(..), ToJSON(..), encode)
-- exceptions
import Control.Monad.Catch (MonadCatch(..), MonadMask(..))
-- gogol
import Network.Google (runGoogle, HasEnv(..), Env, runResourceT, send, upload, AsError(..), Error(..), trying)
import Network.Google.Auth.Scope (AllowScopes(..), HasScope')
import Network.Google.Types (GoogleRequest(..), Scopes)
-- gogol-core
import Network.Google.Data.JSON (JSONValue)
-- gogol-logging
import Network.Google.Logging.Types (loggingWriteScope, LogEntrySeverity(..), WriteLogEntriesRequest, writeLogEntriesRequest, wlerEntries, LogEntry, logEntry, leSeverity, leTextPayload, leLogName, leResource, MonitoredResource, monitoredResource, mrLabels, mrType, MonitoredResourceLabels, mrlAddtional, LogEntryJSONPayload, logEntryJSONPayload, leJSONPayload, lejpAddtional)
import Network.Google.Resource.Logging.Entries.Write (EntriesWrite, entriesWrite, ewPayload)
-- lens
import Control.Lens ((&), (.~), _Just, (%~))
-- logging-effect
import Control.Monad.Log (withBatchedHandler, BatchingOptions(..), defaultBatchingOptions, WithSeverity(..), Handler, runLoggingT)
import qualified Control.Monad.Log as L (Severity(..))
-- text
import Data.Text (Text, pack, unpack)
-- unordered-containers
import qualified Data.HashMap.Strict as HM (HashMap, singleton, insert, fromList)

-- | Logging bracket that periodically flushes the logs to Google Cloud Logging (formerly Stackdriver) https://cloud.google.com/logging/docs
gogolLogging :: (HasScope' s LoggingScopes ~ 'True, AllowScopes s, HasEnv s genv, MonadIO io, MonadMask io) =>
                BatchingOptions
             -> (MonitoredResource -> a -> LogEntry) -- ^ how to produce a log entry e.g. via 'textLogEntry' or 'jsonLogEntry'
             -> genv -- ^ a configuration variable that can provide an 'Env'
             -> MonitoredResource -- ^ annotate the log entries with the resource producing them, e.g. 'gceInstance'
             -> (Handler io a -> io b) -- ^ User program runs in here, via e.g. 'runLoggingT' or similar
             -> io b
gogolLogging opts f genv res = withBatchedHandler opts flush
  where
    flush mq = runResourceT . runGoogle genv $ do
      void $ trying _Error $ send $ entriesWrite (writeLogEntriesRequest & wlerEntries .~ (map (f res) $ NE.toList mq))

type HasLoggingScopes s = (HasScope' s LoggingScopes ~ 'True, AllowScopes s)

type LoggingScopes = '["https://www.googleapis.com/auth/cloud-platform",
                     "https://www.googleapis.com/auth/logging.admin",
                     "https://www.googleapis.com/auth/logging.write"]

-- | Construct log entries made of objects that can be JSON-encoded (via their @aeson@ 'A.ToJSON' instances)
jsonLogEntry :: A.ToJSON a =>
                Text -- ^ logging entry key
             -> MonitoredResource
             -> WithSeverity a -- ^ log payload, with its severity annotation
             -> LogEntry
jsonLogEntry k r (WithSeverity ls t) = jsonLogEntry_ r (mapSeverity $ Just ls) k t

jsonLogEntry_ :: A.ToJSON a =>
                MonitoredResource
             -> LogEntrySeverity
             -> Text
             -> a
             -> LogEntry
jsonLogEntry_ r s k v = logEntry &
                       leSeverity .~ Just s &
                       leResource .~ Just r &
                       leJSONPayload . _Just . lejpAddtional .~ HM.singleton k (A.toJSON v)

-- | Construct log entries made of UTF-8 plain text
textLogEntry :: MonitoredResource
             -> WithSeverity Text -- ^ log payload, with its severity annotation
             -> LogEntry
textLogEntry r (WithSeverity ls t) = textLogEntry_ r (mapSeverity $ Just ls) t

textLogEntry_ :: MonitoredResource
             -> LogEntrySeverity
             -> Text
             -> LogEntry
textLogEntry_ r s t = logEntry &
                     leSeverity .~ Just s &
                     leTextPayload .~ Just t &
                     leResource .~ Just r

mapSeverity :: Maybe L.Severity -> LogEntrySeverity
mapSeverity (Just s) = case s of
  L.Debug -> Debug
  L.Informational -> Info
  L.Notice -> Notice
  L.Warning -> Warning
  L.Error -> Error'
  L.Critical -> Critical
  L.Alert -> Alert
  L.Emergency -> Emergency
mapSeverity Nothing = Default

-- | If the user's service runs within GCE, use this as 'MonitoredResource'
--
-- Full list : https://cloud.google.com/logging/docs/api/v2/resource-list#resource-types 
gceInstance :: Text -- ^ project ID
            -> Text -- ^ instance ID
            -> Text -- ^ zone
            -> MonitoredResource
gceInstance prid insid z = monitoredResource &
  mrType . _Just .~ "gce_instance" &
  mrLabels . _Just . mrlAddtional .~  (HM.fromList [("project_id", prid), ("instance_id", insid), ("zone", z)])
