{-# LANGUAGE LambdaCase #-}
{-# language OverloadedStrings #-}
{-# language TypeFamilies #-}
{-# options_ghc -Wno-unused-imports #-}
module Control.Monad.Log.Gogol where

import Control.Monad.IO.Class (MonadIO(..))
import qualified Data.List.NonEmpty as NE (NonEmpty, nonEmpty, toList)
import Data.Proxy (Proxy(..))

-- aeson
import qualified Data.Aeson as A (FromJSON(..), ToJSON(..), encode)
-- exceptions
import Control.Monad.Catch (MonadCatch(..))
-- gogol
import Network.Google (runGoogle, Env, runResourceT, send, upload)
import Network.Google.Auth.Scope (HasScope')
import Network.Google.Types (GoogleRequest(..), Scopes)
-- gogol-core
import Network.Google.Data.JSON (JSONValue)
-- gogol-logging
import Network.Google.Logging.Types (loggingWriteScope, LogEntrySeverity(..), WriteLogEntriesRequest, writeLogEntriesRequest, wlerEntries, LogEntry, logEntry, leSeverity, leTextPayload, leLogName, leResource, MonitoredResource, monitoredResource, mrLabels, mrType, MonitoredResourceLabels, mrlAddtional, LogEntryJSONPayload, logEntryJSONPayload, leJSONPayload, lejpAddtional)
import Network.Google.Resource.Logging.Entries.Write (EntriesWrite, entriesWrite, ewPayload)
-- lens
import Control.Lens ((&), (.~), _Just)
-- logging-effect
import Control.Monad.Log (withBatchedHandler, BatchingOptions(..), defaultBatchingOptions, WithSeverity(..))
import qualified Control.Monad.Log as L (Severity(..))
-- text
import Data.Text (Text, pack, unpack)
-- unordered-containers
import qualified Data.HashMap.Strict as HM (HashMap, singleton, insert, fromList)

-- gogolLogging genv = withBatchedHandler defaultBatchingOptions flush
--   where
--     flush mq = runResourceT . runGoogle genv $ send (writeLogEntriesRequest & wlerEntries .~ (NE.toList mq))

reqPayload mq = ewPayload . wlerEntries .~ (NE.toList mq)

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

jsonLogEntry :: A.ToJSON a =>
                MonitoredResource
             -> LogEntrySeverity
             -> Text
             -> a
             -> LogEntry
jsonLogEntry r s k v = logEntry &
                       leSeverity .~ Just s &
                       leResource .~ Just r &
                       leJSONPayload . _Just . lejpAddtional .~ HM.singleton k (A.toJSON v)

textLogEntry :: MonitoredResource
             -> LogEntrySeverity
             -> Text
             -> LogEntry
textLogEntry r s t = logEntry &
                     leSeverity .~ Just s &
                     leTextPayload .~ Just t &
                     leResource .~ Just r


gceInstance :: Text -- ^ project ID
            -> Text -- ^ instance ID
            -> Text -- ^ zone
            -> MonitoredResource
gceInstance prid insid z = monitoredResource &
  mrType . _Just .~ "gce_instance" &
  mrLabels . _Just . mrlAddtional .~  (HM.fromList [("project_id", prid), ("instance_id", insid), ("zone", z)])
