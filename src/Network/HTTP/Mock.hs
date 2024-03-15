-- Copyright 2019 Fernando Rincon Martin
-- 
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
-- 
--     http://www.apache.org/licenses/LICENSE-2.0
-- 
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.


-- |
-- Module      :  Network.HTTP.Mock
-- Copyright   :  (c) 2019 Fernando Rincon Martin
-- License     :  Apache-2.0
-- Maintainer  :  Fernando Rincon <f.rincon@protonmail.com>
-- 
-- This module provides tools for testing http clients agains a mocked API.
--
-- Currently the support is very basic, and is only limited to the @'withMockedManager'@,
-- wich, passing a Wai @'Application'@ and a function that accepts a http-client manager, 
-- it will run the function passing a spcial manager who all the request to that manager
-- will be processed by the @'Application'@ passed as an argument.
--
-- Note that the current implementation uses Unix Domain sockets in order to serve all the 
-- request, so most probably the code will not work in windows.

module Network.HTTP.Mock
  ( 
    -- * Mocking Functions
    withMockedManager,
    withMockedTlsManager,
    withMockedManagerSettings
  ) where

import Network.Wai (Application)
import Network.HTTP.Client (Manager, ManagerSettings, defaultManagerSettings, managerRawConnection, socketConnection, newManager, managerTlsConnection)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.Socket 
import System.Directory (removeFile, getTemporaryDirectory)
import Control.Exception (bracket)
import Control.Concurrent.Async (withAsync)
import System.FilePath ((</>))
import System.Random (randomIO)
import Network.Wai.Handler.Warp (runSettingsSocket, defaultSettings)

-- | Run a action passing a http-client 'Manager' that uses the 'Application' as resolver for all requests.
-- Uses default `ManagerSettings`.
withMockedManager :: Application -> (Manager -> IO a) -> IO a
withMockedManager = withMockedManagerSettings defaultManagerSettings

-- | Run a action passing a http-client 'Manager' that uses the 'Application' as resolver for all requests.
-- Uses `ManagerSettings` with TLS enabled.
withMockedTlsManager :: Application -> (Manager -> IO a) -> IO a
withMockedTlsManager = withMockedManagerSettings tlsManagerSettings

-- | Run an action passing a http client 'Manager' that uses the 'Application' as resolver for all requests.
-- The `Manager` will be created from the given `ManagerSettings`.
withMockedManagerSettings :: ManagerSettings -> Application -> (Manager -> IO a) -> IO a
withMockedManagerSettings managerSettings app f =
  bracket startSocket closeSocket $ \serverSocket -> do
  socketAddr <- getSocketName serverSocket
  withAsync (runSettingsSocket defaultSettings serverSocket app) $ \_ -> do
    manager <- newManager $ managerSettings { managerRawConnection = (return $ rawConnection socketAddr)
                                            , managerTlsConnection = (return $ rawConnection socketAddr)
                                            }
    f manager
  where

    startSocket = do
      socketName <- createNewSocketName
      serverSocket <- socket AF_UNIX Stream defaultProtocol
      setSocketOption serverSocket ReuseAddr 1
      bind serverSocket $ SockAddrUnix socketName
      listen serverSocket 10
      return serverSocket

    closeSocket sock = do
      (SockAddrUnix path) <- getSocketName sock
      close sock
      removeFile path

    rawConnection socketAddr _ _ _ = do
      socket <- socket AF_UNIX Stream defaultProtocol
      connect socket socketAddr
      socketConnection socket 1024
    
    createNewSocketName = do
      temporaryDirectoryName <- getTemporaryDirectory
      -- pid <- getProcessID 
      random <- show <$> (randomIO :: IO Int)
      return $ temporaryDirectoryName </> ("socket_mock_" ++ random)