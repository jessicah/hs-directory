module System.Directory.Internal.Haiku where
#if defined(haiku_HOST_OS)
import System.OsPath ((</>))
import qualified System.Directory.Internal.Posix

-- Haiku's default file system, BFS, does not support hard links
createHardLink :: OsPath -> OsPath -> IO ()
createHardLink (OsString p1) (OsString p2) = Posix.createSymbolicLink p1 p2

getXdgDirectoryFallback :: IO OsPath -> XdgDirectory -> IO OsPath
getXdgDirectoryFallback getHomeDirectory xdgDir = do
  (<$> getHomeDirectory) $ flip (</>) $ case xdgDir of
    XdgData   -> os "/boot/home/config/non-packaged/data"
    XdgConfig -> os "/boot/home/config/non-packaged/settings"
    XdgCache  -> os "/boot/home/config/non-packaged/cache"
    XdgState  -> os "/boot/home/config/non-packaged/etc"

getXdgDirectoryListFallback :: XdgDirectoryList -> IO [OsPath]
getXdgDirectoryListFallback xdgDirs =
  pure $ case xdgDirs of
    XdgDataDirs   -> [os "/boot/home/config/non-packaged/data",
                      os "/boot/home/config/data",
                      os "/boot/system/non-packaged/data",
                      os "/boot/system/data"]
    XdgConfigDirs -> [os "/boot/home/config/non-packaged/settings",
                      os "/boot/home/config/settings",
                      os "/boot/system/non-packaged/settings",
                      os "/boot/system/settings"]

getAppUserDataDirectoryInternal :: OsPath -> IO OsPath
getAppUserDataDirectoryInternal appName =
  (\ home -> home <> (os "/config/settings/" <> appName)) <$< Posix.getHomeDirectoryInternal

getUserDocumentsDirectoryInternal :: IO OsPath
getUserDocumentsDirectoryInternal = Posix.getHomeDirectoryInternal

getTemporaryDirectoryInternal :: IO OsPath
getTemporaryDirectoryInternal = fromMaybe (os "/tmp") <$> lookupEnvOs (os "TMPDIR")

#endif
