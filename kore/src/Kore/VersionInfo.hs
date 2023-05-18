{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoDuplicateRecordFields #-}

{- |
Copyright   : (c) Runtime Verification, 2021
License     : BSD-3-Clause
-}
module Kore.VersionInfo (
    VersionInfo (..),
    versionInfo,
) where

import Data.Aeson (
    FromJSON,
 )
import Data.Aeson qualified as Aeson
import Data.Binary (Binary)
import Data.List qualified as List
import Development.GitRev qualified as GitRev
import GHC.Generics qualified as GHC
import Language.Haskell.TH (
    Exp,
    Q,
 )
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Syntax (
    Lift,
 )
import Language.Haskell.TH.Syntax qualified as TH
import Prelude.Kore
import System.Directory qualified as Directory
import System.FilePath (
    isRelative,
    joinPath,
    splitDirectories,
    takeDirectory,
    (</>),
 )

-- | Information about the current version of Kore.
data VersionInfo = VersionInfo
    { gitHash :: !String
    , gitCommitDate :: !String
    , gitBranch :: !(Maybe String)
    , gitDirty :: !Bool
    }
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving stock (Lift)

instance FromJSON VersionInfo

-- | Produce (at compile-time) information about the current version of Kore.
versionInfo :: Q Exp
versionInfo = do
    packageRoot <- getPackageRoot
    let versionFile = packageRoot </> "version.json"
    haveVersionFile <- Directory.doesFileExist versionFile & TH.runIO
    if haveVersionFile
        then readVersionInfoFile versionFile
        else defaultVersionInfo
  where
    readVersionInfoFile versionFile = do
        result <- Aeson.eitherDecodeFileStrict' versionFile & TH.runIO
        either fail (TH.lift @_ @VersionInfo) result
    defaultVersionInfo =
        [|
            VersionInfo
                { gitHash = $(GitRev.gitHash)
                , gitCommitDate = $(GitRev.gitCommitDate)
                , gitBranch = Just $(GitRev.gitBranch)
                , gitDirty = $(GitRev.gitDirty)
                }
            |]

{- | Find the root of the package.

@getPackageRoot@ looks upward from the current file (i.e. the file into which it
is spliced) to find the root directory of the package.
-}
getPackageRoot :: Q FilePath
getPackageRoot = do
    bot <- takeDirectory . TH.loc_filename <$> TH.location
    let parents = getParents bot
    TH.runIO $ findPackageRoot bot parents
  where
    isProjectRoot here = Directory.doesFileExist (here </> "kore.cabal")

    getParents bottom =
        bottom
            & splitDirectories
            & List.inits
            & reverse
            & map joinPath
            & filter (sameRelativity bottom)

    sameRelativity bottom = \here -> isRelative bottom == isRelative here

    -- Find the root directory of the current package. This module file can
    -- be moved safely because the package root is found at build time.
    findPackageRoot bot [] =
        fail ("Could not find kore.cabal above " ++ bot)
    findPackageRoot bot (here : heres) = do
        foundRoot <- isProjectRoot here
        if foundRoot then Directory.makeAbsolute here else goUp
      where
        goUp = findPackageRoot bot heres
