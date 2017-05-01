{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PackageImports #-}

module ShakeUtil.Wrapper(
    module Development.Shake,
    -- renamed
    copyPath, copyChanged,
    readPath, readLines,
    writePath, writeLines,
    -- the rest
    need, needed, want,
    withoutActions,
    priority, alternatives,
    actionOnException, actionFinally,
    getShakeOptions, getProgress, getVerbosity,
    putLoud, putNormal, putQuiet,
    withVerbosity, quietly,
    command, command_,
    ) where

import qualified "shake" Development.Shake as Shake
import           "shake" Development.Shake hiding (
    need, needed, want,
    copyFile', copyFileChanged,
    readFile', readFileLines,
    writeFile', writeFileLines,
    withTempFile, withTempDir,
    withoutActions,
    priority, alternatives,
    actionOnException, actionFinally,
    getShakeOptions, getProgress, getVerbosity,
    putLoud, putNormal, putQuiet,
    withVerbosity, quietly,
    command, command_,
    )
import           ShakeUtil.Wrapper.Internal
import           ShakeUtil.Types


-------------------------------------------------------------------
-- POLYMORPHIC WRAPPERS

need :: (_)=> [FileString] -> act ()
need = liftAction . Shake.need
needed :: (_)=> [FileString] -> act ()
needed = liftAction . Shake.needed

want :: (_)=> [FileString] -> act ()
want = liftRules . Shake.want

copyPath :: (_)=> FileString -> FileString -> act ()
copyPath = liftAction .: Shake.copyFile'
copyChanged :: (_)=> FileString -> FileString -> act ()
copyChanged = liftAction .: Shake.copyFileChanged

readPath :: (_) => FileString -> action String
readPath = liftAction . Shake.readFile'
readLines :: (_) => FileString -> action [String]
readLines = liftAction . Shake.readFileLines

-- FIXME these can probably be MonadIO instead of MonadAction
writePath :: (_) => FileString -> String -> io ()
writePath = liftAction .: Shake.writeFile'
writeLines :: (_) => FileString -> [String] -> io ()
writeLines = liftAction .: Shake.writeFileLines

withoutActions :: (_)=> rules () -> rules ()
withoutActions = liftRules1 Shake.withoutActions

priority :: (_)=> Double -> rules () -> rules ()
priority p = liftRules1 $ Shake.priority p

alternatives :: (_)=> rules () -> rules ()
alternatives = liftRules1 Shake.alternatives

-- argument order is unapologetically flipped from Shake's
actionOnException :: (_)=> IO b -> action a -> action a
actionOnException f = liftAction1 $ flip Shake.actionOnException f

actionFinally :: (_)=> IO b -> action a -> action a
actionFinally f = liftAction1 $ flip Shake.actionFinally f

getShakeOptions :: (_)=> action ShakeOptions
getShakeOptions = liftAction Shake.getShakeOptions

getProgress :: (_)=> action Progress
getProgress = liftAction Shake.getProgress

getVerbosity :: (_)=> action Verbosity
getVerbosity = liftAction Shake.getVerbosity

putLoud :: (_)=> String -> action ()
putLoud = liftAction . Shake.putLoud
putNormal :: (_)=> String -> action ()
putNormal = liftAction . Shake.putNormal
putQuiet :: (_)=> String -> action ()
putQuiet = liftAction . Shake.putQuiet

withVerbosity :: (_)=> Verbosity -> action a -> action a
withVerbosity v = liftAction1 (Shake.withVerbosity v)
quietly :: (_)=> action a -> action a
quietly = liftAction1 Shake.quietly

command :: (CmdResult r, _) => [CmdOption] -> String -> [String] -> action r
command = liftAction .:: Shake.command
command_ :: (_)=> [CmdOption] -> String -> [String] -> action ()
command_ = liftAction .:: Shake.command_

-- no way in hell we're wrapping this.
-- do 'liftAction $ cmd args...' manually
--cmd :: CmdArguments args => args :-> Action r
