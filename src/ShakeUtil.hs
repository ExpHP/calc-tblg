    -- These are not the Defs you are looking for...

-- (definitions have been moved out so we can reduce the number of
--  nasty 'hiding' and reexport lists.)
module ShakeUtil(
    module ShakeUtil.Wrapper,
    module ShakeUtil.Defs,
    module ShakeUtil.Types,
    ) where

-- Most overridden shake commands are hidden in Wrapper.hs
-- The ones hidden here are defined in Defs.hs.
import ShakeUtil.Wrapper hiding (shakeArgs)
import ShakeUtil.Defs
import ShakeUtil.Types
