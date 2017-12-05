module ErrorHandler (unsafeCleanup) where

import Control.Exception (catch, ErrorCall)
import System.IO.Unsafe (unsafePerformIO)

unsafeCleanup :: a -> Maybe a
unsafeCleanup x = unsafePerformIO $ catch (x `seq` return (Just x)) handler
    where
    handler exc = return Nothing  `const`  (exc :: ErrorCall)