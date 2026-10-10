{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE BangPatterns #-}

-- |
-- Module: Censor.Runner.DL
-- Copyright: (c) 2026 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- Minimal @dlopen@\/@dlsym@ bindings for driving a foreign
-- constant-time target that lives in a shared library. Used by the
-- @censor@ executable to resolve a symbol at runtime and hand it to
-- "Censor.FFI" as an @ffiTarget@.
--
-- The underlying handle is opened with @RTLD_NOW | RTLD_LOCAL@: any
-- unresolved symbols surface immediately (so bad shims are caught at
-- 'withLibrary' entry, not at first call), and the library's own
-- symbols do not pollute the global namespace. Not available on
-- Windows.

module Censor.Runner.DL (
    -- * Library handle
    Library
  , withLibrary

    -- * Resolving targets
  , resolveTarget

    -- * Errors
  , DLError(..)
  ) where

import Control.Exception (Exception, bracket, throwIO)
import Data.Word (Word8)
import Foreign.C.String (CString, peekCString, withCString)
import Foreign.Ptr (FunPtr, Ptr, castPtrToFunPtr, nullPtr)

foreign import ccall unsafe "censor_dlopen"
  c_dlopen :: CString -> IO (Ptr ())

foreign import ccall unsafe "censor_dlsym"
  c_dlsym :: Ptr () -> CString -> IO (Ptr ())

foreign import ccall unsafe "censor_dlclose"
  c_dlclose :: Ptr () -> IO Int

foreign import ccall unsafe "censor_dlerror"
  c_dlerror :: IO CString

-- unsafe: this is the timed call, and a safe one would put the RTS's
-- suspend/resume of the calling thread inside the timed region.
foreign import ccall unsafe "dynamic"
  mkTargetFun :: FunPtr (Ptr Word8 -> IO ()) -> Ptr Word8 -> IO ()

-- | An opened shared library. Bracketed by 'withLibrary'; do not
--   retain the value past its callback.
newtype Library = Library (Ptr ())

-- | Why a dynamic-linker operation failed. The 'String' payload is
--   the @dlerror@ message at the moment of failure.
data DLError
  = DLOpenFailed !FilePath !String
    -- ^ @dlopen@ returned null while opening this path.
  | DLSymFailed !String !String
    -- ^ @dlsym@ returned null for this symbol name.
  deriving (Eq, Show)

instance Exception DLError

-- | Open a shared library for the duration of the callback and close
--   it on exit. Throws 'DLOpenFailed' if the library cannot be
--   loaded.
withLibrary :: FilePath -> (Library -> IO a) -> IO a
withLibrary path = bracket (open path) close
  where
    open p = do
      h <- withCString p c_dlopen
      if h == nullPtr
        then do
          !msg <- readErr
          throwIO (DLOpenFailed p msg)
        else pure (Library h)
    close (Library h) = do
      !_ <- c_dlclose h
      pure ()

-- | Resolve a symbol as a @void(uint8_t *)@ target action. Throws
--   'DLSymFailed' if the symbol is absent from the library.
--
--   The resolved 'FunPtr' is not retained past the enclosing
--   'withLibrary' scope; do not call the returned action after the
--   library has been closed.
resolveTarget :: Library -> String -> IO (Ptr Word8 -> IO ())
resolveTarget (Library h) name = do
  p <- withCString name (c_dlsym h)
  if p == nullPtr
    then do
      !msg <- readErr
      throwIO (DLSymFailed name msg)
    else pure $! mkTargetFun (castPtrToFunPtr p)

readErr :: IO String
readErr = do
  s <- c_dlerror
  if s == nullPtr then pure "(unknown)" else peekCString s
