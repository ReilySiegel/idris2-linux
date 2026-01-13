module System.Linux.Attr

import public Data.Buffer
import public Data.ByteString

import public System.Posix.Errno
import public System.Posix.File.ReadRes

import public System.Linux.Attr.Flags
import public System.Linux.Attr.Prim as P


%default total

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

||| Retries the given EPrim on an ERANGE error.
|||
||| Normally, we get ERANGE when something has changed the xattrs of a file
||| between our first call to get the buffer size needed, and our second call
||| to actually change the data, such that our buffer is no longer large
||| enough to hold the data. Unfortunately we can sometimes get ERANGE in
||| non-race error conditions (e.g. empty string passed for attribute name in
||| getxattr), so we should probably limit retries.
%inline
retryOnERANGE : {default 256 retryLimit : Nat} -> EPrim a -> EPrim a
retryOnERANGE {retryLimit=Z}     act t = act t
retryOnERANGE {retryLimit=(S n)} act t =
  case (act t) of
    R v t => R v t
    E e t => case (== ERANGE) <$> project Errno e of
      Just True  => retryOnERANGE {retryLimit=n} act t
      _          => E e t


||| Separate a ByteString of null-terminated strings to a List String.
%inline
nullTermStrings : ByteString -> List String
nullTermStrings bs = toString <$> splitNonEmpty 0 bs

%inline
eliftWithRetry : Has Errno es => EIO1 f => EPrim a -> f es a
eliftWithRetry act = elift1 $ retryOnERANGE act

--------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------

parameters {auto has : Has Errno es}
           {auto eio : EIO1 f}
           (file : String)

  ||| List the extended attributes of `file`.
  export %inline
  listxattr : f es $ List String
  listxattr = nullTermStrings <$> (eliftWithRetry $ P.listxattr file)

  ||| List the extended attributes of `file`, without following symlinks.
  export %inline
  llistxattr : f es $ List String
  llistxattr = nullTermStrings <$> (eliftWithRetry $ P.llistxattr file)

  ||| Get the value of the extended attribute `n` from `file`.
  export %inline
  getxattr : FromBuf a => (name : String) -> f es a
  getxattr n = eliftWithRetry $ P.getxattr file n

  ||| Get the value of the extended attribute `n` from `file`, without
  ||| following symlinks.
  export %inline
  lgetxattr : FromBuf a => (name : String) -> f es a
  lgetxattr n = eliftWithRetry $ P.lgetxattr file n

  ||| Set the value of the extended attribute `n` on `file`.
  export %inline
  setxattr : ToBuf a => (name : String) -> a -> Flags -> f es ()
  setxattr n v fg = elift1 $ P.setxattr file n v fg

  ||| Set the value of the extended attribute `n` on `file`.
  export %inline
  lsetxattr : ToBuf a => (name : String) -> a -> Flags -> f es ()
  lsetxattr n v fg = elift1 $ P.lsetxattr file n v fg

  ||| Remove the extended attribute `n` on `file`.
  export %inline
  removexattr : (name : String) -> f es ()
  removexattr n = elift1 $ P.removexattr file n

  ||| Remove the extended attribute `n` on `file`, without following symlinks.
  export %inline
  lremovexattr : (name : String) -> f es ()
  lremovexattr n = elift1 $ P.lremovexattr file n

parameters {auto has : Has Errno es}
           {auto eio : EIO1 f}
           {auto _ : FileDesc d}
           (fd : d)

  ||| List the extended attributes of `fd`.
  export %inline
  flistxattr : f es $ List String
  flistxattr = nullTermStrings <$> (eliftWithRetry $ P.flistxattr fd)

  ||| Get the value of the extended attribute `n` from `fd`.
  export %inline
  fgetxattr : FromBuf a => (name : String) -> f es a
  fgetxattr n = eliftWithRetry $ P.fgetxattr fd n

  ||| Set the value of the extended attribute `n` on `fd`.
  export %inline
  fsetxattr : ToBuf a => (name : String) -> a -> Flags -> f es ()
  fsetxattr n v fg = elift1 $ P.fsetxattr fd n v fg

  ||| Remove the extended attribute `n` on `fd`.
  export %inline
  fremovexattr : (name : String) -> f es ()
  fremovexattr n = elift1 $ P.fremovexattr fd n
