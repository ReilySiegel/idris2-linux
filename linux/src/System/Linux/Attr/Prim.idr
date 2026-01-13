module System.Linux.Attr.Prim

import public Data.Buffer
import public Data.Buffer.Core
import public Data.ByteString
import public Data.ByteVect
import public Data.C.Ptr

import public System.Posix.Errno
import public System.Posix.File.FileDesc
import public System.Posix.File.ReadRes

import public System.Linux.Attr.Flags

%default total

--------------------------------------------------------------------------------
-- FFI
--------------------------------------------------------------------------------

%foreign "C:li_listxattr, linux-idris"
prim__listxattr : (file : String) -> Buffer -> (bytes : SizeT) -> PrimIO SsizeT

%foreign "C:li_llistxattr, linux-idris"
prim__llistxattr : (file : String) -> Buffer -> (bytes : SizeT) -> PrimIO SsizeT

%foreign "C:li_flistxattr, linux-idris"
prim__flistxattr : (fd : Bits32) -> Buffer -> (bytes : SizeT) -> PrimIO SsizeT

%foreign "C:li_getxattr, linux-idris"
prim__getxattr : (file : String) -> (name : String) -> Buffer
              -> (bytes : SizeT) -> PrimIO SsizeT

%foreign "C:li_lgetxattr, linux-idris"
prim__lgetxattr : (file : String) -> (name : String) -> Buffer
               -> (bytes : SizeT) -> PrimIO SsizeT

%foreign "C:li_fgetxattr, linux-idris"
prim__fgetxattr : (fd : Bits32) -> (name : String) -> Buffer
               -> (bytes : SizeT) -> PrimIO SsizeT

%foreign "C:li_setxattr, linux-idris"
prim__setxattr : (file : String) -> (name : String) -> Buffer
              -> (off,bytes : SizeT) -> (flags : CInt) -> PrimIO CInt

%foreign "C:li_lsetxattr, linux-idris"
prim__lsetxattr : (file : String) -> (name : String) -> Buffer
               -> (off,bytes : SizeT) -> (flags : CInt) -> PrimIO CInt

%foreign "C:li_fsetxattr, linux-idris"
prim__fsetxattr : (fd : Bits32) -> (name : String) -> Buffer
               -> (off,bytes : SizeT) -> (flags : CInt) -> PrimIO CInt

%foreign "C:li_setxattr, linux-idris"
prim__setxattrptr : (file : String) -> (name : String) -> AnyPtr
                 -> (off,bytes : SizeT) -> (flags : CInt) -> PrimIO CInt

%foreign "C:li_lsetxattr, linux-idris"
prim__lsetxattrptr : (file : String) -> (name : String) -> AnyPtr
                  -> (off,bytes : SizeT) -> (flags : CInt) -> PrimIO CInt

%foreign "C:li_fsetxattr, linux-idris"
prim__fsetxattrptr : (fd : Bits32) -> (name : String) -> AnyPtr
                  -> (off,bytes : SizeT) -> (flags : CInt) -> PrimIO CInt

%foreign "C:li_removexattr, linux-idris"
prim__removexattr : (file : String) -> (name : String) -> PrimIO CInt

%foreign "C:li_lremovexattr, linux-idris"
prim__lremovexattr : (file : String) -> (name : String) -> PrimIO CInt

%foreign "C:li_fremovexattr, linux-idris"
prim__fremovexattr : (fd : Bits32) -> (name : String) -> PrimIO CInt

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

||| Calls the given PrimIO with a zero-length buffer, and size 0. Allocates a
||| buffer of the size returned, and calls the given PrimIO again with the
||| newly allocated buffer, and its size.
%inline
getSizeAndRead : FromBuf a => (Buffer -> SizeT -> PrimIO SsizeT) -> EPrim a
getSizeAndRead act t =
  let emptyBuf # t := toF1 (prim__newBuf 0) t
      size     # t := toF1 (act emptyBuf 0) t
  in if size < 0
    then E (inject $ fromNeg size) t
    else let
      buf      # t := toF1 (prim__newBuf $ cast size) t
      rd       # t := toF1 (act buf $ cast size) t
      in if rd < 0
        then E (inject $ fromNeg rd) t
        else let r # t := fromBuf (B (cast rd) buf) t in R r t

||| Given a ToBuf, choose an action funciton based on if the buffer is backed
||| by a bytestring or a pointer.
%inline
toBufChoose : ToBuf b => b
           -> (Buffer -> (off,bytes : SizeT) -> a)
           -> (AnyPtr -> (off,bytes : SizeT) -> a)
           -> a
toBufChoose v buf ptr =
  case (unsafeToBuf v) of
    Left (CP s p) => ptr p 0 (cast s)
    Right (BS s $ BV b o _) =>
      buf (unsafeGetBuffer b) (cast o) (cast s)

--------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------

parameters (file : String)

  ||| List the extended attributes of `file`, as a Buffer of null-terminated
  ||| Strings.
  export %inline
  listxattr : FromBuf a => EPrim a
  listxattr = getSizeAndRead $ prim__listxattr file

  ||| List the extended attributes of `file` without following symlinks,
  ||| as a Buffer of null-terminated Strings.
  export %inline
  llistxattr : FromBuf a => EPrim a
  llistxattr = getSizeAndRead $ prim__llistxattr file

  ||| Get the extended attribute `n` from `file`.
  export %inline
  getxattr : FromBuf a => (name : String) -> EPrim a
  getxattr n = getSizeAndRead $ prim__getxattr file n

  ||| Get the extended attribute `n` from `file`, without following symlinks.
  export %inline
  lgetxattr : FromBuf a => (name : String) -> EPrim a
  lgetxattr n = getSizeAndRead $ prim__lgetxattr file n


  ||| Set the value of the extended attribute `n` on `file`.
  export %inline
  setxattr : ToBuf a => (name : String) -> a -> Flags -> EPrim ()
  setxattr n v f = toUnit $
    toBufChoose v (prim__setxattr file n) (prim__setxattrptr file n) $ fg f

  ||| Set the value of the extended attribute `n` on `file`, without
  ||| following symlinks.
  export %inline
  lsetxattr : ToBuf a => (name : String) -> a -> Flags -> EPrim ()
  lsetxattr n v f = toUnit $
    toBufChoose v (prim__lsetxattr file n) (prim__lsetxattrptr file n) $ fg f

  ||| Remove the extended attribute `n` on `file`.
  export %inline
  removexattr : (name : String) -> EPrim ()
  removexattr n = toUnit $ prim__removexattr file n

  ||| Remove the extended attribute `n` on `file` without following symlinks.
  export %inline
  lremovexattr : (name : String) -> EPrim ()
  lremovexattr n = toUnit $ prim__lremovexattr file n


parameters {auto _ : FileDesc d}
           (fd : d)

  ||| List the extended attributes of `fd`, as a Buffer of null-terminated
  ||| Strings.
  export %inline
  flistxattr : FromBuf a => EPrim a
  flistxattr = getSizeAndRead $ prim__flistxattr $ fileDesc fd

  ||| Get the extended attribute `n` from `fd'.
  export %inline
  fgetxattr : FromBuf a => (name : String) -> EPrim a
  fgetxattr n = getSizeAndRead $ prim__fgetxattr (fileDesc fd) n

  ||| Set the value of the extended attribute `n` on `fd`
  export %inline
  fsetxattr : ToBuf a => (name : String) -> a -> Flags -> EPrim ()
  fsetxattr n v f = toUnit $
    let fd := fileDesc fd
    in toBufChoose v (prim__fsetxattr fd n) (prim__fsetxattrptr fd n) $ fg f

  ||| Remove the extended attribute `n` on `fd`.
  export %inline
  fremovexattr : (name : String) -> EPrim ()
  fremovexattr n = toUnit $ prim__fremovexattr (fileDesc fd) n
