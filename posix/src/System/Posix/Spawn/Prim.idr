module System.Posix.Spawn.Prim

import public Data.C.Ptr

import public System.Posix.File.Prim
import public System.Posix.Errno
import public System.Posix.Spawn.Struct
import public System.Posix.Spawn.Types

%default total

--------------------------------------------------------------------------------
-- FFI
--------------------------------------------------------------------------------

%foreign "C:posix_spawn_file_actions_destroy, libc, spawn.h"
prim__posix_spawn_file_actions_destroy : AnyPtr -> PrimIO Bits32

%foreign "C:posix_spawn_file_actions_init, libc, spawn.h"
prim__posix_spawn_file_actions_init : AnyPtr -> PrimIO Bits32

%foreign "C:posix_spawn_file_actions_addclose, libc, spawn.h"
prim__posix_spawn_file_actions_addclose : AnyPtr -> Bits32 -> PrimIO Bits32

%foreign "C:posix_spawn_file_actions_addopen, libc, spawn.h"
prim__posix_spawn_file_actions_addopen : AnyPtr -> Bits32 -> String -> Bits32 -> ModeT -> PrimIO Bits32

%foreign "C:posix_spawn_file_actions_adddup2, libc, spawn.h"
prim__posix_spawn_file_actions_adddup2 : AnyPtr -> Bits32 -> Bits32 -> PrimIO Bits32

%foreign "C:posix_spawnattr_destroy, libc, spawn.h"
prim__posix_spawnattr_destroy : AnyPtr -> PrimIO Bits32

%foreign "C:posix_spawnattr_init, libc, spawn.h"
prim__posix_spawnattr_init : AnyPtr -> PrimIO Bits32

%foreign "C:li_posix_spawn, posix-idris"
prim__posix_spawn : String -> AnyPtr -> AnyPtr -> AnyPtr -> AnyPtr -> PrimIO CInt

%foreign "C:li_posix_spawnp, posix-idris"
prim__posix_spawnp : String -> AnyPtr -> AnyPtr -> AnyPtr -> AnyPtr -> PrimIO CInt

--------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------

%inline
Struct SSpawnAttrT where
  swrap = SA
  sunwrap = ptr

%inline
SizeOf SpawnAttrT where sizeof_ = posix_spawnattr_t_size

%inline
Struct SSpawnFileActionsT where
  swrap = SFA
  sunwrap = ptr

%inline
SizeOf SpawnFileActionsT where sizeof_ = posix_spawn_file_actions_t_size

export %inline
mkSpawnAttr : EPrim SpawnAttrT
mkSpawnAttr t =
  let a # t  := primStruct SSpawnAttrT t
      x # t := toF1 (prim__posix_spawnattr_init (unwrap a)) t
  in case x of
    0 => R a t
    x => freeFail a (EN x) t

export %inline
destroySpawnAttr : SpawnAttrT -> PrimIO ()
destroySpawnAttr sa w =
  let MkIORes _ w := prim__posix_spawnattr_destroy (unwrap sa) w
  in primRun (freeStruct1 sa) w

export %inline
mkSpawnFileActions : EPrim SpawnFileActionsT
mkSpawnFileActions t =
  let fa # t := primStruct SSpawnFileActionsT t
      x  # t := toF1 (prim__posix_spawn_file_actions_init (unwrap fa)) t
  in case x of
    0 => R fa t
    x => freeFail fa (EN x) t

export %inline
destroySpawnFileActions : SpawnFileActionsT -> PrimIO ()
destroySpawnFileActions fa w =
  let MkIORes _ w := prim__posix_spawn_file_actions_destroy (unwrap fa) w
  in primRun (freeStruct1 fa) w

export %inline
addClose : FileDesc a => SpawnFileActionsT -> a -> EPrim ()
addClose fa d =
  posToUnit $ prim__posix_spawn_file_actions_addclose (unwrap fa) (fileDesc d)

export %inline
addOpen : FileDesc a => SpawnFileActionsT -> a -> String -> Flags -> Mode -> EPrim ()
addOpen fa d path (F f) (M m) =
  posToUnit $ prim__posix_spawn_file_actions_addopen (unwrap fa) (fileDesc d) path f m

export %inline
addDup2 : FileDesc a => FileDesc b => SpawnFileActionsT -> a -> b -> EPrim ()
addDup2 fa d1 d2 =
  posToUnit $ prim__posix_spawn_file_actions_adddup2 (unwrap fa) (fileDesc d1) (fileDesc d2)

export %inline
spawn :
     String -> SpawnFileActionsT -> SpawnAttrT
  -> (args : CArrayIO m (Maybe String))
  -> (env  : CArrayIO n (Maybe String))
  -> EPrim PidT
spawn s (SFA acts) (SA attr) a e =
  toPidT $ prim__posix_spawn s acts attr (unsafeUnwrap a) (unsafeUnwrap e)

export
spawnl : String -> SpawnFileActionsT -> SpawnAttrT
       -> List String -> List (String,String) -> EPrim PidT
spawnl s acts attr a e t =
  let args # t := ioToF1 (fromList (map Just a ++ [Nothing])) t
      env  # t := ioToF1 (fromList (map envpair e ++ [Nothing])) t
      R res  t := spawn s acts attr args env t | E x t => E x t
      _    # t := free1 args t
      _    # t := free1 env t
   in R res t

  where
    envpair : (String,String) -> Maybe String
    envpair (n,v) = Just "\{n}=\{v}"

export %inline
spawnp :
     String -> SpawnFileActionsT -> SpawnAttrT
  -> (args : CArrayIO m (Maybe String))
  -> (env  : CArrayIO n (Maybe String))
  -> EPrim PidT
spawnp s (SFA acts) (SA attr) a e =
  toPidT $ prim__posix_spawnp s acts attr (unsafeUnwrap a) (unsafeUnwrap e)

export
spawnlp : String -> SpawnFileActionsT -> SpawnAttrT
       -> List String -> List (String,String) -> EPrim PidT
spawnlp s acts attr a e t =
  let args # t := ioToF1 (fromList (map Just a ++ [Nothing])) t
      env  # t := ioToF1 (fromList (map envpair e ++ [Nothing])) t
      R res  t := spawnp s acts attr args env t | E x t => E x t
      _    # t := free1 args t
      _    # t := free1 env t
   in R res t

  where
    envpair : (String,String) -> Maybe String
    envpair (n,v) = Just "\{n}=\{v}"
