module System.Posix.Spawn

import public Control.Monad.Resource
import public Control.Monad.MCancel

import public System.Posix.Spawn.Prim as P

%default total

||| Destroy a SpawnFileActionsT.
||| Runs `posix_spawn_file_acitions_destroy`, before freeing memory.
export %inline
destroySpawnFileActions : EIO1 f => SpawnFileActionsT -> f es ()
destroySpawnFileActions fa = elift1 $ f1ToE1 $ ffi (P.destroySpawnFileActions fa)

||| Destroy a SpawnAttrT.
||| Runs `posix_spawnattr_destroy`, before freeing memory.
export %inline
destroySpawnAttr : EIO1 f => SpawnAttrT -> f es ()
destroySpawnAttr sa = elift1 $ f1ToE1 $ ffi (P.destroySpawnAttr sa)

export %inline
ELift1 World f => Resource f SpawnFileActionsT where cleanup = destroySpawnFileActions

export %inline
ELift1 World f => Resource f SpawnAttrT where cleanup = destroySpawnAttr

parameters {auto has : Has Errno es}
           {auto eio : EIO1 f}

  ||| Allocate and initialize a new SpawnFileActionsT.
  export %inline
  mkSpawnFileActions : f es SpawnFileActionsT
  mkSpawnFileActions = elift1 P.mkSpawnFileActions

  ||| Allocate and initialize a new SpawnAttrT.
  export %inline
  mkSpawnAttr : f es SpawnAttrT
  mkSpawnAttr = elift1 P.mkSpawnAttr

  ||| Close the file descriptor `fd` when spawning the new process.
  export %inline
  addClose : FileDesc a => SpawnFileActionsT -> a -> f es ()
  addClose fa d = elift1 $ P.addClose fa d

  ||| Open the file at path `p` with file descriptor `d` when spawning the new
  ||| process.
  export %inline
  addOpen : FileDesc a => SpawnFileActionsT -> a -> String -> Flags -> Mode -> f es ()
  addOpen fa d p f m = elift1 $ P.addOpen fa d p f m

  ||| Duplicate the file descriptor `d1` to `d2`.
  export %inline
  addDup2 : FileDesc a => FileDesc b => SpawnFileActionsT -> a -> b -> f es ()
  addDup2 fa d1 d2 = elift1 $ P.addDup2 fa d1 d2

  ||| Spawn an external program at the absolute path `prog`, performing the file
  ||| descriptor actions specified in `fa` in the new process, and setting
  ||| process attributes according to `sa`.
  export %inline
  spawn : String -> SpawnFileActionsT -> SpawnAttrT
       -> List String -> List (String,String) -> f es PidT
  spawn prog fa sa args env = elift1 $ P.spawnl prog fa sa args env

  ||| Spawn an external program named `prog`, performing the file descriptor
  ||| actions specified in `fa` in the new process, and setting process
  ||| attributes according to `sa`.
  export %inline
  spawnp : String -> SpawnFileActionsT -> SpawnAttrT
       -> List String -> List (String,String) -> f es PidT
  spawnp prog fa sa args env = elift1 $ P.spawnlp prog fa sa args env
