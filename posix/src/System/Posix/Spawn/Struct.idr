module System.Posix.Spawn.Struct

import public Data.Linear.Token

%default total

||| Wrapped pointer to a `posix_spawnattr_t`
public export
record SSpawnAttrT (s : Type) where
  constructor SA
  ptr : AnyPtr

public export
0 SpawnAttrT : Type
SpawnAttrT = SSpawnAttrT World

||| Wrapped pointer to a `posix_spawn_file_actions_t`
public export
record SSpawnFileActionsT (s : Type) where
  constructor SFA
  ptr : AnyPtr

public export
0 SpawnFileActionsT : Type
SpawnFileActionsT = SSpawnFileActionsT World
