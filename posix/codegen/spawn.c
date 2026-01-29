#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <spawn.h>

int main() {
  printf("\npublic export\n");
  printf("posix_spawnattr_t_size : Bits32\n");
  printf("posix_spawnattr_t_size = %zd\n", sizeof(posix_spawnattr_t));

  printf("\npublic export\n");
  printf("posix_spawn_file_actions_t_size : Bits32\n");
  printf("posix_spawn_file_actions_t_size = %zd\n",
         sizeof(posix_spawn_file_actions_t));

  exit(0);
}
