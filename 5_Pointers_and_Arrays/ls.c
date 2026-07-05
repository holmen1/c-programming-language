#include <dirent.h>
#include <stdio.h>
#include <sys/types.h>
#include <unistd.h>

void usage(char *program) {
  fprintf(stderr, "Usage: %s [-a] [path]\n", program);
}

int main(int argc, char *argv[]) {
  int opt;
  int all = 0;

  while ((opt = getopt(argc, argv, "a")) != -1) {
    switch (opt) {
    case 'a':
      all = 1;
      break;
    default:
      usage(argv[0]);
      return 1;
    }
  }

  char *name;
  if (argc == optind)
    name = ".";
  else if (argc == optind + 1)
    name = argv[optind];
  else {
    usage(argv[0]);
    return 1;
  }

  DIR *dir = opendir(name);
  if (!dir) {
    fprintf(stderr, "%s: Not a directory\n", name);
    return 1;
  }

  struct dirent *entry;
  while ((entry = readdir(dir)) != NULL) {
    if (*(*entry).d_name == '.' && !all)
      continue;
    printf("%s\n", (*entry).d_name);
  }

  closedir(dir);
  return 0;
}
