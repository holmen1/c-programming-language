#include <dirent.h>
#include <stdio.h>
#include <sys/types.h>
#include <unistd.h>

static void usage(char *program) {
  fprintf(stderr, "Usage: %s [-a] [path]\n", program);
}

static void _ls(char *name, int list_all) {
  DIR *dir = opendir(name);
  if (!dir)
    fprintf(stderr, "%s: Not a directory\n", name);

  struct dirent *entry;
  while ((entry = readdir(dir)) != NULL) {
    if (*(*entry).d_name == '.' && !list_all)
      continue;
    printf("%s\n", (*entry).d_name);
  }

  closedir(dir);
}

int main(int argc, char *argv[]) {
  int opt;

  opt = getopt(argc, argv, "a");
  if (opt == 'a') {
    if (argc == 2)
      _ls(".", 1);
    if (argc == 3)
      _ls(argv[optind], 1);
    else {
      usage(argv[0]);
      return 1;
    }
  } else if (opt == -1) {
    if (argc == 1)
      _ls(".", 0);
    if (argc == 2)
      _ls(argv[optind], 0);
    else {
      usage(argv[0]);
      return 1;
    }
  } else {
    usage(argv[0]);
    return 1;
  }

  return 0;
}
