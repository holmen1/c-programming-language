// simple expand - convert tabs to spaces
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#define TABN 8 // DEFAULT
#define MAXLINELEN 120

static void detab_line(char *dst, const char *src, int tab_width) {
  int c, nspaces;
  int col = 1;
  while ((c = *src++) != '\n') {
    if (c == '\t') {
      nspaces = tab_width - ((col - 1) % tab_width);
      while (nspaces--) {
        *dst++ = ' ';
        col++;
      }
    } else {
      *dst++ = c;
      col++;
    }
  }
  *dst++ = '\n';
  *dst = '\0';
}

static void detab(FILE *fp, int tab_width) {
  char out[MAXLINELEN];
  char *buffer = NULL;
  size_t bufsize = 0; // Size of the buffer
  ssize_t nread;      // Number of characters read
  while ((nread = getline(&buffer, &bufsize, fp)) != -1) {
    if (nread > MAXLINELEN) {
      free(buffer);
      perror("getline");
    }
    detab_line(out, buffer, tab_width);
    fputs(out, stdout);
  }
  free(buffer);
}

int main(int argc, char *argv[]) {
  int opt, tabn = TABN;
  while ((opt = getopt(argc, argv, "t:")) != -1) {
    switch (opt) {
    case 't':
      tabn = atoi(optarg);
      break;
    default: /* '?' */
      fprintf(stderr, "Usage: %s [-t tabwidth] [file]\n", argv[0]);
      exit(EXIT_FAILURE);
    }
  }
  if (optind >= argc) {
    detab(stdin, tabn);
    fprintf(stderr, "Using stdin...\n");
  } else {
    FILE *fp = fopen(argv[optind], "r");
    if (fp == NULL) {
      perror("fopen");
      exit(EXIT_FAILURE);
    }
    detab(fp, tabn);
  }

  return 0;
}
