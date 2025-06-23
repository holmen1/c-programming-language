#include "getword.h"
#include "tree.h"
#include <stdio.h>
#include <ctype.h>

#define MAXWORD 100

/* word frequency count */
int main(void)
{
  struct tnode *root;
  char word[MAXWORD];

  root = NULL;
  while (getword(word,MAXWORD) != EOF)
    if (isalpha(word[0]))
      root = addtree(root, word);
  treeprint(root);
  freetree(root); /* Free the allocated memory */
  return 0;
}