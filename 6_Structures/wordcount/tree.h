#ifndef TREE_H
#define TREE_H

/* The tree node structure */
struct tnode {
    char *word;
    int count;
    struct tnode *left;
    struct tnode *right;
};

/* Public function prototypes */
struct tnode *addtree(struct tnode *, char *);
void treeprint(struct tnode *);
void freetree(struct tnode *); /* Good practice to add a free function */

#endif