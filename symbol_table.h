// Mike Araujo
// Stack of Binary Trees
// 10/3/2016
// http://www.thegeekstuff.com/2013/02/c-binary-tree/ code for 
// binary tree functions
// Credits to CSCI 132 for stack function code

#include <stdbool.h>
#include <stdio.h> /* load i/o routines */
#include <string.h>
#include <stdlib.h>
#include <ctype.h> /* load character test routines */
#include <stdio.h>


typedef struct bin_tree bin_tree;
typedef struct bin_tree node;
const int maxstack = 999;   //  small value for testing
int Error_code[4] = {1 /*success*/, 2 /*fail*/, 3 /*underflow*/, 4 /*overflow*/};
int count; // number of items in the stack

// define binary tree
struct sym_tab {
	char kind[20];
	char name[20];
	bin_tree* comp_type;  
	bin_tree* Ptype;
	bin_tree* next_param;
	char mode[20];
	int up_bound;
	int low_bound;
	int offset;
	int size;
	int reg_num;
	int start_num;
	int AR_size;
	int ival;
	int value;
};
struct bin_tree {
	struct sym_tab symbol;
   	struct bin_tree * right, * left;
};
// define stack elements
struct stack_tree {
	char *name;
	struct bin_tree * root;
	int AR_size;
};
// define symbol table elements

// declare the stack
struct stack_tree entry[999];

int  push(char *name)
  /*
Pre:  None.
Post: If the Stack is not full, item is added to the top
      of the Stack.  If the Stack is full,
      an Error_code of overflow is returned and the Stack is left unchanged.
  */

{
  int outcome = Error_code[0];
  if (count >= maxstack)
    outcome = Error_code[3];
  else
       entry[count].name = malloc(strlen(name) + 1);
       strcpy(entry[count].name, name);
       entry[count].root = NULL;
	   count++;
  return outcome;
};


int pop()
  /*
Pre:  None.
Post: If the Stack is not empty, the top of
      the Stack is removed.  If the Stack
      is empty, an Error_code of underflow is returned.
  */

{
  int outcome = Error_code[0];
  if (count == 0)
    outcome = Error_code[2];
  else --count;
  return outcome;
};

int size( ) 
{
  return count;
};


bool empty() 
  /*
Pre:  None.
Post: If the Stack is empty, true is returned.
      Otherwise false is returned.
  */

{
  bool outcome = true;
  if (count > 0) outcome = false;
  return outcome;
};


void Stack()
  /*
Pre:  None.
Post: The stack is initialized to be empty.
  */
{
  count = 0;
};

void insert(node ** tree, struct sym_tab data)
{
    node *temp = NULL;
	int cmp_array;
	int cmp_exception;
	int cmp_name_type;
	int cmp_param;
	int cmp_proc;
    if(!(*tree))
    {
        temp = (node *)malloc(sizeof(node));
        temp->left = temp->right = NULL;
        strcpy(temp->symbol.name, data.name);
		strcpy(temp->symbol.kind, data.kind);
		temp->symbol.Ptype = data.Ptype;
		temp->symbol.offset = data.offset;
		temp->symbol.size = data.size;
		cmp_array = strcmp(data.kind, "array");
		cmp_name_type = strcmp(data.kind, "named type");
		if(cmp_array == 0 || cmp_name_type == 0){
			temp->symbol.low_bound = data.low_bound;
			temp->symbol.up_bound = data.up_bound;
		}
		cmp_exception = strcmp(data.kind, "exception");
		if(cmp_exception == 0){
			temp->symbol.ival = data.ival;
		}
		cmp_param = strcmp(data.kind, "param");
		if(cmp_param == 0){
			strcpy(temp->symbol.mode, data.mode);
			temp->symbol.next_param = data.next_param;
		}
		cmp_proc = strcmp(data.kind, "procedure");
		if(cmp_proc == 0){
			temp->symbol.next_param = data.next_param;
		}
        *tree = temp;
        return;
    }
	int cmp = strcmp(data.name, (*tree)->symbol.name);
    
	if(cmp < 0)
    {
        insert(&(*tree)->left, data);
    }
    else if(cmp > 0)
    {
        insert(&(*tree)->right, data);
    }

};

// prints the elements in order from least to greatest
void print_inorder(node * tree)
{
    if (tree)
    {
        print_inorder(tree->left);
        printf("%s - %s ", tree->symbol.name, tree->symbol.kind);
		if(tree->symbol.Ptype != NULL){       
			printf(": w/ parent type %s", tree->symbol.Ptype->symbol.name);
		}
		printf("\n");
        print_inorder(tree->right);
    }
};
// searches the tree for a value, returns the root node of the value if it exists in tree else returns NULL				
node* search(node ** tree, char* data){
    if(!(*tree))
    {
        return NULL;	
    }
	int cmp = strcmp(data, (*tree)->symbol.name);
    if(cmp < 0)
    {
        search(&((*tree)->left), data);
    }
    else if(cmp > 0)
    {
        search(&((*tree)->right), data);
    }
    else if(cmp == 0)
    {
        return *tree;
		
    }
};

int duplicate(node *tree, char* data)
	{
		struct bin_tree* temp;
		temp = search(&tree, data);
		int dup = 0;
		if(temp != NULL){
			printf("ERROR Duplicate IDs\n");
			dup = 1;
		}
		return dup;
	}

bin_tree* name_search(char* data){
	int i = count-1;
	struct bin_tree* lookup = NULL;
	while(i >= 0){
		if(lookup == NULL){
			lookup = search(&entry[i].root, data);
			i--;
		} else {
			i--;
		}
	}
	return lookup;
}

int level_search(char* data){
	int i = count-1;
	int level = 0;
	struct bin_tree* lookup;
	lookup = search(&entry[i].root, data);
	i--;
	while(i > 0){
		if(lookup == NULL){
			lookup = search(&entry[i].root, data);
			level++;
			i--;
		} else {
			i--;
		}
	}
	return level;
}

int size_search(char* data){
	int i = count-1;
	struct bin_tree* lookup;
	while(i >= 0){
		if(lookup == NULL){
			lookup = search(&entry[i].root, data);
			i--;
		} else {
			i--;
		}
	}
	return lookup->symbol.size;
}
		


