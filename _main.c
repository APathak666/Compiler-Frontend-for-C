#include <stdio.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
 
#define TEXTLEN 512
#define NSYMBOLS 1024   // Number of symbol table entries
 
enum {
  T_EOF,
  T_PLUS, T_MINUS,
  T_STAR, T_SLASH,
  T_EQ, T_NE,
  T_LT, T_GT, T_LE, T_GE,
  T_INTLIT, T_SEMI, T_ASSIGN, T_IDENT,
  T_LBRACE, T_RBRACE, T_LPAREN, T_RPAREN,
  // Keywords
  T_PRINT, T_INT, T_IF, T_ELSE, T_WHILE,
  T_FOR, T_READ, T_COMMA
};
 
// Token structure
struct token {
  int token;			// Token type, from the enum list above
  int intvalue;			// For T_INTLIT, the integer value
};
 
// AST node types. The first few line up
// with the related tokens
enum {
  A_ADD = 1, A_SUBTRACT, A_MULTIPLY, A_DIVIDE,
  A_EQ, A_NE, A_LT, A_GT, A_LE, A_GE,
  A_INTLIT,
  A_IDENT, A_LVIDENT, A_ASSIGN, A_PRINT, A_GLUE,
  A_IF, A_WHILE, A_READ, A_FOR
};
 
char* names[] = {"", "+", "-", "*", "/", "==", "!=", "<", ">", "<=", ">=", "literal", "identifier", "lval", "=", "write", "glue", "if", "for", "read"};
 
// Abstract Syntax Tree structure
struct ASTnode {
  int op;			// "Operation" to be performed on this tree
  struct ASTnode *left;		// Left, middle and right child trees
  struct ASTnode *mid;
  struct ASTnode *right;
  union {
    int intvalue;		// For A_INTLIT, the integer value
    int id;			// For A_IDENT, the symbol slot number
  } v;
};
 
#define NOREG	-1		// Use NOREG when the AST generation
				// functions have no register to return
 
// Symbol table structure
struct symtable {
  char *name;			// Name of a symbol
  int val;
};
 
int Line;		// Current line number
int Putback;		// Character put back by scanner
FILE *Infile;		// Input and output files
FILE *Outfile;
struct token Token;	// Last token scanned
char Text[TEXTLEN + 1];	// Last identifier scanned
struct symtable Gsym[NSYMBOLS];	// Global symbol table
int value[NSYMBOLS]={-1e9};
 
int scan(struct token *t);
 
// tree.c
struct ASTnode *mkastnode(int op, struct ASTnode *left,
			  struct ASTnode *mid,
			  struct ASTnode *right, int intvalue);
struct ASTnode *mkastleaf(int op, int intvalue);
struct ASTnode *mkastunary(int op, struct ASTnode *left, int intvalue);
 
// expr.c
struct ASTnode *binexpr(int ptp);
 
// stmt.c
struct ASTnode *compound_statement(void);
 
// misc.c
void match(int t, char *what);
void semi(void);
void lbrace(void);
void rbrace(void);
void lparen(void);
void rparen(void);
void ident(void);
void fatal(char *s);
void fatals(char *s1, char *s2);
void fatald(char *s, int d);
void fatalc(char *s, int c);
 
// sym.c
int findglob(char *s);
int addglob(char *name);
 
// decl.c
void var_declaration(void);
 
int first = 1;
int parity_first = 1;
 
static int chrpos(char *s, int c) {
  char *p;
 
  p = strchr(s, c);
  return (p ? p - s : -1);
}
 
// Get the next character from the input file.
static int next(void) {
  int c;
 
  if (Putback) {		// Use the character put
    c = Putback;		// back if there is one
    Putback = 0;
    return (c);
  }
 
  c = fgetc(Infile);		// Read from input file
  if ('\n' == c)
    Line++;			// Increment line count
  return (c);
}
 
// Put back an unwanted character
static void putback(int c) {
  Putback = c;
}
 
// Skip past input that we don't need to deal with, 
// i.e. whitespace, newlines. Return the first
// character we do need to deal with.
static int skip(void) {
  int c;
 
  c = next();
  while (' ' == c || '\t' == c || '\n' == c || '\r' == c || '\f' == c) {
    c = next();
  }
  return (c);
}
 
// Scan and return an integer literal
// value from the input file. Store
// the value as a string in Text.
static int scanint(int c) {
  int k, val = 0;
 
  // Convert each character into an int value
  while ((k = chrpos("0123456789", c)) >= 0) {
    val = val * 10 + k;
    c = next();
  }
 
  // We hit a non-integer character, put it back.
  putback(c);
  return (val);
}
 
// Scan an identifier from the input file and
// store it in buf[]. Return the identifier's length
static int scanident(int c, char *buf, int lim) {
  int i = 0;
 
  // Allow digits, alpha and underscores
  while (isalpha(c) || isdigit(c) || '_' == c) {
    // Error if we hit the identifier length limit,
    // else append to buf[] and get next character
    if (lim - 1 == i) {
      fatal("Identifier too long");
    } else if (i < lim - 1) {
      buf[i++] = c;
    }
    c = next();
  }
  // We hit a non-valid character, put it back.
  // NUL-terminate the buf[] and return the length
  putback(c);
  buf[i] = '\0';
  return (i);
}
 
// Given a word from the input, return the matching
// keyword token number or 0 if it's not a keyword.
// Switch on the first letter so that we don't have
// to waste time strcmp()ing against all the keywords.
static int keyword(char *s) {
  switch (*s) {
    case 'e':
      if (!strcmp(s, "else"))
	return (T_ELSE);
      break;
    case 'f':
      if (!strcmp(s, "for"))
	return (T_FOR);
      break;
    case 'i':
      if (!strcmp(s, "if"))
	return (T_IF);
      if (!strcmp(s, "int"))
	return (T_INT);
      break;
    case 'w':
      if (!strcmp(s, "write"))
	return (T_PRINT);
      if (!strcmp(s, "while"))
	return (T_WHILE);
      break;
    case 'r':
      if (!strcmp(s, "read"))
        return T_READ;
  }
  return (0);
}
 
// Scan and return the next token found in the input.
// Return 1 if token valid, 0 if no tokens left.
int scan(struct token *t) {
  int c, tokentype;
 
  // Skip whitespace
  c = skip();
 
  // Determine the token based on
  // the input character
  switch (c) {
    case EOF:
      t->token = T_EOF;
      return (0);
    case '+':
      t->token = T_PLUS;
      break;
    case '-':
      t->token = T_MINUS;
      break;
    case '*':
      t->token = T_STAR;
      break;
    case '/':
      t->token = T_SLASH;
      break;
    case ';':
      t->token = T_SEMI;
      break;
    case ',':
      t->token = T_COMMA;
      break;
    case '{':
      t->token = T_LBRACE;
      break;
    case '}':
      t->token = T_RBRACE;
      break;
    case '(':
      t->token = T_LPAREN;
      break;
    case ')':
      t->token = T_RPAREN;
      break;
    case '=':
      if ((c = next()) == '=') {
	t->token = T_EQ;
      } else {
	putback(c);
	t->token = T_ASSIGN;
      }
      break;
    case '!':
      if ((c = next()) == '=') {
	t->token = T_NE;
      } else {
	fatalc("Unrecognised character", c);
      }
      break;
    case '<':
      if ((c = next()) == '=') {
	t->token = T_LE;
      } else {
	putback(c);
	t->token = T_LT;
      }
      break;
    case '>':
      if ((c = next()) == '=') {
	t->token = T_GE;
      } else {
	putback(c);
	t->token = T_GT;
      }
      break;
    default:
 
      // If it's a digit, scan the
      // literal integer value in
      if (isdigit(c)) {
	t->intvalue = scanint(c);
	t->token = T_INTLIT;
	break;
      } else if (isalpha(c) || '_' == c) {
	// Read in a keyword or identifier
	scanident(c, Text, TEXTLEN);
 
	// If it's a recognised keyword, return that token
	if (tokentype = keyword(Text)) {
	  t->token = tokentype;
	  break;
	}
	// Not a recognised keyword, so it must be an identifier
	t->token = T_IDENT;
	break;
      }
      // The character isn't part of any recognised token, error
      fatalc("Unrecognised character", c);
  }
 
  // We found a token
  return (1);
}
 
static struct ASTnode *primary(void) {
  struct ASTnode *n;
  int id;
 
  switch (Token.token) {
    case T_INTLIT:
      // For an INTLIT token, make a leaf AST node for it.
      n = mkastleaf(A_INTLIT, Token.intvalue);
      break;
 
    case T_IDENT:
      // Check that this identifier exists
      id = findglob(Text);
      if (id == -1)
	fatals("Unknown variable", Text);
      // Make a leaf AST node for it
      n = mkastleaf(A_IDENT, id);
      break;
 
    default:
      fatald("Syntax error, token", Token.token);
  }
 
  // Scan in the next token and return the leaf node
  scan(&Token);
  return (n);
}
 
 
// Convert a binary operator token into an AST operation.
// We rely on a 1:1 mapping from token to AST operation
static int arithop(int tokentype) {
  if (tokentype > T_EOF && tokentype < T_INTLIT)
    return (tokentype);
  fatald("Syntax error, token", tokentype);
}
 
// Operator precedence for each token. Must
// match up with the order of tokens in defs.h
static int OpPrec[] = {
  0, 10, 10,			// T_EOF, T_PLUS, T_MINUS
  20, 20,			// T_STAR, T_SLASH
  30, 30,			// T_EQ, T_NE
  40, 40, 40, 40		// T_LT, T_GT, T_LE, T_GE
};
 
// Check that we have a binary operator and
// return its precedence.
static int op_precedence(int tokentype) {
  int prec = OpPrec[tokentype];
  if (prec == 0)
    fatald("Syntax error, token", tokentype);
  return (prec);
}
 
// Return an AST tree whose root is a binary operator.
// Parameter ptp is the previous token's precedence.
struct ASTnode *binexpr(int ptp) {
  struct ASTnode *left, *right;
  int tokentype;
 
  // Get the primary tree on the left.
  // Fetch the next token at the same time.
  left = primary();
 
  // If we hit a semicolon or ')', return just the left node
  tokentype = Token.token;
  if (tokentype == T_SEMI || tokentype == T_RPAREN)
    return (left);
 
  // While the precedence of this token is
  // more than that of the previous token precedence
  while (op_precedence(tokentype) > ptp) {
    // Fetch in the next integer literal
    scan(&Token);
 
    // Recursively call binexpr() with the
    // precedence of our token to build a sub-tree
    right = binexpr(OpPrec[tokentype]);
 
    // Join that sub-tree with ours. Convert the token
    // into an AST operation at the same time.
    left = mkastnode(arithop(tokentype), left, NULL, right, 0);
 
    // Update the details of the current token.
    // If we hit a semicolon or ')', return just the left node
    tokentype = Token.token;
    if (tokentype == T_SEMI || tokentype == T_RPAREN)
      return (left);
  }
 
  // Return the tree we have when the precedence
  // is the same or lower
  return (left);
}
 
static struct ASTnode *single_statement(void);
 
// compound_statement:          // empty, i.e. no statement
//      |      statement
//      |      statement statements
//      ;
//
// statement: print_statement
//      |     declaration
//      |     assignment_statement
//      |     if_statement
//      |     while_statement
//      ;
 
// print_statement: 'print' expression ';'  ;
//
 
void var_declaration(void) {
 
  // Ensure we have an 'int' token followed by an identifier
  // and a semicolon. Text now has the identifier's name.
  // Add it as a known identifier
  match(T_INT, "int");
  ident();
  addglob(Text);
  while(Token.token == T_COMMA)
  {
    scan(&Token);
    addglob(Text);
    scan(&Token);
  }
  semi();
}
 
static struct ASTnode *read_statement(void) {
  struct ASTnode *tree;
  int id;
  // Match a 'print' as the first token
  match(T_READ, "read");
 
  if ((id = findglob(Text)) == -1) {
    fatals("Undeclared variable", Text);
  }
  // Parse the following expression
  
  tree = binexpr(0);
 
  // Make an print AST tree
  tree = mkastunary(A_READ, tree, 0);
  
  // Return the AST
  return (tree);
}
 
static struct ASTnode *print_statement(void) {
  struct ASTnode *tree;
  int id;
  // Match a 'print' as the first token
  match(T_PRINT, "write");
 
  if ((id = findglob(Text)) == -1) {
    fatals("Undeclared variable", Text);
  }
  // Parse the following expression
  tree = binexpr(0);
 
  // Make an print AST tree
  tree = mkastunary(A_PRINT, tree, 0);
 
  // Return the AST
  return (tree);
}
 
// assignment_statement: identifier '=' expression ';'   ;
//
static struct ASTnode *assignment_statement(void) {
  struct ASTnode *left, *right, *tree;
  int id;
 
  // Ensure we have an identifier
  ident();
 
  // Check it's been defined then make a leaf node for it
  if ((id = findglob(Text)) == -1) {
    fatals("Undeclared variable", Text);
  }
  
  right = mkastleaf(A_IDENT, id);
 
  // Ensure we have an equals sign
  match(T_ASSIGN, "=");
 
  // Parse the following expression
  left = binexpr(0);
  value[id]=left->v.intvalue;
 
  // Make an assignment AST tree
  tree = mkastnode(A_ASSIGN, left, NULL, right, 0);
 
  // Return the AST
  return (tree);
}
 
 
// if_statement: if_head
//      |        if_head 'else' compound_statement
//      ;
//
// if_head: 'if' '(' true_false_expression ')' compound_statement  ;
//
// Parse an IF statement including
// any optional ELSE clause
// and return its AST
static struct ASTnode *if_statement(void) {
  struct ASTnode *condAST, *trueAST, *falseAST = NULL;
 
  // Ensure we have 'if' '('
  match(T_IF, "if");
  lparen();
 
  // Parse the following expression
  // and the ')' following. Ensure
  // the tree's operation is a comparison.
  condAST = binexpr(0);
  if (condAST->op < A_EQ || condAST->op > A_GE)
    fatal("Bad comparison operator");
  rparen();
 
  // Get the AST for the compound statement
  trueAST = compound_statement();
 
  // If we have an 'else', skip it
  // and get the AST for the compound statement
  if (Token.token == T_ELSE) {
    scan(&Token);
    falseAST = compound_statement();
  }
  // Build and return the AST for this statement
  return (mkastnode(A_IF, condAST, trueAST, falseAST, 0));
}
 
 
// while_statement: 'while' '(' true_false_expression ')' compound_statement  ;
//
// Parse a WHILE statement
// and return its AST
static struct ASTnode *while_statement(void) {
  struct ASTnode *condAST, *bodyAST;
 
  // Ensure we have 'while' '('
  match(T_WHILE, "while");
  lparen();
 
  // Parse the following expression
  // and the ')' following. Ensure
  // the tree's operation is a comparison.
  condAST = binexpr(0);
  if (condAST->op < A_EQ || condAST->op > A_GE)
    fatal("Bad comparison operator");
  rparen();
 
  // Get the AST for the compound statement
  bodyAST = compound_statement();
 
  // Build and return the AST for this statement
  return (mkastnode(A_WHILE, condAST, NULL, bodyAST, 0));
}
 
// for_statement: 'for' '(' preop_statement ';'
//                          true_false_expression ';'
//                          postop_statement ')' compound_statement  ;
//
// preop_statement:  statement          (for now)
// postop_statement: statement          (for now)
//
// Parse a FOR statement
// and return its AST
static struct ASTnode *for_statement(void) {
  struct ASTnode *condAST, *bodyAST;
  struct ASTnode *preopAST, *postopAST;
  struct ASTnode *tree;
 
  // Ensure we have 'for' '('
  match(T_FOR, "for");
  lparen();
 
  // Get the pre_op statement and the ';'
  preopAST = single_statement();
  semi();
 
  // Get the condition and the ';'
  condAST = binexpr(0);
  if (condAST->op < A_EQ || condAST->op > A_GE)
    fatal("Bad comparison operator");
  semi();
 
  // Get the post_op statement and the ')'
  postopAST = single_statement();
  rparen();
 
  // Get the compound statement which is the body
  bodyAST = compound_statement();
 
  // For now, all four sub-trees have to be non-NULL.
  // Later on, we'll change the semantics for when some are missing
 
  // Glue the compound statement and the postop tree
  tree = mkastnode(A_GLUE, bodyAST, NULL, postopAST, 0);
 
  // Make a WHILE loop with the condition and this new body
  tree = mkastnode(A_WHILE, condAST, NULL, tree, 0);
 
  // And glue the preop tree to the A_WHILE tree
  return (mkastnode(A_GLUE, preopAST, NULL, tree, 0));
}
 
struct ASTnode* literal()
{
    struct ASTnode* ret = binexpr(0);
    semi();
 
    return ret;
}
 
int done = 0;
// Parse a single statement
// and return its AST
static struct ASTnode *single_statement(void) {
  switch (Token.token) {
    case T_PRINT:
      return (print_statement());
    case T_READ:
      return (read_statement());
    case T_INT:
      var_declaration();
      return (NULL);		// No AST generated here
    case T_IDENT:
      return (assignment_statement());
    case T_IF:
      return (if_statement());
    case T_WHILE:
      return (while_statement());
    case T_FOR:
      return (for_statement());
    case T_INTLIT:
        return (literal());
    case T_EOF:
        done = 1;
        return NULL;
    default:
      fatald("Syntax error, token", Token.token);
  }
}
 
// Parse a compound statement
// and return its AST
struct ASTnode *compound_statement(void) {
  struct ASTnode *left = NULL;
  struct ASTnode *tree;
 
  // Require a left curly bracket
       lbrace();
  while (1) {
 
    // Parse a single statement
    tree = single_statement();
    
    // Some statements must be followed by a semicolon
    if (tree != NULL && (tree->op == A_PRINT || tree->op == A_READ || tree->op == A_GLUE || tree->op == A_ASSIGN || tree->op == A_INTLIT))
      semi();
    // For each new tree, either save it in left
    // if left is empty, or glue the left and the
    // new tree together
    
    if (tree != NULL) {
      
      if (left == NULL)
	left = tree;
      else
	left = mkastnode(A_GLUE, left, NULL, tree, 0);
    }
 
    else if (tree == NULL && done && left != NULL)
    {
        return (left);
    }
    // When we hit a right curly bracket,
    // skip past it and return the AST
    
    if (Token.token == T_RBRACE) {
      if(tree && tree->op == A_GLUE)
        printf("%s", names[Token.token]);
      rbrace();
      return (left);
    }
    
  }
}
 
static int Globs = 0;		// Position of next free global symbol slot
 
// Determine if the symbol s is in the global symbol table.
// Return its slot position or -1 if not found.
int findglob(char *s) {
  int i;
 
  for (i = 0; i < Globs; i++) {
    if (*s == *Gsym[i].name && !strcmp(s, Gsym[i].name))
      return (i);
  }
  return (-1);
}
 
// Get the position of a new global symbol slot, or die
// if we've run out of positions.
static int newglob(void) {
  int p;
 
  if ((p = Globs++) >= NSYMBOLS)
    fatal("Too many global symbols");
  return (p);
}
 
// Add a global symbol to the symbol table.
// Return the slot number in the symbol table
int addglob(char *name) {
  int y;
 
  // If this is already in the symbol table, return the existing slot
  if ((y = findglob(name)) != -1)
    return (y);
 
  // Otherwise get a new slot, fill it in and
  // return the slot number
  y = newglob();
  Gsym[y].name = strdup(name);
  return (y);
}
 
struct ASTnode *mkastnode(int op, struct ASTnode *left,
			  struct ASTnode *mid,
			  struct ASTnode *right, int intvalue) {
  struct ASTnode *n;
 
  // Malloc a new ASTnode
  n = (struct ASTnode *) malloc(sizeof(struct ASTnode));
  if (n == NULL)
    fatal("Unable to malloc in mkastnode()");
 
  // Copy in the field values and return it
  n->op = op;
  n->left = left;
  n->mid = mid;
  n->right = right;
  n->v.intvalue = intvalue;
 
    // printf("AST node operand: %d | %s\n", n->op, names[n->op]);
 
    // if (n->op == A_INTLIT)
    //     printf("AST node intlit value: %d\n", n->v.intvalue);
 
  return (n);
}
 
// Make an AST leaf node
struct ASTnode *mkastleaf(int op, int intvalue) {
  return (mkastnode(op, NULL, NULL, NULL, intvalue));
}
 
// Make a unary AST node: only one child
struct ASTnode *mkastunary(int op, struct ASTnode *left, int intvalue) {
  return (mkastnode(op, left, NULL, NULL, intvalue));
}
 
void match(int t, char *what) {
  if (Token.token == t) {
    scan(&Token);
  } else {
    fatals("Expected", what);
  }
}
 
// Match a semicolon and fetch the next token
void semi(void) {
  match(T_SEMI, ";");
}
 
// Match a left brace and fetch the next token
void lbrace(void) {
    if (first == 1)
    {
        first = 0;
        return;
    }
  match(T_LBRACE, "{");
}
 
// Match a right brace and fetch the next token
void rbrace(void) {
 
  match(T_RBRACE, "}");
}
 
// Match a left parenthesis and fetch the next token
void lparen(void) {
  match(T_LPAREN, "(");
}
 
// Match a right parenthesis and fetch the next token
void rparen(void) {
  match(T_RPAREN, ")");
}
 
// Match an identifier and fetch the next token
void ident(void) {
  match(T_IDENT, "identifier");
}
 
// Print out fatal messages
void fatal(char *s) {
  fprintf(stderr, "%s on line %d\n", s, Line);
  exit(1);
}
 
void fatals(char *s1, char *s2) {
  fprintf(stderr, "%s:%s on line %d\n", s1, s2, Line);
  exit(1);
}
 
void fatald(char *s, int d) {
  fprintf(stderr, "%s:%d on line %d\n", s, d, Line);
  exit(1);
}
 
void fatalc(char *s, int c) {
  fprintf(stderr, "%s:%c on line %d\n", s, c, Line);
  exit(1);
}
 
static void init() {
  Line = 1;
  Putback = '\n';
}
 
// Print out a usage if started incorrectly
static void usage(char *prog) {
  fprintf(stderr, "Usage: %s infile\n", prog);
  exit(1);
}
 
// Main program: check arguments and print a usage
// if we don't have an argument. Open up the input
// file and call scanfile() to scan the tokens in it.
 
void print_tree(struct ASTnode* root)
{
    if(root==NULL)
        return ;
    struct ASTnode* left, right;
    fprintf(Outfile, "[%s ", names[root->op]);
    if (!strcmp(names[root->op], "literal") || !strcmp(names[root->op], "intlit"))
        fprintf(Outfile, "%d ", root->v.intvalue);
 
    else if (!strcmp(names[root->op], "identifier"))
        fprintf(Outfile, "%s", Gsym[root->v.intvalue].name);
 
    print_tree(root->left);
    print_tree(root->right);
    fprintf(Outfile, "]");
}
 
void take_read(struct ASTnode* root){
  if(root==NULL)
    return;
  if(!strcmp(names[root->op], "read")){
      int x;
      scanf("%d", &x);
      value[root->left->v.id]=x;
  }
  take_read(root->left);
  take_read(root->right);
}
 
void print_write(struct ASTnode* root){
  if(root==NULL)
    return;
  if(!strcmp(names[root->op], "write")){
    printf("%d\n", value[root->left->v.id]);
  }
  print_write(root->left);
  print_write(root->right);
}
 
void main(int argc, char *argv[]) {
  struct ASTnode *tree;
  
  if (argc != 2)
    usage(argv[0]);
 
  init();
  Outfile = fopen("output.txt", "w+");
  // Open up the input file
  if ((Infile = fopen(argv[1], "r")) == NULL) {
    fprintf(stderr, "Unable to open %s: %s\n", argv[1], strerror(errno));
    exit(1);
  }
  // Create the output file
//   if ((Outfile = fopen("out.s", "w")) == NULL) {
//     fprintf(stderr, "Unable to create out.s: %s\n", strerror(errno));
//     exit(1);
//   }
 
  scan(&Token);
  tree = compound_statement();	// Parse the compound statement in the input
//   fclose(Outfile);		// Close the output file and exit
 
//   for (int i = 0; i < Globs; i++)
//   {
//       printf("%s ", Gsym[i].name);
//   }
    print_tree(tree);
    printf("-----------------Executing your code------------------\n");
    take_read(tree);
    print_write(tree);
  exit(0);
}
 