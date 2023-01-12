#include <stdio.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define TEXTLEN 512
#define NSYMBOLS 1024   // Number of symbol table entries

struct token
{
    int token;
    int intVal;
};

struct ASTNode
{
    int operation;
    struct ASTNode* left;
    struct ASTNode* right;
    union {
        int intvalue;		// For A_INTLIT, the integer value
        int id;			// For A_IDENT, the symbol slot number
    } v;

};

struct symtable {
  char *name;                   // Name of a symbol
};

int scan(struct token *t);
// stmt.c
void statements(void);

// misc.c
void match(int t, char *what);
void stmtEnd();
void var_declaration(void);
int findglob(char *s);

enum
{
    EOF_T, PLUS_T, MINUS_T, MULT_T, DIV_T, EQ_T, LT_T, GT_T, INTLIT_T, SEMI_T, ASSIGN_T, IDENT_T, WRITE_T, INT_T, READ_T, LVIDENT_T
};

enum
{
    PLUS_A=1, MINUS_A, MULT_A, DIV_A, EQ_A, LT_A, GT_A, INTLIT_A, IDENT_A, LVIDENT_A, ASSIGN_A
};

int Line;
int Putback;
FILE *Infile;
struct token Token;
char Text[TEXTLEN + 1];	
struct symtable Gsym[NSYMBOLS];     // Global symbol table
int arr[NSYMBOLS]={0};              //take pos from id of union to get the value
static int Globs = 0;               // Position of next free global symbol slot

char *tokstr[] = { "+", "-", "*", "/", ">", "<", "==", "intlit" };
static int OpPrec[] = { 0, 10, 10, 20, 20, 5, 5, 5, 0 };
//                     EOF  +   -   *   /  > < == INTLIT


static int getNext()
{
    int c;

    if (Putback)
    {
        c = Putback;
        Putback = 0;
        return c;
    }

    c = fgetc(Infile);
    if (c == '\n') Line++;

    return c;
}

static void putback(int c)
{
    Putback = c;
}

static int skip()
{
    int c;
    c = getNext();

    while(c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == '\f') c = getNext();

    return c;
}

static int chrpos(char *s, int c) {
  char *p;

  p = strchr(s, c);
  return (p ? p - s : -1);
}

int scanint(int c)
{
    int k, final = 0;

    while ((k = chrpos("0123456789", c)) >= 0)
    {
        final = 10*final + k;
        c = getNext();
    }

    putback(c);
    return final;
}

static int scanident(int c, char *buf, int lim) {
    int i = 0;

  // Allow digits, alpha and underscores
    while (isalpha(c) || isdigit(c) || '_' == c) {
    // Error if we hit the identifier length limit,
    // else append to buf[] and get next character
        if (lim - 1 == i) {
            printf("identifier too long on line %d\n", Line);
            exit(1);
        } else if (i < lim - 1) {
            buf[i++] = c;
        }
        c = getNext();
    }
  // We hit a non-valid character, put it back.
  // NUL-terminate the buf[] and return the length
    putback(c);
    buf[i] = '\0';
    return (i);
}

static int keyword(char *s)
{
    if (s[0] == 'w')
    {
        if (!strcmp(s, "write"))
            return WRITE_T;
    }

    else if (s[0] == 'r')
    {
        if (!strcmp(s, "read"))
            return READ_T;
    }

    else if (s[0] == 'i')
    {
        if (!strcmp(s, "int"));
            return INT_T;
    }

    return 0;
}

int scan(struct token* t)
{
    int c, tokenType;

    c = skip();

    if (c == EOF)
    {
        t->token = EOF_T;
        return 0;
    }

    else if (c == '+')
        t->token = PLUS_T;

    else if (c == '-')
        t->token = MINUS_T;

    else if (c == '/')
        t->token = DIV_T;

    else if (c == '*')
        t->token = MULT_T;

    else if (isdigit(c))
    {
        t->intVal = scanint(c);
        t->token = INTLIT_T;
    }

    else if (c == ';')
        t->token = SEMI_T;

    else if (c == '=')
        if((c==getNext())== '=')
        t->token= EQ_T;
        else
        t->token = ASSIGN_T;
    
    else if (c == '>')
        t->token = GT_T;

    else if (c == '<')
        t->token = LT_T;
    
    else
    {
        if (isdigit(c))
        {
            t->intVal = scanint(c);
            t->token = INTLIT_T;

            return 1;
        }
        else if (isalpha(c))
        {
            scanident(c, Text, TEXTLEN);
            if (tokenType = keyword(Text))
            {
                t->token = tokenType;
                return 1;
            }

            t->token = IDENT_T;
            return 1;
        }

        printf("Unrecognised character %c on line %d\n", c, Line);
        exit(1);
    }

    return 1;
}

static void usage(char *prog) {
    fprintf(stderr, "Usage: %s infile\n", prog);
    exit(1);
}

static void scanfile() {
    struct token T;

    while (scan(&T)) {
        printf("Token %s", tokstr[T.token]);
        if (T.token == INTLIT_T)
            printf(", value %d", T.intVal);
        printf("\n");
  }
}

struct ASTNode* makeASTNode(int operation, struct ASTNode* left, struct ASTNode* right, int intVal)
{
    struct ASTNode *n;

  // Malloc a new ASTnode
    n = (struct ASTNode* ) malloc(sizeof(struct ASTNode));

    if (n == NULL) {
        fprintf(stderr, "Unable to malloc in mkastnode()\n");
        exit(1);
    }

  // Copy in the field values and return it
    n->operation = operation;
    n->left = left;
    n->right = right;
    n->v.intvalue = intVal;
    return n;
}

struct ASTNode* makeLeaf(int operation, int val)
{
    return makeASTNode(operation, NULL, NULL, val);
}

struct ASTNode* makeUnaryNode(int operation, struct ASTNode* left, int val)
{
    return makeASTNode(operation, left, NULL, val);
}

int op(int token)
{
    if (token == PLUS_T)
        return PLUS_A;

    else if (token == MINUS_T)
        return MINUS_A;

    else if (token == DIV_T)
        return DIV_A;

    else if (token == MULT_T)
        return MULT_A;

    else if (token == EQ_T)
        return EQ_A;

    else if (token == LT_T)
        return LT_A;

    else if (token == GT_T)
        return GT_A;
    
    else if (token == LVIDENT_T)
        return LVIDENT_A;
    
    else if (token == ASSIGN_T)
        return ASSIGN_A;

    else
    {
        printf("%d unknown token in arithop() on line %d\n", token, Line);
        exit(1);
    }
}

struct ASTNode* buildLeft()
{
    struct ASTNode* n;
    int id;
    
    if (Token.token == INTLIT_T)
        n = makeLeaf(INTLIT_A, Token.intVal);

    else if (Token.token == IDENT_T)
    {
        if (findglob(Text) == -1)
        {
            printf("Unknown variable: %s\n", Text);
            exit(1);
        }
        n = makeLeaf(IDENT_A, id);
    }

    else
    {
        printf("Syntax error on line %d\n", Line);
        exit(1);
    }

    scan(&Token);
    return n;
}

struct ASTNode* buildAST()
{

    struct ASTNode* left, *right, *n;
    int nodeType;

    left = buildLeft();

    if (Token.token == EOF_T)
        return(left);

    nodeType = op(Token.token);
    scan(&Token);
    right = buildAST();

    n = makeASTNode(nodeType, left, right, 0);

    return n;
}

int interpretAST(struct ASTNode *n) {    
  int leftval, rightval;
    int a,b;
  // Get the left and right sub-tree values
  if (n->left)
    leftval = interpretAST(n->left);
  if (n->right)
    rightval = interpretAST(n->right);
  switch (n->operation) {
    case PLUS_A:
      return (leftval + rightval);
    case MINUS_A:
      return (leftval - rightval);
    case MULT_A:
      return (leftval * rightval);
    case DIV_A:
      return (leftval / rightval);
    case INTLIT_A:
      return (n->v.intvalue);
    case IDENT_A:
        return (arr[n->v.intvalue]);
    case LT_A:
      return (leftval < rightval ? 1 : 0);
    case GT_A:
      return (leftval > rightval ? 1 : 0);
    case EQ_A:
      return (leftval == rightval ? 1 : 0);
    
    default:
      printf("Unknown AST operator %d\n", n->operation);
      exit(1);
  }
}

static int op_precedence(int tokentype)
{
    int prec = OpPrec[tokentype];
    if (prec == 0)
    {
        fprintf(stderr, "syntax error on line %d, token %d\n", Line, tokentype);
        exit(1);
    }

    return (prec);
}

struct ASTNode* PrattParser(int prev)
{
    struct ASTNode* left, *right;
    int tokenType;

    left = buildLeft();
    tokenType = Token.token;

    if (tokenType == SEMI_T)
        return left;

    while(op_precedence(tokenType) > prev)
    {
        scan(&Token);

        right = PrattParser(OpPrec[tokenType]);
        left = makeASTNode(op(tokenType), left, right, 0);

        tokenType = Token.token;

        if (tokenType == SEMI_T)
            return left;
    }

    return left;
}

void match(int i, char* expr)
{
    if (Token.token == i) {
        scan(&Token);
    } else {
        printf("%s expected on line %d\n", expr, Line);
        exit(1);
    }
}

void stmtEnd()
{
    match(SEMI_T, ";");
}

void ident(void) {
  match(IDENT_T, "identifier");
}

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
    printf("Too many global symbols");
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

void var_declaration(void) {

    // Ensure we have an 'int' token followed by an identifier
    // and a semicolon. Text now has the identifier's name.
    // Add it as a known identifier
    match(INT_T, "int");
    ident();
    addglob(Text);
    stmtEnd();
}

void print_statement()
{
    match(WRITE_T, "write");
    struct ASTNode* tree = PrattParser(0);

    if(tree->operation == INTLIT_A){
        printf("%d",tree->v.intvalue);
    }
    else{
        printf("%d",arr[tree->v.intvalue]);
    }

    stmtEnd();
}

void read_statement()
{
    match(READ_T, "read");
    struct ASTNode* tree = PrattParser(0);

    scanf("%d",&arr[tree->v.intvalue]);
    stmtEnd();
}

void assignment_statement(void) {
    struct ASTNode *left, *right, *tree;
    int id;

    // Ensure we have an identifier
    ident();

    // Check it's been defined then make a leaf node for it
    if ((id = findglob(Text)) == -1) {
        printf("Undeclared variable %s\n", Text);
    }
    right = makeLeaf(LVIDENT_A, id);

    // Ensure we have an equals sign
    match(ASSIGN_T, "=");

    // Parse the following expression
    left = PrattParser(0);
    arr[id]=interpretAST(left);

    // Make an assignment AST tree
    tree = makeASTNode(ASSIGN_A, left, right, 0);
    
    stmtEnd();
}

void statements()
{
    while (1)
    {
        if (Token.token == WRITE_T)
            print_statement();

        else if (Token.token == INT_T)
            var_declaration();

        else if (Token.token == IDENT_T)
            assignment_statement();

        else if(Token.token == READ_T)
            read_statement();

        else if (Token.token == EOF_T)
            return;
    }
}

int main(int argc, char *argv[])
{
    // if (argc != 2)
    //     usage(argv[0]);

    Line = 1;
    Putback = '\n';


    if ((Infile = fopen("inp7.txt", "r")) == NULL) {
        fprintf(stderr, "Unable to open %s: %s\n", argv[1], strerror(errno));
        exit(1);
  }
    // scanfile();

    scan(&Token);
    statements();



    exit(0);
}
