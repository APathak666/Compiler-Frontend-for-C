int interpretAST(struct ASTNode *n) {
  int leftval, rightval;

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
      return (n->intVal);
    default:
      fprintf(stderr, "Unknown AST operator %d\n", n->operation);
      exit(1);
  }
}
