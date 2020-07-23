//estrutura de dados que representa uma pilha
typedef struct pilha {
  int valor;
  struct pilha * prox;
}PILHA;

//estrutura de dados que representa uma variavel
typedef struct variavel {
  char fnome[33];
  int fvalor;
  struct variavel * prox;
}VARIAVEL;

//estrutura de dados que representa uma instrucao
typedef struct instrucao {
  char fnome[8];
  char fvariavel[33];
  int fvalor;
  int n_instrucao;
  struct instrucao * prox;
}INSTRUCAO;

//estrutura de dados que representa uma hash table de variaveis
typedef struct hash_V {
  VARIAVEL * flista;
}HASH_V;

//estrutura de dados que representa uma hash table de instrucoes
typedef struct hash_I {
  INSTRUCAO * flista;
}HASH_I;


