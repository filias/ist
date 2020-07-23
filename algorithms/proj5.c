#include <stdio.h>
#include <ctype.h>
#include <string.h>

#define HASHSIZE 1000

#include "estruturas5.h"

/*int HashFunction(char *p) {
  int hashvalue= 0;
  for(; *p; p++)
    hashvalue= hashvalue * 65599 + *p;
  hashvalue%= HASHSIZE;
  //printf("o hashvalue e %d\n", hashvalue);
  return hashvalue;
  }*/

int HashFunction(int str){
 int hashval= 0, i;
 for(i=0; i<str; i++){
   hashval= 37 * hashval + str;
 }
 if(hashval< 0)
   hashval= - hashval;
 return (hashval % 1000-1);
}

int HashFunction2(char *str){
 int hashval= 0, i;
 for(i=0; str[i]!= 0; i++){
   hashval= 37 * hashval + str[i];
 }
 if(hashval < 0)
   hashval= - hashval;
 //printf("hashvalue: %d\n", hashval%1000);
  return (hashval % 1000-1);
}

int main (int argc, char * argv[]) {
  int i=0, temp=0, tem_palavras=1, e_variavel=0, valor=0, ja_existe=0, mesma_variavel=0, num_read=0;
  int  hashvalue= 0, var_hash= 1, pc= 1;
  char comando[6], variavel[33], c, d;
  FILE * fp= fopen(argv[1],"r");
  PILHA * stack= NULL, * stack_lixo= NULL, * nova_stack=NULL, * novo_topo= NULL;
  VARIAVEL * nova_variavel=NULL, * lista_aux_push=NULL, * lista_aux_pop=NULL, *lista_aux_jmp= NULL;
  INSTRUCAO * nova_instrucao= NULL,* lista_aux= NULL;
  HASH_V hash_v[HASHSIZE];
  HASH_I hash_i[HASHSIZE];

  for(i=0; i<HASHSIZE; i++) {
    hash_v[i].flista= NULL;
    hash_i[i].flista= NULL;
  }


  while(tem_palavras) {
    temp= fscanf(fp, "%s", comando);
    if(temp==EOF) {
      tem_palavras= 0;
    } else {
      if(comando[0] == 'J' || comando[0] == 'P') {
	fscanf(fp, "%s", variavel);
	hashvalue= HashFunction(var_hash);
	lista_aux= hash_i[hashvalue].flista;
	nova_instrucao= (INSTRUCAO *) malloc (sizeof(INSTRUCAO));
	strcpy(nova_instrucao->fnome, comando);
	if(isalpha(variavel[0]) || variavel[0]== '_') {
	  strcpy(nova_instrucao->fvariavel, variavel);
	  nova_instrucao->fvalor= '\0';
	} else {
	  nova_instrucao->fvalor= atoi(variavel);
	  nova_instrucao->fvariavel[0]= '\0';
	}
	nova_instrucao->n_instrucao= var_hash;
	nova_instrucao->prox= hash_i[hashvalue].flista;
	hash_i[hashvalue].flista= nova_instrucao;

      } else {
	hashvalue= HashFunction(var_hash);
	lista_aux= hash_i[hashvalue].flista;
	nova_instrucao= (INSTRUCAO *) malloc (sizeof(INSTRUCAO));
	strcpy(nova_instrucao->fnome, comando);
	nova_instrucao->n_instrucao= var_hash;
	nova_instrucao->prox= hash_i[hashvalue].flista;
	hash_i[hashvalue].flista= nova_instrucao;
      }
      var_hash++;
    }
  }
  

  while(pc< var_hash && pc>= 1) {
    hashvalue= HashFunction(pc);
    lista_aux= hash_i[hashvalue].flista;
    ja_existe= 0;

    while(ja_existe==0) { 
      if(lista_aux->n_instrucao == pc) { 
	ja_existe= 1;
      } else{ 
	lista_aux= lista_aux->prox;
      }
    }
    
    c= lista_aux->fnome[0];
    d= lista_aux->fnome[1];
    
    if(c=='A') {
      nova_stack= stack->prox;                                                                          
      stack->valor+= nova_stack->valor;
      stack->prox= nova_stack->prox;   
      free(nova_stack);
      pc++;
    }
    else if(c=='M') {
      nova_stack= stack->prox;
      stack->valor*= nova_stack->valor;
      stack->prox= nova_stack->prox;
      free(nova_stack);
      pc++;
    }
    else if(c=='R') {
      scanf("%d ", &num_read);
      nova_stack=(PILHA *)malloc(sizeof(PILHA));
      nova_stack->valor= num_read;
      nova_stack->prox= stack;
      stack= nova_stack;
      pc++;
    }   
    else if(c=='W') {
      printf("%d\n", stack->valor);
      stack= stack->prox;
      pc++;
    }
    else if(c=='S') {
      nova_stack= stack->prox;
      stack->valor-= nova_stack->valor;
      stack->prox= nova_stack->prox;
      free(nova_stack);
      pc++; 
    }
    else if(c=='D' && d=='U') {
      novo_topo=(PILHA *)malloc(sizeof(PILHA));
      novo_topo->valor= stack->valor;
      novo_topo->prox= stack;
      stack= novo_topo;
      pc++;
    }
    else if(c=='D' && d=='I') {
      nova_stack= stack->prox;
      stack->valor/= nova_stack->valor;
      stack->prox= nova_stack->prox;
      free(nova_stack); 
      pc++;
    }
    else if(c=='P' && d=='U') {
      if(lista_aux->fvariavel[0] == '\0') {
	novo_topo= (PILHA *) malloc(sizeof(PILHA));
	novo_topo->valor= lista_aux->fvalor;
	novo_topo->prox= stack;
	stack= novo_topo;
      } else {
	hashvalue= HashFunction2(lista_aux->fvariavel);
	lista_aux_push= hash_v[hashvalue].flista;
	mesma_variavel= 0;
	while(lista_aux_push!= NULL && mesma_variavel==0) {
	  if((strcmp(lista_aux_push->fnome, lista_aux->fvariavel))==0) {
	    mesma_variavel= 1;
	    novo_topo= (PILHA *) malloc(sizeof(PILHA));
	    novo_topo->valor= lista_aux_push->fvalor;
	    novo_topo->prox= stack;
	    stack= novo_topo;
	  }
	  else{
	    lista_aux_push= lista_aux_push->prox;
	  }
	}
      }
      pc++;
    }    
    else if(c=='P' && d=='O') {
      hashvalue= HashFunction2(lista_aux->fvariavel);
      lista_aux_pop= hash_v[hashvalue].flista;
      ja_existe= 0;
      
      while(lista_aux_pop!= NULL && ja_existe==0) { 
	if((strcmp(lista_aux_pop->fnome, lista_aux->fvariavel))==0) { 
	  ja_existe= 1;
	  lista_aux_pop->fvalor= stack->valor;
	  stack_lixo= stack;
	  stack= stack->prox;
	  free(stack_lixo);
	}
	else{ 
	  lista_aux_pop= lista_aux_pop->prox;
	}
      }
      if(ja_existe==0) { 
	nova_variavel= (VARIAVEL *) malloc (sizeof(VARIAVEL));
	strcpy(nova_variavel->fnome, lista_aux->fvariavel);
	nova_variavel->fvalor=stack->valor;
	nova_variavel->prox= hash_v[hashvalue].flista;
	hash_v[hashvalue].flista= nova_variavel;
	stack_lixo= stack;
	stack= stack->prox;
	free(stack_lixo);
      }
      pc++;
    }
    else if(c=='J' && d=='U') {
      if(lista_aux->fvalor == '\0') {
	hashvalue= HashFunction2(lista_aux->fvariavel);
	lista_aux_pop= hash_v[hashvalue].flista;
	ja_existe= 0;
	
	while(lista_aux_pop!= NULL && ja_existe==0) { 
	  if((strcmp(lista_aux_pop->fnome, lista_aux->fvariavel))==0) { 
	    ja_existe= 1;
	    pc= lista_aux_pop->fvalor;
	  }
	  else{ 
	    lista_aux_pop= lista_aux_pop->prox;
	  }
	}
      } else {
	pc= lista_aux->fvalor;
      }
      if(pc == 0)
	break;
    }
    else if(lista_aux->fnome[3] == 'P') {
      if(stack->valor > 0) {
	if(lista_aux->fvalor == '\0') {
	  hashvalue= HashFunction2(lista_aux->fvariavel);
	  lista_aux_jmp= hash_v[hashvalue].flista;
	  ja_existe= 0;
	  
	  while(lista_aux_jmp!= NULL && ja_existe==0) { 
	    if((strcmp(lista_aux_jmp->fnome, lista_aux->fvariavel))==0) { 
	      ja_existe= 1;
	      pc= lista_aux_jmp->fvalor;
	    }
	    else{ 
	      lista_aux_jmp= lista_aux_jmp->prox;
	    }
	  }
	} else {
	  pc= lista_aux->fvalor;
	}
      } else {
	pc++;
      }

      stack_lixo= stack;
      stack= stack->prox;
      stack_lixo->prox= NULL;
      free(stack_lixo);
    } else {
      if(stack->valor == 0) {
	if(lista_aux->fvalor == '\0') {
	  hashvalue= HashFunction2(lista_aux->fvariavel);
	  lista_aux_jmp= hash_v[hashvalue].flista;
	  ja_existe= 0;
	  
	  while(lista_aux_jmp!= NULL && ja_existe==0) { 
	    if((strcmp(lista_aux_jmp->fnome, lista_aux->fvariavel))==0) { 
	      ja_existe= 1;
	      pc= lista_aux_jmp->fvalor;
	    }
	    else{ 
	      lista_aux_jmp= lista_aux_jmp->prox;
	    }
	  }
	} else {
	  pc= lista_aux->fvalor;
	}
      } else {
	pc++;
      }

      stack_lixo= stack;
      stack= stack->prox;
      stack_lixo->prox= NULL;
      free(stack_lixo);
    }
  }
}
