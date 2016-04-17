/* Dimas Anastasios - 1663 */
#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>
/*Macro for printing Errors*/
#define ERROR(s) exit(1 + printf("ERROR AT LINE %d: %s\n", line_number,s))
/*Macro that compares strings and returns id's
            (used by lectical analyzer)            */
#define CMP(s1,s2, id) if(!strcmp(s1,s2)) return id
#define LEX() *token_id = lex()
#define WORD        1
#define CONSTANT    2
#define AND         3
#define CALL        4
#define ELSE        5
#define FOR         6
#define IF          7
#define IN          9
#define INOUT       10
#define INT         11
#define INPUT       12
#define NOT         13
#define OR          14
#define PRINT       15
#define PROGRAM     16
#define RETURN      17
#define VOID        18
#define WHILE       19
#define MINUS       21
#define TIMES       22
#define SEMICOLON   23
#define LEFT_PAREN  24
#define RIGHT_PAREN 25
#define COMMA       26
#define LEFT_BRAC   27
#define RIGHT_BRAC  28
#define BLOCK_START 29
#define BLOCK_END   30
#define DIFFER      31
#define LESS        32
#define LESS_EQ     33
#define GREATER_EQ  34
#define GREATER     35
#define ASSIGNMENT  36
#define PLUS_EQ     37
#define PLUS        38
#define DIVIDE      39
#define EQUAL       40
#define DECLAREINT  41

FILE *infile;
FILE *final;
int line_number = 1;
char token[256];

typedef struct args{
      char name[31];
      char type[10];
      struct args *next;
}args;

typedef struct variable{
      int offset;
}variable;

typedef struct function{
      int start_quad;
      args *arguments;
      int framelength;
}function;

typedef struct argument{
      int offset;
      char parMode[6];
}argument;

typedef union ent_type{
      variable var;
      function fun;
      argument arg;
}ent_type;

typedef struct entity{
      char name[31];
      char type[4]; /*can be fun, var or arg*/
      ent_type ent;
      struct entity *next;
}entity;

typedef struct scope{
      entity *entities;
      int nestingLevel;
      char name[31]; //used to test results
      struct scope *enclosingScope;
}scope;

scope *symbol_table = NULL; /* Symbol's table declaration */
scope *last_scope;

typedef struct func_stack{
      function *func;
      struct func_stack *next;
}func_stack;

func_stack *last_func = NULL;

void push_func(function *item){
      func_stack *temp = malloc(sizeof(func_stack));
      temp->func = item;
      temp->func->framelength = 0;
      temp->func->start_quad = 0;
      temp->func->arguments = NULL;
      temp->next = last_func;
      last_func = temp;
}

void pop_func(){
      func_stack *temp = last_func;
      last_func = last_func->next;
      free(temp);
}

typedef struct quad{
      int label;
      char op[12]; /*longest operator can be "begin_block"*/
      char arg1[31];
      char arg2[31];
      char arg3[31];
      struct quad *next;
}quad;

typedef struct Qlabel{
      quad *quad_ptr;
      struct Qlabel *next;
}Qlabel;

typedef struct cond{
      Qlabel *true;
      Qlabel *false;
}cond;

typedef struct next{
      Qlabel *next;
}next;

char *factor(int *);
char *expression(int *);
char *term(int *);
char *add_oper(int *);
char *mul_oper(int *);
cond condition(int *);
char *optional_sign(int *);
args *actualpars(int *);
void brack_or_stat(int *);
void brackets_seq(int *);
void sequence(int *);
void block(int *);
void assignment_stat(int *, char *);
void add_scope(char []);
void remove_scope(void);
void add_entity(char [], char []);
entity *search_entity(char []);
void generate_final(void);
/*Intermediate code*/

/* Stack definition and stack based functions
   Will be used for "begin/end block" code    */
typedef struct stack{
      char func_name[31];
      struct stack *next;
}stack;

stack *func_names = NULL;
stack *temp;
int stack_pointer = 1; /*Always will be at least "progam code" */

void push(char name[]){
      stack *temp;
      temp = malloc(sizeof(stack));
      strcpy(temp->func_name, name);
      temp->next = func_names;
      func_names = temp;
}

char *pop(){
      char *w;
      w = temp->func_name;
      temp = temp->next;
      func_names = temp;
      return w;
}
/* End of stack code */

quad *quadlist = NULL;  /*the list where we will store our quads */
quad *last_node = NULL; /*so we don't have to scan all of the previous quads in our list*/

void backpatch(Qlabel *x, char y[]){
      Qlabel *temp;
      temp = x;
      while(temp != NULL){
            strcpy(temp->quad_ptr->arg3, y);
            temp = temp->next;
      }
}

Qlabel *merge(Qlabel *list1, Qlabel *list2){
      Qlabel *temp;
      if(list1 == NULL) return list2;
      if(list2 == NULL) return list1;
      temp = list1;
      while(temp->next != NULL)
            temp = temp->next;
      temp->next = list2;
      return list1;
}

Qlabel *makelist(quad *label_ptr){
      Qlabel *temp = malloc(sizeof(Qlabel));
      temp->quad_ptr = label_ptr;
      temp->next = NULL;
      return temp;
}

char *newtemp(void){
      char *temp = malloc(10);
      static int count = 0;
      sprintf(temp, "T_%d", count++);
      add_entity(temp, "var");
      return temp;
}

int nextlabel(void){
      static int label = 100;
      return label++;
}

quad *genquad(char op[], char arg1[], char arg2[], char arg3[]){
      quad *newquad;
      newquad = malloc(sizeof(quad));
      newquad->label = nextlabel();
      strcpy(newquad->op, op);
      strcpy(newquad->arg1, arg1);
      strcpy(newquad->arg2, arg2);
      strcpy(newquad->arg3, arg3);
      if(last_node == NULL){
            newquad->next = NULL;
            quadlist = newquad;
            last_node = newquad;
      }
      else{
            newquad->next = NULL;
            last_node->next = newquad;
            last_node = newquad;
      }
      return newquad;
}

int nextquad(){
      return(last_node->label+1);
}

/*Lectical analyzer*/
int digit_found(char c){
	/*Function for recognizing digits*/
	int number;
	int length = 0;
	token[length++] = c;
	while(isdigit(c = getc(infile)))
		token[length++] = c;
	if(isalpha(c))
		ERROR("Variables should start with a letter.");
	if(c == '\n')
		line_number++;
	token[length] = '\0';
	number = atoi(token);
	if(number > 32767 || number < -32767)
		ERROR("Constants range is [-32767, 32767]");
	ungetc(c, infile);
	return CONSTANT;
}

int word_found(char c){
	/*Function for recognizing variables and toy's words*/
	int length = 0;
	token[length++] = c;
	while(isalnum(c = getc(infile)))
		token[length++] = c;
	ungetc(c, infile);

	if(c == '\n')
		line_number++;
	token[length] = '\0';
	if(strlen(token) > 30)
		token[31] = '\0';
	CMP(token, "for", FOR);
	CMP(token, "and", AND);
	CMP(token, "call", CALL);
	CMP(token, "if", IF);
	CMP(token, "in", IN);
	CMP(token, "inout", INOUT);
	CMP(token, "int", INT);
	CMP(token, "input", INPUT);
	CMP(token, "not", NOT);
	CMP(token, "or", OR);
	CMP(token, "print", PRINT);
	CMP(token, "program", PROGRAM);
	CMP(token, "return", RETURN);
	CMP(token, "void", VOID);
	CMP(token, "while", WHILE);
	CMP(token, "else", ELSE);
	CMP(token, "declareint", DECLAREINT);
	return WORD;
}

int slash(){
	/*returns 1 if / stands for division operator and -1 for comments*/
	char c;
	c = getc(infile);
	if(c != '*'){
		token[0] = '/'; token[1] = '\0';
		ungetc(c, infile);
		return 1;
	}
	while(c != EOF){
		if(c == '\n')
			line_number++;
		c = getc(infile);
		if(c == '*')
			if((c = getc(infile)) == '/')
				return -1;
	}
	ERROR("Comments are not terminating\n");
}

int anything_else(char c){

	/*function for recognizing operators and seperators*/
	/*in case we have an one symbol operator*/
	token[0] = c; token[1] = '\0';
	CMP(token, "-", MINUS);
	CMP(token, "*", TIMES);
	CMP(token, ";", SEMICOLON);
	CMP(token, "(", LEFT_PAREN);
	CMP(token, ")", RIGHT_PAREN);
	CMP(token, ",", COMMA);
	CMP(token, "[", LEFT_BRAC);
	CMP(token, "]", RIGHT_BRAC);
	CMP(token, "{", BLOCK_START);
	CMP(token, "}", BLOCK_END);
	CMP(token, "=", EQUAL);

	if(c == '<'){ /*old c is already stored at token[0]*/
		c = getc(infile);
		if(c == EOF)
			ERROR("Invalid usage of <.");
		if(c == '>'){
			token[1] = '>'; token[2] = '\0';
			return DIFFER;
		}
		else if(c == '='){
			token[1] = '='; token[2] = '\0';
			return LESS_EQ;
		}
		else{
			fseek(infile, -1, SEEK_CUR); /*sets file pointer at the right point*/
			return LESS;
		}
	}
	if(c == '>'){ /*old c is already stored at token[0]*/
		c = getc(infile);
		if(c == EOF)
			ERROR("Invalid usage of >.");
		if(c == '='){
			token[1] = '='; token[2] = '\0';
			return GREATER_EQ;
		}
		else{
			fseek(infile, -1, SEEK_CUR);
			return GREATER;
		}
	}
	if(c == '+'){ /*old c is already stored at token[0]*/
		c = getc(infile);
		if(c == EOF)
			ERROR("Invalid usage of +.");
		if(c == '='){
			token[1] = '='; token[2] = '\0';
			return PLUS_EQ;
		}
		else{
			fseek(infile, -1, SEEK_CUR);
			return PLUS;
		}
	}
	if(c == ':'){
		if((c = getc(infile))!= '=')
			ERROR("Character \":\" is invalid, did you mean \":=\" ?"); /*semicolon is missing due to macro definition*/
		else{
			token[1] = '='; token[2] = '\0';
			return ASSIGNMENT;
		}
	}
	sprintf(token,"Character %c is invalid\n", c);
	ERROR(token);
}

int lex(){
	char c;
		while((c = getc(infile))!= EOF){
		if(isspace(c)){
			if(c == '\n')
				line_number++;
			continue;
		}
			if(isdigit(c))
			return digit_found(c);

		if(isalpha(c))
			return word_found(c);

		if(c == '/')
			if(slash(token) == -1)
				continue;
			else
				return DIVIDE;
		return anything_else(c);
	}
	return EOF;
}

/*syntactic analyzer and intermediate code generation*/

/*functions for parameters quads.
  Yes, yes, for something like "max( max(), max())". */
void add_argument(args **list, char *name, char type[]){
      args *temp = malloc(sizeof(args));
      strcpy(temp->name, name);
      strcpy(temp->type, type);
      temp->next = *list;
      *list = temp;
}

void arg_pop(args **list){
      args *temp = *list;
      *list = (*list)->next;
      free(temp);
}

void reverse(args **list){
      args *temp = NULL, *p;
      while(*list != NULL){
            p = *list;
            *list = (*list)->next;
            p->next = temp;
            temp = p;
      }
      *list = temp;
}
/*End of parameters functions */

char *factor(int *token_id){
      char *Fplace = malloc(31);
      strcpy(Fplace, token);
	if(*token_id == CONSTANT){
	      strcpy(Fplace, token);
		LEX();
	}
	else if(*token_id == LEFT_PAREN){
		LEX();
		Fplace = expression(token_id);
		if(*token_id == RIGHT_PAREN)
			LEX();
		else
			ERROR("Expected ')'.");
	}
	else if(*token_id == WORD){
	      char func[31];
	      char *w;
	      strcpy(func, token);
		LEX();
            if(*token_id == LEFT_PAREN){
                  args *x;
                  x = actualpars(token_id);
                  reverse(&x);
                  while(x != NULL){
                        genquad("par", x->name, x->type, "_");
                        arg_pop(&x);
                  }
                  w = newtemp();
                  genquad("par", w, "RET","_");
                  genquad("call", "_", "_", func);
                  Fplace = w;
            }
	}
      return Fplace;
}

char *term(int *token_id){
      char *F1place, *F2place, *w, *sign;
	F1place = factor(token_id);
	while(*token_id == TIMES || *token_id == DIVIDE){
		sign = mul_oper(token_id);
		F2place = factor(token_id);
		w = newtemp();
		if(!strcmp("*", sign))
                  genquad("*", F1place, F2place, w);
            else
                  genquad("/", F1place, F2place, w);
		strcpy(F1place, w);
	}
	return F1place;
}

char *expression(int *token_id){
      char *T1place = malloc(31), *T2place, *w,*sign,*opt_sign;
	opt_sign = optional_sign(token_id);
	if(opt_sign != NULL){
	      w = newtemp();
            genquad("-", "0", term(token_id), w);
            strcpy(T1place, w);
	}
      else
            T1place = term(token_id);
	while(*token_id == PLUS || *token_id == MINUS){
		sign = add_oper(token_id);
		w = newtemp();
		T2place = term(token_id);
		if(!strcmp(sign, "+"))
                  genquad("+", T1place, T2place, w);
            else
                  genquad("-", T1place, T2place, w);
            strcpy(T1place, w);
	}
	return T1place;
}

char *relational_oper(int *token_id){
      char *w = malloc(3);
      strcpy(w, token);
	if(*token_id == EQUAL || *token_id == LESS ||
		*token_id == LESS_EQ || *token_id == DIFFER||
		*token_id == GREATER || *token_id == GREATER_EQ)
		LEX();
      return w;
}

cond boolfactor(int *token_id){
      cond R, B;
      R.true = R.false = NULL;
	if(*token_id == NOT){
		LEX();
		if(*token_id == LEFT_BRAC){
			LEX();
			B = condition(token_id);
			R.true = B.false;
			R.false = B.true;
			if(*token_id == RIGHT_BRAC)
				LEX();
			else
				ERROR("Expected ']' afted condition.");
		}
		else
			ERROR("Expected '[' after not.");
	}
	else if(*token_id == LEFT_BRAC){
		LEX();
		B = condition(token_id);
		R.true = B.true;
		R.false = B.false;
		if(*token_id == RIGHT_BRAC){
			LEX();
		}
		else
			ERROR("Expected ']' after condition.");
	}
	else{
	      char *op1, *op2, *relop;
		op1 = expression(token_id);
		relop = relational_oper(token_id);
		op2 = expression(token_id);
		R.true = makelist(genquad(relop, op1, op2, "_"));
		R.false = makelist(genquad("jump", "_", "_", "_"));

	}
	return R;
}

cond boolterm(int *token_id){
      cond Q, R1;
	R1 = boolfactor(token_id);
	Q.true = R1.true;
	Q.false = R1.false;
	while(*token_id == AND){
	      char quad[5];
	      cond R2;
	      sprintf(quad, "%d", nextquad());
		LEX();
		R2 = boolfactor(token_id);
		backpatch(Q.true, quad);
		Q.false = merge(Q.false, R2.false);
		Q.true = R2.true;
	}
	return Q;
}

cond condition(int *token_id){
      cond B, Q1;
      Q1 = boolterm(token_id);
      B.true = Q1.true;
      B.false = Q1.false;
	while(*token_id == OR){
	      char quad[5];
	      cond Q2;
	      sprintf(quad, "%d", nextquad());
		LEX();
		Q2 = boolterm(token_id);
		backpatch(B.false, quad);
		B.true = merge(B.true, Q2.true);
		B.false = Q2.false;
	}
	return B;
}

char *optional_sign(int *token_id){
	if(*token_id == PLUS || *token_id == MINUS)
		return add_oper(token_id);
      return NULL;
}

char *mul_oper(int *token_id){
      char *w = malloc(2);
	if(*token_id == TIMES || *token_id == DIVIDE){
	      strcpy(w, token);
		LEX();
	}
	return w;
}

char *add_oper(int *token_id){
      char *w = malloc(2);
	if(*token_id == PLUS || *token_id == MINUS){
	      strcpy(w, token);
		LEX();
	}
	return w;
}

void return_stat(int *token_id){
      char *w;
	if(*token_id == LEFT_PAREN){
		LEX();
		w = expression(token_id);
		if(*token_id == RIGHT_PAREN)
			LEX();
		else
			ERROR("Expected ')' after return.");
		}
	else
		ERROR("Expected '(' after return.");
      genquad("ret", w, "_", "_");
}

void actualparitem(int *token_id, args **argsList){
      char *w;
      args *arg = NULL;
	if(*token_id == INOUT){
		LEX();
		add_argument(argsList, token, "REF");
		if(*token_id == WORD)
			LEX();
		else
			ERROR("Invalid actual parameter.");
	}
	else if(*token_id == IN){
		LEX();
		w = expression(token_id);
		add_argument(argsList, w, "CV");
	}
}

args *actualparlist(int *token_id){
      args *argsList = NULL;
	actualparitem(token_id, &argsList);
	while(*token_id == COMMA){
		LEX();
		if(*token_id == IN || *token_id == INOUT)
			actualparitem(token_id, &argsList);
		else
			ERROR("Expected in or inout after comma");
	}
	return argsList;
}

args *actualpars(int *token_id){
      args *argsList;
      if(*token_id == LEFT_PAREN){
		LEX();
		argsList = actualparlist(token_id);
		if(*token_id == RIGHT_PAREN)
			LEX();
		else
			ERROR("Expecting ) at the end of parameters.");
	}
	return argsList;
}

void call_stat(int *token_id){
	if(*token_id == WORD){
	      args *x;
		LEX();
		x = actualpars(token_id);
		reverse(&x);
            while(x != NULL){
                  genquad("par", x->name, x->type, "_");
                  arg_pop(&x);
            }
	}
	else
		ERROR("Invalid function name.");
}

void print_stat(int * token_id){
      char *w;
	if(*token_id == LEFT_PAREN){
		LEX();
		w = expression(token_id);
		if(*token_id == RIGHT_PAREN)
			LEX();
		else
			ERROR("Expected ')' at the end of print.");
	}
	else
		ERROR("Expected '(' after print.");
      genquad("print","_", "_", w);
}

void input_stat(int *token_id){
	if(*token_id == LEFT_PAREN){
		LEX();
		if(*token_id == WORD){
		      genquad("input", "_", "_", token);
			LEX();
			if(*token_id == RIGHT_PAREN)
				LEX();
			else
				ERROR("Expected ')' at the end of input.");
		}
		else
			ERROR("Invalid argument for input");
	}
	else
		ERROR("Expected '(' after input.");
}

void for_stat(int *token_id){
      cond con;
      char *opt_s, *w;
	if(*token_id == LEFT_PAREN){
		LEX();
		if(*token_id == WORD){
		      char left_var[31];
		      strcpy(left_var, token);
		      LEX();
			assignment_stat(token_id, left_var);
                  if(*token_id == SEMICOLON){
			      char condQuad[5];
			      sprintf(condQuad, "%d", nextquad());
                        LEX();
                        con = condition(token_id);
				if(*token_id == SEMICOLON){
				      char assignQuad[5];
				      sprintf(assignQuad, "%d", nextquad());
					LEX();
					if(*token_id == WORD){
					      char temp[2][31];
					      strcpy(temp[1], token);
						LEX();
						if(*token_id == PLUS_EQ){
							LEX();
							opt_s = optional_sign(token_id);
							if(*token_id == CONSTANT){
							      if(opt_s == NULL)
                                                      strcpy(temp[2], token);
                                                else
                                                      strcpy(temp[2], token);
								LEX();
								if(*token_id == RIGHT_PAREN){
								      char sQuad[5], nQuad[5];
								      sprintf(sQuad, "%d", nextquad());
									LEX();
									brack_or_stat(token_id);
									if(opt_s != NULL){
									      w = newtemp();
									      genquad("-", "0", temp[2], w);
									      genquad("+", temp[1], w, temp[1]);
									}
									else
                                                            genquad("+", temp[1], temp[2], temp[1]);
									genquad("jump", "_", "_", condQuad);
									backpatch(con.true, sQuad);
									sprintf(nQuad, "%d", nextquad());
									backpatch(con.false, nQuad);
								}
								else
									ERROR("Expected ')' at the end of for.");
							}
							else
								ERROR("Expected constant after '+='.");
						}
						else
							ERROR("Expected '+=' after variable.");
					}
					else
						ERROR("Expected variable after ';'.");
				}
				else
					ERROR("Expected ';' after condition.");
                  }
			else
                        ERROR("Semicolon is missing");
		}
		else
			ERROR("Expected variable after '('.");
	}
	else
		ERROR("Expected '(' after for.");
}

void while_stat(int *token_id){
      char p1quad[5], p2quad[5], Nquad[5];
      cond B;
      sprintf(p1quad, "%d", nextquad());
	if(*token_id == LEFT_PAREN){
		LEX();
		B = condition(token_id);
		sprintf(p2quad, "%d", nextquad());
		if(*token_id == RIGHT_PAREN){
			LEX();
			brack_or_stat(token_id);
			backpatch(B.true, p2quad);
			genquad("jump", "_", "_", p1quad);
			sprintf(Nquad, "%d", nextquad());
			backpatch(B.false, Nquad);
		}
		else
			ERROR("Expected ')' at the end of while statement.");
	}
	else
		ERROR("Expected '(' after while statement.");
}

void elsepart(int *token_id, Qlabel *list){
      char nQuad[5];
	if(*token_id == ELSE){
		LEX();
		brack_or_stat(token_id);
	}
	sprintf(nQuad, "%d", nextquad());
	backpatch(list, nQuad);
}

void if_stat(int *token_id){
      cond B;
      Qlabel *list;
      char p1quad[5], p2quad[5];
	if(*token_id == LEFT_PAREN){
		LEX();
		B = condition(token_id);
		sprintf(p1quad, "%d", nextquad());
		if(*token_id == RIGHT_PAREN){
			LEX();
			brack_or_stat(token_id);
			list = makelist(genquad("jump", "_", "_", "_"));
			sprintf(p2quad, "%d", nextquad());
			elsepart(token_id, list);
			backpatch(B.true, p1quad);
			backpatch(B.false, p2quad);
		}
		else
			ERROR("Expected ')' at the end of if statement.");
	}
	else
		ERROR("Expected '(' after if statement.");
}

void assignment_stat(int *token_id, char *left_var){
      char *Eplace;
	if(*token_id == ASSIGNMENT){
		LEX();
		Eplace = expression(token_id);
		genquad(":=", Eplace, "_", left_var);
	}
	else
		ERROR("Expected ':=' after variable.");

}

void statement(int *token_id){
	if(*token_id == IF){
		LEX();
		if_stat(token_id);
	}
	if(*token_id == WHILE){
		LEX();
		while_stat(token_id);
	}
	if(*token_id == FOR){
		LEX();
		for_stat(token_id);
	}
	if(*token_id == INPUT){
		LEX();
		input_stat(token_id);
	}
	if(*token_id == PRINT){
		LEX();
		print_stat(token_id);
	}
	if(*token_id == CALL){
	      char proc_name[31];
		LEX();
		strcpy(proc_name, token);
		call_stat(token_id);
		genquad("call", "_", "_", proc_name);
	}
	if(*token_id == RETURN){
		LEX();
		return_stat(token_id);
	}
	if(*token_id == WORD){
	      char left_var[31];
	      strcpy(left_var, token);
		LEX();
		assignment_stat(token_id, left_var);
	}
}

void brack_or_stat(int *token_id){
	if(*token_id == BLOCK_START)
		brackets_seq(token_id);
	else{
		statement(token_id);
		LEX();
	}
}

void brackets_seq(int *token_id){
	if(*token_id == BLOCK_START){
		LEX();
		sequence(token_id);
		if(*token_id == BLOCK_END)
			LEX();
		else
			ERROR("Block is not terminating.");
	}
	else
		ERROR("Every block should start with '{'");
}

void sequence(int *token_id){
	statement(token_id);
	while(*token_id == SEMICOLON){
		LEX();
		statement(token_id);
	}
}

void formalparitem(int *token_id, args **List){
	if(*token_id == IN){
		LEX();
		if(*token_id == WORD){
		      add_argument(List, token, "in");
                  add_entity(token, "in");
			LEX();
		}
		else
			ERROR("Please use a valid name for formal parameters.");
	}
      else if(*token_id == INOUT){
            LEX();
            if(*token_id == WORD){
                  add_argument(List, token, "inout");
                  add_entity(token, "inout");
                  LEX();
            }
		else
			ERROR("Please use a valid name for formal parameters.");
	}
}

void formalparlist(int *token_id){
      args *List = NULL;
	formalparitem(token_id, &List);
	while(*token_id == COMMA){
			LEX();
			if(*token_id == IN || *token_id == INOUT)
				formalparitem(token_id, &List);
			else
				ERROR("Expected 'in' or 'inout' after comma.");
	}
	if(List != NULL){
            reverse(&List);
            last_func->func->arguments = List;
	}
      else
            last_func->func->arguments = NULL;
}

void formalpars(int *token_id){
	if(*token_id == LEFT_PAREN){
		LEX();
		if(*token_id == RIGHT_PAREN){ /*if we don't have parameters*/
			LEX();
			return;
		}
		formalparlist(token_id);
		if(*token_id == RIGHT_PAREN)
			LEX();
		else
			ERROR("Expected ')' at the end of parameters declaration.");
	}
	else
			ERROR("Expected '(' at the begining of formal parameters declaration.");
}

void funcbody(int *token_id){
	formalpars(token_id);
	block(token_id);
}

void procorfunc(int *token_id){
      if(*token_id == WORD){
	      push(token);
	      add_scope(token);
	      stack_pointer++;
		LEX();
		funcbody(token_id);
	}
      else
		ERROR("Invalid function name.");
}

void subprograms(int *token_id){
	while(*token_id == VOID || *token_id == INT){
		LEX();
		add_entity(token, "fun");
		procorfunc(token_id);
		last_func->func->framelength = next_offset(last_scope->entities);
		pop_func();
            generate_final();
		remove_scope();
	}
}

void varlist(int *token_id){
	if(*token_id == WORD){
	      add_entity(token, "var");
		LEX();
		while(*token_id == COMMA){
			LEX();
			if(*token_id == WORD){
                        add_entity(token, "var");
				LEX();
			}
			else
				ERROR("Expecting variable's name after comma.");

		}
	}
}

void declarations(int *token_id){
	if(*token_id == DECLAREINT){
		LEX();
		varlist(token_id);
		if(*token_id == SEMICOLON)
			LEX();
		else
			ERROR("Declaration should be terminated with ';'.");
	}
}

quad *firstquad;

void block(int *token_id){
      char *w, conv[5];
      quad *start;
	if(*token_id == BLOCK_START){
		LEX();
		declarations(token_id);
		temp = func_names;
		subprograms(token_id);
		if(0 != stack_pointer){
                  w = pop();
                  stack_pointer--;
                  start = genquad("begin_block", w, "_", "_");
                  if(!strcmp(w, "program")){
                        fseek(final,0 ,SEEK_SET);
                        fprintf(final, "L100:\tjmp  L%d\n", nextlabel()+1);
                        fseek(final, 0, SEEK_END);
                        sprintf(conv, "%d", start->label+1);
                        strcpy(firstquad->arg3, conv);
                  }
                  else
                        last_func->func->start_quad = start->label + 1;
            }
		sequence(token_id);
		if(!strcmp(w, "program"))
                  genquad("halt", "_", "_", "_");
		genquad("end_block",w, "_", "_");
		if(*token_id == BLOCK_END)
			LEX();
		else
			ERROR("Expected '}' at the end every of block.");
	}
	else
		ERROR("Command blocks should start with '{' character");
}

void program(int *token_id){
      firstquad = genquad("jump", "_", "_", "_"); /* Creates the first quad. Will be filled properly */
	if(*token_id == PROGRAM){
	    push("program");
		LEX();
		add_scope(token);
		if(*token_id == WORD){
			LEX();
			block(token_id);
		}
		else
			ERROR("Please insert a valid program name.");
	}
	else
		ERROR("Every program must start with keyword 'program'");
}

/* Symbols table functions */

void add_scope(char n[]){
      scope *temp = malloc(sizeof(scope));
      strcpy(temp->name,n);
      temp->entities = NULL;
      if(symbol_table == NULL)
            temp->nestingLevel = 0;
      else
            temp->nestingLevel = symbol_table->nestingLevel + 1;
      temp->enclosingScope = symbol_table;
      symbol_table = temp;
      last_scope = temp;
}

void remove_scope(){
      scope *temp;
      temp = symbol_table->enclosingScope;
      free(symbol_table);
      symbol_table = temp;
      last_scope = temp;
}

int next_offset(entity *list){
      entity *temp = list;
      int offset;
      int vars = 0;
      while(temp != NULL){
            if(!strcmp(temp->type, "var") || !strcmp(temp->type, "arg"))
                  offset = temp->ent.var.offset;
            temp = temp->next;
            vars++;
      }
      if(vars == 0)
            return 12;
      return offset + 4;
}

void add_entity(char name[], char type[]){
      static offset;
      entity *temp = malloc(sizeof(entity));
      entity *temp1 = last_scope->entities;
      strcpy(temp->name, name);
      strcpy(temp->type, type);
      temp->next = NULL;

      if(!strcmp(type, "in") || !strcmp(type, "inout")){
            strcpy(temp->type, "arg");
            if(temp1 == NULL)
                  temp->ent.arg.offset = offset = 12;
            else
                  temp->ent.arg.offset = next_offset(temp1);
            strcpy(temp->ent.arg.parMode, type);
      }

      if(!strcmp(type, "fun"))
            push_func(&(temp->ent.fun));

      else if(!strcmp(type, "var"))
            if(temp1 == NULL)
                  temp->ent.var.offset = offset = 12;
            else
                  temp->ent.var.offset = next_offset(temp1);

      if(temp1 == NULL)
            last_scope->entities = temp;
      else{
            while(temp1->next != NULL)
                  temp1 = temp1->next;
            temp1->next = temp;
      }
}

entity *search_entity(char word[]){
      scope *temp = symbol_table;
      while(temp != NULL){
            entity *temp_ent = temp->entities;
            while(temp_ent != NULL)
                  if(!strcmp(temp_ent->name, word))
                        return temp_ent;
                  else
                        temp_ent = temp_ent->next;
            temp = temp->enclosingScope;
      }
      printf("Undeclared variable : %s\n", word);
      exit(1);
}

/* End of Symbol table functions */

/* Final code */
int find_nesting_level(char *v){
      entity *temp;
      scope *temp_sc = symbol_table;
      while(temp_sc != NULL){
            temp = temp_sc->entities;
            while(temp != NULL){
                  if(!strcmp(v, temp->name))
                        return temp_sc->nestingLevel;
                  temp = temp->next;
            }
            temp_sc = temp_sc->enclosingScope;
      }
}

void gnlvcode(entity *r){
      int nLevel = find_nesting_level(r->name);
      int nCounter = last_scope->nestingLevel - nLevel;
      fprintf(final, "\tmovi R[255], M[4 + R[0]]\n");
      while(nCounter){
            fprintf(final,"\tmovi R[255], M[4 + R[255]]\n");
            nCounter--;
      }
      if(!strcmp(r->type, "var"))
            fprintf(final, "\tmovi R[254], %d\n", r->ent.var.offset);
      if(!strcmp(r->type, "arg"))
            fprintf(final, "\tmovi R[254], %d\n", r->ent.arg.offset);
      fprintf(final, "\taddi R[255], R[254], R[255]\n");
}

void loadvr(char *v, int r){
      entity *res;
      if(isdigit(v[0])){
            fprintf(final, "\tmovi R[%d], %s\n", r,v);
            return;
      }
      res = search_entity(v);
      if(!strcmp(res->type, "var") && !find_nesting_level(v))
            fprintf(final, "\tmovi R[%d], M[%d]\n", r, 600 + res->ent.var.offset);
      else if(!strcmp(res->type, "var") && find_nesting_level(v) == last_scope->nestingLevel)
            fprintf(final, "\tmovi R[%d], M[%d + R[0]]\n", r, res->ent.var.offset);
      else if(!strcmp(res->type, "arg") && find_nesting_level(v) == last_scope->nestingLevel && !strcmp(res->ent.arg.parMode, "in"))
            fprintf(final, "\tmovi R[%d], M[%d + R[0]]\n", r, res->ent.arg.offset);
      else if(!strcmp(res->type, "arg") && !strcmp(res->ent.arg.parMode, "inout") && find_nesting_level(v) == last_scope->nestingLevel){
            fprintf(final, "\tmovi R[255], M[%d + R[0]]\n", res->ent.arg.offset);
            fprintf(final, "\tmovi R[%d], M[R[255]]\n", r);
      }
      else if(!strcmp(res->type, "var") || (!strcmp(res->type, "arg") && !strcmp(res->ent.arg.parMode, "in")) && find_nesting_level(v) < last_scope->nestingLevel){
              gnlvcode(res);
              fprintf(final, "\tmovi R[%d], M[R[255]]\n", r);
      }
      else if(!strcmp(res->type, "arg") && !strcmp(res->ent.arg.parMode, "inout") && find_nesting_level(v) < last_scope->nestingLevel){
            gnlvcode(res);
            fprintf(final, "\tmovi R[255], M[R[255]]\n");
            fprintf(final, "\tmovi R[%d], M[R[255]]\n", r);
      }
}

void storerv(int r, char *v){
      entity *res = search_entity(v);
      if(!strcmp(res->type,"var") && !find_nesting_level(v))
            fprintf(final, "\tmovi M[%d], R[%d]\n", 600+res->ent.var.offset, r);
      else if(!strcmp(res->type, "var") && find_nesting_level(v) == last_scope->nestingLevel)
            fprintf(final, "\tmovi M[%d + R[0]], R[%d]\n",res->ent.var.offset, r);
      else if(!strcmp(res->type, "arg") && find_nesting_level(v) == last_scope->nestingLevel && !strcmp(res->ent.arg.parMode, "in"))
            fprintf(final, "\tmovi M[%d + R[0]], R[%d]\n", res->ent.arg.offset, r);
      else if(!strcmp(res->type, "arg") && !strcmp(res->ent.arg.parMode, "inout") && (find_nesting_level(v) == last_scope->nestingLevel)){
            fprintf(final, "\tmovi R[255], M[%d + R[0]]\n", res->ent.arg.offset);
            fprintf(final, "\tmovi M[R[255]], R[%d]\n", r);
      }
      else if(!strcmp(res->type, "var") || (!strcmp(res->type, "arg") && !strcmp(res->ent.arg.parMode, "in")) && find_nesting_level(v) < last_scope->nestingLevel){
            gnlvcode(res);
            fprintf(final, "\tmovi M[R[255]], R[%d]\n", r);
      }
      else if(!strcmp(res->type, "arg") && !strcmp(res->ent.arg.parMode, "inout") && find_nesting_level(v) < last_scope->nestingLevel){
            gnlvcode(res);
            fprintf(final, "\tmovi R[255], M[R[255]]\n");
            fprintf(final, "\tmovi M[R[255]], R[%d]\n", r);
      }
}

int find_framelength(){
      entity *temp = last_scope->entities;
      int frlength = 12;
      while(temp != NULL){
            if(!strcmp(temp->type, "var") || !strcmp(temp->type, "arg"))
                  frlength += 4;
            temp = temp->next;
      }
      return frlength;
}

void generate_final(){
      int args_counter = 0;
      quad *temp = quadlist;
      quad *temp1;
      int framelength;
      fprintf(final,"     	            \n");
      while(temp != NULL){
            char c;
            c = temp->op[0];
            if(c == ':'){
                  fprintf(final, "L%d:", temp->label);
                  loadvr(temp->arg1, 1);
                  storerv(1, temp->arg3);
            }
            if(!strcmp(temp->op, "print")){
                  fprintf(final, "L%d:", temp->label);
                  loadvr(temp->arg3, 1);
                  fprintf(final, "\touti R[1]\n");
            }
            if(!strcmp(temp->op, "halt")){
                  fprintf(final, "L%d:", temp->label);
                  fprintf(final, "\thalt\n");
            }
            if(!strcmp(temp->op, "input")){
                  fprintf(final, "L%d:", temp->label);
                  fprintf(final, "\tini  R[1]\n");
                  storerv(1, temp->arg3);
            }
            if(c == '+' || c == '*' || c == '-' || c == '/'){
                  char op[5];
                  if(c == '+') strcpy(op, "addi");
                  if(c == '*') strcpy(op, "muli");
                  if(c == '/') strcpy(op, "divi");
                  if(c == '-') strcpy(op, "subi");
                  fprintf(final, "L%d:", temp->label);
                  loadvr(temp->arg1, 1);
                  loadvr(temp->arg2, 2);
                  fprintf(final, "\t%s R[3], R[1], R[2]\n", op);
                  storerv(3,temp->arg3);
            }
            if(!strcmp(temp->op, "jump") && temp != quadlist){
                  fprintf(final, "L%d:", temp->label);
                  fprintf(final, "\tjmp  L%s  \n", temp->arg3);
            }
            if(c == '=' || c == '<' || c == '>'){
                  char relop[4];
                  fprintf(final, "L%d:", temp->label);
                  loadvr(temp->arg1, 1);
                  loadvr(temp->arg2, 2);
                  fprintf(final, "\tcmpi R[1], R[2]\n");
                  if(!strcmp(temp->op, "=" ))    strcpy(relop, "je");
                  if(!strcmp(temp->op, "<>"))    strcpy(relop, "jne");
                  if(!strcmp(temp->op, ">" ))    strcpy(relop, "jb");
                  if(!strcmp(temp->op, ">="))    strcpy(relop, "jbe");
                  if(!strcmp(temp->op, "<" ))    strcpy(relop, "ja");
                  if(!strcmp(temp->op, "<="))    strcpy(relop, "jae");
                  fprintf(final, "\t%s  L%s\n", relop, temp->arg3);
            }
            if(!strcmp(temp->op, "call")){
                  entity *res = search_entity(temp->arg3);
                  int frlength = find_framelength();
                  fprintf(final, "L%d:", temp->label);
                  if(last_scope->nestingLevel == find_nesting_level(temp->arg3)){
                        fprintf(final, "\tmovi R[255], M[4 + R[0]]\n");
                        fprintf(final, "\tmovi M[%d + R[0]], R[255]\n", frlength + 4);
                  }
                  if(last_scope->nestingLevel > find_nesting_level(temp->arg3))
                        fprintf(final, "\tmovi M[%d + R[0]], R[0]\n", frlength + 4);

                  fprintf(final, "\tmovi R[255], %d\n",frlength);
                  fprintf(final, "\taddi R[0], R[255], R[0]\n");
                  fprintf(final, "\tmovi R[255], $\n");
                  fprintf(final, "\tmovi R[254], 15\n");
                  fprintf(final, "\taddi R[255], R[255], R[254]\n");
                  fprintf(final, "\tmovi M[R[0]], R[255]\n");
                  fprintf(final, "\tjmp  L%d\n", res->ent.fun.start_quad);
                  fprintf(final, "\tmovi R[255], %d\n", frlength);
                  fprintf(final, "\tsubi R[0], R[0], R[255]\n");
                  args_counter = 0;
            }
            if(!strcmp(temp->op, "par") && !strcmp(temp->arg2, "CV")){
                  fprintf(final, "L%d:", temp->label);
                  loadvr(temp->arg1, 255);
                  fprintf(final, "\tmovi M[%d + R[0]], R[255]\n", find_framelength() + 12 + (4*args_counter++));
            }
            if(!strcmp(temp->op, "par") && !strcmp(temp->arg2, "RET")){
                  entity *res = search_entity(temp->arg1);
                  fprintf(final, "L%d:", temp->label);
                  fprintf(final, "\tmovi R[255], R[0]\n");
                  fprintf(final, "\tmovi R[254], %d\n", res->ent.arg.offset);
                  fprintf(final, "\taddi R[255], R[254], R[255]\n");
                  fprintf(final, "\tmovi M[%d +R[0]], R[255]\n", find_framelength() + 8);
            }
            if(!strcmp(temp->op, "par") && !strcmp(temp->arg2, "REF")){
                  entity *res = search_entity(temp->arg1);
                  fprintf(final, "L%d:", temp->label);
                  if(find_nesting_level(temp->arg1) == last_scope->nestingLevel){
                        if(!strcmp(res->type, "var") || (!strcmp(res->type, "arg") && !strcmp(res->ent.arg.parMode, "in"))){
                              fprintf(final, "\tmovi R[255], R[0]\n");
                              if(!strcmp(res->type, "var"))
                                    fprintf(final, "\tmovi R[254], %d\n", res->ent.var.offset);
                              else
                                    fprintf(final, "\tmovi R[254], %d\n", res->ent.arg.offset);
                              fprintf(final, "\taddi R[255], R[254], R[255]\n");
                              fprintf(final, "\tmovi M[%d + R[0]], R[255]\n", find_framelength() + 12 + (4*args_counter++));
                        }
                        if(!strcmp(res->ent.arg.parMode, "inout")){
                              fprintf(final, "\tmovi R[255], R[0]\n");
                              fprintf(final, "\tmovi R[254], %d\n", res->ent.arg.offset);
                              fprintf(final, "\taddi R[255], R[254], R[255]\n");
                              fprintf(final, "\tmovi R[1], M[R[255]]");
                              fprintf(final, "\tmovi M[%d + R[0]], R[1]", find_framelength() + 12 + (4*args_counter++));
                        }
                  }
                  if(find_nesting_level(res->name) != last_scope->nestingLevel){
                        if(!strcmp(res->type, "var") || (!strcmp(res->type, "arg") && !strcmp(res->ent.arg.parMode, "in"))){
                              gnlvcode(res);
                              fprintf(final, "\tmovi M[%d + R[0]], R[255]\n", find_framelength() + 12 + (4*args_counter++));
                        }
                        if(!strcmp(res->type, "arg") && !strcmp(res->ent.arg.parMode, "inout")){
                              gnlvcode(res);
                              fprintf(final, "\tmovi R[1], M[R[255]]\n");
                              fprintf(final, "\tmovi M[%d + R[0]], R[1]\n", find_framelength() + 12 + (4*args_counter++));
                        }
                  }

            }
            if(!strcmp(temp->op, "ret")){
                  fprintf(final, "L%d:", temp->label);
                  loadvr(temp->arg1,1);
                  fprintf(final, "\tmovi R[255], M[8+R[0]]\n");
                  fprintf(final, "\tmovi M[R[255]], R[1]\n");
                  fprintf(final, "\tjmp M[R[0]]\n"); /* terminates function when a "return" has been executed" */
            }
            if(!strcmp(temp->op, "end_block") && strcmp(temp->arg1, "program")){
                  fprintf(final, "L%d:", temp->label);
                  fprintf(final, "\tjmp M[R[0]]\n");
            }
            temp1 = temp;
            temp = temp->next;
            free(temp1);
      }
      quadlist = last_node = NULL;
}

int main(int argc, char *argv[]){
	int token_id;
	if(argc != 2)
		exit(1 + printf("NO INPUT FILE\n"));
	infile = fopen(argv[1], "r");
	/*check if the file exists or if it's not a .toy file */
	if(infile == NULL)
		exit(1 + printf("FILE NOT FOUND\n"));

	if(strcmp((char *)&argv[1][strlen(argv[1]) - 4], ".toy"))
		exit(1 + printf("NOT A .toy FILE\n"));

      final = fopen("Final_code.txt", "w");

	token_id = lex();
	program(&token_id);
	generate_final();
	fclose(final);
	fclose(infile);
	puts("Saving final code at 'Final_code.txt' file...");
	return 0;
}
