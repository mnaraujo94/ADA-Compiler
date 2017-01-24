%{

/* Mike Araujo
   ada.y
*/
 extern int lineno;
 int offset = 0;
 int main_start;
 int main_AR_size;
 int jump_table[99];
 int in_exception_part = 0;
 int handler_done = 0;
 int cond_jump;
 int else_if = 0;

%}

%token IS BEG PROCEDURE ID NUMBER TYPE ARRAY RAISE OTHERS QUOTE WHILE FOR REVERSE
%token RECORD IN OUT RANGE CONSTANT ASSIGN EXCEPTION NULLWORD LOOP IF STAR
%token THEN ELSEIF ELSE EXIT WHEN AND OR EQ NEQ LT GT GTE LTE TICK DIVEQ
%token NOT EXP ARROW OF DOTDOT ENDIF ENDREC ENDLOOP EXITWHEN DERIVES COMMA END
%type <integer> NUMBER constant
%type <var> ID type_name mode multiplying_op adding_op relational_op boolean_op exception_handlers when choice_sequence reverse
%type <listptr> identifier_list actual_parameter opt_act_param_part
%type <bin_treeptr> parameters formal_parameter_part
%type <variable> primary factor term simple_expr relation expression opt_assign check_range
%union {
	int integer;
	char *var;
	struct idnode* listptr;
	struct bin_tree* bin_treeptr;
	struct operand *variable;
}

%%
program                  : main_body {printf ("\n*******\nDone.\n*******\n");}
                         ;
main_body                : main_specification IS
                           declarative_part  do_beg
                           sequence_of_statements
                           exception_part do_end
                         ;
main_specification       : PROCEDURE ID  
                         {
							 push($2);
							 printf("pushing new scope for %s\n", entry[count-1].name);
							 offset = 0;
							 entry[count-1].AR_size = 4;
						 }                    
                         ;
procedure_body           : procedure_specification IS
                           declarative_part do_beg
                           sequence_of_statements
                           exception_part do_end 
                         ;
do_beg                   : BEG
                         {
							 struct bin_tree* lookup = name_search(entry[count-1].name);
							 if(lookup == NULL){
								 main_start = instruction_count;
							 }else{
							 lookup->symbol.start_num = instruction_count;
							 lookup->symbol.AR_size = lookup->symbol.AR_size + 4;
							 }
						 } 
                         ;
do_end                   : END ';'
                         {
							 
							 printf("Popping scope for %s \n", entry[count-1].name);
							 int main = strcmp(entry[count-1].name, "main");
							 int inOut;
							 int out;
							 int reg_num = get_reg();
							 if(main != 0){
								 struct bin_tree* lookup = name_search(entry[count-1].name);
								 struct bin_tree* param_list;
								 printf("%s the procedure end\n", entry[count-1].name);
								 if(lookup->symbol.next_param != NULL){
									  	 param_list = lookup->symbol.next_param;
										 while(param_list != NULL){
											 inOut = strcmp(param_list->symbol.mode, "in out");
											 out = strcmp(param_list->symbol.mode, "out");
											 if(inOut == 0 || out == 0){
												 fprintf(ptr_file, "%d r%d := contents b, %d\n", instruction_count, reg_num, param_list->symbol.offset + 1);
												 instruction_count++;
												 fprintf(ptr_file, "%d contents r%d := contents b, %d\n", instruction_count, reg_num, param_list->symbol.offset);
												 instruction_count++;
											 }
											 param_list = param_list->symbol.next_param;
										 }
										 
								 }
							 } 
							 reg_num = get_reg();
							 fprintf(ptr_file, "%d r%d := contents b, 1\n", instruction_count, reg_num, reg_num);
							 instruction_count++;
							 fprintf(ptr_file, "%d b := contents b, 3\n", instruction_count);
							 instruction_count++;
							 fprintf(ptr_file, "%d pc := r%d\n", instruction_count, reg_num);
							 instruction_count++;
							 print_inorder(entry[count-1].root);
							 main_AR_size = entry[count-1].AR_size;
							 count--;
                         }
                         ;
procedure_specification  : PROCEDURE ID formal_parameter_part
                         {
							 struct sym_tab data;
							 push($2);
							 printf("pushing new scope for %s\n", $2);
							 entry[count-1].AR_size = 4;
							 strcpy(data.name, $2);
							 strcpy(data.kind, "procedure");
							 data.Ptype = NULL;
							 data.AR_size = 4;
							 
							 if ($3 != NULL){
							  	 data.next_param = $3;
						     }else{
							 	 data.next_param = NULL;
							 }
							 insert(&entry[count-2].root, data); 
						    
						 }
                         ;
formal_parameter_part    : '(' parameters ')'
                         {
						 	 $$ = $2;
						 }
                         | /*E*/
						 {
 						 	 $$ = NULL;
							 offset = 0;
						 }
                         ;
parameters               : identifier_list ':' mode type_name ';' parameters 
                         {

						 	 struct sym_tab data;
							 struct bin_tree* lookup;
							 int in_out = strcmp($3, "in out");
							 int out = strcmp($3, "out");
							 lookup = name_search(entry[count-1].name);
						 	 print_list($1);
						 	 printf("\n");
						 	 while($1 != NULL){
						 		 strcpy(data.name, $1->name);
						 		 strcpy(data.kind, "param");
						 		 strcpy(data.mode, $3);
						 		 data.Ptype = name_search($4);
						 		 data.next_param = $6;
								 //data.is_local = 1;
								 data.offset = 4 + offset;
								 if(in_out == 0 || out == 0){
									 offset++;
								 }
								 printf("offsets value is %d\n", offset);
								 printf("right before it goes in the tree its name is %s and offset is %d\n", data.name, data.offset);
								 data.size = size_search(data.name);
								 offset = offset + data.size;
								 /* entry[count-1].AR_size = entry[count-1].AR_size + data.size; */
								 /* if(lookup != NULL){ */
								 /* 	 lookup->symbol.AR_size = lookup->symbol.AR_size + data.size; */
								 /* } */

						 		 insert(&entry[count-1].root, data);
						 		 $1 = $1->next;
						 	 }	 
							 $$ = search(&entry[count-1].root, data.name);
                         }
                         | identifier_list ':' mode type_name
                         {	 
							 if(offset != 0){
								 offset = 0;
							 }
						 	 struct sym_tab data;
							 struct bin_tree* lookup;
							 int in_out = strcmp($3, "in out");
							 int out = strcmp($3, "out");
							 lookup = name_search(entry[count-1].name);
						 	 print_list($1);
						 	 printf("\n");
						 	 while($1 != NULL){
						 		 strcpy(data.name, $1->name);
						 		 strcpy(data.kind, "param");
						 		 strcpy(data.mode, $3);
						 		 data.Ptype = name_search($4);
						 		 data.next_param = NULL;
								 data.offset = 4 + offset;
								 //data.is_local = 1;
								 if(in_out == 0 || out == 0){
									 offset++;
								 }
								 printf("offsets value is %d\n", offset);
								 data.size = size_search(data.name);
								 offset = offset + data.size;
								 printf("right before it goes in the tree its name is %s and offset is %d\n", data.name, data.offset);
								 /* entry[count-1].AR_size = entry[count-1].AR_size + data.size; */
								 /* if(lookup != NULL){ */
								 /* 	 lookup->symbol.AR_size = lookup->symbol.AR_size + data.size; */
								 /* } */
						 		 insert(&entry[count-1].root, data);

						 		 $1 = $1->next;

						 	 }
						 	 $$ = search(&entry[count-1].root, data.name);
							 
						 }
                         ;
mode		        	 : IN {strcpy($$, "in"); }
                         | OUT  {strcpy($$, "out"); }
		                 | IN OUT {strcpy($$, "in out"); }
			             | /*E*/ {strcpy($$, "in"); }
			             ;
identifier_list          : ID 
                         {
							 alist = malloc(sizeof(idnode));
							 (*alist).next = NULL;
							 strcpy((*alist).name, $1);
							 $$ = alist;
                         }
			             | ID ',' identifier_list 
						 {
							 alist = malloc(sizeof(idnode));
							 (*alist).next = $3;
							 strcpy((*alist).name, $1);
							 $$ = alist;
						 }
		                 ;
declarative_part	     : sequence_of_decls sequence_of_procs
			             ;
sequence_of_procs        : procedure_body sequence_of_procs 
			             | /*E*/
			             ;
sequence_of_decls        : decl_list sequence_of_decls
			             | /*E*/
			             ;
decl_list                : array_decl ';'
			             | record_decl ';'
			             | named_type_decl ';'
			             | variable_object_decl ';'
			             | constant_variable_decl ';'
			             | exception_decl ';'
			             ;  
array_decl		         : TYPE ID IS ARRAY '(' constant DOTDOT constant ')' OF type_name
                         {
							 struct sym_tab data;
							 struct bin_tree* lookup;
							 int cmp;
							 int duplicate_val = duplicate(entry[count-1].root, $2);
							 if(!duplicate_val){
								 lookup = search(&entry[0].root, $11);
								 if (lookup == NULL){
									 printf("error invalid type\n");
								 }else{
									 cmp = strcmp(lookup->symbol.kind, "type");
									 if(cmp != 0){
										 printf("error invalid type\n");
									 }else{
										 strcpy(data.name, $2);
										 strcpy(data.kind, "array");
										 data.Ptype = NULL;
										 data.up_bound = $8;
										 data.low_bound = $6;
										 data.size = $8 - $6 + 1;
										 data.comp_type = lookup;
										 insert(&entry[count-1].root, data);
									 }
								 }
							 }
						 }
			             ;
constant		         : ID
                         {
							 $$ = 0;
						 }
			             | NUMBER
                         {
							 $$ = $1;
						 }
			             ;
record_decl		         : TYPE ID IS RECORD component_list END RECORD
                         {
							 struct sym_tab data;
							 //int duplicate_val; FINSIH DUPLICATE STUFF
							 strcpy(data.name, $2);
							 strcpy(data.kind, "record");
							 data.Ptype = NULL;
							 data.offset = 4 + offset;
							 duplicate(entry[count-1].root, $2);
							 insert(&entry[count-1].root, data);
                         }
			             ;
component_list		     : variable_object_decl ';' component_list
			             | variable_object_decl ';'
			             ;
named_type_decl	 	     : TYPE ID IS RANGE constant DOTDOT constant 
                         {
							 struct sym_tab data;
							 //int duplicate_val;		 //int duplicate_val; FINSIH DUPLICATE STUFF
							 strcpy(data.name,$2);
							 strcpy(data.kind, "named type");
							 data.Ptype = NULL;
							 data.offset = offset;
							 data.up_bound = $7;
							 data.low_bound = $5;
							 duplicate(entry[count-1].root, $2);
							 insert(&entry[count-1].root, data);
                         }
		                 ;
variable_object_decl     : identifier_list ':' type_name 
                         {
			     			 struct idnode* start = NULL;							
							 struct sym_tab data;
							 struct bin_tree* lookup;
							 lookup = name_search(entry[count-1].name);
							 int i = count - 1;
							 start = $1;
							 int duplicate_val;
						     while($1 != NULL){
								 duplicate_val = duplicate(entry[count-1].root, $1->name);
							     if(!duplicate_val){
									 strcpy(data.name, $1->name);
									 strcpy(data.kind, "object");
									 data.Ptype = name_search($3);
									 data.offset = 4 + offset;
									 data.size = size_search(data.name);
									 offset = offset + data.size;
									 entry[count-1].AR_size = entry[count-1].AR_size + data.size;
									 if(lookup != NULL){
       									 lookup->symbol.AR_size = lookup->symbol.AR_size + data.size;
									 }
									 insert(&entry[count-1].root, data);
								 }
								 $1 = $1->next;
							 } 
							 if(!duplicate_val){
							 printf("line#: %d - ", lineno); print_list(start);
							 printf(" : %s", data.Ptype->symbol.name);
							 printf("\n");
							 }
						 }
			             ;
type_name		         : ID
                         {
							 $$ = $1;
						 }
			             ;
constant_variable_decl   : identifier_list ':' CONSTANT ASSIGN constant_expression 
                         {
							 printf("line %d ", lineno); 
							 print_list($1);
						 }
			             ;
constant_expression	     : constant
                         ;
exception_decl		     : identifier_list ':' EXCEPTION 
                         {
							 printf("line %d, ", lineno);
							 print_list($1);
							 printf("\n");
							 struct sym_tab data;
							 int duplicate_val;
							  while($1 != NULL){
							 	 duplicate_val = duplicate(entry[count-1].root, $1->name);
							     if(!duplicate_val){
							 		 strcpy(data.name, $1->name);
							 		 strcpy(data.kind, "exception");
							 		 data.ival = next_exception;
							 		 next_exception++;
							 		 insert(&entry[count-1].root, data);
							 	 }
							 	 $1 = $1->next;
							 } 
							 
						 }
			             ;
sequence_of_statements   : statement_list sequence_of_statements
			             | /*E*/
			             ;
statement_list           : NULLWORD ';'
			             | assign_or_proc ';' 
			             | loop ';'
			             | if ';'
			             | raise_exception ';'
						 | raise ';'
						 | while ';'
                         | for_loop ';'
			             ;
assign_or_proc           : ID opt_act_param_part opt_assign
                         {

							 struct bin_tree* check_kind;
							 check_kind = name_search($1);
							 
							 int cmp;
							 int cmp_read, cmp_write, cmp_bool;
							  
							 cmp_read = strcmp($1, "read");
							 cmp_write = strcmp($1, "write");
							 cmp = strcmp(check_kind->symbol.kind, "procedure");
							 
							 if(cmp == 0 || cmp_read == 0 || cmp_write == 0){
								 
							 	 struct operand* variable = (struct operand*)malloc(sizeof(struct operand));
							 	 struct bin_tree* lookup = name_search($1);
							 	 struct bin_tree* lookup2;
							 	 if($2 != NULL){
							 		 lookup2 = name_search($2->variable.name);
							 	 }
							 	 int level;
							 	 int reg_num;
							 	 //check outter context for read and write, if it is one of these then dont do any of the stuff below				 	 
							 	 if(cmp_read == 0){
							 		 if($2 == NULL){
							 			 printf("ERROR NO CONTENTS TO READ\n");
							 		 }
									 level = level_search($2->variable.name);
							 		 if (level != 0){
							 			 reg_num = get_reg();
							 			 fprintf(ptr_file, "%d r%d := contents b, 2\n", instruction_count, reg_num);
							 			 instruction_count++;
							 			 while (level > 1){
							 				 fprintf(ptr_file, "%d r%d := contents r%d, 2\n", instruction_count, reg_num, reg_num);
							 				 instruction_count++;
							 				 level--;
							 			 }
							 			 cmp_bool = strcmp(lookup2->symbol.Ptype->symbol.name, "boolean");
							 			 if(cmp_bool == 0){
							 				 fprintf(ptr_file, "%d read_boolean contents r%d, %d\n", instruction_count, reg_num, $2->variable.offset);
							 			 }else{
							 				 fprintf(ptr_file, "%d read_integer contents r%d, %d\n", instruction_count, reg_num, $2->variable.offset);
							 			 }
							 			 instruction_count++;
							 		 }else{
							 			 cmp_bool = strcmp(lookup2->symbol.Ptype->symbol.name, "boolean");
							 			 if(cmp_bool == 0){
							 				 fprintf(ptr_file, "%d read_boolean contents b, %d\n", instruction_count, $2->variable.offset);
							 			 }else{
							 				 fprintf(ptr_file, "%d read_integer contents b, %d\n", instruction_count, $2->variable.offset);
							 			 }
										 instruction_count++;
							 		 }
							 	 }else if(cmp_write == 0){
							 		 if($2 == NULL){
							 			 printf("ERROR NO CONTENTS TO WRITE\n");
							 		 }
							 		 level = level_search($2->variable.name);
							 		 if(level != 0){
							 			 reg_num = get_reg();
							 			 fprintf(ptr_file, "%d r%d := contents b, 2\n", instruction_count, reg_num);
							 			 instruction_count++;
							 			 while (level > 1){
							 				 fprintf(ptr_file, "%d r%d := contents r%d, 2\n", instruction_count, reg_num, reg_num);
							 				 instruction_count++;
							 				 level--;
							 			 }
							 		 }

							 		 while($2 != NULL){
							 			 fprintf(ptr_file, "%d write r%d\n", instruction_count, $2->variable.reg_num);
							 			 instruction_count++;
							 			 $2 = $2->next;
							 		 }
							 	 }




								 else{
									 
							 		 level = level_search($1);
							 		 variable->reg_num = get_reg();
							 		 fprintf(ptr_file, "%d r%d := b\n", instruction_count, variable->reg_num);
							 		 instruction_count++;
							 		 fprintf(ptr_file, "%d b := contents r%d, 0\n", instruction_count, variable->reg_num);
							 		 instruction_count++;
							 		 fprintf(ptr_file, "%d contents b, 3 := r%d\n", instruction_count, variable->reg_num);
							 		 instruction_count++;
							 		 if (level == 0){
							 			 fprintf(ptr_file, "%d contents b, 2 := r%d\n", instruction_count, variable->reg_num);
							 			 instruction_count++;
							 		 }else{
							 			 reg_num = get_reg();
							 			 fprintf(ptr_file, "%d r%d := contents r%d, 2\n", instruction_count, reg_num, variable->reg_num);
							 			 instruction_count++;
							 			 level--;
							 			 while(level > 0){
							 				 fprintf(ptr_file, "%d r%d := contents r%d, 2\n", instruction_count, reg_num, reg_num);
							 				 instruction_count++;
							 				 level--;
							 			 }
							 			 fprintf(ptr_file, "%d contents b, 2 := r%d\n", instruction_count, reg_num);
							 			 instruction_count++;
							 		 }

							 		 int next_base_reg = get_reg();
									 int out_mode;
									 int in_out_mode;
									  struct bin_tree* param_list = lookup->symbol.next_param;
									  while(param_list != NULL){
									 	  lookup->symbol.AR_size++;
									 	  out_mode = strcmp(param_list->symbol.mode, "out");
									 	  in_out_mode = strcmp(param_list->symbol.mode, "in out");
									 	  if(out_mode == 0 || in_out_mode == 0){
									 		  lookup->symbol.AR_size++;
									 	  }
									 	  param_list = param_list->symbol.next_param;
										  
									  }
							 		 fprintf(ptr_file, "%d r%d := %d\n", instruction_count, next_base_reg, lookup->symbol.AR_size);
							 		 instruction_count++;
							 		 fprintf(ptr_file, "%d contents b, 0 := b + r%d\n", instruction_count, next_base_reg);
							 		 instruction_count++;
									 struct idnode* params = $2;
									 param_list = lookup->symbol.next_param;
									 int in;
									 int inOut;
									 int out;
									 while(param_list != NULL){
									 	 out = strcmp(param_list->symbol.mode, "out");
									 	 inOut = strcmp(param_list->symbol.mode, "in out");
										 printf("%s is the name of the param its offset is %d\n", param_list->symbol.name, param_list->symbol.offset);
									 		 if(params->variable.is_var == 0){
									 			 fprintf(ptr_file, "%d contents b, %d := r%d\n", instruction_count, param_list->symbol.offset, params->variable.reg_num);
									 			 instruction_count++;
									 		 }else{
									 			 if(params->variable.is_local == 1){
									 				 fprintf(ptr_file, "%d contents b, %d := contents r%d, %d\n", instruction_count, param_list->symbol.offset, variable->reg_num, params->variable.offset);
									 				 instruction_count++;
									 			 }else{
									 				 fprintf(ptr_file, "%d contents b, %d := contents r%d, 4\n", instruction_count, param_list->symbol.offset, params->variable.reg_num, params->variable.offset);
									 				 instruction_count++;
									 			 }
									 		 }
									 	 	 if(out == 0 || inOut == 0){
									 		 int offset_reg = get_reg();
									 		 int address_reg = get_reg();
											 int level;
											 int reg_num = get_reg();
									 		 fprintf(ptr_file, "%d r%d := %d\n", instruction_count, offset_reg, params->variable.offset);
									 		 instruction_count++;
									 		 if(params->variable.is_local == 1){
									 			 fprintf(ptr_file, "%d r%d := r%d + r%d\n", instruction_count, address_reg, variable->reg_num, offset_reg);
									 			 instruction_count++;
									 		 }else{
												 level = level_search(params->variable.name);
												 if(level != 0){
												 	 reg_num = get_reg();
												 	 fprintf(ptr_file, "%d r%d := contents b, 2\n", instruction_count, reg_num);
												 	 instruction_count++;
												 	 while (level > 1){
												 		 fprintf(ptr_file, "%d r%d := contents r%d, 2\n", instruction_count, reg_num, reg_num);
												 		 instruction_count++;
												 		 level--;
												 	 }
												 }
									 			 fprintf(ptr_file, "%d r%d := r%d + r%d\n", instruction_count, address_reg, reg_num, offset_reg);
									 			 instruction_count++;
									 		 }
									 		 int offset_param = param_list->symbol.offset;
									 		 /* if(inOut == 0){ */
									 		 /* 	 offset_param = offset_param + param_list->symbol.size; */
									 		 /* } */
									 		 fprintf(ptr_file, "%d contents b, %d := r%d\n", instruction_count,  param_list->symbol.offset + 1, address_reg);
									 		 instruction_count++;

									 		 }
									 
									 	 params = params->next;
									 	 param_list = param_list->symbol.next_param;
									 }
								 
							 
										 
							 		 int fix_return_address = get_reg();
							 		 fprintf(ptr_file, "%d r%d := %d\n", instruction_count, fix_return_address, instruction_count + 3);
							 		 instruction_count++;
							 		 fprintf(ptr_file, "%d contents b, 1 := r%d\n", instruction_count, fix_return_address);
							 		 instruction_count++;
							 		 fprintf(ptr_file, "%d pc := %d\n", instruction_count, lookup->symbol.start_num);
							 		 instruction_count++;
									 fprintf(ptr_file, "%d PC := ? if r1\n", instruction_count);
									 struct idnode* raise_ptr;
									 raise_ptr = raise_list;
									 if(raise_ptr == NULL){
										 raise_list = malloc(sizeof(idnode));
										 raise_list->curr_instruction = instruction_count;
										 raise_list->next = NULL;
									 }else{
										 while(raise_ptr->next != NULL){
											 raise_ptr = raise_ptr->next;
										 }
										 raise_ptr->next = malloc(sizeof(idnode));
										 raise_ptr->next->curr_instruction = instruction_count;
										 raise_ptr->next->next = NULL;
									 }
									 instruction_count++;
									 printf("printing list after proc ends ");
									 print_list(raise_list);
							 	 }



							 }else{
								 
							 	 struct bin_tree* lookup_assign_right_array;
								 struct bin_tree* lookup_assign;
								 struct bin_tree* var_value;
							 	 struct operand* variable1 = (struct operand*)malloc(sizeof(struct operand));
							 	 struct operand* variable2 = (struct operand*)malloc(sizeof(struct operand));
							 	 int level;
								 int cmp_array_id;
								 if($3 != NULL){		 
									 cmp_array_id = strcmp($3->array_id, "x");
									 if(cmp_array_id != 0){
										 //printf("about to do  array on right\n\n");
										 lookup_assign_right_array = name_search($3->array_id);	
										 level = level_search($3->array_id);
										 if(level != 0){									
											 variable2->reg_num = get_reg();
											 variable2->is_local = 0;
											 fprintf(ptr_file, "%d r%d := contents b, 2\n", instruction_count, variable2->reg_num);
											 instruction_count++;
											 while (level > 1){
												 fprintf(ptr_file, "%d r%d := contents r%d, 2\n", instruction_count, variable2->reg_num, variable2->reg_num);
												 instruction_count++;
												 level--;
											 }
										 }else{
											 variable2->is_local = 1;
										 }
										 
									 }

									 lookup_assign = name_search($1);
									 int cmp_array = strcmp(lookup_assign->symbol.Ptype->symbol.kind, "array");
									 int cmp_name_type = strcmp(lookup_assign->symbol.Ptype->symbol.kind, "named type");
									 if($3->is_var == 0){
									 	 if(cmp_name_type == 0){
									 		 if ($3->value < lookup_assign->symbol.Ptype->symbol.low_bound || $3->value > lookup_assign->symbol.Ptype->symbol.up_bound){
									 			 fprintf(ptr_file, "error r%d is out of range\n", $3->reg_num);
									 		 }
									 	 }
									 }else{
										 if(cmp_name_type == 0){
											 var_value = name_search($3->name);
											 if(var_value->symbol.value < lookup_assign->symbol.Ptype->symbol.low_bound || var_value->symbol.value > lookup_assign->symbol.Ptype->symbol.up_bound){
												 fprintf(ptr_file, "error r%d is out of range\n", $3->reg_num);
											 }
										 }
									 }
									 struct bin_tree* lookup = name_search($1);
									 int kind = strcmp(lookup->symbol.kind, "param");
										 if(kind != 0){
											 level = level_search($1);
											 if(level != 0){
												 variable1->reg_num = get_reg();
												 variable1->is_local = 0;
												 fprintf(ptr_file, "%d r%d := contents b, 2 \n", instruction_count, variable1->reg_num);
												 instruction_count++;
												 while (level > 1){
													 fprintf(ptr_file, "%d r%d := contents r%d, 2\n", instruction_count, variable1->reg_num, variable1->reg_num);
													 instruction_count++;
													 level--;
												 }
											 }else{
												 variable1->is_local = 1;
											 }
										 }else{
											 variable1->is_local = 1;
										 }
									 
								
									 //printf("left side offset is %d \n\n", lookup_assign->symbol.offset);
									 //printf("right side offset is %d \n\n", lookup_assign_right_array->symbol.offset);
									 if(cmp_array_id != 0){
										 variable2->offset = lookup_assign_right_array->symbol.offset + $3->value - lookup_assign_right_array->symbol.Ptype->symbol.low_bound;
										 variable1->is_var = 1;
										 variable2->is_var = 1;
										 variable1->extra = 0;
										 variable2->extra = 0;
										 if($3->is_var == 1 && cmp_array_id != 0){
										 	 variable2->extra = $3->reg_num;
										 }
										 //printf("%d\n\n", lookup_assign_right_array->symbol.Ptype->symbol.low_bound);
										 //printf("%s\n\n", lookup_assign_right_array->symbol.Ptype->symbol.name);
										 cmp = strcmp(lookup_assign->symbol.Ptype->symbol.kind, "array");
										 if(cmp == 0){
											 if($2->variable.is_var == 0){
												 
											 	 if(cmp_array == 0){
											 		 if ($2->variable.value < lookup_assign->symbol.Ptype->symbol.low_bound || $2->variable.value > lookup_assign->symbol.Ptype->symbol.up_bound){
											 			 fprintf(ptr_file, "error r%d is out of range\n", $2->variable.reg_num);
											 		 }
											 	 }
											 }else{
												 var_value = name_search($2->variable.name);
												 if(var_value->symbol.value < lookup_assign->symbol.Ptype->symbol.low_bound || var_value->symbol.value > lookup_assign->symbol.Ptype->symbol.up_bound){
													 fprintf(ptr_file, "error r%d is out of range\n", $2->variable.reg_num);
												 }
											 }
											 variable1->offset = lookup_assign->symbol.offset + $2->variable.value - lookup_assign->symbol.Ptype->symbol.low_bound;												   if($2->variable.is_var == 1 && cmp == 0){
												 variable1->extra = $2->variable.reg_num;
											 }
										 }else{
											 variable1->offset = lookup_assign->symbol.offset;
										 }
										 if($3->is_var == 0){
										 lookup_assign->symbol.value = $3->value;
										 }
										 emit_assign(variable1, variable2);
									 }else{ 
										 cmp = strcmp(lookup_assign->symbol.Ptype->symbol.kind, "array");
										 if(cmp == 0){
											 if($2->variable.is_var == 0){
												 
											 	 if(cmp_array == 0){
											 		 if ($2->variable.value < lookup_assign->symbol.Ptype->symbol.low_bound || $2->variable.value > lookup_assign->symbol.Ptype->symbol.up_bound){
											 			 fprintf(ptr_file, "error r%d is out of range\n", $2->variable.reg_num);
											 		 }
											 	 }
											 }else{
												 var_value = name_search($2->variable.name);
												 if(var_value->symbol.value < lookup_assign->symbol.Ptype->symbol.low_bound || var_value->symbol.value > lookup_assign->symbol.Ptype->symbol.up_bound){
													 fprintf(ptr_file, "error r%d is out of range\n", $2->variable.reg_num);
												 }
											 }
											 
											 variable1->offset = lookup_assign->symbol.offset + $2->variable.value - lookup_assign->symbol.Ptype->symbol.low_bound;
											 variable1->extra = 0;
											 if($2->variable.is_var == 1 && cmp == 0){
												 variable1->extra = $2->variable.reg_num;
											 }
										 }else{
											 variable1->offset = lookup_assign->symbol.offset;
											 variable1->extra = 0;
										 }
										 variable1->is_var = 1;
										 if($3->is_var == 0){
										 lookup_assign->symbol.value = $3->value;
										 }
										 emit_assign(variable1, $3);
									 }
							 }
						 }
						 }

								 
                         ;
opt_assign		         : ASSIGN expression
                         {
							 $$ = $2;
							 strcpy($$->array_id, "x");
						 }
                         | ASSIGN ID '(' expression ')'
						 {
							 $$ = $4;
							 strcpy($$->array_id, $2);
							 struct bin_tree* range = name_search($2);
							 struct bin_tree* var_value;
							 printf("$4 -> vlaue is %d\n\n\n", $4->is_var);
							 if($4->is_var == 1){
								 var_value = name_search($4->name);
								 if(var_value->symbol.value < range->symbol.Ptype->symbol.low_bound || var_value->symbol.value > range->symbol.Ptype->symbol.up_bound){
									 fprintf(ptr_file, "error r%d is out of range\n", $4->reg_num);
								 }
							 }else{
								 if($4->value < range->symbol.Ptype->symbol.low_bound || $4->value > range->symbol.Ptype->symbol.up_bound){
									 fprintf(ptr_file, "error r%d is out of range\n", $4->reg_num);
								 }
							 }
						 }
                         |/*E*/
                         {
							 $$ = NULL;
						 }
			             ;
loop			         : beg_loop loop_body end_loop
			             ; 
beg_loop                 : LOOP
                        {
							 loop[loop_count] = malloc(sizeof(idnode));
							 loop[loop_count]->curr_instruction = instruction_count;
							 loop[loop_count]->exit = 0;
							 loop[loop_count]->next = NULL;
							 loop_count++;
						 }
                         ;
end_loop                 : END LOOP
                         {
							 fprintf(ptr_file, "%d PC := %d\n", instruction_count, loop[loop_count-1]->curr_instruction);
							 struct idnode* patch_ptr;
							 struct idnode* semantic_ptr;
							 patch_ptr = patch_list;
							 semantic_ptr = loop[loop_count - 1];
							 while(patch_ptr->next != NULL){
							 	 patch_ptr = patch_ptr->next;
							 }
							 if(semantic_ptr->exit != 1){
							 	 semantic_ptr = semantic_ptr->next;
							 }
							 while(semantic_ptr != NULL){
							 	 patch_ptr->curr_instruction = semantic_ptr->curr_instruction;
							 	 patch_ptr->jump_instruction = instruction_count + 1;
							 	 semantic_ptr = semantic_ptr->next;
							 	 patch_ptr->next = malloc(sizeof(idnode));
							 	 patch_ptr->next->next = NULL;
							 	 patch_ptr = patch_ptr->next;
							 }
							 instruction_count++;
							 loop_count--;
						 }
                         ;
loop_body                : statement_list loop_body
                         | statement_list
                         | exit loop_body
                         | exit
                         ;
exit			         : EXIT ';'
                         {
							 fprintf(ptr_file, "%d PC := ?\n", instruction_count);
							 struct idnode* loop_ptr;
							 loop_ptr = loop[loop_count-1];
							 while(loop_ptr->next != NULL){
							 	 loop_ptr = loop_ptr->next;
							 }
							 loop_ptr->next = malloc(sizeof(idnode));
							 loop_ptr->next->exit = 1;
							 loop_ptr->next->curr_instruction = instruction_count;
							 loop_ptr->next->next = NULL;
							 instruction_count++;
						 }
			             | EXIT WHEN expression ';'
                         {
							 fprintf(ptr_file, "%d PC := ? if r%d\n", instruction_count, $3->reg_num);
							 struct idnode* loop_ptr;
							 loop_ptr = loop[loop_count-1];
							 while(loop_ptr->next != NULL){
							 	 loop_ptr = loop_ptr->next;
							 }
							 loop_ptr->next = malloc(sizeof(idnode));
							 loop_ptr->next->exit = 1;
							 loop_ptr->next->curr_instruction = instruction_count;
							 loop_ptr->next->next = NULL;
							 instruction_count++;
						 }
			             ;
while                    : begin_while check_expr LOOP loop_body end_loop
                         ;
begin_while              : WHILE 
{ 
							 loop[loop_count] = malloc(sizeof(idnode));
							 loop[loop_count]->curr_instruction = instruction_count;
							 loop[loop_count]->exit = 0;
							 loop[loop_count]->next = NULL;
							 loop_count++;
}
                         ;
check_expr               : expression
                         {
							 fprintf(ptr_file, "%d PC := ? if not r%d\n", instruction_count, $1->reg_num);
							 struct idnode* loop_ptr;
							 loop_ptr = loop[loop_count-1];
							 while(loop_ptr->next != NULL){
							 	 loop_ptr = loop_ptr->next;
							 }
							 loop_ptr->next = malloc(sizeof(idnode));
							 loop_ptr->next->exit = 1;
							 loop_ptr->next->curr_instruction = instruction_count;
							 loop_ptr->next->next = NULL;
							 instruction_count++;
						 } 
for_loop                 : begin_for check_range LOOP loop_body END LOOP
                         {
						 	 if($2->reverse == 0){
								 fprintf(ptr_file, "%d r%d := r%d + 1\n", instruction_count, $2->reg_num, $2->reg_num);
							 }else{
								  fprintf(ptr_file, "%d r%d := r%d - 1\n", instruction_count, $2->reg_num, $2->reg_num);
							 }
							 instruction_count++;
							 fprintf(ptr_file, "%d PC := %d\n", instruction_count, loop[loop_count-1]->curr_instruction + 4);
							 struct idnode* patch_ptr;
							 struct idnode* semantic_ptr;
							 patch_ptr = patch_list;
							 semantic_ptr = loop[loop_count - 1];
							 while(patch_ptr->next != NULL){
							 	 patch_ptr = patch_ptr->next;
							 }
							 if(semantic_ptr->exit != 1){
							 	 semantic_ptr = semantic_ptr->next;
							 }
							 while(semantic_ptr != NULL){
							 	 patch_ptr->curr_instruction = semantic_ptr->curr_instruction;
							 	 patch_ptr->jump_instruction = instruction_count + 1;
							 	 semantic_ptr = semantic_ptr->next;
							 	 patch_ptr->next = malloc(sizeof(idnode));
							 	 patch_ptr->next->next = NULL;
							 	 patch_ptr = patch_ptr->next;
							 }
							 instruction_count++;
							 loop_count--;
						 }
                         ;
begin_for                : FOR
                         {  
							 loop[loop_count] = malloc(sizeof(idnode));
							 loop[loop_count]->curr_instruction = instruction_count;
							 loop[loop_count]->exit = 0;
							 loop[loop_count]->next = NULL;
							 loop_count++;
                         }
                         ;
check_range              : expression IN reverse primary DOTDOT primary 
                         {
								 if($3 != NULL){
									 fprintf(ptr_file, "%d r%d := r%d\n", instruction_count, $1->reg_num, $6->reg_num);
									 instruction_count++;
									 fprintf(ptr_file, "%d PC := ? if r%d < r%d\n", instruction_count, $4->reg_num, $1->reg_num);
									 $1->reverse = 1;
								 }else{
									 fprintf(ptr_file, "%d r%d := r%d\n", instruction_count, $1->reg_num, $4->reg_num);
									 instruction_count++;
									 fprintf(ptr_file, "%d PC := ? if r%d < r%d\n", instruction_count, $1->reg_num, $6->reg_num);
									 $1->reverse = 0;
								 }
								 struct idnode* loop_ptr;
								 loop_ptr = loop[loop_count-1];
								 //printf("%d loop int count\n\n", loop_ptr->curr_instruction);
								 while(loop_ptr->next != NULL){
									 loop_ptr = loop_ptr->next;
								 }
								 loop_ptr->next = malloc(sizeof(idnode));
								 loop_ptr->next->exit = 1;
								 loop_ptr->next->curr_instruction = instruction_count;
								 loop_ptr->next->next = NULL;
								 instruction_count++;
								 $$ = $1;
						 }
                         ;
reverse                  : REVERSE
                         {
							 strcpy($$, "reverse");
						 }
                         | /*E*/
						 {							 			   
							 $$ = NULL;
						 } 
                         ;
if			             : begin_if body_of_statements elsif_body else_body end_if
			             ;
begin_if                 : IF expression THEN
                         {
							 if_stack[if_count] = malloc(sizeof(idnode));
							 if_stack[if_count]->curr_instruction = instruction_count;
							 if_stack[if_count]->exit = 0;
							 if_stack[if_count]->next = NULL;
							 if_count++;
							 else_if = 0;
							 if($2->is_bool == 1){
								 fprintf(ptr_file, "%d PC := ? if not %s\n", instruction_count, $2->name);

							 }else{
								 
							 fprintf(ptr_file, "%d PC := ? if not r%d\n", instruction_count, $2->reg_num);
							 }
							 instruction_count++;
						 }
                         ;
end_if                   : END IF
                         {
							 struct idnode* patch_ptr;
							 struct idnode* semantic_ptr;
							 patch_ptr = patch_list;
							 semantic_ptr = if_stack[if_count - 1];
							 print_list(if_stack[if_count-1]);
							 while(patch_ptr->next != NULL){
							 	 patch_ptr = patch_ptr->next;
							 }

							 while(semantic_ptr != NULL){
								 if(semantic_ptr->exit == 0 && else_if == 1){
									 semantic_ptr = semantic_ptr->next;
								 }else{
								 patch_ptr->curr_instruction = semantic_ptr->curr_instruction;
								 patch_ptr->jump_instruction = instruction_count;
								 semantic_ptr = semantic_ptr->next;
								 patch_ptr->next = malloc(sizeof(idnode));
								 patch_ptr->next->next = NULL;
								 patch_ptr = patch_ptr->next;
								 }
							 }
							 else_if == 0;
							 if_count--;
						 }
                         ;
elsif_body      		 : elsif THEN body_of_statements elsif_body 
                         | /*E*/
			             ;
elsif                    : ELSEIF expression
                         {
							 struct idnode* if_ptr;
							 struct idnode* tail;
							 else_if = 1;
							 if_ptr = if_stack[if_count-1]->next;
							 tail = if_stack[if_count-1];
							 while(if_ptr->next != NULL){
							 	 if_ptr = if_ptr->next;
								 tail = tail->next;
							 }
							 struct idnode* patch_ptr;
							 patch_ptr = patch_list;
							 while(patch_ptr->next != NULL){
							 	 patch_ptr = patch_ptr->next;
							 }
							 patch_ptr->curr_instruction = tail->curr_instruction;
							 patch_ptr->jump_instruction = cond_jump + 1;
							 patch_ptr->next = malloc(sizeof(idnode));
							 patch_ptr->next->next = NULL;
							 
							 if_ptr->next = malloc(sizeof(idnode));
							 if_ptr->next->curr_instruction = instruction_count;
							 if_ptr->next->exit = 0;
							 if_ptr->next->next = NULL;
							 fprintf(ptr_file, "%d PC := ? if not r%d\n", instruction_count, $2->reg_num);
							 instruction_count++;
                         }

                         ;
else_body                : ELSE sequence_of_statements
                         {
							 struct idnode* if_ptr;
							 struct idnode* tail;
							 else_if = 1;
							 if_ptr = if_stack[if_count-1]->next;
							 tail = if_stack[if_count-1];
							 while(if_ptr->next != NULL){
							 	 if_ptr = if_ptr->next;
							 	 tail = tail->next;
							 }
							 struct idnode* patch_ptr;
							 patch_ptr = patch_list;
							 while(patch_ptr->next != NULL){
							 	 patch_ptr = patch_ptr->next;
							 }
							 patch_ptr->curr_instruction = tail->curr_instruction;
							 patch_ptr->jump_instruction = cond_jump + 1;
							 patch_ptr->next = malloc(sizeof(idnode));
							 patch_ptr->next->next = NULL;
							 
							 if_ptr->next = malloc(sizeof(idnode));
							 if_ptr->next->curr_instruction = instruction_count;
							 if_ptr->next->exit = 0;
							 if_ptr->next->next = NULL;
						 }
						 | /*E*/
						 ;
body_of_statements       : sequence_of_statements
                         {
							 
							 fprintf(ptr_file, "%d PC := ? \n", instruction_count);
							 struct idnode* if_ptr;
							 if_ptr = if_stack[if_count-1];
							 while(if_ptr->next != NULL){
							 	 if_ptr = if_ptr->next;
							 }
							 if_ptr->next = malloc(sizeof(idnode));
							 if_ptr->next->curr_instruction = instruction_count;
							 if_ptr->next->exit = 1;
							 if_ptr->next->next = NULL;
							 cond_jump = instruction_count;
							 instruction_count++;
						 }							 
                         ;
raise_exception		     : RAISE ID
                         {
							 struct bin_tree* lookup = name_search($2);
							 int cmp = strcmp(lookup->symbol.kind, "exception");
							 struct idnode* raise_ptr;
							 raise_ptr = raise_list;
							 if (cmp != 0){
							 	 fprintf(ptr_file, "error no exception\n");
							 }else{
							 	 fprintf(ptr_file, "%d r1 := %d\n", instruction_count, lookup->symbol.ival);
							 	 instruction_count++;
							 	 fprintf(ptr_file, "%d PC := ?\n", instruction_count);

								 if(raise_ptr == NULL){
									 raise_list = malloc(sizeof(idnode));
									 raise_list->curr_instruction = instruction_count;
									 raise_list->next = NULL;
								 }else{
									 while(raise_ptr->next != NULL){
										 raise_ptr = raise_ptr->next;
									 }
									 raise_ptr->next = malloc(sizeof(idnode));
									 raise_ptr->next->curr_instruction = instruction_count;
									 raise_ptr->next->next = NULL;
								 }
								 instruction_count++;
							 }
						 }
								 
			             ;
raise                    : RAISE
                         {
							 struct idnode* raise_ptr;
							 raise_ptr = raise_list;
							 if(in_exception_part != 1){
							 	 fprintf(ptr_file, "error not in an exception\n");
							 }else{
							 	 fprintf(ptr_file, "%d PC := ?\n", instruction_count);
								 if(raise_ptr == NULL){
									 raise_list = malloc(sizeof(idnode));
									 raise_list->curr_instruction = instruction_count;
									 raise_list->next = NULL;
								 }else{
									 while(raise_ptr->next != NULL){
										 raise_ptr = raise_ptr->next;
									 }
									 raise_ptr->next = malloc(sizeof(idnode));
									 raise_ptr->next->curr_instruction = instruction_count;
									 raise_ptr->next->next = NULL;
								 }
							 }
							 instruction_count++;

						 }	 
                         ;
			             ;
opt_act_param_part       : '(' actual_parameter ')'
                         {
							 $$ = $2;
						 }
			             | /*E*/ 
                         {
							 $$ = NULL;
						 }
							 
			             ;
actual_parameter         : expression ',' actual_parameter // when these are actual parameters I need a conditional
                         {
							 alist = malloc(sizeof(idnode));
							 (*alist).next = $3;
							 alist->variable.offset = $1->offset;
							 alist->variable.is_local = $1->is_local;
							 alist->variable.reg_num = $1->reg_num;
							 alist->variable.is_var = $1->is_var;
							 //alist->variable.mode = $1->mode;
							 if($1->is_var == 0){
								 alist->variable.value = $1->value;
							 }
							 alist->variable.is_bool = $1->is_bool;
							 strcpy(alist->variable.not, $1->not);
							 strcpy(alist->variable.unary_minus, $1->unary_minus);
							 strcpy(alist->variable.name, $1->name);
							 $$ = alist;
						 }
			             | expression
                         {
							 alist = malloc(sizeof(idnode));
							 (*alist).next = NULL;
							 alist->variable.offset = $1->offset;
							 alist->variable.is_local = $1->is_local;
							 alist->variable.reg_num = $1->reg_num;
							 alist->variable.is_var = $1->is_var;
							 alist->variable.is_bool = $1->is_bool;
							 if($1->is_var == 0){
							 alist->variable.value = $1->value;
							 }
							 strcpy(alist->variable.not, $1->not);
							 strcpy(alist->variable.unary_minus, $1->unary_minus);
							 strcpy(alist->variable.name, $1->name);
							 $$ = alist;
						 }
							 
     
			             ;
expression		         : expression boolean_op relation
                         {
							 $$ = math($1, $2, $3);
						 }
			             | relation
                         {
							 $$ = $1;
						 }
			             ;
relation		         : relation relational_op simple_expr
                         {
							 $$ = math($1, $2, $3);
						 }
			             | simple_expr
                         {
							 $$ = $1;
						 }
			             ;
simple_expr		         : simple_expr adding_op term
                         {
							 $$ = math($1, $2, $3);
						 }
			             | term
                         {
							  $$ = $1;
						 }
			             | '-' term
						 {
							 strcpy($2->unary_minus, "-");
							 $$ = $2;
						 }
			             ;
term			         : term multiplying_op factor
                         {
							 $$ = math($1, $2, $3);		
						 }
 			             | factor
						 {
							 $$ = $1;
						 }
			             ;
factor			         : factor STAR primary
						 {
							 $$ = NULL;
						 }
			             | primary
                         {
							 $$ = $1;
						 }
			             | NOT primary
						 {
							 strcpy($2->not, "not");
							 $$ = $2;
						 }
			             ;
primary			         : NUMBER 
                         { 
							 struct operand *variable = (struct operand*)malloc(sizeof(struct operand));
							 variable->reg_num = get_reg();
							 variable->is_var = 0;
							 variable->is_local = 1;
							 variable->value = $1;
							 strcpy(variable->unary_minus, "");
							 strcpy(variable->not, "");
							 fprintf(ptr_file, "%d r%d := %d\n", instruction_count, variable->reg_num, $1);
							 instruction_count++;
							 $$ = variable;	 
                         }
			             | ID 
						 {
							 struct operand *variable = (struct operand*)malloc(sizeof(struct operand));
							 struct bin_tree *lookup = name_search($1);
							 int level;
							 int cmp;
							 variable->offset = lookup->symbol.offset;
							 variable->is_var = 1;
							 variable->reg_num = get_reg();
							 variable->value = 0;
							 strcpy(variable->unary_minus, "");
							 strcpy(variable->not, "");
							 strcpy(variable->name, $1);
							 /* cmp = strcmp(lookup->symbol.kind, "param"); */
							 /* if(cmp == 0){ */
							 /* 	 strcmp(variable->mode, $1->symbol.mode); */
							 /* } */
							 level = level_search($1);
							 if(level!= 0){
								 variable->is_local = 0;
							 }else{
								 variable->is_local = 1;
							 }
							 cmp = strcmp(lookup->symbol.kind, "value");
							 if(cmp == 0){
								 variable->is_bool = 1;
							 }else{
								 variable->is_bool = 0;
							 }
							 if(variable->is_bool == 0){
								 if(level != 0){ 
									 fprintf(ptr_file, "%d r%d := contents b, 2\n", instruction_count, variable->reg_num);
									 instruction_count++;
									 while (level > 1){
										 fprintf(ptr_file, "%d r%d := contents r%d, 2\n", instruction_count, variable->reg_num, variable->reg_num);
										 instruction_count++;
										 level--;
									 }
									 //fprintf(ptr_file, "%d r%d := contents r%d, %d\n", instruction_count, variable->reg_num, variable->reg_num, variable->offset);
									 instruction_count++;
								 }else{
								 	 fprintf(ptr_file, "%d r%d := contents b, %d\n", instruction_count, variable->reg_num, variable->offset);
								 	 instruction_count++;
								 }
								 }
							 $$ = variable;
							
						 }
			             | '(' expression ')'
						 {  
							 $$ = $2;
						 }
			             ;
boolean_op		         : AND
                         {
							 $$ = "and";
						 }
			             | OR
						 {
							 $$ = "or";
						 }
			             ;
relational_op		     : EQ
                         {
							 $$ = "=";
						 }
			             | NEQ
						 {
							 $$ = "!=";
						 }
			             | LT
						 {
							 $$ = "<";
						 }
			             | LTE
						 {
							 $$ = "<=";
						 }
			             | GT
						 {
							 $$ = ">";
						 }
			             | GTE
						 {
							 $$ = ">=";
						 }
		                 ;
adding_op		         : '+'
                         {
							 $$ = "+";
						 }
			             | '-'
                         {
							 $$ = "-";
						 }
	 		             ;
multiplying_op           : '*'
                         {
							 $$ = "*";
						 }
			             | '/'
						 {
							 $$ = "/";
						 }
			             ;
exception_part		     : exception exception_handlers
                         {
							 print_list(raise_list);
							 jump_table_addr.jump_instruction = instruction_count - 1;
							 struct idnode* patch_ptr;
							 patch_ptr = patch_list;
							 while(patch_ptr->next != NULL){
							 	 patch_ptr = patch_ptr->next;
							 }
							 	 patch_ptr->curr_instruction = jump_table_addr.curr_instruction;
							 	 patch_ptr->jump_instruction = jump_table_addr.jump_instruction;
							 	 patch_ptr->next = malloc(sizeof(idnode));
							 	 patch_ptr->next->next = NULL;
							 int i = 1;
							 int icount = instruction_count;
							 int cmp = strcmp($2, "others");
							 for(i; i < next_exception; i++){
							 	 if(cmp != 0){
							 		 if(jump_table[i] == 0){
							 			 jump_table[i] = icount + next_exception - 1;
							 		 }
							 	 }
							 	 fprintf(ptr_file, "%d PC := %d\n", instruction_count, jump_table[i]);
							 	 instruction_count++;
							 }
							 print_list(raise_list);
							 struct idnode* raise_ptr;
							 patch_ptr = patch_list;
							 raise_ptr = raise_list;
							 while(patch_ptr->next != NULL){
							 	 patch_ptr = patch_ptr->next;
							 }
							 while(raise_ptr != NULL){
							 	 patch_ptr->curr_instruction = raise_ptr->curr_instruction;
							 	 patch_ptr->jump_instruction = instruction_count;
							 	 raise_ptr = raise_ptr->next;
							 	 patch_ptr->next = malloc(sizeof(idnode));
							 	 patch_ptr->next->next = NULL;
							 	 patch_ptr = patch_ptr->next;
							 }
							 in_exception_part = 0;
							 raise_list = NULL;
						 }

			             | /*E*/
                         {
							 struct idnode* patch_ptr;
							 struct idnode* raise_ptr;
							 patch_ptr = patch_list;
							 raise_ptr = raise_list;
							 while(patch_ptr->next != NULL){
							 	 patch_ptr = patch_ptr->next;
							 }
							 while(raise_ptr != NULL){
							 	 patch_ptr->curr_instruction = raise_ptr->curr_instruction;
							 	 patch_ptr->jump_instruction = instruction_count;
							 	 raise_ptr = raise_ptr->next;
							 	 patch_ptr->next = malloc(sizeof(idnode));
							 	 patch_ptr->next->next = NULL;
							 	 patch_ptr = patch_ptr->next;
							 }
							 raise_list = NULL;
						 }
			             ;
exception                : EXCEPTION
                           {
							 int icount = instruction_count;
							 int i = 0;
							 struct idnode* raise_ptr;
							 struct idnode* patch_ptr;
							 patch_ptr = patch_list;
							 raise_ptr = raise_list;
							 fprintf(ptr_file, "%d PC := ?\n", instruction_count);
							 while(patch_ptr->next != NULL){
							 	 patch_ptr = patch_ptr->next;
							 }
							 while(raise_ptr != NULL){
							 	 patch_ptr->curr_instruction = raise_ptr->curr_instruction;
							 	 patch_ptr->jump_instruction = instruction_count + 1;
							 	 raise_ptr = raise_ptr->next;
							 	 patch_ptr->next = malloc(sizeof(idnode));
							 	 patch_ptr->next->next = NULL;
							 	 patch_ptr = patch_ptr->next;
							 }
							 raise_list = NULL;
							 raise_list = malloc(sizeof(idnode));
							 raise_list->curr_instruction = instruction_count;
							 raise_list->next = NULL;
							 instruction_count++;
							 in_exception_part = 1;
							 handler_done = 0;
							 fprintf(ptr_file, "%d PC := r1, ?\n", instruction_count);
							 jump_table_addr.curr_instruction = instruction_count;
							 instruction_count++;
							 for(i; i <= 99; i++){
							 	 jump_table[i] = 0;
							 }
						 }

exception_handlers	     : when exception_handlers
                         {
							 int cmp = strcmp($1, "others");
							 if (cmp == 0){
							 	 strcpy($$, "others");
							 }
						 }	
                         | when
                         {
							 int cmp = strcmp($1, "others");
							 if (cmp == 0){
							 	 strcpy($$, "others");
							 }
						 }	 
                         ;
when                     :  WHEN choice_sequence ARROW sequence_of_statements
                         {
							 int cmp = strcmp($2, "others");
							 if (cmp == 0){
							 	 strcpy($$, "others");
							 }
							 struct idnode* raise_ptr;
							 raise_ptr = raise_list;
							 fprintf(ptr_file, "%d r1 := 0\n", instruction_count);
							 instruction_count++;
							 fprintf(ptr_file, "%d PC := ?\n", instruction_count);
							 if(raise_ptr == NULL){
							 	 raise_ptr = malloc(sizeof(idnode));
							 	 raise_ptr->curr_instruction = instruction_count;
							 	 raise_ptr->next = NULL;
							 }else{
							 	 while(raise_ptr->next != NULL){
							 		 raise_ptr = raise_ptr->next;
							 	 }
							 	 raise_ptr->next = malloc(sizeof(idnode));
							 	 raise_ptr->next->curr_instruction = instruction_count;
							 	 raise_ptr->next->next = NULL;
							 	 instruction_count++;
							 }
						 }
			             ;
choice_sequence		     : ID '|' choice_sequence
                         {
							 int cmp;
							 if(handler_done == 1){
							 }else{
							 	 struct bin_tree* lookup = name_search($1);
							 	 cmp = strcmp(lookup->symbol.kind, "exception");
							 	 if(cmp == 0){
 							 		 jump_table[lookup->symbol.ival] = instruction_count;
							 	 }
							 }
						 }
			             | ID
                         {
							 int cmp;
							 if(handler_done == 1){
							 }else{
							  	 struct bin_tree* lookup = name_search($1);
							  	 cmp = strcmp(lookup->symbol.kind, "exception");
							 	 if(cmp == 0){
							 		 jump_table[lookup->symbol.ival] = instruction_count;
							 	 }
							  }
						 }
			             | OTHERS
						 {
							 strcpy($$, "others");
							 int i = 0;
							 if(handler_done == 1){
							 	 fprintf(ptr_file, "error the handler is already done\n");
							 }else{
							 	 handler_done = 1;
							 	 for(i; i <= 99; i++){
							 		 if (jump_table[i] == 0){
							 			 jump_table[i] = instruction_count;
							 		 }
							 	 }
							 }
						 }
								 
			             ;

%%
#include <stdio.h>
#include <string.h>
#include "linked_list.h"
#include "symbol_table.h"

extern FILE *yyin;
main(){
	
	ptr_file = fopen("output.txt", "w");
    printf("About to scan. . . . . .\n");
	prologue();
    push("Outer Context");
    struct sym_tab data;
	strcpy(data.name, "integer");
	strcpy(data.kind, "type");
	data.Ptype = NULL;
	data.size = 1;
	insert(&entry[count-1].root, data);
	strcpy(data.name, "boolean");
	strcpy(data.kind, "type");
	data.Ptype = NULL;
	data.size = 1;
	insert(&entry[count-1].root, data);
	strcpy(data.name, "false");
	strcpy(data.kind, "value");
	data.Ptype = NULL;
	data.size = 1;
	insert(&entry[count-1].root, data);
	strcpy(data.name, "maxint");
	strcpy(data.kind, "value");
	data.Ptype = NULL;
	data.size = 1;
	insert(&entry[count-1].root, data);
	strcpy(data.name, "true");
	strcpy(data.kind,"value");
	data.Ptype = NULL;
	data.size = 1;
	insert(&entry[count-1].root, data);
	strcpy(data.name, "read");
	strcpy(data.kind,"read_routine");
	data.Ptype = NULL;
	data.size = 1;
	insert(&entry[count-1].root, data);
	strcpy(data.name, "write");
	strcpy(data.kind,"write_routine");
	data.Ptype = NULL;
	data.size = 1;
	insert(&entry[count-1].root, data);
	strcpy(data.name, "constraint_error");
	strcpy(data.kind,"exception");
	data.Ptype = NULL;
	data.size = 1;
	data.ival = 1;
	insert(&entry[count-1].root, data);
	strcpy(data.name, "numeric_error");
	strcpy(data.kind,"exception");
	data.Ptype = NULL;
	data.size = 1;
	data.ival = 2;
	insert(&entry[count-1].root, data);
	printf("Outer Context \n");
	print_inorder(entry[count-1].root);
	patch_list = malloc(sizeof(idnode));
	patch_list->next = NULL;
	raise_list = NULL;
	yyparse();
	fprintf(ptr_file, "0 := %d\n", instruction_count);
	fprintf(ptr_file, "1 := %d\n", main_AR_size + instruction_count);
	fprintf(ptr_file, "3 := %d\n", main_start);
	struct idnode* loop_ptr;
	loop_ptr = patch_list;
	while(loop_ptr->next != NULL){
		fprintf(ptr_file, "%d := %d \n", loop_ptr->curr_instruction, loop_ptr->jump_instruction);
		loop_ptr = loop_ptr->next;
		//fclose(ptr_file );
	}
}
