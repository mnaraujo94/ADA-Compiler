#include <stdbool.h>
#include <stdio.h> /* load i/o routines */
#include <string.h>
#include <stdlib.h>
#include <ctype.h> /* load character test routines */
#include <math.h>

struct operand {
	int offset;
	int is_local;
	int reg_num;
	int is_var;
	int is_bool;
	int value;
	char not[20];
	char unary_minus[20];
	char name[20];
	char array_id[20];
	int extra;
	char mode[20];
	int reverse;
};

int instruction_count = 0;
int register_count = 2;
int next_exception;
FILE *ptr_file;

void prologue(){
  
	fprintf(ptr_file, "%i b := ?\n", instruction_count);
	instruction_count++;
	fprintf(ptr_file, "%i contents b, 0 := ?\n", instruction_count);
	instruction_count++;
	fprintf(ptr_file, "%i contents b, 1 := 4\n", instruction_count);
	instruction_count++;
	fprintf(ptr_file, "%i pc := ?\n", instruction_count);
	instruction_count++;
    fprintf(ptr_file, "%i halt\n", instruction_count);
	instruction_count++;
	fprintf(ptr_file, "%d r1 := 0\n", instruction_count);
	instruction_count++;
	next_exception = 3;
}

int get_reg(){
	return register_count++;
}

struct operand* math(struct operand* left_side, char* op, struct operand* right_side){
	 
	struct operand *variable  = (struct operand*)malloc(sizeof(struct operand));
	int cmp;
	int cmp2;
	int level_left;
	int level_right;
	int reg_num = 0;
	int reg_num2 = 0;

	cmp = strcmp(op, ">");
	cmp2 = strcmp(op, ">=");
	if(cmp == 0){
			variable->reg_num = get_reg();
			variable->is_var = 0;
			fprintf(ptr_file, "%d r%i := %s %sr%i < %s %sr%i\n", instruction_count, variable->reg_num, right_side->not, right_side->unary_minus, right_side->reg_num, left_side->not, left_side->unary_minus, left_side->reg_num);
			instruction_count++;
	}else if(cmp2 == 0){
		if (left_side->is_var == 0 && right_side->is_var == 0){
			variable->reg_num = get_reg();
			variable->is_var = 0;
			fprintf(ptr_file, "%d r%i := %s %sr%i <= %s %sr%i\n", instruction_count, variable->reg_num, right_side->not, right_side->unary_minus, right_side->reg_num, left_side->not, left_side->unary_minus, left_side->reg_num);
			instruction_count++;
		}
	}else{
			variable->reg_num = get_reg();
			variable->is_var = 0;
			fprintf(ptr_file, "%d r%i := %s %sr%i %s %s %sr%i\n", instruction_count, variable->reg_num, left_side->not, left_side->unary_minus, left_side->reg_num, op, right_side->not, right_side->unary_minus, right_side->reg_num);
			instruction_count++;
	}
	return variable;
}

void emit_operand(struct operand* op){
	if(op->is_bool == 1){
		fprintf(ptr_file, "%s", op->name);
	}else if(op->is_var == 1){//is in memory or not
		if(op->is_local == 1){ //is local or not
			fprintf(ptr_file, "%s%scontents b, %d", op->not, op->unary_minus, op->offset);
			//printf("%s%sr%d", op->not, op->unary_minus, op->offset);
		}else{
			fprintf(ptr_file, "%s%scontents r%d, %d", op->not, op->unary_minus, op->reg_num, op->offset);
		}
		if(op->extra != 0){
			fprintf(ptr_file, ", r%d", op->extra);
		}
    }else{
		fprintf(ptr_file, "r%d ", op->reg_num);
	}
}

void emit_assign(struct operand* to, struct operand* from){
	fprintf(ptr_file, "%d ", instruction_count);
	emit_operand(to);
	fprintf(ptr_file, " := ");
	emit_operand(from);
	instruction_count++;
	fprintf(ptr_file, "\n");
}
