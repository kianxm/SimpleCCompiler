/*
 * File:	generator.cpp
 *
 * Description:	This file contains the public and member function
 *		definitions for the code generator for Simple C.
 *
 *		Extra functionality:
 *		- putting all the global declarations at the end
 */

# include <vector>
# include <cassert>
# include <iostream>
# include <string>
# include <map>
# include "generator.h"
# include "machine.h"
# include "Tree.h"
# include "Label.h"
# include "string.h"

using namespace std;

static int offset;
static string funcname;
static string suffix(Expression *expr);
static ostream &operator <<(ostream &ostr, Expression *expr);

void assign(Expression *expr, Register *reg);
void load(Expression *expr, Register *reg);
Register *getreg();

static Register *rax = new Register("%rax", "%eax", "%al");
static Register *rbx = new Register("%rbx", "%ebx", "%bl");
static Register *rcx = new Register("%rcx", "%ecx", "%cl");
static Register *rdx = new Register("%rdx", "%edx", "%dl");
static Register *rsi = new Register("%rsi", "%esi", "%sil");
static Register *rdi = new Register("%rdi", "%edi", "%dil");
static Register *r8 = new Register("%r8", "%r8d", "%r8b");
static Register *r9 = new Register("%r9", "%r9d", "%r9b");
static Register *r10 = new Register("%r10", "%r10d", "%r10b");
static Register *r11 = new Register("%r11", "%r11d", "%r11b");
static Register *r12 = new Register("%r12", "%r12d", "%r12b");
static Register *r13 = new Register("%r13", "%r13d", "%r13b");
static Register *r14 = new Register("%r14", "%r14d", "%r14b");
static Register *r15 = new Register("%r15", "%r15d", "%r15b");

static vector<Register *> parameters = {rdi, rsi, rdx, rcx, r8, r9};
static vector<Register *> registers = {rax, rdi, rsi, rdx, rcx, r8, r9, r10, r11};

static map<string, Label> strings;

/*
 * Function:	suffix (private)
 *
 * Description:	Return the suffix for an opcode based on the given size.
 */

static string suffix(unsigned long size)
{
    return size == 1 ? "b\t" : (size == 4 ? "l\t" : "q\t");
}


/*
 * Function:	suffix (private)
 *
 * Description:	Return the suffix for an opcode based on the size of the
 *		given expression.
 */

static string suffix(Expression *expr)
{
    return suffix(expr->type().size());
}


/*
 * Function:	align (private)
 *
 * Description:	Return the number of bytes necessary to align the given
 *		offset on the stack.
 */

static int align(int offset)
{
    if (offset % STACK_ALIGNMENT == 0)
	return 0;

    return STACK_ALIGNMENT - (abs(offset) % STACK_ALIGNMENT);
}


/*
 * Function:	operator << (private)
 *
 * Description:	Convenience function for writing the operand of an
 *		expression using the output stream operator.
 */

static ostream &operator <<(ostream &ostr, Expression *expr)
{
    if (expr->_register != nullptr)
	    return ostr << expr->_register;

    expr->operand(ostr);
    return ostr;
}


/*
 * Function:	Expression::operand
 *
 * Description:	Write an expression as an operand to the specified stream.
 */

void Expression::operand(ostream &ostr) const
{
    assert(_offset != 0);
    ostr << _offset << "(%rbp)";
}


/*
 * Function:	Identifier::operand
 *
 * Description:	Write an identifier as an operand to the specified stream.
 */

void Identifier::operand(ostream &ostr) const
{
    if (_symbol->_offset == 0)
	ostr << global_prefix << _symbol->name() << global_suffix;
    else
	ostr << _symbol->_offset << "(%rbp)";
}


/*
 * Function:	Number::operand
 *
 * Description:	Write a number as an operand to the specified stream.
 */

void Number::operand(ostream &ostr) const
{
    ostr << "$" << _value;
}

void String::operand(ostream &ostr) const 
{
    Label string;
    if(strings.find(_value) == strings.end()) {
        strings.insert({_value, string});
    }
    else {
        string = strings.find(_value)->second;
    }
    ostr << ".L" << string.number();
}


/*
 * Function:	Call::generate
 *
 * Description:	Generate code for a function call expression.
 *
 *		On a 64-bit platform, the stack needs to be aligned on a
 *		16-byte boundary.  So, if the stack will not be aligned
 *		after pushing any arguments, we first adjust the stack
 *		pointer.
 *
 *		Since all arguments are 8-bytes wide, we could simply do:
 *
 *		    if (args.size() > 6 && args.size() % 2 != 0)
 *			subq $8, %rsp
 */

void Call::generate()
{
    unsigned numBytes;


    /* Generate code for the arguments first. */ 

    numBytes = 0;

    for (int i = _args.size() - 1; i >= 0; i --)
	_args[i]->generate();


    /* Adjust the stack if necessary */

    if (_args.size() > NUM_PARAM_REGS) {
	numBytes = align((_args.size() - NUM_PARAM_REGS) * SIZEOF_PARAM);

	if (numBytes > 0)
	    cout << "\tsubq\t$" << numBytes << ", %rsp" << endl;
    }


    /* Move the arguments into the correct registers or memory locations. */

    for (int i = _args.size() - 1; i >= 0; i --) {
	if (i >= NUM_PARAM_REGS) {
	    numBytes += SIZEOF_PARAM;
	    load(_args[i], rax);
	    cout << "\tpushq\t%rax" << endl;

	} else
	    load(_args[i], parameters[i]);

	assign(_args[i], nullptr);
    }


    /* Call the function and then reclaim the stack space.  Technically, we
       only need to assign the number of floating point arguments passed in
       vector registers to %eax if the function being called takes a
       variable number of arguments.  But, it never hurts. */

    for (auto reg : registers)
	load(nullptr, reg);

    if (_id->type().parameters() == nullptr)
	cout << "\tmovl\t$0, %eax" << endl;

    cout << "\tcall\t" << global_prefix << _id->name() << endl;

    if (numBytes > 0)
	cout << "\taddq\t$" << numBytes << ", %rsp" << endl;

    assign(this, rax);
}


/*
 * Function:	Block::generate
 *
 * Description:	Generate code for this block, which simply means we
 *		generate code for each statement within the block.
 */

void Block::generate()
{
    for (auto stmt : _stmts) {
	stmt->generate();

	for (auto reg : registers)
	    assert(reg->_node == nullptr);
    }
}


/*
 * Function:	Simple::generate
 *
 * Description:	Generate code for a simple (expression) statement, which
 *		means simply generating code for the expression.
 */

void Simple::generate()
{
    _expr->generate();
    assign(_expr, nullptr);
}


/*
 * Function:	Function::generate
 *
 * Description:	Generate code for this function, which entails allocating
 *		space for local variables, then emitting our prologue, the
 *		body of the function, and the epilogue.
 */

void Function::generate()
{
    int param_offset;
    unsigned size;
    Parameters *params;
    Symbols symbols;


    /* Assign offsets to the parameters and local variables. */

    param_offset = 2 * SIZEOF_REG;
    offset = param_offset;
    allocate(offset);


    /* Generate our prologue. */

    funcname = _id->name();
    cout << global_prefix << funcname << ":" << endl;
    cout << "\tpushq\t%rbp" << endl;
    cout << "\tmovq\t%rsp, %rbp" << endl;
    cout << "\tmovl\t$" << funcname << ".size, %eax" << endl;
    cout << "\tsubq\t%rax, %rsp" << endl;


    /* Spill any parameters. */

    params = _id->type().parameters();
    symbols = _body->declarations()->symbols();

    for (unsigned i = 0; i < NUM_PARAM_REGS; i ++)
	if (i < params->size()) {
	    size = symbols[i]->type().size();
	    cout << "\tmov" << suffix(size) << parameters[i]->name(size);
	    cout << ", " << symbols[i]->_offset << "(%rbp)" << endl;
	} else
	    break;


    /* Generate the body of this function. */

    _body->generate();


    /* Generate our epilogue. */

    cout << endl << global_prefix << funcname << ".exit:" << endl;
    cout << "\tmovq\t%rbp, %rsp" << endl;
    cout << "\tpopq\t%rbp" << endl;
    cout << "\tret" << endl << endl;

    offset -= align(offset - param_offset);
    cout << "\t.set\t" << funcname << ".size, " << -offset << endl;
    cout << "\t.globl\t" << global_prefix << funcname << endl << endl;
}


/*
 * Function:	generateGlobals
 *
 * Description:	Generate code for any global variable declarations.
 */

void generateGlobals(Scope *scope)
{
    const Symbols &symbols = scope->symbols();

    for (auto symbol : symbols)
	if (!symbol->type().isFunction()) {
	    cout << "\t.comm\t" << global_prefix << symbol->name() << ", ";
	    cout << symbol->type().size() << endl;
	}

    cout << "\t.data" << endl;
    for(pair<string, Label> string_pair : strings) {
        cout << string_pair.second << ":\t.asciz\t\"" << escapeString(string_pair.first) << "\"" << endl;
    }
}


/*
 * Function:	Assignment::generate
 *
 * Description:	Generate code for an assignment statement.
 *
 *		NOT FINISHED: Only works if the right-hand side is an
 *		integer literal and the left-hand side is an integer
 *		scalar.
 */
void Assignment::generate() {
    Expression *pointer;
    _right->generate();

    if(_left->isDereference(pointer)) {
        pointer->generate();

        if(pointer->_register == nullptr)
            load(pointer, getreg());

        if(_right->_register == nullptr)
            load(_right, getreg());

        cout << "\tmov" << suffix(_right) << _right << ",\t(" << pointer << ")" << endl;

        assign(pointer, nullptr);

    }
    else {
        if(_right->_register == nullptr)
            load(_right, getreg());
        
        _left->generate();

        cout << "\tmov" << suffix(_right) << _right << ", " << _left << endl;
    }

    assign(_right, nullptr);
}


void assign(Expression *expr, Register *reg) {
    if (expr != nullptr) {
        if (expr->_register != nullptr)
             expr->_register->_node = nullptr;
        expr->_register = reg;
    }

    if (reg != nullptr) {
        if (reg->_node != nullptr)
            reg->_node->_register = nullptr;
        reg->_node = expr;
    }
}


void load(Expression *expr, Register *reg)
{
    if (reg->_node != expr) {
        if (reg->_node != nullptr) {
            unsigned size = reg->_node->type().size();
            offset -= size;
            reg->_node->_offset = offset;
            cout << "\tmov" << suffix(reg->_node);
            cout << reg->name(size) << ", ";
            cout << offset << "(%rbp)" << endl;
        }
        
        if(expr != nullptr) {
            unsigned size = expr->type().size();
            cout << "\tmov" << suffix(expr) << expr;
            cout << ",\t" << reg->name(size) << endl;
        }

        assign(expr, reg);
    }
}

Register *getreg() {
    for(auto reg : registers) {
        if(reg->_node == nullptr)
            return reg;
    }
    load(nullptr, registers[0]);
    return registers[0];
}


void Add::generate() {
    //Generate left and right
    _left->generate();
    _right->generate();

    //Load left
    if(_left->_register == nullptr)
        load(_left, getreg());

    //Run operation
    cout << "\tadd" << suffix(_left);
    cout << _right << ",\t" << _left << endl;

    //Unassign right register
    assign(_right, nullptr);

    //Assign left register to this
    assign(this, _left->_register);
}

void Subtract::generate() {
    //Generate left and right
    _left->generate();
    _right->generate();

    //Load left
    if(_left->_register == nullptr)
        load(_left, getreg());

    //Run operation
    cout << "\tsub" << suffix(_left);
    cout << _right << ",\t" << _left << endl;

    //Unassign right register
    assign(_right, nullptr);

    //Assign left register to this
    assign(this, _left->_register);
}

void Multiply::generate() {
    //Generate left and right
    _left->generate();
    _right->generate();

    //Load left
    if(_left->_register == nullptr)
        load(_left, getreg());

    //Run operation
    cout << "\timul" << suffix(_left);
    cout << _right << ",\t" << _left << endl;

    //Unassign right register
    assign(_right, nullptr);

    //Assign left register to this
    assign(this, _left->_register);
}

void Divide::generate() {
    //Generate left and right
    _left->generate();
    _right->generate();

    //Load left into rax
    if(_left->_register == nullptr)
        load(_left, rax);

    //Unload rdx
    load(nullptr, rdx);

    //Load right into rcx
    load(_right, rcx);

    //Sign extend rax into rdx
    if(_left->type().size() == 4) {
        cout << "\tcltd\t" << endl;
    } else {
        cout << "\tcqto\t" << endl;
    }

    // idiv (with proper suffix) right
    cout << "\tidiv" << suffix(_right) << _right << endl;

    // Unassign left and right register
    assign(_left, nullptr);
    assign(_right, nullptr);
    
    // Assign this to correct register
    assign(this, rax);
}

void Remainder::generate() {
    //Generate left and right
    _left->generate();
    _right->generate();

    //Load left into rax
    if(_left->_register == nullptr)
        load(_left, rax);

    //Unload rdx
    load(nullptr, rdx);

    //Load right into rcx
    load(_right, rcx);

    //Sign extend rax into rdx
    if(_left->type().size() == 4) {
        cout << "\tcltd\t" << endl;
    } else {
        cout << "\tcqto\t" << endl;
    }

    // idiv (with proper suffix) right
    cout << "\tidiv" << suffix(_right) << _right << endl;

    // Unassign left and right register
    assign(_left, nullptr);
    assign(_right, nullptr);
    
    // Assign this to correct register
    assign(this, rdx);
}

void LessThan::generate() {
    //Generate left and right
    _left->generate();
    _right->generate();

    Register *ptr;

    //Load left
    if(_left->_register == nullptr)
        load(_left, getreg());

    //Compare left and right
    cout << "\tcmp" << suffix(_left) << _right << ",\t" << _left << endl;

    //Unassign left and right
    assign(_left, nullptr);
    assign(_right, nullptr);

    //Assign this to a register
    assign(this, ptr = getreg());
    

    //Store result of condition code in byte register
    cout << "\tsetl\t" << this->_register->byte() << endl;

    //Zero-extend byte to long (using “movzbl”)
    cout << "\tmovzbl\t" << ptr->byte() << ",\t" << this << endl;
}

void GreaterThan::generate() {
    //Generate left and right
    _left->generate();
    _right->generate();

    Register *ptr;

    //Load left
    if(_left->_register == nullptr)
        load(_left, getreg());

    //Compare left and right
    cout << "\tcmp" << suffix(_left) << _right << ",\t" << _left << endl;

    //Unassign left and right
    assign(_left, nullptr);
    assign(_right, nullptr);

    //Assign this to a register
    assign(this, ptr = getreg());
    

    //Store result of condition code in byte register
    cout << "\tsetg\t" << this->_register->byte() << endl;

    //Zero-extend byte to long (using “movzbl”)
    cout << "\tmovzbl\t" << ptr->byte() << ",\t" << this << endl;

}

void LessOrEqual::generate() {
    //Generate left and right
    _left->generate();
    _right->generate();

    Register *ptr;

    //Load left
    if(_left->_register == nullptr)
        load(_left, getreg());

    //Compare left and right
    cout << "\tcmp" << suffix(_left) << _right << ",\t" << _left << endl;

    //Unassign left and right
    assign(_left, nullptr);
    assign(_right, nullptr);

    //Assign this to a register
    assign(this, ptr = getreg());

    //Store result of condition code in byte register
    cout << "\tsetle\t" << this->_register->byte() << endl;

    //Zero-extend byte to long (using “movzbl”)
    cout << "\tmovzbl\t" << ptr->byte() << ",\t" << this << endl;
}

void GreaterOrEqual::generate() {
    //Generate left and right
    _left->generate();
    _right->generate();

    Register *ptr;

    //Load left
    if(_left->_register == nullptr)
        load(_left, getreg());

    //Compare left and right
    cout << "\tcmp" << suffix(_left) << _right << ",\t" << _left << endl;

    //Unassign left and right
    assign(_left, nullptr);
    assign(_right, nullptr);

    //Assign this to a register
    assign(this, ptr = getreg());
    

    //Store result of condition code in byte register
    cout << "\tsetge\t" << this->_register->byte() << endl; //? check this->byte() vs this->_register->byte()

    //Zero-extend byte to long (using “movzbl”)
    cout << "\tmovzbl\t" << ptr->byte() << ",\t" << this << endl;
}

void Equal::generate() {
    //Generate left and right
    _left->generate();
    _right->generate();

    Register *ptr;

    //Load left
    if(_left->_register == nullptr)
        load(_left, getreg());

    //Compare left and right
    cout << "\tcmp" << suffix(_left) << _right << ",\t" << _left << endl;

    //Unassign left and right
    assign(_left, nullptr);
    assign(_right, nullptr);

    //Assign this to a register
    assign(this, ptr = getreg());
    

    //Store result of condition code in byte register
    cout << "\tsete\t" << this->_register->byte() << endl; //? check this->byte() vs this->_register->byte()

    //Zero-extend byte to long (using “movzbl”)
    cout << "\tmovzbl\t" << ptr->byte() << ",\t" << this << endl;
}

void NotEqual::generate() {
    //Generate left and right
    _left->generate();
    _right->generate();

    Register *ptr;

    //Load left
    if(_left->_register == nullptr)
        load(_left, getreg());

    //Compare left and right
    cout << "\tcmp" << suffix(_left) << _right << ",\t" << _left << endl;

    //Unassign left and right
    assign(_left, nullptr);
    assign(_right, nullptr);

    //Assign this to a register
    assign(this, ptr = getreg());
    

    //Store result of condition code in byte register
    cout << "\tsetne\t" << this->_register->byte() << endl; //? check this->byte() vs this->_register->byte()

    //Zero-extend byte to long (using “movzbl”)
    cout << "\tmovzbl\t" << ptr->byte() << ",\t" << this << endl;
}

void Not::generate() {
    _expr->generate();
    if(_expr->_register == nullptr)
        load(_expr, getreg());

    cout << "\tcmp" << suffix(_expr) << "$0,\t" << _expr << endl;
    cout << "\tsete\t" << _expr->_register->byte() << endl;
    cout << "\tmovzbl\t" << _expr->_register->byte() << ",\t" << _expr->_register << endl;

    assign(this, _expr->_register);
}

void Negate::generate() {
    _expr->generate();
    if(_expr->_register == nullptr)
        load(_expr, getreg());

    cout << "\tneg" << suffix(_expr) << _expr->_register << endl;

    assign(this, _expr->_register);
}

void Expression::test(const Label &label, bool ifTrue)
{
    generate();
    if (_register == nullptr)
        load(this, getreg());

    cout << "\tcmp" << suffix(this) << "$0, " << this << endl;
    cout << (ifTrue ? "\tjne\t" : "\tje\t") << label << endl;

    assign(this, nullptr);
}

void While::generate()
{
    Label loop, exit;
    cout << loop << ":" << endl;

    _expr->test(exit, false);
    _stmt->generate();

    cout << "\tjmp\t" << loop << endl;
    cout << exit << ":" << endl;
}

void Address::generate() {
    Expression *pointer;
    if(_expr->isDereference(pointer)) {
        pointer->generate();

        if(pointer->_register == nullptr) 
            load(pointer, getreg());
        
        assign(this, pointer->_register);
    } 
    else {
        assign(this, getreg());
        cout << "\tlea" << suffix(this) << _expr << ",\t" << this << endl;
    }
}

void Dereference::generate() {
    _expr->generate();
    if(_expr->_register == nullptr)
        load(_expr, getreg());

    cout << "\tmov" << suffix(_expr) << "(" << _expr << ")" << ",\t" << _expr << endl;
    assign(this, _expr->_register);
}

void Return::generate() {
    _expr->generate();
    load(_expr, rax);
    cout << "\tjmp\t" << funcname << ".exit" << endl;
    assign(_expr, nullptr);
}

void Cast::generate() {
    Register *reg;
    unsigned source, target;

    source = _expr->type().size();
    target = _type.size();
    _expr->generate();

    if(source >= target) {
        if(_expr->_register == nullptr) {
            load(_expr, reg = getreg());
        }
        assign(this, _expr->_register);
    }
    else {
        if(source == 1 && target == 4) { //char to int
            if(_expr->_register != nullptr) {
                reg = _expr->_register;
            }
            else {
                reg = getreg();
            }  
            cout << "\tmovsbl\t" << _expr << ",\t" << reg->name(target) << endl;
        }
        else if(source == 1 && target == 8) { //char to long
            //movsbq
            if(_expr->_register != nullptr) {
                reg = _expr->_register;
            }
            else {
                reg = getreg();
            }  
            cout << "\tmovsbq\t" << _expr << ",\t" << reg->name(target) << endl;
        }
        else { //source == 4 target == 8 //int to long
            //movslq
            if(_expr->_register != nullptr) {
                reg = _expr->_register;
            }
            else {
                reg = getreg();
            }  
            cout << "\tmovslq\t" << _expr << ",\t" << reg->name(target) << endl;
        }
        //bind this to the register with casted value
        assign(this, reg);
    }
}

void LogicalAnd::generate() {
    Label skip;
    _left->test(skip, false);

    _right->generate();
    if(_right->_register == nullptr)
        load(_right, getreg());
    
    cout << "\tcmp" << suffix(_right) << "$0,\t" << _right << endl;

    cout << skip << ":" << endl;
    cout << "\tsetne\t" << _right->_register->byte() << endl;
    cout << "\tmovzb" << suffix(_right) << _right->_register->byte() << ",\t" << _right << endl;
    assign(this, _right->_register);
}

void LogicalOr::generate() {
    Label skip;
    _left->test(skip, true);

    _right->generate();
    if(_right->_register == nullptr) {
        load(_right, getreg());
    }

    cout << "\tcmp" << suffix(_right) << "$0,\t" << _right << endl;
    cout << skip << ":" << endl;
    cout << "\tsetne\t" << _right->_register->byte() << endl;
    cout << "\tmovzb" << suffix(_right) << _right->_register->byte() << ",\t" << _right << endl;
    assign(this, _right->_register);
}

void For::generate() {
    Label loop, exit;
    _init->generate();
    cout << loop << ":" << endl;

    _expr->test(exit, false);
    _stmt->generate();
    _incr->generate();

    cout << "\tjmp\t" << loop << endl;
    cout << exit << ":" << endl;
}

void If::generate() {
    Label skip, exit;

    _expr->test(skip, false);
    _thenStmt->generate();

    if(_elseStmt == nullptr) {
        cout << skip << ":" << endl;
    }
    else {
        cout << "\tjmp\t" << exit << endl;
        cout << skip << ":" << endl;
        _elseStmt->generate();
        cout << exit << ":" << endl;
    }
}
