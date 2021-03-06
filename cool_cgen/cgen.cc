
//**************************************************************
//
// Code generator SKELETON
//
// Read the comments carefully. Make sure to
//    initialize the base class tags in
//       `CgenClassTable::CgenClassTable'
//
//    Add the label for the dispatch tables to
//       `IntEntry::code_def'
//       `StringEntry::code_def'
//       `BoolConst::code_def'
//
//    Add code to emit everyting else that is needed
//       in `CgenClassTable::code'
//
//
// The files as provided will produce code to begin the code
// segments, declare globals, and emit constants.  You must
// fill in the rest.
//
//**************************************************************

#include "cgen.h"
#include "cgen_gc.h"
#include <typeinfo>

extern void emit_string_constant(ostream& str, char *s);
extern int cgen_debug;

SymbolTable<Symbol, char> addrTab;
class__class* current_class = NULL;
int label = 0;
int fp_offset = 0;

//
// Three symbols from the semantic analyzer (semant.cc) are used.
// If e : No_type, then no code is generated for e.
// Special code is generated for new SELF_TYPE.
// The name "self" also generates code different from other references.
//
//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
Symbol
       arg,
       arg2,
       Bool,
       concat,
       cool_abort,
       copy,
       Int,
       in_int,
       in_string,
       IO,
       length,
       Main,
       main_meth,
       No_class,
       No_type,
       Object,
       out_int,
       out_string,
       prim_slot,
       self,
       SELF_TYPE,
       Str,
       str_field,
       substr,
       type_name,
       val;
//
// Initializing the predefined symbols.
//
static void initialize_constants(void)
{
  arg         = idtable.add_string("arg");
  arg2        = idtable.add_string("arg2");
  Bool        = idtable.add_string("Bool");
  concat      = idtable.add_string("concat");
  cool_abort  = idtable.add_string("abort");
  copy        = idtable.add_string("copy");
  Int         = idtable.add_string("Int");
  in_int      = idtable.add_string("in_int");
  in_string   = idtable.add_string("in_string");
  IO          = idtable.add_string("IO");
  length      = idtable.add_string("length");
  Main        = idtable.add_string("Main");
  main_meth   = idtable.add_string("main");
//   _no_class is a symbol that can't be the name of any
//   user-defined class.
  No_class    = idtable.add_string("_no_class");
  No_type     = idtable.add_string("_no_type");
  Object      = idtable.add_string("Object");
  out_int     = idtable.add_string("out_int");
  out_string  = idtable.add_string("out_string");
  prim_slot   = idtable.add_string("_prim_slot");
  self        = idtable.add_string("self");
  SELF_TYPE   = idtable.add_string("SELF_TYPE");
  Str         = idtable.add_string("String");
  str_field   = idtable.add_string("_str_field");
  substr      = idtable.add_string("substr");
  type_name   = idtable.add_string("type_name");
  val         = idtable.add_string("_val");
}

static char *gc_init_names[] =
  { "_NoGC_Init", "_GenGC_Init", "_ScnGC_Init" };
static char *gc_collect_names[] =
  { "_NoGC_Collect", "_GenGC_Collect", "_ScnGC_Collect" };


//  BoolConst is a class that implements code generation for operations
//  on the two booleans, which are given global names here.
BoolConst falsebool(FALSE);
BoolConst truebool(TRUE);

//*********************************************************
//
// Define method for code generation
//
// This is the method called by the compiler driver
// `cgtest.cc'. cgen takes an `ostream' to which the assembly will be
// emmitted, and it passes this and the class list of the
// code generator tree to the constructor for `CgenClassTable'.
// That constructor performs all of the work of the code
// generator.
//
//*********************************************************
CgenClassTable *codegen_classtable = NULL;

void program_class::cgen(ostream &os)
{
  // spim wants comments to start with '#'
  os << "# start of generated code\n";

  initialize_constants();
  codegen_classtable = new CgenClassTable(classes,os);
  for(int i = classes->first(); classes->more(i); i = classes->next(i)) {
    class__class* c = (class__class *) classes->nth(i);
    current_class = c;
    c->code(codegen_classtable->str);
  }

  os << "\n# end of generated code\n";
}


//////////////////////////////////////////////////////////////////////////////
//
//  emit_* procedures
//
//  emit_X  writes code for operation "X" to the output stream.
//  There is an emit_X for each opcode X, as well as emit_ functions
//  for generating names according to the naming conventions (see emit.h)
//  and calls to support functions defined in the trap handler.
//
//  Register names and addresses are passed as strings.  See `emit.h'
//  for symbolic names you can use to refer to the strings.
//
//////////////////////////////////////////////////////////////////////////////

static void emit_load(char *dest_reg, int offset, char *source_reg, ostream& s)
{
  s << LW << dest_reg << " " << offset * WORD_SIZE << "(" << source_reg << ")"
    << endl;
}

static void emit_store(char *source_reg, int offset, char *dest_reg, ostream& s)
{
  s << SW << source_reg << " " << offset * WORD_SIZE << "(" << dest_reg << ")"
      << endl;
}

static void emit_load_imm(char *dest_reg, int val, ostream& s)
{ s << LI << dest_reg << " " << val << endl; }

static void emit_load_address(char *dest_reg, char *address, ostream& s)
{ s << LA << dest_reg << " " << address << endl; }

static void emit_partial_load_address(char *dest_reg, ostream& s)
{ s << LA << dest_reg << " "; }

static void emit_load_bool(char *dest, const BoolConst& b, ostream& s)
{
  emit_partial_load_address(dest,s);
  b.code_ref(s);
  s << endl;
}

static void emit_load_string(char *dest, StringEntry *str, ostream& s)
{
  emit_partial_load_address(dest,s);
  str->code_ref(s);
  s << endl;
}

static void emit_load_int(char *dest, IntEntry *i, ostream& s)
{
  emit_partial_load_address(dest,s);
  i->code_ref(s);
  s << endl;
}

static void emit_move(char *dest_reg, char *source_reg, ostream& s)
{ s << MOVE << dest_reg << " " << source_reg << endl; }

static void emit_neg(char *dest, char *src1, ostream& s)
{ s << NEG << dest << " " << src1 << endl; }

static void emit_add(char *dest, char *src1, char *src2, ostream& s)
{ s << ADD << dest << " " << src1 << " " << src2 << endl; }

static void emit_addu(char *dest, char *src1, char *src2, ostream& s)
{ s << ADDU << dest << " " << src1 << " " << src2 << endl; }

static void emit_addiu(char *dest, char *src1, int imm, ostream& s)
{ s << ADDIU << dest << " " << src1 << " " << imm << endl; }

static void emit_div(char *dest, char *src1, char *src2, ostream& s)
{ s << DIV << dest << " " << src1 << " " << src2 << endl; }

static void emit_mul(char *dest, char *src1, char *src2, ostream& s)
{ s << MUL << dest << " " << src1 << " " << src2 << endl; }

static void emit_sub(char *dest, char *src1, char *src2, ostream& s)
{ s << SUB << dest << " " << src1 << " " << src2 << endl; }

static void emit_sll(char *dest, char *src1, int num, ostream& s)
{ s << SLL << dest << " " << src1 << " " << num << endl; }

static void emit_jalr(char *dest, ostream& s)
{ s << JALR << "\t" << dest << endl; }

static void emit_jal(char *address,ostream &s)
{ s << JAL << address << endl; }

static void emit_return(ostream& s)
{ s << RET << endl; }

static void emit_gc_assign(ostream& s)
{ s << JAL << "_GenGC_Assign" << endl; }

static void emit_disptable_ref(Symbol sym, ostream& s)
{  s << sym << DISPTAB_SUFFIX; }

static void emit_init_ref(Symbol sym, ostream& s)
{ s << sym << CLASSINIT_SUFFIX; }

static void emit_label_ref(int l, ostream &s)
{ s << "label" << l; }

static void emit_protobj_ref(Symbol sym, ostream& s)
{ s << sym << PROTOBJ_SUFFIX; }

static void emit_method_ref(Symbol classname, Symbol methodname, ostream& s)
{ s << classname << METHOD_SEP << methodname; }

static void emit_label_def(int l, ostream &s)
{
  emit_label_ref(l,s);
  s << ":" << endl;
}

static void emit_beqz(char *source, int label, ostream &s)
{
  s << BEQZ << source << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_beq(char *src1, char *src2, int label, ostream &s)
{
  s << BEQ << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bne(char *src1, char *src2, int label, ostream &s)
{
  s << BNE << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bleq(char *src1, char *src2, int label, ostream &s)
{
  s << BLEQ << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_blt(char *src1, char *src2, int label, ostream &s)
{
  s << BLT << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_blti(char *src1, int imm, int label, ostream &s)
{
  s << BLT << src1 << " " << imm << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bgti(char *src1, int imm, int label, ostream &s)
{
  s << BGT << src1 << " " << imm << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_branch(int l, ostream& s)
{
  s << BRANCH;
  emit_label_ref(l,s);
  s << endl;
}

//
// Push a register on the stack. The stack grows towards smaller addresses.
//
static void emit_push(char *reg, ostream& str)
{
  emit_store(reg,0,SP,str);
  emit_addiu(SP,SP,-4,str);
}

static void emit_pop(char *reg, ostream& str)
{
  emit_load(reg,1,SP,str);
  emit_addiu(SP,SP,4,str);
}

//
// Fetch the integer value in an Int object.
// Emits code to fetch the integer value of the Integer object pointed
// to by register source into the register dest
//
static void emit_fetch_int(char *dest, char *source, ostream& s)
{ emit_load(dest, DEFAULT_OBJFIELDS, source, s); }

//
// Emits code to store the integer value contained in register source
// into the Integer object pointed to by dest.
//
static void emit_store_int(char *source, char *dest, ostream& s)
{ emit_store(source, DEFAULT_OBJFIELDS, dest, s); }


static void emit_test_collector(ostream &s)
{
  emit_push(ACC, s);
  emit_move(ACC, SP, s); // stack end
  emit_move(A1, ZERO, s); // allocate nothing
  s << JAL << gc_collect_names[cgen_Memmgr] << endl;
  emit_addiu(SP,SP,4,s);
  emit_load(ACC,0,SP,s);
}

static void emit_gc_check(char *source, ostream &s)
{
  if (source != (char*)A1) emit_move(A1, source, s);
  s << JAL << "_gc_check" << endl;
}


///////////////////////////////////////////////////////////////////////////////
//
// coding strings, ints, and booleans
//
// Cool has three kinds of constants: strings, ints, and booleans.
// This section defines code generation for each type.
//
// All string constants are listed in the global "stringtable" and have
// type StringEntry.  StringEntry methods are defined both for String
// constant definitions and references.
//
// All integer constants are listed in the global "inttable" and have
// type IntEntry.  IntEntry methods are defined for Int
// constant definitions and references.
//
// Since there are only two Bool values, there is no need for a table.
// The two booleans are represented by instances of the class BoolConst,
// which defines the definition and reference methods for Bools.
//
///////////////////////////////////////////////////////////////////////////////

//
// Strings
//
void StringEntry::code_ref(ostream& s)
{
  s << STRCONST_PREFIX << index;
}

//
// Emit code for a constant String.
// You should fill in the code naming the dispatch table.
//

void StringEntry::code_def(ostream& s, int stringclasstag)
{
  IntEntryP lensym = inttable.add_int(len);

  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s  << LABEL                                             // label
      << WORD << stringclasstag << endl                                 // tag
      << WORD << (DEFAULT_OBJFIELDS + STRING_SLOTS + (len+4)/4) << endl // size
      << WORD;


 /***** Add dispatch information for class String ******/

      s << Str << DISPTAB_SUFFIX << endl;         // dispatch table
      s << WORD;  lensym->code_ref(s);  s << endl;            // string length
  emit_string_constant(s,str);                                // ascii string
  s << ALIGN;                                                 // align to word
}

//
// StrTable::code_string
// Generate a string object definition for every string constant in the
// stringtable.
//
void StrTable::code_string_table(ostream& s, int stringclasstag)
{
  for (List<StringEntry> *l = tbl; l; l = l->tl())
    l->hd()->code_def(s,stringclasstag);
}

//
// Ints
//
void IntEntry::code_ref(ostream &s)
{
  s << INTCONST_PREFIX << index;
}

//
// Emit code for a constant Integer.
// You should fill in the code naming the dispatch table.
//

void IntEntry::code_def(ostream &s, int intclasstag)
{
  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s << LABEL                                // label
      << WORD << intclasstag << endl                      // class tag
      << WORD << (DEFAULT_OBJFIELDS + INT_SLOTS) << endl  // object size
      << WORD;

 /***** Add dispatch information for class Int ******/

      s << Int << DISPTAB_SUFFIX << endl;                  // dispatch table
      s << WORD << str << endl;                           // integer value
}


//
// IntTable::code_string_table
// Generate an Int object definition for every Int constant in the
// inttable.
//
void IntTable::code_string_table(ostream &s, int intclasstag)
{
  for (List<IntEntry> *l = tbl; l; l = l->tl())
    l->hd()->code_def(s,intclasstag);
}


//
// Bools
//
BoolConst::BoolConst(int i) : val(i) { assert(i == 0 || i == 1); }

void BoolConst::code_ref(ostream& s) const
{
  s << BOOLCONST_PREFIX << val;
}

//
// Emit code for a constant Bool.
// You should fill in the code naming the dispatch table.
//

void BoolConst::code_def(ostream& s, int boolclasstag)
{
  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s << LABEL                                  // label
      << WORD << boolclasstag << endl                       // class tag
      << WORD << (DEFAULT_OBJFIELDS + BOOL_SLOTS) << endl   // object size
      << WORD;

 /***** Add dispatch information for class Bool ******/

      s << Bool << DISPTAB_SUFFIX << endl;                  // dispatch table
      s << WORD << val << endl;                             // value (0 or 1)
}

//////////////////////////////////////////////////////////////////////////////
//
//  CgenClassTable methods
//
//////////////////////////////////////////////////////////////////////////////

//***************************************************
//
//  Emit code to start the .data segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_data()
{
  Symbol main    = idtable.lookup_string(MAINNAME);
  Symbol string  = idtable.lookup_string(STRINGNAME);
  Symbol integer = idtable.lookup_string(INTNAME);
  Symbol boolc   = idtable.lookup_string(BOOLNAME);

  str << "\t.data\n" << ALIGN;
  //
  // The following global names must be defined first.
  //
  str << GLOBAL << CLASSNAMETAB << endl;
  str << GLOBAL; emit_protobj_ref(main,str);    str << endl;
  str << GLOBAL; emit_protobj_ref(integer,str); str << endl;
  str << GLOBAL; emit_protobj_ref(string,str);  str << endl;
  str << GLOBAL; falsebool.code_ref(str);  str << endl;
  str << GLOBAL; truebool.code_ref(str);   str << endl;
  str << GLOBAL << INTTAG << endl;
  str << GLOBAL << BOOLTAG << endl;
  str << GLOBAL << STRINGTAG << endl;

  //
  // We also need to know the tag of the Int, String, and Bool classes
  // during code generation.
  //
  str << INTTAG << LABEL
      << WORD << intclasstag << endl;
  str << BOOLTAG << LABEL
      << WORD << boolclasstag << endl;
  str << STRINGTAG << LABEL
      << WORD << stringclasstag << endl;
}


//***************************************************
//
//  Emit code to start the .text segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_text()
{
  str << GLOBAL << HEAP_START << endl
      << HEAP_START << LABEL
      << WORD << 0 << endl
      << "\t.text" << endl
      << GLOBAL;
  emit_init_ref(idtable.add_string("Main"), str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("Int"),str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("String"),str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("Bool"),str);
  str << endl << GLOBAL;
  emit_method_ref(idtable.add_string("Main"), idtable.add_string("main"), str);
  str << endl;
}

void CgenClassTable::code_bools(int boolclasstag)
{
  falsebool.code_def(str,boolclasstag);
  truebool.code_def(str,boolclasstag);
}

void CgenClassTable::code_select_gc()
{
  //
  // Generate GC choice constants (pointers to GC functions)
  //
  str << GLOBAL << "_MemMgr_INITIALIZER" << endl;
  str << "_MemMgr_INITIALIZER:" << endl;
  str << WORD << gc_init_names[cgen_Memmgr] << endl;
  str << GLOBAL << "_MemMgr_COLLECTOR" << endl;
  str << "_MemMgr_COLLECTOR:" << endl;
  str << WORD << gc_collect_names[cgen_Memmgr] << endl;
  str << GLOBAL << "_MemMgr_TEST" << endl;
  str << "_MemMgr_TEST:" << endl;
  str << WORD << (cgen_Memmgr_Test == GC_TEST) << endl;
}


//********************************************************
//
// Emit code to reserve space for and initialize all of
// the constants.  Class names should have been added to
// the string table (in the supplied code, is is done
// during the construction of the inheritance graph), and
// code for emitting string constants as a side effect adds
// the string's length to the integer table.  The constants
// are emmitted by running through the stringtable and inttable
// and producing code for each entry.
//
//********************************************************

void CgenClassTable::code_constants()
{
  //
  // Add constants that are required by the code generator.
  //
  stringtable.add_string("");
  inttable.add_string("0");

  stringtable.code_string_table(str,stringclasstag);
  inttable.code_string_table(str,intclasstag);
  code_bools(boolclasstag);
}


CgenClassTable::CgenClassTable(Classes classes, ostream& s) : str(s)
{
   // set tag in install_class()
   stringclasstag = 0 /* Change to your String class tag here */;
   intclasstag =    0 /* Change to your Int class tag here */;
   boolclasstag =   0 /* Change to your Bool class tag here */;
   current_tag = 0;

   enterscope();
   if (cgen_debug) cout << "Building CgenClassTable" << endl;
   install_basic_classes();
   install_classes(classes);
   build_inheritance_tree();

   code();
   exitscope();
}

void CgenClassTable::install_basic_classes()
{

// The tree package uses these globals to annotate the classes built below.
  //curr_lineno  = 0;
  Symbol filename = stringtable.add_string("<basic class>");

//
// A few special class names are installed in the lookup table but not
// the class list.  Thus, these classes exist, but are not part of the
// inheritance hierarchy.
// No_class serves as the parent of Object and the other special classes.
// SELF_TYPE is the self class; it cannot be redefined or inherited.
// prim_slot is a class known to the code generator.
//
  addid(No_class,
	new CgenNode(class_(No_class,No_class,nil_Features(),filename),
			    Basic,this));
  addid(SELF_TYPE,
	new CgenNode(class_(SELF_TYPE,No_class,nil_Features(),filename),
			    Basic,this));
  addid(prim_slot,
	new CgenNode(class_(prim_slot,No_class,nil_Features(),filename),
			    Basic,this));

//
// The Object class has no parent class. Its methods are
//        cool_abort() : Object    aborts the program
//        type_name() : Str        returns a string representation of class name
//        copy() : SELF_TYPE       returns a copy of the object
//
// There is no need for method bodies in the basic classes---these
// are already built in to the runtime system.
//
  install_class(
   new CgenNode(
    class_(Object,
	   No_class,
	   append_Features(
           append_Features(
           single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
           single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
           single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
	   filename),
    Basic,this));

//
// The IO class inherits from Object. Its methods are
//        out_string(Str) : SELF_TYPE          writes a string to the output
//        out_int(Int) : SELF_TYPE               "    an int    "  "     "
//        in_string() : Str                    reads a string from the input
//        in_int() : Int                         "   an int     "  "     "
//
   install_class(
    new CgenNode(
     class_(IO,
            Object,
            append_Features(
            append_Features(
            append_Features(
            single_Features(method(out_string, single_Formals(formal(arg, Str)),
                        SELF_TYPE, no_expr())),
            single_Features(method(out_int, single_Formals(formal(arg, Int)),
                        SELF_TYPE, no_expr()))),
            single_Features(method(in_string, nil_Formals(), Str, no_expr()))),
            single_Features(method(in_int, nil_Formals(), Int, no_expr()))),
	   filename),
    Basic,this));

//
// The Int class has no methods and only a single attribute, the
// "val" for the integer.
//
   install_class(
    new CgenNode(
     class_(Int,
	    Object,
            single_Features(attr(val, prim_slot, no_expr())),
	    filename),
     Basic,this));

//
// Bool also has only the "val" slot.
//
    install_class(
     new CgenNode(
      class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename),
      Basic,this));

//
// The class Str has a number of slots and operations:
//       val                                  ???
//       str_field                            the string itself
//       length() : Int                       length of the string
//       concat(arg: Str) : Str               string concatenation
//       substr(arg: Int, arg2: Int): Str     substring
//
   install_class(
    new CgenNode(
      class_(Str,
	     Object,
             append_Features(
             append_Features(
             append_Features(
             append_Features(
             single_Features(attr(val, Int, no_expr())),
            single_Features(attr(str_field, prim_slot, no_expr()))),
            single_Features(method(length, nil_Formals(), Int, no_expr()))),
            single_Features(method(concat,
				   single_Formals(formal(arg, Str)),
				   Str,
				   no_expr()))),
	    single_Features(method(substr,
				   append_Formals(single_Formals(formal(arg, Int)),
						  single_Formals(formal(arg2, Int))),
				   Str,
				   no_expr()))),
	     filename),
        Basic,this));

}

// CgenClassTable::install_class
// CgenClassTable::install_classes
//
// install_classes enters a list of classes in the symbol table.
//
void CgenClassTable::install_class(CgenNodeP nd)
{
  Symbol name = nd->get_name();

  if (probe(name))
    {
      return;
    }

  // set tag
  if (name == Str) {
    stringclasstag = current_tag;
  } else if (name == Int) {
    intclasstag = current_tag;
  } else if (name == Bool) {
    boolclasstag = current_tag;
  }
  nd->tag = current_tag;
  nds.push_back(nd);
  nd_map.insert(std::make_pair(name, nd));
  current_tag++;
  addid(name,nd);
}

void CgenClassTable::install_classes(Classes cs)
{
  for(int i = cs->first(); cs->more(i); i = cs->next(i))
    install_class(new CgenNode(cs->nth(i),NotBasic,this));
}

//
// CgenClassTable::build_inheritance_tree
//
void CgenClassTable::build_inheritance_tree()
{
  for(int i = 0; i < current_tag; i++)
      set_relations(nds[i]);

  // BFS inheritance graph to build attribute and method lists
  std::queue<CgenNodeP> cls_queue;
  cls_queue.push(root());
  while (!cls_queue.empty()) {
    CgenNodeP nd = cls_queue.front(); cls_queue.pop();
    // copy parent's list
    nd->attrs_ordered = nd->get_parentnd()->attrs_ordered;
    nd->methods_ordered = nd->get_parentnd()->methods_ordered;
    nd->methods_class_ordered = nd->get_parentnd()->methods_class_ordered;

    // insert its own features with method dedup
    Features features = nd->get_features();
    for (int i = features->first(); features->more(i); i = features->next(i)) {
      Feature f = features->nth(i);
      if (f->is_method) {
        std::vector<method_class*>::iterator fit;
        bool overrided = false;
        method_class* m = (method_class*) f;
        for (unsigned int j = 0; j < nd->methods_ordered.size(); j++) {
          if (m->name->get_string() == 
              nd->methods_ordered[j]->name->get_string()) {
            overrided = true;
            nd->methods_ordered[j] = m;
            nd->methods_class_ordered[j] = nd;
            break;
          }
        }
        if (!overrided) {
          nd->methods_ordered.push_back(m);
          nd->methods_class_ordered.push_back(nd);
        }
      } else {
        nd->attrs_ordered.push_back((attr_class*) f);
      }
    }

    // add children to BFS queue
    std::set<CgenNodeP>::iterator it;
    for (it = nd->children.begin(); it != nd->children.end(); it++) {
      cls_queue.push(*it);
    }
  }
}

int CgenClassTable::method_index(Symbol c, Symbol m) {
  CgenNodeP n = nd_map[c];
  for (unsigned int i = 0; i < n->methods_ordered.size(); i++) {
    if (m == n->methods_ordered[i]->get_name()) {
      return i;
    }
  }
  return -1;
}

//
// CgenClassTable::set_relations
//
// Takes a CgenNode and locates its, and its parent's, inheritance nodes
// via the class table.  Parent and child pointers are added as appropriate.
//
void CgenClassTable::set_relations(CgenNodeP nd)
{
  CgenNode *parent_node = probe(nd->get_parent());
  nd->set_parentnd(parent_node);
  parent_node->add_child(nd);
}

void CgenNode::add_child(CgenNodeP n)
{
  children.insert(n);
}

void CgenNode::set_parentnd(CgenNodeP p)
{
  assert(parentnd == NULL);
  assert(p != NULL);
  parentnd = p;
}


void CgenClassTable::code_class_nameTab() {
  str << CLASSNAMETAB << LABEL;
  for (int i = 0; i < current_tag; i++) {
    str << WORD;
    stringtable.lookup_string(nds[i]->get_name()->get_string())
      ->code_ref(str);
    str << endl;
  }
}

void CgenClassTable::code_class_objTab() {
  str << CLASSOBJTAB << LABEL;
  for (int i = 0; i < current_tag; i++) {
    str << WORD << nds[i]->get_name() << PROTOBJ_SUFFIX << endl;
    str << WORD << nds[i]->get_name() << CLASSINIT_SUFFIX << endl;
  }

}

void CgenClassTable::code_dispatch_table() {
  for (int i = 0; i < current_tag; i++) {
    str << nds[i]->get_name() << DISPTAB_SUFFIX << LABEL;
    for (unsigned int j = 0; j < nds[i]->methods_ordered.size(); j++) {
      str << WORD << nds[i]->methods_class_ordered[j]->get_name()
          << METHOD_SEP << nds[i]->methods_ordered[j]->get_name()
          << endl;
    }
  }
}

void CgenClassTable::code_class_prototypes() {
  // refer to cool runtime manual Figure 2
  for (int i = 0; i < current_tag; i++) {
    str << WORD << "-1" << endl; // for garbage collector
    str << nds[i]->get_name() << PROTOBJ_SUFFIX << LABEL;
    str << WORD << nds[i]->tag << endl;
    str << WORD << DEFAULT_OBJFIELDS + nds[i]->attrs_ordered.size() << endl;
    str << WORD << nds[i]->get_name() << DISPTAB_SUFFIX << endl;

    for (unsigned int j = 0; j < nds[i]->attrs_ordered.size(); j++) {
      attr_class* attr = nds[i]->attrs_ordered[j];
      str << WORD;

      if (attr->get_type_decl() == Int) {
        IntEntryP ie = inttable.lookup_string("0");
        ie->code_ref(str);
      } else if (attr->get_type_decl() == Bool) {
        falsebool.code_ref(str);
      } else if (attr->get_type_decl() == Str) {
        StringEntryP se = stringtable.lookup_string("");
        se->code_ref(str);
      } else {
        str << 0;
      }

      str << endl;
    }
  }
}

void CgenClassTable::code_class_init() {
  for (int i = 0; i < current_tag; i++) {
    emit_init_ref(nds[i]->get_name(), str); str << LABEL;

    // push registers to stack like funciton calls
    str << "# enter class init: " << nds[i]->get_name() << endl;
    emit_push(FP, str);
    emit_push(RA, str);
    emit_push(SELF, str); // store the pointer to the current object on stack
    emit_addiu(FP, SP, 4, str);
    emit_move(SELF, ACC, str);

    addrTab.enterscope();
    // init parent class
    if (nds[i]->get_parent() != No_class) {
      str << "# init parent class" << endl;
      std::string ptr =
        std::string(nds[i]->get_parent()->get_string()) + CLASSINIT_SUFFIX;
      emit_jal((char*) ptr.c_str(), str);
    }

    // init own attributes
    int start = 0;
    if (nds[i]->get_parent() != No_class) {
      start = nds[i]->get_parentnd()->attrs_ordered.size();
    }

    for (unsigned int j = 0; j < nds[i]->attrs_ordered.size(); j++) {
      attr_class* attr = nds[i]->attrs_ordered[j];
      char* addr = new char[50];
      sprintf(addr, "%d($s0)", j * 4 + 12);
      addrTab.addid(attr->get_name(), addr);
    }

    for (unsigned int j = start; j < nds[i]->attrs_ordered.size(); j++) {
      attr_class* attr = nds[i]->attrs_ordered[j];
      str << "# init attribute: " << attr->get_name() << endl;
      attr->init->code(str);
      if (typeid(*attr->init) == typeid(no_expr_class)) continue;
      emit_store(ACC, j + DEFAULT_OBJFIELDS, SELF, str);
      if (cgen_Memmgr == GC_GENGC) {
        emit_addiu(A1, SELF, 4 * (j + DEFAULT_OBJFIELDS), str);
        emit_gc_assign(str);
      }
    }
    addrTab.exitscope();

    str << "# exit class init: " << nds[i]->get_name() << endl;
    emit_move(ACC, SELF, str);
    emit_pop(SELF, str);
    emit_pop(RA, str);
    emit_pop(FP, str);
    emit_return(str);
  }
}

void CgenClassTable::code()
{
  if (cgen_debug) cout << "coding global data" << endl;
  code_global_data();

  if (cgen_debug) cout << "choosing gc" << endl;
  code_select_gc();

  if (cgen_debug) cout << "coding constants" << endl;
  code_constants();

  if (cgen_debug) cout << "coding class name table" << endl;
  code_class_nameTab();

  if (cgen_debug) cout << "coding class object tables" << endl;
  code_class_objTab();

  if (cgen_debug) cout << "coding dispatch tables" << endl;
  code_dispatch_table();

  if (cgen_debug) cout << "coding class prototypes" << endl;
  code_class_prototypes();

  if (cgen_debug) cout << "coding global text" << endl;
  code_global_text();

  if (cgen_debug) cout << "coding class init" << endl;
  code_class_init();
}


CgenNodeP CgenClassTable::root()
{
   return probe(Object);
}


///////////////////////////////////////////////////////////////////////
//
// CgenNode methods
//
///////////////////////////////////////////////////////////////////////

CgenNode::CgenNode(Class_ nd, Basicness bstatus, CgenClassTableP ct) :
   class__class((const class__class &) *nd),
   parentnd(NULL),
   basic_status(bstatus)
{
   stringtable.add_string(name->get_string());          // Add class name to string table
}


//******************************************************************
//
//   Fill in the following methods to produce code for the
//   appropriate expression.  You may add or remove parameters
//   as you wish, but if you do, remember to change the parameters
//   of the declarations in `cool-tree.h'  Sample code for
//   constant integers, strings, and booleans are provided.
//
//*****************************************************************

void class__class::code(ostream &s) {
  addrTab.enterscope();
  CgenNodeP n = codegen_classtable->nd_map[name];

  for (unsigned int j = 0; j < n->attrs_ordered.size(); j++) {
    attr_class* attr = n->attrs_ordered[j];
    char* addr = new char[50];
    sprintf(addr, "%d($s0)", j * 4 + 12);
    addrTab.addid(attr->get_name(), addr);
  }

  for(int i = features->first(); features->more(i); i = features->next(i)) {
    if (features->nth(i)->is_method) {
      ((method_class *)(features->nth(i)))->code(s);
    }
  }
  addrTab.exitscope();
}

void method_class::code(ostream &s) {
  addrTab.enterscope();
  int nf = formals->len();

  for(int i = formals->first(); formals->more(i); i = formals->next(i)) {
    formal_class* f = (formal_class *)formals->nth(i);
    char* addr = new char[50];
    sprintf(addr, "%d($fp)", i * 4 + 12);
    addrTab.addid(f->get_name(), addr);
  }

  emit_method_ref(current_class->name, name, s); s << LABEL;

  emit_push(FP, s);
  emit_push(RA, s);
  emit_push(SELF, s);
  emit_move(SELF, ACC, s);
  emit_addiu(FP, SP, 4, s);

  s << "# coding method expr" << endl;
  fp_offset = 0; // reset stack distance to frame pointer
  expr->code(s);

  s << "# exit method: "; emit_method_ref(current_class->name, name, s); s << endl;
  emit_load(SELF, 1, SP, s);
  emit_load(RA, 2, SP, s);
  emit_load(FP, 3, SP, s);
  emit_addiu(SP, SP, 4 * (nf + 3), s);
  emit_return(s);
  addrTab.exitscope();
}

void assign_class::code(ostream &s) {
  expr->code(s);
  char* addr = addrTab.lookup(name);
  s << SW << ACC << " " << addr << endl;
  if (cgen_Memmgr == GC_GENGC) {
    emit_load_address(A1, addr, s);
    emit_gc_assign(s);
  }
}

void handle_dispatch_on_void(ostream& s, int line_number) {
  int continue_label = label++;
  emit_bne(ACC, ZERO, continue_label, s);
  s << LA << ACC << " " << STRCONST_PREFIX << "0" << endl;
  emit_load_imm(T1, line_number, s);
  emit_jal("_dispatch_abort", s);
  emit_label_def(continue_label, s);
}

void static_dispatch_class::code(ostream &s) {
  int na = actual->len();
  for(int i = actual->first(); actual->more(i); i = actual->next(i)) {
    Expression e = (Expression) actual->nth(na - 1 - i);
    e->code(s);
    emit_push(ACC, s);
  }

  expr->code(s);
  handle_dispatch_on_void(s, line_number);

  Symbol class_name = type_name;
  std::string class_str(type_name->get_string());
  if (type_name == SELF_TYPE) {
    class_name = current_class->get_name();
    class_str = current_class->get_name()->get_string();
  }

  std::string disp_tab = class_str + DISPTAB_SUFFIX;
  int method_offset = codegen_classtable->method_index(class_name, name);
  emit_load_address(T1, (char*) disp_tab.c_str(), s);
  emit_load(T1, method_offset, T1, s);
  emit_jalr(T1, s);
}

void dispatch_class::code(ostream &s) {
  int na = actual->len();
  for(int i = actual->first(); actual->more(i); i = actual->next(i)) {
    Expression e = (Expression) actual->nth(na - 1 - i);
    s << "# code dispatch actual number " << na-1-i  << " out of " 
      << na << endl;
    e->code(s);
    emit_push(ACC, s);
  }

  s << "# code dispatch expr" <<endl;
  expr->code(s);
  //handle_dispatch_on_void(s, line_number);

  //dispatch on void

  Symbol class_name = expr->get_type();
  if (class_name == SELF_TYPE) {
    class_name = current_class->get_name();
  }

  s << "# code dispatch method call: " << class_name << "." 
    << name <<endl;
  int method_offset = codegen_classtable->method_index(class_name, name);
  emit_load(T1, 2, ACC, s);
  emit_load(T1, method_offset, T1, s);
  emit_jalr(T1, s);

}

void cond_class::code(ostream &s) {
  int false_branch = label++;
  int end_branch = label++;

  pred->code(s);
  emit_load(T1, 3, ACC, s);
  emit_beqz(T1, false_branch, s);

  then_exp->code(s);
  emit_branch(end_branch, s);

  emit_label_def(false_branch, s);
  else_exp->code(s);

  emit_label_def(end_branch, s);
}

void loop_class::code(ostream &s) {
  int cond_branch = label++;
  int end_branch = label++;


  emit_label_def(cond_branch, s);
  pred->code(s);
  emit_load(T1, 3, ACC, s);
  emit_beqz(T1, end_branch, s);

  body->code(s);
  emit_branch(cond_branch, s);

  emit_label_def(end_branch, s);
  emit_move(ACC, ZERO, s);
}

void emit_match_children_class(
  char* reg,
  CgenNodeP n,
  int expr_label,
  ostream& s) {
  emit_load_imm(T3, n->tag, s);
  emit_beq(reg, T3, expr_label, s);

  std::set<CgenNodeP>::iterator it;
  for (it = n->children.begin(); it != n->children.end(); it++) {
    emit_match_children_class(reg, *it, expr_label, s);
  }
}

void typcase_class::code(ostream &s) {
  expr->code(s);
  // case on void
  int continue_label = label++;
  emit_bne(ACC, ZERO, continue_label, s);
  s << LA << ACC << " " << STRCONST_PREFIX << "0" << endl;
  emit_load_imm(T1, line_number, s);
  emit_jal("_case_abort2", s);

  emit_label_def(continue_label, s);
  emit_load(T1, 0, ACC, s); // class tag of expr, it will not be overwritten
  int end_label = label++;

  emit_push(ACC, s);
  fp_offset -= 4;
  char* addr = new char[50];
  sprintf(addr, "%d($fp)", fp_offset);

  for(int i = cases->first(); cases->more(i); i = cases->next(i)) {
    addrTab.enterscope();
    branch_class* b = (branch_class*) cases->nth(i);
    CgenNodeP nd = codegen_classtable->nd_map[b->type_decl];

    int expr_label = label++;
    int next_label = label++;
    emit_match_children_class(T1, nd, expr_label, s);
    emit_branch(next_label, s);

    emit_label_def(expr_label, s);
    addrTab.addid(b->name, addr);
    b->expr->code(s);
    emit_pop(T2, s); // T2 is not used
    emit_branch(end_label, s);
    emit_label_def(next_label, s);

    addrTab.exitscope();
  }
  // case no match
  emit_jal("_case_abort", s);
  emit_label_def(end_label, s);

  fp_offset += 4;
}

bool is_basic_class(Symbol c) {
  return c == Int || c == Bool || c == Str || c == IO;
}

void let_class::code(ostream &s) {
  addrTab.enterscope();

  if (typeid(*init) == typeid(no_expr_class) && is_basic_class(type_decl)) {
    std::string class_name = std::string(type_decl->get_string());
    std::string prototype = class_name + PROTOBJ_SUFFIX;
    std::string init_method = class_name + CLASSINIT_SUFFIX;
    emit_load_address(ACC, (char*) prototype.c_str(), s);
    emit_jal("Object.copy", s);
    emit_jal((char*) init_method.c_str(), s);
  } else {
    init->code(s);
  }

  // put identifier's instance to stack and record it's position relative to
  // frame pointer in order to reference it.
  emit_push(ACC, s);
  fp_offset -= 4;
  char* addr = new char[50];
  sprintf(addr, "%d($fp)", fp_offset);
  addrTab.addid(identifier, addr);

  body->code(s);

  emit_pop(T1, s); // don't touch ACC returned from body
  fp_offset += 4;
  addrTab.exitscope();
}

void branch_class::code(ostream &s) {
  expr->code(s);
}

void block_class::code(ostream &s) {
  for(int i = body->first(); body->more(i); i = body->next(i)) {
    Expression e = body->nth(i);
    e->code(s);
  }
}

void double_reg(Expression e1, Expression e2, ostream &s) {
  e1->code(s);
  emit_load(T1, 3, ACC, s);
  emit_push(T1, s);
  e2->code(s);
  emit_jal("Object.copy", s);
  emit_load(T2, 3, ACC, s);
  emit_pop(T1, s);
}

void plus_class::code(ostream &s) {
  double_reg(e1, e2, s);
  emit_add(T3, T1, T2, s);
  emit_store(T3, 3, ACC, s);
}

void sub_class::code(ostream &s) {
  double_reg(e1, e2, s);
  emit_sub(T3, T1, T2, s);
  emit_store(T3, 3, ACC, s);
}

void mul_class::code(ostream &s) {
  double_reg(e1, e2, s);
  emit_mul(T3, T1, T2, s);
  emit_store(T3, 3, ACC, s);
}

void divide_class::code(ostream &s) {
  double_reg(e1, e2, s);
  emit_div(T3, T1, T2, s);
  emit_store(T3, 3, ACC, s);
}

void comp_common_begin(Expression e1, Expression e2, ostream &s) {
  double_reg(e1, e2, s);

  emit_partial_load_address(ACC, s);
  truebool.code_ref(s); s << endl;
}

void comp_common_end(Expression e1, Expression e2, ostream &s) {
  emit_partial_load_address(ACC, s);
  falsebool.code_ref(s); s << endl;
}

void neg_class::code(ostream &s) {
  e1->code(s);
  emit_load(T1, 3, ACC, s);
  emit_neg(T2, T1, s);
  emit_store(T2, 3, ACC, s);
}

void lt_class::code(ostream &s) {
  comp_common_begin(e1, e2, s);

  int tmp_label = label++;
  emit_blt(T1, T2, tmp_label, s);

  comp_common_end(e1, e2, s);

  emit_label_def(tmp_label, s);
}

void eq_class::code(ostream &s) {
  e1->code(s);
  emit_push(ACC, s);
  e2->code(s);
  emit_move(T2, ACC, s);
  emit_pop(T1, s);

  emit_partial_load_address(ACC, s);
  truebool.code_ref(s); s << endl;

  int tmp_label = label++;
  emit_beq(T1, T2, tmp_label, s);

  emit_partial_load_address(ACC, s);
  falsebool.code_ref(s); s << endl;

  emit_label_def(tmp_label, s);
}

void leq_class::code(ostream &s) {
  comp_common_begin(e1, e2, s);

  int tmp_label = label++;
  emit_bleq(T1, T2, tmp_label, s);

  comp_common_end(e1, e2, s);

  emit_label_def(tmp_label, s);
}

void comp_class::code(ostream &s) {
  e1->code(s);

  emit_load(T1, 3, ACC, s);

  emit_partial_load_address(ACC, s);
  truebool.code_ref(s); s << endl;

  int tmp_label = label++;
  emit_beqz(T1, tmp_label, s);

  emit_partial_load_address(ACC, s);
  falsebool.code_ref(s); s << endl;

  emit_label_def(tmp_label, s);
}

void int_const_class::code(ostream& s)
{
  //
  // Need to be sure we have an IntEntry *, not an arbitrary Symbol
  //
  emit_load_int(ACC,inttable.lookup_string(token->get_string()),s);
}

void string_const_class::code(ostream& s)
{
  emit_load_string(ACC,stringtable.lookup_string(token->get_string()),s);
}

void bool_const_class::code(ostream& s)
{
  emit_load_bool(ACC, BoolConst(val), s);
}

void new__class::code(ostream &s) {
  std::string class_name = std::string(type_name->get_string());
  if (type_name == SELF_TYPE) { // need contextual info
    emit_load(T1, 0, SELF, s);
    emit_sll(T1, T1, 3, s);
    emit_load_address(T2, CLASSOBJTAB, s);
    emit_addu(T2, T2, T1, s);
    emit_move(ACC, T1, s);
    emit_push(T1, s);
    emit_jal("Object.copy", s);
    emit_pop(T1, s);
    emit_load(T2, 1, T1, s);
    emit_jalr(T2, s);
  } else {
    std::string prototype = class_name + PROTOBJ_SUFFIX;
    std::string init_method = class_name + CLASSINIT_SUFFIX;
    emit_load_address(ACC, (char*) prototype.c_str(), s);
    emit_jal("Object.copy", s);
    emit_jal((char*) init_method.c_str(), s);
  }
}

void isvoid_class::code(ostream &s) {
  emit_move(T1, ACC, s);

  emit_partial_load_address(ACC, s);
  truebool.code_ref(s); s << endl;

  int tmp_label = label++;
  emit_beqz(T1, tmp_label, s);

  emit_partial_load_address(ACC, s);
  falsebool.code_ref(s); s << endl;

  emit_label_def(tmp_label, s);
}

void no_expr_class::code(ostream &s) {
  emit_move(ACC, ZERO, s);
}

void object_class::code(ostream &s) {
  s << "# object class: " << name << endl;
  if (name == self) {
    emit_move(ACC, SELF, s);
  } else {
    char* addr = addrTab.lookup(name);
    s << LW << ACC << " " << addr <<endl;
  }
}

// tricky code
// dispatch table
// object copy during basic arithmetic operations
// $s0 register
