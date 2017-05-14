

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include "semant.h"
#include "utilities.h"

#define IG(a, b) inheritance_graph[a].insert(b)
#define CI(a, b) class_info.insert(std::make_pair(a, b))

typedef std::set<Symbol>::iterator SymSetIter;

extern int semant_debug;
extern char *curr_filename;

//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
static Symbol 
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



ClassTable::ClassTable(Classes classes) : semant_errors(0) , error_stream(cerr) {
  install_basic_classes();
  install_program_classes(classes);
  check_cycle();
  install_class_features();
  if (semant_errors > 0) {
    abort();
  }
}

void ClassTable::install_basic_classes() {

    // The tree package uses these globals to annotate the classes built below.
   // curr_lineno  = 0;
    Symbol filename = stringtable.add_string("<basic class>");
    
    // The following demonstrates how to create dummy parse trees to
    // refer to basic Cool classes.  There's no need for method
    // bodies -- these are already built into the runtime system.
    
    // IMPORTANT: The results of the following expressions are
    // stored in local variables.  You will want to do something
    // with those variables at the end of this method to make this
    // code meaningful.

    // 
    // The Object class has no parent class. Its methods are
    //        abort() : Object    aborts the program
    //        type_name() : Str   returns a string representation of class name
    //        copy() : SELF_TYPE  returns a copy of the object
    //
    // There is no need for method bodies in the basic classes---these
    // are already built in to the runtime system.

    Class_ Object_class =
	class_(Object, 
	       No_class,
	       append_Features(
			       append_Features(
					       single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
					       single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
			       single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
	       filename);

    // 
    // The IO class inherits from Object. Its methods are
    //        out_string(Str) : SELF_TYPE       writes a string to the output
    //        out_int(Int) : SELF_TYPE            "    an int    "  "     "
    //        in_string() : Str                 reads a string from the input
    //        in_int() : Int                      "   an int     "  "     "
    //
    Class_ IO_class = 
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
	       filename);  

    //
    // The Int class has no methods and only a single attribute, the
    // "val" for the integer. 
    //
    Class_ Int_class =
	class_(Int, 
	       Object,
	       single_Features(attr(val, prim_slot, no_expr())),
	       filename);

    //
    // Bool also has only the "val" slot.
    //
    Class_ Bool_class =
	class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename);

    //
    // The class Str has a number of slots and operations:
    //       val                                  the length of the string
    //       str_field                            the string itself
    //       length() : Int                       returns length of the string
    //       concat(arg: Str) : Str               performs string concatenation
    //       substr(arg: Int, arg2: Int): Str     substring selection
    //       
    Class_ Str_class =
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
	       filename);

  IG(No_class, Object);
  IG(Object, IO);
  IG(Object, Int);
  IG(Object, Bool);
  IG(Object, Str);

  CI(Object, (class__class *)Object_class);
  CI(IO, (class__class *)IO_class);
  CI(Int, (class__class *)Int_class);
  CI(Bool, (class__class *)Bool_class);
  CI(Str, (class__class *)Str_class);
}

void ClassTable::install_program_classes(Classes classes) {
  bool has_main_class = false;
  
  // first pass to check and build class_info map
  for (int i = classes->first(); classes->more(i); i = classes->next(i)) {
    class__class* c = (class__class*) classes->nth(i);
    
    // check if the class is already defined
    if (class_info.find(c->get_name()) != class_info.end()) {
      semant_error(c->get_filename(), c)
        << "The class has already been defined: " << c->get_name()
        << endl;
    }    
    
    // check existence of main class
    if (c->get_name()==Main) {
      has_main_class = true;
    }
  
    CI(c->get_name(), c);
  }


  if (!has_main_class) {
    semant_error()<<"Main class is not defined"<<endl;
  }
  
  // second pass to check and build inheritance map
  for (int i = classes->first(); classes->more(i); i = classes->next(i)) {
    class__class* c = (class__class*) classes->nth(i);
    Symbol parent = c->get_parent();

    if (parent==Int || parent==Bool || parent==Str || parent==SELF_TYPE) {
      semant_error(c->get_filename(), c)
      << "parent cannot be basic classes or SELF_TYPE!" << endl;
    }
   
    if (class_info.find(parent)==class_info.end()) {
      semant_error(c->get_filename(), c)
      << "parent class does not exist!"<< endl;
    }

    IG(parent, c->get_name());
  }
}

void ClassTable::check_cycle(){
  cycle_found = false;

  std::map<Symbol, std::set<Symbol> >:: iterator it;
  std::set<Symbol> visited;

  for (it = inheritance_graph.begin(); it != inheritance_graph.end(); ++it){
     DFS(visited, it->first);
     if (cycle_found){
       break;
     }
  }

  if (cycle_found){
    semant_error() << "cycle exists in inheritance graph!" << endl;
  }
}

void ClassTable::DFS(std::set<Symbol> visited, Symbol c){
  std::set<Symbol>::iterator it;
  
  if (visited.find(c)!=visited.end()){
    cycle_found = true;
    return;
  }
  else{
    visited.insert(c);
  }

  for (it = inheritance_graph[c].begin(); it!= inheritance_graph [c].end(); ++it){
    DFS(visited, *it);
  }
}

void ClassTable::verify_method_formals(class__class* c, method_class* m) {
  // use to verify inherited method has the same formals
  Formals parent_formals = NULL;
  bool has_parent_method = false;
  Formals formals = m->get_formals();

  if (class_method_map[c->get_name()].count(m->get_name())) {
    method_class* pm = 
      (method_class*) class_method_map[c->get_name()][m->get_name()];
    parent_formals = pm->get_formals();
    has_parent_method = true;
    // check return type
    if (pm->get_return_type()->get_string() != m->get_return_type()->get_string()) {
      semant_error(c->get_filename(), m)
        << "Inherited method has different return type than parent definition: " << m->get_name()
        << endl;
    }
    // check # of formals are the same
    if (formals->len() != parent_formals->len()) {
      semant_error(c->get_filename(), m)
        << "Inherited method has different number of formals than parent definition: " << m->get_name()
        << endl;
    }
  }

  for (int i = formals->first(); formals->more(i); i = formals->next(i)) {
    formal_class* fm = (formal_class*) formals->nth(i);

    // check formal types
    if (fm->get_name() == self) {
      semant_error(c->get_filename(), m)
        << "formal cannot by self" << endl;
    }
    // check parent method has same formal type
    if (has_parent_method) {
      formal_class* pfm = (formal_class*) parent_formals->nth(i);
      if (fm->get_type_decl()->get_string() != pfm->get_type_decl()->get_string()) {
        semant_error(c->get_filename(), m)
          << "Inherited method has different formal type: "
          << fm->get_name() << endl;
      }
    }

    // check return type is well defined
    if (m->get_return_type() != SELF_TYPE &&
        !class_info.count(m->get_return_type())) {
        semant_error(c->get_filename(), m)
          << "method doesn't have a valid return type." << endl;
    }
  }
  return;
}

void ClassTable::install_class_features() {
  bool has_main_method = false;

  // Use BFS to iterate over all class top-down, and fill in the class-methods map
  std::queue<Symbol> class_queue;
  class_queue.push(Object);
  while (!class_queue.empty()) {
    Symbol c = class_queue.front();
    class_queue.pop();

    class__class* cls = class_info[c];
    Features features = cls->get_features();

    // inherit all parent class features
    class_method_map[c] = class_method_map[cls->get_parent()];
    class_attr_map[c] = class_attr_map[cls->get_parent()];

    for(int i = features->first(); features->more(i); i = features->next(i)) {
      Feature f = features->nth(i);

      if (f->is_attribute()) {
        attr_class* a = (attr_class*) f;
        if (class_attr_map[c].count(a->get_name())) {
          semant_error(cls->get_filename(), cls)
            << "Attributes definted in parent class cannot be redefined: "
            << a->get_name() << endl;
        }
        class_attr_map[c][a->get_name()] = a;
      } else { //method
        method_class* m = (method_class*) f;
        verify_method_formals(cls, m);

        if (c ==  Main && m->get_name() == main_meth) {
          if (m->get_formals()->len() > 0) {
            semant_error(cls->get_filename(), cls)
              << "Formals are not allowed in main function. "
              << endl;
          }
          has_main_method = true;
        }
        class_method_map[c][m->get_name()] = m;
      }
    }

    // enqueue all child classes
    std::set<Symbol> child_classes = inheritance_graph[c];
    SymSetIter it = child_classes.begin();
    while (it != child_classes.end()) {
      class_queue.push(*it);
      ++it;
    }
  }

  if (!has_main_method) {
    semant_error() << "No main method defined!" << endl;
  }
}

void ClassTable::abort(){
  cerr<<"Abort compilation"<<endl;
  exit(1);
}

////////////////////////////////////////////////////////////////////
//
// semant_error is an overloaded function for reporting errors
// during semantic analysis.  There are three versions:
//
//    ostream& ClassTable::semant_error()                
//
//    ostream& ClassTable::semant_error(Class_ c)
//       print line number and filename for `c'
//
//    ostream& ClassTable::semant_error(Symbol filename, tree_node *t)  
//       print a line number and filename
//
///////////////////////////////////////////////////////////////////

ostream& ClassTable::semant_error(Class_ c)
{                                                             
    return semant_error(c->get_filename(),c);
}    

ostream& ClassTable::semant_error(Symbol filename, tree_node *t)
{
    error_stream << filename << ":" << t->get_line_number() << ": ";
    return semant_error();
}

ostream& ClassTable::semant_error()                  
{                                                 
    semant_errors++;                            
    return error_stream;
} 



/*   This is the entry point to the semantic checker.

     Your checker should do the following two things:

     1) Check that the program is semantically correct
     2) Decorate the abstract syntax tree with type information
        by setting the `type' field in each Expression node.
        (see `tree.h')

     You are free to first do 1), make sure you catch all semantic
     errors. Part 2) can be done in a second stage, when you want
     to build mycoolc.
 */
void program_class::semant()
{
    initialize_constants();

    /* ClassTable constructor may do some semantic analysis */
    ClassTable *classtable = new ClassTable(classes);

    /* some semantic analysis code may go here */

    if (classtable->errors()) {
	cerr << "Compilation halted due to static semantic errors." << endl;
	exit(1);
    }
}


