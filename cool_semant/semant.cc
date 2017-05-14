

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <typeinfo>
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
    if (c->get_name() == Main) {
      has_main_class = true;
    }
  
    CI(c->get_name(), c);
  }


  if (!has_main_class) {
    semant_error() << "Main class is not defined" << endl;
  }
  
  // second pass to check and build inheritance map
  for (int i = classes->first(); classes->more(i); i = classes->next(i)) {
    class__class* c = (class__class*) classes->nth(i);
    Symbol parent = c->get_parent();
   
    if (class_info.find(parent) == class_info.end()) {
      semant_error(c->get_filename(), c)
        << "parent class does not exist!" << endl;
    }

    if (parent == Int || parent == Bool || parent == Str || parent == SELF_TYPE) {
      semant_error(c->get_filename(), c)
        << "parent cannot be basic classes or SELF_TYPE!" << endl;
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

  if (class_method_map[c->get_parent()].count(m->get_name())) {
    method_class* pm = 
      (method_class*) class_method_map[c->get_parent()][m->get_name()];
    parent_formals = pm->get_formals();
    has_parent_method = true;
    // check return type
    if (pm->get_return_type() != m->get_return_type()) {
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

  // check return type is well defined
  if (m->get_return_type() != SELF_TYPE &&
      !class_info.count(m->get_return_type())) {
      semant_error(c->get_filename(), m)
        << "method doesn't have a valid return type." << endl;
  }

  for (int i = formals->first(); formals->more(i); i = formals->next(i)) {
    formal_class* fm = (formal_class*) formals->nth(i);

    // check formal types
    if (fm->get_name() == self) {
      semant_error(c->get_filename(), m)
        << "formal cannot be self" << endl;
    }
    // check parent method has same formal type
    if (has_parent_method) {
      formal_class* pfm = (formal_class*) parent_formals->nth(i);
      if (fm->get_type_decl() != pfm->get_type_decl()) {
        semant_error(c->get_filename(), m)
          << "Inherited method has different formal type: "
          << fm->get_name() << endl;
      }
    }
  }
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

    for(int i = features->first(); features->more(i); i = features->next(i)) {
      Feature f = features->nth(i);

      if (f->is_attribute()) {
        attr_class* a = (attr_class*) f;
        if (class_attr_map[cls->get_parent()].count(a->get_name())) {
          semant_error(cls->get_filename(), a)
            << "Attributes definted in parent class cannot be redefined: "
            << a->get_name() << endl;
        }
        if (class_attr_map[c].count(a->get_name())) {
          semant_error(cls->get_filename(), a)
            << "Attributes cannot be redefined: "
            << a->get_name() << endl;
        }
        class_attr_map[c][a->get_name()] = a;
      } else { //method
        method_class* m = (method_class*) f;
        verify_method_formals(cls, m);

        if (c ==  Main && m->get_name() == main_meth) {
          if (m->get_formals()->len() > 0) {
            semant_error(cls->get_filename(), m)
              << "Formals are not allowed in main function. "
              << endl;
          }
          has_main_method = true;
        }
        
        if (class_method_map[c].count(m->get_name())){
          semant_error(cls->get_filename(), m)
          << "Method definition duplicates within class:"
          << c << endl;
        }
        else{
          class_method_map[c][m->get_name()] = m;
        }
      }
    }

    // inherit all parent class features
    class_method_map[c].insert(
      class_method_map[cls->get_parent()].begin(),
      class_method_map[cls->get_parent()].end()
    );
    class_attr_map[c].insert(
      class_attr_map[cls->get_parent()].begin(), 
      class_attr_map[cls->get_parent()].end()
    );
    
    // enqueue all child classes
    std::set<Symbol> child_classes = inheritance_graph[c];
    SymSetIter it = child_classes.begin();
    while (it != child_classes.end()) {
      class_queue.push(*it);
      ++it;
    }
  }

  if (!has_main_method) {
    semant_error() << "No main method defined within Main class!" << endl;
  }
}

void ClassTable::abort(){
  cerr<<"Abort compilation"<<endl;
  exit(1);
}

bool ClassTable::check_child_class(Symbol parent, Symbol child) {
  while (parent != child && child != Object) {
    if (class_info.count(child)) break;
    child = class_info[child]->get_parent();
  }
  return parent == child;
}

bool ClassTable::check_class_exists(Symbol c) {
  return c == SELF_TYPE || class_info.count(c);
}

bool ClassTable::has_method(Symbol c, Symbol m) {
  // class doesn't exist
  if (!class_method_map.count(c)) return false;
  return class_method_map[c].count(m); 
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

    TypeChecker typechecker(classtable);
    /* some semantic analysis code may go here */

    typechecker.check(this);

    if (classtable->errors()) {
	cerr << "Compilation halted due to static semantic errors." << endl;
	exit(1);
    }
    delete classtable;
}

ostream& TypeChecker::semant_error(tree_node *t) {
  if (current_class) {
    return class_table->semant_error(current_class->get_filename(), t);
  } else {
    return class_table->semant_error();
  }
}

Symbol ClassTable::least_upper_bound(Symbol ca, Symbol cb, Symbol c) {
  if (ca == cb) return ca; // cover both ca and cb are SELF_TYPE
  if (ca == SELF_TYPE) ca = c; // current class
  if (cb == SELF_TYPE) cb = c; // current class

  Symbol lub = Object;
  bool is_lub = true;
  while (is_lub) {
    std::set<Symbol>::iterator it;
    std::set<Symbol> children = inheritance_graph[lub];
    is_lub = false;

    for (it = children.begin(); it != children.end(); it++) {
      if (check_child_class(*it, ca) && check_child_class(*it, cb)) {
        is_lub = true;
        lub = *it;
        break;
      }
    }
  }

  return lub;
}
void TypeChecker::check(program_class* p) {
  enterscope();
  Classes classes = p->get_classes();
  for(int i = classes->first(); classes->more(i); i = classes->next(i)) {
    class__class* cls = (class__class*)classes->nth(i);
    check(cls);
  }
  exitscope();
}

void TypeChecker::check(class__class* cls) {
  enterscope();
  current_class = cls;
  Features features = cls->get_features();
  for(int i = features->first(); features->more(i); i = features->next(i)) {
    Feature f = (Feature) features->nth(i);
    if (f->is_attribute()) {
      attr_class* a = (attr_class*) f;
      tree_node* n = probe(a->get_name());
      if (n == NULL) { // TODO conflict with other classes?
        addid(a->get_name(), a);
      }
      check(a);
    } else {
      method_class* m = (method_class*) f;
      check(m);
    }
  }
  exitscope();
}

void TypeChecker::check(attr_class* a) {
  if (a->get_name() == self) {
    semant_error(a) << "attribute cannot be self type." << endl;
  }

  if (!class_table->check_class_exists(a->get_type_decl())) {
    semant_error(a) << "attribute's class "
      << a->get_type_decl() << " is undefined." << endl;
  }

  Expression e = a->get_init(); 
  check(e);
  // no initialization
  if (e->get_type() == NULL) return;

  // check init type matches declaration type
  Symbol decl_type = a->get_type_decl();
  Symbol init_type = e->get_type();
  if (decl_type == SELF_TYPE) {
    decl_type = current_class->get_name();
  }
  if (init_type == SELF_TYPE) {
    init_type = current_class->get_name();
  }
  if (!class_table->check_child_class(decl_type, init_type)) {
    semant_error(a) << "attribute " << a->get_name()
      << " declaratino type " << decl_type 
      << " doesn't match initialization type " << init_type << endl;
  }
}

void TypeChecker::check(method_class* m) {
  enterscope();
  Formals formals = m->get_formals();
  for(int i = formals->first(); formals->more(i); i = formals->next(i)) {
    formal_class* f = (formal_class*)formals->nth(i);
    check(f);
    tree_node* node = probe(f->get_name());
    if (node) {
      semant_error(m) << "Formal " << f->get_name()
        << " is defined multiple time" << endl;
    } else {
      addid(f->get_name(), f);
    }
  }

  check(m->get_expr());

  // check return type through graph inheritance graph
  Symbol return_type_method = m->get_return_type();
  Symbol return_type_expr = m->get_expr()->get_type();
  if (return_type_method == SELF_TYPE) {
    return_type_method = current_class->get_name();
  }
  if (return_type_expr == SELF_TYPE) {
    return_type_expr = current_class->get_name();
  }
  if (!class_table->check_child_class(return_type_method, return_type_expr)) {
    semant_error(m) << "method return types don't match." << endl;
  }
  exitscope();
}

void TypeChecker::check(formal_class* f) {
  if (!class_table->check_class_exists(f->get_type_decl())) {
    semant_error(f) << "Formal " << f->get_name() 
      <<" deslaration class " << f->get_type_decl()
      << " doesn't exist." << endl;
  }
  if (f->get_type_decl() == SELF_TYPE) {
    semant_error(f) << "Formal " << f->get_name() 
      << " type cannot be SELF_TYPE." << endl;
  }
}

void TypeChecker::check(Expression_class* e) {
  if (typeid(*e) == typeid(isvoid_class)) {
    isvoid_class* ce = (isvoid_class*) e;
    check(ce);
  } else if (typeid(*e) == typeid(new__class)) {
    new__class* ce = (new__class*) e;
    check(ce);
  } else if (typeid(*e) == typeid(no_expr_class)) {
    no_expr_class* ce = (no_expr_class*) e;
    check(ce);
  } else if (typeid(*e) == typeid(object_class)) {
    object_class* ce = (object_class*) e;
    check(ce);
  } else if (typeid(*e) == typeid(comp_class)) {
    comp_class* ce = (comp_class*) e;
    check(ce);
  } else if (typeid(*e) == typeid(branch_class)) {
    branch_class* ce = (branch_class*) e;
    check(ce);
  } else if (typeid(*e) == typeid(assign_class)) {
    assign_class* ce = (assign_class*) e;
    check(ce);
  } else if (typeid(*e) == typeid(static_dispatch_class)) {
    static_dispatch_class* ce = (static_dispatch_class*) e;
    check(ce);
  } else if (typeid(*e) == typeid(dispatch_class)) {
    dispatch_class* ce = (dispatch_class*) e;
    check(ce);
  } else if (typeid(*e) == typeid(cond_class)) {
    cond_class* ce = (cond_class*) e;
    check(ce);
  } else if (typeid(*e) == typeid(loop_class)) {
    loop_class* ce = (loop_class*) e;
    check(ce);
  } else if (typeid(*e) == typeid(typcase_class)) {
    typcase_class* ce = (typcase_class*) e;
    check(ce);
  } else if (typeid(*e) == typeid(block_class)) {
    block_class* ce = (block_class*) e;
    check(ce);
  } else if (typeid(*e) == typeid(let_class)) {
    let_class* ce = (let_class*) e;
    check(ce);
  } else if (typeid(*e) == typeid(plus_class)) {
    plus_class* ce = (plus_class*) e;
    check(ce);
  } else if (typeid(*e) == typeid(sub_class)) {
    sub_class* ce = (sub_class*) e;
    check(ce);
  } else if (typeid(*e) == typeid(mul_class)) {
    mul_class* ce = (mul_class*) e;
    check(ce);
  } else if (typeid(*e) == typeid(divide_class)) {
    divide_class* ce = (divide_class*) e;
    check(ce);
  } else if (typeid(*e) == typeid(neg_class)) {
    neg_class* ce = (neg_class*) e;
    check(ce);
  } else if (typeid(*e) == typeid(lt_class)) {
    lt_class* ce = (lt_class*) e;
    check(ce);
  } else if (typeid(*e) == typeid(eq_class)) {
    eq_class* ce = (eq_class*) e;
    check(ce);
  } else if (typeid(*e) == typeid(leq_class)) {
    leq_class* ce = (leq_class*) e;
    check(ce);
  } else if (typeid(*e) == typeid(int_const_class)) {
    int_const_class* ce = (int_const_class*) e;
    check(ce);
  } else if (typeid(*e) == typeid(bool_const_class)) {
    bool_const_class* ce = (bool_const_class*) e;
  } else if (typeid(*e) == typeid(string_const_class)) {
    string_const_class* ce = (string_const_class*) e;
    check(ce);
  }
}

void TypeChecker::check(isvoid_class* e) {
  check(e->get_e1());
  e->set_type(Bool);
}

void TypeChecker::check(new__class* e) {
  Symbol type_name = e->get_type_name();
  if (!class_table->check_class_exists(type_name)) {
    semant_error(e) << "new: class " << type_name
      << " is not registered." << endl;
    e->set_type(Object);
    return;
  }
  e->set_type(type_name); 
}

void TypeChecker::check(no_expr_class* e) {
  return;
}

void TypeChecker::check(object_class* e) {
  if (e->get_name() == self) {
    e->set_type(SELF_TYPE);
    return;
  }
  tree_node* node = lookup(e->get_name());
  if (!node) {
    semant_error(e) << "identifier is not defined" << endl;
    e->set_type(Object);
    return;
  }
  e->set_type(get_tree_node_type(node));
}

Symbol TypeChecker::get_tree_node_type(tree_node* node) {
  Symbol type = NULL;
  if (typeid(*node) == typeid(attr_class)) {
    type = ((attr_class*) node)->get_type_decl();
  } else if (typeid(*node) == typeid(formal_class)) {
    type = ((formal_class*) node)->get_type_decl();
  } else if (typeid(*node) == typeid(branch_class)) {
    type = ((branch_class*) node)->get_type_decl();
  } else if (typeid(*node) == typeid(let_class)) {
    type = ((let_class*) node)->get_type_decl();
  }
  return type;
}

void TypeChecker::check(comp_class* e) {
  check(e->get_e1());
  if (e->get_e1()->get_type() == Bool){
    e->set_type(Bool);
  }else{
    semant_error(e) << "comp subexpression type is not Bool!" << endl;
    e->set_type(Object);
  }
}

void TypeChecker::check(branch_class* e) {
  check(e->get_expr());

  if (!class_table->check_class_exists(e->get_type_decl())) {
    semant_error(e) << "case branch: class doesn't exists." <<endl;
  }
  if (e->get_name() == self) {
    semant_error(e) << "case branch: identifier cannot be self" << endl;
  }
  if (e->get_type_decl() == SELF_TYPE) {
    semant_error(e) << "case branch: class cannot be SELF_TYPE." << endl;
  }
}

void TypeChecker::check(assign_class* e) {
  check(e->get_expr());
  Symbol name = e->get_name();
  //check name
  if (name == self) {
    semant_error(e) << "Cannot assign to self." << endl;
    e->set_type(Object);
    return;
  }
  tree_node* node = lookup(name);
  if (!node) {
    semant_error(e) << "Cannot find the assigned name in the current scope: "
      << name << endl;
    e->set_type(Object);
    return;
  }

  // type matching
  Symbol expr_type = e->get_expr()->get_type();
  if (expr_type == SELF_TYPE) expr_type = current_class->get_name();
  Symbol decl_type = get_tree_node_type(node);
  if (decl_type == SELF_TYPE) decl_type = current_class->get_name();
  if (!class_table->check_child_class(decl_type, expr_type)) {
    semant_error(e) << "Assigned value's type " << decl_type
      << " doesn't match expression type " << expr_type << endl;
    e->set_type(Object);
    return;
  }

  e->set_type(expr_type); //TODO SELF_TYPE??
}

void TypeChecker::check(static_dispatch_class* e) {
  check(e->get_expr());
  
  Expressions actual = e->get_actual();
  for (int i = actual->first(); actual->more(i); i = actual->next(i)) {
    Expression_class* a = (Expression_class*) actual->nth(i);
    check(a);
  }

  // check the type of the call function is well defined
  Symbol type_name = e->get_type_name();
  Symbol expr_type = e->get_expr()->get_type();

  if (type_name == SELF_TYPE) {
    semant_error(e) << "static type T cannot be SELF_TYPE" << endl;
    e->set_type(Object);
    return;
  }

  if (!class_table->check_class_exists(type_name)) {
    semant_error(e) << "static dispatch class doesn't exist." << endl;
    e->set_type(Object);
    return;
  }
  if (!class_table->has_method(type_name, e->get_name())) {
    semant_error(e) << "dispatch method doesn't exist for the class. " << endl;
    e->set_type(Object);
    return;
  }
  if (!class_table->check_child_class(type_name, expr_type)) {
    semant_error(e) << "the caller expression's type doesn't match static type" << endl;
    e->set_type(Object);
    return;
  }

  // check actuals are well typed
  method_class* method = class_table->get_method(type_name, e->get_name());
  check(method);
  if (!check_actuals(e, method, e->get_actual())) {
    e->set_type(Object);
    return;
  }

  // set return type to tree node type
  Symbol return_type = method->get_return_type();
  if (return_type == SELF_TYPE) {
    return_type = expr_type;
  }
  e->set_type(return_type);
}

bool TypeChecker::check_actuals(
  Expression e, 
  method_class* method, 
  Expressions actuals
) {
  Formals formals = method->get_formals();
  if (formals->len() != actuals->len()) {
    semant_error(e) << "The number of caller actuals doesn't match method definition. "
      <<endl;
    return false;
  }
  for (int i = formals->first(); formals->more(i); i = formals->next(i)) {
    formal_class* f = (formal_class*) formals->nth(i);
    Expression a = actuals->nth(i);
    
    Symbol formal_type = f->get_type_decl();
    Symbol actual_type = a->get_type();
    if (formal_type == SELF_TYPE) {
      semant_error(e) << "formal cannot be SELF_TYPE!" << endl;
    }
    if (actual_type == SELF_TYPE) {
      semant_error(e) << "actual cannot be SELF_TYPE!" << endl;
      }
    if (!class_table->check_child_class(formal_type, actual_type)) {
      semant_error(e) << "declaration type " << formal_type
        << " doesn't match actual type " << actual_type
        << " in method " << method->get_name() << "." << endl;
      return false;
    }
  }
  return true;
}

void TypeChecker::check(dispatch_class* e) {
  check(e->get_expr());
  
  Expressions actual = e->get_actual();
  for (int i = actual->first(); actual->more(i); i = actual->next(i)) {
    Expression_class* a = (Expression_class*) actual->nth(i);
    check(a);
  }
  
  // check the type of the call function is well defined
  Symbol expr_type = e->get_expr()->get_type();
  
  if (expr_type == SELF_TYPE) expr_type =  current_class->get_name();

  if (!class_table->has_method(expr_type, e->get_name())) {
    semant_error(e) << "dispatch method doesn't exist for the class. " << endl;
    e->set_type(Object);
    return;
  }

  // check actuals are well typed
  method_class* method = class_table->get_method(expr_type, e->get_name());
  check(method);
  if (!check_actuals(e, method, e->get_actual())) {
    e->set_type(Object);
    return;
  }

  // set return type to tree node type
  Symbol return_type = method->get_return_type();
  if (return_type == SELF_TYPE) {
    return_type = expr_type;
  }
  e->set_type(return_type);
}

void TypeChecker::check(cond_class* e) {
  Expression pred = e->get_pred();
  Expression then_exp = e->get_then_exp();
  Expression else_exp = e->get_else_exp();
  
  check(pred);
  check(then_exp);
  check(else_exp);
  
  if (pred->get_type() == Bool){
    e->set_type(
      class_table->least_upper_bound(
        then_exp->get_type(), 
        else_exp->get_type(), 
        current_class->get_name()
      )
    );
  } else {
    semant_error(e) << "cond pred is not Bool!" << endl;
    e->set_type(Object);
  }
}

void TypeChecker::check(loop_class* e) {
  check(e->get_pred());
  check(e->get_body());
  if (e->get_pred()->get_type()!= Bool){
    semant_error(e) << "Loop pred is not Bool" << endl;
  }
  e->set_type(Object);
}

void TypeChecker::check(typcase_class* e) {
  check(e->get_expr());
  Cases cases = e->get_cases();
  if (cases->len() == 0) {
    e->set_type(Object);
    return;
  }

  std::set<Symbol> class_set;
  Symbol type = NULL;
  for (int i = cases->first(); cases->more(i); i = cases->next(i)) {
    branch_class* b = (branch_class*) cases->nth(i);
    check(b);

    if (class_set.count(b->get_type_decl())) {
      semant_error(e) << "Branch classes " << b->get_type_decl()
        << " have duplicates." << endl;
      return;
    } else {
      class_set.insert(b->get_type_decl());
    }

    Symbol current_type = b->get_expr()->get_type();
    type = class_table->least_upper_bound(type, current_type, current_class->get_name());
  }

  e->set_type(type);
}

void TypeChecker::check(block_class* e) {
  Expressions body = e->get_body();
  Symbol type = NULL;

  for (int i = body->first(); body->more(i); i = body->next(i) ){
    Expression_class* ex = (Expression_class*) body->nth(i);
    check(ex);
    type = ex->get_type();
  }

  e->set_type(type);
}

void TypeChecker::check(let_class* e) {
  Symbol type_decl = e->get_type_decl();
  Symbol identifier = e->get_identifier();
  Expression body = e->get_body();
  Expression init = e->get_init();

  if (type_decl == SELF_TYPE) type_decl = current_class->get_name();
  if (identifier == self){
    semant_error(e) << "let identifier cannot be self" << endl;
  }
  
  enterscope();
  addid(identifier, e);
  check(body);
  exitscope();

  if (typeid(*init) != typeid(no_expr_class)) {
    check(init);
    if (class_table->check_child_class(type_decl, init->get_type())){
      e->set_type(body->get_type());
    } else {
      semant_error(e) 
        << "let init and declared type are not compatible!" << endl;
      e->set_type(Object);
    }
  }
}


void TypeChecker::arithmetic_check(
  Expression e,
  Expression e1,
  Expression e2
) {
  check(e1);
  check(e2);
  if (e1->get_type() == Int && e2->get_type() == Int){
    e->set_type(Int);
  }else{
    semant_error(e) << "Wrong types are used for arithmetic operation" << endl;
  }
}

void TypeChecker::check(plus_class* e) {
  arithmetic_check(e, e->get_e1(), e->get_e2());
}

void TypeChecker::check(sub_class* e) {
  arithmetic_check(e, e->get_e1(), e->get_e2());
}

void TypeChecker::check(mul_class* e) {
  arithmetic_check(e, e->get_e1(), e->get_e2());
}

void TypeChecker::check(divide_class* e) {
  arithmetic_check(e, e->get_e1(), e->get_e2());
}

void TypeChecker::check(neg_class* e) {
  check(e->get_e1());
  if (e->get_e1()->get_type() == Int) {
    e->set_type(Int);
  } else {
    semant_error(e) << "Only integer can be negated." << endl;
  }
}

void TypeChecker::check(lt_class* e) {
  check(e->get_e1());
  check(e->get_e2());
  
  if (e->get_e1()->get_type() != Int || e->get_e2()->get_type() != Int) {
    semant_error(e) << "Cannot compare non-integers. " << endl;
    return;
  }

  e->set_type(Bool);
}

void TypeChecker::check(eq_class* e) {
  check(e->get_e1());
  check(e->get_e2());
  Symbol type1 = e->get_e1()->get_type();
  Symbol type2 = e->get_e2()->get_type();
  bool isBasic = type1 == Int || type1 == Str || type1 == Bool;
  if (type1 == type2 && isBasic){
    e->set_type(Bool);
  }else{
    semant_error(e) << "Eq subexpression type is not correct" << endl;
  }
}

void TypeChecker::check(leq_class* e) {
  check(e->get_e1());
  check(e->get_e2());
  Symbol type1 = e->get_e1()->get_type();
  Symbol type2 = e->get_e2()->get_type();
  if (type1 == Int && type2 == Int){
    e->set_type(Bool);
  } else {
    semant_error(e) << "leq subexpression type is not Int" << endl;
  }
}

void TypeChecker::check(int_const_class* e) {
  e->set_type(Int);
}

void TypeChecker::check(bool_const_class* e) {
  e->set_type(Bool);
}

void TypeChecker::check(string_const_class* e) {
 e->set_type(Str);
}


