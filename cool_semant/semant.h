#ifndef SEMANT_H_
#define SEMANT_H_

#include <assert.h>
#include <iostream>  
#include "cool-tree.h"
#include "stringtab.h"
#include "symtab.h"
#include "list.h"

#include <map>
#include <set>
#include <queue>

#define TRUE 1
#define FALSE 0

class ClassTable;
typedef ClassTable *ClassTableP;

// This is a structure that may be used to contain the semantic
// information such as the inheritance graph.  You may use it or not as
// you like: it is only here to provide a container for the supplied
// methods.

class ClassTable {
private:
  int semant_errors;
  bool cycle_found;

  ostream& error_stream;
  std::map<Symbol, std::set<Symbol> > inheritance_graph; 
  std::map<Symbol, class__class*> class_info;
  std::map<Symbol, std::map<Symbol, method_class*> > class_method_map;
  std::map<Symbol, std::map<Symbol, attr_class*> > class_attr_map;

  void install_basic_classes();
  void install_program_classes(Classes classes);
  void check_cycle();
  void DFS(std::set<Symbol> visited, Symbol c);
  void install_class_features();
  void abort();
  void verify_method_formals(class__class*, method_class*);

public:
  ClassTable(Classes);
  int errors() { return semant_errors; }
  ostream& semant_error();
  ostream& semant_error(Class_ c);
  ostream& semant_error(Symbol filename, tree_node *t);
  bool check_child_class(Symbol parent, Symbol child);
  bool check_class_exists(Symbol c);
};

class TypeChecker {
private:
  SymbolTable<Symbol, tree_node> symbol_table;
  ClassTable* class_table;
  class__class* current_class;

  tree_node* probe(Symbol a) { return symbol_table.probe(a); }
  tree_node* lookup(Symbol a) { return symbol_table.lookup(a); }
  void addid(Symbol a, tree_node* n) { symbol_table.addid(a, n); } 
  void enterscope() { symbol_table.enterscope(); }
  void exitscope() { symbol_table.exitscope(); }
  ostream& semant_error(tree_node *t);

public:
  TypeChecker(ClassTable* c) : class_table(c), current_class(NULL) {};

  void check(program_class*);
  void check(class__class*);
  void check(attr_class*);
  void check(method_class*);
  void check(formal_class*);

  void check(Expression_class*);
  void check(isvoid_class*);
  void check(new__class*);
  void check(no_expr_class*);
  void check(object_class*);
  void check(comp_class*);

  void check(branch_class*);
  void check(assign_class*);
  void check(static_dispatch_class*);
  void check(dispatch_class*);
  void check(cond_class*);
  void check(loop_class*);
  void check(typcase_class*);
  void check(block_class*);
  void check(let_class*);

  void check(plus_class*);
  void check(sub_class*);
  void check(mul_class*);
  void check(divide_class*);
  void check(neg_class*);

  void check(lt_class*);
  void check(eq_class*);
  void check(leq_class*);

  void check(int_const_class*);
  void check(bool_const_class*);
  void check(string_const_class*);

};

#endif

