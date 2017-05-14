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
};

class TypeChecker {
private:
  SymbolTable<Symbol, tree_node> symbol_table;
  ClassTable* class_table;
  class__class* current_class;

public:
  TypeChecker(ClassTable* c) : class_table(c), current_class(NULL) {};
  ostream& semant_error(tree_node *t);

  tree_node* probe(Symbol a) { return symbol_table.probe(a); }
  tree_node* lookup(Symbol a) { return symbol_table.lookup(a); }
  void addid(Symbol a, tree_node* n) { symbol_table.addid(a, n); } 
  void enterscope() { symbol_table.enterscope(); }
  void exitscope() { symbol_table.exitscope(); }

  void check(program_class*);
};

#endif

