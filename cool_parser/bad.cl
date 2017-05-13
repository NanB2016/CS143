
(*
 *  execute "coolc bad.cl" to see the error messages that the coolc parser
 *  generates
 *
 *  execute "myparser bad.cl" to see the error messages that your parser
 *  generates
 *)

(* no error *)
class A {
};

(* error:  b is not a type identifier *)
Class b inherits A {
};

(* error:  a is not a type identifier *)
Class C inherits a {
};

(* error:  keyword inherits is misspelled *)
Class D inherts A {
};

(* error:  closing brace is missing *)
Class E inherits A {
;

class A Inherits IO {
ana(): int {
(let x:Int <- 1 in 2)+3
};

result:Int <- 2+3;

newMethod() : Int{

 {
out_string("print current result");
  
out_int(result)

result <-2;
out_string("print new result");
out_int(result);
}
}

};

class BB__ inherits A {

};

class Silly {
  copy() : SELF_TYPE { self };
};

class Sally inherits Silly { };

class Main {
  x: Sally <- (new Sally).copy();
  
  main(): Sally { x };
}

class BB__ inherits A {

(* test assignment *)
a: Int <- 1;
b: Int <- a*a+a;
c: Bool <-b=a;
d: Bool <- false
d: String <- 'OK!';

bMethod(a): Int{

{
if d then out_int(a) else out_int(b) fi;
while d loop b <- b+1 pool;

let x int<-a/b, y:Int<-a+b, z:Bool<-a<=b in if z then out_int(a) else out_int(b) fi; 
}
};




