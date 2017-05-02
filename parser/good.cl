class A Inherits IO {
ana(): Int {
(let x:Int<- 1 in 2)+3
};

result:Int <- 2+3;

newMethod() : Void{

 {
out_string("print current result");
  out_int(result);
result <-2;
out_string("print new result");
out_int(result);
}
};

};

class BB__ inherits A {

(* test assignment *)
a: Int <- 1;
b: Int <- a*a+a;
c: Bool <-b=a;
d: Bool <- false;
d: String <- "OK!";

bMethod(a:Int): Int{

{
if d then out_int(a) else out_int(b) fi;
while d loop b <- b+1 pool;

let x: Int<-a/b, y:Int<-a+b, z:Bool<-a<=b in if z then out_int(a) else out_int(b) fi; 


}
};
}
;
class Silly {
  copy() : SELF_TYPE { self };
};

class Sally inherits Silly { };

class Main {
  x: Sally <- (new Sally).copy();
  
  main(): Sally { x };
};
