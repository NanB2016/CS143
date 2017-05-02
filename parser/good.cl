class A {
ana(): Int {
(let x:Int <- 1 in 2)+3
};
};

class BB__ inherits A {
};

class Silly {
  copy() : SELF_TYPE { self };
};

class Sally inherits Silly { };

(*class Main {
  x: Sally <- (new Sally).copy();
  
  main(): Sally { x };

}*)


