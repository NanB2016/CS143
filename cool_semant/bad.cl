
class C {
	a : Int;
	b : Bool;
	init(x : Int, y : Bool) : C {
           {
		a <- x;
		b <- y;
		self;
           }
	};
};

Class Main {
	main():C {
	 {
	  (new C).init(1,1);
	  (new C).init(1,true,3);
	  (new C).iinit(1,true);
	  (new C);
	 }
	};
};

ClASS D inherits E{
};

class D {
};

Class AA{
  a: Int<-0;
  b: Bool;

  c(c: Int): Int{
  {
  a <- c+1;
  out_string("not defined");
  if 1 = true then 1 else 0 fi;
  }
  };
  
  d(c: Int): Bool{
  {
  a<=c;
  }
};
};

Class BB inherits AA{
  a: Bool;
  b: Int;
  
  c(): Bool{
  {
a;
  }
};

d(c: Bool): Bool{
{
c;
}
};

e(): EEE{
{
true;
}
};
};

-- detect cycle
-- comment out because semant will halt if there are cycles
-- class A inherits B{
-- };

-- class B inherits A{
-- };


