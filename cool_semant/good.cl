class C inherits IO {
	b : Bool;
	init(x : Int, y : Bool) : C {
           {
		a <- x;
		b <- y;
		self;
           }
	};
	a : Int;
  h() : Object {
    b <- true
  };
  g(x : Int) : Int {
    {
      x + 1 * x;
    }
  };
};

Class D inherits C {
  f() : Object {
{
    out_string("test print");
    self@C.h();
    case a of
    y : Int => a*a;
    z : IO => a + a;
    esac;
    let x : Int<-2 in {
      1*x;
    };
		if (true = false) then h() else 0 fi;
		if 1 = 1 then h() else 0 fi;
		if "a" = "a" then h() else 0 fi;
		if "a" = (new C) then h() else 0 fi;
    g(2) + g(1);
}
  };
  h() : Object {
{
    b <- false;
    while b loop {
      a <- a + 1;
      b <- false;
    } pool;
}
  };
};

Class Main {
	main():C {
	  (new C).init(1,true)
	};
};
