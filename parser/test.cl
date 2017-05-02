class A Inherits IO {
result:Int;
ana(): Int {
(let x:Int <- 1 in 2)+3
};
mymethod() : Int {
        {
            out_string("Current Type is A");
            result <- 1;
        }
    };
};

Class BB__ inherits A {
    (*Test assignment*)
    a:Int <- 2;
    a1:Int <- 0;
    b:Bool <- fAlse;
    c:String <- "yes!";
    c2:String <- "no!";
    d:IO;
    e:BB;
    calc(): Int {
        {   (* arithmetic operation and *)
            a1 <- a + a;
            a1 <- a1 - a;
            a1 <- a * a;
            a1 <- a1 / a;
            b <- ~b;
            a1 <- a * a;
            (* if operation *)
            if (a1 < a) then out_string(c) else output_string(c2) fi;
            if (a1 <= a) then out_string(c) else output_string(c2) fi;
            if (a1 = a) then out_string(c) else output_string(c2) fi;
            (* while operation*)
            while (not a < 10) loop a <- a + 1 pool;
            
        }
    };
    mymethod() : Int {
        {
        out_string("Current Type is BB__");
        2;
        }
    };
};

class CC__ Inherits A {
(*test let and case*)
    result : Int;
    my_method() : Int {
        {
        out_string("Current Type is CC_");
        3;
        }
    };    
    test(var : A) : Int {
        {
            out_string("Hello").out_string("World").out_string("!"); -- dispatch  
            case var of                         
                bb : BB__ => out_string("Current type is BB__");
                cc : CC__ => out_string("Current type is CC__");
            esac;     -- case
            
        }
    };
    (*static dispatch and dynamic ones*)
    testtype(s : String) : Void{
        if s = "a" then  result <- (new CC__)@A.my_method() else
        if s = "b" then  result <- (new CC__)@BB__.my_method() else
        if s = "c" then  result <- (new CC__)@CC__.my_method() else (new CC__)@CC__.my_method() --default 
        fi fi fi
    };
    
    testlet(a : Int, b : Int) : A {
        (let x : Int in
       {
              x <- ~(a + b);
              x;
       }
        )
    };
};
