-- test whether cycle exists
-- test whether main exists
-- test class being redefined
-- test parent class does not exist
-- test inherits from basic classes
-- test attributes being redefined
-- test return type of method
-- test formal length of method
-- test formal type of method
-- test main method exists in main class


Class AA{
  a: Int<-0;
  b: Bool;

  c(c: Int): Int{
  {
  a <- c+1;
  }
  };
  
  d(c: Int, self: SELF_TYPE): Bool{
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

e():SELF_TYPE{
{
self;
}
};

e(): EEE{
{
true;
}
};
};


