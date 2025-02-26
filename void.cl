class Animal {
    a : Int;
    init() : Animal {
        {
            a <- 0;
            self;
        }
    };
    say_hello() : Object {
        {
            a <- 5;
            out_int(a);
            a <- 6;
            out_int(a);
            out_string("\nI am an animal\n");
        }
    };
};

class Main {
  hello(): Object {
    out_string("Hello World!\n")
  };

  main() : Object { 
    {
      
      out_string("isvoid new Animal");
      
    }
  };
};

