class Animal {
    a : Int;
    init(b : Int) : Animal {
        {
            a <- b;
            self;
        }
    };
    say_hello() : Object {
        {
            out_int(a);
            a <- 5;
            out_int(a);
            a <- 6;
            out_int(a);
            out_string("\nI am an animal\n");
        }
    };
};

class Main {
    x : Animal;
  hello(): Object {
    out_string("Hello World!\n")
  };

  main() : Object { 
    {
      
            x <- (new Animal).init(4);
            x.say_hello();
    out_string(x.type_name());
      
    }
  };
};

