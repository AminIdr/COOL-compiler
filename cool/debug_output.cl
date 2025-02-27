class Animal {
    say_hello() : String {
        {
            out_string("I am an animal\n");
        }
    };
};

class Main {
  main(): Object {
    {
      let x:Int <- 5 in {
        case x of
          a : String => out_string("One\n");
          b : Animal => out_string("Two\n");
          c : Int => out_string("Three\n");
        esac;
        case x of
          a : String => out_string("One\n");
          b : Animal => out_string("Two\n");
          c : Int => out_string("Three\n");
        esac;
        out_string("Inside\n");
      };
      out_string("Outside");
    }
  };
};