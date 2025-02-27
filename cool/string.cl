class Main {
  hello(): Object {
    out_string("Hello World!\n")
  };
  main() : Object { 
    {
      let x : String <- "Hello" in {out_string("The string \"");
      out_string(x);
      out_string("\" is of length : ");
      out_int(x.length());
      };
    }
  };
};