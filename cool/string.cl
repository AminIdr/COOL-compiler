class Main {
  cal() : Int {
    {
     out_string("aa"); 
    }
  };
  hello(): Object {
    out_string("Hello World!\n")
  };
  main() : Object { 
    {
      let x : String <- "Hello" in {out_string("\nThe string \"");
      out_string(x);
      out_string("\" is of length : ");
      out_int(x.length());
      out_string(" and of type :");
      out_string(x.type_name());
      out_string("\n");
      };

      let y : Int <- 5 in {out_string(y.type_name());};
    }
  };
};