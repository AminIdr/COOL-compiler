class Main {
  hello(): Object {
    out_string("Hello World!\n")
  };
  main() : Object { 
    {
      --hello();
      hello();
      out_string("3 < 2: ");
      if 3 < 2 then out_string("True") else out_string("False") fi;
      out_string("\n4 < 5: ");
      if 4 < 5 then out_string("True") else out_string("False") fi;
      out_string("\nI am here");
    }
  };
};