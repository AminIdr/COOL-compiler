

class Main {
  main() : Object { 
    {
      out_string("Enter your string : ");
      let x : String <- in_string() in {out_string("The string \"");
      out_string(x);
      out_string("\" is of length : ");
      out_int(x.length());
      out_string("Its type is : ");
      out_string(x.type_name());
      out_string("\n");
      out_string(x.substr(2, 4));
      };
    }
  };  
};