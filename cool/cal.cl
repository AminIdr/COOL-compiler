class Main inherits IO {
   cal() : Int {
    3 + 5
   };

   main() : Object {
      {
         out_int(cal() + 5);
      }
   };
};