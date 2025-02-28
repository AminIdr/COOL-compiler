class Animal inherits IO{
    say_hello() : String {
        {
            out_string("I am an animal\n");
        }
    };
    custom_animal(): String {
      {
           out_string("Method of Animal\n");
      }
    };
};

class Dog inherits Animal {
  say_hello(): String{
    {
      out_string("I am a dog\n");
    }
  };
  custom_dog(): String{
    {
      out_string("Method of Dog\n");
    }
  };
};


class Dobermann inherits Dog {
    say_hello() : String {
        {
            out_string("I am a Dobermann\n");
        }
    };
      custom_dobermann(): String{
    {
      out_string("Method of Dobermann\n");
    }
  };
};
class Main inherits IO{
  hello(): Object {
    out_string("Hello World!\n")
  };

  main() : Object { 
    {
      let y:Animal <- new Animal in {y.say_hello();y.custom_animal();out_string(y.type_name());out_string("\n");};
      let x:Dog <- new Dog in {x.say_hello();x.custom_dog();out_string(x.type_name());out_string("\n");};
      let z:Dobermann <- new Dobermann in {z.say_hello();z.custom_dobermann();out_string(z.type_name());};
    }
  };
};
