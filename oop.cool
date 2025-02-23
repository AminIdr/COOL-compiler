class Animal {
    say_hello() : String {
        {
            out_string("I am an animal\n");
        }
    };
};

class Dog inherits Animal {
    say_hello() : String {
        {
            out_string("I am a Dog\n");
        }
    };
};

class Dobermann inherits Dog {
    say_hello() : String {
        {
            out_string("I am a Dobermann\n");
        }
    };
};

class Main {
    main(): Object {
        {
        let x:Animal <- new Animal in {x.say_hello();};
        let y:Dog <- new Dog in {y.say_hello();};
        let z:Dobermann <- new Dobermann in {z.say_hello();};
        }
    };
};