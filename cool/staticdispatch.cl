class Animal inherits IO {
    say_hello() : SELF_TYPE {
        {
            out_string("I am an Animal\n");
        }
    };
};

class Dog inherits Animal {
    say_hello() : SELF_TYPE {
        {
            out_string("I am a Dog\n");
        }
    };
};

class Dobermann inherits Dog {
    say_hello() : SELF_TYPE {
        {
            out_string("I am a Dobermann\n");
        }
    };
};

class Main {
    main(): Object {
        {
        let z:Dobermann <- new Dobermann in {
            z.say_hello();
            z@Dog.say_hello();
            z@Animal.say_hello();
        };
        
        }
    };
};