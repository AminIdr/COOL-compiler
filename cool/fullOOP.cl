class Animal {
    name : String;

    init(n : String) : Animal {
        {
            name <- n;
            self;
        }
    };

    setName(n : String) : Animal {
        {
            name <- n;
            self;
        }
       
    };

    getName() : String {
        name
    };
    
    print() : Object {
        out_string("this is an animal \n")
    };
};

class Dog inherits Animal {
    breed : String;

    init(n : String) : Dog {
        {
            name <- n;
            self;
        }
    };

    getBreed() : String {
        breed     
    };

    print() : Object {
        out_string("this is a dog \n")
    };
};

class Main {
   a : Animal;
    b : Int;
    c : String;
    e : Bool;
    d : Int;
    f:String;

    sum (i : Int, j : Int, t: Int) : Int {
        i + j + t
    };

    main() : Object {
        {
        
            out_string(d.type_name());
            out_string("\n");
            d <- 10;

            let myDog : Dog <- new Dog in {
                out_string(myDog.getBreed());

            };
    }
};
};