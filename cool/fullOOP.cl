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

            let myDog : Dog <- (new Dog).init("Buddy") in {
                out_string("myDog's name is ");
                myDog.setName("Max");
                out_string("\n");
                let dog2 : Dog <- myDog.copy() in {
                    out_string(dog2.getName());
                };
                out_string("\n");
                out_string("myDog's breed is ");
                out_string(myDog.getBreed());
                out_string("\n");  

            };
            b <- 5;
            while (b < 7) loop
                b <- b + 1
            pool;
            out_string("b is ");
            out_string(b.type_name());
            out_string(" and its value is ");
            out_int(b);
            c <- "Imane";
            f<- " Fjer";
            out_string(c.concat(f));
            c <- c.substr(2, 3);

            if (b = 10) then
                out_string("b is indeed 10")
            else
            {
out_string("b is not 10");
                out_int(c.length());
            }
            fi;
                        case b of
                dog : Dog => out_string("b is a dog");
                animal : Animal => out_string("b is an animal");
                int : Int  => out_string("b is an andsimal");
                object: Object => out_string("b is unknown");
            esac;
            if true then out_string("b is unknown")  else out_string("b is an animal")fi ;
            
        }
    };
};