class Main {
    age:Int <- 6;

    fct(): Object {
        out_int(age)
    };

    main() : Object{
        {
        out_int(age);
        age <- age + 3;
        out_int(age);
        out_string("########\n");
        fct();
        }
    };


};