class Main inherits IO{
    age:Int <- 18;

    fct(): SELF_TYPE {
        out_int(age)
    };

    main() : SELF_TYPE {
        {
        out_int(age);
        age <- age + 3;
        out_int(age);
        out_string("########\n");
        fct();
        }
    };


};