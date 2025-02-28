class Main {
    age:Int <- 6;
    main() : Object{
        {
        out_int(age);
        age <- age + 1;
        out_int(age);
        }
    };
};