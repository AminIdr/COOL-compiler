class Main {
    a:Int;
    main(): Object{
        {
            a <- 0;
            while a < 10 loop{
                a <- a + 1;
                while (a < 8) loop {out_int(a); a <- a+2;} pool;
            } pool;
            out_string("ererere");
        }
    };
};