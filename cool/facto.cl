class Main inherits IO{
    factorial(n: Int): Int {
        if (n = 0) then 
            1 
        else 
            n * factorial(n - 1) 
        fi
    };
    
    main(): SELF_TYPE {
        let num: Int <- 5 in
            out_int(factorial(num))
    };
};