class Main {
    factorial(n: Int): Int {
        if (n = 0) then 
            1 
        else 
            n * factorial(n - 1) 
        fi
    };
    
    main(): Object {
        let num: Int <- 55 in
            out_int(factorial(num))
    };
};