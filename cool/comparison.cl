class Main inherits IO{

main() : SELF_TYPE {
    let a : Int <- 3 in {
        let b : Int <- 4 in {
            if not (a = b) then out_string("true")
            else out_string("false") fi;
        };
    }
};
};