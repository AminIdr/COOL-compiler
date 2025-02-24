
class Node {
    data : Object;
    next : Node;

    init(d : Object, n : Node) : Node {
        {
            data <- d;
            next <- n;
            self;
        }
    };

    data() : Object { data };

    next() : Node { next };
};

class List {
    first : Node;
    size : Int <- 0;

    init() : List {
        {
            first <- void;
            self;
        }
    };

    isEmpty() : Bool {
        first = void
    };

    cons(data : Object) : List {
        let new_node : Node <- (new Node).init(data, first) in {
            first <- new_node;
            size <- size + 1;
            self;
        }
    };

    car() : Object {
        if isEmpty() then
            abort()
        else 
            first.data()
        fi
    };

    cdr() : List {
        if isEmpty() then
            abort() 
        else {
            first <- first.next();
            size <- size - 1;
            self;
        }
        fi
    };

    length() : Int { size };

    append(other : List) : List {
        if isEmpty() then
            other
        else 
            if other.isEmpty() then
                self
            else {
                cons(other.car());
                append(other.cdr());
            }
            fi
        fi
    };
}
