module MyLinkedList;
class Node {
    value : Int;
    next  : Node;

    get() : Int {
        value
    };

    set(val : Int) : Node {
        {
            value <- val;
            self;
        }
    };

    getNext() : Node {
        next
    };

    setNext(n : Node) : Node {
        {
            next <- n;
            self;
        }
    };

    initNode() : Node {
        {
            value <- 0;
            next <- self;
            self;
        }
    };
};

class LinkedList inherits IO {
    head : Node;
    initLinkedList() : LinkedList {
        {
            head <- new Node;
            head.initNode();
            self;
        }
    };

    insert(val : Int) : LinkedList {
        let newNode : Node <- new Node in {
            newNode.set(val);
            newNode.setNext(head.getNext());
            head.setNext(newNode);
            self;
        }
    };

    print() : LinkedList {
        let tmpNode : Node <- head.getNext() in {
            while not (tmpNode = head) loop {
                out_int(tmpNode.get());
                out_string("###\n");
                tmpNode <- tmpNode.getNext();
            } pool;
            self;
        }
    };

    search(val : Int) : Bool {
        let tmpNode : Node <- head.getNext() in {
            let found : Bool <- false in {
                while not (tmpNode = head) loop {
                    if tmpNode.get() = val then {
                        found <- true;
                        tmpNode <- head;
                    } else {
                        tmpNode <- tmpNode.getNext();
                    } fi;
                } pool;
                found;
            };
        }
    };
};

class Main inherits IO {
    main() : Object {
        let l : LinkedList <- new LinkedList in {
            l.initLinkedList();
            let i : Int <- 1 in {   
                while i <= 10 loop {
                    l.insert(i);
                    i <- i + 1;
                } pool;
            };
            l.print();
            out_string("Enter an integer: ");
            let valueSearch : Int <- in_int() in {
                if l.search(valueSearch) then
                    out_string("The value is in the linked list.")
                else
                    out_string("The value is not in the linked list.")
                fi;
            };
            self;
        }
    };
};