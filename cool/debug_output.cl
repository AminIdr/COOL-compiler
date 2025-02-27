class MyClass {
    hello() :  Object{
        out_string("hello")
    };
};
class Main {
    main() : Object {
        let obj : MyClass <- new MyClass in {
            obj.hello();
        }
    };
};