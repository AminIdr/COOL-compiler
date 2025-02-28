# COOL Compiler Implementation

> **⚠️ Important Note**: This project has been tested and verified on Windows machines. Some colleagues have reported issues when running on macOS or Linux systems. While the core functionality should work, there might be platform-specific adjustments needed for non-Windows environments.

## Introduction

This project implements a compiler for COOL (Classroom Object-Oriented Language), developed as part of a compiler construction course. The compiler translates COOL source code into LLVM IR, which can then be compiled to native machine code.

## Project Overview

COOL is a small but powerful object-oriented language designed for teaching compiler construction. Key features include:
- Strong static typing
- Class-based inheritance
- Automatic memory management
- Built-in support for basic I/O operations
- Support for modular programming through class imports

## Project Architecture

### 1. Module System (`/importer`)
- Handles modular programming support
- Resolves class dependencies and imports
- Prevents circular dependencies
- Maintains module hierarchy


### 2. Lexical Analysis (`/lexer`)
- Handles COOL lexical specifications:
  - Keywords (`class`, `if`, `then`, `else`, `fi`, etc.)
  - Identifiers
  - Integer/String literals
  - Operators and special symbols
  - ...
- Error reporting for invalid tokens

### 3. Parser (`/parser`)
- Builds Abstract Syntax Tree (AST)
- Handles COOL grammar constructs:
  - Class definitions
  - Method declarations
  - Expressions
  - Control structures
  - ...
- Provides detailed syntax error messages

### 4. Semantic Analysis (`/semant`)
- Type checking and inference
- Class hierarchy validation
- Method override checking
- Scope analysis
- Name resolution
- Features:
  - Method dispatch resolution
  - Attribute initialization checking

### 5. Code Generation (`/codegen`)
- Generates LLVM IR (Intermediate Representation)

## Examples
### Example 1
Input COOL code (`attribute.cl`): This program tests class attribute handling.
```cool
class Main inherits IO {
    age:Int <- 18;

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
```
Output:
```cool
18
21
########
21
```
## Example 2

Input COOL code (`call.cl`):
This program makes a method call which returns an Int and adds a number to its return value.
```cool
class Main inherits IO {
   cal() : Int {
    3 + 5
   };

   main() : Object {
      {
         out_int(cal() + 5);
      }
   };
};
```
Output:
```cool
13
```

## Example 3

Input COOL code (`case.cl`):
This program tests the functionality of case expression.
```cool
class Animal inherits IO{
    say_hello() : String {
        {
            out_string("I am an animal\n");
        }
    };
};

class Main inherits IO{
  main(): Object {
    {
      let x:Int <- 5 in {
        case x of
          a : String => out_string("One\n");
          b : Animal => out_string("Two\n");
          c : Int => out_string("Three\n");
        esac;
        out_string("Inside\n");
      };
      out_string("Outside");
    }
  };
};
```
Output:
```cool
Three
Inside
Outside
```


## Example 4

Input COOL code (`facto.cl`):
This program runs a recursive factorial. The compiler handles recursive calls successfully.
```cool
class Main inherits IO{
    factorial(n: Int): Int {
        if (n = 0) then 
            1 
        else 
            n * factorial(n - 1) 
        fi
    };
    
    main(): Object {
        let num: Int <- 5 in
            out_int(factorial(num))
    };
};
```
Output:
```cool
5
```




## Example 5

Input COOL code (`isprime.cl`):
This program checks prime numbers smaller than a given number.
```cool
class Main inherits IO {
   findPrimes(max : Int) : Object {
      let i : Int <- 2,
          j : Int <- 2,
          isPrime : Bool in
      {
         while i <= max loop
         {
            isPrime <- true;
            j <- 2;
            
            while j * j <= i loop
            {
               if i - (i/j)*j = 0 then
               {
                  isPrime <- false;
                  j <- i;  (* Break inner loop *)
               }
               else
                  j <- j + 1
               fi;
            } pool;
            
            if isPrime then
            {
               out_int(i);
               out_string(" ");
            }
            else 0
            fi;
            
            i <- i + 1;
         } pool;
         out_string("\n");
      }
   };

   main() : Object {
      {
         out_string("Prime numbers up to 30:\n");
         findPrimes(30);
      }
   };
};
```
Output:
```cool
Prime numbers up to 30:
 2
 3
 5
 7
 11
 13
 17
 19
 23
 29
 
```


## Example 6

Input COOL code (`multiwhile.cl`):
This program tests nested while loops.
```cool
class Main inherits IO{
    a:Int;
    main(): Object{
        {
            a <- 0;
            while a < 10 loop{
                a <- a + 1;
                while (a < 8) loop {out_int(a); a <- a+2;} pool;
            } pool;
            out_string("End");
        }
    };
};
```
Output:
```cool
1
3
5
7
End
```




## Example 7

Input COOL code (`OOP.cl`):
This program demonstrates how OOP is handled through inheritance and polymorphism in the project. It also demonstrates dynamic dispatch handling.
```cool
class Animal {
    say_hello() : String {
        {
            out_string("I am an animal\n");
        }
    };
    custom_animal(): String {
      {
           out_string("Method of Animal\n");
      }
    };
};

class Dog inherits Animal {
  say_hello(): String{
    {
      out_string("I am a dog\n");
    }
  };
  custom_dog(): String{
    {
      out_string("Method of Dog\n");
    }
  };
};


class Dobermann inherits Dog {
    say_hello() : String {
        {
            out_string("I am a Dobermann\n");
        }
    };
      custom_dobermann(): String{
    {
      out_string("Method of Dobermann\n");
    }
  };
};
class Main {
  hello(): Object {
    out_string("Hello World!\n")
  };

  main() : Object { 
    {
      let y:Animal <- new Animal in {y.say_hello();y.custom_animal();out_string(y.type_name());out_string("\n");};
      let x:Dog <- new Dog in {x.say_hello();x.custom_dog();out_string(x.type_name());out_string("\n");};
      let z:Dobermann <- new Dobermann in {z.say_hello();z.custom_dobermann();out_string(z.type_name());};
    }
  };
};

```
Output:
```cool
I am an animal
Method of Animal
Animal
I am a dog
Method of Dog
Dog
I am a Dobermann
Method of Dobermann
Dobermann
```



## Example 8

Input COOL code (`staticdispatch.cl`):
This program demonstrates how static dispatch is handled using @. 
```cool
class Animal {
    say_hello() : String {
        {
            out_string("I am an Animal\n");
        }
    };
};

class Dog inherits Animal {
    say_hello() : String {
        {
            out_string("I am a Dog\n");
        }
    };
};

class Dobermann inherits Dog {
    say_hello() : String {
        {
            out_string("I am a Dobermann\n");
        }
    };
};

class Main {
    main(): Object {
        {
        let z:Dobermann <- new Dobermann in {
            z.say_hello();
            z@Dog.say_hello();
            z@Animal.say_hello();
        };
        
        }
    };
};
```
Output:
```cool
I am a Dobermann
I am a Dog
I am an Animal
```



## Example 9

Input COOL code (`string.cl`):
This program demonstrates how methods of String class are implemented.
```cool


class Main inherits IO{
  main() : Object { 
    {
      out_string("Enter your string : ");
      let x : String <- in_string() in {out_string("The string \"");
      out_string(x);
      out_string("\" is of length : ");
      out_int(x.length());
      out_string("Its type is : ");
      out_string(x.type_name());
      out_string("\n");
      out_string(x.substr(2, 4));
      };
    }
  };  
};
```
Output:
```cool
Enter your string : Amine
The string "Amine" is of length : 5
Its type is : String
ine
```

# Linked List Extension
COOL has been extended with a linked list implementation that can be imported using the module system. It can be found in
```cool
./cool/MyLinkedList.cl
```.
It then can be imported directly in another file.
# Module System
## Module
```cool
module MyModule;
class MyClass {
    hello() :  Object{
        out_string("hello")
    };
};
```

## Import
```cool
import MyModule;
class Main {
    main() : Object {
        let obj : MyClass <- new MyClass in {
            obj.hello();
        }
    };
};
```
Output:
```cool
hello
```
The hello method is from the imported module containing the class MyClass.


### Running the Compiler (in Windows)
```bash
go run main.go -i ./cool/*file_name*.cl -o ./cool/*file_name*.ll
```
This will generate:
- `input.ll` (LLVM IR)
Then 
```bash
clang -o ./cool/*file_name*.exe ./cool/*file_name*.ll 
```
This will generate:
- `input.exe` (Executable if compilation succeeds)

Finally, run the executable:
```bash
./cool/*file_name*.exe
```
