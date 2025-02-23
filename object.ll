@str.0 = constant [3 x i8] c"%s\00"
@str.1 = constant [4 x i8] c"%d\0A\00"
@str.2 = constant [10 x i8] c"%1023[^\0A]\00"
@str.3 = constant [4 x i8] c"%*c\00"
@str.4 = constant [3 x i8] c"%d\00"
@io_instance = global { i8* } zeroinitializer
@str.6 = constant [16 x i8] c"I am an animal\0A\00"
@str.7 = constant [12 x i8] c"I am a Dog\0A\00"
@str.8 = constant [14 x i8] c"Hello World!\0A\00"
@str.9 = constant [8 x i8] c"3 < 2: \00"
@str.10 = constant [5 x i8] c"True\00"
@str.11 = constant [6 x i8] c"False\00"
@str.12 = constant [9 x i8] c"\0A4 < 5: \00"
@str.13 = constant [5 x i8] c"True\00"
@str.14 = constant [6 x i8] c"False\00"
@str.15 = constant [2 x i8] c"\0A\00"
@str.16 = constant [4 x i8] c"Dog\00"
@str.17 = constant [11 x i8] c"\0AI am here\00"
@main_instance = global { i8* } zeroinitializer

declare i8* @malloc(i32 %size)

declare external ccc void @exit(i32 %status)

define { i8* }* @Object_abort({ i8* }* %self) {
0:
	tail call void @exit(i32 1)
	unreachable
}

define i8* @Object_type_name({ i8* }* %self) {
0:
	%1 = getelementptr { i8* }, { i8* }* %self, i32 0, i32 0
	%2 = load i8*, i8** %1
	ret i8* %2
}

define { i8* }* @Object_copy({ i8* }* %self) {
0:
	%1 = call i8* @malloc(i32 8)
	%2 = bitcast { i8* }* %self to i8*
	%3 = bitcast i8* %1 to i8*
	%4 = call i8* @memcpy(i8* %3, i8* %2, i32 8)
	%5 = bitcast i8* %1 to { i8* }*
	ret { i8* }* %5
}

declare i8* @memcpy(i8* %dest, i8* %src, i32 %size)

define { i8*, i32 }* @Int_init({ i8*, i32 }* %self) {
0:
	%1 = getelementptr { i8*, i32 }, { i8*, i32 }* %self, i32 0, i32 1
	store i32 0, i32* %1
	ret { i8*, i32 }* %self
}

declare i32 @printf(i8* %format, ...)

declare i32 @scanf(i8* %format, ...)

declare i32 @strlen(i8* %str)

declare i8* @strcpy(i8* %dest, i8* %src)

define { i8* }* @IO_out_string({ i8* }* %self, i8* %x) {
0:
	%1 = call i32 (i8*, ...) @printf([3 x i8]* @str.0, i8* %x)
	%2 = call i32 @fflush(i8* null)
	ret { i8* }* %self
}

declare i32 @fflush(i8* %stream)

define { i8* }* @IO_out_int({ i8* }* %self, i32 %x) {
0:
	%1 = call i32 (i8*, ...) @printf([4 x i8]* @str.1, i32 %x)
	ret { i8* }* %self
}

define i8* @IO_in_string({ i8* }* %self) {
0:
	%1 = call i8* @malloc(i32 1024)
	%2 = call i32 (i8*, ...) @scanf([10 x i8]* @str.2, i8* %1)
	%3 = call i32 (i8*, ...) @scanf([4 x i8]* @str.3)
	ret i8* %1
}

define i32 @IO_in_int({ i8* }* %self) {
0:
	%1 = alloca i32
	%2 = call i32 (i8*, ...) @scanf([3 x i8]* @str.4, i32* %1)
	%3 = load i32, i32* %1
	ret i32 %3
}

define i8* @Animal_say_hello({ i8* }* %self) {
0:
	%1 = call { i8* }* @IO_out_string({ i8* }* @io_instance, [16 x i8]* @str.6)
	ret { i8* }* %1
}

define i8* @Dog_say_hello({ i8* }* %self) {
0:
	%1 = call { i8* }* @IO_out_string({ i8* }* @io_instance, [12 x i8]* @str.7)
	ret { i8* }* %1
}

define { i8* }* @Main_hello({ i8* }* %self) {
0:
	%1 = call { i8* }* @IO_out_string({ i8* }* @io_instance, [14 x i8]* @str.8)
	ret { i8* }* %1
}

define { i8* }* @Main_main({ i8* }* %self) {
0:
	%1 = call { i8* }* @Main_hello({ i8* }* %self)
	%2 = call { i8* }* @IO_out_string({ i8* }* @io_instance, [8 x i8]* @str.9)
	%3 = icmp slt i32 3, 2
	br i1 %3, label %if.then.1, label %if.else.1

if.then.1:
	%4 = call { i8* }* @IO_out_string({ i8* }* @io_instance, [5 x i8]* @str.10)
	br label %if.merge.1

if.else.1:
	%5 = call { i8* }* @IO_out_string({ i8* }* @io_instance, [6 x i8]* @str.11)
	br label %if.merge.1

if.merge.1:
	%6 = phi { i8* }* [ %4, %if.then.1 ], [ %5, %if.else.1 ]
	%7 = call { i8* }* @IO_out_string({ i8* }* @io_instance, [9 x i8]* @str.12)
	%8 = icmp slt i32 4, 5
	br i1 %8, label %if.then.2, label %if.else.2

if.then.2:
	%9 = call { i8* }* @IO_out_string({ i8* }* @io_instance, [5 x i8]* @str.13)
	br label %if.merge.2

if.else.2:
	%10 = call { i8* }* @IO_out_string({ i8* }* @io_instance, [6 x i8]* @str.14)
	br label %if.merge.2

if.merge.2:
	%11 = phi { i8* }* [ %9, %if.then.2 ], [ %10, %if.else.2 ]
	%12 = call { i8* }* @IO_out_string({ i8* }* @io_instance, [2 x i8]* @str.15)
	%13 = alloca { i8* }*
	%14 = call i8* @malloc(i32 8)
	%15 = bitcast i8* %14 to { i8* }*
	%16 = getelementptr { i8* }, { i8* }* %15, i32 0, i32 0
	%17 = bitcast [4 x i8]* @str.16 to i8*
	store i8* %17, i8** %16
	store { i8* }* %15, { i8* }** %13
	%18 = load { i8* }*, { i8* }** %13
	%19 = call i8* @Object_type_name({ i8* }* %18)
	%20 = call { i8* }* @IO_out_string({ i8* }* @io_instance, i8* %19)
	%21 = call { i8* }* @IO_out_string({ i8* }* @io_instance, [11 x i8]* @str.17)
	ret { i8* }* %21
}

define i32 @main() {
0:
	%1 = call { i8* }* @Main_main({ i8* }* @main_instance)
	ret i32 0
}
