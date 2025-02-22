@str.0 = constant [3 x i8] c"%s\00"
@str.1 = constant [4 x i8] c"%d\0A\00"
@str.2 = constant [10 x i8] c"%1023[^\0A]\00"
@str.3 = constant [4 x i8] c"%*c\00"
@str.4 = constant [3 x i8] c"%d\00"
@io_instance = global { i8* } zeroinitializer
@str.6 = constant [16 x i8] c"Testing loops:\0A\00"
@str.7 = constant [4 x i8] c"i: \00"
@str.8 = constant [2 x i8] c"\0A\00"
@str.9 = constant [4 x i8] c"j: \00"
@str.10 = constant [2 x i8] c"\0A\00"
@main_instance = global { i8* } zeroinitializer

declare i32 @printf(i8* %format, ...)

declare i32 @scanf(i8* %format, ...)

declare i8* @malloc(i32 %size)

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

define i8* @Main_main({ i8* }* %self) {
0:
	%1 = call { i8* }* @IO_out_string({ i8* }* @io_instance, [16 x i8]* @str.6)
	%2 = alloca i32
	store i32 0, i32* %2
	br label %while.cond.1

while.cond.1:
	%3 = load i32, i32* %2
	%4 = icmp slt i32 %3, 10
	br i1 %4, label %while.body.1, label %while.after.1

while.body.1:
	%5 = call { i8* }* @IO_out_string({ i8* }* @io_instance, [4 x i8]* @str.7)
	%6 = load i32, i32* %2
	%7 = call { i8* }* @IO_out_int({ i8* }* @io_instance, i32 %6)
	%8 = call { i8* }* @IO_out_string({ i8* }* @io_instance, [2 x i8]* @str.8)
	%9 = load i32, i32* %2
	%10 = add i32 %9, 1
	store i32 %10, i32* %2
	br label %while.cond.1

while.after.1:
	%11 = alloca i32
	store i32 0, i32* %11
	br label %while.cond.2

while.cond.2:
	%12 = load i32, i32* %11
	%13 = icmp slt i32 %12, 10
	br i1 %13, label %while.body.2, label %while.after.2

while.body.2:
	%14 = call { i8* }* @IO_out_string({ i8* }* @io_instance, [4 x i8]* @str.9)
	%15 = load i32, i32* %11
	%16 = call { i8* }* @IO_out_int({ i8* }* @io_instance, i32 %15)
	%17 = call { i8* }* @IO_out_string({ i8* }* @io_instance, [2 x i8]* @str.10)
	%18 = load i32, i32* %11
	%19 = add i32 %18, 1
	store i32 %19, i32* %11
	br label %while.cond.2

while.after.2:
	ret i8* null
}

define i32 @main() {
0:
	%1 = call i8* @Main_main({ i8* }* @main_instance)
	ret i32 0
}
