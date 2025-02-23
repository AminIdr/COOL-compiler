@str.0 = constant [3 x i8] c"%s\00"
@str.1 = constant [4 x i8] c"%d\0A\00"
@str.2 = constant [10 x i8] c"%1023[^\0A]\00"
@str.3 = constant [4 x i8] c"%*c\00"
@str.4 = constant [3 x i8] c"%d\00"
@io_instance = global { i8* } zeroinitializer
@str.6 = constant [16 x i8] c"I am an animal\0A\00"
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

define i8* @Animal_say_hello({ i8* }* %self) {
0:
	%1 = call { i8* }* @IO_out_string({ i8* }* @io_instance, [16 x i8]* @str.6)
	ret { i8* }* %1
}

define i8* @Main_main({ i8* }* %self) {
0:
	%1 = alloca { i8* }*
	%2 = call i8* @malloc(i32 8)
	%3 = bitcast i8* %2 to { i8* }*
	%4 = getelementptr { i8* }, { i8* }* %3, i32 0, i32 0
	store i8* null, i8** %4
	store { i8* }* %3, { i8* }** %1
	%5 = load { i8* }*, { i8* }** %1
	%6 = call i8* @Animal_say_hello({ i8* }* %5)
	%7 = alloca { i8* }*
	%8 = call i8* @malloc(i32 8)
	%9 = bitcast i8* %8 to { i8* }*
	%10 = getelementptr { i8* }, { i8* }* %9, i32 0, i32 0
	store i8* null, i8** %10
	store { i8* }* %9, { i8* }** %7
	%11 = load { i8* }*, { i8* }** %7
	%12 = call i8* @Animal_say_hello({ i8* }* %11)
	%13 = alloca { i8* }*
	%14 = call i8* @malloc(i32 8)
	%15 = bitcast i8* %14 to { i8* }*
	%16 = getelementptr { i8* }, { i8* }* %15, i32 0, i32 0
	store i8* null, i8** %16
	store { i8* }* %15, { i8* }** %13
	%17 = load { i8* }*, { i8* }** %13
	%18 = call i8* @Animal_say_hello({ i8* }* %17)
	ret i8* %18
}

define i32 @main() {
0:
	%1 = call i8* @Main_main({ i8* }* @main_instance)
	ret i32 0
}
