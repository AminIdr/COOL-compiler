@str.0 = constant [3 x i8] c"%s\00"
@str.1 = constant [4 x i8] c"%d\0A\00"
@str.2 = constant [10 x i8] c"%1023[^\0A]\00"
@str.3 = constant [4 x i8] c"%*c\00"
@str.4 = constant [3 x i8] c"%d\00"
@io_instance = global { i8* } zeroinitializer
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

define i32 @Main_factorial({ i8* }* %self, i32 %n) {
0:
	%1 = icmp eq i32 %n, 0
	br i1 %1, label %if.then.1, label %if.else.1

if.then.1:
	br label %if.merge.1

if.else.1:
	%2 = sub i32 %n, 1
	%3 = call i32 @Main_factorial({ i8* }* %self, i32 %2)
	%4 = mul i32 %n, %3
	br label %if.merge.1

if.merge.1:
	%5 = phi i32 [ 1, %if.then.1 ], [ %4, %if.else.1 ]
	ret i32 %5
}

define i8* @Main_main({ i8* }* %self) {
0:
	%1 = alloca i32
	store i32 55, i32* %1
	%2 = load i32, i32* %1
	%3 = call i32 @Main_factorial({ i8* }* %self, i32 %2)
	%4 = call { i8* }* @IO_out_int({ i8* }* @io_instance, i32 %3)
	ret { i8* }* %4
}

define i32 @main() {
0:
	%1 = call i8* @Main_main({ i8* }* @main_instance)
	ret i32 0
}
