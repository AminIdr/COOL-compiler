package codegen

import (
	"cool-compiler/ast"
	"fmt"

	"github.com/llir/llvm/ir"
	"github.com/llir/llvm/ir/constant"
	"github.com/llir/llvm/ir/enum"
	"github.com/llir/llvm/ir/types"
	"github.com/llir/llvm/ir/value"
)

type Generator struct {
	module           *ir.Module
	classes          map[string]types.Type
	currentFunc      *ir.Func
	currentBlock     *ir.Block
	currentClassName string
	symbolTable      map[string]map[string]value.Value
	ioInstance       value.Value
	mallocFunc       *ir.Func
	ifCounter        int
}

func NewGenerator() *Generator {
	g := &Generator{
		module:      ir.NewModule(),
		classes:     make(map[string]types.Type),
		symbolTable: make(map[string]map[string]value.Value),
		ifCounter:   0,
	}
	return g
}

// func (g *Generator) createClassType(class *ast.Class) {
// 	// Create struct type for class attributes
// 	fields := make([]types.Type, 0)

// 	// Add vtable pointer as first field - change from void to i8
// 	fields = append(fields, types.NewPointer(types.I8)) // Changed from types.Void

// 	// Add fields for class attributes
// 	for _, feature := range class.Features {
// 		if attr, ok := feature.(*ast.Attribute); ok {
// 			fields = append(fields, g.convertType(attr.Type.Value))
// 		}
// 	}

// 	classType := types.NewStruct(fields...)
// 	g.classes[class.Name.Value] = classType
// }

func (g *Generator) createClassType(class *ast.Class) {
	// Create struct type for class attributes
	fields := make([]types.Type, 0)

	// Add vtable pointer as first field
	fields = append(fields, types.NewPointer(types.I8))

	// Initialize symbol table for this class if not exists
	g.initializeSymbolTable(class.Name.Value)

	// Add fields for class attributes and store their indices
	for idx, feature := range class.Features {
		if attr, ok := feature.(*ast.Attribute); ok {
			fields = append(fields, g.convertType(attr.Type.Value))
			// Store field index for later use in assignments
			g.addSymbol(class.Name.Value, attr.Name.Value, constant.NewInt(types.I32, int64(idx+1)))
		}
	}

	classType := types.NewStruct(fields...)
	g.classes[class.Name.Value] = classType
}

func (g *Generator) generateClassMethods(class *ast.Class) {
	for _, feature := range class.Features {
		if method, ok := feature.(*ast.Method); ok {
			g.generateMethod(class.Name.Value, method)
		}
	}
}

func (g *Generator) generateMethod(className string, method *ast.Method) {
	g.currentClassName = className
	returnType := g.convertType(method.Type.Value)

	// Create function
	funcName := className + "_" + method.Name.Value
	fn := g.module.NewFunc(funcName, returnType)

	// Add self parameter
	selfParam := ir.NewParam("self", types.NewPointer(g.classes[className]))
	fn.Params = append(fn.Params, selfParam)

	// Add other parameters
	for _, formal := range method.Formals {
		param := ir.NewParam(formal.Name.Value, g.convertType(formal.Type.Value))
		fn.Params = append(fn.Params, param)
	}

	// Store current function
	g.currentFunc = fn

	// Generate function body
	block := fn.NewBlock("")
	g.currentBlock = block
	result := g.generateExpression(method.Expression)

	// Add return instruction if not already present
	if g.currentBlock.Term == nil {
		g.handleReturnValue(result, returnType)
	}
}

func (g *Generator) convertType(coolType string) types.Type {
	switch coolType {
	case "Int":
		return types.I32
	case "Bool":
		return types.I1
	case "String":
		return types.NewPointer(types.I8)
	default:
		if classType, ok := g.classes[coolType]; ok {
			return types.NewPointer(classType)
		}
		// Default to i8* instead of void for unknown types
		return types.NewPointer(types.I8) // Changed from types.Void
	}
}

func (g *Generator) generateExpression(expr ast.Expression) value.Value {
	// Store current block
	if g.currentBlock == nil {
		g.currentBlock = g.currentFunc.Blocks[len(g.currentFunc.Blocks)-1]
	}

	switch e := expr.(type) {
	case *ast.IntegerLiteral:
		return constant.NewInt(types.I32, int64(e.Value))
	case *ast.BooleanLiteral:
		if e.Value {
			return constant.NewInt(types.I1, 1)
		}
		return constant.NewInt(types.I1, 0)
	case *ast.StringLiteral:
		return g.createStringConstant(e.Value)
	case *ast.BinaryExpression:
		return g.generateBinaryExpression(e)
	case *ast.ObjectIdentifier:
		// Get the alloca instruction for this variable from symbol table
		if value, exists := g.getSymbol(g.currentClassName, e.Value); exists {
			block := g.currentBlock
			if alloca, ok := value.(*ir.InstAlloca); ok {
				return block.NewLoad(alloca.ElemType, alloca)
			}
			return value
		}
		return constant.NewNull(types.NewPointer(types.I8))

	case *ast.CallExpression:
		// Get function to call
		funcName := ""
		if objId, ok := e.Function.(*ast.ObjectIdentifier); ok {
			funcName = "IO_" + objId.Value
		}

		function := g.getFunction(funcName)
		if function == nil {
			return constant.NewInt(types.I32, 0)
		}

		// Generate arguments
		args := []value.Value{}

		// Use the shared IO instance
		args = append(args, g.getIOInstance())

		// Generate argument expressions
		for _, arg := range e.Arguments {
			args = append(args, g.generateExpression(arg))
		}

		// Create call instruction
		return g.currentBlock.NewCall(function, args...)

	case *ast.BlockExpression:
		var lastValue value.Value
		// Generate code for each expression in the block
		for _, expr := range e.Expressions {
			lastValue = g.generateExpression(expr)
		}
		return lastValue // Return the value of the last expression

	case *ast.Assignment:
		currBlock := g.currentBlock
		self := g.currentFunc.Params[0]

		// Get the index of the attribute from symbol table
		fieldIndex, exists := g.getSymbol(g.currentClassName, e.Name)
		if !exists {
			return constant.NewNull(types.NewPointer(types.I8))
		}

		// Generate the value to assign
		value := g.generateExpression(e.Expression)

		// Generate pointer to the field
		fieldPtr := currBlock.NewGetElementPtr(g.classes[g.currentClassName], self,
			constant.NewInt(types.I32, 0),
			fieldIndex)

		// Store the value directly since we're already working with heap memory
		currBlock.NewStore(value, fieldPtr)
		return value

	case *ast.LetExpression:
		return g.generateLetExpression(e)

	case *ast.IfExpression:
		return g.generateIfExpression(e)

	default:
		return constant.NewInt(types.I32, 0)
	}
}

func (g *Generator) generateLetExpression(expr *ast.LetExpression) value.Value {
	block := g.currentBlock

	// For each binding in the let expression
	for _, binding := range expr.Bindings {
		// Allocate space for the variable
		varType := g.convertType(binding.Type.Value)
		alloca := block.NewAlloca(varType)

		// If there's an initialization value, generate it and store it
		if binding.Init != nil {
			initValue := g.generateExpression(binding.Init)
			block.NewStore(initValue, alloca)
		}

		// Add the variable to the symbol table for the current scope
		g.addSymbol(g.currentClassName, binding.Identifier.Value, alloca)
	}

	// Generate and return the 'in' expression
	return g.generateExpression(expr.In)
}

func (g *Generator) createStringConstant(value string) value.Value {
	// Add a counter to the Generator struct if not already present
	strCount := len(g.module.Globals)
	globalName := fmt.Sprintf("str.%d", strCount)

	strConst := constant.NewCharArrayFromString(value + "\x00")
	global := g.module.NewGlobalDef(globalName, strConst)
	global.Immutable = true
	return global
}

func (g *Generator) generateBinaryExpression(expr *ast.BinaryExpression) value.Value {
	left := g.generateExpression(expr.Left)
	right := g.generateExpression(expr.Right)
	block := g.currentBlock

	switch expr.Operator {
	case "+":
		return block.NewAdd(left, right)
	case "-":
		return block.NewSub(left, right)
	case "*":
		return block.NewMul(left, right)
	case "/":
		return block.NewSDiv(left, right)
	case "=":
		// Handle different types of equality comparisons
		if left.Type().Equal(types.I32) {
			return block.NewICmp(enum.IPredEQ, left, right)
		} else if types.IsPointer(left.Type()) {
			// For pointer types, ensure right is also a pointer
			if types.IsPointer(right.Type()) {
				return block.NewICmp(enum.IPredEQ, left, right)
			}
			// If comparing pointer with integer (likely 0/null)
			if right.Type().Equal(types.I32) {
				// Cast left.Type() to *types.PointerType and create null pointer
				if ptrType, ok := left.Type().(*types.PointerType); ok {
					nullPtr := constant.NewNull(ptrType)
					return block.NewICmp(enum.IPredEQ, left, nullPtr)
				}
			}
		}
		// Convert other types to i32 for comparison
		return block.NewICmp(enum.IPredEQ,
			block.NewPtrToInt(left, types.I32),
			block.NewPtrToInt(right, types.I32))
	case "<":
		return block.NewICmp(enum.IPredSLT, left, right)
	case "<=":
		return block.NewICmp(enum.IPredSLE, left, right)
	default:
		return constant.NewInt(types.I32, 0)
	}
}

func (g *Generator) initializeSymbolTable(className string) {
	if g.symbolTable[className] == nil {
		g.symbolTable[className] = make(map[string]value.Value)
	}
}

func (g *Generator) addSymbol(className, name string, value value.Value) {
	g.initializeSymbolTable(className)
	g.symbolTable[className][name] = value
}

func (g *Generator) getSymbol(className, name string) (value.Value, bool) {
	if table, ok := g.symbolTable[className]; ok {
		if val, exists := table[name]; exists {
			fmt.Printf("Found symbol %s in class %s\n", name, className)
			return val, true
		}
	}
	fmt.Printf("Symbol %s not found in class %s\n", name, className)

	return nil, false
}

func (g *Generator) generateMainFunction(classes []*ast.Class) {
	// Create main function
	mainFn := g.module.NewFunc("main", types.I32)
	block := mainFn.NewBlock("")

	// Create Main class instance
	mainInstance := g.module.NewGlobalDef("main_instance",
		constant.NewZeroInitializer(g.classes["Main"]))

	// Call Main_main function
	mainMain := g.getFunction("Main_main")
	if mainMain != nil {
		block.NewCall(mainMain, mainInstance)
	}

	// Return 0
	block.NewRet(constant.NewInt(types.I32, 0))
}

// Add after the Generator struct definition
func (g *Generator) declareIOFunctions() {
	// Create variadic function types for printf and scanf
	// printfType := types.NewFunc(types.I32)
	printfFormatParam := ir.NewParam("format", types.NewPointer(types.I8))

	// Create printf function with format parameter
	printf := g.module.NewFunc("printf", types.I32, printfFormatParam)
	printf.Sig.Variadic = true

	// Create scanf function with format parameter
	// scanfType := types.NewFunc(types.I32)
	scanfFormatParam := ir.NewParam("format", types.NewPointer(types.I8))
	scanf := g.module.NewFunc("scanf", types.I32, scanfFormatParam)
	scanf.Sig.Variadic = true

	// Store these functions in the generator for later use
	g.addSymbol("IO", "printf", printf)
	g.addSymbol("IO", "scanf", scanf)

}

func (g *Generator) generateIOMethods() {
	// out_string implementation
	outString := g.module.NewFunc("IO_out_string", types.NewPointer(g.classes["IO"]))
	self := ir.NewParam("self", types.NewPointer(g.classes["IO"]))
	str := ir.NewParam("x", types.NewPointer(types.I8))
	outString.Params = append(outString.Params, self, str)

	block := outString.NewBlock("")
	format := g.createStringConstant("%s")
	printf, _ := g.getSymbol("IO", "printf")
	block.NewCall(printf, format, str)
	// Add fflush call
	fflush := g.module.NewFunc("fflush", types.I32, ir.NewParam("stream", types.NewPointer(types.I8)))
	block.NewCall(fflush, constant.NewNull(types.NewPointer(types.I8)))
	block.NewRet(self)

	// out_int implementation
	outInt := g.module.NewFunc("IO_out_int", types.NewPointer(g.classes["IO"]))
	self = ir.NewParam("self", types.NewPointer(g.classes["IO"]))
	num := ir.NewParam("x", types.I32)
	outInt.Params = append(outInt.Params, self, num)

	block = outInt.NewBlock("")
	format = g.createStringConstant("%d\n")
	block.NewCall(printf, format, num)
	block.NewRet(self)

	// in_string implementation
	inString := g.module.NewFunc("IO_in_string", types.NewPointer(types.I8))
	self = ir.NewParam("self", types.NewPointer(g.classes["IO"]))
	inString.Params = append(inString.Params, self)

	block = inString.NewBlock("")

	// Allocate buffer
	buffer := block.NewCall(g.mallocFunc, constant.NewInt(types.I32, 1024))

	// Use %[^\n] to read until newline instead of %s
	format = g.createStringConstant("%1023[^\n]")
	scanf, _ := g.getSymbol("IO", "scanf")
	block.NewCall(scanf, format, buffer)

	// Consume the newline character
	format = g.createStringConstant("%*c")
	block.NewCall(scanf, format)

	// Return the heap-allocated buffer
	block.NewRet(buffer)

	// in_int implementation
	inInt := g.module.NewFunc("IO_in_int", types.I32)
	self = ir.NewParam("self", types.NewPointer(g.classes["IO"]))
	inInt.Params = append(inInt.Params, self)

	block = inInt.NewBlock("")
	intPtr := block.NewAlloca(types.I32)
	format = g.createStringConstant("%d")
	block.NewCall(scanf, format, intPtr)
	loadedValue := block.NewLoad(types.I32, intPtr)
	block.NewRet(loadedValue)
}

// Modify the Generate method
func (g *Generator) Generate(program *ast.Program) *ir.Module {
	// Declare IO functions first
	g.declareIOFunctions()
	// Declare required C functions
	// Declare required C functions once
	g.mallocFunc = g.module.NewFunc("malloc", types.NewPointer(types.I8),
		ir.NewParam("size", types.I32))
	g.module.NewFunc("strlen", types.I32,
		ir.NewParam("str", types.NewPointer(types.I8)))
	g.module.NewFunc("strcpy", types.NewPointer(types.I8),
		ir.NewParam("dest", types.NewPointer(types.I8)),
		ir.NewParam("src", types.NewPointer(types.I8)))

	// Create IO class type
	ioType := types.NewStruct(types.NewPointer(types.I8)) // vtable pointer
	g.classes["IO"] = ioType

	// Create Main class type (inherits from IO)
	mainType := types.NewStruct(types.NewPointer(types.I8)) // inherits IO's layout
	g.classes["Main"] = mainType

	// First pass: create class types
	for _, class := range program.Classes {
		if class.Name.Value != "IO" { // Skip IO class if present
			g.createClassType(class)
		}
	}

	// Generate IO methods
	g.generateIOMethods()

	// Second pass: generate methods
	for _, class := range program.Classes {
		if class.Name.Value != "IO" { // Skip IO class if present
			g.generateClassMethods(class)
		}
	}

	// Generate main function last
	g.generateMainFunction(program.Classes)

	return g.module
}

// Add this helper function to find functions in the module
func (g *Generator) getFunction(name string) *ir.Func {
	for _, f := range g.module.Funcs {
		if f.Name() == name {
			return f
		}
	}
	return nil
}

func (g *Generator) getIOInstance() value.Value {
	if g.ioInstance == nil {
		g.ioInstance = g.module.NewGlobalDef("io_instance",
			constant.NewZeroInitializer(g.classes["IO"]))
	}
	return g.ioInstance
}

func (g *Generator) handleReturnValue(result value.Value, returnType types.Type) {
	if types.IsPointer(returnType) {
		if !types.IsPointer(result.Type()) {
			g.currentBlock.NewRet(constant.NewNull(types.NewPointer(types.I8)))
		} else {
			g.currentBlock.NewRet(result)
		}
	} else {
		if returnType.Equal(result.Type()) {
			g.currentBlock.NewRet(result)
		} else {
			g.currentBlock.NewRet(constant.NewInt(types.I32, 0))
		}
	}
}
func (g *Generator) generateIfExpression(expr *ast.IfExpression) value.Value {
	// Increment counter for unique block names
	g.ifCounter++
	currentIf := g.ifCounter

	// Generate condition and blocks with unique names
	condition := g.generateExpression(expr.Condition)
	thenBlock := g.currentFunc.NewBlock(fmt.Sprintf("if.then.%d", currentIf))
	elseBlock := g.currentFunc.NewBlock(fmt.Sprintf("if.else.%d", currentIf))
	mergeBlock := g.currentFunc.NewBlock(fmt.Sprintf("if.merge.%d", currentIf))

	// Setup condition branching
	var condValue value.Value
	if !condition.Type().Equal(types.I1) {
		zero := constant.NewInt(types.I32, 0)
		condValue = g.currentBlock.NewICmp(enum.IPredNE, condition, zero)
	} else {
		condValue = condition
	}
	g.currentBlock.NewCondBr(condValue, thenBlock, elseBlock)

	// Generate then block
	g.currentBlock = thenBlock
	thenValue := g.generateExpression(expr.Consequence)
	if g.currentBlock.Term == nil {
		g.currentBlock.NewBr(mergeBlock)
	}

	// Generate else block
	g.currentBlock = elseBlock
	elseValue := g.generateExpression(expr.Alternative)
	if g.currentBlock.Term == nil {
		g.currentBlock.NewBr(mergeBlock)
	}

	// Merge block with PHI node
	g.currentBlock = mergeBlock
	phi := mergeBlock.NewPhi(
		ir.NewIncoming(thenValue, thenBlock),
		ir.NewIncoming(elseValue, elseBlock),
	)

	return phi
}
