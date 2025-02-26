package codegen

import (
	"cool-compiler/ast"
	"fmt"
	"sort" // Add this import

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
	ifCounter        int
	whileCounter     int
	classInheritance map[string]string   // Maps class name to parent class name
	classFields      map[string][]string // Maps class name to field names in order
	mallocFunc       *ir.Func
	exitFunc         *ir.Func
	typeStrings      map[string]value.Value // Maps class names to their string constants
}

func NewGenerator() *Generator {
	g := &Generator{
		module:           ir.NewModule(),
		classes:          make(map[string]types.Type),
		symbolTable:      make(map[string]map[string]value.Value),
		ifCounter:        0,
		whileCounter:     0,
		classInheritance: make(map[string]string),
		classFields:      make(map[string][]string),
		typeStrings:      make(map[string]value.Value),
	}
	return g
}

func (g *Generator) createClassType(class *ast.Class) {
	// Record inheritance relationship
	if class.Parent != nil {
		// IDR: Diro f semantic
		if class.Parent.Value == "Int" {
			panic(fmt.Sprintf("Class %s cannot inherit from Int", class.Name.Value))
		}
		g.classInheritance[class.Name.Value] = class.Parent.Value
	} else {
		g.classInheritance[class.Name.Value] = "Object"
	}

	// Get parent fields first
	fields := make([]types.Type, 0)
	fieldNames := make([]string, 0)

	// Start with vtable pointer
	fields = append(fields, types.NewPointer(types.I8))

	// Add parent class fields if any
	if parentName, exists := g.classInheritance[class.Name.Value]; exists && parentName != "Object" {
		if parentFields, ok := g.classFields[parentName]; ok {
			for _, fieldName := range parentFields {
				if _, exists := g.getSymbol(parentName, fieldName); exists {
					fields = append(fields, g.convertType(fieldName))
					fieldNames = append(fieldNames, fieldName)
				}
			}
		}
	}

	// Initialize symbol table for this class
	g.initializeSymbolTable(class.Name.Value)

	// Add this class's fields
	for _, feature := range class.Features {
		if attr, ok := feature.(*ast.Attribute); ok {
			fields = append(fields, g.convertType(attr.Type.Value))
			fieldNames = append(fieldNames, attr.Name.Value)
			// Store field index for later use in assignments
			g.addSymbol(class.Name.Value, attr.Name.Value,
				constant.NewInt(types.I32, int64(len(fields)-1)))
		}
	}

	// Store field names for inheritance
	g.classFields[class.Name.Value] = fieldNames

	classType := types.NewStruct(fields...)
	g.classes[class.Name.Value] = classType
}

func (g *Generator) generateClassMethods(class *ast.Class) {
	g.generateConstructor(class)
	for _, feature := range class.Features {
		if method, ok := feature.(*ast.Method); ok {
			g.generateMethod(class.Name.Value, method)
		}
	}
}
func (g *Generator) generateConstructor(class *ast.Class) {
	className := class.Name.Value
	constructorName := fmt.Sprintf("%s_init", className)

	constructor := g.module.NewFunc(constructorName, types.NewPointer(g.classes[className]))
	self := ir.NewParam("self", types.NewPointer(g.classes[className]))
	constructor.Params = append(constructor.Params, self)

	block := constructor.NewBlock("")
	g.currentBlock = block
	g.currentClassName = className
	g.currentFunc = constructor

	// Initialize parent class first
	if parentName, exists := g.classInheritance[className]; exists && parentName != "Object" {
		parentInit := g.getFunction(fmt.Sprintf("%s_init", parentName))
		if parentInit != nil {
			block.NewCall(parentInit, self)
		}
	}

	// Initialize class attributes
	for _, feature := range class.Features {
		if attr, ok := feature.(*ast.Attribute); ok {
			if fieldIdx, exists := g.getSymbol(className, attr.Name.Value); exists {
				fieldPtr := block.NewGetElementPtr(g.classes[className], self,
					constant.NewInt(types.I32, 0),
					fieldIdx)

				var initValue value.Value
				if attr.Expression != nil {
					g.currentBlock = block
					initValue = g.generateExpression(attr.Expression)
				} else {
					// Default initialization
					switch attr.Type.Value {
					case "Int":
						initValue = constant.NewInt(types.I32, 0)
					case "Bool":
						initValue = constant.NewInt(types.I1, 0)
					case "String":
						initValue = g.createStringConstant("")
					default:
						if classType, ok := g.classes[attr.Type.Value]; ok {
							initValue = constant.NewNull(types.NewPointer(classType))
						} else {
							initValue = constant.NewNull(types.NewPointer(types.I8))
						}
					}
				}

				// Handle type conversion if needed
				targetType := fieldPtr.Type().(*types.PointerType).ElemType
				if !initValue.Type().Equal(targetType) {
					if types.IsPointer(targetType) && types.IsPointer(initValue.Type()) {
						initValue = block.NewBitCast(initValue, targetType)
					}
				}

				block.NewStore(initValue, fieldPtr)
			}
		}
	}

	block.NewRet(self)
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
		if value, exists := g.getSymbol(g.currentClassName, e.Value); exists {
			block := g.currentBlock
			if alloca, ok := value.(*ir.InstAlloca); ok {
				// Local variable
				return block.NewLoad(alloca.ElemType, alloca)
			}

			// Class field access
			self := g.currentFunc.Params[0]
			fieldPtr := block.NewGetElementPtr(g.classes[g.currentClassName], self,
				constant.NewInt(types.I32, 0),
				value) // value here is the field index

			// Load the field value based on its type
			fieldType := fieldPtr.Type().(*types.PointerType).ElemType
			return block.NewLoad(fieldType, fieldPtr)
		}

		// Check parameters
		for _, param := range g.currentFunc.Params {
			if param.Name() == e.Value {
				return param
			}
		}

		fmt.Printf("Warning: Object identifier '%s' not found\n", e.Value)
		return constant.NewInt(types.I32, 0)

	case *ast.CallExpression:
		// Get function to call
		funcName := ""
		var receiver value.Value

		// Handle method calls on objects or self
		if objId, ok := e.Function.(*ast.ObjectIdentifier); ok {
			switch objId.Value {
			case "abort":
				abortFunc := g.getFunction("Object_abort")
				if abortFunc != nil {
					return g.currentBlock.NewCall(abortFunc, g.currentFunc.Params[0])
				}
			case "type_name":
				typeNameFunc := g.getFunction("Object_type_name")
				if typeNameFunc != nil {
					return g.currentBlock.NewCall(typeNameFunc, g.currentFunc.Params[0])
				}
			case "copy":
				copyFunc := g.getFunction("Object_copy")
				if copyFunc != nil {
					return g.currentBlock.NewCall(copyFunc, g.currentFunc.Params[0])
				}
			}

			if objId.Value == "out_string" || objId.Value == "out_int" ||
				objId.Value == "in_string" || objId.Value == "in_int" {
				// IO methods
				funcName = "IO_" + objId.Value
				receiver = g.getIOInstance()
			} else {
				// Regular method calls
				funcName = g.currentClassName + "_" + objId.Value
				// Use self parameter for method calls
				receiver = g.currentFunc.Params[0]
			}
		}

		function := g.getFunction(funcName)
		if function == nil {
			return constant.NewInt(types.I32, 0)
		}

		// Generate arguments
		args := []value.Value{receiver} // Start with receiver (self/IO instance)

		// Generate argument expressions
		for _, arg := range e.Arguments {
			argValue := g.generateExpression(arg)
			if argValue != nil {
				args = append(args, argValue)
			}
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
		assignValue := g.generateExpression(e.Expression)

		if varValue, exists := g.getSymbol(g.currentClassName, e.Name); exists {
			if alloca, ok := varValue.(*ir.InstAlloca); ok {
				// Local variable assignment
				currBlock.NewStore(assignValue, alloca)
				return assignValue
			}

			// Class field assignment
			self := g.currentFunc.Params[0]
			fieldPtr := currBlock.NewGetElementPtr(g.classes[g.currentClassName], self,
				constant.NewInt(types.I32, 0),
				varValue)

			// Handle type conversion if needed
			targetType := fieldPtr.Type().(*types.PointerType).ElemType
			if !assignValue.Type().Equal(targetType) {
				if types.IsPointer(targetType) && types.IsPointer(assignValue.Type()) {
					assignValue = currBlock.NewBitCast(assignValue, targetType)
				} else if targetType.Equal(types.I32) && types.IsPointer(assignValue.Type()) {
					assignValue = currBlock.NewPtrToInt(assignValue, types.I32)
				} else if types.IsPointer(targetType) && assignValue.Type().Equal(types.I32) {
					assignValue = currBlock.NewIntToPtr(assignValue, targetType)
				}
			}

			currBlock.NewStore(assignValue, fieldPtr)
			return assignValue
		}

		fmt.Printf("Warning: Assignment target '%s' not found\n", e.Name)
		return assignValue

	case *ast.LetExpression:
		return g.generateLetExpression(e)

	case *ast.IfExpression:
		return g.generateIfExpression(e)

	case *ast.WhileExpression:
		return g.generateWhileExpression(e)

	case *ast.NewExpression:
		// Get the class type being instantiated
		classType, ok := g.classes[e.Type.Value]
		if !ok {
			fmt.Printf("Error: Unknown class type %s\n", e.Type.Value)
			return constant.NewNull(types.NewPointer(types.I8))
		}

		// Calculate size of the class type
		structType, ok := classType.(*types.StructType)
		if !ok {
			fmt.Printf("Error: Class type %s is not a struct type\n", e.Type.Value)
			return constant.NewNull(types.NewPointer(types.I8))
		}

		// Get size in bytes (each field is 8 bytes on 64-bit systems)
		sizeInBytes := len(structType.Fields) * 8
		size := constant.NewInt(types.I32, int64(sizeInBytes))

		// Create allocation for the new object
		malloc := g.currentBlock.NewCall(g.mallocFunc, size)

		// Bitcast malloc result to correct class type pointer
		instance := g.currentBlock.NewBitCast(malloc, types.NewPointer(classType))

		// Store class name in vtable
		vtablePtr := g.currentBlock.NewGetElementPtr(classType, instance,
			constant.NewInt(types.I32, 0),
			constant.NewInt(types.I32, 0))

		// Get or create the string constant for this class name
		typeStr, exists := g.typeStrings[e.Type.Value]
		if !exists {
			typeStr = g.createStringConstant(e.Type.Value)
			g.typeStrings[e.Type.Value] = typeStr
		}
		typeStrPtr := g.currentBlock.NewBitCast(typeStr, types.NewPointer(types.I8))
		g.currentBlock.NewStore(typeStrPtr, vtablePtr)
		// Call constructor/initializer if it exists
		constructorName := fmt.Sprintf("%s_init", e.Type.Value)
		if constructor := g.getFunction(constructorName); constructor != nil {
			g.currentBlock.NewCall(constructor, instance)
		}

		return instance

	case *ast.DispatchExpression:
		// Generate code for the object
		receiver := g.generateExpression(e.Object)

		// Determine class type of receiver
		var className string
		if ptrType, ok := receiver.Type().(*types.PointerType); ok {
			if structType, ok := ptrType.ElemType.(*types.StructType); ok {
				// Get class name from type - you'll need to maintain a reverse mapping
				// from LLVM types to class names
				className = g.getClassNameFromType(structType)
			}
		}

		// Get the method name
		methodName := fmt.Sprintf("%s_%s", className, e.Method.Value)

		// Look up the method
		method := g.getFunction(methodName)
		if method == nil {
			fmt.Printf("Error: Method %s not found in class %s\n", e.Method.Value, className)
			return constant.NewNull(types.NewPointer(types.I8))
		}

		// Generate code for arguments
		args := []value.Value{receiver} // First argument is always 'self'
		for _, arg := range e.Arguments {
			argValue := g.generateExpression(arg)
			args = append(args, argValue)
		}

		// Generate the method call
		return g.currentBlock.NewCall(method, args...)

	case *ast.StaticDispatchExpression:
		// Generate code for the object first
		receiver := g.generateExpression(e.Object)
		if receiver == nil {
			return constant.NewNull(types.NewPointer(types.I8))
		}

		// Get the static type specified in the dispatch
		staticTypeName := e.StaticType.Value

		// Get method name by combining static type and method name
		methodName := fmt.Sprintf("%s_%s", staticTypeName, e.Method.Value)

		// Look up the method
		method := g.getFunction(methodName)
		if method == nil {
			fmt.Printf("Error: Method %s not found in class %s\n", e.Method.Value, staticTypeName)
			return constant.NewNull(types.NewPointer(types.I8))
		}

		// Generate code for arguments
		args := []value.Value{receiver} // First argument is always 'self'

		// Generate code for each argument
		for _, arg := range e.Arguments {
			argValue := g.generateExpression(arg)
			if argValue != nil {
				args = append(args, argValue)
			}
		}

		// Generate the static method call
		return g.currentBlock.NewCall(method, args...)

	case *ast.CaseExpression:
		// Generate test expression
		test := g.generateExpression(e.Expression)
		if test == nil {
			return constant.NewNull(types.NewPointer(types.I8))
		}

		// Get the type of the test expression
		var typeStr value.Value
		if types.IsPointer(test.Type()) {
			if test.Type().Equal(types.NewPointer(types.I8)) {
				// For strings, create string type directly
				typeStr = g.createStringConstant("String")
				typeStr = g.currentBlock.NewBitCast(typeStr, types.NewPointer(types.I8))
			} else {
				// For class instances, get type from vtable
				vtablePtr := g.currentBlock.NewGetElementPtr(test.Type().(*types.PointerType).ElemType,
					test,
					constant.NewInt(types.I32, 0),
					constant.NewInt(types.I32, 0))
				typeStr = g.currentBlock.NewLoad(types.NewPointer(types.I8), vtablePtr)
			}
		} else {
			// For primitive types (Int, Bool), create type string directly
			switch test.Type() {
			case types.I32:
				typeStr = g.createStringConstant("Int")
			case types.I1:
				typeStr = g.createStringConstant("Bool")
			default:
				typeStr = g.createStringConstant("Object")
			}
			typeStr = g.currentBlock.NewBitCast(typeStr, types.NewPointer(types.I8))
		}

		// Sort branches by specificity
		sortedBranches := g.sortBranchesBySpecificity(e.Cases)

		// Create merge block for final result
		mergeBlock := g.currentFunc.NewBlock("case.merge")

		// Create PHI node for collecting results
		incomingValues := []value.Value{}
		incomingBlocks := []*ir.Block{}

		// Store current block for branching
		currentBlock := g.currentBlock

		// Generate branches
		var nextBlock *ir.Block
		for i, branch := range sortedBranches {
			caseBlock := g.currentFunc.NewBlock(fmt.Sprintf("case.%d", i))
			nextBlock = g.currentFunc.NewBlock(fmt.Sprintf("case.next.%d", i))

			// Compare types
			branchTypeStr := g.createStringConstant(branch.TypeIdentifier.Value)
			strcmp := g.getFunction("strcmp")
			cmpResult := currentBlock.NewCall(strcmp, typeStr,
				currentBlock.NewBitCast(branchTypeStr, types.NewPointer(types.I8)))
			isMatch := currentBlock.NewICmp(enum.IPredEQ, cmpResult, constant.NewInt(types.I32, 0))
			currentBlock.NewCondBr(isMatch, caseBlock, nextBlock)

			// Generate case body
			g.currentBlock = caseBlock
			// Create new scope for the case variable
			g.initializeSymbolTable(g.currentClassName)

			// Bind the matched value to the case variable
			alloca := caseBlock.NewAlloca(test.Type())
			caseBlock.NewStore(test, alloca)
			g.addSymbol(g.currentClassName, branch.ObjectIdentifier.Value, alloca)

			// Generate the branch body
			result := g.generateExpression(branch.Body)
			if result != nil {
				incomingValues = append(incomingValues, result)
				incomingBlocks = append(incomingBlocks, g.currentBlock)
				if g.currentBlock.Term == nil {
					g.currentBlock.NewBr(mergeBlock)
				}
			}

			// Move to next comparison
			currentBlock = nextBlock
		}

		// Handle no-match case (abort)
		abortBlock := g.currentFunc.NewBlock("case.abort")
		currentBlock.NewBr(abortBlock)
		g.currentBlock = abortBlock

		// Call abort
		abortFunc := g.getFunction("Object_abort")
		if abortFunc != nil {
			abortResult := g.currentBlock.NewCall(abortFunc, g.currentFunc.Params[0])
			incomingValues = append(incomingValues, abortResult)
			incomingBlocks = append(incomingBlocks, g.currentBlock)
			if g.currentBlock.Term == nil {
				g.currentBlock.NewBr(mergeBlock)
			}
		}

		// Set up merge block with PHI node
		g.currentBlock = mergeBlock
		if len(incomingValues) > 0 {
			incomings := make([]*ir.Incoming, len(incomingValues))
			for i := range incomingValues {
				incomings[i] = ir.NewIncoming(incomingValues[i], incomingBlocks[i])
			}
			return mergeBlock.NewPhi(incomings...)
		}

		return constant.NewNull(types.NewPointer(types.I8))

	case *ast.IsVoidExpression:
		return g.handleIsVoidExpression(e)

	default:
		return constant.NewInt(types.I32, 0)
	}
}

func (g *Generator) generateLetExpression(expr *ast.LetExpression) value.Value {
	block := g.currentBlock
	var result value.Value

	// For each binding in the let expression
	for _, binding := range expr.Bindings {
		var initValue value.Value

		if binding.Init != nil {
			if strLit, ok := binding.Init.(*ast.StringLiteral); ok {
				// Create string constant and get pointer to first element
				strConst := g.createStringConstant(strLit.Value)
				initValue = block.NewBitCast(strConst, types.NewPointer(types.I8))
			} else {
				initValue = g.generateExpression(binding.Init)
			}
		} else {
			// Default initialization
			switch binding.Type.Value {
			case "String":
				// Create empty string constant and get pointer to first element
				strConst := g.createStringConstant("")
				initValue = block.NewBitCast(strConst, types.NewPointer(types.I8))
			case "Int":
				initValue = constant.NewInt(types.I32, 0)
			case "Bool":
				initValue = constant.NewInt(types.I1, 0)
			default:
				// Create null pointer of the correct type
				if ptrType, ok := g.convertType(binding.Type.Value).(*types.PointerType); ok {
					initValue = constant.NewNull(ptrType)
				} else {
					initValue = constant.NewNull(types.NewPointer(types.I8))
				}
			}
		}

		// Allocate space for the variable
		varType := g.convertType(binding.Type.Value)
		alloca := block.NewAlloca(varType)
		block.NewStore(initValue, alloca)

		// Add the variable to the symbol table for the current scope
		g.addSymbol(g.currentClassName, binding.Identifier.Value, alloca)
	}

	// Generate and return the 'in' expression
	if expr.In != nil {
		result = g.generateExpression(expr.In)
	}

	return result
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
	// Convert pointers to integers for arithmetic operations
	if types.IsPointer(left.Type()) {
		left = block.NewPtrToInt(left, types.I32)
	}
	if types.IsPointer(right.Type()) {
		right = block.NewPtrToInt(right, types.I32)
	}

	switch expr.Operator {
	case "+":
		result := block.NewAdd(left, right)
		// If this is part of an assignment (i <- i + 1),
		// the Assignment case will handle storing the result
		return result
	case "-":
		if types.IsPointer(left.Type()) {
			// If left is pointer and right is integer, scale the integer
			if right.Type().Equal(types.I32) {
				return block.NewIntToPtr(
					block.NewSub(
						block.NewPtrToInt(left, types.I32),
						right,
					),
					left.Type(),
				)
			}
		} else if types.IsPointer(right.Type()) {
			// If right is pointer and left is integer, scale the integer
			if left.Type().Equal(types.I32) {
				return block.NewIntToPtr(
					block.NewSub(
						left,
						block.NewPtrToInt(right, types.I32),
					),
					right.Type(),
				)
			}
		}
		// Default case for non-pointer subtraction
		return block.NewSub(left, right)
	case "*":
		if types.IsPointer(left.Type()) {
			// If left is pointer and right is integer, scale the integer
			if right.Type().Equal(types.I32) {
				return block.NewIntToPtr(
					block.NewMul(
						block.NewPtrToInt(left, types.I32),
						right,
					),
					left.Type(),
				)
			}
		} else if types.IsPointer(right.Type()) {
			// If right is pointer and left is integer, scale the integer
			if left.Type().Equal(types.I32) {
				return block.NewIntToPtr(
					block.NewMul(
						left,
						block.NewPtrToInt(right, types.I32),
					),
					right.Type(),
				)
			}
		}
		// Default case for non-pointer multiplication
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
			// fmt.Printf("Found symbol %s in class %s\n", name, className)
			return val, true
		}
	}
	// fmt.Printf("Symbol %s not found in class %s\n", name, className)

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

func (g *Generator) Generate(program *ast.Program) *ir.Module {
	// Initialize malloc if not already done
	if g.mallocFunc == nil {
		g.mallocFunc = g.module.NewFunc("malloc",
			types.NewPointer(types.I8),
			ir.NewParam("size", types.I32))
		g.mallocFunc.Linkage = enum.LinkageExternal
		g.mallocFunc.CallingConv = enum.CallingConvC
	}

	strcmp := g.module.NewFunc("strcmp", types.I32,
		ir.NewParam("s1", types.NewPointer(types.I8)),
		ir.NewParam("s2", types.NewPointer(types.I8)))
	strcmp.Linkage = enum.LinkageExternal
	strcmp.CallingConv = enum.CallingConvC

	// Declare all C functions with proper linkage once
	strlenFunc := g.module.NewFunc("strlen", types.I32,
		ir.NewParam("str", types.NewPointer(types.I8)))
	strlenFunc.Linkage = enum.LinkageExternal
	strlenFunc.CallingConv = enum.CallingConvC

	strcpyFunc := g.module.NewFunc("strcpy", types.NewPointer(types.I8),
		ir.NewParam("dest", types.NewPointer(types.I8)),
		ir.NewParam("src", types.NewPointer(types.I8)))
	strcpyFunc.Linkage = enum.LinkageExternal
	strcpyFunc.CallingConv = enum.CallingConvC

	// Declare exit function globally
	exitFunc := g.module.NewFunc("exit", types.Void,
		ir.NewParam("status", types.I32))
	exitFunc.Linkage = enum.LinkageExternal
	exitFunc.CallingConv = enum.CallingConvC
	g.exitFunc = exitFunc

	g.generateObjectClass()
	g.generateIntClass()
	g.generateStringMethods()

	// Declare IO functions first
	g.declareIOFunctions()

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

func (g *Generator) generateWhileExpression(expr *ast.WhileExpression) value.Value {
	// Increment counter for unique block names
	g.whileCounter++
	currentWhile := g.whileCounter

	// Create blocks for loop with unique names
	condBlock := g.currentFunc.NewBlock(fmt.Sprintf("while.cond.%d", currentWhile))
	bodyBlock := g.currentFunc.NewBlock(fmt.Sprintf("while.body.%d", currentWhile))
	afterBlock := g.currentFunc.NewBlock(fmt.Sprintf("while.after.%d", currentWhile))

	// Branch from current block to condition block
	g.currentBlock.NewBr(condBlock)

	// Generate condition code
	g.currentBlock = condBlock
	condition := g.generateExpression(expr.Condition)

	// Convert condition to boolean if needed
	var condValue value.Value
	if !condition.Type().Equal(types.I1) {
		zero := constant.NewInt(types.I32, 0)
		condValue = g.currentBlock.NewICmp(enum.IPredNE, condition, zero)
	} else {
		condValue = condition
	}

	// Conditional branch based on condition
	g.currentBlock.NewCondBr(condValue, bodyBlock, afterBlock)

	// Generate body code
	g.currentBlock = bodyBlock
	g.generateExpression(expr.Body)

	// Jump back to condition block at end of body
	if g.currentBlock.Term == nil {
		g.currentBlock.NewBr(condBlock)
	}

	// Set current block to after block
	g.currentBlock = afterBlock

	// Return zero since while expressions return void in COOL
	return constant.NewInt(types.I32, 0)
}

func (g *Generator) getClassNameFromType(structType *types.StructType) string {
	// Iterate through class map to find matching type
	for className, classType := range g.classes {
		if classType == structType {
			return className
		}
	}
	return "Object" // default to Object if not found
}

func (g *Generator) findMethodInHierarchy(className, methodName string) (string, bool) {
	current := className
	for {
		// Try to find method in current class
		methodFullName := fmt.Sprintf("%s_%s", current, methodName)
		if method := g.getFunction(methodFullName); method != nil {
			return methodFullName, true
		}

		// Move up inheritance chain
		parent, exists := g.classInheritance[current]
		if !exists {
			// Try Object methods as last resort
			objectMethod := fmt.Sprintf("Object_%s", methodName)
			if method := g.getFunction(objectMethod); method != nil {
				return objectMethod, true
			}
			break
		}
		current = parent
	}
	return "", false
}

func (g *Generator) generateObjectClass() {
	// Create Object class type with just vtable pointer
	if g.mallocFunc == nil {
		g.mallocFunc = g.module.NewFunc("malloc",
			types.NewPointer(types.I8),
			ir.NewParam("size", types.I32))
	}

	// Create Object class type with just vtable pointer
	objectType := types.NewStruct(types.NewPointer(types.I8))
	g.classes["Object"] = objectType

	// Generate abort() method
	abortFunc := g.module.NewFunc("Object_abort", types.NewPointer(g.classes["Object"]))
	self := ir.NewParam("self", types.NewPointer(g.classes["Object"]))
	abortFunc.Params = append(abortFunc.Params, self)
	block := abortFunc.NewBlock("")

	callInst := block.NewCall(g.exitFunc, constant.NewInt(types.I32, 1))
	fmt.Println("I am here")
	callInst.Tail = enum.TailTail
	block.NewUnreachable()

	// Generate type_name() method
	typeNameFunc := g.module.NewFunc("Object_type_name", types.NewPointer(types.I8))
	self = ir.NewParam("self", types.NewPointer(g.classes["Object"]))
	typeNameFunc.Params = append(typeNameFunc.Params, self)
	block = typeNameFunc.NewBlock("")

	// Get the vtable pointer (first field)
	vtablePtr := block.NewGetElementPtr(g.classes["Object"], self,
		constant.NewInt(types.I32, 0),
		constant.NewInt(types.I32, 0))

	// Load the string pointer from vtable
	typeName := block.NewLoad(types.NewPointer(types.I8), vtablePtr)
	block.NewRet(typeName)

	// Generate copy() method
	copyFunc := g.module.NewFunc("Object_copy", types.NewPointer(g.classes["Object"]))
	self = ir.NewParam("self", types.NewPointer(g.classes["Object"]))
	copyFunc.Params = append(copyFunc.Params, self)
	block = copyFunc.NewBlock("")

	// Allocate new object
	size := constant.NewInt(types.I32, 8) // size of Object (just vtable pointer)
	newObj := block.NewCall(g.mallocFunc, size)

	// Copy contents
	srcCast := block.NewBitCast(self, types.NewPointer(types.I8))
	dstCast := block.NewBitCast(newObj, types.NewPointer(types.I8))
	// Copy size bytes from source to destination
	memcpyFunc := g.module.NewFunc("memcpy",
		types.NewPointer(types.I8),
		ir.NewParam("dest", types.NewPointer(types.I8)),
		ir.NewParam("src", types.NewPointer(types.I8)),
		ir.NewParam("size", types.I32))
	block.NewCall(memcpyFunc, dstCast, srcCast, size)

	block.NewRet(block.NewBitCast(newObj, types.NewPointer(g.classes["Object"])))
}

func (g *Generator) generateIntClass() {
	// Create Int class type with just a value field and vtable pointer
	intType := types.NewStruct(
		types.NewPointer(types.I8), // vtable pointer
		types.I32,                  // value field
	)
	g.classes["Int"] = intType

	// Store that Int is a basic class that can't be inherited from
	g.classInheritance["Int"] = "Object"

	// Generate constructor for Int
	intInit := g.module.NewFunc("Int_init", types.NewPointer(g.classes["Int"]))
	self := ir.NewParam("self", types.NewPointer(g.classes["Int"]))
	intInit.Params = append(intInit.Params, self)
	block := intInit.NewBlock("")

	// Initialize value to 0
	valuePtr := block.NewGetElementPtr(g.classes["Int"], self,
		constant.NewInt(types.I32, 0),
		constant.NewInt(types.I32, 1)) // index 1 is the value field
	block.NewStore(constant.NewInt(types.I32, 0), valuePtr)

	block.NewRet(self)
}

func (g *Generator) generateStringMethods() {
	// Create String class type with vtable pointer and string data
	stringType := types.NewStruct(
		types.NewPointer(types.I8), // vtable pointer
		types.NewPointer(types.I8), // actual string data
	)
	g.classes["String"] = stringType
	g.classInheritance["String"] = "Object"

	// Generate length method
	g.generateStringLength()
}

func (g *Generator) generateStringLength() {
	lengthFunc := g.module.NewFunc("String_length", types.NewPointer(g.classes["Int"]))
	self := ir.NewParam("self", types.NewPointer(g.classes["String"]))
	lengthFunc.Params = append(lengthFunc.Params, self)

	block := lengthFunc.NewBlock("")

	// Create a new Int object
	intObj := block.NewCall(g.mallocFunc, constant.NewInt(types.I32, 16)) // size of Int (vtable + value)
	typedIntObj := block.NewBitCast(intObj, types.NewPointer(g.classes["Int"]))

	// Set vtable pointer for Int object
	vtablePtr := block.NewGetElementPtr(g.classes["Int"], typedIntObj,
		constant.NewInt(types.I32, 0),
		constant.NewInt(types.I32, 0))
	typeStr := g.createStringConstant("Int")
	block.NewStore(block.NewBitCast(typeStr, types.NewPointer(types.I8)), vtablePtr)

	// Get string data pointer from self
	strDataPtr := block.NewGetElementPtr(g.classes["String"], self,
		constant.NewInt(types.I32, 0),
		constant.NewInt(types.I32, 1))
	strPtr := block.NewLoad(types.NewPointer(types.I8), strDataPtr)

	// Call strlen
	strlenFunc := g.getFunction("strlen")
	length := block.NewCall(strlenFunc, strPtr)

	// Store length in Int object
	valuePtr := block.NewGetElementPtr(g.classes["Int"], typedIntObj,
		constant.NewInt(types.I32, 0),
		constant.NewInt(types.I32, 1))
	block.NewStore(length, valuePtr)

	block.NewRet(typedIntObj)
}

func (g *Generator) sortBranchesBySpecificity(branches []*ast.Case) []*ast.Case {
	sorted := make([]*ast.Case, len(branches))
	copy(sorted, branches)

	// Sort branches based on inheritance hierarchy (most specific first)
	sort.Slice(sorted, func(i, j int) bool {
		type1 := sorted[i].TypeIdentifier.Value
		type2 := sorted[j].TypeIdentifier.Value

		// Check if type1 is a subclass of type2
		current := type1
		for current != "Object" {
			if current == type2 {
				return true
			}
			current = g.classInheritance[current]
		}
		return false
	})

	return sorted
}

func (g *Generator) handleIsVoidExpression(expr *ast.IsVoidExpression) value.Value {
	// Generate code for the expression to test
	testExpr := g.generateExpression(expr.Expression)
	if testExpr == nil {
		return constant.NewInt(types.I1, 1) // true if expression is nil
	}

	// Handle different types
	switch testExpr.Type() {
	case types.I32: // Int
		return constant.NewInt(types.I1, 0) // Int can't be void

	case types.I1: // Bool
		return constant.NewInt(types.I1, 0) // Bool can't be void

	default:
		// For class types (which are always pointers in our implementation)
		if ptrType, ok := testExpr.Type().(*types.PointerType); ok {
			nullPtr := constant.NewNull(ptrType)
			return g.currentBlock.NewICmp(enum.IPredEQ, testExpr, nullPtr)
		}

		// For any other type, cast to i8* and compare with null
		castedExpr := g.currentBlock.NewBitCast(testExpr, types.NewPointer(types.I8))
		nullPtr := constant.NewNull(types.NewPointer(types.I8))
		return g.currentBlock.NewICmp(enum.IPredEQ, castedExpr, nullPtr)
	}
}
