package codegen

import (
	"cool-compiler/ast"
	"fmt"
	"sort"
	"strings"

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
	caseCounter      int
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
		caseCounter:      0,
		classInheritance: make(map[string]string),
		classFields:      make(map[string][]string),
		typeStrings:      make(map[string]value.Value),
	}
	return g
}

func (g *Generator) createClassType(class *ast.Class) {
	if class.Parent != nil {
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

	constructorName := fmt.Sprintf("%s_init", class.Name.Value)
	if g.getFunction(constructorName) == nil {
		g.generateConstructor(class)
	}
	// First, collect all methods from parent classes
	inheritedMethods := make(map[string]*ir.Func)
	currentClass := class.Name.Value
	for currentClass != "Object" {
		if parent, exists := g.classInheritance[currentClass]; exists {
			// Look for methods in parent class
			parentPrefix := parent + "_"
			for _, fn := range g.module.Funcs {
				if fnName := fn.Name(); strings.HasPrefix(fnName, parentPrefix) {
					methodName := strings.TrimPrefix(fnName, parentPrefix)
					// Only add if not already overridden
					if _, exists := inheritedMethods[methodName]; !exists {
						inheritedMethods[methodName] = fn
					}
				}
			}
			currentClass = parent
		} else {
			break
		}
	}

	// Generate this class's methods
	for _, feature := range class.Features {
		if method, ok := feature.(*ast.Method); ok {
			// Override inherited method if it exists
			delete(inheritedMethods, method.Name.Value)
			g.generateMethod(class.Name.Value, method)
		}
	}

	// Create forwarding methods for inherited methods that weren't overridden
	for methodName, inheritedMethod := range inheritedMethods {
		if methodName != "init" {
			g.generateForwardingMethod(class.Name.Value, methodName, inheritedMethod)
		}
	}
}

func (g *Generator) generateForwardingMethod(className, methodName string, inheritedMethod *ir.Func) {
	// Create a new method with the same signature but for the child class
	newMethodName := fmt.Sprintf("%s_%s", className, methodName)
	params := make([]*ir.Param, len(inheritedMethod.Params))
	paramTypes := make([]types.Type, len(inheritedMethod.Params))

	// Create parameters with same types
	for i, param := range inheritedMethod.Params {
		paramTypes[i] = param.Type()
		params[i] = ir.NewParam(param.Name(), param.Type())
	}

	// Create the forwarding method
	newMethod := g.module.NewFunc(newMethodName, inheritedMethod.Sig.RetType, params...)
	block := newMethod.NewBlock("")

	// Cast 'self' to parent type if needed
	self := params[0]
	var castSelf value.Value
	if self.Type() != inheritedMethod.Params[0].Type() {
		castSelf = block.NewBitCast(self, inheritedMethod.Params[0].Type())
	} else {
		castSelf = self
	}

	// Forward the call to parent method
	args := make([]value.Value, len(params))
	args[0] = castSelf
	for i := 1; i < len(params); i++ {
		args[i] = params[i]
	}

	result := block.NewCall(inheritedMethod, args...)
	block.NewRet(result)
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
			// Find the field index in the class
			fieldIdx, symbolExists := g.getSymbol(className, attr.Name.Value)
			if !symbolExists {
				fmt.Printf("Warning: Symbol %s not found in class %s\n", attr.Name.Value, className)
				continue
			}

			// Get pointer to the field
			fieldPtr := block.NewGetElementPtr(g.classes[className], self,
				constant.NewInt(types.I32, 0),
				fieldIdx)

			var initValue value.Value

			if attr.Init != nil {
				// Explicitly set current block, className, and func for proper context
				g.currentBlock = block
				g.currentClassName = className
				g.currentFunc = constructor

				// Generate the initialization expression
				initValue = g.generateExpression(attr.Init)
				fmt.Printf("Init value generated for %s: %v (type: %v)\n",
					attr.Name.Value, initValue, initValue.Type())

				if initValue == nil {
					fmt.Printf("Warning: Init expression for %s returned nil\n", attr.Name.Value)
					// Fall back to default initialization
					initValue = g.getDefaultValueForType(attr.Type.Value)
				}
			} else {
				// Default initialization
				initValue = g.getDefaultValueForType(attr.Type.Value)
			}

			// Handle type conversion if needed
			targetType := fieldPtr.Type().(*types.PointerType).ElemType
			if !initValue.Type().Equal(targetType) {
				if types.IsPointer(targetType) && types.IsPointer(initValue.Type()) {
					initValue = block.NewBitCast(initValue, targetType)
				} else if targetType.Equal(types.I32) && types.IsPointer(initValue.Type()) {
					initValue = block.NewPtrToInt(initValue, types.I32)
				} else if types.IsPointer(targetType) && initValue.Type().Equal(types.I32) {
					initValue = block.NewIntToPtr(initValue, targetType)
				}
			}

			// Store the value in the field
			block.NewStore(initValue, fieldPtr)
		}
	}

	block.NewRet(self)
}

// Helper method to get default values based on type
func (g *Generator) getDefaultValueForType(typeName string) value.Value {
	switch typeName {
	case "Int":
		return constant.NewInt(types.I32, 0)
	case "Bool":
		return constant.NewInt(types.I1, 0)
	case "String":
		return g.createStringConstant("")
	default:
		if classType, ok := g.classes[typeName]; ok {
			return constant.NewNull(types.NewPointer(classType))
		}
		return constant.NewNull(types.NewPointer(types.I8))
	}
}

func (g *Generator) generateMethod(className string, method *ast.Method) {
	g.currentClassName = className
	returnType := g.convertType(method.Type.Value)

	// Create function name
	funcName := className + "_" + method.Name.Value

	// Check if function already exists - THIS IS THE KEY FIX
	existingFunc := g.getFunction(funcName)
	var fn *ir.Func

	if existingFunc != nil {
		// Use the existing function
		fn = existingFunc
	} else {
		// Create a new function if it doesn't exist
		fn = g.module.NewFunc(funcName, returnType)

		// Add self parameter
		selfParam := ir.NewParam("self", types.NewPointer(g.classes[className]))
		fn.Params = append(fn.Params, selfParam)

		// Add other parameters
		for _, formal := range method.Formals {
			param := ir.NewParam(formal.Name.Value, g.convertType(formal.Type.Value))
			fn.Params = append(fn.Params, param)
		}
	}

	// Store current function
	g.currentFunc = fn

	// Only generate function body if the function doesn't have blocks yet
	if len(fn.Blocks) == 0 {
		// Generate function body
		block := fn.NewBlock("")
		g.currentBlock = block
		result := g.generateExpression(method.Expression)

		// Add return instruction if not already present
		if g.currentBlock.Term == nil {
			g.handleReturnValue(result, returnType)
		}
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
		return g.generateStringObject(e.Value)
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

	case *ast.Attribute:
		// If an attribute is used as an expression, we need to:
		// 1. Generate code for its initialization expression if it exists
		// 2. Or return a default value if no initialization
		if e.Init != nil {
			return g.generateExpression(e.Init)
		}

		// Default values based on type
		switch e.Type.Value {
		case "Int":
			return constant.NewInt(types.I32, 0)
		case "Bool":
			return constant.NewInt(types.I1, 0)
		case "String":
			return g.createStringConstant("")
		default:
			// For class types, return null pointer
			if classType, ok := g.classes[e.Type.Value]; ok {
				return constant.NewNull(types.NewPointer(classType))
			}
			return constant.NewNull(types.NewPointer(types.I8))
		}

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
		if receiver == nil {
			return constant.NewNull(types.NewPointer(types.I8))
		}

		// Determine class type of receiver
		var className string
		if types.IsPointer(receiver.Type()) {
			if ptrType, ok := receiver.Type().(*types.PointerType); ok {
				if structType, ok := ptrType.ElemType.(*types.StructType); ok {
					// Get class name from type
					className = g.getClassNameFromType(structType)
				} else if ptrType.ElemType.Equal(types.I8) {
					// Special case for string literals (pointer to I8)
					className = "String"
				}
			}
		} else if receiver.Type().Equal(types.I32) {
			// Special case for Int
			className = "Int"

			// Create a proper Int object
			intObj := g.currentBlock.NewCall(g.mallocFunc, constant.NewInt(types.I32, 16))
			typedIntObj := g.currentBlock.NewBitCast(intObj, types.NewPointer(g.classes["Int"]))

			// Set vtable pointer
			vtablePtr := g.currentBlock.NewGetElementPtr(g.classes["Int"], typedIntObj,
				constant.NewInt(types.I32, 0),
				constant.NewInt(types.I32, 0))
			typeStr := g.createStringConstant("Int")
			g.currentBlock.NewStore(g.currentBlock.NewBitCast(typeStr, types.NewPointer(types.I8)), vtablePtr)

			// Set int value
			valuePtr := g.currentBlock.NewGetElementPtr(g.classes["Int"], typedIntObj,
				constant.NewInt(types.I32, 0),
				constant.NewInt(types.I32, 1))
			g.currentBlock.NewStore(receiver, valuePtr)

			// Use the Int object as the receiver
			receiver = typedIntObj
		} else if receiver.Type().Equal(types.I1) {
			// Special case for Bool
			className = "Bool"

			// Create a proper Bool object
			boolObj := g.currentBlock.NewCall(g.mallocFunc, constant.NewInt(types.I32, 16))
			typedBoolObj := g.currentBlock.NewBitCast(boolObj, types.NewPointer(g.classes["Bool"]))

			// Set vtable pointer
			vtablePtr := g.currentBlock.NewGetElementPtr(g.classes["Bool"], typedBoolObj,
				constant.NewInt(types.I32, 0),
				constant.NewInt(types.I32, 0))
			typeStr := g.createStringConstant("Bool")
			g.currentBlock.NewStore(g.currentBlock.NewBitCast(typeStr, types.NewPointer(types.I8)), vtablePtr)

			// Set bool value
			valuePtr := g.currentBlock.NewGetElementPtr(g.classes["Bool"], typedBoolObj,
				constant.NewInt(types.I32, 0),
				constant.NewInt(types.I32, 1))
			g.currentBlock.NewStore(receiver, valuePtr)

			// Use the Bool object as the receiver
			receiver = typedBoolObj
		}

		// If we couldn't determine class name, try to get it from vtable
		if className == "" {
			fmt.Printf("Warning: Could not determine class name for receiver type %v\n", receiver.Type())
			className = "Object" // Default to Object if we can't determine the class
		}

		// Find the method in the class hierarchy
		methodName, found := g.findMethodInHierarchy(className, e.Method.Value)
		if !found {
			fmt.Printf("Error: Method %s not found in class %s or its ancestors\n",
				e.Method.Value, className)
			return constant.NewNull(types.NewPointer(types.I8))
		}

		// Look up the method
		method := g.getFunction(methodName)
		if method == nil {
			fmt.Printf("Error: Method %s was found but function not generated\n", methodName)
			return constant.NewNull(types.NewPointer(types.I8))
		}

		// Cast receiver to expected parameter type if needed
		if !receiver.Type().Equal(method.Params[0].Type()) {
			receiver = g.currentBlock.NewBitCast(receiver, method.Params[0].Type())
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

		// Increment counter for unique block names
		g.caseCounter++
		currentCase := g.caseCounter

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
		mergeBlock := g.currentFunc.NewBlock(fmt.Sprintf("case.merge.%d", currentCase))

		// Create PHI node for collecting results
		incomingValues := []value.Value{}
		incomingBlocks := []*ir.Block{}

		// Store current block for branching
		currentBlock := g.currentBlock

		// Generate branches
		var nextBlock *ir.Block
		for i, branch := range sortedBranches {
			caseBlock := g.currentFunc.NewBlock(fmt.Sprintf("case.%d.%d", currentCase, i))
			nextBlock = g.currentFunc.NewBlock(fmt.Sprintf("case.next.%d.%d", currentCase, i))

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
		abortBlock := g.currentFunc.NewBlock(fmt.Sprintf("case.abort.%d", currentCase))
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

	case *ast.UnaryExpression:
		return g.generateUnaryExpression(e)

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
				initValue = g.generateStringObject(strLit.Value)

			} else {
				initValue = g.generateExpression(binding.Init)
			}
		} else {
			// Default initialization
			switch binding.Type.Value {
			case "String":
				initValue = g.generateStringObject("")
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
		// Check if types are compatible, if not perform appropriate conversion
		if !initValue.Type().Equal(alloca.ElemType) {
			if types.IsPointer(alloca.ElemType) && types.IsPointer(initValue.Type()) {
				// If both are pointers but of different types, do a bitcast
				initValue = block.NewBitCast(initValue, alloca.ElemType)
			} else if types.IsPointer(alloca.ElemType) && !types.IsPointer(initValue.Type()) {
				// If destination is pointer but source is not, convert integer 0 to null
				if initValue.Type().Equal(types.I32) || initValue.Type().Equal(types.I1) {
					// Add type assertion to convert to *types.PointerType
					if ptrType, ok := alloca.ElemType.(*types.PointerType); ok {
						initValue = constant.NewNull(ptrType)
					}
				}
			}
		}
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

	// Calculate proper size for Main object
	mainType, ok := g.classes["Main"].(*types.StructType)
	if !ok {
		fmt.Println("Error: Main is not a struct type")
		return
	}

	// Count the number of fields in Main (including inherited)
	fieldCount := len(mainType.Fields)

	// Allocate 8 bytes per field (accounting for pointers and alignment)
	sizeInBytes := fieldCount * 8
	size := constant.NewInt(types.I32, int64(sizeInBytes))

	mainMalloc := block.NewCall(g.mallocFunc, size)
	mainInstance := block.NewBitCast(mainMalloc, types.NewPointer(g.classes["Main"]))

	// Initialize the Main instance by calling its constructor
	mainInit := g.getFunction("Main_init")
	if mainInit != nil {
		block.NewCall(mainInit, mainInstance)
	}

	// Call Main_main function with initialized instance
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
	strObj := ir.NewParam("x", types.NewPointer(g.classes["String"]))
	outString.Params = append(outString.Params, self, strObj)

	block := outString.NewBlock("")

	// Extract the actual string pointer from the String object
	strDataPtr := block.NewGetElementPtr(g.classes["String"], strObj,
		constant.NewInt(types.I32, 0),
		constant.NewInt(types.I32, 1))
	strPtr := block.NewLoad(types.NewPointer(types.I8), strDataPtr)

	// Call printf with the extracted string
	format := g.createStringConstant("%s")
	printf, _ := g.getSymbol("IO", "printf")
	block.NewCall(printf, format, strPtr)

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
	inString := g.module.NewFunc("IO_in_string", types.NewPointer(g.classes["String"]))
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

	// Create a new String object
	stringObj := block.NewCall(g.mallocFunc, constant.NewInt(types.I32, 16))
	typedStringObj := block.NewBitCast(stringObj, types.NewPointer(g.classes["String"]))

	// Set vtable pointer
	vtablePtr := block.NewGetElementPtr(g.classes["String"], typedStringObj,
		constant.NewInt(types.I32, 0),
		constant.NewInt(types.I32, 0))
	typeStr := g.createStringConstant("String")
	block.NewStore(block.NewBitCast(typeStr, types.NewPointer(types.I8)), vtablePtr)

	// Set string data pointer
	dataPtr := block.NewGetElementPtr(g.classes["String"], typedStringObj,
		constant.NewInt(types.I32, 0),
		constant.NewInt(types.I32, 1))
	block.NewStore(buffer, dataPtr)

	// Return the String object
	block.NewRet(typedStringObj)

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
	g.generateBoolClass()
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
	g.declareAllMethodsFirst(program)

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

// Add this method to your Generator struct
func (g *Generator) declareAllMethodsFirst(program *ast.Program) {
	// First declare all methods with empty bodies
	for _, class := range program.Classes {
		for _, feature := range class.Features {
			if method, ok := feature.(*ast.Method); ok {
				// Create function signature
				funcName := class.Name.Value + "_" + method.Name.Value
				returnType := g.convertType(method.Type.Value)

				// Create function with parameters but no body yet
				fn := g.module.NewFunc(funcName, returnType)

				// Add self parameter
				selfParam := ir.NewParam("self", types.NewPointer(g.classes[class.Name.Value]))
				fn.Params = append(fn.Params, selfParam)

				// Add other parameters
				for _, formal := range method.Formals {
					param := ir.NewParam(formal.Name.Value, g.convertType(formal.Type.Value))
					fn.Params = append(fn.Params, param)
				}
			}
		}
	}
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

	// Ensure both values have the same type for the PHI node
	if thenValue != nil && elseValue != nil {
		// If one branch returns a pointer and the other returns an integer 0,
		// convert the integer 0 to a null pointer of the appropriate type
		if types.IsPointer(thenValue.Type()) &&
			(elseValue.Type().Equal(types.I32) || elseValue.Type().Equal(types.I1)) {
			// Convert integer 0 to null pointer of the same type as thenValue
			if ptrType, ok := thenValue.Type().(*types.PointerType); ok {
				elseValue = constant.NewNull(ptrType)
			}
		} else if types.IsPointer(elseValue.Type()) &&
			(thenValue.Type().Equal(types.I32) || thenValue.Type().Equal(types.I1)) {
			// Convert integer 0 to null pointer of the same type as elseValue
			if ptrType, ok := elseValue.Type().(*types.PointerType); ok {
				thenValue = constant.NewNull(ptrType)
			}
		}
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
	if _, ok := g.classes["String"]; !ok {
		// Create String class type with vtable pointer and string data
		stringType := types.NewStruct(
			types.NewPointer(types.I8), // vtable pointer
			types.NewPointer(types.I8), // actual string data
		)
		g.classes["String"] = stringType
	}
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
	callInst.Tail = enum.TailTail
	block.NewUnreachable()

	typeNameFunc := g.module.NewFunc("Object_type_name", types.NewPointer(g.classes["String"]))
	self = ir.NewParam("self", types.NewPointer(g.classes["Object"]))
	typeNameFunc.Params = append(typeNameFunc.Params, self)
	block = typeNameFunc.NewBlock("")

	// Get the vtable pointer (first field)
	vtablePtr := block.NewGetElementPtr(g.classes["Object"], self,
		constant.NewInt(types.I32, 0),
		constant.NewInt(types.I32, 0))

	// Load the string pointer from vtable
	typeNameStr := block.NewLoad(types.NewPointer(types.I8), vtablePtr)

	// Create a new String object directly
	stringObj := block.NewCall(g.mallocFunc, constant.NewInt(types.I32, 16))
	typedStringObj := block.NewBitCast(stringObj, types.NewPointer(g.classes["String"]))

	// Set vtable pointer
	vtableStrPtr := block.NewGetElementPtr(g.classes["String"], typedStringObj,
		constant.NewInt(types.I32, 0),
		constant.NewInt(types.I32, 0))
	typeStr := g.createStringConstant("String")
	block.NewStore(block.NewBitCast(typeStr, types.NewPointer(types.I8)), vtableStrPtr)

	// Set string data pointer
	dataPtr := block.NewGetElementPtr(g.classes["String"], typedStringObj,
		constant.NewInt(types.I32, 0),
		constant.NewInt(types.I32, 1))
	block.NewStore(typeNameStr, dataPtr)

	// Return the String object
	block.NewRet(typedStringObj)

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

func (g *Generator) generateBoolClass() {
	// Create Bool class type with just a value field and vtable pointer
	boolType := types.NewStruct(
		types.NewPointer(types.I8), // vtable pointer
		types.I1,                   // value field
	)
	g.classes["Bool"] = boolType

	// Store that Bool is a basic class that can't be inherited from
	g.classInheritance["Bool"] = "Object"

	// Generate constructor for Bool
	boolInit := g.module.NewFunc("Bool_init", types.NewPointer(g.classes["Bool"]))
	self := ir.NewParam("self", types.NewPointer(g.classes["Bool"]))
	boolInit.Params = append(boolInit.Params, self)
	block := boolInit.NewBlock("")

	// Initialize value to false (0)
	valuePtr := block.NewGetElementPtr(g.classes["Bool"], self,
		constant.NewInt(types.I32, 0),
		constant.NewInt(types.I32, 1)) // index 1 is the value field
	block.NewStore(constant.NewInt(types.I1, 0), valuePtr)

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

	// Store field names for String
	g.classFields["String"] = []string{"vtable", "data"}

	// Initialize symbol table for String class
	g.initializeSymbolTable("String")
	g.addSymbol("String", "data", constant.NewInt(types.I32, 1)) // Index of data field

	// Generate constructor for String
	stringInit := g.module.NewFunc("String_init", types.NewPointer(g.classes["String"]))
	self := ir.NewParam("self", types.NewPointer(g.classes["String"]))
	stringInit.Params = append(stringInit.Params, self)
	block := stringInit.NewBlock("")

	// Initialize vtable pointer with String type
	vtablePtr := block.NewGetElementPtr(g.classes["String"], self,
		constant.NewInt(types.I32, 0),
		constant.NewInt(types.I32, 0))
	typeStr := g.createStringConstant("String")
	block.NewStore(block.NewBitCast(typeStr, types.NewPointer(types.I8)), vtablePtr)

	// Initialize data to empty string
	emptyStr := g.createStringConstant("")
	dataPtr := block.NewGetElementPtr(g.classes["String"], self,
		constant.NewInt(types.I32, 0),
		constant.NewInt(types.I32, 1))
	block.NewStore(block.NewBitCast(emptyStr, types.NewPointer(types.I8)), dataPtr)

	block.NewRet(self)

	// Generate length method
	g.generateStringLength()

	// Generate concat method
	g.generateStringConcat()

	// Generate substr method (optional implementation)
	g.generateStringSubstr()
}

func (g *Generator) generateStringObject(value string) value.Value {
	// Create the string constant first (raw C string)
	strConst := g.createStringConstant(value)

	// Allocate memory for String object (vtable ptr + string data ptr)
	stringObj := g.currentBlock.NewCall(g.mallocFunc, constant.NewInt(types.I32, 16))
	typedStringObj := g.currentBlock.NewBitCast(stringObj, types.NewPointer(g.classes["String"]))

	// Set vtable pointer to indicate it's a String
	vtablePtr := g.currentBlock.NewGetElementPtr(g.classes["String"], typedStringObj,
		constant.NewInt(types.I32, 0),
		constant.NewInt(types.I32, 0))
	typeStr := g.createStringConstant("String")
	g.currentBlock.NewStore(g.currentBlock.NewBitCast(typeStr, types.NewPointer(types.I8)), vtablePtr)

	// Set string data pointer
	dataPtr := g.currentBlock.NewGetElementPtr(g.classes["String"], typedStringObj,
		constant.NewInt(types.I32, 0),
		constant.NewInt(types.I32, 1))
	g.currentBlock.NewStore(g.currentBlock.NewBitCast(strConst, types.NewPointer(types.I8)), dataPtr)

	return typedStringObj
}

func (g *Generator) generateStringLength() {
	lengthFunc := g.module.NewFunc("String_length", types.I32)
	self := ir.NewParam("self", types.NewPointer(g.classes["String"]))
	lengthFunc.Params = append(lengthFunc.Params, self)

	block := lengthFunc.NewBlock("")

	// Get string data pointer from self
	strDataPtr := block.NewGetElementPtr(g.classes["String"], self,
		constant.NewInt(types.I32, 0),
		constant.NewInt(types.I32, 1))
	strPtr := block.NewLoad(types.NewPointer(types.I8), strDataPtr)

	// Call strlen
	strlenFunc := g.getFunction("strlen")
	if strlenFunc == nil {
		// If strlen function isn't found, create it
		strlenFunc = g.module.NewFunc("strlen", types.I32,
			ir.NewParam("str", types.NewPointer(types.I8)))
		strlenFunc.Linkage = enum.LinkageExternal
	}

	length := block.NewCall(strlenFunc, strPtr)
	block.NewRet(length)
}

func (g *Generator) generateStringConcat() {
	concatFunc := g.module.NewFunc("String_concat", types.NewPointer(g.classes["String"]))
	self := ir.NewParam("self", types.NewPointer(g.classes["String"]))
	other := ir.NewParam("s", types.NewPointer(g.classes["String"]))
	concatFunc.Params = append(concatFunc.Params, self, other)

	block := concatFunc.NewBlock("")

	// Get string data from self
	selfDataPtr := block.NewGetElementPtr(g.classes["String"], self,
		constant.NewInt(types.I32, 0),
		constant.NewInt(types.I32, 1))
	selfStr := block.NewLoad(types.NewPointer(types.I8), selfDataPtr)

	// Get string data from other
	otherDataPtr := block.NewGetElementPtr(g.classes["String"], other,
		constant.NewInt(types.I32, 0),
		constant.NewInt(types.I32, 1))
	otherStr := block.NewLoad(types.NewPointer(types.I8), otherDataPtr)

	// Get lengths
	strlenFunc := g.getFunction("strlen")
	selfLen := block.NewCall(strlenFunc, selfStr)
	otherLen := block.NewCall(strlenFunc, otherStr)

	// Calculate total length (+1 for null terminator)
	totalLen := block.NewAdd(selfLen, otherLen)
	allocSize := block.NewAdd(totalLen, constant.NewInt(types.I32, 1))

	// Allocate buffer for concatenated string
	buffer := block.NewCall(g.mallocFunc, allocSize)

	// Copy first string
	strcpyFunc := g.getFunction("strcpy")
	if strcpyFunc == nil {
		strcpyFunc = g.module.NewFunc("strcpy", types.NewPointer(types.I8),
			ir.NewParam("dest", types.NewPointer(types.I8)),
			ir.NewParam("src", types.NewPointer(types.I8)))
		strcpyFunc.Linkage = enum.LinkageExternal
	}
	block.NewCall(strcpyFunc, buffer, selfStr)

	// Concatenate second string
	strcatFunc := g.getFunction("strcat")
	if strcatFunc == nil {
		strcatFunc = g.module.NewFunc("strcat", types.NewPointer(types.I8),
			ir.NewParam("dest", types.NewPointer(types.I8)),
			ir.NewParam("src", types.NewPointer(types.I8)))
		strcatFunc.Linkage = enum.LinkageExternal
	}
	block.NewCall(strcatFunc, buffer, otherStr)

	// Create new String object
	newStringObj := block.NewCall(g.mallocFunc, constant.NewInt(types.I32, 16))
	typedStringObj := block.NewBitCast(newStringObj, types.NewPointer(g.classes["String"]))

	// Set vtable pointer
	vtablePtr := block.NewGetElementPtr(g.classes["String"], typedStringObj,
		constant.NewInt(types.I32, 0),
		constant.NewInt(types.I32, 0))
	typeStr := g.createStringConstant("String")
	block.NewStore(block.NewBitCast(typeStr, types.NewPointer(types.I8)), vtablePtr)

	// Set string data pointer
	dataPtr := block.NewGetElementPtr(g.classes["String"], typedStringObj,
		constant.NewInt(types.I32, 0),
		constant.NewInt(types.I32, 1))
	block.NewStore(buffer, dataPtr)

	block.NewRet(typedStringObj)
}

func (g *Generator) generateStringSubstr() {
	// Implementation for substring method
	substrFunc := g.module.NewFunc("String_substr", types.NewPointer(g.classes["String"]))
	self := ir.NewParam("self", types.NewPointer(g.classes["String"]))
	i := ir.NewParam("i", types.I32)
	l := ir.NewParam("l", types.I32)
	substrFunc.Params = append(substrFunc.Params, self, i, l)

	block := substrFunc.NewBlock("")

	// Implement basic substring functionality
	// You may want to add bounds checking in a complete implementation

	// Get string data
	dataPtr := block.NewGetElementPtr(g.classes["String"], self,
		constant.NewInt(types.I32, 0),
		constant.NewInt(types.I32, 1))
	strPtr := block.NewLoad(types.NewPointer(types.I8), dataPtr)

	// Calculate position
	pos := block.NewGetElementPtr(types.I8, strPtr, i)

	// Allocate buffer for result (length + 1 for null terminator)
	buffer := block.NewCall(g.mallocFunc, block.NewAdd(l, constant.NewInt(types.I32, 1)))

	// Copy substring using strncpy
	strncpyFunc := g.getFunction("strncpy")
	if strncpyFunc == nil {
		strncpyFunc = g.module.NewFunc("strncpy", types.NewPointer(types.I8),
			ir.NewParam("dest", types.NewPointer(types.I8)),
			ir.NewParam("src", types.NewPointer(types.I8)),
			ir.NewParam("n", types.I32))
		strncpyFunc.Linkage = enum.LinkageExternal
	}
	block.NewCall(strncpyFunc, buffer, pos, l)

	// Ensure null termination
	endPtr := block.NewGetElementPtr(types.I8, buffer, l)
	block.NewStore(constant.NewInt(types.I8, 0), endPtr)

	// Create new String object
	newStringObj := block.NewCall(g.mallocFunc, constant.NewInt(types.I32, 16))
	typedStringObj := block.NewBitCast(newStringObj, types.NewPointer(g.classes["String"]))

	// Set vtable pointer
	vtablePtr := block.NewGetElementPtr(g.classes["String"], typedStringObj,
		constant.NewInt(types.I32, 0),
		constant.NewInt(types.I32, 0))
	typeStr := g.createStringConstant("String")
	block.NewStore(block.NewBitCast(typeStr, types.NewPointer(types.I8)), vtablePtr)

	// Set string data pointer
	newDataPtr := block.NewGetElementPtr(g.classes["String"], typedStringObj,
		constant.NewInt(types.I32, 0),
		constant.NewInt(types.I32, 1))
	block.NewStore(buffer, newDataPtr)

	block.NewRet(typedStringObj)
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

func (g *Generator) generateUnaryExpression(expr *ast.UnaryExpression) value.Value {
	operand := g.generateExpression(expr.Right)
	if operand == nil {
		return constant.NewInt(types.I32, 0)
	}

	switch expr.Operator {
	case "not":
		// Handle boolean not
		if operand.Type().Equal(types.I1) {
			return g.currentBlock.NewXor(operand, constant.NewInt(types.I1, 1))
		}
		// Convert non-boolean to boolean first
		cmp := g.currentBlock.NewICmp(enum.IPredNE, operand, constant.NewInt(types.I32, 0))
		return g.currentBlock.NewXor(cmp, constant.NewInt(types.I1, 1))

	case "~":
		// Handle numeric negation
		if types.IsPointer(operand.Type()) {
			// Convert pointer to integer first
			operand = g.currentBlock.NewPtrToInt(operand, types.I32)
		}
		return g.currentBlock.NewSub(constant.NewInt(types.I32, 0), operand)

	case "isvoid":
		// Handle isvoid operator
		return g.handleIsVoidExpression(&ast.IsVoidExpression{Expression: expr.Right})

	default:
		fmt.Printf("Warning: Unknown unary operator %s\n", expr.Operator)
		return constant.NewInt(types.I32, 0)
	}
}
