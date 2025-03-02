package semant

import (
	"cool-compiler/ast"
	"cool-compiler/lexer"
	"fmt"
)

type SymbolTable struct {
	symbols map[string]*SymbolEntry
	parent  *SymbolTable
}

type SymbolEntry struct {
	Type     string
	Token    lexer.Token
	AttrType *ast.TypeIdentifier
	Method   *ast.Method
	Scope    *SymbolTable
}

func NewSymbolTable(parent *SymbolTable) *SymbolTable {
	return &SymbolTable{
		symbols: make(map[string]*SymbolEntry),
		parent:  parent,
	}
}

func (st *SymbolTable) AddEntry(name string, entry *SymbolEntry) {
	st.symbols[name] = entry
}

func (st *SymbolTable) Lookup(name string) (*SymbolEntry, bool) {
	entry, ok := st.symbols[name]
	if !ok && st.parent != nil {
		return st.parent.Lookup(name)
	}
	return entry, ok
}

type SemanticAnalyser struct {
	globalSymbolTable *SymbolTable
	errors            []string

	inheritanceGraph map[string]string // maps class name to parent class name
}

func NewSemanticAnalyser() *SemanticAnalyser {
	return &SemanticAnalyser{
		globalSymbolTable: NewSymbolTable(nil),
		errors:            []string{},

		inheritanceGraph: make(map[string]string),
	}
}

func (sa *SemanticAnalyser) Errors() []string {
	return sa.errors
}

func (sa *SemanticAnalyser) Analyze(program *ast.Program) {
	// Check for Main class
	foundMain := false
	var mainClass *ast.Class
	for _, class := range program.Classes {
		if class.Name.Value == "Main" {
			foundMain = true
			mainClass = class
			break
		}
	}

	if !foundMain {
		sa.errors = append(sa.errors, "Program does not contain a Main class")
		return // Early return since this is a critical error
	}

	// Check for main method in Main class
	foundMainMethod := false
	for _, feature := range mainClass.Features {
		if method, ok := feature.(*ast.Method); ok {
			if method.Name.Value == "main" {
				foundMainMethod = true
				break
			}
		}
	}

	if !foundMainMethod {
		sa.errors = append(sa.errors, "Main class does not contain a main method")
	}

	sa.buildClassesSymboltables(program)
	sa.buildInheritanceGraph(program)
	sa.buildSymboltables(program)
	sa.typeCheck(program)
}

func (sa *SemanticAnalyser) typeCheck(program *ast.Program) {
	for _, class := range program.Classes {
		st := sa.globalSymbolTable.symbols[class.Name.Value].Scope
		sa.typeCheckClass(class, st)
	}
}

func (sa *SemanticAnalyser) typeCheckClass(cls *ast.Class, st *SymbolTable) {
	for _, feature := range cls.Features {
		switch f := feature.(type) {
		case *ast.Attribute:
			sa.typeCheckAttribute(f, st)
		case *ast.Method:
			sa.typeCheckMethod(f, st)
		}
	}
}

func (sa *SemanticAnalyser) typeCheckAttribute(attribute *ast.Attribute, st *SymbolTable) {
	if attribute.Init != nil {
		expressionType := sa.getExpressionType(attribute.Init, st)
		if !sa.isTypeConformant(expressionType, attribute.Type.Value) {
			sa.errors = append(sa.errors, fmt.Sprintf("attribute %s cannot be of type %s, expected %s",
				attribute.Name.Value, expressionType, attribute.Type.Value))
		}
	}
}
func (sa *SemanticAnalyser) typeCheckMethod(method *ast.Method, st *SymbolTable) {
	methodSt := st.symbols[method.Name.Value].Scope

	// Create new scope for method parameters and body
	paramScope := NewSymbolTable(methodSt)

	// Check formals in the new parameter scope
	for _, formal := range method.Formals {
		if _, ok := paramScope.Lookup(formal.Name.Value); ok {
			sa.errors = append(sa.errors, fmt.Sprintf("argument %s in method %s is already defined",
				formal.Name.Value, method.Name.Value))
			continue
		}

		// Add formal to parameter scope instead of parent scope
		paramScope.AddEntry(formal.Name.Value, &SymbolEntry{
			Token: formal.Token,
			Type:  formal.TypeDecl.Value,
		})
	}

	// Check if method has an expression
	if method.Expression != nil {
		// Get the declared return type from the method signature
		declaredReturnType := method.TypeDecl.Value
		if declaredReturnType == "" {
			fmt.Printf("WARNING: Method %s has empty return type, defaulting to Object\n", method.Name.Value)
			declaredReturnType = "Object"

			// Fix the return type in the AST
			method.TypeDecl.Value = declaredReturnType
		}

		// Get the actual type of the method body expression
		methodExpressionType := sa.getExpressionType(method.Expression, paramScope)

		// Special case for SELF_TYPE
		if declaredReturnType == "SELF_TYPE" {
			// Find the containing class
			currentTable := st
			for currentTable != nil && currentTable.parent != sa.globalSymbolTable {
				currentTable = currentTable.parent
			}

			// Find which class this symbol table belongs to
			var className string
			if currentTable != nil && currentTable.parent == sa.globalSymbolTable {
				for name, entry := range sa.globalSymbolTable.symbols {
					if entry.Scope == currentTable {
						className = name
						break
					}
				}
			}

			// When return type is SELF_TYPE, expression's type should be the same as the class
			// it's defined in or a subclass of it
			if className != "" && !sa.isTypeConformant(methodExpressionType, className) {
				sa.errors = append(sa.errors, fmt.Sprintf("method %s is expected to return %s (SELF_TYPE), found %s",
					method.Name.Value, className, methodExpressionType))
			}
		} else {
			// Normal case: Check that the expression's type conforms to the declared return type
			if !sa.isTypeConformant(methodExpressionType, declaredReturnType) {
				sa.errors = append(sa.errors, fmt.Sprintf("method %s is expected to return %s, found %s",
					method.Name.Value, declaredReturnType, methodExpressionType))
			}
		}
	}
}

func (sa *SemanticAnalyser) isTypeConformant(type1, type2 string) bool {

	// A type always conforms to itself
	if type1 == type2 {
		return true
	}

	// Special case: everything conforms to Object
	if type2 == "Object" {
		return true
	}

	// Object does NOT conform to arbitrary types
	if type1 == "Object" && type2 != "Object" {
		return false
	}

	// Walk up the inheritance tree from type1
	current := type1
	for current != "Object" {
		parent, exists := sa.inheritanceGraph[current]
		if !exists {
			return false
		}
		if parent == type2 {
			return true
		}
		current = parent
	}

	return false
}
func (sa *SemanticAnalyser) getExpressionType(expression ast.Expression, st *SymbolTable) string {

	switch e := expression.(type) {
	case *ast.IntegerLiteral:
		return "Int"
	case *ast.StringLiteral:
		return "String"
	case *ast.BooleanLiteral:
		return "Bool"
	case *ast.BlockExpression:
		return sa.getBlockExpressionType(e, st)
	case *ast.IfExpression:
		return sa.getIfExpressionType(e, st)
	case *ast.WhileExpression:
		return sa.getWhileExpressionType(e, st)
	case *ast.NewExpression:
		return sa.GetNewExpressionType(e, st)
	case *ast.LetExpression:
		return sa.GetLetExpressionType(e, st)
	case *ast.Assignment:
		return sa.GetAssignmentExpressionType(e, st)
	case *ast.UnaryExpression:
		return sa.GetUnaryExpressionType(e, st)
	case *ast.BinaryExpression:
		return sa.GetBinaryExpressionType(e, st)
	case *ast.CaseExpression:
		return sa.GetCaseExpressionType(e, st)
	case *ast.StaticDispatchExpression:
		return sa.GetStaticDispatchExpressionType(e, st)
	case *ast.DispatchExpression:
		return sa.GetDispatchExpressionType(e, st)
	case *ast.CallExpression:
		return sa.GetCallExpressionType(e, st)
	case *ast.IsVoidExpression:
		return "Bool"
	case *ast.ObjectIdentifier:
		// Special handling for 'self' identifier
		if e.Value == "self" {
			// Find the containing class by walking up the symbol table
			currentTable := st
			for currentTable != nil && currentTable.parent != sa.globalSymbolTable {
				currentTable = currentTable.parent
			}

			// If we found a class scope
			if currentTable != nil && currentTable.parent == sa.globalSymbolTable {
				// Find which class this symbol table belongs to
				for className, entry := range sa.globalSymbolTable.symbols {
					if entry.Scope == currentTable {
						return className
					}
				}
			}
			return "Object" // Default if we can't determine the class
		}

		// Look up the identifier in scope
		if entry, ok := st.Lookup(e.Value); ok {
			// If it's an attribute, get its type
			if entry.AttrType != nil {
				return entry.AttrType.Value
			}
			// Otherwise return the entry's type
			return entry.Type
		}
		sa.errors = append(sa.errors, fmt.Sprintf("undefined identifier: %s", e.Value))
		return "Object"
	default:
		return "Object"
	}
}

func (sa *SemanticAnalyser) getWhileExpressionType(wexpr *ast.WhileExpression, st *SymbolTable) string {
	conditionType := sa.getExpressionType(wexpr.Condition, st)
	if conditionType != "Bool" {
		sa.errors = append(sa.errors, fmt.Sprintf("condition of if statement is of type %s, expected Bool", conditionType))
		return "Object"
	}

	return sa.getExpressionType(wexpr.Body, st)
}

func (sa *SemanticAnalyser) getBlockExpressionType(bexpr *ast.BlockExpression, st *SymbolTable) string {
	lastType := ""
	for _, expression := range bexpr.Expressions {
		lastType = sa.getExpressionType(expression, st)
	}

	return lastType
}

func (sa *SemanticAnalyser) getIfExpressionType(ifexpr *ast.IfExpression, st *SymbolTable) string {
	conditionType := sa.getExpressionType(ifexpr.Condition, st)
	if conditionType != "Bool" {
		sa.errors = append(sa.errors, fmt.Sprintf("condition of if statement is of type %s, expected Bool", conditionType))
	}

	consType := sa.getExpressionType(ifexpr.Consequence, st)
	altType := sa.getExpressionType(ifexpr.Alternative, st)

	// If the types are the same, return that type
	if consType == altType {
		return consType
	}

	// Otherwise, find the least common ancestor
	return sa.findLCA(consType, altType)
}

func (sa *SemanticAnalyser) buildClassesSymboltables(program *ast.Program) {
	// Create Object class with its methods
	objectClassST := NewSymbolTable(sa.globalSymbolTable)
	sa.globalSymbolTable.AddEntry("Object", &SymbolEntry{
		Type:  "Class",
		Token: lexer.Token{Literal: "Object"},
		Scope: objectClassST,
	})

	// Add fundamental Object methods
	objectClassST.AddEntry("abort", &SymbolEntry{
		Type:  "Method",
		Token: lexer.Token{Literal: "abort"},
		Method: &ast.Method{
			Name:     &ast.ObjectIdentifier{Value: "abort"},
			Formals:  []*ast.Formal{}, // no parameters
			TypeDecl: ast.TypeIdentifier{Value: "Object"},
		},
	})

	objectClassST.AddEntry("type_name", &SymbolEntry{
		Type:  "Method",
		Token: lexer.Token{Literal: "type_name"},
		Method: &ast.Method{
			Name:     &ast.ObjectIdentifier{Value: "type_name"},
			Formals:  []*ast.Formal{}, // no parameters
			TypeDecl: ast.TypeIdentifier{Value: "String"},
		},
	})

	objectClassST.AddEntry("copy", &SymbolEntry{
		Type:  "Method",
		Token: lexer.Token{Literal: "copy"},
		Method: &ast.Method{
			Name:     &ast.ObjectIdentifier{Value: "copy"},
			Formals:  []*ast.Formal{}, // no parameters
			TypeDecl: ast.TypeIdentifier{Value: "SELF_TYPE"},
		},
	})

	// Create String class with its methods
	stringClassST := NewSymbolTable(sa.globalSymbolTable)
	sa.globalSymbolTable.AddEntry("String", &SymbolEntry{
		Type:  "Class",
		Token: lexer.Token{Literal: "String"},
		Scope: stringClassST,
	})

	// Add String methods
	stringClassST.AddEntry("length", &SymbolEntry{
		Type:  "Method",
		Token: lexer.Token{Literal: "length"},
		Method: &ast.Method{
			Name:     &ast.ObjectIdentifier{Value: "length"},
			Formals:  []*ast.Formal{}, // no parameters
			TypeDecl: ast.TypeIdentifier{Value: "Int"},
		},
	})

	stringClassST.AddEntry("concat", &SymbolEntry{
		Type:  "Method",
		Token: lexer.Token{Literal: "concat"},
		Method: &ast.Method{
			Name: &ast.ObjectIdentifier{Value: "concat"},
			Formals: []*ast.Formal{
				{
					Name:     &ast.ObjectIdentifier{Value: "s"},
					TypeDecl: ast.TypeIdentifier{Value: "String"},
				},
			},
			TypeDecl: ast.TypeIdentifier{Value: "String"},
		},
	})

	stringClassST.AddEntry("substr", &SymbolEntry{
		Type:  "Method",
		Token: lexer.Token{Literal: "substr"},
		Method: &ast.Method{
			Name: &ast.ObjectIdentifier{Value: "substr"},
			Formals: []*ast.Formal{
				{
					Name:     &ast.ObjectIdentifier{Value: "i"},
					TypeDecl: ast.TypeIdentifier{Value: "Int"},
				},
				{
					Name:     &ast.ObjectIdentifier{Value: "l"},
					TypeDecl: ast.TypeIdentifier{Value: "Int"},
				},
			},
			TypeDecl: ast.TypeIdentifier{Value: "String"},
		},
	})

	sa.globalSymbolTable.AddEntry("Int", &SymbolEntry{Type: "Class", Token: lexer.Token{Literal: "Int"}})
	sa.globalSymbolTable.AddEntry("Bool", &SymbolEntry{Type: "Class", Token: lexer.Token{Literal: "Bool"}})

	// Add IO class
	ioClassST := NewSymbolTable(sa.globalSymbolTable)
	sa.globalSymbolTable.AddEntry("IO", &SymbolEntry{
		Type:  "Class",
		Token: lexer.Token{Literal: "IO"},
		Scope: ioClassST,
	})

	// Add IO methods with proper Method structures
	ioClassST.AddEntry("out_string", &SymbolEntry{
		Type:  "Method",
		Token: lexer.Token{Literal: "out_string"},
		Method: &ast.Method{
			Name: &ast.ObjectIdentifier{Value: "out_string"},
			Formals: []*ast.Formal{
				{
					Name:     &ast.ObjectIdentifier{Value: "x"},
					TypeDecl: ast.TypeIdentifier{Value: "String"},
				},
			},
			TypeDecl: ast.TypeIdentifier{Value: "SELF_TYPE"},
		},
	})

	ioClassST.AddEntry("out_int", &SymbolEntry{
		Type:  "Method",
		Token: lexer.Token{Literal: "out_int"},
		Method: &ast.Method{
			Name: &ast.ObjectIdentifier{Value: "out_int"},
			Formals: []*ast.Formal{
				{
					Name:     &ast.ObjectIdentifier{Value: "x"},
					TypeDecl: ast.TypeIdentifier{Value: "Int"},
				},
			},
			TypeDecl: ast.TypeIdentifier{Value: "SELF_TYPE"},
		},
	})

	ioClassST.AddEntry("in_string", &SymbolEntry{
		Type:  "Method",
		Token: lexer.Token{Literal: "in_string"},
		Method: &ast.Method{
			Name:     &ast.ObjectIdentifier{Value: "in_string"},
			Formals:  []*ast.Formal{},
			TypeDecl: ast.TypeIdentifier{Value: "String"},
		},
	})

	ioClassST.AddEntry("in_int", &SymbolEntry{
		Type:  "Method",
		Token: lexer.Token{Literal: "in_int"},
		Method: &ast.Method{
			Name:     &ast.ObjectIdentifier{Value: "in_int"},
			Formals:  []*ast.Formal{},
			TypeDecl: ast.TypeIdentifier{Value: "Int"},
		},
	})

	for _, class := range program.Classes {
		if _, ok := sa.globalSymbolTable.Lookup(class.Name.Value); ok {
			sa.errors = append(sa.errors, fmt.Sprintf("class %s is already defined", class.Name.Value))
			continue
		}

		// Create a scope for this class
		classScope := NewSymbolTable(sa.globalSymbolTable)
		sa.globalSymbolTable.AddEntry(class.Name.Value, &SymbolEntry{
			Type:  "Class",
			Token: class.Name.Token,
			Scope: classScope,
		})
	}
}

func (sa *SemanticAnalyser) buildSymboltables(program *ast.Program) {
	for _, class := range program.Classes {
		classEntry, _ := sa.globalSymbolTable.Lookup(class.Name.Value)
		// classEntry.Scope = NewSymbolTable(sa.globalSymbolTable)

		for _, feature := range class.Features {
			switch f := feature.(type) {
			case *ast.Attribute:
				// Verify the attribute type is a valid type
				if _, ok := sa.globalSymbolTable.Lookup(f.Type.Value); !ok {
					sa.errors = append(sa.errors, fmt.Sprintf("undefined type %s for attribute %s in class %s",
						f.Type.Value, f.Name.Value, class.Name.Value))
					continue
				}
				if _, ok := classEntry.Scope.Lookup(f.Name.Value); ok {
					sa.errors = append(sa.errors, fmt.Sprintf("attribute %s is already defined in class %s", f.Name.Value, class.Name.Value))
					continue
				}
				// Fix: Use f.Type directly instead of taking its address
				classEntry.Scope.AddEntry(f.Name.Value, &SymbolEntry{Token: f.Name.Token, AttrType: f.Type})
			case *ast.Method:
				if f.Type != nil && f.TypeDecl.Value == "" {
					f.TypeDecl.Value = f.Type.Value
					f.TypeDecl.Token = f.Type.Token
				}

				methodST := NewSymbolTable(classEntry.Scope)
				classEntry.Scope.AddEntry(f.Name.Value, &SymbolEntry{
					Token:  f.Name.Token,
					Scope:  methodST,
					Method: f,
				})
			}
		}
	}
}
func (sa *SemanticAnalyser) GetNewExpressionType(ne *ast.NewExpression, st *SymbolTable) string {
	// Handle SELF_TYPE
	if ne.Type.Value == "SELF_TYPE" {
		// Find the containing class by walking up the symbol table
		currentTable := st
		for currentTable != nil && currentTable.parent != sa.globalSymbolTable {
			currentTable = currentTable.parent
		}

		// If we found a class scope
		if currentTable != nil && currentTable.parent == sa.globalSymbolTable {
			// Find which class this symbol table belongs to
			for className, entry := range sa.globalSymbolTable.symbols {
				if entry.Scope == currentTable {
					return className
				}
			}
		}

		// If we couldn't determine the class, default to Object
		sa.errors = append(sa.errors, "could not determine containing class for SELF_TYPE")
		return "Object"
	}

	// Handle regular type - first check if it exists in the symbol table
	if _, ok := sa.globalSymbolTable.Lookup(ne.Type.Value); !ok {
		sa.errors = append(sa.errors, fmt.Sprintf("undefined type %s in new expression", ne.Type.Value))
		return "Object"
	}

	// Return the actual type, not Object
	return ne.Type.Value
}

//	func (sa *SemanticAnalyser) GetLetExpressionType(le *ast.LetExpression, st *SymbolTable) string {
//		for _, binding := range le.Bindings {
//			exprType := sa.getExpressionType(binding.Init, st)
//			if exprType != binding.Type.Value {
//				sa.errors = append(sa.errors, fmt.Sprintf("Let binding with wrong type %s, expected %s", exprType, binding.Type.Value))
//			}
//		}
//		return sa.getExpressionType(le.In, st)
//	}
func (sa *SemanticAnalyser) GetLetExpressionType(le *ast.LetExpression, st *SymbolTable) string {
	// Create a new scope for let bindings
	letScope := NewSymbolTable(st)

	// Process each binding
	for _, binding := range le.Bindings {
		// Check if type exists
		if binding.Type == nil || binding.Type.Value == "" {
			sa.errors = append(sa.errors, "missing type declaration in let binding")
			continue
		}

		// Verify the declared type exists
		declaredType := binding.Type.Value
		if _, ok := sa.globalSymbolTable.Lookup(declaredType); !ok {
			sa.errors = append(sa.errors, fmt.Sprintf("undefined type %s in let binding", declaredType))
			declaredType = "Object" // Fall back to Object if type doesn't exist
		}

		// If there's an initialization expression, check type conformance
		if binding.Init != nil {
			initType := sa.getExpressionType(binding.Init, letScope)

			// Check for type conformance - init type must conform to declared type
			if !sa.isTypeConformant(initType, declaredType) {
				sa.errors = append(sa.errors, fmt.Sprintf(
					"type mismatch in let binding '%s': initialization expression of type %s does not conform to declared type %s",
					binding.Identifier.Value, initType, declaredType))
			}
		}

		// Always add binding with the declared type (left of colon), never the expression type
		letScope.AddEntry(binding.Identifier.Value, &SymbolEntry{
			Type:  declaredType,
			Token: binding.Identifier.Token,
		})
	}

	// Type check the body expression in the new scope
	return sa.getExpressionType(le.In, letScope)
}

func (sa *SemanticAnalyser) GetAssignmentExpressionType(a *ast.Assignment, st *SymbolTable) string {
	// Get the type of the right-hand expression
	expressionType := sa.getExpressionType(a.Expression, st)
	if expressionType == "" {
		expressionType = "Object" // Default to Object for empty types
	}

	// Look for the identifier in symbol tables (walking up the scope chain)
	entry, found := st.Lookup(a.Name)
	if !found {
		sa.errors = append(sa.errors, fmt.Sprintf("undefined identifier '%s' in assignment", a.Name))
		return expressionType // Return expression type anyway to continue analysis
	}

	// Get the target variable's type
	var targetType string
	if entry.AttrType != nil {
		targetType = entry.AttrType.Value
	} else {
		targetType = entry.Type
	}

	// Make sure targetType is not empty
	if targetType == "" {
		sa.errors = append(sa.errors, fmt.Sprintf("variable '%s' has undefined type", a.Name))
		targetType = "Object" // Default
	}

	// Check if the expression type conforms to the variable's type
	if expressionType != "Object" && !sa.isTypeConformant(expressionType, targetType) {
		sa.errors = append(sa.errors, fmt.Sprintf("cannot assign value of type %s to identifier '%s' of type %s",
			expressionType, a.Name, targetType))
	}

	// Assignments return the value being assigned
	return expressionType
}
func (sa *SemanticAnalyser) GetUnaryExpressionType(uexpr *ast.UnaryExpression, st *SymbolTable) string {
	rightType := sa.getExpressionType(uexpr.Right, st)
	switch uexpr.Operator {
	case "~":
		if rightType != "Int" {
			sa.errors = append(sa.errors, fmt.Sprintf("bitwise negation on non-Int type: %s", rightType))
		}
		return "Int"
	case "not":
		if rightType != "Bool" {
			sa.errors = append(sa.errors, fmt.Sprintf("logical negation on non-Bool type: %s", rightType))
		}
		return "Bool"
	default:
		sa.errors = append(sa.errors, fmt.Sprintf("unknown unary operator %s", uexpr.Operator))
		return "Object"
	}
}

func isComparable(t string) bool {
	return t == "Int" || t == "Bool" || t == "String"
}
func (sa *SemanticAnalyser) GetBinaryExpressionType(be *ast.BinaryExpression, st *SymbolTable) string {
	leftType := sa.getExpressionType(be.Left, st)
	rightType := sa.getExpressionType(be.Right, st)

	// Ensure we don't have empty strings for types
	if leftType == "" {
		leftType = "Object"
	}
	if rightType == "" {
		rightType = "Object"
	}

	switch be.Operator {
	case "+", "-", "*", "/":
		if leftType == "Object" || rightType == "Object" {
			return "Int"
		}

		// Standard check for arithmetic operations
		if leftType != "Int" || rightType != "Int" {
			sa.errors = append(sa.errors, fmt.Sprintf("arithmetic operation '%s' requires Int types, got: %s and %s",
				be.Operator, leftType, rightType))
		}
		return "Int"

	case "<", "<=":
		if leftType == "Object" || rightType == "Object" {
			return "Bool"
		}

		// Standard check for comparison operations
		if leftType != rightType {
			sa.errors = append(sa.errors, fmt.Sprintf("comparison '%s' requires same types, got: %s and %s",
				be.Operator, leftType, rightType))
		} else if !isComparable(leftType) {
			sa.errors = append(sa.errors, fmt.Sprintf("type %s is not comparable with '%s'", leftType, be.Operator))
		}
		return "Bool"

	case "=":
		if leftType == "Object" || rightType == "Object" {
			return "Bool"
		}

		// For equality, same types can always be compared
		if leftType == rightType {
			return "Bool"
		}
		// Check if either type conforms to the other
		if sa.isTypeConformant(leftType, rightType) || sa.isTypeConformant(rightType, leftType) {
			return "Bool"
		}
		sa.errors = append(sa.errors, fmt.Sprintf("cannot compare values of incompatible types: %s and %s",
			leftType, rightType))
		return "Bool"

	default:
		sa.errors = append(sa.errors, fmt.Sprintf("unknown binary operator: %s", be.Operator))
		return "Object"
	}
}

func (sa *SemanticAnalyser) GetCaseExpressionType(ce *ast.CaseExpression, st *SymbolTable) string {
	exprType := sa.getExpressionType(ce.Expression, st)
	if exprType == "" {
		return "Object"
	}

	if len(ce.Cases) == 0 {
		return "Object"
	}

	// Get type of first case's body as initial LCA candidate
	branchType := sa.getExpressionType(ce.Cases[0].Body, st)

	// For each subsequent case, find LCA of current type and case's body type
	for _, cse := range ce.Cases[1:] {
		// Create new scope for this branch
		branchSt := NewSymbolTable(st)

		// Check for required fields in the case
		if cse.TypeIdentifier == nil {
			sa.errors = append(sa.errors, "missing type identifier in case branch")
			continue
		}

		// Check if the pattern type exists in the global symbol table
		patternType := cse.TypeIdentifier.Value
		if _, ok := sa.globalSymbolTable.Lookup(patternType); !ok {
			sa.errors = append(sa.errors, fmt.Sprintf("undefined type %s in case pattern", patternType))
			continue
		}

		// Add the case variable to the branch scope if it exists
		if cse.ObjectIdentifier != nil {
			branchSt.AddEntry(cse.ObjectIdentifier.Value, &SymbolEntry{
				Type:  patternType,
				Token: cse.ObjectIdentifier.Token,
			})
		}

		// Get this branch's body type
		currentBranchType := sa.getExpressionType(cse.Body, branchSt)

		// Find LCA of current running type and this branch's type
		branchType = sa.findLCA(branchType, currentBranchType)
	}

	return branchType
}

func (sa *SemanticAnalyser) buildInheritanceGraph(program *ast.Program) {
	// Add built-in class inheritance information first
	sa.inheritanceGraph["Int"] = "Object"
	sa.inheritanceGraph["String"] = "Object"
	sa.inheritanceGraph["Bool"] = "Object"
	sa.inheritanceGraph["IO"] = "Object"
	sa.inheritanceGraph["Object"] = "" // Object has no parent

	// First pass: collect all inheritance relationships without checking cycles
	for _, class := range program.Classes {
		// Check for invalid inheritance from Int or Bool
		if class.Parent != nil && (class.Parent.Value == "Int" || class.Parent.Value == "Bool") {
			sa.errors = append(sa.errors, fmt.Sprintf("class %s cannot inherit from %s",
				class.Name.Value, class.Parent.Value))
			continue
		}

		// Check if class is trying to redefine Int or Bool
		if class.Name.Value == "Int" || class.Name.Value == "Bool" {
			sa.errors = append(sa.errors, fmt.Sprintf("class %s cannot be redefined", class.Name.Value))
			continue
		}

		// Record inheritance relationship
		if class.Parent == nil {
			sa.inheritanceGraph[class.Name.Value] = "Object"
		} else {
			// Check if parent class exists
			if _, ok := sa.globalSymbolTable.Lookup(class.Parent.Value); !ok {
				sa.errors = append(sa.errors, fmt.Sprintf("class %s inherits from undefined class %s",
					class.Name.Value, class.Parent.Value))
				// continue
			}

			sa.inheritanceGraph[class.Name.Value] = class.Parent.Value
		}
	}

	// Second pass: check for inheritance cycles
	for _, class := range program.Classes {
		if class.Parent != nil {
			// Check for cycles using a visited set
			visited := make(map[string]bool)
			current := class.Name.Value

			for current != "Object" && current != "" {
				if visited[current] {
					sa.errors = append(sa.errors, fmt.Sprintf("inheritance cycle detected for class %s",
						class.Name.Value))
					break
				}

				visited[current] = true
				current, _ = sa.inheritanceGraph[current]
			}
		}
	}
}

// findLCA finds the least common ancestor type of two types in the inheritance hierarchy
func (sa *SemanticAnalyser) findLCA(type1, type2 string) string {
	// If types are the same, that's the LCA
	if type1 == type2 {
		return type1
	}

	// Get all ancestors of type1 (including type1)
	ancestors1 := make(map[string]bool)
	current := type1
	ancestors1[current] = true
	for current != "Object" {
		parent, exists := sa.inheritanceGraph[current]
		if !exists {
			return "Object"
		}
		ancestors1[parent] = true
		current = parent
	}

	// Walk up type2's ancestry until we find a common ancestor
	current = type2
	for {
		if ancestors1[current] {
			return current
		}
		if current == "Object" {
			return "Object"
		}
		parent, exists := sa.inheritanceGraph[current]
		if !exists {
			return "Object"
		}
		current = parent
	}
}

// In static dispatch, the static type to the left of “@”must conform to the type specified to the right of “@”
func (sa *SemanticAnalyser) GetStaticDispatchExpressionType(sde *ast.StaticDispatchExpression, st *SymbolTable) string {
	// Get the type of the object expression
	objectType := sa.getExpressionType(sde.Object, st)
	if objectType == "" {
		objectType = "Object" // Ensure we never have empty type names
	}

	// Get the static type specified after @
	staticType := sde.StaticType.Value
	if staticType == "" {
		staticType = "Object"
	}

	// Check if the static type exists
	if _, ok := sa.globalSymbolTable.Lookup(staticType); !ok {
		sa.errors = append(sa.errors, fmt.Sprintf("undefined type %s in static dispatch", staticType))
		return "Object"
	}

	// Check if the object's type conforms to the static type
	if !sa.isTypeConformant(objectType, staticType) {
		sa.errors = append(sa.errors, fmt.Sprintf(
			"static dispatch type error: expression of type %s does not conform to declared type %s",
			objectType, staticType))
		return "Object"
	}

	// Look up the method in the static type's class
	classEntry, ok := sa.globalSymbolTable.Lookup(staticType)
	if !ok || classEntry.Scope == nil {
		sa.errors = append(sa.errors, fmt.Sprintf("cannot find class scope for type %s", staticType))
		return "Object"
	}

	methodEntry, ok := classEntry.Scope.Lookup(sde.Method.Value)
	if !ok || methodEntry.Method == nil {
		sa.errors = append(sa.errors, fmt.Sprintf("undefined method %s in type %s", sde.Method.Value, staticType))
		return "Object"
	}

	// Check argument count
	if len(sde.Arguments) != len(methodEntry.Method.Formals) {
		sa.errors = append(sa.errors, fmt.Sprintf(
			"wrong number of arguments for method %s: expected %d, got %d",
			sde.Method.Value, len(methodEntry.Method.Formals), len(sde.Arguments)))
		return methodEntry.Method.TypeDecl.Value
	}

	// Check argument types
	for i, arg := range sde.Arguments {
		argType := sa.getExpressionType(arg, st)
		if argType == "" {
			argType = "Object" // Default to Object for empty types
		}

		formalType := methodEntry.Method.Formals[i].TypeDecl.Value
		// Ensure formal type is not empty
		if formalType == "" {
			formalType = "Object"
		}

		if !sa.isTypeConformant(argType, formalType) {
			sa.errors = append(sa.errors, fmt.Sprintf(
				"argument %d of method %s has wrong type: expected %s, got %s",
				i+1, sde.Method.Value, formalType, argType))
		}
	}

	// Return the method's return type
	returnType := methodEntry.Method.TypeDecl.Value
	if returnType == "" {
		returnType = "Object" // Default to Object for empty return types
	}
	return returnType
}
func (sa *SemanticAnalyser) GetDispatchExpressionType(de *ast.DispatchExpression, st *SymbolTable) string {
	// Get the type of the object being dispatched on
	var objectType string
	if de.Object == nil {
		// Self dispatch code...
	} else {
		objectType = sa.getExpressionType(de.Object, st)
	}

	// Look up the method in the object's class
	classEntry, ok := sa.globalSymbolTable.Lookup(objectType)
	if !ok || classEntry.Scope == nil {
		sa.errors = append(sa.errors, fmt.Sprintf("cannot find class scope for type %s", objectType))
		return "Object"
	}

	methodEntry, ok := classEntry.Scope.Lookup(de.Method.Value)

	if !ok || methodEntry.Method == nil {
		// Check in parent classes
		current := objectType
		found := false
		for current != "" && current != "Object" {
			parent, exists := sa.inheritanceGraph[current]
			if !exists {
				break
			}

			parentEntry, ok := sa.globalSymbolTable.Lookup(parent)
			if ok && parentEntry.Scope != nil {
				methodEntry, ok = parentEntry.Scope.Lookup(de.Method.Value)

				if ok && methodEntry.Method != nil {
					found = true
					break
				}
			}
			current = parent
		}

		if !found {
			sa.errors = append(sa.errors, fmt.Sprintf("undefined method %s in type %s", de.Method.Value, objectType))
			return "Object"
		}
	}

	// Before returning the type
	returnType := methodEntry.Method.TypeDecl.Value
	if returnType == "SELF_TYPE" {
		returnType = objectType
	}
	if returnType == "" {
		returnType = "Object" // Default to Object for empty return types
	}

	return returnType
}
func (sa *SemanticAnalyser) GetCallExpressionType(ce *ast.CallExpression, st *SymbolTable) string {
	// For call expressions without an explicit object (e.g. foo()),
	// the receiver is implicitly self

	// Find the containing class scope
	currentTable := st
	var classScope *SymbolTable
	for currentTable != nil && currentTable.parent != sa.globalSymbolTable {
		currentTable = currentTable.parent
	}
	classScope = currentTable

	if classScope == nil {
		sa.errors = append(sa.errors, "could not determine containing class for method call")
		return "Object"
	}

	// Find the current class name
	var className string
	for name, entry := range sa.globalSymbolTable.symbols {
		if entry.Scope == classScope {
			className = name
			break
		}
	}

	// Get the function name
	var methodName string
	switch fn := ce.Function.(type) {
	case *ast.ObjectIdentifier:
		methodName = fn.Value
	default:
		sa.errors = append(sa.errors, "invalid method call format")
		return "Object"
	}

	// Look up the method in the current class and its hierarchy
	var methodEntry *SymbolEntry
	var found bool

	// First try the current class
	classEntry, ok := sa.globalSymbolTable.Lookup(className)
	if ok && classEntry.Scope != nil {
		methodEntry, found = classEntry.Scope.Lookup(methodName)
	}

	// If not found in current class, check parent classes
	if !found || methodEntry.Method == nil {
		current := className
		for current != "" && current != "Object" {
			parent, exists := sa.inheritanceGraph[current]
			if !exists {
				break
			}

			parentEntry, ok := sa.globalSymbolTable.Lookup(parent)
			if ok && parentEntry.Scope != nil {
				methodEntry, found = parentEntry.Scope.Lookup(methodName)
				if found && methodEntry.Method != nil {
					break
				}
			}
			current = parent
		}
	} else {
	}

	if !found || methodEntry == nil || methodEntry.Method == nil {
		sa.errors = append(sa.errors, fmt.Sprintf("undefined method %s", methodName))
		return "Object"
	}

	// Check argument count
	if len(ce.Arguments) != len(methodEntry.Method.Formals) {
		sa.errors = append(sa.errors, fmt.Sprintf(
			"wrong number of arguments for method %s: expected %d, got %d",
			methodName, len(methodEntry.Method.Formals), len(ce.Arguments)))
	}

	// Type check arguments
	for i, arg := range ce.Arguments {
		if i >= len(methodEntry.Method.Formals) {
			break // Avoid index out of bounds if wrong number of args
		}

		argType := sa.getExpressionType(arg, st)
		if argType == "" {
			argType = "Object" // Default to Object for empty types
		}

		formalType := methodEntry.Method.Formals[i].TypeDecl.Value
		// Ensure formal type is not empty
		if formalType == "" {
			formalType = "Object"
		}

		if !sa.isTypeConformant(argType, formalType) {
			sa.errors = append(sa.errors, fmt.Sprintf(
				"argument %d of method %s has wrong type: expected %s, got %s",
				i+1, methodName, formalType, argType))
		}
	}

	// Return the method's return type with null check
	returnType := methodEntry.Method.TypeDecl.Value

	// Handle SELF_TYPE - this is what was missing!
	if returnType == "SELF_TYPE" {
		returnType = className
	}

	if returnType == "" {
		returnType = "Object" // Default to Object for empty return types
	}

	return returnType
}
