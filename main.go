package main

import (
	"cool-compiler/codegen"
	"cool-compiler/importer"
	"cool-compiler/lexer"
	"cool-compiler/parser"
	"cool-compiler/semant"

	"flag"
	"fmt"
	"os"
	"strings"
)

func main() {
	inputFile := flag.String("i", "", "Input COOL source file")
	outputFile := flag.String("o", "output.ll", "Output LLVM IR file")
	flag.Parse()

	fmt.Println("Starting compilation...")

	// Check if input file is provided
	if *inputFile == "" {
		fmt.Println("Error: Input file is required")
		flag.PrintDefaults()
		os.Exit(1)
	}

	fmt.Printf("Processing imports for file: %s\n", *inputFile)
	// Create new importer
	imp := importer.New()

	// Process the file and its imports
	processedSource, err := imp.ProcessFile(*inputFile)
	if err != nil {
		fmt.Printf("Error processing imports: %v\n", err)
		os.Exit(1)
	}

	fmt.Println("Creating lexer...")
	// Create lexer from processed source
	l := lexer.NewLexer(strings.NewReader(processedSource))

	fmt.Println("Parsing program...")
	// Parse COOL program
	p := parser.New(l)
	program := p.ParseProgram()

	if len(p.Errors()) > 0 {
		fmt.Println("Parsing errors:")
		for _, err := range p.Errors() {
			fmt.Printf("\t%s\n", err)
		}
		os.Exit(1)
	}

	fmt.Println("Performing semantic analysis...")
	// Perform semantic analysis
	analyzer := semant.NewSemanticAnalyser()
	analyzer.Analyze(program)

	if len(analyzer.Errors()) > 0 {
		fmt.Println("Semantic errors:")
		for _, err := range analyzer.Errors() {
			fmt.Printf("\t%s\n", err)
		}
		os.Exit(1)
	}

	fmt.Println("Generating LLVM IR...")
	// Generate LLVM IR
	gen := codegen.NewGenerator()
	module := gen.Generate(program)

	fmt.Printf("Writing output to: %s\n", *outputFile)
	// Write LLVM IR to file
	f, err := os.Create(*outputFile)
	if err != nil {
		fmt.Printf("Error creating output file: %v\n", err)
		os.Exit(1)
	}
	defer f.Close()

	if _, err := fmt.Fprintf(f, "%s", module.String()); err != nil {
		fmt.Printf("Error writing output: %v\n", err)
		os.Exit(1)
	}

	fmt.Printf("Successfully generated LLVM IR in %s\n", *outputFile)
}
