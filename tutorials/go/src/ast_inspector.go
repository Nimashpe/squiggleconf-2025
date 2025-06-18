package main

import (
	"fmt"
	"go/ast"
	"go/parser"
	"go/token"
	"os"
)

// AST visitor for sophisticated code analysis
type Inspector struct {
	functionDepth int
	complexityMap map[string]int
}

func (i *Inspector) Visit(n ast.Node) ast.Visitor {
	if n == nil {
		return nil
	}

	switch x := n.(type) {
	case *ast.FuncDecl:
		name := x.Name.Name
		i.functionDepth++
		i.complexityMap[name] = 1 // Base complexity
		defer func() { i.functionDepth-- }()
		
	case *ast.IfStmt, *ast.ForStmt, *ast.RangeStmt, *ast.SwitchStmt:
		// Increment complexity for the current function
		for name := range i.complexityMap {
			i.complexityMap[name]++
			break // Only update the most recent function
		}
	}
	
	return i
}

func main() {
	fset := token.NewFileSet()
	if len(os.Args) < 2 {
		fmt.Println("Usage: ast_inspector <file.go>")
		return
	}
	
	f, err := parser.ParseFile(fset, os.Args[1], nil, parser.ParseComments)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error parsing file: %v\n", err)
		return
	}
	
	inspector := &Inspector{
		complexityMap: make(map[string]int),
	}
	
	ast.Walk(inspector, f)
	
	fmt.Println("Function Complexity Analysis:")
	for fn, complexity := range inspector.complexityMap {
		fmt.Printf("  %s: %d\n", fn, complexity)
	}
}
