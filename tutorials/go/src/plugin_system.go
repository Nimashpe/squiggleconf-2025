package main

import (
	"fmt"
	"plugin"
	"reflect"
)

// Plugin interface that all plugins must implement
type CodeAnalyzer interface {
	Name() string
	Analyze(code string) ([]Issue, error)
}

// Issue represents a code issue found by an analyzer
type Issue struct {
	Line    int
	Column  int
	Message string
	Severity string
}

// PluginManager handles loading and managing plugins
type PluginManager struct {
	plugins map[string]CodeAnalyzer
}

func NewPluginManager() *PluginManager {
	return &PluginManager{
		plugins: make(map[string]CodeAnalyzer),
	}
}

// LoadPlugin loads a plugin from a .so file
func (pm *PluginManager) LoadPlugin(path string) error {
	p, err := plugin.Open(path)
	if err != nil {
		return fmt.Errorf("failed to open plugin: %w", err)
	}
	
	// Look up the symbol "Analyzer" which should implement CodeAnalyzer
	analyzerSymbol, err := p.Lookup("Analyzer")
	if err != nil {
		return fmt.Errorf("failed to find Analyzer symbol: %w", err)
	}
	
	// Check if the symbol is the correct type
	analyzer, ok := analyzerSymbol.(CodeAnalyzer)
	if !ok {
		// Try if it's a pointer to CodeAnalyzer
		ptr := reflect.ValueOf(analyzerSymbol)
		if ptr.Kind() == reflect.Ptr && ptr.Elem().Type().Implements(reflect.TypeOf((*CodeAnalyzer)(nil)).Elem()) {
			analyzer = ptr.Elem().Interface().(CodeAnalyzer)
		} else {
			return fmt.Errorf("symbol is not a CodeAnalyzer")
		}
	}
	
	pm.plugins[analyzer.Name()] = analyzer
	return nil
}

// RunAnalysis runs all loaded plugins on the given code
func (pm *PluginManager) RunAnalysis(code string) map[string][]Issue {
	results := make(map[string][]Issue)
	
	for name, analyzer := range pm.plugins {
		issues, err := analyzer.Analyze(code)
		if err != nil {
			fmt.Printf("Error running analyzer %s: %v\n", name, err)
			continue
		}
		results[name] = issues
	}
	
	return results
}

func main() {
	// Example usage
	pm := NewPluginManager()
	
	// In a real application, these would be loaded from plugin files
	// err := pm.LoadPlugin("path/to/linter_plugin.so")
	// if err != nil {
	//     fmt.Printf("Error loading plugin: %v\n", err)
	//     return
	// }
	
	// For demo purposes, simulate some loaded plugins
	simulatePlugins(pm)
	
	// Run analysis on some code
	code := `package main

import "fmt"

func main() {
	var x int
	fmt.Println("Hello, world!")
}
`
	results := pm.RunAnalysis(code)
	
	// Print results
	for name, issues := range results {
		fmt.Printf("Results from %s:\n", name)
		if len(issues) == 0 {
			fmt.Println("  No issues found")
		} else {
			for _, issue := range issues {
				fmt.Printf("  Line %d, Col %d: [%s] %s\n", 
					issue.Line, issue.Column, issue.Severity, issue.Message)
			}
		}
	}
}

// Simulate some loaded plugins for demo purposes
func simulatePlugins(pm *PluginManager) {
	// Unused variable detector
	pm.plugins["unused-var"] = &simulatedAnalyzer{
		name: "unused-var",
		analyzeFunc: func(code string) ([]Issue, error) {
			// In a real plugin, this would parse the code and perform analysis
			return []Issue{
				{
					Line:     5,
					Column:   6,
					Message:  "Variable 'x' declared but not used",
					Severity: "warning",
				},
			}, nil
		},
	}
	
	// Import checker
	pm.plugins["import-checker"] = &simulatedAnalyzer{
		name: "import-checker",
		analyzeFunc: func(code string) ([]Issue, error) {
			// This would check for unused or redundant imports
			return []Issue{}, nil
		},
	}
}

// Simulated analyzer for demo purposes
type simulatedAnalyzer struct {
	name        string
	analyzeFunc func(string) ([]Issue, error)
}

func (a *simulatedAnalyzer) Name() string {
	return a.name
}

func (a *simulatedAnalyzer) Analyze(code string) ([]Issue, error) {
	return a.analyzeFunc(code)
}
