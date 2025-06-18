/**
 * Building a macro system for TypeScript
 * This demonstrates advanced techniques for extending the language
 */

import * as ts from 'typescript'
import * as fs from 'fs'
import * as path from 'path'

// Type definitions for our macro system
namespace Macros {
  export interface MacroFunction<T extends any[] = any[]> {
    (...args: T): ts.Expression | ts.Statement | ts.Node
  }
  
  export interface MacroRegistry {
    [name: string]: MacroFunction
  }
  
  export interface MacroContext {
    factory: ts.NodeFactory
    checker: ts.TypeChecker
    sourceFile: ts.SourceFile
    program: ts.Program
  }
}

// Standard library of macros
const standardMacros: Macros.MacroRegistry = {
  // Assertion macro similar to Rust's assert!
  assert(condition: ts.Expression, message?: ts.Expression) {
    const factory = ts.factory
    
    const condition_str = ts.isStringLiteral(condition) 
      ? condition.text 
      : condition.getText()
    
    const errorMessage = message || factory.createStringLiteral(
      `Assertion failed: ${condition_str}`
    )
    
    return factory.createIfStatement(
      factory.createPrefixUnaryExpression(
        ts.SyntaxKind.ExclamationToken,
        condition
      ),
      factory.createBlock([
        factory.createThrowStatement(
          factory.createNewExpression(
            factory.createIdentifier('Error'),
            undefined,
            [errorMessage]
          )
        )
      ]),
      undefined
    )
  },
  
  // SQL template string macro
  sql(template: ts.Expression, ...params: ts.Expression[]) {
    const factory = ts.factory
    
    // We expect the template to be a string literal
    if (!ts.isStringLiteral(template)) {
      throw new Error('sql macro requires a string literal')
    }
    
    // In a real implementation, we would validate the SQL
    // and create proper parameterized query objects
    
    return factory.createObjectLiteralExpression([
      factory.createPropertyAssignment(
        'sql',
        template
      ),
      factory.createPropertyAssignment(
        'params',
        factory.createArrayLiteralExpression(params)
      ),
      factory.createMethodDeclaration(
        undefined,
        undefined,
        undefined,
        'execute',
        undefined,
        undefined,
        [],
        undefined,
        factory.createBlock([
          factory.createReturnStatement(
            factory.createCallExpression(
              factory.createPropertyAccessExpression(
                factory.createIdentifier('db'),
                'query'
              ),
              undefined,
              [
                factory.createPropertyAccessExpression(
                  factory.createThis(),
                  'sql'
                ),
                factory.createPropertyAccessExpression(
                  factory.createThis(),
                  'params'
                )
              ]
            )
          )
        ])
      )
    ])
  },
  
  // Memoize function results
  memoize(func: ts.Expression) {
    const factory = ts.factory
    
    return factory.createCallExpression(
      factory.createArrowFunction(
        undefined,
        undefined,
        [],
        undefined,
        factory.createToken(ts.SyntaxKind.EqualsGreaterThanToken),
        factory.createBlock([
          factory.createVariableStatement(
            undefined,
            factory.createVariableDeclarationList(
              [factory.createVariableDeclaration(
                'cache',
                undefined,
                undefined,
                factory.createNewExpression(
                  factory.createIdentifier('Map'),
                  undefined,
                  []
                )
              )],
              ts.NodeFlags.Const
            )
          ),
          factory.createReturnStatement(
            factory.createArrowFunction(
              undefined,
              undefined,
              [factory.createParameterDeclaration(
                undefined,
                undefined,
                undefined,
                'args',
                undefined,
                undefined,
                undefined
              )],
              undefined,
              factory.createToken(ts.SyntaxKind.EqualsGreaterThanToken),
              factory.createBlock([
                factory.createVariableStatement(
                  undefined,
                  factory.createVariableDeclarationList(
                    [factory.createVariableDeclaration(
                      'key',
                      undefined,
                      undefined,
                      factory.createCallExpression(
                        factory.createPropertyAccessExpression(
                          factory.createIdentifier('JSON'),
                          'stringify'
                        ),
                        undefined,
                        [factory.createIdentifier('args')]
                      )
                    )],
                    ts.NodeFlags.Const
                  )
                ),
                factory.createIfStatement(
                  factory.createCallExpression(
                    factory.createPropertyAccessExpression(
                      factory.createIdentifier('cache'),
                      'has'
                    ),
                    undefined,
                    [factory.createIdentifier('key')]
                  ),
                  factory.createBlock([
                    factory.createReturnStatement(
                      factory.createCallExpression(
                        factory.createPropertyAccessExpression(
                          factory.createIdentifier('cache'),
                          'get'
                        ),
                        undefined,
                        [factory.createIdentifier('key')]
                      )
                    )
                  ]),
                  undefined
                ),
                factory.createVariableStatement(
                  undefined,
                  factory.createVariableDeclarationList(
                    [factory.createVariableDeclaration(
                      'result',
                      undefined,
                      undefined,
                      factory.createCallExpression(
                        func,
                        undefined,
                        [
                          factory.createSpreadElement(
                            factory.createIdentifier('args')
                          )
                        ]
                      )
                    )],
                    ts.NodeFlags.Const
                  )
                ),
                factory.createExpressionStatement(
                  factory.createCallExpression(
                    factory.createPropertyAccessExpression(
                      factory.createIdentifier('cache'),
                      'set'
                    ),
                    undefined,
                    [
                      factory.createIdentifier('key'),
                      factory.createIdentifier('result')
                    ]
                  )
                ),
                factory.createReturnStatement(
                  factory.createIdentifier('result')
                )
              ])
            )
          )
        ])
      ),
      undefined,
      []
    )
  },
  
  // Timing macro
  time(expr: ts.Expression, label?: ts.Expression) {
    const factory = ts.factory
    
    const labelExpr = label || factory.createStringLiteral('Execution time')
    
    return factory.createCallExpression(
      factory.createArrowFunction(
        undefined,
        undefined,
        [],
        undefined,
        factory.createToken(ts.SyntaxKind.EqualsGreaterThanToken),
        factory.createBlock([
          factory.createVariableStatement(
            undefined,
            factory.createVariableDeclarationList(
              [factory.createVariableDeclaration(
                'start',
                undefined,
                undefined,
                factory.createCallExpression(
                  factory.createPropertyAccessExpression(
                    factory.createIdentifier('performance'),
                    'now'
                  ),
                  undefined,
                  []
                )
              )],
              ts.NodeFlags.Const
            )
          ),
          factory.createVariableStatement(
            undefined,
            factory.createVariableDeclarationList(
              [factory.createVariableDeclaration(
                'result',
                undefined,
                undefined,
                expr
              )],
              ts.NodeFlags.Const
            )
          ),
          factory.createExpressionStatement(
            factory.createCallExpression(
              factory.createPropertyAccessExpression(
                factory.createIdentifier('console'),
                'log'
              ),
              undefined,
              [
                labelExpr,
                factory.createBinaryExpression(
                  factory.createCallExpression(
                    factory.createPropertyAccessExpression(
                      factory.createIdentifier('performance'),
                      'now'
                    ),
                    undefined,
                    []
                  ),
                  factory.createToken(ts.SyntaxKind.MinusToken),
                  factory.createIdentifier('start')
                ),
                factory.createStringLiteral('ms')
              ]
            )
          ),
          factory.createReturnStatement(
            factory.createIdentifier('result')
          )
        ])
      ),
      undefined,
      []
    )
  }
}

// Macro transformer for TypeScript compiler
function createMacroTransformer(
  program: ts.Program,
  macros: Macros.MacroRegistry = standardMacros
): ts.TransformerFactory<ts.SourceFile> {
  return context => {
    const checker = program.getTypeChecker()
    
    return sourceFile => {
      const visitor = (node: ts.Node): ts.Node => {
        // Look for calls to our special macro marker
        if (ts.isCallExpression(node) && 
            ts.isIdentifier(node.expression) && 
            node.expression.text === 'macro') {
          
          // The first argument should be the name of the macro
          if (node.arguments.length < 1 || !ts.isStringLiteral(node.arguments[0])) {
            throw new Error('First argument to macro() must be a string literal')
          }
          
          const macroName = node.arguments[0].text
          const macroFunction = macros[macroName]
          
          if (!macroFunction) {
            throw new Error(`Macro not found: ${macroName}`)
          }
          
          // Pass the remaining arguments to the macro function
          const macroArgs = node.arguments.slice(1)
          const context: Macros.MacroContext = {
            factory: ts.factory,
            checker,
            sourceFile,
            program
          }
          
          // Execute the macro function
          return macroFunction.apply(context, macroArgs)
        }
        
        // Recursively visit children
        return ts.visitEachChild(node, visitor, context)
      }
      
      return ts.visitNode(sourceFile, visitor) as ts.SourceFile
    }
  }
}

// Example of how to use the macro system
function processMacros(filePath: string, outputPath: string) {
  const program = ts.createProgram([filePath], {
    target: ts.ScriptTarget.ES2020,
    module: ts.ModuleKind.ESNext,
  })
  
  const sourceFile = program.getSourceFile(filePath)
  if (!sourceFile) {
    throw new Error(`Could not find source file: ${filePath}`)
  }
  
  // Apply the macro transformer
  const result = ts.transform(
    sourceFile,
    [createMacroTransformer(program)]
  )
  
  // Print the transformed source
  const printer = ts.createPrinter()
  const transformedCode = printer.printFile(result.transformed[0] as ts.SourceFile)
  
  // Write the result to file
  fs.writeFileSync(outputPath, transformedCode)
  
  console.log(`Processed macros in ${filePath}`)
  console.log(`Output written to ${outputPath}`)
}

// Example code with macros
const exampleCode = `
// TypeScript code with macros
function fibonacci(n: number): number {
  // Assertions using macros
  macro("assert", n >= 0, "Fibonacci requires n >= 0")
  
  // Memoize computation
  const memoizedFib = macro("memoize", function(x: number): number {
    if (x <= 1) return x
    return fibonacci(x - 1) + fibonacci(x - 2)
  })
  
  // Time the execution
  return macro("time", memoizedFib(n), "Fibonacci(" + n + ")")
}

// SQL query using macros
function getUsersByRole(role: string) {
  return macro("sql", "SELECT * FROM users WHERE role = ?", role)
}

// Call the functions
fibonacci(10)
getUsersByRole('admin')
`

// Save example code to file for processing
const exampleFile = path.join(__dirname, 'example_with_macros.ts')
fs.writeFileSync(exampleFile, exampleCode)

// Process macros and show the result
const outputFile = path.join(__dirname, 'example_processed.ts')
processMacros(exampleFile, outputFile)

// Read and log the processed file
console.log('\nProcessed code:')
console.log('===============')
console.log(fs.readFileSync(outputFile, 'utf-8'))
