import * as ts from 'typescript'
import * as fs from 'fs'
import * as path from 'path'

/**
 * Advanced compiler API usage for building developer tools
 * Staff+ engineers need deep knowledge of the compiler for building:
 * - Custom linters
 * - Code generators
 * - Migration tools
 * - Code analyzers
 */

// Custom transformer that inserts performance instrumentation
function createPerformanceTransformer(context: ts.TransformationContext) {
  return (sourceFile: ts.SourceFile): ts.SourceFile => {
    // Skip declaration files
    if (sourceFile.isDeclarationFile) {
      return sourceFile
    }

    function visit(node: ts.Node): ts.Node {
      // Only instrument function declarations and method declarations
      if (ts.isFunctionDeclaration(node) && node.body) {
        return instrumentFunction(node)
      } else if (ts.isMethodDeclaration(node) && node.body) {
        return instrumentMethod(node)
      }

      return ts.visitEachChild(node, visit, context)
    }

    function instrumentFunction(node: ts.FunctionDeclaration): ts.FunctionDeclaration {
      const funcName = node.name?.text || 'anonymous'
      return instrumentFunctionLike(node, funcName)
    }

    function instrumentMethod(node: ts.MethodDeclaration): ts.MethodDeclaration {
      let className = 'UnknownClass'
      
      // Try to determine the class name
      let parent = node.parent
      while (parent) {
        if (ts.isClassDeclaration(parent) && parent.name) {
          className = parent.name.text
          break
        }
        parent = parent.parent
      }
      
      const methodName = node.name.getText()
      return instrumentFunctionLike(node, `${className}.${methodName}`) as ts.MethodDeclaration
    }

    function instrumentFunctionLike<T extends ts.FunctionDeclaration | ts.MethodDeclaration>(
      node: T,
      name: string
    ): T {
      // Create performance tracking statements
      const startVar = ts.factory.createUniqueName('_perf_start')
      const startStatement = ts.factory.createVariableStatement(
        undefined,
        ts.factory.createVariableDeclarationList(
          [ts.factory.createVariableDeclaration(
            startVar,
            undefined,
            undefined,
            ts.factory.createCallExpression(
              ts.factory.createPropertyAccessExpression(
                ts.factory.createIdentifier('performance'),
                'now'
              ),
              undefined,
              []
            )
          )],
          ts.NodeFlags.Const
        )
      )

      // Create try-finally to ensure timing is always recorded
      const tryBlock = ts.factory.createBlock(
        [...(node.body?.statements || [])],
        true
      )

      const finallyBlock = ts.factory.createBlock([
        ts.factory.createExpressionStatement(
          ts.factory.createCallExpression(
            ts.factory.createPropertyAccessExpression(
              ts.factory.createIdentifier('console'),
              'log'
            ),
            undefined,
            [
              ts.factory.createTemplateExpression(
                ts.factory.createTemplateHead(`Performance: ${name} took `),
                [
                  ts.factory.createTemplateSpan(
                    ts.factory.createBinaryExpression(
                      ts.factory.createCallExpression(
                        ts.factory.createPropertyAccessExpression(
                          ts.factory.createIdentifier('performance'),
                          'now'
                        ),
                        undefined,
                        []
                      ),
                      ts.SyntaxKind.MinusToken,
                      startVar
                    ),
                    ts.factory.createTemplateTail(' ms')
                  )
                ]
              )
            ]
          )
        )
      ])

      // Create new body with instrumentation
      const newBody = ts.factory.createBlock(
        [
          startStatement,
          ts.factory.createTryStatement(
            tryBlock,
            undefined,
            finallyBlock
          )
        ],
        true
      )

      // Return a new function with the instrumented body
      return ts.factory.updateFunctionDeclaration(
        node as ts.FunctionDeclaration,
        node.decorators,
        node.modifiers,
        node.asteriskToken,
        node.name,
        node.typeParameters,
        node.parameters,
        node.type,
        newBody
      ) as T
    }

    return ts.visitEachChild(sourceFile, visit, context)
  }
}

// Type checker that finds and reports complex type issues
class ComplexTypeAnalyzer {
  private typeChecker: ts.TypeChecker
  private sourceFile: ts.SourceFile
  private issues: { message: string; node: ts.Node }[] = []

  constructor(program: ts.Program, sourceFile: ts.SourceFile) {
    this.typeChecker = program.getTypeChecker()
    this.sourceFile = sourceFile
  }

  analyze(): { message: string; line: number; column: number }[] {
    this.visit(this.sourceFile)
    
    return this.issues.map(issue => {
      const { line, character } = this.sourceFile.getLineAndCharacterOfPosition(issue.node.getStart())
      return {
        message: issue.message,
        line: line + 1,
        column: character + 1
      }
    })
  }

  private visit(node: ts.Node) {
    // Check for complex type assertions
    if (ts.isAsExpression(node)) {
      this.checkTypeAssertion(node)
    }
    
    // Check for complex generic instantiations
    if (ts.isTypeReferenceNode(node) && node.typeArguments && node.typeArguments.length > 3) {
      this.checkComplexGeneric(node)
    }
    
    // Check for complex conditional types
    if (ts.isConditionalTypeNode && ts.isConditionalTypeNode(node)) {
      this.checkConditionalType(node)
    }

    // Check for complex mapped types
    if (ts.isMappedTypeNode && ts.isMappedTypeNode(node)) {
      this.checkMappedType(node)
    }
    
    // Recursively visit all children
    node.forEachChild(child => this.visit(child))
  }

  private checkTypeAssertion(node: ts.AsExpression) {
    const targetType = this.typeChecker.getTypeAtLocation(node.type)
    const sourceType = this.typeChecker.getTypeAtLocation(node.expression)
    
    // Check if this is a potentially unsafe assertion
    if (!this.typeChecker.isTypeAssignableTo(sourceType, targetType) && 
        !this.typeChecker.isTypeAssignableTo(targetType, sourceType)) {
      this.issues.push({
        message: `Potentially unsafe type assertion from ${this.typeChecker.typeToString(sourceType)} to ${this.typeChecker.typeToString(targetType)}`,
        node
      })
    }
  }

  private checkComplexGeneric(node: ts.TypeReferenceNode) {
    this.issues.push({
      message: `Complex generic type with ${node.typeArguments!.length} type arguments may affect readability`,
      node
    })
  }

  private checkConditionalType(node: ts.ConditionalTypeNode) {
    // Check for nested conditional types
    let nestedLevel = 0
    const countNesting = (n: ts.Node): void => {
      if (ts.isConditionalTypeNode(n)) {
        nestedLevel++
      }
      n.forEachChild(countNesting)
    }
    
    countNesting(node.extendsType)
    countNesting(node.trueType)
    countNesting(node.falseType)
    
    if (nestedLevel > 2) {
      this.issues.push({
        message: `Complex conditional type with ${nestedLevel} nested conditions may affect readability and compilation performance`,
        node
      })
    }
  }

  private checkMappedType(node: ts.MappedTypeNode) {
    // Check for complex mapped types
    const hasConditional = this.hasConditionalType(node.type)
    
    if (hasConditional) {
      this.issues.push({
        message: `Complex mapped type with conditional mapping may affect type inference performance`,
        node
      })
    }
  }

  private hasConditionalType(node?: ts.Node): boolean {
    if (!node) return false
    if (ts.isConditionalTypeNode(node)) return true
    
    let found = false
    node.forEachChild(child => {
      if (this.hasConditionalType(child)) {
        found = true
      }
    })
    
    return found
  }
}

// Usage example
function runCompilerAPIDemo(filePath: string) {
  console.log(`Analyzing and transforming file: ${filePath}`)
  
  // Read the source file
  const fileContents = fs.readFileSync(filePath, 'utf-8')
  
  // Create compiler options
  const compilerOptions: ts.CompilerOptions = {
    target: ts.ScriptTarget.ESNext,
    module: ts.ModuleKind.ESNext,
    strict: true,
  }
  
  // Create virtual filesystem and compiler host
  const host = ts.createCompilerHost(compilerOptions)
  
  // Create a program
  const program = ts.createProgram([filePath], compilerOptions, host)
  
  // Get the source file
  const sourceFile = program.getSourceFile(filePath)
  if (!sourceFile) {
    console.error(`Could not find source file: ${filePath}`)
    return
  }
  
  // Analyze the file for complex types
  console.log(`\nRunning complex type analysis:`)
  const analyzer = new ComplexTypeAnalyzer(program, sourceFile)
  const issues = analyzer.analyze()
  
  if (issues.length === 0) {
    console.log(`No complex type issues found`)
  } else {
    issues.forEach(issue => {
      console.log(`Line ${issue.line}, Column ${issue.column}: ${issue.message}`)
    })
  }
  
  // Transform the file to add performance instrumentation
  console.log(`\nTransforming code to add performance instrumentation:`)
  const result = ts.transform(sourceFile, [createPerformanceTransformer])
  const printer = ts.createPrinter()
  const transformedCode = printer.printFile(result.transformed[0] as ts.SourceFile)
  
  // Output path for transformed file
  const outputPath = path.join(
    path.dirname(filePath),
    `${path.basename(filePath, path.extname(filePath))}.instrumented${path.extname(filePath)}`
  )
  
  // Write the transformed file
  fs.writeFileSync(outputPath, transformedCode)
  console.log(`Transformed code written to: ${outputPath}`)
}

// If run directly
if (require.main === module) {
  const filePath = process.argv[2]
  if (!filePath) {
    console.error('Please provide a file path to analyze')
    process.exit(1)
  }
  
  runCompilerAPIDemo(filePath)
}
