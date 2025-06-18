// Advanced TypeScript type manipulation for staff+ engineers
// Techniques for building powerful domain-specific type systems

// Deep readonly type that preserves exact shape
type DeepReadonly<T> = {
  readonly [P in keyof T]: T[P] extends object 
    ? T[P] extends Function 
      ? T[P] 
      : DeepReadonly<T[P]> 
    : T[P]
}

// Deep partial with exact type preservation
type DeepPartial<T> = {
  [P in keyof T]?: T[P] extends object
    ? T[P] extends Function
      ? T[P]
      : DeepPartial<T[P]>
    : T[P]
}

// Path-based property access for type-safe record traversal
type Path<T> = T extends object
  ? { [K in keyof T]: [K] | [K, ...Path<T[K]>] }[keyof T]
  : never

type PathValue<T, P extends Path<T>> = P extends [infer K]
  ? K extends keyof T
    ? T[K]
    : never
  : P extends [infer K, ...infer Rest]
    ? K extends keyof T
      ? Rest extends Path<T[K]>
        ? PathValue<T[K], Rest>
        : never
      : never
    : never

// Type-safe record getter
function getPath<T, P extends Path<T>>(obj: T, path: P): PathValue<T, P> {
  return path.reduce((acc, key) => acc[key], obj as any) as PathValue<T, P>
}

// Branded types for compile-time nominal typing
declare const brand: unique symbol
type Brand<T, B> = T & { [brand]: B }

type UserId = Brand<string, "UserId">
type OrderId = Brand<string, "OrderId">

// Type-safe branding functions
function brandUserId(id: string): UserId {
  // Runtime validation would go here
  return id as UserId
}

function brandOrderId(id: string): OrderId {
  // Runtime validation would go here
  return id as OrderId
}

// Sample usage
interface User {
  id: UserId
  name: string
  permissions: {
    roles: string[]
    features: {
      admin: boolean
      developer: boolean
    }
  }
  orders: OrderId[]
}

// Type registry for code generation systems
class TypeRegistry<Base = any> {
  private types = new Map<string, Record<string, any>>()

  register<T extends Base>(name: string, shape: T) {
    this.types.set(name, shape)
    return this
  }

  get<T extends Base>(name: string): T {
    const type = this.types.get(name)
    if (!type) throw new Error(`Type ${name} not registered`)
    return type as T
  }

  generateTypeDefinitions(): string {
    let output = ""
    
    for (const [name, shape] of this.types.entries()) {
      output += `export interface ${name} {\n`
      
      for (const [key, value] of Object.entries(shape)) {
        const typeStr = this.getTypeString(value)
        output += `  ${key}: ${typeStr};\n`
      }
      
      output += "}\n\n"
    }
    
    return output
  }

  private getTypeString(value: any): string {
    if (value === null) return "null"
    
    switch (typeof value) {
      case "string":
        return "string"
      case "number":
        return "number"
      case "boolean":
        return "boolean"
      case "object":
        if (Array.isArray(value)) {
          if (value.length === 0) return "any[]"
          return `${this.getTypeString(value[0])}[]`
        }
        
        const entries = Object.entries(value)
        if (entries.length === 0) return "{}"
        
        return `{\n${entries
          .map(([k, v]) => `    ${k}: ${this.getTypeString(v)}`)
          .join(";\n")}\n  }`
      default:
        return "any"
    }
  }
}

// Demonstration
function demo() {
  // Using branded types
  const userId = brandUserId("user-123")
  const orderId = brandOrderId("order-456")
  
  // This would cause a type error:
  // const badUsage: UserId = orderId
  
  // Create a user with nested structure
  const user: User = {
    id: userId,
    name: "Alice",
    permissions: {
      roles: ["admin", "user"],
      features: {
        admin: true,
        developer: true
      }
    },
    orders: [orderId]
  }
  
  // Use path-based access with type safety
  const isAdmin = getPath(user, ["permissions", "features", "admin"])
  console.log(`User is admin: ${isAdmin}`)
  
  // This would cause a type error (path doesn't exist):
  // const invalid = getPath(user, ["permissions", "nonexistent"])
  
  // Type registry for code generation
  const registry = new TypeRegistry()
  
  registry
    .register("Product", {
      id: "",
      name: "",
      price: 0,
      tags: [""]
    })
    .register("Order", {
      id: "",
      products: [{ id: "", quantity: 0 }],
      customer: {
        id: "",
        name: ""
      }
    })
  
  console.log("Generated type definitions:")
  console.log(registry.generateTypeDefinitions())
}

demo()
