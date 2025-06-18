/**
 * Advanced type-safe API design patterns
 * Techniques used by principal engineers to design extensible APIs
 */

// TypeScript builder pattern with full type inference and fluent API
class QueryBuilder<
  Entity extends Record<string, any>,
  Selected extends keyof Entity = keyof Entity,
  Filtered extends Partial<Record<keyof Entity, any>> = {},
  Sorted extends Array<[keyof Entity, 'asc' | 'desc']> = []
> {
  private selectedFields: Selected[] = [] as unknown as Selected[]
  private filters: Filtered = {} as Filtered
  private sorts: Sorted = [] as unknown as Sorted
  private limitValue?: number
  private offsetValue?: number

  // Select specific fields
  select<F extends keyof Entity>(
    ...fields: F[]
  ): QueryBuilder<Entity, F, Filtered, Sorted> {
    return new QueryBuilder<Entity, F, Filtered, Sorted>()
      .withSelectedFields(fields as unknown as Selected[])
      .withFilters(this.filters)
      .withSorts(this.sorts)
      .withLimit(this.limitValue)
      .withOffset(this.offsetValue)
  }

  // Add a filter condition
  where<F extends keyof Entity, V extends Entity[F]>(
    field: F,
    value: V
  ): QueryBuilder<Entity, Selected, Filtered & Record<F, V>, Sorted> {
    const newFilters = {
      ...this.filters,
      [field]: value
    } as Filtered & Record<F, V>

    return new QueryBuilder<Entity, Selected, Filtered & Record<F, V>, Sorted>()
      .withSelectedFields(this.selectedFields)
      .withFilters(newFilters)
      .withSorts(this.sorts)
      .withLimit(this.limitValue)
      .withOffset(this.offsetValue)
  }

  // Add a sort directive
  orderBy<F extends keyof Entity>(
    field: F,
    direction: 'asc' | 'desc' = 'asc'
  ): QueryBuilder<Entity, Selected, Filtered, [...Sorted, [F, 'asc' | 'desc']]> {
    const newSorts = [
      ...this.sorts,
      [field, direction]
    ] as unknown as [...Sorted, [F, 'asc' | 'desc']]

    return new QueryBuilder<Entity, Selected, Filtered, [...Sorted, [F, 'asc' | 'desc']]>()
      .withSelectedFields(this.selectedFields)
      .withFilters(this.filters)
      .withSorts(newSorts)
      .withLimit(this.limitValue)
      .withOffset(this.offsetValue)
  }

  // Add pagination
  limit(value: number): this {
    this.limitValue = value
    return this
  }

  offset(value: number): this {
    this.offsetValue = value
    return this
  }

  // Internal methods to clone state
  private withSelectedFields(fields: Selected[]): this {
    this.selectedFields = fields
    return this
  }

  private withFilters(filters: Filtered): this {
    this.filters = filters
    return this
  }

  private withSorts(sorts: Sorted): this {
    this.sorts = sorts
    return this
  }

  private withLimit(limit?: number): this {
    this.limitValue = limit
    return this
  }

  private withOffset(offset?: number): this {
    this.offsetValue = offset
    return this
  }

  // Execute the query and return results with inferred type
  async execute(): Promise<
    { [K in Selected]: Entity[K] }[]
  > {
    // In a real implementation, this would make a DB call
    console.log('Executing query with:')
    console.log('Selected fields:', this.selectedFields)
    console.log('Filters:', this.filters)
    console.log('Sorts:', this.sorts)
    console.log('Limit:', this.limitValue)
    console.log('Offset:', this.offsetValue)

    // Return mock data
    return [] as unknown as { [K in Selected]: Entity[K] }[]
  }

  // Build a query string (for debugging or use with raw queries)
  toSql(): string {
    const selectClause = this.selectedFields.length > 0
      ? this.selectedFields.join(', ')
      : '*'

    const whereConditions = Object.entries(this.filters)
      .map(([field, value]) => {
        if (typeof value === 'string') {
          return `${field} = '${value}'`
        }
        return `${field} = ${value}`
      })
      .join(' AND ')

    const whereClause = whereConditions ? `WHERE ${whereConditions}` : ''

    const orderClause = this.sorts.length > 0
      ? `ORDER BY ${this.sorts
          .map(([field, direction]) => `${field} ${direction}`)
          .join(', ')}`
      : ''

    const limitClause = this.limitValue ? `LIMIT ${this.limitValue}` : ''
    const offsetClause = this.offsetValue ? `OFFSET ${this.offsetValue}` : ''

    return `SELECT ${selectClause} FROM Entity ${whereClause} ${orderClause} ${limitClause} ${offsetClause}`.trim()
  }
}

// Type-safe event emitter with strictly typed events
interface EventMap {
  [event: string]: any
}

class TypedEventEmitter<Events extends EventMap> {
  private listeners: {
    [E in keyof Events]?: Array<(data: Events[E]) => void>
  } = {}

  // Add an event listener
  on<E extends keyof Events>(event: E, listener: (data: Events[E]) => void): this {
    if (!this.listeners[event]) {
      this.listeners[event] = []
    }
    this.listeners[event]!.push(listener)
    return this
  }

  // Remove an event listener
  off<E extends keyof Events>(event: E, listener: (data: Events[E]) => void): this {
    if (!this.listeners[event]) {
      return this
    }
    
    const idx = this.listeners[event]!.indexOf(listener)
    if (idx !== -1) {
      this.listeners[event]!.splice(idx, 1)
    }
    return this
  }

  // Emit an event
  emit<E extends keyof Events>(event: E, data: Events[E]): boolean {
    if (!this.listeners[event]) {
      return false
    }
    
    this.listeners[event]!.forEach(listener => {
      try {
        listener(data)
      } catch (err) {
        console.error(`Error in listener for event ${String(event)}:`, err)
      }
    })
    
    return true
  }

  // Get the count of listeners for an event
  listenerCount<E extends keyof Events>(event: E): number {
    return this.listeners[event]?.length || 0
  }
}

// Usage demonstration
function apiDemo() {
  // Define entity type for our query builder
  interface User {
    id: number
    name: string
    email: string
    age: number
    isActive: boolean
    role: 'admin' | 'user' | 'guest'
    createdAt: Date
  }

  // Create a query
  const query = new QueryBuilder<User>()
    .select('name', 'email', 'role')
    .where('isActive', true)
    .where('role', 'admin')
    .orderBy('createdAt', 'desc')
    .limit(10)
    .offset(20)

  // Show the generated SQL
  console.log('\nGenerated query:')
  console.log(query.toSql())

  // Define event types for our typed event emitter
  interface AppEvents {
    'user:login': { userId: number; timestamp: Date }
    'user:logout': { userId: number; timestamp: Date }
    'data:updated': { resource: string; ids: number[] }
    'error': { code: number; message: string }
  }

  // Create an event emitter
  const events = new TypedEventEmitter<AppEvents>()

  // Add type-safe event listeners
  events.on('user:login', ({ userId, timestamp }) => {
    console.log(`User ${userId} logged in at ${timestamp}`)
  })

  events.on('error', ({ code, message }) => {
    console.error(`Error ${code}: ${message}`)
  })

  // Emit events
  console.log('\nEmitting events:')
  events.emit('user:login', { 
    userId: 123, 
    timestamp: new Date() 
  })

  events.emit('error', { 
    code: 500, 
    message: 'Internal server error' 
  })

  // This would cause a type error:
  // events.emit('user:login', { userId: '123' })
  // events.emit('unknown-event', {})
}

apiDemo()
