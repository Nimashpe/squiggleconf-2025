/**
 * Implementing an Effect system in TypeScript
 * For enterprise-scale applications that require robust error handling,
 * dependency injection, and composition
 */

// Unique symbol for Effect brand
const EffectSymbol = Symbol('Effect')

// Effect type for representing computations
class Effect<R, E, A> {
  readonly _R!: (_: R) => void
  readonly _E!: () => E
  readonly _A!: () => A
  readonly [EffectSymbol]: true = true

  constructor(readonly run: (env: R) => Promise<Either<E, A>>) {}

  // Functor map
  map<B>(f: (a: A) => B): Effect<R, E, B> {
    return new Effect(env =>
      this.run(env).then(either => either.map(f))
    )
  }

  // Applicative ap
  ap<B>(fab: Effect<R, E, (a: A) => B>): Effect<R, E, B> {
    return fab.flatMap(f => this.map(f))
  }

  // Monad flatMap
  flatMap<R2, E2, B>(
    f: (a: A) => Effect<R2, E2, B>
  ): Effect<R & R2, E | E2, B> {
    return new Effect((env: R & R2) =>
      this.run(env).then(either =>
        either.isRight()
          ? f(either.value).run(env)
          : Promise.resolve(Either.left(either.value))
      )
    )
  }

  // Recover from errors
  catchAll<R2, E2, B>(
    f: (error: E) => Effect<R2, E2, B>
  ): Effect<R & R2, E2, A | B> {
    return new Effect((env: R & R2) =>
      this.run(env).then(either =>
        either.isLeft()
          ? f(either.value).run(env)
          : Promise.resolve(Either.right(either.value))
      )
    )
  }

  // Fold both error and success cases
  fold<R2, E2, B, C>(
    onError: (error: E) => Effect<R2, E2, B>,
    onSuccess: (value: A) => Effect<R2, E2, C>
  ): Effect<R & R2, E2, B | C> {
    return this.catchAll(onError).flatMap(onSuccess)
  }

  // Provide the environment to this effect
  provide(env: R): Effect<unknown, E, A> {
    return new Effect(() => this.run(env))
  }

  // Run the effect with the given environment
  async runPromise(env: R): Promise<A> {
    const result = await this.run(env)
    
    if (result.isLeft()) {
      throw result.value
    }
    
    return result.value
  }

  // Run with a handler for errors
  async runPromiseEither(env: R): Promise<Either<E, A>> {
    return this.run(env)
  }

  // Static combinators
  static succeed<A>(a: A): Effect<unknown, never, A> {
    return new Effect(() => Promise.resolve(Either.right(a)))
  }

  static fail<E>(e: E): Effect<unknown, E, never> {
    return new Effect(() => Promise.resolve(Either.left(e)))
  }

  static fromPromise<A, E = unknown>(
    promise: () => Promise<A>
  ): Effect<unknown, E, A> {
    return new Effect(async () => {
      try {
        const result = await promise()
        return Either.right(result)
      } catch (error) {
        return Either.left(error as E)
      }
    })
  }

  static access<R, A>(f: (r: R) => A): Effect<R, never, A> {
    return new Effect(env => Promise.resolve(Either.right(f(env))))
  }

  static accessM<R, E, A>(f: (r: R) => Effect<R, E, A>): Effect<R, E, A> {
    return new Effect(env => f(env).run(env))
  }

  static all<R, E, A>(effects: Effect<R, E, A>[]): Effect<R, E, A[]> {
    if (effects.length === 0) {
      return Effect.succeed([])
    }

    return new Effect(env =>
      Promise.all(effects.map(effect => effect.run(env))).then(results => {
        const errors = results.filter(r => r.isLeft())
        if (errors.length > 0) {
          return Either.left(errors[0].value)
        }
        
        return Either.right(results.map(r => r.value as A))
      })
    )
  }
}

// Either type for representing success/failure
class Either<L, R> {
  private constructor(
    readonly isLeft: () => boolean,
    readonly value: L | R
  ) {}

  isRight(): boolean {
    return !this.isLeft()
  }

  map<B>(f: (r: R) => B): Either<L, B> {
    return this.isLeft()
      ? Either.left(this.value as L)
      : Either.right(f(this.value as R))
  }

  flatMap<B>(f: (r: R) => Either<L, B>): Either<L, B> {
    return this.isLeft()
      ? Either.left(this.value as L)
      : f(this.value as R)
  }

  getOrElse<B>(defaultValue: B): R | B {
    return this.isLeft() ? defaultValue : (this.value as R)
  }

  fold<B>(onLeft: (l: L) => B, onRight: (r: R) => B): B {
    return this.isLeft()
      ? onLeft(this.value as L)
      : onRight(this.value as R)
  }

  static left<L, R>(l: L): Either<L, R> {
    return new Either(() => true, l)
  }

  static right<L, R>(r: R): Either<L, R> {
    return new Either(() => false, r)
  }
}

// Effect usage example - realistic enterprise-scale application
async function effectDemo() {
  // Define error types for better type safety
  interface DatabaseError {
    type: 'database'
    code: number
    message: string
  }
  
  interface ValidationError {
    type: 'validation'
    field: string
    message: string
  }
  
  interface NotFoundError {
    type: 'not_found'
    id: string
    entity: string
  }
  
  type AppError = DatabaseError | ValidationError | NotFoundError
  
  // Define service dependencies
  interface Logger {
    log(level: 'info' | 'warn' | 'error', message: string, meta?: Record<string, any>): void
  }
  
  interface Database {
    query<T>(sql: string, params?: any[]): Promise<T[]>
    exec(sql: string, params?: any[]): Promise<number>
  }
  
  interface Config {
    databaseUrl: string
    apiKeys: string[]
    features: {
      audit: boolean
      metrics: boolean
    }
  }
  
  // Environment type
  interface AppEnv {
    logger: Logger
    db: Database
    config: Config
  }
  
  // Domain model
  interface User {
    id: string
    email: string
    name: string
    role: string
    createdAt: Date
  }
  
  // Service layer
  class UserService {
    // Get user by ID
    static getUserById(id: string): Effect<AppEnv, AppError, User> {
      return Effect.accessM(({ logger, db }: AppEnv) => {
        logger.log('info', `Getting user with ID: ${id}`)
        
        return Effect.fromPromise<User[], DatabaseError>(() => 
          db.query<User>('SELECT * FROM users WHERE id = ?', [id])
        ).flatMap(users => {
          if (users.length === 0) {
            return Effect.fail<NotFoundError>({
              type: 'not_found',
              id,
              entity: 'user'
            })
          }
          
          return Effect.succeed(users[0])
        })
      })
    }
    
    // Create a new user
    static createUser(email: string, name: string): Effect<AppEnv, AppError, User> {
      return Effect.accessM(({ logger, db, config }: AppEnv) => {
        // Validate input
        if (!email || !email.includes('@')) {
          return Effect.fail<ValidationError>({
            type: 'validation',
            field: 'email',
            message: 'Invalid email address'
          })
        }
        
        if (!name || name.length < 2) {
          return Effect.fail<ValidationError>({
            type: 'validation',
            field: 'name',
            message: 'Name must be at least 2 characters'
          })
        }
        
        logger.log('info', `Creating new user: ${email}`)
        
        // Generate a new user ID
        const id = Math.random().toString(36).substring(2, 15)
        const now = new Date()
        
        const user: User = {
          id,
          email,
          name,
          role: 'user',
          createdAt: now
        }
        
        // Insert into database
        return Effect.fromPromise<number, DatabaseError>(() => 
          db.exec(
            'INSERT INTO users (id, email, name, role, created_at) VALUES (?, ?, ?, ?, ?)',
            [id, email, name, 'user', now.toISOString()]
          )
        ).map(() => {
          // Audit logging if enabled
          if (config.features.audit) {
            logger.log('info', 'User created', { userId: id, action: 'create' })
          }
          
          return user
        })
      })
    }
  }
  
  // Application layer
  async function runApplication() {
    // Mock environment
    const env: AppEnv = {
      logger: {
        log(level, message, meta) {
          console.log(`[${level.toUpperCase()}] ${message}`, meta || '')
        }
      },
      db: {
        async query<T>(sql: string, params?: any[]): Promise<T[]> {
          console.log(`DB Query: ${sql}`, params)
          
          // Mock response for demonstration
          if (sql.includes('WHERE id =') && params && params[0] === '123') {
            return [{
              id: '123',
              email: 'alice@example.com',
              name: 'Alice',
              role: 'admin',
              createdAt: new Date('2025-01-01')
            }] as unknown as T[]
          }
          
          return [] as T[]
        },
        async exec(sql: string, params?: any[]): Promise<number> {
          console.log(`DB Exec: ${sql}`, params)
          return 1 // Affected rows
        }
      },
      config: {
        databaseUrl: 'postgres://localhost:5432/app',
        apiKeys: ['key1', 'key2'],
        features: {
          audit: true,
          metrics: false
        }
      }
    }
    
    console.log('\n=== Getting existing user ===')
    
    try {
      const user = await UserService.getUserById('123').runPromise(env)
      console.log('Found user:', user)
    } catch (error) {
      console.error('Error getting user:', error)
    }
    
    console.log('\n=== Creating new user ===')
    
    try {
      const newUser = await UserService.createUser('bob@example.com', 'Bob').runPromise(env)
      console.log('Created user:', newUser)
    } catch (error) {
      console.error('Error creating user:', error)
    }
    
    console.log('\n=== Validation error example ===')
    
    const invalidResult = await UserService.createUser('invalid', '').runPromiseEither(env)
    invalidResult.fold(
      error => console.error('Got expected validation error:', error),
      user => console.log('Unexpectedly created user:', user)
    )
  }
  
  await runApplication()
}

effectDemo()
