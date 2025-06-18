//! Advanced ownership and memory management techniques
//! 
//! For systems with specific memory requirements and constraints

#![feature(allocator_api)]
#![feature(slice_ptr_get)]
#![feature(slice_ptr_len)]
#![feature(strict_provenance)]

use std::alloc::{Allocator, Global, Layout};
use std::cell::{Cell, RefCell, UnsafeCell};
use std::mem::{self, MaybeUninit};
use std::ptr::{self, NonNull};
use std::rc::Rc;
use std::sync::Arc;

// Custom arena allocator for reducing allocation overhead
struct BumpAllocator {
    // Memory arena
    arena: UnsafeCell<Vec<u8>>,
    // Current allocation position
    position: Cell<usize>,
}

unsafe impl Allocator for BumpAllocator {
    fn allocate(&self, layout: Layout) -> Result<NonNull<[u8]>, std::alloc::AllocError> {
        let arena = unsafe { &mut *self.arena.get() };
        let position = self.position.get();
        
        // Ensure alignment
        let align_offset = position % layout.align();
        let aligned_position = if align_offset == 0 {
            position
        } else {
            position + (layout.align() - align_offset)
        };
        
        // Calculate new position
        let new_position = aligned_position + layout.size();
        
        // Check if we have enough space
        if new_position > arena.len() {
            // Not enough space, fall back to global allocator
            let layout = Layout::from_size_align(
                layout.size().max(4096),
                layout.align()
            ).unwrap();
            
            // Allocate new memory
            let new_capacity = arena.len() + layout.size();
            arena.reserve(new_capacity - arena.len());
            arena.resize(new_capacity, 0);
        }
        
        // Update position
        self.position.set(new_position);
        
        // Return pointer to allocated memory
        let ptr = unsafe { NonNull::new_unchecked(arena.as_mut_ptr().add(aligned_position)) };
        let ptr = NonNull::slice_from_raw_parts(ptr, layout.size());
        Ok(ptr)
    }
    
    unsafe fn deallocate(&self, _ptr: NonNull<u8>, _layout: Layout) {
        // No-op: memory is only freed when the allocator is dropped
    }
}

impl BumpAllocator {
    fn new(initial_capacity: usize) -> Self {
        Self {
            arena: UnsafeCell::new(Vec::with_capacity(initial_capacity)),
            position: Cell::new(0),
        }
    }
    
    fn reset(&self) {
        self.position.set(0);
    }
}

// Example of a custom data structure that uses arena allocation
struct ArenaVec<T, A: Allocator = Global> {
    ptr: NonNull<T>,
    len: usize,
    capacity: usize,
    allocator: A,
}

impl<T> ArenaVec<T, BumpAllocator> {
    fn new(allocator: BumpAllocator) -> Self {
        Self {
            ptr: NonNull::dangling(),
            len: 0,
            capacity: 0,
            allocator,
        }
    }
    
    fn push(&mut self, value: T) {
        if self.len == self.capacity {
            // Need to reallocate
            let new_capacity = if self.capacity == 0 { 4 } else { self.capacity * 2 };
            let new_layout = Layout::array::<T>(new_capacity).unwrap();
            
            let new_ptr = match self.allocator.allocate(new_layout) {
                Ok(ptr) => ptr.cast::<T>().as_ptr(),
                Err(_) => panic!("Failed to allocate memory"),
            };
            
            // Copy existing elements if any
            if self.len > 0 {
                unsafe {
                    ptr::copy_nonoverlapping(self.ptr.as_ptr(), new_ptr, self.len);
                }
            }
            
            self.ptr = unsafe { NonNull::new_unchecked(new_ptr) };
            self.capacity = new_capacity;
        }
        
        // Add new element
        unsafe {
            ptr::write(self.ptr.as_ptr().add(self.len), value);
        }
        self.len += 1;
    }
    
    fn get(&self, index: usize) -> Option<&T> {
        if index >= self.len {
            None
        } else {
            unsafe {
                Some(&*self.ptr.as_ptr().add(index))
            }
        }
    }
}

impl<T, A: Allocator> Drop for ArenaVec<T, A> {
    fn drop(&mut self) {
        // Drop all elements
        for i in 0..self.len {
            unsafe {
                ptr::drop_in_place(self.ptr.as_ptr().add(i));
            }
        }
        
        // No need to deallocate when using BumpAllocator
        // It will be reset or freed when the allocator itself is dropped
    }
}

// Safe interior mutability with compile-time guarantees
struct Slot<T> {
    // Data is wrapped in UnsafeCell to allow interior mutability
    data: UnsafeCell<T>,
    // Flag to track if data is borrowed
    borrowed: Cell<bool>,
}

impl<T> Slot<T> {
    fn new(value: T) -> Self {
        Self {
            data: UnsafeCell::new(value),
            borrowed: Cell::new(false),
        }
    }
    
    // Safe borrow method
    fn borrow(&self) -> Option<&T> {
        if self.borrowed.get() {
            None
        } else {
            self.borrowed.set(true);
            // Safety: We've checked and set the borrowed flag
            Some(unsafe { &*self.data.get() })
        }
    }
    
    // Safe mutable borrow method
    fn borrow_mut(&self) -> Option<&mut T> {
        if self.borrowed.get() {
            None
        } else {
            self.borrowed.set(true);
            // Safety: We've checked and set the borrowed flag
            Some(unsafe { &mut *self.data.get() })
        }
    }
    
    // Release the borrow
    fn release(&self) {
        self.borrowed.set(false);
    }
}

impl<T> Drop for Slot<T> {
    fn drop(&mut self) {
        // No need to do anything special here
        // The UnsafeCell will drop the inner value
    }
}

// Type for implementing double-ended shared mutable queues
struct SharedQueue<T> {
    data: RefCell<Vec<T>>,
}

impl<T> SharedQueue<T> {
    fn new() -> Self {
        Self {
            data: RefCell::new(Vec::new()),
        }
    }
    
    fn push_back(&self, value: T) {
        self.data.borrow_mut().push(value);
    }
    
    fn push_front(&self, value: T) {
        self.data.borrow_mut().insert(0, value);
    }
    
    fn pop_back(&self) -> Option<T> {
        self.data.borrow_mut().pop()
    }
    
    fn pop_front(&self) -> Option<T> {
        if self.data.borrow().is_empty() {
            None
        } else {
            Some(self.data.borrow_mut().remove(0))
        }
    }
    
    fn len(&self) -> usize {
        self.data.borrow().len()
    }
}

// Type-safe work stealing queue for parallel processing
struct WorkStealingQueue<T> {
    local: RefCell<Vec<T>>,
    shared: Arc<SharedQueue<T>>,
}

impl<T> WorkStealingQueue<T> {
    fn new() -> Self {
        Self {
            local: RefCell::new(Vec::new()),
            shared: Arc::new(SharedQueue::new()),
        }
    }
    
    fn clone_stealer(&self) -> WorkStealer<T> {
        WorkStealer {
            shared: Arc::clone(&self.shared),
        }
    }
    
    fn push(&self, value: T) {
        self.local.borrow_mut().push(value);
    }
    
    fn pop(&self) -> Option<T> {
        // Try local queue first
        if let Some(value) = self.local.borrow_mut().pop() {
            return Some(value);
        }
        
        // Try stealing from shared queue
        self.shared.pop_back()
    }
    
    fn share(&self) {
        // Move some items to shared queue
        let mut local = self.local.borrow_mut();
        if local.len() > 1 {
            let half = local.len() / 2;
            for _ in 0..half {
                if let Some(value) = local.pop() {
                    self.shared.push_back(value);
                }
            }
        }
    }
}

struct WorkStealer<T> {
    shared: Arc<SharedQueue<T>>,
}

impl<T> WorkStealer<T> {
    fn steal(&self) -> Option<T> {
        self.shared.pop_front()
    }
}

// Memory pool for object reuse to avoid allocations
struct Pool<T> {
    objects: RefCell<Vec<T>>,
}

impl<T> Pool<T> {
    fn new() -> Self {
        Self {
            objects: RefCell::new(Vec::new()),
        }
    }
    
    fn acquire<F>(&self, factory: F) -> PoolGuard<T>
    where
        F: FnOnce() -> T,
    {
        let obj = self.objects.borrow_mut().pop().unwrap_or_else(factory);
        PoolGuard {
            obj: Some(obj),
            pool: self,
        }
    }
    
    fn return_object(&self, obj: T) {
        self.objects.borrow_mut().push(obj);
    }
}

struct PoolGuard<'a, T> {
    obj: Option<T>,
    pool: &'a Pool<T>,
}

impl<'a, T> std::ops::Deref for PoolGuard<'a, T> {
    type Target = T;
    
    fn deref(&self) -> &Self::Target {
        self.obj.as_ref().unwrap()
    }
}

impl<'a, T> std::ops::DerefMut for PoolGuard<'a, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.obj.as_mut().unwrap()
    }
}

impl<'a, T> Drop for PoolGuard<'a, T> {
    fn drop(&mut self) {
        if let Some(obj) = self.obj.take() {
            self.pool.return_object(obj);
        }
    }
}

// Customizable reference counting with hooks for memory reclamation
struct CustomRc<T> {
    data: NonNull<RcData<T>>,
}

struct RcData<T> {
    strong: Cell<usize>,
    weak: Cell<usize>,
    value: T,
}

impl<T> CustomRc<T> {
    fn new(value: T) -> Self {
        let data = Box::new(RcData {
            strong: Cell::new(1),
            weak: Cell::new(0),
            value,
        });
        
        Self {
            data: unsafe { NonNull::new_unchecked(Box::into_raw(data)) },
        }
    }
    
    fn strong_count(&self) -> usize {
        unsafe { (*self.data.as_ptr()).strong.get() }
    }
    
    fn weak_count(&self) -> usize {
        unsafe { (*self.data.as_ptr()).weak.get() }
    }
    
    fn downgrade(&self) -> CustomWeak<T> {
        unsafe {
            let weak = (*self.data.as_ptr()).weak.get();
            (*self.data.as_ptr()).weak.set(weak + 1);
        }
        
        CustomWeak { data: self.data }
    }
    
    fn get_ref(&self) -> &T {
        unsafe { &(*self.data.as_ptr()).value }
    }
}

impl<T> Clone for CustomRc<T> {
    fn clone(&self) -> Self {
        unsafe {
            let strong = (*self.data.as_ptr()).strong.get();
            (*self.data.as_ptr()).strong.set(strong + 1);
        }
        
        Self { data: self.data }
    }
}

impl<T> Drop for CustomRc<T> {
    fn drop(&mut self) {
        unsafe {
            let strong = (*self.data.as_ptr()).strong.get();
            if strong == 1 {
                // Last strong reference, drop the value
                let weak = (*self.data.as_ptr()).weak.get();
                if weak == 0 {
                    // No weak references, free the memory
                    drop(Box::from_raw(self.data.as_ptr()));
                } else {
                    // Still have weak references, just drop the value
                    ptr::drop_in_place(&mut (*self.data.as_ptr()).value);
                    (*self.data.as_ptr()).strong.set(0);
                }
            } else {
                // More strong references exist
                (*self.data.as_ptr()).strong.set(strong - 1);
            }
        }
    }
}

struct CustomWeak<T> {
    data: NonNull<RcData<T>>,
}

impl<T> CustomWeak<T> {
    fn upgrade(&self) -> Option<CustomRc<T>> {
        unsafe {
            let strong = (*self.data.as_ptr()).strong.get();
            if strong == 0 {
                // Original value was dropped
                None
            } else {
                // Increment strong count
                (*self.data.as_ptr()).strong.set(strong + 1);
                Some(CustomRc { data: self.data })
            }
        }
    }
}

impl<T> Clone for CustomWeak<T> {
    fn clone(&self) -> Self {
        unsafe {
            let weak = (*self.data.as_ptr()).weak.get();
            (*self.data.as_ptr()).weak.set(weak + 1);
        }
        
        Self { data: self.data }
    }
}

impl<T> Drop for CustomWeak<T> {
    fn drop(&mut self) {
        unsafe {
            let weak = (*self.data.as_ptr()).weak.get();
            if weak == 1 {
                // Last weak reference
                let strong = (*self.data.as_ptr()).strong.get();
                if strong == 0 {
                    // No strong references, free the memory
                    drop(Box::from_raw(self.data.as_ptr()));
                } else {
                    // Still have strong references
                    (*self.data.as_ptr()).weak.set(0);
                }
            } else {
                // More weak references exist
                (*self.data.as_ptr()).weak.set(weak - 1);
            }
        }
    }
}

// Uninitialized memory manipulation for performance-critical code
struct RingBuffer<T> {
    buffer: Box<[MaybeUninit<T>]>,
    head: usize,
    tail: usize,
    capacity: usize,
    len: usize,
}

impl<T> RingBuffer<T> {
    fn new(capacity: usize) -> Self {
        let mut buffer = Vec::with_capacity(capacity);
        // Safety: We're creating uninitialized memory, which is valid for MaybeUninit
        buffer.resize_with(capacity, || MaybeUninit::uninit());
        
        Self {
            buffer: buffer.into_boxed_slice(),
            head: 0,
            tail: 0,
            capacity,
            len: 0,
        }
    }
    
    fn push_back(&mut self, value: T) -> Result<(), T> {
        if self.len == self.capacity {
            return Err(value);
        }
        
        // Write value to buffer
        self.buffer[self.tail] = MaybeUninit::new(value);
        
        // Update tail and length
        self.tail = (self.tail + 1) % self.capacity;
        self.len += 1;
        
        Ok(())
    }
    
    fn pop_front(&mut self) -> Option<T> {
        if self.len == 0 {
            return None;
        }
        
        // Read value from buffer
        let value = unsafe {
            // Safety: We know this slot is initialized
            ptr::read(self.buffer[self.head].as_ptr())
        };
        
        // Update head and length
        self.head = (self.head + 1) % self.capacity;
        self.len -= 1;
        
        Some(value)
    }
    
    fn len(&self) -> usize {
        self.len
    }
    
    fn capacity(&self) -> usize {
        self.capacity
    }
}

impl<T> Drop for RingBuffer<T> {
    fn drop(&mut self) {
        // Drop all initialized elements
        while self.pop_front().is_some() {}
    }
}

fn main() {
    // Example 1: Custom Arena Allocator
    println!("=== Arena Allocator Example ===");
    let allocator = BumpAllocator::new(1024);
    let mut vec = ArenaVec::<i32, _>::new(allocator);
    
    for i in 0..10 {
        vec.push(i);
    }
    
    for i in 0..10 {
        println!("vec[{}] = {:?}", i, vec.get(i));
    }
    
    // Example 2: Slot for safe interior mutability
    println!("\n=== Slot Example ===");
    let slot = Slot::new(42);
    
    {
        let value = slot.borrow().unwrap();
        println!("Borrowed value: {}", value);
        // This would fail at runtime:
        // let value_mut = slot.borrow_mut().unwrap();
    }
    
    slot.release();
    
    {
        let value_mut = slot.borrow_mut().unwrap();
        println!("Borrowed mutable value: {}", value_mut);
    }
    
    slot.release();
    
    // Example 3: Work Stealing Queue
    println!("\n=== Work Stealing Queue Example ===");
    let queue = WorkStealingQueue::new();
    let stealer = queue.clone_stealer();
    
    // Push items
    for i in 0..5 {
        queue.push(i);
    }
    
    // Share work
    queue.share();
    
    // Pop from local
    println!("Popped: {:?}", queue.pop());
    
    // Steal work
    println!("Stolen: {:?}", stealer.steal());
    
    // Example 4: Object Pool
    println!("\n=== Object Pool Example ===");
    let pool = Pool::new();
    
    {
        let mut obj1 = pool.acquire(|| Vec::new());
        obj1.push(1);
        obj1.push(2);
        println!("Using object from pool: {:?}", *obj1);
    } // obj1 returned to pool
    
    {
        let mut obj2 = pool.acquire(|| Vec::new());
        println!("Reused object from pool: {:?}", *obj2);
        obj2.push(3);
        println!("After modification: {:?}", *obj2);
    } // obj2 returned to pool
    
    // Example 5: Custom Reference Counting
    println!("\n=== Custom Rc Example ===");
    let rc1 = CustomRc::new(String::from("Hello, world!"));
    let rc2 = rc1.clone();
    let weak = rc1.downgrade();
    
    println!("Strong count: {}", rc1.strong_count());
    println!("Weak count: {}", rc1.weak_count());
    println!("Value: {}", rc1.get_ref());
    
    drop(rc1);
    println!("After dropping rc1 - Strong count: {}", rc2.strong_count());
    
    let rc3 = weak.upgrade().unwrap();
    println!("Upgraded weak to rc3 - Value: {}", rc3.get_ref());
    
    // Example 6: Ring Buffer
    println!("\n=== Ring Buffer Example ===");
    let mut buffer: RingBuffer<String> = RingBuffer::new(3);
    
    buffer.push_back(String::from("Item 1")).unwrap();
    buffer.push_back(String::from("Item 2")).unwrap();
    buffer.push_back(String::from("Item 3")).unwrap();
    
    // Buffer is full
    let overflow = buffer.push_back(String::from("Overflow"));
    println!("Push result: {:?}", overflow.err().unwrap());
    
    println!("Buffer size: {}/{}", buffer.len(), buffer.capacity());
    
    // Pop items
    while let Some(item) = buffer.pop_front() {
        println!("Popped: {}", item);
    }
}
