package main

import (
	"fmt"
	"os"
	"runtime"
	"runtime/pprof"
	"time"
)

// Pool of fixed-size byte buffers to minimize GC pressure
type BufferPool struct {
	pool chan []byte
	size int
}

func NewBufferPool(size, count int) *BufferPool {
	return &BufferPool{
		pool: make(chan []byte, count),
		size: size,
	}
}

func (p *BufferPool) Get() []byte {
	select {
	case buf := <-p.pool:
		return buf
	default:
		return make([]byte, p.size)
	}
}

func (p *BufferPool) Put(buf []byte) {
	if len(buf) != p.size {
		return // Wrong size, let it be garbage collected
	}
	
	select {
	case p.pool <- buf:
		// Buffer returned to pool
	default:
		// Pool is full, let it be garbage collected
	}
}

func main() {
	// Create heap profile
	f, err := os.Create("heap.prof")
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error creating heap profile: %v\n", err)
		os.Exit(1)
	}
	defer f.Close()
	
	// Create a buffer pool for 1KB buffers
	pool := NewBufferPool(1024, 100)
	
	// Example workload - process mock data
	processWorkload(pool)
	
	// Take a heap snapshot
	runtime.GC() // Force GC to get accurate memory stats
	if err := pprof.WriteHeapProfile(f); err != nil {
		fmt.Fprintf(os.Stderr, "Error writing heap profile: %v\n", err)
		os.Exit(1)
	}
	
	// Print memory stats
	var m runtime.MemStats
	runtime.ReadMemStats(&m)
	fmt.Printf("Alloc: %v MiB\n", m.Alloc/1024/1024)
	fmt.Printf("TotalAlloc: %v MiB\n", m.TotalAlloc/1024/1024)
	fmt.Printf("Sys: %v MiB\n", m.Sys/1024/1024)
	fmt.Printf("NumGC: %v\n", m.NumGC)
}

func processWorkload(pool *BufferPool) {
	// Simulate processing 10,000 requests
	for i := 0; i < 10000; i++ {
		buf := pool.Get()
		
		// Simulate work
		for j := 0; j < len(buf); j++ {
			buf[j] = byte(j % 256)
		}
		
		// Return buffer to pool
		pool.Put(buf)
		
		// Every 1000 requests, force a GC to simulate pressure
		if i%1000 == 0 {
			runtime.GC()
			time.Sleep(10 * time.Millisecond)
		}
	}
}
