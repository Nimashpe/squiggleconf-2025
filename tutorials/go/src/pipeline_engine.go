package main

import (
	"context"
	"fmt"
	"sync"
	"time"
)

// Stage represents a processing stage in a pipeline
type Stage func(ctx context.Context, in <-chan interface{}) <-chan interface{}

// Pipeline composes multiple stages
func Pipeline(ctx context.Context, stages ...Stage) Stage {
	return func(ctx context.Context, in <-chan interface{}) <-chan interface{} {
		for _, stage := range stages {
			in = stage(ctx, in)
		}
		return in
	}
}

// FanOut distributes work across multiple goroutines
func FanOut(stage Stage, n int) Stage {
	return func(ctx context.Context, in <-chan interface{}) <-chan interface{} {
		channels := make([]<-chan interface{}, n)
		for i := 0; i < n; i++ {
			channels[i] = stage(ctx, in)
		}
		return MergeChannels(ctx, channels...)
	}
}

// MergeChannels merges multiple channels into one
func MergeChannels(ctx context.Context, channels ...<-chan interface{}) <-chan interface{} {
	var wg sync.WaitGroup
	out := make(chan interface{})
	
	// Start an output goroutine for each input channel
	output := func(c <-chan interface{}) {
		defer wg.Done()
		for n := range c {
			select {
			case out <- n:
			case <-ctx.Done():
				return
			}
		}
	}
	
	wg.Add(len(channels))
	for _, c := range channels {
		go output(c)
	}
	
	// Start a goroutine to close out once all the output goroutines are done
	go func() {
		wg.Wait()
		close(out)
	}()
	
	return out
}

// Example usage for processing large volumes of source code
func main() {
	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()
	
	// Example pipeline for processing source files
	tokenize := func(ctx context.Context, in <-chan interface{}) <-chan interface{} {
		out := make(chan interface{})
		go func() {
			defer close(out)
			for file := range in {
				// Tokenize file content
				out <- fmt.Sprintf("Tokenized: %v", file)
			}
		}()
		return out
	}
	
	parse := func(ctx context.Context, in <-chan interface{}) <-chan interface{} {
		out := make(chan interface{})
		go func() {
			defer close(out)
			for tokens := range in {
				// Parse tokens into AST
				out <- fmt.Sprintf("Parsed: %v", tokens)
			}
		}()
		return out
	}
	
	analyze := func(ctx context.Context, in <-chan interface{}) <-chan interface{} {
		out := make(chan interface{})
		go func() {
			defer close(out)
			for ast := range in {
				// Analyze AST
				out <- fmt.Sprintf("Analyzed: %v", ast)
			}
		}()
		return out
	}
	
	// Create pipeline with fan-out for CPU-intensive parsing stage
	pipeline := Pipeline(ctx,
		tokenize,
		FanOut(parse, 4), // Use 4 goroutines for parsing
		analyze,
	)
	
	// Feed input and collect results
	input := make(chan interface{})
	go func() {
		defer close(input)
		// Simulate feeding source files
		for i := 1; i <= 10; i++ {
			input <- fmt.Sprintf("file%d.go", i)
		}
	}()
	
	// Process results
	for result := range pipeline(ctx, input) {
		fmt.Println(result)
	}
}
