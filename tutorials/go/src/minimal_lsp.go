package main

import (
	"encoding/json"
	"fmt"
	"io"
	"log"
	"os"
	"strings"
)

// LSP message structures
type Message struct {
	JsonRPC string      `json:"jsonrpc"`
	ID      interface{} `json:"id,omitempty"`
	Method  string      `json:"method,omitempty"`
	Params  interface{} `json:"params,omitempty"`
	Result  interface{} `json:"result,omitempty"`
	Error   *Error      `json:"error,omitempty"`
}

type Error struct {
	Code    int         `json:"code"`
	Message string      `json:"message"`
	Data    interface{} `json:"data,omitempty"`
}

// Minimal language server implementation
func main() {
	log.SetOutput(os.Stderr)
	
	// Set up channels for communication
	inChan := make(chan Message)
	outChan := make(chan Message)
	
	// Start reader
	go readMessages(os.Stdin, inChan)
	
	// Start writer
	go writeMessages(os.Stdout, outChan)
	
	// Main loop - process messages
	for msg := range inChan {
		switch msg.Method {
		case "initialize":
			// Respond with capabilities
			outChan <- Message{
				JsonRPC: "2.0",
				ID:      msg.ID,
				Result: map[string]interface{}{
					"capabilities": map[string]interface{}{
						"textDocumentSync": 1, // Full sync
						"completionProvider": map[string]interface{}{
							"triggerCharacters": []string{".", ":"},
						},
						"hoverProvider": true,
					},
				},
			}
			
		case "textDocument/completion":
			// Provide completions
			outChan <- Message{
				JsonRPC: "2.0",
				ID:      msg.ID,
				Result: map[string]interface{}{
					"items": []map[string]interface{}{
						{
							"label": "fmt",
							"kind":  9, // Module
							"detail": "Standard library package",
						},
						{
							"label": "Println",
							"kind":  3, // Function
							"detail": "func Println(a ...interface{}) (n int, err error)",
						},
					},
				},
			}
			
		case "shutdown":
			outChan <- Message{
				JsonRPC: "2.0",
				ID:      msg.ID,
				Result:  nil,
			}
			
		case "exit":
			close(outChan)
			return
			
		default:
			// Unhandled method
			log.Printf("Unhandled method: %s", msg.Method)
			if msg.ID != nil {
				outChan <- Message{
					JsonRPC: "2.0",
					ID:      msg.ID,
					Error: &Error{
						Code:    -32601,
						Message: "Method not found",
					},
				}
			}
		}
	}
}

// Read LSP messages from reader
func readMessages(r io.Reader, ch chan<- Message) {
	defer close(ch)
	
	buf := make([]byte, 1024)
	
	for {
		// Read header
		header := ""
		for !strings.Contains(header, "\r\n\r\n") {
			n, err := r.Read(buf)
			if err != nil {
				if err != io.EOF {
					log.Printf("Error reading: %v", err)
				}
				return
			}
			header += string(buf[:n])
		}
		
		// Parse Content-Length
		parts := strings.Split(header, "\r\n")
		contentLength := 0
		for _, part := range parts {
			if strings.HasPrefix(part, "Content-Length: ") {
				fmt.Sscanf(part, "Content-Length: %d", &contentLength)
				break
			}
		}
		
		if contentLength == 0 {
			log.Printf("Invalid header: %s", header)
			continue
		}
		
		// Read content
		content := make([]byte, contentLength)
		_, err := io.ReadFull(r, content)
		if err != nil {
			log.Printf("Error reading content: %v", err)
			return
		}
		
		// Parse message
		var msg Message
		if err := json.Unmarshal(content, &msg); err != nil {
			log.Printf("Error parsing message: %v", err)
			continue
		}
		
		ch <- msg
	}
}

// Write LSP messages to writer
func writeMessages(w io.Writer, ch <-chan Message) {
	for msg := range ch {
		content, err := json.Marshal(msg)
		if err != nil {
			log.Printf("Error marshaling message: %v", err)
			continue
		}
		
		header := fmt.Sprintf("Content-Length: %d\r\n\r\n", len(content))
		
		if _, err := w.Write([]byte(header)); err != nil {
			log.Printf("Error writing header: %v", err)
			return
		}
		
		if _, err := w.Write(content); err != nil {
			log.Printf("Error writing content: %v", err)
			return
		}
	}
}
