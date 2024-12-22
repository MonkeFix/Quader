package main

import (
	"fmt"
	"net/http"
	"time"
	ws "github.com/gorilla/websocket"
)

// Upgrader to handle WebSocket upgrades
var upgrader = ws.Upgrader{
	CheckOrigin: func(r *http.Request) bool {
		return true // Allow all origins (for testing)
	},
}

func handleWebSocket(w http.ResponseWriter, r *http.Request) {
	conn, err := upgrader.Upgrade(w, r, nil)
	if err != nil {
		fmt.Println("Error upgrading to WebSocket:", err)
		return
	}
	defer conn.Close()

	// Set the PingHandler to automatically respond with a Pong
	conn.SetPingHandler(func(appData string) error {
		fmt.Println("Received ping")
		// Send a pong response
		return conn.WriteControl(ws.PongMessage, []byte(appData), time.Now().Add(time.Second))
	})

	// Read messages from the client
	for {
		messageType, message, err := conn.ReadMessage()
		if err != nil {
			if ws.IsCloseError(err, ws.CloseNormalClosure, ws.CloseGoingAway) {
				fmt.Println("Connection closed:", err)
				break
			}
			fmt.Println("Error reading message:", err)
			break
		}

		// Print received messages
		fmt.Printf("Received message: %s\n", message)

		// Echo the message back to the client
		if err := conn.WriteMessage(messageType, message); err != nil {
			fmt.Println("Error writing message:", err)
			break
		}
	}
}

func main() {
	fmt.Println("Starting server on localhost:8080...")
	http.HandleFunc("/ws", handleWebSocket)
	http.ListenAndServe(":8080", nil)
}
