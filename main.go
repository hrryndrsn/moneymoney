package main

import (
	"log"
	"net/http"
)

func main() {
	fs := http.FileServer(http.Dir("."))
	log.Fatal(http.ListenAndServe(":8080", fs))
}
