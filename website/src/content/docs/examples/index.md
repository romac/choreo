---
title: Examples
sidebar:
  order: 0
---

Choreo ships with worked examples that demonstrate the core concepts of choreographic programming.
Each example defines a global protocol as a `Choreo[IO, A]` value, projects it to per-location
network programs, and runs them concurrently using a backend.

## Running the examples

All examples live in the `examples` module and can be run with Mill:

```shell
# Key-Value Store -- interactive client/server REPL
./mill examples.runMain choreo.examples.kv

# Bookseller -- two-party purchase protocol with branching
./mill examples.runMain choreo.examples.bookseller
```

## Available examples

### [Key-Value Store](kv-store/)

A client/server protocol where the client sends `GET` and `PUT` requests to a
server that maintains an in-memory key-value store. Demonstrates:

- Local computation with `locally`
- Point-to-point communication with `send` / `to`
- Stateful server using `Ref`
- Looping with `foreverM`

### [Bookseller](bookseller/)

A two-party purchase protocol adapted from the choreographic programming
literature. A buyer queries a seller for a book price, then decides whether to
complete the purchase. Demonstrates:

- Multi-step communication between two participants
- Label-based branching with `select`
- Knowledge-of-choice propagation
- Optional result types across locations
