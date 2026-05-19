---
title: Core Concepts
sidebar:
  order: 0
---

Choreo is structured around a three-layer architecture. You write a single global protocol description as a **choreography**, which is automatically projected into per-participant **network** programs, then executed within a concrete **effect** monad.

```
Choreography (Choreo[M, A])          -- global protocol DSL
    |  Endpoint.project
    v
Network (Network[M, A])              -- per-location network operations
    |  backend.runNetwork
    v
Effect (M[A])                        -- concrete execution (e.g. IO)
```

## Choreography Layer

The choreography layer provides a DSL for expressing the global behavior of a distributed system. Operations like `locally`, `send/to`, `cond`, `select`, and `|*|` describe what each participant does and how they interact. The choreography is parameterized by an effect type `M[_]` (typically `cats.effect.IO`) used for local computations.

A `Choreo[M, A]` is a free monad over `ChoreoSig[M, _]`, which means choreographies compose naturally using for-comprehensions.

## Network Layer

The network layer represents the program that a single participant executes. It consists of low-level operations: `Run` (execute a local effect), `Send` (send a message), `Recv` (receive a message), `Broadcast` (send to all), and `Par` (run two network programs in parallel).

The `Endpoint.project` function transforms a global choreography into a network program for a given location, following well-defined projection rules.

## Effect Layer

A backend interprets `Network[M, A]` into the effect monad `M[A]`. Choreo provides two backends:

- **LocalBackend** -- runs all participants in a single process using in-memory queues. Useful for testing and development.
- **TcpBackend** -- connects participants over TCP sockets with serialized messages. Suitable for distributed deployment.

## Topics

- [Locations](locations/) -- the type-level mechanism for tracking data ownership.
- [Communication](communication/) -- local computation and point-to-point messaging.
- [Branching](branching/) -- conditional execution and label-based selection.
- [Parallel Composition](parallel/) -- running independent choreographies concurrently.
- [Endpoint Projection](endpoint-projection/) -- how global choreographies become local network programs.
