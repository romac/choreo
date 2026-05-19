---
title: Backends
sidebar:
  order: 0
---

A backend provides the communication infrastructure that network programs run on.
After endpoint projection transforms a global `Choreo` into per-location `Network`
programs, a backend interprets the network operations (send, receive, broadcast)
using a concrete transport mechanism.

## The Backend trait

```scala
trait Backend[B, M[_]]:
  extension (backend: B) def runNetwork[A](at: Loc)(network: Network[M, A]): M[A]
  extension (backend: B) def locs: Set[Loc]
```

`Backend[B, M]` is a type class parameterized by:

- `B` -- the backend implementation type (e.g., `LocalBackend[M]` or `TcpBackend[M]`)
- `M[_]` -- the effect type (e.g., `IO`)

It provides two operations:

- `runNetwork(at)(network)` -- Interprets a `Network[M, A]` program at the given
  location, translating `Send`, `Recv`, `Broadcast`, and `Par` operations into
  concrete effects.
- `locs` -- Returns the set of all locations managed by this backend.

## The Channel type

```scala
type Channel = (from: Loc, to: Loc)
```

A `Channel` is a named tuple identifying a directed communication path from one
location to another. Backends use channels to route messages -- for example, the
local backend maintains one queue per channel, and the TCP backend maintains one
socket connection per channel.

## From Choreo to execution

The typical flow from choreography to execution is:

```scala
// 1. Define a global choreography
val choreo: Choreo[IO, A] = ...

// 2. Create a backend
val backend <- Backend.local(List(loc1, loc2))

// 3. Project and run at each location
val task1 = choreo.project(backend, loc1)
val task2 = choreo.project(backend, loc2)

// 4. Run all tasks concurrently
(task1, task2).parTupled
```

The `.project(backend, loc)` extension method on `Choreo` performs two steps internally:

1. **Endpoint projection (EPP)** -- Transforms the global `Choreo` into a `Network`
   program specific to `loc`. Operations involving other locations become sends,
   receives, or no-ops as appropriate.

2. **Interpretation** -- Passes the resulting `Network` program to
   `backend.runNetwork(loc)(...)`, which maps each network operation to concrete
   effects in `M`.

## Available backends

| Backend | Transport | Use case |
|---------|-----------|----------|
| [LocalBackend](local/) | In-memory queues | Testing, demos, single-process execution |
| [TcpBackend](tcp/) | TCP sockets | Distributed execution across processes or machines |
