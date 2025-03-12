---
title: Packed Data support in Haskell 
date: 2025-XX-XX 
tags: [Meelo]
description: Packed Data x Haskell = Type-safety, performance and portability
---

Blog Post = TLDR of Paper published in ECOOP 25

# Introduction: Packed Data

JSON, XML, YAML: widespread format to exchange data (over the network, between processes, etc.)
They are human readable -> lot of noise
Size (data in json) >= Size (data in memory)

but widespread + human-readable => easy solution

Alternative: binary format to encode data: reduces noise, smaller payload => faster transfer times
E.g. Protobuf: Encode/decode binary data

Capnproto: Use binary data as is: no deserialisation step => avoid cost of building back the tree of the object at reception
But larger payloads (paddings, indirection, contains layout info)

Binary data = no pointer. Can leverage L1 cache to traverse tree faster. E.g. compilers and DOM

What if there was a unique binary representation/layout between memory and disk, like cap n proto, but the layout information was determined by the client's (and more specifically, the type system):
Introduce Packed data.

Gibbon is the only compiler that uses packed data natively. Uses a subset of Haskell's syntax

Example code with memory layout

OK but what about mainstream languages?

## `packed-data`

What is it
What it does

No changes to the runtime system/compiler => solution is portable

API: Indexed monad
TH: to generate typeclasses to pack/unpack
Flag for indirections

## Example

Example of a sumTree function

Better than pointer arithmetic

## Benchmark

List of benchmark cases from the paper
Results
Screens for stack bench

## Limitations and Future work

Overhead caused by IO
Can provide better perfs

Indirections are too big and sometime superfluous

Pattern matching is a pain

Could be turned into an EDSL


