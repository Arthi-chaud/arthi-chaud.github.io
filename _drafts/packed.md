---
title: Packed Data support in Haskell 
date: 2025-XX-XX 
tags: [Meelo]
description: Packed Data x Haskell = Type-safety, performance and portability
---

Blog Post = TLDR of Paper published in ECOOP 25

# Introduction: Packed Data

When programs what to persist data or send it over the network, they need to serialise it (e.g. using JSON or XML). On the other hand, when a program receives data (e.g. from the network) or reads it from a file, data needs to be deserialised.

These de/serialisation steps are necessary because the in-memory representation/layout of the data cannot be used to save if to a file, or send it over the network, mainly because of pointers. 

Additionally, de/serialising data has a cost: it takes time, and the serialised version of the data is usually bigger than its in-memory representation.

Now, what if we didn't have to serialise the data before sending it to a client, and what if the client could use the data as-is, without any marshalling step?
We could save some time, on both the server and client side. Binary, 'packed' formats would allow this. 

Cap'n Proto[^1] is a popular library that allows packing data (i.e. build a binary buffer containing the data), using that buffer as-is (e.g. traverse it and 'unpack' a specific field of a structure).

[^1]: [Cap'n Proto website](https://capnproto.org)

No languages support packed data natively. Haskell does support compact normal forms, but the `Compact` data type does not allow using (e.g. deconstructing) packed values as-is. 
We should note the existence of Gibbon, a research compiler that accepts functional programs and generates programs that use packed data natively.

Packed data remains to be leveraged by mainstream programming languages. The performance improvements enabled by this data format remain limited to experimental research projects.

In this post, I will introduce the `packed-data` Haskell library. It allows packing and unpacking data, as well as traversing packed data (e.g. to a switch case on constructors) as-is, with no marshalling step, thanks to the power of types. 

This blog post is a short version of the paper published at ECOOP 2025, titled _Type-safe and portable support for packed data_

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


