# Sourcer 

An Erlang preprocessor+scanner+parser tailored for use in an IDE, keeping track of the exact source code in the files. It was originally developed as part of [erlide](https://github.com/erlide/erlide), but it is easier to handle and to get feedback if it is a separate project.

[![Build Status](https://travis-ci.org/erlide/sourcer.svg?branch=master)](https://travis-ci.org/erlide/sourcer) [![Coverage Status](https://coveralls.io/repos/erlide/sourcer/badge.svg?branch=master&service=github)](https://coveralls.io/github/erlide/sourcer?branch=master)

## Rationale

The normal tools for processing source code are aiming to support the compiler and thus are lossy: information that is uninteresting to the compiler is dropped. When supporting editors, cross-referencing tools and other tools, we find ourselves needing to access all the information about the source code.

For example, if sections of a module are conditionally compiled and I am searching for all places where a certain function is called (maybe to rename it), I want to look even into the sections that are invisible to the regular parser because of the values of the currently defined macros. When looking at the resulting list of references, they could be annotated with the boolean macro expression deciding whether they are visible or not to the compiler.

Having to keep an implementation of these tools parallel with the OTP ones is not easy, but the kind of functionality we are after can't be integrated into the OTP parser tools either, so we have no other option. 

## Definitions

* Referenceable elements: elements with a name that other parts of the code can refer to: modules, functions, macros, records, record fields, variables. 

## Requirements

* All source constructs are preserved, but we keep track of the expansion of macros and includes and also of conditional blocks. The original source text must be possible to reconstruct.
* Source code that is incomplete or in error should be handled gracefully. 
* The lexical tokens are linked in several streams: the source code stream and the expanded stream.
* The parser is mostly interested in the structure of the source file and thus ignores most details about the expressions inside forms, except for detecting usage of referenceable elements. This might also allow handling of ill-behaved macros, whose values are not correct expressions.
* Nodes in the syntax trees produced by the parser must keep track of their respective ranges in the lexical token streams. Some nodes might have distinguished ranges of tokens that define their "main feature" or name.
* The tools must be able to support Erlang code that targets even older OTP versions than the latest. I think the customary (current + 2 older versions) policy is a good decision. This doesn't mean that it must run on older versions, but it must be able to correctly parse older code. I think that it can be required to run on the latest OTP version, thus being able to use all current features, because the kind of tools that would use this parser run usually on the developer's desktop, not on live systems. 
* The APIs for the tools and the syntax tree should follow as much as possible the ones for the corresponding OTP alternatives.
* The token location information should include even the offset (in characters) from the beginning of the file. 
* Module and function comments should be available from the respective nodes in the parse tree. Likewise, function specs. 

* It would be cool if even parse transforms could be handled while keeping all connections to the original code, but that is probably for future development. 
* If possible, parsing should be incremental: a change takes into consideration the previous parse tree, so that for example a string with an unclosed quote doesn't make the rest of the file appear as being part of the string.

## Design

* Reimplementing the lexical scanner and keeping it updated when things change in OTP is tedious and error-prone. It is better to use erl_scan and wrap it in our own module, computing the extra information and providing an extended token data structure.
* Because erl_scan has slightly different APIs in different OTP versions, we can't use the OTP module directly without being able to detect and convert all the variants. Alternatively (which I think is simpler), we keep a local renamed copy of erl_scan and utility modules and update it whenever necessary.

