# erlide_parser 

_(suggestions for better names are welcome)_

An Erlang preprocessor+scanner+parser tailored for use in an IDE, keeping track of the exact source code in the files. It was originally developed as part of [erlide](https://github.com/erlide/erlide), but it is easier to handle and to get feedback if it is a separate project.

## Rationale

The normal tools for processing source code are aiming to support the compiler and thus are lossy: information that is uninteresting to the compiler is dropped. When supporting editors, cross-referencing tools and other tools, we find ourselves needing to access all the information about the source code.

For example, if sections of a module are conditionally compiled and I am searching for all places where a certain function is called (maybe to rename it), I want to look even into the sections that are invisible to the regular parser because of the values of the currently defined macros. When looking at the resulting list of references, they could be annotated with the boolean macro expression desiding whether they are visible or not to the compiler.

Having to keep an implementation of these tools parallel with the OTP ones is not easy, but the kind of functionality we are after can't be integrated into the OTP parser tools either, so we have no other option. 

## Definitions

* Referenceable elements: elements with a name that other parts of the code can refer to: modules, functions, macros, records, record fields, variables. 

## Requirements

* All source constructs are preserved, but we keep track of the expansion of macros and includes and also of conditional blocks. The original source text must be possible to reconstruct.
* The lexical tokens are linked in several streams: the source code stream and the expanded stream.
* The parser is mostly interested in the structure of the source file and thus ignores most details about the expressions inside forms, except for detecting usage of referenceable elements. This might also allow handling of ill-behaved macros, whose values are not correct expressions.
* Nodes in the syntax trees produced by the parser must keep track of their respective ranges in the lexical token streams. Some nodes might have distinguished rangesof tokens that define their "main feature" or name.
* The tools must be able to support Erlang code that targets even older OTP versions than the latest. I think the customary (current + 2 older versions) policy is a good decision. This doesn't mean that it must run on older versions, but it must be able to correctly parse older code. I think that it can be required to run on the latest OTP version, thus being able to use all current features, because the kind of tools that would use this parser run usually on the developer's desktop, not on live systems. 
* The APIs for the tools and the syntax tree should follow as much as possible the ones for the corresponding OTP alternatives.


