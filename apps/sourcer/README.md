# sourcer: erlang language services

IDEs offer sophisticated services to users of programming languages. This library implements such services for Elang, so that clients (IDEs, editors) don't need to. Microsoft's Language Server Protocol is offering a standard way for clients and servers to talk about the code being edited and we follow their APIs. This aplication should not be dependent on LSP specifics, but in the beginning it will be easier to follow those APIs closely.

There are two components that are meant to be loosely coupled. One is a parser that reads code and produces a ctags-like database with all interesting elemnts in the source code and their attributes. The other is a query engine that can traverse the database and gather information relative to specific services (like completion, references or documentation).

From our experience with [erlide](http://erlide.org), we know that the user experience is much better if the database is readily available even for large libraries (especially the OTP). So a goal is to allow libraries to provide their own chunks of the database, built off-line.

## Build

    $ rebar3 compile

# Notes, issues & thoughts

- I'd like to use file offsets alongside line and column numbers, but there is a problem when caching the information: the line endings. On Windows, the offsets will be shifted with one position for each line, compared with Linux and OSX. So maybe it is better to let the client care about the conversion.

- We only load library apps on demand. There might be exceptions like `_checkouts` which should be considered as part of the project. For legacy (non-rebar) projects, it may be needed to specify this somehow, since we can't rely on conventions.


