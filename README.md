# sourcer

> This is a new incarnation of the sourcer project, the old code is still available on the `legacy` branch.

*Sourcer* aims to provide:

- a generic language server implementation `lsp_server`
- Erlang support for the above `erlang_ls`
- tooling for extracting information about code in a simple format `sourcer`

## Generic language server

The language server uses a TCP connection to talk LSP with clients. It delegates the actual work to a language specific server. It includes an implementation of an cancelable worker process.

## Erlang server

This is an adapter from LSP to the 'sourcer' data (see below) and back.

## Sourcer

The actual work is done by this application. It is meant to be LSP-agnostic so that it can be used in other contexts and tools.

An own parser (largely based on the legacy 'sourcer' code, but simpler) processes the source code and produces a ctags-like database containing information about all interesting code entities. The database should be distributed, in the sense that libraries should be able to provide the data about their own code (produced at build time).
