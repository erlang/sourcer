# Sourcer - language services for IDEs

> This is a new incarnation of the sourcer project, the old code is still available on the `legacy` branch.

*Sourcer* aims to provide:

- a generic language server implementation `lsp_server`
- Erlang support for the above `erlang_ls`
- Erlang language services library `sourcer`

## Generic language server `lsp_server`

The generic language server uses a TCP connection to talk LSP with clients. It encodes/decodes the messages and delegates the actual work to a language specific server, using cancelable worker processes. It also supports making requests to the client.

## Erlang server `erlang_ls`

This is the "real" server, connecting the generic and specific parts and an adapter from LSP to the 'sourcer' data format (see below) and back.

### Supported Options

 - --help,    -h,  undefined, Shows help;
 - --dump,    -d,  string,    Dump sourcer db for file;
 - --port,    -p,  integer,   LSP server port;
 - --verbose, -v,  integer,   Verbosity level;
 - --indent,  -i,  string,    Indent file(s) and exit;
 - --config,  N/A, string,    Configuration file.


### Configuration

Configuration file contains property list.

Currently we only support these keys:

- `indent` - controls indentation parameters.

Example:

```erlang
{indent, [{indentW, 2}]}.

```



## Language services library `sourcer`

The actual work is done by this application. It is meant to be LSP-agnostic so that it can be used in other contexts and tools.

An own parser (largely based on the legacy 'sourcer' code, but simpler) processes the source code and produces a ctags-like database containing information about all interesting code entities. The database should be distributed, in the sense that libraries should be able to provide the data about their own code (produced at build time).

Query facilities are provided so that the information in the database is presented in a way that the LSP expects.
