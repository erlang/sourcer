-module(lsp_server_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

start_link() ->
    R = supervisor:start_link({local, ?MODULE}, ?MODULE, []),
    R.

init([]) ->
    Children = children(),
    %% no supervisor restarts
    {ok, {{one_for_one, 0, 1}, Children}}.

%% TODO configure it!!!

children() ->
    {ok, Port} = application:get_env(lsp_server, port),
    JsonRpc = {jsonrpc, {jsonrpc, start_link, [Port, lsp_server, lsp_client]},
        permanent, 60000, worker, [jsonrpc]},
    IdeServer = {lsp_server, {lsp_server, start_link, [sourcer]},
        permanent, 60000, worker, [lsp_server]},
    IdeClient = {lsp_client, {lsp_client, start_link, []},
        permanent, 60000, worker, [lsp_client]},
    [
        JsonRpc, IdeServer, IdeClient
    ].

