%%%-------------------------------------------------------------------
%% @doc hello public API
%% @end
%%%-------------------------------------------------------------------

-module(hello_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
        Dispatch = cowboy_router:compile([
            {'_', [
                {"/", default_page_h, []}
            ]}
        ]),
        cowboy:start_clear(
            my_http_listener,
            [{port, 8080}],
            #{env => #{dispatch => Dispatch}}
        ),
        hello_sup:start_link().
stop(_State) ->
    ok.                                                                                     

%% internal functions

%% internal functions
