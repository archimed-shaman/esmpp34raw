-module(esmpp34raw_app).
-author("Morozov Alexander aka ~ArchimeD~").

-behaviour(application).

%% Application callbacks
-export([start/2, 
         stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    esmpp34raw_sup:start_link().

stop(_State) ->
    ok.
