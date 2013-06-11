%% Feel free to use, reuse and abuse the code in this file.

-module(niuniupoker).

%% API.
-export([start/0]).

%code:add_path("deps/cowboy/ebin").
%code:add_path("deps/ranch/ebin"). 
%code:add_path("deps/mimetypes/ebin").

start() ->
	ok = application:start(crypto),
	ok = application:start(ranch),
	ok = application:start(cowboy),
	ok = application:start(niuniupoker).
