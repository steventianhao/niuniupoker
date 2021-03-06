-module(niuniupoker_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    Dispatch=cowboy_router:compile([
    	{'_',[
    			{"/",cowboy_static,[
                    {directory,{priv_dir,niuniupoker,[]}},
                    {file,<<"html_ws_client.html">>},
                    {mimetypes,{fun mimetypes:path_to_mimes/2,default}}
                ]},
    			{"/websocket",ws_handler,[]},
    			{"/static/[...]",cowboy_static,[
    				{directory,{priv_dir,niuniupoker,[<<"static">>]}},
    				{mimetypes,{fun mimetypes:path_to_mimes/2,default}}
    			]}
    	]}
    ]),
    {ok,_} = cowboy:start_http(http,100,[{port,7788}],[{env,[{dispatch,Dispatch}]}]),
    niuniupoker_sup:start_link().

stop(_State) ->
    ok.
