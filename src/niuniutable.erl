-module(niuniutable).
-behavior(gen_server).

-export([handle_call/3,handle_info/2,init/1,terminate/2,handle_cast/2,code_change/3]).
-export([start_link/1]).

-record(state,{id}).



start_link(Id)->
	gen_server:start_link(?MODULE,Id,[]).

init(Id)->
	gproc:reg({p,l,{table,Id}}),
	{ok,#state{id=Id}}.

handle_call(_Request,_From,State)->
	{noreply,State}.

handle_cast(Request,State)->
	gproc:send({p,l,{room,State#state.id}},Request),
	{noreply,State}.

handle_info({talk,User,Words},State)->
	io:format("user#~p is saying words#~p on table ~p~n",[User,Words,State#state.id]),
	gproc:send({p,l,{room,State#state.id}},{talk,User,Words}),
	{noreply,State}.

terminate(_Reason,_State)->
	ok.

code_change(_OldVsn,State,_Extra)->
	{ok,State}.