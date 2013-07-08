-module(niuniuplayer).
-behavior(gen_fsm).


%% 一个通用的erlang module，代表了一个用户连接，不管最前端的连接的协议是什么，这个模块只接收erlang message。
%% 而且message的协议是通过函数包装好的，其他process通过调用这些方法来给这个process发消息
%% 支持两种协议websocket和tcp

%%beforeauth
-record(state,{curr_user,table,user_map,tables}).

-export([code_change/4,handle_event/3,handle_info/3,handle_sync_event/4,init/1,terminate/3]).
-export([before_auth/2,after_auth/2,at_table/2]).
-export([quit/1,auth/2,talk/2,start_link/0,join/2,status/1]).

init_users()->
	D0=dict:new(),
	D1=dict:append("t1",{1,"simon"},D0),
	D2=dict:append("t2",{2,"valor"},D1),
	D3=dict:append("t3",{3,"sammi"},D2),
	D3.

init_tables()->
	[1,2,3,4,5,6].

start_link()->
	Users=init_users(),
	Tables=init_tables(),
	gen_fsm:start_link(?MODULE,[Users,Tables],[]).

quit(Pid)->
	gen_fsm:send_all_state_event(Pid,quit).

status(Pid)->
	gen_fsm:send_all_state_event(Pid,status).

join(Pid,TableId)->
	gen_fsm:send_event(Pid,{join,TableId}).

auth(Pid,Token)->
	gen_fsm:send_event(Pid,{auth,Token}).

talk(Pid,Words)->
	gen_fsm:send_event(Pid,{talk,Words}).

init([Users,Tables])->
	{ok,before_auth,#state{user_map=Users,tables=Tables}}.

before_auth({auth,Token},StateData)->
	Users=StateData#state.user_map,
	case dict:find(Token,Users) of
		{ok,Value}->
			NextState=StateData#state{curr_user=Value},
			{next_state,after_auth,NextState};
		error->
			{next_state,before_auth,StateData}
	end.

after_auth({join,TableId},StateData)->
	case lists:any(fun(El)-> El=:=TableId end,StateData#state.tables) of
		true-> 
			NextState=StateData#state{table=TableId},
			gproc:reg({p,l,{at_table,TableId}}),
			gproc:send({p,l,{at_table,TableId}},{join,StateData#state.curr_user}),
			{next_state,at_table,NextState};
		false->
			{next_state,after_auth,StateData}
	end.

at_table({talk,Words},StateData)->
	TableId=StateData#state.table,
	User=StateData#state.curr_user,
	gproc:send({p,l,{at_table,TableId}},{talk,User,Words}),
	{next_state,at_table,StateData}.

handle_info({join,User},StateName,StateData)->
	case StateData#state.curr_user of
		User->ok;
		_->io:format("User# ~p join table# ~p~n",[User,StateData#state.table])
	end,
	{next_state,StateName,StateData};

handle_info({quit,User},StateName,StateData)->
	case StateData#state.curr_user of
		User->ok;
		_->io:format("User# ~p quit table# ~p~n",[User,StateData#state.table])
	end,
	{next_state,StateName,StateData};

handle_info({talk,User,Words},StateName,StateData)->
	case StateData#state.curr_user of
		User->ok;
		_->io:format("i got msg from ~p, the content is ~p~n",[User,Words])
	end,
	{next_state,StateName,StateData}.

handle_sync_event(_Event,_From,_StateName,StateData)->
	{stop,"unexpected invocation",StateData}.

handle_event(status,StateName,StateData)->
	io:format("table# ~p, user# ~p~n",[StateData#state.table,StateData#state.curr_user]),
	{next_state,StateName,StateData};
handle_event(quit,StateName,StateData)->
	case StateName of
		before_auth->{stop,normal,StateData};
		_->	
			gproc:send({p,l,{at_table,StateData#state.table}},{quit,StateData#state.curr_user}),
			{stop,normal,StateData}
	end.

terminate(_Reason,_StateName,_StateData)->
	ok.

code_change(_OldVsn,StateName,StateData,_Extra)->
	{ok,StateName,StateData}.