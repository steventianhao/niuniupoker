-module(niuniuplayer).
-behavior(gen_fsm).


%% 一个通用的erlang module，代表了一个用户连接，不管最前端的连接的协议是什么，这个模块只接收erlang message。
%% 而且message的协议是通过函数包装好的，其他process通过调用这些方法来给这个process发消息
%% 支持两种协议websocket和tcp

%%beforeauth
-record(state,{curr_user,room,user_map,rooms}).

-export([code_change/4,handle_event/3,handle_info/3,handle_sync_event/4,init/1,terminate/3]).
-export([before_auth/2,after_auth/2,join_room/2]).
-export([quit/0,auth/1,talk/1,start_link/0,join/1,status/0]).

-define(SERVER,?MODULE).

init_users()->
	D0=dict:new(),
	D1=dict:append("t1",{1,"simon"},D0),
	D2=dict:append("t2",{2,"valor"},D1),
	D3=dict:append("t3",{3,"sammi"},D2),
	D3.

init_rooms()->
	[1,2,3,4,5,6].

start_link()->
	Users=init_users(),
	Rooms=init_rooms(),
	gen_fsm:start_link({local,?SERVER},?MODULE,[Users,Rooms],[]).

quit()->
	gen_fsm:send_all_state_event(?SERVER,quit).

status()->
	gen_fsm:send_all_state_event(?SERVER,status).

join(RoomNo)->
	gen_fsm:send_event(?SERVER,{join,RoomNo}).

auth(Token)->
	gen_fsm:send_event(?SERVER,{auth,Token}).

talk(Words)->
	gen_fsm:send_event(?SERVER,{talk,Words}).

init([Users,Rooms])->
	{ok,before_auth,#state{user_map=Users,rooms=Rooms}}.

before_auth({auth,Token},StateData)->
	Users=StateData#state.user_map,
	io:format("users ~p#~p# ~n",[Token,Users]),
	case dict:find(Token,Users) of
		{ok,Value}->
			NextState=StateData#state{curr_user=Value},
			{next_state,after_auth,NextState};
		error->
			{next_state,before_auth,StateData}
	end.

after_auth({join,RoomNo},StateData)->
	case lists:any(fun(El)-> El=:=RoomNo end,StateData#state.rooms) of
		true-> 
			NextState=StateData#state{room=RoomNo},
			gproc:reg({p,l,{room,RoomNo}}),
			{next_state,join_room,NextState};
		false->
			{next_state,after_auth,StateData}
	end.

join_room({talk,Words},StateData)->
	RoomNo=StateData#state.room,
	User=StateData#state.curr_user,
	gproc:send({p,l,{table,RoomNo}},{talk,User,Words}),
	{next_state,after_auth,StateData}.

handle_info({talk,User,Words},StateName,StateData)->
	io:format("i got msg from ~p, the content is ~p~n",[User,Words]),
	{next_state,StateName,StateData}.

handle_sync_event(_Event,_From,_StateName,StateData)->
	{stop,"unexpected invocation",StateData}.

handle_event(status,StateName,StateData)->
	io:format("room# ~p, user# ~p~n",[StateData#state.room,StateData#state.curr_user]),
	{next_state,StateName,StateData};
handle_event(quit,_StateName,StateData)->
	{stop,normal,StateData}.

terminate(_Reason,_StateName,_StateData)->
	ok.

code_change(_OldVsn,StateName,StateData,_Extra)->
	{ok,StateName,StateData}.