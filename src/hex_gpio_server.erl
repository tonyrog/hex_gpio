%%%---- BEGIN COPYRIGHT -------------------------------------------------------
%%%
%%% Copyright (C) 2007 - 2014, Rogvall Invest AB, <tony@rogvall.se>
%%%
%%% This software is licensed as described in the file COPYRIGHT, which
%%% you should have received as part of this distribution. The terms
%%% are also available at http://www.rogvall.se/docs/copyright.txt.
%%%
%%% You may opt to use, copy, modify, merge, publish, distribute and/or sell
%%% copies of the Software, and permit persons to whom the Software is
%%% furnished to do so, under the terms of the COPYRIGHT file.
%%%
%%% This software is distributed on an "AS IS" basis, WITHOUT WARRANTY OF ANY
%%% KIND, either express or implied.
%%%
%%%---- END COPYRIGHT ---------------------------------------------------------
%%%-------------------------------------------------------------------
%%% @author Tony Rogvall <tony@rogvall.se>
%%% @doc
%%%
%%% @end
%%% Created :  7 Feb 2014 by Tony Rogvall <tony@rogvall.se>
%%%-------------------------------------------------------------------
-module(hex_gpio_server).

-behaviour(gen_server).

-include_lib("hex/include/hex.hrl").
%% API
-export([start_link/0, stop/0]).
-export([add_event/3, del_event/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-define(EDGE_NONE,    0).
-define(EDGE_RISING,  1).
-define(EDGE_FALLING, 2).
-define(EDGE_BOTH,    3).

-type edge_mask_t() :: 0..3.  %% ?EDGE_xxx (union of all subs)

-record(sub,
	{
	  ref        :: reference(),
	  edge_mask  :: edge_mask_t(),
	  polarity   :: boolean(),
	  signal     :: term(),
	  callback   :: atom() | function()
	}).

-record(pinsub,
	{
	  pin_key        :: {PinReg::integer(),Pin::integer()},
	  edge_mask = 0  :: edge_mask_t(),
	  subs = [] :: [#sub{}]
	}).

-record(state, {
	  joined = false  :: boolean(),
	  pin_list = [] :: [#pinsub{}],
	  ref_list = [] :: [{Ref::reference(),{PinReg::integer,Pin::integer()}}]
	 }).

%%%===================================================================
%%% API
%%%===================================================================

add_event(Flags, Signal, Cb) when is_atom(Cb); is_function(Cb,2) ->
    gen_server:call(?MODULE, {add_event, self(), Flags, Signal, Cb}).

del_event(Ref) ->
    gen_server:call(?MODULE, {del_event, Ref}).

stop() ->
    gen_server:call(?MODULE, stop).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    Joined = hex:auto_join(hex_gpio),
    {ok, #state{joined=Joined}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({add_event,Pid,Flags,Signal,Cb}, _From, State) ->
    PinReg = proplists:get_value(pin_reg, Flags, 0),
    Pin = proplists:get_value(pin, Flags),
    Edge = proplists:get_value(interrupt, Flags, both),
    Polarity = proplists:get_value(polarity, Flags, false),
    EdgeMask = case Edge of
		   falling -> ?EDGE_FALLING;
		   rising  -> ?EDGE_RISING;
		   both    -> ?EDGE_BOTH
	       end,
    Ref = erlang:monitor(process, Pid),
    case add_pinsub(Ref,{PinReg,Pin},EdgeMask,Polarity,Signal,Cb,State) of
	{ok,State1} ->
	    {reply, {ok,Ref}, State1};
	Error ->
	    {reply, Error, State}
    end;

handle_call({del_event,Ref}, _From, State) ->
    case del_pinsub(Ref, State) of
	{ok, State1} ->
	    erlang:demonitor(Ref, [flush]),
	    {reply, ok, State1};
	Error ->
	    {reply, Error, State}
    end;
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({gpio_interrupt, PinReg, Pin, Value}, State) ->
    Key = {PinReg,Pin},
    case lists:keyfind(Key, #pinsub.pin_key, State#state.pin_list) of
	false ->
	    {noreply, State};
	PinSub ->
	    TriggerMask = if Value =:= 0 -> ?EDGE_FALLING;
			     true -> ?EDGE_RISING
			  end,
	    %% polarity? on this level? then easy to share.
	    lists:foreach(
	      fun(#sub { edge_mask=EdgeMask, 
			 polarity=Polarity,
			 signal=Signal,
			 callback=Cb}) ->
		      if EdgeMask band TriggerMask =/= 0 ->
			      Value1 = if Polarity -> 1-Value;
					  true -> Value
				       end,
			      callback(Cb,Signal,[{value,Value1}]);
			 true ->
			      ok
		      end
	      end, PinSub#pinsub.subs),
	    {noreply, State}
    end;
handle_info({'DOWN',Ref,process,_Pid,_Reason}, State) ->
    lager:debug("monitor DOWN ~p ~p", [_Pid,_Reason]),
    case del_pinsub(Ref, State) of
	{ok, State1} ->
	    {noeply,State1};
	_Error ->
	    {noreply,State}
    end;
handle_info(_Info, State) ->
    lager:debug("unhandled info ~p", [_Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

callback(Cb,Signal,Env) when is_atom(Cb) ->
    Cb:event(Signal, Env);
callback(Cb,Signal,Env) when is_function(Cb, 2) ->
    Cb(Signal,Env).


add_pinsub(Ref,Key={PinReg,Pin},EdgeMask,Polarity,Signal,Cb,State) ->
    case lists:keytake(Key, #pinsub.pin_key, State#state.pin_list) of
	false ->
	    case set_interrupt_mask(PinReg, Pin, EdgeMask) of
		ok ->
		    Sub = #sub { ref = Ref,
				 edge_mask = EdgeMask,
				 polarity  = Polarity,
				 signal    = Signal,
				 callback  = Cb },
		    PinSub = #pinsub { pin_key   = Key,
				       edge_mask = EdgeMask,
				       subs    = [Sub]
				     },
		    PinList1 = [PinSub | State#state.pin_list],
		    RefList1 = [{Ref, Key} | State#state.ref_list],
		    {ok,State#state { ref_list = RefList1,
				      pin_list = PinList1 }};
		Error ->
		    Error
	    end;
	{value,PinSub,PinList} ->
	    Mask1 = PinSub#pinsub.edge_mask bor EdgeMask,
	    Res = if Mask1 =/= PinSub#pinsub.edge_mask ->
			  %% update only when mask is changed
			  set_interrupt_mask(PinReg,Pin,Mask1);
		     true ->
			  ok
		  end,
	    if Res =:= ok ->
		    Sub = #sub { ref = Ref,
				 edge_mask = EdgeMask,
				 polarity  = Polarity,
				 signal    = Signal,
				 callback  = Cb },
		    Subs1 = [Sub|PinSub#pinsub.subs],
		    PinSub1 = PinSub#pinsub { edge_mask = Mask1,
					      subs      = Subs1 },
		    PinList1 = [PinSub1 | PinList],
		    RefList1 = [{Ref,Key} | State#state.ref_list],
		    {ok,State#state { ref_list = RefList1,
				      pin_list = PinList1 }};
	       true ->
		    Res
	    end
    end.

del_pinsub(Ref, State) ->
    case lists:keytake(Ref, 1, State#state.ref_list) of
	false ->
	    {error, enoent};
	{value,{_,Key={PinReg,Pin}}, RefList1} ->
	    case lists:keytake(Key, #pinsub.pin_key, State#state.pin_list) of
		false ->
		    %% {error, enoent}; %% strange!
		    {ok, State#state { ref_list = RefList1 }};
		{value,PinSub,PinList} ->
		    case lists:keytake(Ref, #sub.ref, PinSub#pinsub.subs) of
			false ->
			    %% {error, enoent}; %% strange!
			    {ok, State#state { ref_list = RefList1 }};
			{value,#sub{},Subs1} ->
			    %% re calculate the pin mask
			    Mask1 = lists:foldl(fun({_,M,_}, M0) ->
							M bor M0
						end, 0, Subs1),
			    Res = if Mask1 =/= PinSub#pinsub.edge_mask ->
					  set_interrupt_mask(PinReg,Pin,Mask1);
				     true ->
					  ok
				  end,
			    %% handle Res (log with lager!)
			    if Res =/= ok ->
				    io:format("set_interrupt_mask = ~p\n",
					      [Res]);
			       true -> ok
			    end,
			    PinSub1 = PinSub#pinsub { edge_mask = Mask1,
						      subs      = Subs1 },
			    PinList1 = [PinSub1 | PinList],
			    {ok,State#state { ref_list = RefList1,
					      pin_list = PinList1 }}
		    end
	    end
    end.

%% set interrupt
set_interrupt_mask(PinReg, Pin, ?EDGE_NONE) -> 
    gpio:set_interrupt(PinReg, Pin, none);
set_interrupt_mask(PinReg, Pin, ?EDGE_RISING) ->
    gpio:set_interrupt(PinReg, Pin, rising);
set_interrupt_mask(PinReg, Pin, ?EDGE_FALLING) ->
    gpio:set_interrupt(PinReg, Pin, falling);
set_interrupt_mask(PinReg, Pin, ?EDGE_BOTH) ->
    gpio:set_interrupt(PinReg, Pin, both).
