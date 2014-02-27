%%%-------------------------------------------------------------------
%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2014, Tony Rogvall
%%% @doc
%%%
%%% @end
%%% Created :  7 Feb 2014 by Tony Rogvall <tony@rogvall.se>
%%%-------------------------------------------------------------------
-module(hex_gpio_server).

-behaviour(gen_server).

-include_lib("lager/include/log.hrl").
-include_lib("hex/include/hex.hrl").
%% API
-export([start_link/0, stop/0]).
-export([add_event/2, del_event/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-define(EDGE_NONE,    0).
-define(EDGE_RISING,  1).
-define(EDGE_FALLING, 2).
-define(EDGE_BOTH,    3).

-type edge_mask_t() :: 0..3.  %% ?EDGE_xxx (union of all subs)

-record(pinsub,
	{
	  pin_key        :: {PinReg::integer(),Pin::integer()},
	  edge_mask = 0  :: edge_mask_t(),
	  subs = [] :: [{Ref::reference(),Edge::edge_mask_t(),Signal::term()}]
	}).

-record(state, {
	  pin_list = [] :: [#pinsub{}],
	  ref_list = [] :: [{Ref::reference(),{PinReg::integer,Pin::integer()}}]
	 }).

%%%===================================================================
%%% API
%%%===================================================================
add_event(Flags, Signal) ->
    gen_server:call(?MODULE, {add_event, Flags, Signal}).

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
    {ok, #state{}}.

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
handle_call({add_event,Flags,Signal}, _From, State) ->
    %% pin_reg = 0..N, pin = 0..N, interrupt=rising|falling|both
    PinReg = proplists:get_value(pin_reg, Flags, 0),
    Pin = proplists:get_value(pin, Flags),
    Edge = proplists:get_value(interrupt, Flags, both),
    EdgeMask = case Edge of
		   falling -> ?EDGE_FALLING;
		   rising  -> ?EDGE_RISING;
		   both    -> ?EDGE_BOTH
	       end,
    Ref = make_ref(),
    case add_pinsub(Ref, {PinReg,Pin}, EdgeMask, Signal, State) of
	{ok,State1} ->
	    {reply, {ok,Ref}, State1};
	Error ->
	    {reply, Error, State}
    end;

handle_call({del_event,Ref}, _From, State) ->
    case del_pinsub(Ref, State) of
	{ok, State1} ->
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
	      fun({_Ref,EdgeMask,Signal}) ->
		      if EdgeMask band TriggerMask =/= 0 ->
			      hex_server:event(Signal,[{value,Value}]);
			 true ->
			      ok
		      end
	      end, PinSub#pinsub.subs),
	    {noreply, State}
    end;

handle_info(_Info, State) ->
    io:format("got info ~p\n", [_Info]),
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

add_pinsub(Ref, Key={PinReg,Pin}, EdgeMask, Signal, State) ->
    case lists:keytake(Key, #pinsub.pin_key, State#state.pin_list) of
	false ->
	    case set_interrupt_mask(PinReg, Pin, EdgeMask) of
		ok ->
		    PinSub = #pinsub { pin_key   = Key,
				       edge_mask = EdgeMask,
				       subs    = [{Ref,EdgeMask,Signal}]
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
		    Subs1 = [{Ref,EdgeMask,Signal}|PinSub#pinsub.subs],
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
		    case lists:keytake(Ref, 1, PinSub#pinsub.subs) of
			false ->
			    %% {error, enoent}; %% strange!
			    {ok, State#state { ref_list = RefList1 }};
			{value,{_Ref,_Edge,_Signal},Subs1} ->
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
