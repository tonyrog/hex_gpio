%%%-------------------------------------------------------------------
%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2014, Tony Rogvall
%%% @doc
%%%    Hex GPIO plugin 
%%% @end
%%% Created :  24 Feb 2014 by Tony Rogvall <tony@rogvall.se>
%%%-------------------------------------------------------------------
-module(hex_gpio).

-behaviour(hex_plugin).

-export([validate_event/2, 
	 init_event/2,
	 add_event/2, 
	 del_event/1, 
	 output/2]).

%%
%%  add_event(Flags::[{atom(),term()}, Signal::signal()) ->    
%%     {ok, Ref:reference()} | {error, Reason}
%%
add_event(Flags, Signal) ->
    hex_gpio_server:add_event(Flags, Signal).

%%
%%  del_event(Ref::reference()) ->
%%     ok.
del_event(Ref) ->
    hex_gpio_server:del_event(Ref).
%%
%% output(Flags::[{atom(),term()}], Env::[{atom(),term()}]) ->
%%    ok.
%%
output(Flags, _Env) ->
    Pin = proplists:get_value(pin, Flags),
    PinReg = proplists:get_value(pin_reg, Flags, 0),
    PinReg = proplists:get_value(pin_reg, Flags, 0),
    Value = case proplists:get_value(value, Flags, false) of
		0 -> false;
		1 -> true;
		false -> false;
		true -> true
	    end,
    Polarity = proplists:get_value(polarity, Flags, false),
    case Value =/= Polarity of
	false -> gpio:clr(PinReg, Pin);
	true ->gpio:set(PinReg, Pin)
    end.

%%
%% init_event(in | out, Flags::[{atom(),term()}]) -> ok | {error,Reason}
%% validate_event is assumed to have been run before init !
init_event(in,Flags) ->
    Pin = proplists:get_value(pin, Flags),
    PinReg = proplists:get_value(pin_reg, Flags, 0),
    case gpio:init(Pin, PinReg) of
	ok -> gpio:set_direction(Pin, PinReg, in);
	Error -> Error
    end;
init_event(out,Flags) ->
    Pin    = proplists:get_value(pin, Flags),
    PinReg = proplists:get_value(pin_reg, Flags, 0),
    Direct = proplists:get_bool(direct, Flags),
    InitValue  = case proplists:get_value(init_value, Flags) of
		     high -> high;
		     low  -> low;
		     undefined -> out
		 end,
    R = if Direct -> gpio:init_direct(Pin, PinReg);
	   true -> gpio:init(Pin, PinReg)
	end,
    case R of
	ok -> gpio:set_direction(Pin, PinReg, InitValue);
	_ -> R
    end.

%%
%% validate_event(in | out, Flags::[{atom(),term()}])
%%
validate_event(Dir, Flags) ->
    case lists:keytake(pin, 1, Flags) of
	false ->
	    {error, {mandatory, [pin]}};
	{value,{pin,Pin},Flags1} when is_integer(Pin), Pin >= 0 ->
	    validate_event1(Dir, Flags1);
	_ ->
	    {error, {badarg, [pin]}}
    end.

validate_event1(_Dir, []) ->
    ok;
validate_event1(Dir, [{Key,Value}|Kvs]) ->
    case Key of
	pin_reg when is_integer(Value), Value >= 0 ->
	    validate_event1(Dir, Kvs);
	value when Dir =:= out, is_boolean(Value) ->
	    validate_event1(Dir, Kvs);
	value when Dir =:= out, is_integer(Value), Value >= 0, Value =< 1 ->
	    validate_event1(Dir, Kvs);
	interrupt when Value =:= rising;
		       Value =:= falling;
		       Value =:= both ->
	    validate_event1(Dir, Kvs);
	init_value when Value =:= high;
			Value =:= low ->
	    validate_event1(Dir, Kvs);
	polarity when is_boolean(Value) ->
	    validate_event1(Dir, Kvs);
	direct when is_boolean(Value) ->
	    %% direct access (require that gpio is initalized with chipset!)
	    validate_event1(Dir, Kvs);
	_ ->
	    lager:debug("validate_event: unknown option/value ~p", 
			[{Key,Value}]),
	    {error, badarg}
    end.
