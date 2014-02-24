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

-export([add_event/2, del_event/1, output/2]).
-export([validate_flags/2]).

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
%% validate_flags(in | out, Flags::[{atom(),term()}])
%%
validate_flags(_Dir, []) ->
    ok;
validate_flags(Dir, [{Key,Value}|Kvs]) ->
    case Key of
	pin_reg when is_integer(Value), Value >= 0 ->
	    validate_flags(Dir, Kvs);
	pin when is_integer(Value), Value >= 0 ->
	    validate_flags(Dir, Kvs);
	value when Dir =:= out, is_boolean(Value) ->
	    validate_flags(Dir, Kvs);
	value when Dir =:= out, is_integer(Value), Value >= 0, Value =< 1 ->
	    validate_flags(Dir, Kvs);
	interrupt when Value =:= rising;
		       Value =:= falling;
		       Value =:= both ->
	    validate_flags(Dir, Kvs);
	polarity when is_boolean(Value) ->
	    validate_flags(Dir, Kvs);
	_ ->
	    lager:debug("validate_flags: unknown option/value ~p", 
			[{Key,Value}]),
	    {error, badarg}
    end.
