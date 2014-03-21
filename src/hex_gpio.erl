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
%%%    Hex GPIO plugin 
%%% @end
%%% Created :  24 Feb 2014 by Tony Rogvall <tony@rogvall.se>
%%%-------------------------------------------------------------------
-module(hex_gpio).

-behaviour(hex_plugin).

-export([validate_event/2, 
	 init_event/2,
	 add_event/3, 
	 del_event/1, 
	 output/2]).

%%
%%  add_event(Flags::[{atom(),term()}, Signal::signal()) ->    
%%     {ok, Ref:reference()} | {error, Reason}
%%
add_event(Flags, Signal, Cb) ->
    hex_gpio_server:add_event(Flags, Signal, Cb).

%%
%%  del_event(Ref::reference()) ->
%%     ok.
del_event(Ref) ->
    hex_gpio_server:del_event(Ref).
%%
%% output(Flags::[{atom(),term()}], Env::[{atom(),term()}]) ->
%%    ok.
%%
output(Flags, Env) ->
    Pin = proplists:get_value(pin, Flags),
    PinReg = proplists:get_value(pin_reg, Flags, 0),
    PinReg = proplists:get_value(pin_reg, Flags, 0),
    Value = case value(Flags, undefined) of
		undefined -> value(Env, undefined);
		V -> V
	    end,
    Polarity = proplists:get_value(polarity, Flags, false),
    case Value =/= Polarity of
	false -> gpio:clr(PinReg, Pin);
	true ->gpio:set(PinReg, Pin)
    end.

value(Flags,Default) ->
    case proplists:get_value(value, Flags) of
	undefined -> Default;
	1 -> true;
	0 -> false;
	Bool -> Bool
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
%% validate_event(in | out, Flags::[{atom(),term()}]) -> ok | {error,Error}
%%
validate_event(in, Flags) ->
    hex:validate_flags(Flags, input_spec());
validate_event(out, Flags) ->
    hex:validate_flags(Flags, output_spec()).

output_spec() ->
    [{pin, mandatory, unsigned, undefined},
     {pin_reg, optional, unsigned, 0},
     {value, optional, {alt,[boolean,unsigned1]}, undefined},
     {init_value, optional, {alt,[{const,high},
				  {const,low},
				  {const,out}]}, out},
     {polarity, optional, boolean, false},
     {direct, optional, boolean, false}
    ].

input_spec() ->
    [{pin, mandatory, unsigned, undefined},
     {pin_reg, optional, unsigned, 0},
     {interrupt, optional, {alt,[{const,none},
				 {const,rising},
				 {const,falling},
				 {const,both}]}, none},
     {polarity, optional, boolean, false},
     {direct, optional, boolean, false}
    ].
