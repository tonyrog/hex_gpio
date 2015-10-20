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
	 event_spec/1,
	 init_event/2,
	 mod_event/2,
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
    Value = value(Flags,Env,undefined),
    Polarity = proplists:get_value(polarity, Flags, false),
    if Value =/= undefined ->
	    case Value =/= Polarity of
		false -> gpio:clr(PinReg, Pin);
		true ->gpio:set(PinReg, Pin)
	    end;
       true ->
	    ok
    end.

value(Flags,Env,Default) ->
    case lookup_value_flag(Flags,Env,Default) of
	undefined -> Default;
	Value when is_integer(Value) -> (Value =/= 0);
	Value when is_boolean(Value) -> Value
    end.

lookup_value_flag(Flags,Env,Default) ->
    case proplists:get_value(value, Flags, undefined) of
	undefined ->
	    case proplists:get_value(map,Flags,"") of
		"" -> Default;
		Name -> proplists:get_value(Name, Env, Default)
	    end;
	Value -> Value
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

mod_event(Dir,Flags) ->
    %% fixme: send to gpio_server to update ...
    init_event(Dir, Flags).
    
%%
%% validate_event(in | out, Flags::[{atom(),term()}]) -> ok | {error,Error}
%%
validate_event(Dir, Flags) ->
    hex:validate_flags(Flags, event_spec(Dir)).

event_spec(out) ->
    [{leaf,pin,[{type,uint8,[]},{mandatory,true,[]}]},
     {leaf,pin_reg,[{type,uint8,[]},{default,0,[]}]},
     {leaf,value,[{type,uint8,[{range,[{0,1}],[]}]}]}, %% boolean?
     {leaf,map,[{type,string,[]},{default,"",[]}]}, %% output map
     {leaf,init_value,[{type,enumeration,
			[{enum,high,[]},
			 {enum,low,[]},
			 {enum,out,[]}]},
		       {default, out, []}]},
     {leaf,polarity,[{type,boolean,[]},
		     {default,false,[]}]},
     {leaf,direct,[{type,boolean,[]},
		   {default,false,[]}]}
    ];
event_spec(in) ->
    [{leaf,pin,[{type,uint8,[]},{mandatory,true,[]}]},
     {leaf,pin_reg,[{type,uint8,[]},{default,0,[]}]},
     {leaf,interrupt,[{type,enumeration,
		      [{enum,none,[]},
		       {enum,rising,[]},
		       {enum,falling,[]},
		       {enum,both,[]}]},
		     {default,none,[]}]},
     {leaf,debounce,[{type,uint32,[]},
		     {default,0,[]}]},
     {leaf,polarity,[{type,boolean,[]},
		     {default,false,[]}]},
     {leaf,direct,[{type,boolean,[]},
		   {default,false,[]}]}
    ].
