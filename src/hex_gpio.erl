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
    Body = proplists:get_value(body, Flags, ""),
    {Fs,_} = proplists:split(Flags, [smsc,rp,udhi,udh,srr,mref,
				     vpf,vp,addr,pid,dcs,type,class,
				     alphabet,compression,store,wait_type,
				     notify,ref]),
    Opts = lists:append(Fs),
    gsms:send(Opts, Body).

%%
%% validate_flags(in | out, Flags::[{atom(),term()}])
%%
validate_flags(_Dir, []) ->
    ok;
validate_flags(Dir, [{Key,Value}|Kvs]) ->
    case Key of
	smsc when is_list(Value) ->
	    %% FIXME: validate msisdn
	    validate_flags(Dir, Kvs);
	rp when is_boolean(Value) ->
	    validate_flags(Dir, Kvs);
	udhi when is_boolean(Value) ->
	    validate_flags(Dir, Kvs);
	udh when Value =:= [] ->
	    validate_flags(Dir, Kvs);
	udh when is_list(Value) ->
	    %% FIXME: validate udh codings
	    validate_flags(Dir, Kvs);
	srr when is_boolean(Value) ->
	    validate_flags(Dir, Kvs);
	mref when is_integer(Value), Value>=0, Value=< 255 ->
	    validate_flags(Dir, Kvs);
	vpf when Value =:= none;
		 Value =:= relative; 
		 Value =:= enhanced; 
		 Value =:= absolute ->
	    validate_flags(Dir, Kvs);
	vp ->
	    case Value of
		none ->
		    validate_flags(Dir, Kvs);
		{relative, Seconds} when is_integer(Seconds), Seconds>=0 ->
		    validate_flags(Dir, Kvs);
		{absolute,DateTimeTz} ->
		    case gsms_codec:is_valid_scts(DateTimeTz) of
			true ->
			    validate_flags(Dir, Kvs);
			false ->
			    {error, badarg}
		    end;
		{enhanced,_} ->
		    {error, not_supported}  %% yet
	    end;
	addr when is_list(Value) ->
	    %% FIXME: validate msisdn
	    validate_flags(Dir, Kvs);
	pid when is_integer(Value), Value>=0, Value=<255 ->
	    validate_flags(Dir, Kvs);
	dcs when is_integer(Value) ->
	    _Dcs = gsms_codec:decode_dcs(Value),
	    validate_flags(Dir, Kvs);
	type -> %% fixme test
	    validate_flags(Dir, Kvs);
	class -> %% fixme test
	    validate_flags(Dir, Kvs);
	alphabet -> %% fixme test
	    validate_flags(Dir, Kvs);
	compression -> %% fixme test
	    validate_flags(Dir, Kvs);
	store -> %% fixme test
	    validate_flags(Dir, Kvs);
	wait_type -> %% fixme test
	    validate_flags(Dir, Kvs);
	%% recognized options, but not for pdu
	notify ->  %% fixme test?
	    validate_flags(Dir, Kvs);
	ref ->  %% fixme test?
	    validate_flags(Dir, Kvs);
	_ ->
	    lager:debug("validate_flags: unknown pdu option ~p", 
			[{Key,Value}]),
	    {error, badarg}
    end.
