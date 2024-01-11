-module(arithmatic).
-export([start_factorializer/0,start_adder/0,start_subtracter/0,start_multiplier/0,start_divider/0,
		factorializer/0,adder/0,subtracter/0,multiplier/0,divider/0,
		factorial_of/2,add/3,subtract/3]).

%%
%% Put your functions here. The factorializer, adder, subtracter, multiplier, and divider functions 
%% are to be created such that they can be spawned.
%%

% spawn factorializer 
start_factorializer() ->
	spawn(?MODULE,factorializer,[]).

% facrorial client // Like an API that records the arguments for the peramaters
factorial_of(Factorial_pid, Number) ->
	Factorial_pid ! {self(),Number},
	receive
		Response ->
			Response
	end.

% server factorializer
factorializer() ->
	receive
		{From, Number} when is_integer(Number) == false ->
			From ! {fail, Number,is_not_integer},
			factorializer();
		{From, Number} when Number < 0 ->
			From ! {fail, Number,is_negative},
			factorializer();
		{From, Number} ->
			From ! lists:foldl(fun(X,Accum) -> X*Accum end,1,lists:seq(1,Number)),
			factorializer()
		end.

% spawn adder
start_adder() ->
	spawn(?MODULE,adder,[]).

% add client
add(Adder_pid, Num1, Num2) ->
	Adder_pid ! {self(),Num1,Num2},
	receive
		Response ->
			Response
	end.

% server adder
adder() ->
	receive
		{From, Num1, _Num2} when (is_number(Num1) == false)  ->
			From ! {fail, Num1, is_not_number},
			adder();
		{From, _Num1, Num2} when (is_number(Num2) == false)  ->
			From ! {fail, Num2, is_not_number},
			adder();
		{From, Num1, Num2} ->
			From ! Num1 + Num2,
			adder()
	end.


% spawn subtracter
start_subtracter() ->
	spawn(?MODULE,subtracter,[]).

% subtract client
subtract(Subtracter_pid, Request1, Request2) ->
	Subtracter_pid ! {self(),Request1,Request2},
	receive
		Response ->
			Response
	end.

% server subtracter
subtracter() ->
	receive
		{From,Request1,_Request2} when (is_number(Request1) == false)  ->
			From ! {fail,Request1,is_not_number}, 
			subtracter();
		{From,_Request1,Request2} when (is_number(Request2) == false)  ->
			From ! {fail,Request2,is_not_number}, 
			subtracter();
		{From,Request1,Request2} ->
			From ! Request1-Request2, 
			subtracter()
	end.


% spawn multiplier
start_multiplier() ->
	spawn(?MODULE,multiplier,[]).

% multiply client
multiply(Multiplier_pid, Multiplicand, Multiplier) ->
	Multiplier_pid ! {self(),Multiplicand,Multiplier},
	receive
		Response ->
			Response
	end.

% server multiplier
multiplier() ->
	receive
		{From,Multiplicand,_Multiplier} when (is_number(Multiplicand) == false)  ->
			From ! {fail,Multiplicand,is_not_number},
			multiplier();
		{From,_Multiplicand,Multiplier} when (is_number(Multiplier) == false)  ->
			From ! {fail,Multiplier,is_not_number},
			multiplier();
		{From,Multiplicand,Multiplier} ->
			From ! Multiplicand*Multiplier,
			multiplier()
	end.


% spawn divider
start_divider() ->
	spawn(?MODULE,divider,[]).

% divide client
divide(Dividerer_pid, Dividend, Divisor) ->
	Dividerer_pid ! {self(),Dividend,Divisor},
	receive
		Response ->
			Response
	end.

% server divider
divider() ->
	receive
		{From,Dividend,_Divisor} when (is_number(Dividend) == false)  ->
			From ! {fail,Dividend,is_not_number},
			divider();
		{From,_Dividend,Divisor} when (is_number(Divisor) == false)  ->
			From ! {fail,Divisor,is_not_number},
			divider();
		{From,_Dividend,0} ->
			From ! {fail,division_by_zero},
			divider();
		{From,Dividend,Divisor} ->
			From ! Dividend/Divisor,
			divider()
	end.


-ifdef(EUNIT).
%%
%% Unit tests go here. 
%%

-include_lib("eunit/include/eunit.hrl").


factorializer_test_() ->
{setup,
	fun()->%runs before any of the tests
			Pid = spawn(?MODULE,factorializer,[]),	
			register(test_factorializer,Pid)
		end,
	%fun(_)->%runs after all of the tests
		%there is no teardown needed, so this fun doesn't need to be implemented.
	%end,
	%factorializer tests start here
	[ ?_assertEqual(120,factorial_of(test_factorializer,5)),%happy path
	%nasty thoughts start here
	?_assertEqual(1,factorial_of(test_factorializer,0)),
	?_assertEqual({fail,-3,is_negative},factorial_of(test_factorializer,-3)),
	?_assertEqual({fail,bob,is_not_integer},factorial_of(test_factorializer,bob)),
	?_assertEqual({fail,5.0,is_not_integer},factorial_of(test_factorializer,5.0))
	]
}.

adder_test_() ->
{setup,
	fun()->%runs before any of the tests
			Pid = spawn(?MODULE,adder,[]),	
			register(test_adder,Pid)
		end,
	%fun(_)->%runs after all of the tests
		%there is no teardown needed, so this fun doesn't need to be implemented.
	%end,
	%factorializer tests start here
	[ ?_assertEqual(8,add(test_adder,5,3)),%happy path
	%nasty thoughts start here
	?_assertEqual(0,add(test_adder,0,0)),
	?_assertEqual(0.0,add(test_adder,0.0,0.0)),
	?_assertEqual(0,add(test_adder,-5,5)),
	?_assertEqual(1.5,add(test_adder,0.75,0.75)),
	?_assertEqual({fail,bob,is_not_number},add(test_adder,bob,3)),
	?_assertEqual({fail,sue,is_not_number},add(test_adder,3,sue)),
	?_assertEqual({fail,bob,is_not_number},add(test_adder,bob,sue))
	]
}.

subtracter_test_() ->
{setup,
	fun()->%runs before any of the tests
			Pid = spawn(?MODULE,subtracter,[]),	
			register(test_subtracter,Pid)
		end,
	%fun(_)->%runs after all of the tests
		%there is no teardown needed, so this fun doesn't need to be implemented.
	%end,
	%factorializer tests start here
	[ ?_assertEqual(2,subtract(test_subtracter,5,3)),%happy path
	%nasty thoughts start here
	?_assertEqual(0,subtract(test_subtracter,0,0)),
	?_assertEqual(0.0,subtract(test_subtracter,0.0,0.0)),
	?_assertEqual(-10,subtract(test_subtracter,-5,5)),
	?_assertEqual(0.75,subtract(test_subtracter,1.5,0.75)),
	?_assertEqual({fail,bob,is_not_number},subtract(test_subtracter,bob,3)),
	?_assertEqual({fail,sue,is_not_number},subtract(test_subtracter,3,sue)),
	?_assertEqual({fail,bob,is_not_number},subtract(test_subtracter,bob,sue))
	]
}.

multiplier_test_() ->
{setup,
	fun()->%runs before any of the tests
			Pid = spawn(?MODULE,multiplier,[]),	
			register(test_multiplier,Pid)
		end,
	%fun(_)->%runs after all of the tests
		%there is no teardown needed, so this fun doesn't need to be implemented.
	%end,
	%factorializer tests start here
	[ ?_assertEqual(15,multiply(test_multiplier,5,3)),%happy path
	%nasty thoughts start here
	?_assertEqual(0,multiply(test_multiplier,0,0)),
	?_assertEqual(0.0,multiply(test_multiplier,0.0,0.0)),
	?_assertEqual(-25,multiply(test_multiplier,-5,5)),
	?_assertEqual(1.125,multiply(test_multiplier,1.5,0.75)),
	?_assertEqual({fail,bob,is_not_number},multiply(test_multiplier,bob,3)),
	?_assertEqual({fail,sue,is_not_number},multiply(test_multiplier,3,sue)),
	?_assertEqual({fail,bob,is_not_number},multiply(test_multiplier,bob,sue))
	]
}.

divider_test_() ->
{setup,
	fun()->%runs before any of the tests
			Pid = spawn(?MODULE,divider,[]),	
			register(test_divider,Pid)
		end,
	%fun(_)->%runs after all of the tests
		%there is no teardown needed, so this fun doesn't need to be implemented.
	%end,
	%factorializer tests start here
	[ ?_assert((1.6 < divide(test_divider,5,3)) and (divide(test_divider,5,3) < 1.7)),%happy path
	%nasty thoughts start here
	?_assertEqual(-1.0,divide(test_divider,-5,5)),
	?_assertEqual(2.0,divide(test_divider,1.5,0.75)),
	?_assertEqual({fail,bob,is_not_number},divide(test_divider,bob,3)),
	?_assertEqual({fail,sue,is_not_number},divide(test_divider,3,sue)),
	?_assertEqual({fail,bob,is_not_number},divide(test_divider,bob,sue))
	]
}.

-endif.