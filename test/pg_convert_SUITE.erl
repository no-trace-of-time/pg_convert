%%%-------------------------------------------------------------------
%%% @author simon
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. 十月 2017 11:44
%%%-------------------------------------------------------------------
-module(pg_convert_SUITE).
-include_lib("eunit/include/eunit.hrl").
-author("simon").

%% API
-export([]).


setup() ->
  lager:start(),
  application:start(pg_convert),

  env_init(),

  ok.

env_init() ->
  Cfgs = [
    {pg_convert,
      [
        {debug, true}
        , {field_existance_validate, true}

      ]
    }
  ],

  pg_test_utils:env_init(Cfgs),
  ok.


my_test_() ->
  {
    setup,
    fun setup/0,
    {
      inorder,
      [
        fun convert_test_1/0
      ]
    }
  }.

-define(M_TEST, pg_convert_t_protocol_up_resp_pay).

%%=============================================================================
convert_test_1() ->
  Protocol = pg_model:new(?M_TEST, []),

  ProtocolResult = pg_model:set(?M_TEST, Protocol,
    [{version, <<"5.0.0.3.3">>}, {encoding, <<"GBK">>}]),
  lager:start(),
  ?assertEqual(ProtocolResult, pg_convert:convert(?M_TEST, [Protocol, Protocol], list_copy)),

  ProtocolResult2 = pg_model:set(?M_TEST, Protocol, [{version, <<"5.0.0.3.3">>}]),
  ?assertEqual(ProtocolResult2, pg_convert:convert(?M_TEST, Protocol)),
  ?assertEqual(ProtocolResult2, pg_convert:convert(?M_TEST, Protocol, default)),
  ?assertEqual(ProtocolResult2, pg_convert:convert(?M_TEST, [Protocol])),

  ?assertEqual(Protocol,
    pg_convert:convert(?M_TEST, Protocol, all_fields)),
  ?assertEqual(ProtocolResult,
    pg_convert:convert(?M_TEST, [Protocol, Protocol], all_fields_plus_one)),
  ok.
