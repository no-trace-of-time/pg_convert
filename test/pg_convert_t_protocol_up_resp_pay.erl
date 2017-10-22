%%%-------------------------------------------------------------------
%%% @author simon
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. Jan 2017 9:30 PM
%%%-------------------------------------------------------------------
-module(pg_convert_t_protocol_up_resp_pay).
-compile({parse_trans, exprecs}).
-include_lib("eunit/include/eunit.hrl").
%%-include("include/type_binaries.hrl").
%%-include("include/type_up_protocol.hrl").
-author("simon").
-behavior(pg_model).
-behaviour(pg_convert).
%%-behaviour(pg_protocol).

%% API
%% callbacks of pg_protocol
-export([
  in_2_out_map/0
]).

-export([
  convert_config/0
]).
%% callbacks of pg_model
-export([
  pr_formatter/1
  , get/2
]).


%%-------------------------------------------------------------------
-define(TXN, ?MODULE).

-record(?TXN, {
  version = <<"5.0.0">>
  , encoding = <<"UTF-8">>
  , certId = <<"9">>
  , signature = <<"sig">>
  , signMethod = <<"01">>
  , txnType = <<"01">>
  , txnSubType = <<"01">>
  , bizType = <<"000201">>
  , accessType = <<"0">>
  , merId = <<"012345678901234">>
  , orderId = <<"01234567">>
  , txnTime = <<"19991212121212">>
  , txnAmt = <<"0">>
  , currencyCode = <<"156">>
  , reqReserved = <<"reqReserved">>
  , reserved = <<>>
  , accNo = <<>>
  , queryId = <<>>
  , respCode = <<>>
  , respMsg = <<>>
  , settleAmt = <<>>
  , settleCurrencyCode = <<>>
  , settleDate = <<>>
  , traceNo = <<>>
  , traceTime = <<>>
  , exchangeRate
}).
-type ?TXN() :: #?TXN{}.
%%-opaque ?TXN() :: #?TXN{}.
-export_type([?TXN/0]).
-export_records([?TXN]).

%%-------------------------------------------------------------------
pr_formatter(Field)
  when (Field =:= reqReserved)
  or (Field =:= respMsg)
  or (Field =:= signature)
  ->
  string;
pr_formatter(_) ->
  default.

%%-------------------------------------------------------------------
get(Protocol, up_index_key) when is_tuple(Protocol) ->
  VL = pg_model:get(Protocol, [merId, txnTime, orderId]),
  list_to_tuple(VL);
get(Protocol, Key) when is_tuple(Protocol), is_atom(Key) ->
  pg_model:get(?MODULE, Protocol, Key).

%%-------------------------------------------------------------------
in_2_out_map() ->
  #{

    version => <<"version">>
    , encoding=> <<"encoding">>
    , certId=> <<"certId">>
    , signature=> <<"signature">>
    , signMethod=> <<"signMethod">>
    , txnType=> <<"txnType">>
    , txnSubType=> <<"txnSubType">>
    , bizType=> <<"bizType">>
    , channelType=> <<"channelType">>
    , accessType=> <<"accessType">>
    , merId=> <<"merId">>
    , txnTime=> <<"txnTime">>
    , orderId=> <<"orderId">>
    , queryId=> <<"queryId">>

    , txnAmt => {<<"txnAmt">>, integer}
    , currencyCode =><<"currencyCode">>
    , reqReserved =><<"reqReserved">>
    , respCode =><<"respCode">>
    , respMsg =><<"respMsg">>
    , settleAmt =>{<<"settleAmt">>, integer}
    , settleCurrencyCode =><<"settleCurrencyCode">>
    , settleDate =><<"settleDate">>
    , traceNo =><<"traceNo">>
    , traceTime =><<"traceTime">>
    , txnSubType =><<"txnSubType">>
    , txnTime =><<"txnTime">>
    , reserved =><<"reserved">>
    , accNo =><<"accNo">>

    , origRespCode => <<"origRespCode">>
    , origRespMsg => <<"origRespMsg">>
    , issuerIdentifyMode => <<"issuerIdentifyMode">>

    , exchangeRate => <<"exchangeRate">>
  }.


convert_config() ->
  [
    {list_copy,
      [
        {?TXN,
          [
            {version, {fun t1/1, [version]}}
          ]
        },
        {?TXN,
          [
            {encoding, {fun t2/0, []}}

          ]
        }
      ]
    },
    {default,
      [
        {?TXN,
          [
            {version, {fun t1/1, [version]}}
          ]
        }
      ]
    }
  ].

t1(A) ->
  <<A/binary, ".3.3">>.

t2() ->
  <<"GBK">>.

%%======================================================================
%% UT functions
convert_test() ->
  Protocol = pg_model:new(?MODULE, []),

  ProtocolResult = pg_model:set(?MODULE, Protocol,
    [{version, <<"5.0.0.3.3">>}, {encoding, <<"GBK">>}]),
  lager:start(),
  ?assertEqual(ProtocolResult, pg_convert:convert(?MODULE, [Protocol, Protocol], list_copy)),

  ProtocolResult2 = pg_model:set(?MODULE, Protocol, [{version, <<"5.0.0.3.3">>}]),
  ?assertEqual(ProtocolResult2, pg_convert:convert(?MODULE, Protocol)),
  ?assertEqual(ProtocolResult2, pg_convert:convert(?MODULE, Protocol, default)),
  ?assertEqual(ProtocolResult2, pg_convert:convert(?MODULE, [Protocol])),
  ok.

