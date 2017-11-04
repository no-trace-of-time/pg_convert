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
-behaviour(pg_convert).

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


-compile(export_all).
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
        {to, ?MODULE},
        {from,
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
        }
      ]
    },
    {all_fields,
      [
        {to, ?MODULE},
        {from,
          [
            {?TXN, all}
          ]
        }

      ]
    },
    {all_fields_plus_one,
      [
        {to, ?MODULE},
        {from,
          [
            {?TXN,
              [
                {version, {fun t1/1, [version]}}
                , {encoding, {fun t2/0, []}}
              ]}
            , {?TXN, all}
          ]
        }

      ]
    },
    {default,
      [
        {to, ?MODULE},
        {from,
          [
            {?TXN,
              [
                {version, {fun t1/1, [version]}}
              ]
            }
          ]
        }
      ]
    },
    {from_name_is_func,
      [
        {to, ?MODULE},
        {from,
          [
            {{fun from_model_name/0, []},
              [
                {version, {fun t1/1, [version]}}
              ]
            }
          ]

        }
      ]
    },
    {to_proplists,
      [
        {to, proplists},
        {from,
          [
            {{fun from_model_name/0, []},
              [
                {version, {fun t1/1, [version]}}
              ]
            }
          ]

        }
      ]
    }
  ].

from_model_name() ->
  ?MODULE.

from_model_name_test() ->
  Model = pg_model:new_empty(?MODULE),
  ModelResult = pg_convert:convert(?MODULE, Model, from_name_is_func),
  ?assertEqual(<<"5.0.0.3.3">>, pg_model:get(?MODULE, ModelResult, version)),
  ok.

to_proplists_test() ->
  Model = pg_model:new_empty(?MODULE),
  VLResult = pg_convert:convert(?MODULE, Model, to_proplists),
  ?assertEqual(<<"5.0.0.3.3">>, proplists:get_value(version, VLResult)),
  ok.


t1(A) ->
  <<A/binary, ".3.3">>.

t2() ->
  <<"GBK">>.

%%======================================================================
%% UT functions

