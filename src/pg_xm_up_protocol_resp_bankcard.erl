%%%-------------------------------------------------------------------
%%% @author simon
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. 十月 2017 14:02
%%%-------------------------------------------------------------------
-module(pg_xm_up_protocol_resp_bankcard).
-include_lib("eunit/include/eunit.hrl").
-include_lib("mixer/include/mixer.hrl").
-compile({parse_trans, exprecs}).
-author("simon").
-behaviour(pg_model).
-behavior(pg_protocol).
-behaviour(pg_up_protocol).

%% API
%% callbacks of up protocol
-mixin([{pg_xm_up_protocol, [
  pr_formatter/1
  , in_2_out_map/0
]}]).

%% API
%% callbacks of pg_up_protocol
-export([
  sign_fields/0
  , options/0
%%  , to_list/1
]).
%% callbacks of pg_protocol
-export([
  convert_config/0
]).


-compile(export_all).
%%-------------------------------------------------------------------------
-define(P, ?MODULE).

-record(?P, {
  code
  ,message
  ,signature
  ,out_trade_no
  ,tran_amt
  ,tran_time
}).

-type ?P() :: #?P{}.
-export_type([?P/0]).
-export_records([?P]).

%%---------------------------------------------------------------------------------
sign_fields() ->
  [
    code
    ,message
    ,out_trade_no
    ,tran_amt
    ,tran_time
  ].

options() ->
  #{
    channel_type => xm_up,
    txn_type => xm_up_bankcard,
    direction => resp
  }.


convert_config() ->
  [
    %% up_req_resp -> up_txn_log
    {default,
      [
        {to, proplists},
        {from,
          [
            {?MODULE,
              [
               {up_respCode,code}
                , {up_respMsg, message}
                , {txnAmt, tran_amt}
                , {txn_status, {fun txn_status/1, [code]}}
              ]
            }
          ]
        }
      ]
    }
  ].

txn_status(RespCode)->
 case lists:member(RespCode,xm_up_config:get_config(success_resp_code_list)) of
   true -> success;
   _ -> fail
 end.




