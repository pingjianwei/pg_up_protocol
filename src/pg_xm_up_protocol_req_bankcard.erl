%%%-------------------------------------------------------------------
%%% @author simon
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. 十月 2017 14:02
%%%-------------------------------------------------------------------
-module(pg_xm_up_protocol_req_bankcard).
-include_lib("eunit/include/eunit.hrl").
-include_lib("mixer/include/mixer.hrl").
-compile({parse_trans, exprecs}).
-author("simon").
-behavior(pg_protocol).
-behaviour(pg_up_protocol).

%% API
%% callbacks of up protocol
-mixin([{pg_xm_up_protocol, [
  pr_formatter/1
  , in_2_out_map/0
]}]).

%% API
%% callbacks of pg_xm_up_protocol
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
  signature = <<"0">> :: pg_up_protocol:signature()
  , merId = <<"012345678901234">> :: pg_up_protocol:merId()
  , orderId = <<"0">> :: pg_up_protocol:orderId()
  , txnTime = <<"19991212090909">> :: pg_up_protocol:txnTime()
  , verifyType= <<"0040">>
  , accNo = <<>> :: binary()
  , mcht_index_key = <<>> :: pg_up_protocol:mcht_index_key()
  , certNo = <<>> :: pg_mcht_protocol:id_no()
  , accName = <<>> :: pg_mcht_protocol:id_name()
  , phone = <<>> :: pg_mcht_protocol:mobile()
}).

-type ?P() :: #?P{}.
-export_type([?P/0]).
-export_records([?P]).

%%---------------------------------------------------------------------------------
sign_fields() ->
  [
    orderId
    , txnTime
    , verifyType
    , accNo
    , accName
    , certNo
    , phone
  ].

options() ->
  #{
    channel_type => xm_up,
    txn_type=>xm_bankcard,
    direction => req
  }.


convert_config() ->
  [
    %% mcht_req_collect -> up_req_collect
    {default,
      [
        {to, pg_xm_up_protocol_req_bankcard},
        {from,
          [
            {pg_mcht_protocol, pg_mcht_protocol_req_bankcard,
              [
                {accNo, bank_card_no}
                , {merId, {fun mer_id/1, [mcht_id]}}
                , {certNo, id_no}
                , {accName, id_name}
                , {phone, mobile}
                , {accNo, bank_card_no}
                , {txnTime, {fun now_txn/0, []}}
                , {orderId, {fun xfutils:get_new_order_id/0, []}}
                , {verifyType, {fun get_verify_type/1, [mobile]}}
                , {mcht_index_key, mcht_index_key}
              ]
            }
          ]
        }
      ]
    },
    {save_req,
      [
        {to, {fun repo_up_module/0, []}},
        {from,
          [
            {?MODULE,
              [
                {txn_type, {static, xm_bankcatd}}
                , {txn_status, {static, waiting}}
                , {mcht_index_key, pg_model, mcht_index_key}
                , {up_merId, merId}
                , {up_txnTime, txnTime}
                , {up_orderId, orderId}
                , {up_index_key, pg_up_protocol, up_index_key}
                , {up_accNo, accNo}
                , {up_idNo, certNo}
                , {up_idName, accName}
                , {up_mobile, phone}
              ]
            }
          ]
        }
      ]
    }
  ].

repo_up_module() ->
  pg_up_protocol:repo_module(up_txn_log).

-define(APP, pg_up_protocol).
xm_up_mer_id(MchtId) ->
  MRepoMchants = pg_up_protocol:repo_module(mchants),
%%  {ok, MRepoMchants} = application:get_env(?APP, mchants_repo_name),
  [PaymentMethod] = pg_repo:fetch_by(MRepoMchants, MchtId, payment_method),
  MerId = xm_up_config:get_mer_id(PaymentMethod),
  MerId.

mer_id(MchtId) ->
  MerIdAtom = xm_up_mer_id(MchtId),
  ?debugFmt("MerId = ~p", [MerIdAtom]),
  MerIdBin = atom_to_binary(MerIdAtom, utf8),
  MerIdBin.

mer_id_test_1() ->
  ?assertEqual(<<"898319849000017">>, mer_id(1)),
  ok.
%%------------------------------------------------------------------------------
public_key(MchtId) ->
  MerId = mer_id(MchtId),
  PublicKey = up_config:get_mer_prop(MerId, publicKey),
  PublicKey.

now_txn() ->
  datetime_x_fin:now(txn).
%%--------------------------------------------------------------------------
get_verify_type(Mobile) ->
  case Mobile of
    unfined -> <<"0030">>;
    <<>> -> <<"0030">>;
    _ -> <<"0040">>
  end.






