%%%-------------------------------------------------------------------
%%% @author pingjianwei
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. 十一月 2017 17:06
%%%-------------------------------------------------------------------
-module(pg_xm_up_protocol).
-include_lib("eunit/include/eunit.hrl").
-include("include/type_up_protocol.hrl").
-compile({parse_trans, ct_expand}).
-author("pingjianwei").

%% callbacks
-callback sign_fields() -> [atom()].
-callback options() -> map().

%% API
-export([]).

%% API exports
%% callbacks of pg_protocol
-export([
  in_2_out_map/0
]).

%% callbacks of pg_model
-export([
  pr_formatter/1
]).

%% own api
-export([
  get/3
  , verify/2
  , sign_string/2
  , sign/2
%%  , sign/4
  , validate_format/1
  , save/2
  , repo_module/1
  , out_2_in/2
  , in_2_out/3
]).



-define(APP, pg_up_protocol).
%%====================================================================
%% API functions
%%====================================================================
pr_formatter(Field)
  when (Field =:= respMsg)
  or (Field =:= reserved)
  or (Field =:= reqReserved)
  or (Field =:= origRespMsg)
  ->
  string;
pr_formatter(_) ->
  default.


%%------------------------------------------------------
build_in_2_out_map() ->
  Fields = [
    %%common fiels of req and resp
    orderId
    , txnTime
    , signature
    %%req fields
    , merId
    , txnTyp
    , accNo
    , accName
    , certNo
    , phone

    %%resp fields
    , respCode
    , respMsg
    , {txnAmt, integer}
  ],

  F = fun
        ({Key, Type}) ->
          {Key, {atom_to_binary(Key, utf8), Type}};
        (Key) ->
          {Key, atom_to_binary(Key, utf8)}
      end,
  VL = [F(Field) || Field <- Fields],
  maps:from_list(VL).

%%厦门银联银行卡验证内外字段映射配置原则：key的配置和银联全渠道一致
in_2_out_map() ->
  #{
    %%common fiels of req and resp
    orderId => <<"out_trade_no">>
    , txnTime=> <<"tran_time">>
    , signature=> <<"sign">>
    %%req fields
    , merId=> <<"customer_code">>
    , verifyType=> <<"verify_type">>
    , accNo=> <<"acct_no">>
    , accName=> <<"name">>
    , certNo=> <<"cert_no">>
    , phone => <<"phone">>

    %%resp fields
    , respCode=> <<"code">>
    , respMsg=> <<"message">>
    , txnAmt=> <<"tran_amt">>


  }.

%%------------------------------------------------------
-spec get(M :: atom(), Model :: pg_model:pg_model(), Field :: atom())
      -> Value :: any().

get(M, Model, up_index_key) when is_atom(M), is_tuple(Model) ->
  {
    pg_model:get(M, Model, merId)
    , pg_model:get(M, Model, txnTime)
    , pg_model:get(M, Model, orderId)
  };
get(M, Model, Field) when is_atom(Field) ->
  pg_model:get(M, Model, Field);
get(M, Model, Fields) when is_list(Fields) ->
  [?MODULE:get(M, Model, Field) || Field <- Fields].

%%------------------------------------------------------
-spec verify(M, Protocol) -> PassOrNot when
  M :: atom(),
  Protocol :: pg_model:pg_model(),
  PassOrNot :: ok | fail.

verify(M, P) when is_atom(M), is_tuple(P) ->
  SignString = sign_string(M, P),

  Sig = pg_model:get(M, P, signature),
  SigDecoded = signature_decode(Sig),

  PK = up_config:get_config(public_key),

  case public_key:verify(SignString, sha, SigDecoded, PK) of
    true -> ok;
    false ->
      UpIndexKey = get(M, P, up_index_key),
      lager:error("Up Txn ~p sig verify failed.SignString = ~ts,Sig = ~ts",
        [UpIndexKey, SignString, Sig]),
      fail
  end.

%%------------------------------------------------
-spec sign(M, P) -> {SignString, Sig} when
  M :: atom(),
  P :: pg_model:pg_model(),
  SignString :: binary(),
  Sig :: binary() | iolist().

sign(M, P) when is_atom(M), is_tuple(P) ->
  SignString = sign_string(M, P),
  [MerId] = pg_model:get(M, P, [merId]),
  Sig = sign(M, SignString, MerId),
  {SignString, Sig}.

-spec sign(M, SignString, MerId) -> Sig when
  M :: atom(),
  SignString :: binary(),
  MerId :: pg_up_protocol:merId(),
  Sig :: binary() | iolist().
sign(M, SignString, MerId)
  when is_atom(M), is_binary(SignString), is_binary(MerId) ->
  Key = xm_up_config:get_mer_prop(MerId, privateKey),
  SignBin=do_sign(SignString, Key),
  lager:debug("SignString = ~ts,Sig=~ts", [SignString, SignBin]),
  ?debugFmt("SignString = ~ts,Sig=~ts", [SignString, SignBin]),
  SignBin.

do_sign(DigestBin, PK) when is_binary(DigestBin) ->
  base64:encode(public_key:sign(DigestBin, 'sha', PK)).


%%------------------------------------------------
validate_format(P) ->
  ok.

%%------------------------------------------------
-spec save(M, P) -> Result when
  M :: atom(),
  P :: pg_model:pg_model(),
  Result :: ok|fail.

save(M, P) when is_atom(M), is_tuple(P) ->
%%  VL = M:to_list(P),
%%  MRepo = repo_up_module(),
%%  Repo = pg_model:new(MRepo, VL),
  Repo = pg_convert:convert(M, [P], save_req),
  xfutils:cond_lager(?MODULE, debug, error, "Repo to be saved = ~p", [Repo]),
  lager:error("Repo to be saveddd = ~p", [Repo]),
  pg_repo:save(Repo).


%%------------------------------------------------
repo_module(up_txn_log) ->
  {ok, Module} = application:get_env(?APP, up_repo_name),
  Module;
repo_module(mchants) ->
  {ok, Module} = application:get_env(?APP, mchants_repo_name),
  Module.
%%====================================================================
%% Internal functions
%%====================================================================
-spec sign_string(M, Model) -> Sig when
  M :: atom(),
  Model :: pg_model:pg_model(),
  Sig :: binary().
sign_string(M, Model) when is_atom(M), is_tuple(Model) ->
  SignFields = M:sign_fields(),
  L = [
    one_sign_field(string:to_upper(atom_to_list(X)), pg_model:get(M, Model, X))
    || X <- SignFields
  ],
  list_to_binary(L).

one_sign_field(X, Value) when((X =:= "PHONE") or (X =:= "CERTNO")) and
  ( Value =/= <<>>) and (Value =/= undefined) ->
  %% last field
  [X, <<"=">>, Value];
one_sign_field(_X, EmptyValue)
  when (EmptyValue =:= <<>>)
  or (EmptyValue =:= undefined)
  ->
  [];
one_sign_field(X, Value) when is_integer(Value) ->
  [X, <<"=">>, integer_to_binary(Value), <<"&">>];
one_sign_field(X, Value) when is_binary(Value);is_list(Value) ->
  [X, <<"=">>, Value, <<"&">>].

one_sign_field_test() ->
  ?assertEqual(<<"PHONE=13721422283">>, list_to_binary(one_sign_field("PHONE", "13721422283"))),
  ?assertEqual(<<"NAME=jack&">>, list_to_binary(one_sign_field("NAME", "jack"))),
  ?assertEqual([], one_sign_field("NAME", <<>>)).

%%---------------------------------------------------


%%---------------------------------------------------
-spec signature_decode(binary()) -> binary().
signature_decode(Signature) ->
  base64:decode(Signature).

%%---------------------------------------------------
out_2_in(M, PV) when is_atom(M), is_list(PV) ->
  pg_protocol:out_2_in(M, PV).
%%---------------------------------------------------
out_fields(M) when is_atom(M) ->
  [signature | M:sign_fields()].
%%---------------------------------------------------
in_2_out(M, Protocol, proplists) when is_atom(M), is_tuple(Protocol) ->
  pg_model:to(M, Protocol, {proplists, out_fields(M), in_2_out_map()});
in_2_out(M, Protocol, post) when is_atom(M), is_tuple(Protocol) ->
  pg_model:to(M, Protocol, {poststring, out_fields(M), in_2_out_map()}).

%%-----------------------------------------------------



