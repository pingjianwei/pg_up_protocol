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
  , des3_ecb_encrypt/2
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

%%厦门银联银行卡验证内外字段映射配置原则：key的配置和银联全渠道一致
in_2_out_map() ->
  #{
    %%common fiels of req and resp
    out_trade_no => <<"out_trade_no">>
    , tran_time=> <<"tran_time">>
    , signature=> <<"sign">>
    %%req fields
    , customer_code=> <<"customer_code">>
    , verify_type=> <<"verify_type">>
    , acct_no=> <<"acct_no">>
    , name=> <<"name">>
    , cert_no=> <<"cert_no">>
    , phone => <<"phone">>

    %%resp fields
    , code=> <<"code">>
    , message=> <<"message">>
    , tran_amt=> <<"tran_amt">>


  }.

%%------------------------------------------------------
-spec get(M :: atom(), Model :: pg_model:pg_model(), Field :: atom())
      -> Value :: any().

get(M, Model, up_index_key) when is_atom(M), is_tuple(Model) ->
  {
    pg_model:get(M, Model, customer_code)
    , pg_model:get(M, Model, tran_time)
    , pg_model:get(M, Model, out_trade_no)
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

  PK = xm_up_config:get_config(public_key),

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
  [MerId] = pg_model:get(M, P, [customer_code]),
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
  SignBin = do_sign(SignString, Key),
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
  Bin = list_to_binary(L),
  binary:part(Bin, 0, byte_size(Bin) - 1).

one_sign_field(_X, EmptyValue)
  when (EmptyValue =:= <<>>)
  or (EmptyValue =:= undefined)
  ->
  [];
one_sign_field(X, Value) when is_integer(Value) ->
  [X, <<"=">>, integer_to_binary(Value), <<"&">>];
one_sign_field(X, Value) when is_binary(Value);is_list(Value) ->
  [X, <<"=">>, Value, <<"&">>].

%%---------------------------------------------------
-spec signature_decode(binary()) -> binary().
signature_decode(Signature) ->
  base64:decode(Signature).

%%---------------------------------------------------
out_2_in(M, PV) when is_atom(M), is_list(PV) ->
  pg_protocol:out_2_in(M, PV).
%%---------------------------------------------------
out_fields(M) when is_atom(M) ->
  [signature | M:sign_fields()] ++ [customer_code].
%%---------------------------------------------------
in_2_out(M, Protocol, proplists) when is_atom(M), is_tuple(Protocol) ->
  pg_model:to(M, Protocol, {proplists, out_fields(M), in_2_out_map()});
in_2_out(M, Protocol, post) when is_atom(M), is_tuple(Protocol) ->
  pg_model:to(M, Protocol, {poststring, out_fields(M), in_2_out_map()});
in_2_out(M, Protocol, json) when is_atom(M), is_tuple(Protocol) ->
  Proplists = pg_model:to(M, Protocol, {proplists, out_fields(M), in_2_out_map()}),
  jsx:encode(Proplists).

%%-----------------------------------------------------
des3_ecb_encrypt(Key, PlainText) when is_binary(Key), is_binary(PlainText) ->
  F = fun(N, Acc) ->
    SubKey = binary:part(Key, N * 8, 8),
    case N of
      0 -> crypto:block_encrypt(des_ecb, SubKey, pkcs5_padding(Acc, 8));
      1 -> crypto:block_decrypt(des_ecb, SubKey, Acc);
      2 -> crypto:block_encrypt(des_ecb, SubKey, Acc)
    end
      end,
  EncryptedData = lists:foldl(F, PlainText, lists:seq(0, 2)),
  lager:debug("KeysList:~p ;PlainText:~p", [hexstring(EncryptedData), PlainText]),
  hexstring(EncryptedData).

pkcs5_padding(PlainText, BlockSize) when is_binary(PlainText) ->
  Rem = size(PlainText) rem BlockSize,
  Padding = lists:duplicate(BlockSize - Rem, BlockSize - Rem),
  Binary = list_to_binary(Padding),
  <<PlainText/binary, Binary/binary>>;
pkcs5_padding(PlainText, BlockSize) when is_list(PlainText) ->
  Rem = length(PlainText) rem BlockSize,
  Padding = lists:duplicate(BlockSize - Rem, BlockSize - Rem),
  PlainText ++ Padding.

hexstring(Binary) when is_binary(Binary) ->
  lists:flatten(lists:map(
    fun(X) -> io_lib:format("~2.16.0b", [X]) end,
    binary_to_list(Binary))).




