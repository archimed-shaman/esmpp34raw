-module(esmpp34raw_outbind).
-author("Morozov Alexander aka ~ArchimeD~").

-export([
	 unpack/1,
	 pack/1
	]).

-include("esmpp34raw_types.hrl").
-include("esmpp34raw_tags.hrl").



-spec unpack (binary())   -> #outbind{}.
-spec pack   (#outbind{}) -> binary().



unpack(Stream) when is_binary(Stream) ->
    {SystemId, Stream_1} = esmpp34raw_utils:get_var_c_octet_string(Stream, 16),
    {Password, _}        = esmpp34raw_utils:get_var_c_octet_string(Stream_1, 9),
    #outbind {system_id = SystemId,
	      password  = Password}.


pack(#outbind{system_id = SystemId,
	      password  = Password}) ->
    BinSystemId     = esmpp34raw_utils:pack_var_c_octet_string(SystemId, 16),
    BinPassword     = esmpp34raw_utils:pack_var_c_octet_string(Password, 9),
    <<BinSystemId /binary,
      BinPassword /binary>>.
