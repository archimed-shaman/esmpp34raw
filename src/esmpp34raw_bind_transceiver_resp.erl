-module(esmpp34raw_bind_transceiver_resp).
-author("Morozov Alexander aka ~ArchimeD~").

-export([
         unpack/1,
         pack/1
        ]).

-include("esmpp34raw_types.hrl").
-include("esmpp34raw_tags.hrl").
-include("esmpp34raw_utils.hrl").



-spec unpack (binary())                 -> #bind_transceiver_resp{}.
-spec pack   (#bind_transceiver_resp{}) -> binary().



unpack(Stream) when is_binary(Stream) ->
    {SystemId, Stream_1} = esmpp34raw_utils:get_var_c_octet_string(Stream, 16),
    unpack_optional(#bind_transceiver_resp{system_id = SystemId}, Stream_1).



pack(#bind_transceiver_resp{system_id = SystemId, sc_interface_version = ScInterfaceVersion}) ->
    BinSystemId            = esmpp34raw_utils:pack_var_c_octet_string(SystemId, 16),
    BinScInterfaceVersion  = ?pack_optional(ScInterfaceVersion, ?sc_interface_version),
    <<BinSystemId/binary, BinScInterfaceVersion/binary>>.



?unpack_optional (bind_transceiver_resp);
?define_optional (bind_transceiver_resp, sc_interface_version, ?sc_interface_version);
?optional_done   (bind_transceiver_resp).
