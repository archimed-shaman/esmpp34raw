-module(esmpp34raw_bind_transmitter).
-author("Morozov Alexander aka ~ArchimeD~").

-export([
	 unpack/1,
	 pack/1
	]).

-include("esmpp34raw_types.hrl").



-spec unpack (binary())            -> #bind_transmitter{}.
-spec pack   (#bind_transmitter{}) -> binary().



unpack(Stream) when is_binary(Stream) ->
    {SystemId, Stream_1}   = esmpp34raw_utils:get_var_c_octet_string(Stream, 16),
    {Password, Stream_2}   = esmpp34raw_utils:get_var_c_octet_string(Stream_1, 9),
    {SystemType, Stream_3} = esmpp34raw_utils:get_var_c_octet_string(Stream_2, 13),
    <<InterfaceVersion: 8/big-unsigned-integer, 
      AddrTon:          8/big-unsigned-integer,
      AddrNpi:          8/big-unsigned-integer,
      Stream_4           /binary>> = Stream_3,
    {AddressRange, _}      = esmpp34raw_utils:get_var_c_octet_string(Stream_4, 41),
    #bind_transmitter {system_id         = SystemId,
		       password          = Password,
		       system_type       = SystemType,
		       interface_version = InterfaceVersion,
		       addr_ton          = AddrTon,
		       addr_npi          = AddrNpi,
		       address_range     = AddressRange}.



pack(#bind_transmitter{system_id         = SystemId,
		       password          = Password,
		       system_type       = SystemType,
		       interface_version = InterfaceVersion,
		       addr_ton          = AddrTon,
		       addr_npi          = AddrNpi,
		       address_range     = AddressRange}) ->
    BinSystemId     = esmpp34raw_utils:pack_var_c_octet_string(SystemId, 16),
    BinPassword     = esmpp34raw_utils:pack_var_c_octet_string(Password, 9),
    BinSystemType   = esmpp34raw_utils:pack_var_c_octet_string(SystemType, 13),
    BinAddressRange = esmpp34raw_utils:pack_var_c_octet_string(AddressRange, 41),
    <<BinSystemId        /binary,
      BinPassword        /binary,
      BinSystemType      /binary,
      InterfaceVersion: 8/big-unsigned-integer,
      AddrTon:          8/big-unsigned-integer,
      AddrNpi:          8/big-unsigned-integer,
      BinAddressRange    /binary>>.
