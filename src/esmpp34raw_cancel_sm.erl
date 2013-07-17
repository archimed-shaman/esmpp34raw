-module(esmpp34raw_cancel_sm).
-author("Morozov Alexander aka ~ArchimeD~").

-export([
	 unpack/1,
	 pack/1
	]).

-include("esmpp34raw_types.hrl").
-include("esmpp34raw_tags.hrl").



-spec unpack (binary())      -> #cancel_sm{}.
-spec pack   (#cancel_sm{})  -> binary().



unpack(Stream) when is_binary(Stream) ->
    {ServiceType, Stream_1} = esmpp34raw_utils:get_var_c_octet_string(Stream, 6),
    {MessageId, Stream_2}   = esmpp34raw_utils:get_var_c_octet_string(Stream_1, 65),
    << SourceAddrTon: 8/big-unsigned-integer,
       SourceAddrNpi: 8/big-unsigned-integer,
       Stream_3        /binary>> = Stream_2,
    {SourceAddr, Stream_4}  = esmpp34raw_utils:get_var_c_octet_string(Stream_3, 21),
    << DestAddrTon:   8/big-unsigned-integer,
       DestAddrNpi:   8/big-unsigned-integer,
       Stream_5        /binary>> = Stream_4,
    {DestinationAddr, <<>>} = esmpp34raw_utils:get_var_c_octet_string(Stream_5, 21),
    #cancel_sm { service_type     = ServiceType,
		 message_id       = MessageId,
		 source_addr_ton  = SourceAddrTon,
		 source_addr_npi  = SourceAddrNpi,
		 source_addr      = SourceAddr,
		 dest_addr_ton    = DestAddrTon,
		 dest_addr_npi    = DestAddrNpi,
		 destination_addr = DestinationAddr}.



pack(#cancel_sm{} = Body) ->
    ServiceType     = esmpp34raw_utils:pack_var_c_octet_string(Body#cancel_sm.service_type, 6),
    MessageId       = esmpp34raw_utils:pack_var_c_octet_string(Body#cancel_sm.message_id, 65),
    SourceAddrTon   = Body#cancel_sm.source_addr_ton,
    SourceAddrNpi   = Body#cancel_sm.source_addr_npi,
    SourceAddr      = esmpp34raw_utils:pack_var_c_octet_string(Body#cancel_sm.source_addr, 21),
    DestAddrTon     = Body#cancel_sm.dest_addr_ton,
    DestAddrNpi     = Body#cancel_sm.dest_addr_npi,
    DestinationAddr = esmpp34raw_utils:pack_var_c_octet_string(Body#cancel_sm.destination_addr, 21),
    << ServiceType     /binary,
       MessageId       /binary,
       SourceAddrTon: 8/big-unsigned-integer,
       SourceAddrNpi: 8/big-unsigned-integer,
       SourceAddr      /binary,
       DestAddrTon:   8/big-unsigned-integer,
       DestAddrNpi:   8/big-unsigned-integer,
       DestinationAddr /binary >>.

