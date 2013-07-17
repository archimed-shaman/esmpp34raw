-module(esmpp34raw_alert_notification).
-author("Morozov Alexander aka ~ArchimeD~").

-export([
	 unpack/1,
	 pack/1
	]).

-include("esmpp34raw_types.hrl").
-include("esmpp34raw_tags.hrl").
-include("esmpp34raw_utils.hrl").



-spec unpack (binary())              -> #alert_notification{}.
-spec pack   (#alert_notification{}) -> binary().



unpack(Stream) when is_binary(Stream) ->
    << SourceAddrTon: 8/big-unsigned-integer,
       SourceAddrNpi: 8/big-unsigned-integer,
       Stream_1        /binary >> = Stream,
    {SourceAddr, Stream_2} = esmpp34raw_utils:get_var_c_octet_string(Stream_1, 65),
    << EsmeAddrTon:   8/big-unsigned-integer,
       EsmeAddrNpi:   8/big-unsigned-integer,
       Stream_3        /binary >> = Stream_2,
    {EsmeAddr, Stream_4}   = esmpp34raw_utils:get_var_c_octet_string(Stream_3, 65),
    unpack_optional(#alert_notification{ source_addr_ton = SourceAddrTon,
					 source_addr_npi = SourceAddrNpi,
					 source_addr     = SourceAddr,
					 esme_addr_ton   = EsmeAddrTon,
					 esme_addr_npi   = EsmeAddrNpi,
					 esme_addr       = EsmeAddr }, Stream_4).



pack(#alert_notification{} = Data) ->
    SourceAddrTon        = Data#alert_notification.source_addr_ton,
    SourceAddrNpi        = Data#alert_notification.source_addr_npi,
    SourceAddr           = esmpp34raw_utils:pack_var_c_octet_string(Data#alert_notification.source_addr, 65),
    EsmeAddrTon          = Data#alert_notification.esme_addr_ton,
    EsmeAddrNpi          = Data#alert_notification.esme_addr_npi,
    EsmeAddr             = esmpp34raw_utils:pack_var_c_octet_string(Data#alert_notification.esme_addr, 65),

    MsAvailabilityStatus = ?pack_optional(Data#alert_notification.ms_availability_status, ?ms_availability_status),
    << SourceAddrTon:      8/big-unsigned-integer,
       SourceAddrNpi:      8/big-unsigned-integer,
       SourceAddr           /binary,
       EsmeAddrTon:        8/big-unsigned-integer,
       EsmeAddrNpi:        8/big-unsigned-integer,
       EsmeAddr             /binary,
       MsAvailabilityStatus /binary>>.



?unpack_optional (alert_notification);
?define_optional (alert_notification, ms_availability_status, ?ms_availability_status);
?optional_done   (alert_notification).

