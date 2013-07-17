-module(esmpp34raw_replace_sm).
-author("Morozov Alexander aka ~ArchimeD~").

-export([
	 unpack/1,
	 pack/1
	]).

-include("esmpp34raw_types.hrl").
-include("esmpp34raw_tags.hrl").



-spec unpack (binary())      -> #replace_sm{}.
-spec pack   (#replace_sm{}) -> binary().



unpack(Stream) when is_binary(Stream) ->
    {MessageId, Stream_1}            = esmpp34raw_utils:get_var_c_octet_string(Stream, 65),
    << SourceAddrTon:     8/big-unsigned-integer,
       SourceAddrNpi:     8/big-unsigned-integer,
       Stream_2            /binary>> = Stream_1,
    {SourceAddr, Stream_3}           = esmpp34raw_utils:get_var_c_octet_string(Stream_2, 21),
    {ScheduleDeliveryTime, Stream_4} = esmpp34raw_utils:get_c_octet_string(Stream_3, [1, 17]),
    {ValidityPeriod, Stream_5}       = esmpp34raw_utils:get_c_octet_string(Stream_4, [1, 17]),
    << RegistereDelivery: 8/big-unsigned-integer,
       SmDefaultMsgId:    8/big-unsigned-integer,
       SmLength:          8/big-unsigned-integer,
       Stream_6            /binary >> = Stream_5,
    {ShortMessage, <<>>}              = esmpp34raw_utils:get_c_octet_string(Stream_6, [SmLength]),
    #replace_sm { message_id             = MessageId,
		  source_addr_ton        = SourceAddrTon,
		  source_addr_npi        = SourceAddrNpi,
		  source_addr            = SourceAddr,
		  schedule_delivery_time = ScheduleDeliveryTime,
		  validity_period        = ValidityPeriod,
		  registered_delivery    = RegistereDelivery,
		  sm_default_msg_id      = SmDefaultMsgId,
		  sm_length              = SmLength,
		  short_message          = ShortMessage}.



pack(#replace_sm{} = Body) ->
    MessageId            = esmpp34raw_utils:pack_var_c_octet_string(Body#replace_sm.message_id, 65),
    SourceAddrTon        = Body#replace_sm.source_addr_ton,
    SourceAddrNpi        = Body#replace_sm.source_addr_npi,
    SourceAddr           = esmpp34raw_utils:pack_var_c_octet_string(Body#replace_sm.source_addr, 21),
    ScheduleDeliveryTime = esmpp34raw_utils:pack_var_c_octet_string(Body#replace_sm.schedule_delivery_time, 17),
    ValidityPeriod       = esmpp34raw_utils:pack_var_c_octet_string(Body#replace_sm.validity_period, 17),
    RegisteredDelivery   = Body#replace_sm.registered_delivery,
    SmDefaultMsgId       = Body#replace_sm.sm_default_msg_id,
    SmLength             = Body#replace_sm.sm_length,
    ShortMessage         = case length(Body#replace_sm.short_message) /= SmLength of
			       true -> throw(bad_short_message_length);
			       _ -> list_to_binary(Body#replace_sm.short_message)
			   end,
    << MessageId            /binary,
       SourceAddrTon:      8/big-unsigned-integer,
       SourceAddrNpi:      8/big-unsigned-integer,
       SourceAddr           /binary,
       ScheduleDeliveryTime /binary,
       ValidityPeriod       /binary,
       RegisteredDelivery: 8/big-unsigned-integer,
       SmDefaultMsgId:     8/big-unsigned-integer,
       SmLength:           8/big-unsigned-integer,
       ShortMessage         /binary >>.

