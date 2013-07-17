-module(esmpp34raw_submit_sm).
-author("Morozov Alexander aka ~ArchimeD~").

-export([
	 unpack/1,
	 pack/1
	]).

-include("esmpp34raw_types.hrl").
-include("esmpp34raw_tags.hrl").
-include("esmpp34raw_utils.hrl").



-spec unpack (binary())     -> #submit_sm{}.
-spec pack   (#submit_sm{}) -> binary().



unpack(Stream) when is_binary(Stream) ->
    {ServiceType, Stream_1}               = esmpp34raw_utils:get_var_c_octet_string(Stream, 6),
    <<SourceAddrTon:        8/big-unsigned-integer,
      SourceAddrNpi:        8/big-unsigned-integer,
      Stream_2               /binary>> = Stream_1,
    {SourceAddr, Stream_3}                = esmpp34raw_utils:get_var_c_octet_string(Stream_2, 21),
    <<DestAddrTon:          8/big-unsigned-integer,
      DestAddrNpi:          8/big-unsigned-integer,
      Stream_4               /binary>> = Stream_3,
    {DestinationAddr, Stream_5}           = esmpp34raw_utils:get_var_c_octet_string(Stream_4, 21),
    <<EsmClass:             8/big-unsigned-integer,
      ProtocolId:           8/big-unsigned-integer,
      PriorityFlag:         8/big-unsigned-integer,
      Stream_6               /binary>> = Stream_5,
    {ScheduleDeliveryTime, Stream_7}      = esmpp34raw_utils:get_c_octet_string(Stream_6, [1, 17]),
    {ValidityPeriod, Stream_8}            = esmpp34raw_utils:get_c_octet_string(Stream_7, [1, 17]),
    <<RegisteredDelivery:   8/big-unsigned-integer,
      ReplaceIfPresentFlag: 8/big-unsigned-integer,
      DataCoding:           8/big-unsigned-integer,
      SmDefaultMsgId:       8/big-unsigned-integer,
      SmLength:             8/big-unsigned-integer,
      ShortMessage:  SmLength/binary,
      Stream_9               /binary>> = Stream_8,

    %% decode optional
    unpack_optional(#submit_sm{service_type            = ServiceType,
			       source_addr_ton         = SourceAddrTon,
			       source_addr_npi         = SourceAddrNpi,
			       source_addr             = SourceAddr,
			       dest_addr_ton           = DestAddrTon,
			       dest_addr_npi           = DestAddrNpi,
			       destination_addr        = DestinationAddr,
			       esm_class               = EsmClass,
			       protocol_id             = ProtocolId,
			       priority_flag           = PriorityFlag,
			       schedule_delivery_time  = ScheduleDeliveryTime,
			       validity_period         = ValidityPeriod,
			       registered_delivery     = RegisteredDelivery,
			       replace_if_present_flag = ReplaceIfPresentFlag,
			       data_coding             = DataCoding,
			       sm_default_msg_id       = SmDefaultMsgId,
			       sm_length               = SmLength,
			       short_message           = binary_to_list(ShortMessage)}, Stream_9).



pack(#submit_sm{} = Body) ->
    %% pack mandatory
    ServiceType          = esmpp34raw_utils:pack_var_c_octet_string(Body#submit_sm.service_type, 6),
    SourceAddrTon        = Body#submit_sm.source_addr_ton,
    SourceAddrNpi        = Body#submit_sm.source_addr_npi,
    SourceAddr           = esmpp34raw_utils:pack_var_c_octet_string(Body#submit_sm.source_addr, 21),
    DestAddrTon          = Body#submit_sm.dest_addr_ton,
    DestAddrNpi          = Body#submit_sm.dest_addr_npi,
    DestinationAddr      = esmpp34raw_utils:pack_var_c_octet_string(Body#submit_sm.destination_addr, 21),
    EsmClass             = Body#submit_sm.esm_class,
    ProtocolId           = Body#submit_sm.protocol_id,
    PriorityFlag         = Body#submit_sm.priority_flag,
    ScheduleDeliveryTime = esmpp34raw_utils:pack_var_c_octet_string(Body#submit_sm.schedule_delivery_time, 17),
    ValidityPeriod       = esmpp34raw_utils:pack_var_c_octet_string(Body#submit_sm.validity_period, 17),
    RegisteredDelivery   = Body#submit_sm.registered_delivery,
    ReplaceIfPresentFlag = Body#submit_sm.replace_if_present_flag,
    DataCoding           = Body#submit_sm.data_coding,
    SmDefaultMsgId       = Body#submit_sm.sm_default_msg_id,
    SmLength             = Body#submit_sm.sm_length,
    ShortMessage = case	length(Body#submit_sm.short_message) /= SmLength of
		       true -> throw(bad_short_message_length);
		       _ -> list_to_binary(Body#submit_sm.short_message)
		   end,

    %% pack optional

    UserMessageReference = ?pack_optional(Body#submit_sm.user_message_reference, ?user_message_reference),
    SourcePort           = ?pack_optional(Body#submit_sm.source_port,            ?source_port),
    SourceAddrSubunit    = ?pack_optional(Body#submit_sm.source_addr_subunit,    ?source_addr_subunit),
    DestinationPort      = ?pack_optional(Body#submit_sm.destination_port,       ?destination_port),
    DestAddrSubunit      = ?pack_optional(Body#submit_sm.dest_addr_subunit,      ?dest_addr_subunit),
    SarMsgRefNum         = ?pack_optional(Body#submit_sm.sar_msg_ref_num,        ?sar_msg_ref_num),
    SarTotalSegments     = ?pack_optional(Body#submit_sm.sar_total_segments,     ?sar_total_segments),
    SarSegmentSeqnum     = ?pack_optional(Body#submit_sm.sar_segment_seqnum,     ?sar_segment_seqnum),
    MoreMessagesToSend   = ?pack_optional(Body#submit_sm.more_messages_to_send,  ?more_messages_to_send),
    PayloadType          = ?pack_optional(Body#submit_sm.payload_type,           ?payload_type),
    MessagePayload       = ?pack_optional(Body#submit_sm.message_payload,        ?message_payload),
    PrivacyIndicator     = ?pack_optional(Body#submit_sm.privacy_indicator,      ?privacy_indicator),
    CallbackNum          = ?pack_optional(Body#submit_sm.callback_num,           ?callback_num),
    CallbackNumPresInd   = ?pack_optional(Body#submit_sm.callback_num_pres_ind,  ?callback_num_pres_ind),
    CallbackNumAtag      = ?pack_optional(Body#submit_sm.callback_num_atag,      ?callback_num_atag),
    SourceSubaddress     = ?pack_optional(Body#submit_sm.source_subaddress,      ?source_subaddress),
    DestSubaddress       = ?pack_optional(Body#submit_sm.dest_subaddress,        ?dest_subaddress),
    UserResponseCode     = ?pack_optional(Body#submit_sm.user_response_code,     ?user_response_code),
    DisplayTime          = ?pack_optional(Body#submit_sm.display_time,           ?display_time),
    SmsSignal            = ?pack_optional(Body#submit_sm.sms_signal,             ?sms_signal),
    MsValidity           = ?pack_optional(Body#submit_sm.ms_validity,            ?ms_validity),
    MsMsgWaitFacilities  = ?pack_optional(Body#submit_sm.ms_msg_wait_facilities, ?ms_msg_wait_facilities),
    NumberOfMessages     = ?pack_optional(Body#submit_sm.number_of_messages,     ?number_of_messages),
    AlertOnMsgDelivery   = ?pack_optional(Body#submit_sm.alert_on_msg_delivery,  ?alert_on_msg_delivery),
    LanguageIndicator    = ?pack_optional(Body#submit_sm.language_indicator,     ?language_indicator),
    ItsReplyType         = ?pack_optional(Body#submit_sm.its_reply_type,         ?its_reply_type),
    ItsSessionInfo       = ?pack_optional(Body#submit_sm.its_session_info,       ?its_session_info),
    UssdServiceOp        = ?pack_optional(Body#submit_sm.ussd_service_op,        ?ussd_service_op),

    << ServiceType            /binary,
       SourceAddrTon:        8/big-unsigned-integer,
       SourceAddrNpi:        8/big-unsigned-integer,
       SourceAddr             /binary,
       DestAddrTon:          8/big-unsigned-integer,
       DestAddrNpi:          8/big-unsigned-integer,
       DestinationAddr        /binary,
       EsmClass:             8/big-unsigned-integer,
       ProtocolId:           8/big-unsigned-integer,
       PriorityFlag:         8/big-unsigned-integer,
       ScheduleDeliveryTime   /binary,
       ValidityPeriod         /binary,
       RegisteredDelivery:   8/big-unsigned-integer,
       ReplaceIfPresentFlag: 8/big-unsigned-integer,
       DataCoding:           8/big-unsigned-integer,
       SmDefaultMsgId:       8/big-unsigned-integer,
       SmLength:             8/big-unsigned-integer,
       ShortMessage           /binary,

       UserMessageReference   /binary,
       SourcePort             /binary,
       SourceAddrSubunit      /binary,
       DestinationPort        /binary,
       DestAddrSubunit        /binary,
       SarMsgRefNum           /binary,
       SarTotalSegments       /binary,
       SarSegmentSeqnum       /binary,
       MoreMessagesToSend     /binary,
       PayloadType            /binary,
       MessagePayload         /binary,
       PrivacyIndicator       /binary,
       CallbackNum            /binary,
       CallbackNumPresInd     /binary,
       CallbackNumAtag        /binary,
       SourceSubaddress       /binary,
       DestSubaddress         /binary,
       UserResponseCode       /binary,
       DisplayTime            /binary,
       SmsSignal              /binary,
       MsValidity             /binary,
       MsMsgWaitFacilities    /binary,
       NumberOfMessages       /binary,
       AlertOnMsgDelivery     /binary,
       LanguageIndicator      /binary,
       ItsReplyType           /binary,
       ItsSessionInfo         /binary,
       UssdServiceOp          /binary >>.



?unpack_optional (submit_sm);
?define_optional (submit_sm, user_message_reference, ?user_message_reference);
?define_optional (submit_sm, source_port,            ?source_port);
?define_optional (submit_sm, source_addr_subunit,    ?source_addr_subunit);
?define_optional (submit_sm, destination_port,       ?destination_port);
?define_optional (submit_sm, dest_addr_subunit,      ?dest_addr_subunit);
?define_optional (submit_sm, sar_msg_ref_num,        ?sar_msg_ref_num);
?define_optional (submit_sm, sar_total_segments,     ?sar_total_segments);
?define_optional (submit_sm, sar_segment_seqnum,     ?sar_segment_seqnum);
?define_optional (submit_sm, more_messages_to_send,  ?more_messages_to_send);
?define_optional (submit_sm, payload_type,           ?payload_type);
?define_optional (submit_sm, message_payload,        ?message_payload);
?define_optional (submit_sm, privacy_indicator,      ?privacy_indicator);
?define_optional (submit_sm, callback_num,           ?callback_num);
?define_optional (submit_sm, callback_num_pres_ind,  ?callback_num_pres_ind);
?define_optional (submit_sm, callback_num_atag,      ?callback_num_atag);
?define_optional (submit_sm, source_subaddress,      ?source_subaddress);
?define_optional (submit_sm, dest_subaddress,        ?dest_subaddress);
?define_optional (submit_sm, user_response_code,     ?user_response_code);
?define_optional (submit_sm, display_time,           ?display_time);
?define_optional (submit_sm, sms_signal,             ?sms_signal);
?define_optional (submit_sm, ms_validity,            ?ms_validity);
?define_optional (submit_sm, ms_msg_wait_facilities, ?ms_msg_wait_facilities);
?define_optional (submit_sm, number_of_messages,     ?number_of_messages);
?define_optional (submit_sm, alert_on_msg_delivery,  ?alert_on_msg_delivery);
?define_optional (submit_sm, language_indicator,     ?language_indicator);
?define_optional (submit_sm, its_reply_type,         ?its_reply_type);
?define_optional (submit_sm, its_session_info,       ?its_session_info);
?define_optional (submit_sm, ussd_service_op,        ?ussd_service_op);
?optional_done   (submit_sm).

