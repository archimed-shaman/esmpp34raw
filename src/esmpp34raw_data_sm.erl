-module(esmpp34raw_data_sm).
-author("Morozov Alexander aka ~ArchimeD~").

-export([
         unpack/1,
         pack/1
        ]).

-include("esmpp34raw_types.hrl").
-include("esmpp34raw_tags.hrl").
-include("esmpp34raw_utils.hrl").



-spec unpack (binary())      -> #data_sm{}.
-spec pack   (#data_sm{})    -> binary().



unpack(Stream) when is_binary(Stream) ->
    {ServiceType, Stream_1}               = esmpp34raw_utils:get_var_c_octet_string(Stream, 6),
    <<SourceAddrTon:      8/big-unsigned-integer,
      SourceAddrNpi:      8/big-unsigned-integer,
      Stream_2               /binary>> = Stream_1,
    {SourceAddr, Stream_3}                = esmpp34raw_utils:get_var_c_octet_string(Stream_2, 21),
    <<DestAddrTon:        8/big-unsigned-integer,
      DestAddrNpi:        8/big-unsigned-integer,
      Stream_4               /binary>> = Stream_3,
    {DestinationAddr, Stream_5}           = esmpp34raw_utils:get_var_c_octet_string(Stream_4, 21),
    <<EsmClass:           8/big-unsigned-integer,
      RegisteredDelivery: 8/big-unsigned-integer,
      DataCoding:         8/big-unsigned-integer,
      Stream_6             /binary>> = Stream_5,
    unpack_optional(#data_sm{service_type        = ServiceType,
                             source_addr_ton     = SourceAddrTon,
                             source_addr_npi     = SourceAddrNpi,
                             source_addr         = SourceAddr,
                             dest_addr_ton       = DestAddrTon,
                             dest_addr_npi       = DestAddrNpi,
                             destination_addr    = DestinationAddr,
                             esm_class           = EsmClass,
                             registered_delivery = RegisteredDelivery,
                             data_coding         = DataCoding }, Stream_6).



pack(#data_sm{} = Body) ->
    %% pack mandatory
    ServiceType          = esmpp34raw_utils:pack_var_c_octet_string(Body#data_sm.service_type, 6),
    SourceAddrTon        = Body#data_sm.source_addr_ton,
    SourceAddrNpi        = Body#data_sm.source_addr_npi,
    SourceAddr           = esmpp34raw_utils:pack_var_c_octet_string(Body#data_sm.source_addr, 21),
    DestAddrTon          = Body#data_sm.dest_addr_ton,
    DestAddrNpi          = Body#data_sm.dest_addr_npi,
    DestinationAddr      = esmpp34raw_utils:pack_var_c_octet_string(Body#data_sm.destination_addr, 21),
    EsmClass             = Body#data_sm.esm_class,
    RegisteredDelivery   = Body#data_sm.registered_delivery,
    DataCoding           = Body#data_sm.data_coding,

    %% pack optional
    SourcePort           = ?pack_optional(Body#data_sm.source_port,            ?source_port),
    SourceAddrSubunit    = ?pack_optional(Body#data_sm.source_addr_subunit,    ?source_addr_subunit),
    SourceNetworkType    = ?pack_optional(Body#data_sm.source_network_type,    ?source_network_type),
    SourceBearerType     = ?pack_optional(Body#data_sm.source_bearer_type,     ?source_bearer_type),
    SourceTelematicsId   = ?pack_optional(Body#data_sm.source_telematics_id,   ?source_telematics_id),
    DestinationPort      = ?pack_optional(Body#data_sm.destination_port,       ?destination_port),
    DestAddrSubunit      = ?pack_optional(Body#data_sm.dest_addr_subunit,      ?dest_addr_subunit),
    DestNetworkType      = ?pack_optional(Body#data_sm.dest_network_type,      ?dest_network_type),
    DestBearerType       = ?pack_optional(Body#data_sm.dest_bearer_type,       ?dest_bearer_type),
    DestTelematicsId     = ?pack_optional(Body#data_sm.dest_telematics_id,     ?dest_telematics_id),
    SarMsgRefNum         = ?pack_optional(Body#data_sm.sar_msg_ref_num,        ?sar_msg_ref_num),
    SarTotalSegments     = ?pack_optional(Body#data_sm.sar_total_segments,     ?sar_total_segments),
    SarSegmentSeqnum     = ?pack_optional(Body#data_sm.sar_segment_seqnum,     ?sar_segment_seqnum),
    MoreMessagesToSend   = ?pack_optional(Body#data_sm.more_messages_to_send,  ?more_messages_to_send),
    QosTimeToLive        = ?pack_optional(Body#data_sm.qos_time_to_live,       ?qos_time_to_live),
    PayloadType          = ?pack_optional(Body#data_sm.payload_type,           ?payload_type),
    MessagePayload       = ?pack_optional(Body#data_sm.message_payload,        ?message_payload),
    SetDpf               = ?pack_optional(Body#data_sm.set_dpf,                ?set_dpf),
    ReceiptedMessageId   = ?pack_optional(Body#data_sm.receipted_message_id,   ?receipted_message_id),
    MessageState         = ?pack_optional(Body#data_sm.message_state,          ?message_state),
    NetworkErrorCode     = ?pack_optional(Body#data_sm.network_error_code,     ?network_error_code),
    UserMessageReference = ?pack_optional(Body#data_sm.user_message_reference, ?user_message_reference),
    PrivacyIndicator     = ?pack_optional(Body#data_sm.privacy_indicator,      ?privacy_indicator),
    CallbackNum          = ?pack_optional(Body#data_sm.callback_num,           ?callback_num),
    CallbackNumPresInd   = ?pack_optional(Body#data_sm.callback_num_pres_ind,  ?callback_num_pres_ind),
    CallbackNumAtag      = ?pack_optional(Body#data_sm.callback_num_atag,      ?callback_num_atag),
    SourceSubaddress     = ?pack_optional(Body#data_sm.source_subaddress,      ?source_subaddress),
    DestSubaddress       = ?pack_optional(Body#data_sm.dest_subaddress,        ?dest_subaddress),
    UserResponseCode     = ?pack_optional(Body#data_sm.user_response_code,     ?user_response_code),
    DisplayTime          = ?pack_optional(Body#data_sm.display_time,           ?display_time),
    SmsSignal            = ?pack_optional(Body#data_sm.sms_signal,             ?sms_signal),
    MsValidity           = ?pack_optional(Body#data_sm.ms_validity,            ?ms_validity),
    MsMsgWaitFacilities  = ?pack_optional(Body#data_sm.ms_msg_wait_facilities, ?ms_msg_wait_facilities),
    NumberOfMessages     = ?pack_optional(Body#data_sm.number_of_messages,     ?number_of_messages),
    AlertOnMsgDelivery   = ?pack_optional(Body#data_sm.alert_on_msg_delivery,  ?alert_on_msg_delivery),
    LanguageIndicator    = ?pack_optional(Body#data_sm.language_indicator,     ?language_indicator),
    ItsReplyType         = ?pack_optional(Body#data_sm.its_reply_type,         ?its_reply_type),
    ItsSessionInfo       = ?pack_optional(Body#data_sm.its_session_info,       ?its_session_info),

    << ServiceType          /binary,
       SourceAddrTon:      8/big-unsigned-integer,
       SourceAddrNpi:      8/big-unsigned-integer,
       SourceAddr           /binary,
       DestAddrTon:        8/big-unsigned-integer,
       DestAddrNpi:        8/big-unsigned-integer,
       DestinationAddr      /binary,
       EsmClass:           8/big-unsigned-integer,
       RegisteredDelivery: 8/big-unsigned-integer,
       DataCoding:         8/big-unsigned-integer,

       SourcePort           /binary,
       SourceAddrSubunit    /binary,
       SourceNetworkType    /binary,
       SourceBearerType     /binary,
       SourceTelematicsId   /binary,
       DestinationPort      /binary,
       DestAddrSubunit      /binary,
       DestNetworkType      /binary,
       DestBearerType       /binary,
       DestTelematicsId     /binary,
       SarMsgRefNum         /binary,
       SarTotalSegments     /binary,
       SarSegmentSeqnum     /binary,
       MoreMessagesToSend   /binary,
       QosTimeToLive        /binary,
       PayloadType          /binary,
       MessagePayload       /binary,
       SetDpf               /binary,
       ReceiptedMessageId   /binary,
       MessageState         /binary,
       NetworkErrorCode     /binary,
       UserMessageReference /binary,
       PrivacyIndicator     /binary,
       CallbackNum          /binary,
       CallbackNumPresInd   /binary,
       CallbackNumAtag      /binary,
       SourceSubaddress     /binary,
       DestSubaddress       /binary,
       UserResponseCode     /binary,
       DisplayTime          /binary,
       SmsSignal            /binary,
       MsValidity           /binary,
       MsMsgWaitFacilities  /binary,
       NumberOfMessages     /binary,
       AlertOnMsgDelivery   /binary,
       LanguageIndicator    /binary,
       ItsReplyType         /binary,
       ItsSessionInfo       /binary>>.



%% decode optional
?unpack_optional (data_sm);
?define_optional (data_sm, source_port,            ?source_port);
?define_optional (data_sm, source_addr_subunit,    ?source_addr_subunit);
?define_optional (data_sm, source_network_type,    ?source_network_type);
?define_optional (data_sm, source_bearer_type,     ?source_bearer_type);
?define_optional (data_sm, source_telematics_id,   ?source_telematics_id);
?define_optional (data_sm, destination_port,       ?destination_port);
?define_optional (data_sm, dest_addr_subunit,      ?dest_addr_subunit);
?define_optional (data_sm, dest_network_type,      ?dest_network_type);
?define_optional (data_sm, dest_bearer_type,       ?dest_bearer_type);
?define_optional (data_sm, dest_telematics_id,     ?dest_telematics_id);
?define_optional (data_sm, sar_msg_ref_num,        ?sar_msg_ref_num);
?define_optional (data_sm, sar_total_segments,     ?sar_total_segments);
?define_optional (data_sm, sar_segment_seqnum,     ?sar_segment_seqnum);
?define_optional (data_sm, more_messages_to_send,  ?more_messages_to_send);
?define_optional (data_sm, qos_time_to_live,       ?qos_time_to_live);
?define_optional (data_sm, payload_type,           ?payload_type);
?define_optional (data_sm, message_payload,        ?message_payload);
?define_optional (data_sm, set_dpf,                ?set_dpf);
?define_optional (data_sm, receipted_message_id,   ?receipted_message_id);
?define_optional (data_sm, message_state,          ?message_state);
?define_optional (data_sm, network_error_code,     ?network_error_code);
?define_optional (data_sm, user_message_reference, ?user_message_reference);
?define_optional (data_sm, privacy_indicator,      ?privacy_indicator);
?define_optional (data_sm, callback_num,           ?callback_num);
?define_optional (data_sm, callback_num_pres_ind,  ?callback_num_pres_ind);
?define_optional (data_sm, callback_num_atag,      ?callback_num_atag);
?define_optional (data_sm, source_subaddress,      ?source_subaddress);
?define_optional (data_sm, dest_subaddress,        ?dest_subaddress);
?define_optional (data_sm, user_response_code,     ?user_response_code);
?define_optional (data_sm, display_time,           ?display_time);
?define_optional (data_sm, sms_signal,             ?sms_signal);
?define_optional (data_sm, ms_validity,            ?ms_validity);
?define_optional (data_sm, ms_msg_wait_facilities, ?ms_msg_wait_facilities);
?define_optional (data_sm, number_of_messages,     ?number_of_messages);
?define_optional (data_sm, alert_on_msg_delivery,  ?alert_on_msg_delivery);
?define_optional (data_sm, language_indicator,     ?language_indicator);
?define_optional (data_sm, its_reply_type,         ?its_reply_type);
?define_optional (data_sm, its_session_info,       ?its_session_info);
?optional_done   (data_sm).
