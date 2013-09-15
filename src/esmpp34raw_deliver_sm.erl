-module(esmpp34raw_deliver_sm).
-author("Morozov Alexander aka ~ArchimeD~").

-export([
         unpack/1,
         pack/1
        ]).

-include("esmpp34raw_types.hrl").
-include("esmpp34raw_tags.hrl").
-include("esmpp34raw_utils.hrl").



-spec unpack (binary())      -> #deliver_sm{}.
-spec pack   (#deliver_sm{}) -> binary().



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
    {_ScheduleDeliveryTime, Stream_7}     = esmpp34raw_utils:get_c_octet_string(Stream_6, [1]),
    {_ValidityPeriod, Stream_8}           = esmpp34raw_utils:get_c_octet_string(Stream_7, [1]),
    <<RegisteredDelivery:   8/big-unsigned-integer,
      0:                    8/big-unsigned-integer, %% replace_if_present_flag
      DataCoding:           8/big-unsigned-integer,
      0:                    8/big-unsigned-integer, %% sm_default_msg_id
      SmLength:             8/big-unsigned-integer,
      ShortMessage:  SmLength/binary,
      Stream_9               /binary>> = Stream_8,

    %% decode optional
    unpack_optional(#deliver_sm{service_type            = ServiceType,
                                source_addr_ton         = SourceAddrTon,
                                source_addr_npi         = SourceAddrNpi,
                                source_addr             = SourceAddr,
                                dest_addr_ton           = DestAddrTon,
                                dest_addr_npi           = DestAddrNpi,
                                destination_addr        = DestinationAddr,
                                esm_class               = EsmClass,
                                protocol_id             = ProtocolId,
                                priority_flag           = PriorityFlag,
                                registered_delivery     = RegisteredDelivery,
                                data_coding             = DataCoding,
                                sm_length               = SmLength,
                                short_message           = binary_to_list(ShortMessage)}, Stream_9).



pack(#deliver_sm{} = Body) ->
    %% pack mandatory
    ServiceType          = esmpp34raw_utils:pack_var_c_octet_string(Body#deliver_sm.service_type, 6),
    SourceAddrTon        = Body#deliver_sm.source_addr_ton,
    SourceAddrNpi        = Body#deliver_sm.source_addr_npi,
    SourceAddr           = esmpp34raw_utils:pack_var_c_octet_string(Body#deliver_sm.source_addr, 21),
    DestAddrTon          = Body#deliver_sm.dest_addr_ton,
    DestAddrNpi          = Body#deliver_sm.dest_addr_npi,
    DestinationAddr      = esmpp34raw_utils:pack_var_c_octet_string(Body#deliver_sm.destination_addr, 21),
    EsmClass             = Body#deliver_sm.esm_class,
    ProtocolId           = Body#deliver_sm.protocol_id,
    PriorityFlag         = Body#deliver_sm.priority_flag,
    ScheduleDeliveryTime = esmpp34raw_utils:pack_var_c_octet_string(Body#deliver_sm.schedule_delivery_time, 1),
    ValidityPeriod       = esmpp34raw_utils:pack_var_c_octet_string(Body#deliver_sm.validity_period, 1),
    RegisteredDelivery   = Body#deliver_sm.registered_delivery,
    ReplaceIfPresentFlag = Body#deliver_sm.replace_if_present_flag,
    DataCoding           = Body#deliver_sm.data_coding,
    SmDefaultMsgId       = Body#deliver_sm.sm_default_msg_id,
    SmLength             = Body#deliver_sm.sm_length,
    ShortMessage = case length(Body#deliver_sm.short_message) /= SmLength of
                       true -> throw(bad_short_message_length);
                       _ -> list_to_binary(Body#deliver_sm.short_message)
                   end,

    %% pack optional

    UserMessageReference = ?pack_optional(Body#deliver_sm.user_message_reference, ?user_message_reference),
    SourcePort           = ?pack_optional(Body#deliver_sm.source_port,            ?source_port),
    DestinationPort      = ?pack_optional(Body#deliver_sm.destination_port,       ?destination_port),
    SarMsgRefNum         = ?pack_optional(Body#deliver_sm.sar_msg_ref_num,        ?sar_msg_ref_num),
    SarTotalSegments     = ?pack_optional(Body#deliver_sm.sar_total_segments,     ?sar_total_segments),
    SarSegmentSeqnum     = ?pack_optional(Body#deliver_sm.sar_segment_seqnum,     ?sar_segment_seqnum),
    UserResponseCode     = ?pack_optional(Body#deliver_sm.user_response_code,     ?user_response_code),
    PrivacyIndicator     = ?pack_optional(Body#deliver_sm.privacy_indicator,      ?privacy_indicator),
    PayloadType          = ?pack_optional(Body#deliver_sm.payload_type,           ?payload_type),
    MessagePayload       = ?pack_optional(Body#deliver_sm.message_payload,        ?message_payload),
    CallbackNum          = ?pack_optional(Body#deliver_sm.callback_num,           ?callback_num),
    SourceSubaddress     = ?pack_optional(Body#deliver_sm.source_subaddress,      ?source_subaddress),
    DestSubaddress       = ?pack_optional(Body#deliver_sm.dest_subaddress,        ?dest_subaddress),
    LanguageIndicator    = ?pack_optional(Body#deliver_sm.language_indicator,     ?language_indicator),
    ItsSessionInfo       = ?pack_optional(Body#deliver_sm.its_session_info,       ?its_session_info),
    NetworkErrorCode     = ?pack_optional(Body#deliver_sm.network_error_code,     ?network_error_code),
    MessageState         = ?pack_optional(Body#deliver_sm.message_state,          ?message_state),
    ReceiptedMessageId   = ?pack_optional(Body#deliver_sm.receipted_message_id,   ?receipted_message_id),

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
       DestinationPort        /binary,
       SarMsgRefNum           /binary,
       SarTotalSegments       /binary,
       SarSegmentSeqnum       /binary,
       UserResponseCode       /binary,
       PrivacyIndicator       /binary,
       PayloadType            /binary,
       MessagePayload         /binary,
       CallbackNum            /binary,
       SourceSubaddress       /binary,
       DestSubaddress         /binary,
       LanguageIndicator      /binary,
       ItsSessionInfo         /binary,
       NetworkErrorCode       /binary,
       MessageState           /binary,
       ReceiptedMessageId     /binary>>.



?unpack_optional (deliver_sm);
?define_optional (deliver_sm, user_message_reference, ?user_message_reference);
?define_optional (deliver_sm, source_port,            ?source_port);
?define_optional (deliver_sm, destination_port,       ?destination_port);
?define_optional (deliver_sm, sar_msg_ref_num,        ?sar_msg_ref_num);
?define_optional (deliver_sm, sar_total_segments,     ?sar_total_segments);
?define_optional (deliver_sm, sar_segment_seqnum,     ?sar_segment_seqnum);
?define_optional (deliver_sm, user_response_code,     ?user_response_code);
?define_optional (deliver_sm, privacy_indicator,      ?privacy_indicator);
?define_optional (deliver_sm, payload_type,           ?payload_type);
?define_optional (deliver_sm, message_payload,        ?message_payload);
?define_optional (deliver_sm, callback_num,           ?callback_num);
?define_optional (deliver_sm, source_subaddress,      ?source_subaddress);
?define_optional (deliver_sm, dest_subaddress,        ?dest_subaddress);
?define_optional (deliver_sm, language_indicator,     ?language_indicator);
?define_optional (deliver_sm, its_session_info,       ?its_session_info);
?define_optional (deliver_sm, network_error_code,     ?network_error_code);
?define_optional (deliver_sm, message_state,          ?message_state);
?define_optional (deliver_sm, receipted_message_id,   ?receipted_message_id);
?optional_done   (deliver_sm).
