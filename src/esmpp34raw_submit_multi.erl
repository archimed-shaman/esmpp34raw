-module(esmpp34raw_submit_multi).
-author("Morozov Alexander aka ~ArchimeD~").

-export([
         unpack/1,
         pack/1
        ]).

-include("esmpp34raw_types.hrl").
-include("esmpp34raw_tags.hrl").
-include("esmpp34raw_utils.hrl").



-spec unpack                   (binary())                    -> #submit_multi{}.
-spec pack                     (#submit_multi{})             -> binary().
-spec get_dest_addresses_list  (binary(), integer(), list()) -> {list(), binary()}.
-spec pack_dest_addresses_list (binary(), integer(), list()) -> {integer(), binary()}.


unpack(Stream) when is_binary(Stream) ->
    {ServiceType, Stream_1}          = esmpp34raw_utils:get_var_c_octet_string(Stream, 6),
    <<SourceAddrTon:        8/big-unsigned-integer,
      SourceAddrNpi:        8/big-unsigned-integer,
      Stream_2               /binary>> = Stream_1,
    {SourceAddr, Stream_3}           = esmpp34raw_utils:get_var_c_octet_string(Stream_2, 21),
    <<NumberOfDests:        8/big-unsigned-integer,
      Stream_4               /binary>> = Stream_3,
    {DestAddress, Stream_5}          = get_dest_addresses_list(Stream_4, NumberOfDests, []),
    <<EsmClass:             8/big-unsigned-integer,
      ProtocolId:           8/big-unsigned-integer,
      PriorityFlag:         8/big-unsigned-integer,
      Stream_6               /binary>> = Stream_5,
    {ScheduleDeliveryTime, Stream_7} = esmpp34raw_utils:get_c_octet_string(Stream_6, [1, 17]),
    {ValidityPeriod, Stream_8}       = esmpp34raw_utils:get_c_octet_string(Stream_7, [1, 17]),
    <<RegisteredDelivery:   8/big-unsigned-integer,
      ReplaceIfPresentFlag: 8/big-unsigned-integer,
      DataCoding:           8/big-unsigned-integer,
      SmDefaultMsgId:       8/big-unsigned-integer,
      SmLength:             8/big-unsigned-integer,
      ShortMessage:  SmLength/binary,
      Stream_9               /binary>> = Stream_8,

    %% decode optional
    unpack_optional(#submit_multi{service_type            = ServiceType,
                                  source_addr_ton         = SourceAddrTon,
                                  source_addr_npi         = SourceAddrNpi,
                                  source_addr             = SourceAddr,
                                  number_of_dests         = NumberOfDests,
                                  dest_address            = DestAddress,
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



pack(#submit_multi{} = Body) ->
    %% pack mandatory
    ServiceType          = esmpp34raw_utils:pack_var_c_octet_string(Body#submit_multi.service_type, 6),
    SourceAddrTon        = Body#submit_multi.source_addr_ton,
    SourceAddrNpi        = Body#submit_multi.source_addr_npi,
    SourceAddr           = esmpp34raw_utils:pack_var_c_octet_string(Body#submit_multi.source_addr, 21),
    NumberOfDests        = Body#submit_multi.number_of_dests,
    %% FIXME: test this firstly
    PackedAddresses      = pack_dest_addresses_list(<<>>, 0, Body#submit_multi.dest_address),
    DestAddress          = case PackedAddresses of
                               {NumberOfDests, Addr} -> Addr;
                               _ -> throw(bad_number_of_dests)
                           end,
    EsmClass             = Body#submit_multi.esm_class,
    ProtocolId           = Body#submit_multi.protocol_id,
    PriorityFlag         = Body#submit_multi.priority_flag,
    ScheduleDeliveryTime = esmpp34raw_utils:pack_var_c_octet_string(Body#submit_multi.schedule_delivery_time, 17),
    ValidityPeriod       = esmpp34raw_utils:pack_var_c_octet_string(Body#submit_multi.validity_period, 17),
    RegisteredDelivery   = Body#submit_multi.registered_delivery,
    ReplaceIfPresentFlag = Body#submit_multi.replace_if_present_flag,
    DataCoding           = Body#submit_multi.data_coding,
    SmDefaultMsgId       = Body#submit_multi.sm_default_msg_id,
    SmLength             = Body#submit_multi.sm_length,
    ShortMessage         = case length(Body#submit_multi.short_message) /= SmLength of
                               true -> throw(bad_short_message_length);
                               _ -> list_to_binary(Body#submit_multi.short_message)
                           end,

    %% pack optional

    UserMessageReference = ?pack_optional(Body#submit_multi.user_message_reference, ?user_message_reference),
    SourcePort           = ?pack_optional(Body#submit_multi.source_port,            ?source_port),
    SourceAddrSubunit    = ?pack_optional(Body#submit_multi.source_addr_subunit,    ?source_addr_subunit),
    DestinationPort      = ?pack_optional(Body#submit_multi.destination_port,       ?destination_port),
    DestAddrSubunit      = ?pack_optional(Body#submit_multi.dest_addr_subunit,      ?dest_addr_subunit),
    SarMsgRefNum         = ?pack_optional(Body#submit_multi.sar_msg_ref_num,        ?sar_msg_ref_num),
    SarTotalSegments     = ?pack_optional(Body#submit_multi.sar_total_segments,     ?sar_total_segments),
    SarSegmentSeqnum     = ?pack_optional(Body#submit_multi.sar_segment_seqnum,     ?sar_segment_seqnum),
    PayloadType          = ?pack_optional(Body#submit_multi.payload_type,           ?payload_type),
    MessagePayload       = ?pack_optional(Body#submit_multi.message_payload,        ?message_payload),
    PrivacyIndicator     = ?pack_optional(Body#submit_multi.privacy_indicator,      ?privacy_indicator),
    CallbackNum          = ?pack_optional(Body#submit_multi.callback_num,           ?callback_num),
    CallbackNumPresInd   = ?pack_optional(Body#submit_multi.callback_num_pres_ind,  ?callback_num_pres_ind),
    CallbackNumAtag      = ?pack_optional(Body#submit_multi.callback_num_atag,      ?callback_num_atag),
    SourceSubaddress     = ?pack_optional(Body#submit_multi.source_subaddress,      ?source_subaddress),
    DestSubaddress       = ?pack_optional(Body#submit_multi.dest_subaddress,        ?dest_subaddress),
    DisplayTime          = ?pack_optional(Body#submit_multi.display_time,           ?display_time),
    SmsSignal            = ?pack_optional(Body#submit_multi.sms_signal,             ?sms_signal),
    MsValidity           = ?pack_optional(Body#submit_multi.ms_validity,            ?ms_validity),
    MsMsgWaitFacilities  = ?pack_optional(Body#submit_multi.ms_msg_wait_facilities, ?ms_msg_wait_facilities),
    AlertOnMsgDelivery   = ?pack_optional(Body#submit_multi.alert_on_msg_delivery,  ?alert_on_msg_delivery),
    LanguageIndicator    = ?pack_optional(Body#submit_multi.language_indicator,     ?language_indicator),

    << ServiceType            /binary,
       SourceAddrTon:        8/big-unsigned-integer,
       SourceAddrNpi:        8/big-unsigned-integer,
       SourceAddr             /binary,
       NumberOfDests:        8/big-unsigned-integer,
       DestAddress            /binary,
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
       PayloadType            /binary,
       MessagePayload         /binary,
       PrivacyIndicator       /binary,
       CallbackNum            /binary,
       CallbackNumPresInd     /binary,
       CallbackNumAtag        /binary,
       SourceSubaddress       /binary,
       DestSubaddress         /binary,
       DisplayTime            /binary,
       SmsSignal              /binary,
       MsValidity             /binary,
       MsMsgWaitFacilities    /binary,
       AlertOnMsgDelivery     /binary,
       LanguageIndicator      /binary>>.



?unpack_optional (submit_multi);
?define_optional (submit_multi, user_message_reference, ?user_message_reference);
?define_optional (submit_multi, source_port,            ?source_port);
?define_optional (submit_multi, source_addr_subunit,    ?source_addr_subunit);
?define_optional (submit_multi, destination_port,       ?destination_port);
?define_optional (submit_multi, dest_addr_subunit,      ?dest_addr_subunit);
?define_optional (submit_multi, sar_msg_ref_num,        ?sar_msg_ref_num);
?define_optional (submit_multi, sar_total_segments,     ?sar_total_segments);
?define_optional (submit_multi, sar_segment_seqnum,     ?sar_segment_seqnum);
?define_optional (submit_multi, payload_type,           ?payload_type);
?define_optional (submit_multi, message_payload,        ?message_payload);
?define_optional (submit_multi, privacy_indicator,      ?privacy_indicator);
?define_optional (submit_multi, callback_num,           ?callback_num);
?define_optional (submit_multi, callback_num_pres_ind,  ?callback_num_pres_ind);
?define_optional (submit_multi, callback_num_atag,      ?callback_num_atag);
?define_optional (submit_multi, source_subaddress,      ?source_subaddress);
?define_optional (submit_multi, dest_subaddress,        ?dest_subaddress);
?define_optional (submit_multi, display_time,           ?display_time);
?define_optional (submit_multi, sms_signal,             ?sms_signal);
?define_optional (submit_multi, ms_validity,            ?ms_validity);
?define_optional (submit_multi, ms_msg_wait_facilities, ?ms_msg_wait_facilities);
?define_optional (submit_multi, alert_on_msg_delivery,  ?alert_on_msg_delivery);
?define_optional (submit_multi, language_indicator,     ?language_indicator);
?optional_done   (submit_multi).



get_dest_addresses_list(Stream, 0, Accumulator) ->
    {lists:reverse(Accumulator), Stream};

%% 5.2.25 dest_flag, 1 - SME Address
get_dest_addresses_list(<< 1:           8/big-unsigned-integer,
                           DestAddrTon: 8/big-unsigned-integer,
                           DestAddrNpi: 8/big-unsigned-integer,
                           Stream        /binary>>, NumberOfDests, Accumulator) ->
    {DestinationAddr, Tail} = esmpp34raw_utils:get_var_c_octet_string(Stream, 21),
    get_dest_addresses_list(Tail, NumberOfDests - 1, [{DestAddrTon, DestAddrNpi, DestinationAddr} | Accumulator]);

%% 5.2.25 dest_flag, 2 - Distribution List Name
get_dest_addresses_list(<< 2:    8/big-unsigned-integer,
                           Stream /binary>>, NumberOfDests, Accumulator) ->
    {DlName, Tail} = esmpp34raw_utils:get_var_c_octet_string(Stream, 21),
    get_dest_addresses_list(Tail, NumberOfDests - 1, [DlName | Accumulator]).



pack_dest_addresses_list(Accumulator, Size, []) ->
    {Size, Accumulator};

pack_dest_addresses_list(Accumulator, Size, [{DestAddrTon, DestAddrNpi, DestinationAddr} | Rest]) ->
    PackedDestinationAddr = esmpp34raw_utils:pack_var_c_octet_string(DestinationAddr, 21),
    pack_dest_addresses_list( << Accumulator           /binary,
                                 1:                   8/big-unsigned-integer,
                                 DestAddrTon:         8/big-unsigned-integer,
                                 DestAddrNpi:         8/big-unsigned-integer,
                                 PackedDestinationAddr /binary>>, Size + 1, Rest);

pack_dest_addresses_list(Accumulator, Size, [DlName | Rest]) ->
    PackedDlName = esmpp34raw_utils:pack_var_c_octet_string(DlName, 21),
    pack_dest_addresses_list( << Accumulator  /binary,
                                 2:          8/big-unsigned-integer,
                                 PackedDlName /binary>>, Size + 1, Rest).
