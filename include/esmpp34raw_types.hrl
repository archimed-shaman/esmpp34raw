-ifndef(esmpp34raw_types).
-define(esmpp34raw_types, true).

-define(generic_nack,          16#80000000).
-define(bind_receiver,         16#00000001).
-define(bind_receiver_resp,    16#80000001).
-define(bind_transmitter,      16#00000002).
-define(bind_transmitter_resp, 16#80000002).
-define(query_sm,              16#00000003).
-define(query_sm_resp,         16#80000003).
-define(submit_sm,             16#00000004).
-define(submit_sm_resp,        16#80000004).
-define(deliver_sm,            16#00000005).
-define(deliver_sm_resp,       16#80000005).
-define(unbind,                16#00000006).
-define(unbind_resp,           16#80000006).
-define(replace_sm,            16#00000007).
-define(replace_sm_resp,       16#80000007).
-define(cancel_sm,             16#00000008).
-define(cancel_sm_resp,        16#80000008).
-define(bind_transceiver,      16#00000009).
-define(bind_transceiver_resp, 16#80000009).
-define(outbind,               16#0000000B).
-define(enquire_link,          16#00000015).
-define(enquire_link_resp,     16#80000015).
-define(submit_multi,          16#00000021).
-define(submit_multi_resp,     16#80000021).
-define(alert_notification,    16#00000102).
-define(data_sm,               16#00000103).
-define(data_sm_resp,          16#80000103).


-record (pdu, {command_length,
               command_id,
               command_status   = 0,
               sequence_number,
               body}).

-record (unknown_pdu, {error,
                       binary_sequence}).

-record (bind_transmitter, {system_id,
                            password           = [],
                            system_type,
                            interface_version,
                            addr_ton           = 0,
                            addr_npi           = 0,
                            address_range      = []}).

-record (bind_transmitter_resp, {system_id,
                                 sc_interface_version}).

-record (bind_receiver, {system_id,
                         password           = [],
                         system_type,
                         interface_version,
                         addr_ton           = 0,
                         addr_npi           = 0,
                         address_range      = []}).

-record (bind_receiver_resp, {system_id,
                              sc_interface_version}).

-record (bind_transceiver, {system_id,
                            password           = [],
                            system_type,
                            interface_version,
                            addr_ton           = 0,
                            addr_npi           = 0,
                            address_range      = []}).

-record (bind_transceiver_resp, {system_id,
                                 sc_interface_version}).

-record(outbind, {system_id,
                  password = []}).

-record(unbind, {}).

-record(unbind_resp, {}).

-record(generic_nack, {}).

-record(submit_sm, { service_type             = [],
                     source_addr_ton          = 0,
                     source_addr_npi          = 0,
                     source_addr              = [],
                     dest_addr_ton,
                     dest_addr_npi,
                     destination_addr,
                     esm_class,
                     protocol_id,
                     priority_flag,
                     schedule_delivery_time   = [],
                     validity_period          = [],
                     registered_delivery,
                     replace_if_present_flag,
                     data_coding,
                     sm_default_msg_id        = 0,
                     sm_length,
                     short_message,
                     %% optional
                     user_message_reference,
                     source_port,
                     source_addr_subunit,
                     destination_port,
                     dest_addr_subunit,
                     sar_msg_ref_num,
                     sar_total_segments,
                     sar_segment_seqnum,
                     more_messages_to_send,
                     payload_type,
                     message_payload,
                     privacy_indicator,
                     callback_num,
                     callback_num_pres_ind,
                     callback_num_atag,
                     source_subaddress,
                     dest_subaddress,
                     user_response_code,
                     display_time,
                     sms_signal,
                     ms_validity,
                     ms_msg_wait_facilities,
                     number_of_messages,
                     alert_on_msg_delivery,
                     language_indicator,
                     its_reply_type,
                     its_session_info,
                     ussd_service_op}).

-record (submit_sm_resp, {message_id}).

-record(submit_multi, { service_type            = [],
                        source_addr_ton         = 0,
                        source_addr_npi         = 0,
                        source_addr             = [],
                        number_of_dests,
                        dest_address,
                        esm_class,
                        protocol_id,
                        priority_flag,
                        schedule_delivery_time  = [],
                        validity_period         = [],
                        registered_delivery,
                        %% TODO: remove from record, just encode/decode
                        replace_if_present_flag = 0,
                        data_coding,
                        sm_default_msg_id       = 0,
                        sm_length,
                        short_message,
                        %% optional
                        user_message_reference,
                        source_port,
                        source_addr_subunit,
                        destination_port,
                        dest_addr_subunit,
                        sar_msg_ref_num,
                        sar_total_segments,
                        sar_segment_seqnum,
                        payload_type,
                        message_payload,
                        privacy_indicator,
                        callback_num,
                        callback_num_pres_ind,
                        callback_num_atag,
                        source_subaddress,
                        dest_subaddress,
                        display_time,
                        sms_signal,
                        ms_validity,
                        ms_msg_wait_facilities,
                        alert_on_msg_delivery,
                        language_indicator}).

-record (submit_multi_resp, { message_id,
                              no_unsuccess,
                              unsuccess_sme}).

-record(deliver_sm, { service_type,
                      source_addr_ton         = 0,
                      source_addr_npi         = 0,
                      source_addr             = [],
                      dest_addr_ton,
                      dest_addr_npi,
                      destination_addr,
                      esm_class,
                      protocol_id,
                      priority_flag,
                      %% TODO: remove from record, unused. Just encode/decode
                      schedule_delivery_time  = [],
                      %% TODO: remove from record, unused. Just encode/decode
                      validity_period         = [],
                      registered_delivery,
                      %% TODO: remove from record, unused. Just encode/decode
                      replace_if_present_flag = 0,
                      data_coding,
                      %% TODO: remove from record, unused. Just encode/decode
                      sm_default_msg_id       = 0,
                      sm_length,
                      short_message,
                      %% optional
                      user_message_reference,
                      source_port,
                      destination_port,
                      sar_msg_ref_num,
                      sar_total_segments,
                      sar_segment_seqnum,
                      user_response_code,
                      privacy_indicator,
                      payload_type,
                      message_payload,
                      callback_num,
                      source_subaddress,
                      dest_subaddress,
                      language_indicator,
                      its_session_info,
                      network_error_code,
                      message_state,
                      receipted_message_id}).

%% TODO: remove message_id from record, unused.
-record (deliver_sm_resp, { message_id = []}).

-record (data_sm, { service_type,
                    source_addr_ton         = 0,
                    source_addr_npi         = 0,
                    source_addr,
                    dest_addr_ton,
                    dest_addr_npi,
                    destination_addr,
                    esm_class,
                    registered_delivery,
                    data_coding,
                    %% optional
                    source_port,
                    source_addr_subunit,
                    source_network_type,
                    source_bearer_type,
                    source_telematics_id,
                    destination_port,
                    dest_addr_subunit,
                    dest_network_type,
                    dest_bearer_type,
                    dest_telematics_id,
                    sar_msg_ref_num,
                    sar_total_segments,
                    sar_segment_seqnum,
                    more_messages_to_send,
                    qos_time_to_live,
                    payload_type,
                    message_payload,
                    set_dpf,
                    receipted_message_id,
                    message_state,
                    network_error_code,
                    user_message_reference,
                    privacy_indicator,
                    callback_num,
                    callback_num_pres_ind,
                    callback_num_atag,
                    source_subaddress,
                    dest_subaddress,
                    user_response_code,
                    display_time,
                    sms_signal,
                    ms_validity,
                    ms_msg_wait_facilities,
                    number_of_messages,
                    alert_on_msg_delivery,
                    language_indicator,
                    its_reply_type,
                    its_session_info}).

-record (data_sm_resp, { message_id,
                         %% optional
                         delivery_failure_reason,
                         network_error_code,
                         additional_status_info_text,
                         dpf_result}).

-record (query_sm, { message_id,
                     source_addr_ton = 0,
                     source_addr_npi = 0,
                     source_addr     = []}).

-record (query_sm_resp, { message_id,
                          final_date     = [],
                          message_state,
                          error_code}).

-record (cancel_sm, { service_type     = [],
                      message_id       = [],
                      source_addr_ton  = 0,
                      source_addr_npi  = 0,
                      source_addr,
                      dest_addr_ton    = 0,
                      dest_addr_npi    = 0,
                      destination_addr = []}).

-record (cancel_sm_resp, {}).

-record (replace_sm, { message_id,
                       source_addr_ton        = 0,
                       source_addr_npi        = 0,
                       source_addr,
                       schedule_delivery_time = [],
                       validity_period        = [],
                       registered_delivery,
                       sm_default_msg_id,
                       sm_length,
                       short_message}).

-record (replace_sm_resp, {}).

-record (enquire_link, {}).

-record (enquire_link_resp, {}).

-record (alert_notification, { source_addr_ton = 0,
                               source_addr_npi = 0,
                               source_addr,
                               esme_addr_ton,
                               esme_addr_npi,
                               esme_addr,
                               ms_availability_status}).


%% TYPES

-type pdu_body() :: #unknown_pdu{}
                  | #bind_transmitter{}
                  | #bind_transmitter_resp{}
                  | #bind_receiver{}
                  | #bind_receiver_resp{}
                  | #bind_transceiver{}
                  | #bind_transceiver_resp{}
                  | #outbind{}
                  | #unbind{}
                  | #unbind_resp{}
                  | #generic_nack{}
                  | #submit_sm{}
                  | #submit_sm_resp{}
                  | #submit_multi{}
                  | #submit_multi_resp{}
                  | #deliver_sm{}
                  | #deliver_sm_resp{}
                  | #data_sm{}
                  | #data_sm_resp{}
                  | #query_sm{}
                  | #query_sm_resp{}
                  | #cancel_sm{}
                  | #cancel_sm_resp{}
                  | #replace_sm{}
                  | #replace_sm_resp{}
                  | #enquire_link{}
                  | #enquire_link_resp{}
                  | #alert_notification{}.

-endif.
