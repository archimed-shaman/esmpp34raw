-module(esmpp34raw_optionals).
-author("Morozov Alexander aka ~ArchimeD~").

-export([
         get_decoder/2,
         get_encoder/1
        ]).

-include("esmpp34raw_types.hrl").
-include("esmpp34raw_tags.hrl").



-spec get_decoder (integer(), integer()) -> fun((binary()) -> any()).
-spec get_encoder (integer())            -> fun((any()) -> binary()).



get_decoder(?dest_addr_subunit, 2 = _Size) ->
    fun(<<X:16/big-unsigned-integer>>) -> X end;                          %% 5.3.2.1

get_decoder(?source_addr_subunit, 1 = _Size) ->
    fun(<<X: 8/big-unsigned-integer>>) -> X end;                          %% 5.3.2.2

get_decoder(?dest_network_type, 1 = _Size) ->
    fun(<<X: 8/big-unsigned-integer>>) -> X end;                          %% 5.3.2.3

get_decoder(?source_network_type, 1 = _Size) ->
    fun(<<X: 8/big-unsigned-integer>>) -> X end;                          %% 5.3.2.4

get_decoder(?dest_bearer_type, 1 = _Size) ->
    fun(<<X: 8/big-unsigned-integer>>) -> X end;                          %% 5.3.2.5

get_decoder(?source_bearer_type, 1 = _Size) ->
    fun(<<X: 8/big-unsigned-integer>>) -> X end;                          %% 5.3.2.6

%% FIXME: it is said, that dest_telematics_id should be of 2 bytes, and the source_telematics_id
%% of one byte. Check, if it is a mustake
get_decoder(?dest_telematics_id, 2 = _Size) ->
    fun(<<X: 16/big-unsigned-integer>>) -> X end;                         %% 5.3.2.7

get_decoder(?source_telematics_id, 1 = _Size) ->
    fun(<<X: 8/big-unsigned-integer>>) -> X end;                          %% 5.3.2.8

get_decoder(?qos_time_to_live, 4 = _Size) ->
    fun(<<X: 32/big-unsigned-integer>>) -> X end;                         %% 5.3.2.9

get_decoder(?payload_type, 1 = _Size) ->
    fun(<<X: 8/big-unsigned-integer>>) -> X end;                          %% 5.3.2.10

get_decoder(?additional_status_info_text, Size) when Size =< 256 ->
    fun(X) ->
            {Y, <<>>} = esmpp34raw_utils:get_c_octet_string(X, [Size]),     %% 5.3.2.11
            Y
    end;

get_decoder(?receipted_message_id, Size) when Size =< 65 ->
    fun(X) ->
            {Y, <<>>} = esmpp34raw_utils:get_c_octet_string(X, [Size]),   %% 5.3.2.12
            Y
    end;

%% TODO: add decoder for ms_msg_wait_facilities bitmask
get_decoder(?ms_msg_wait_facilities, 1 = _Size) ->
    fun(<<X: 8/big-unsigned-integer>>) -> X end;                          %% 5.3.2.13

get_decoder(?privacy_indicator, 1 = _Size) ->
    fun(<<X: 8/big-unsigned-integer>>) -> X end;                          %% 5.3.2.14

%% TODO: add decoder for source_subaddress bitmask and for the other octets
get_decoder(?source_subaddress, Size) when Size >= 2; Size =< 23 ->
    fun(<<X: Size/binary>>) -> binary_to_list(X) end;                     %% 5.3.2.15

%% TODO: add decoder for dest_subaddress bitmask and for the other octets
get_decoder(?dest_subaddress, Size) when Size >= 2, Size =< 23 ->
    fun(<<X: Size/binary>>) -> binary_to_list(X) end;                     %% 5.3.2.16

get_decoder(?user_message_reference, 2 = _Size) ->
    fun(<<X:16/big-unsigned-integer>>) -> X end;                          %% 5.3.2.17

get_decoder(?user_response_code, 1 = _Size) ->
    fun(<<X: 8/big-unsigned-integer>>) -> X end;                          %% 5.3.2.18

get_decoder(?language_indicator, 1 = _Size) ->
    fun(<<X: 8/big-unsigned-integer>>) -> X end;                          %% 5.3.2.19

get_decoder(?source_port, 2 = _Size) ->
    fun(<<X:16/big-unsigned-integer>>) -> X end;                          %% 5.3.2.20

get_decoder(?destination_port, 2 = _Size) ->
    fun(<<X:16/big-unsigned-integer>>) -> X end;                          %% 5.3.2.21

get_decoder(?sar_msg_ref_num, 2 = _Size) ->
    fun(<<X:16/big-unsigned-integer>>) -> X end;                          %% 5.3.2.22

get_decoder(?sar_total_segments, 1 = _Size) ->
    fun(<<X: 8/big-unsigned-integer>>) -> X end;                          %% 5.3.2.23

get_decoder(?sar_segment_seqnum, 1 = _Size) ->
    fun(<<X: 8/big-unsigned-integer>>) -> X end;                          %% 5.3.2.24

get_decoder(?sc_interface_version, 1 = _Size) ->
    fun(<<X: 8/big-unsigned-integer>>) -> X end;                          %% 5.3.2.25

get_decoder(?display_time, 1 = _Size) ->
    fun(<<X: 8/big-unsigned-integer>>) -> X end;                          %% 5.3.2.26

get_decoder(?ms_validity, 1 = _Size) ->
    fun(<<X: 8/big-unsigned-integer>>) -> X end;                          %% 5.3.2.27

get_decoder(?dpf_result, 1 = _Size) ->
    fun(<<X: 8/big-unsigned-integer>>) -> X end;                          %% 5.3.2.28

get_decoder(?set_dpf, 1 = _Size) ->
    fun(<<X: 8/big-unsigned-integer>>) -> X end;                          %% 5.3.2.29

get_decoder(?ms_availability_status, 1 = _Size) ->
    fun(<<X: 8/big-unsigned-integer>>) -> X end;                          %% 5.3.2.30

get_decoder(?network_error_code, 3 = _Size) ->
    fun(<<X: 3/binary>>) -> binary_to_list(X) end;                        %% 5.3.2.31

get_decoder(?message_payload, Size) ->
    fun(<<X: Size/binary>>) -> binary_to_list(X) end;                     %% 5.3.2.32

get_decoder(?delivery_failure_reason, 1 = _Size) ->
    fun(<<X: 8/big-unsigned-integer>>) -> X end;                          %% 5.3.2.33

get_decoder(?more_messages_to_send, 1 = _Size) ->
    fun(<<X: 8/big-unsigned-integer>>) -> X end;                          %% 5.3.2.34

get_decoder(?message_state, 1 = _Size) ->
    fun(<<X: 8/big-unsigned-integer>>) -> X end;                          %% 5.3.2.35

%% TODO: add decoder for callback_num bitmask
get_decoder(?callback_num, Size) when Size >= 4, Size =< 19 ->
    fun(<<X: Size/binary>>) -> binary_to_list(X) end;                     %% 5.3.2.36

%% TODO: add decoder for callback_num_pres_ind bitmask
get_decoder(?callback_num_pres_ind, 1 = _Size) ->
    fun(<<X: 8/big-unsigned-integer>>) -> X end;                          %% 5.3.2.37

%% TODO: add decoder for callback_num_atag bitmask in the first octet
get_decoder(?callback_num_atag, Size) when Size =< 65 ->
    fun(<<X: Size/binary>>) -> binary_to_list(X) end;                     %% 5.3.2.38

get_decoder(?number_of_messages, 1 = _Size) ->
    fun(<<X: 8/big-unsigned-integer>>) -> X end;                          %% 5.3.2.39

get_decoder(?sms_signal, 2 = _Size) ->
    fun(<<X:16/big-unsigned-integer>>) -> X end;                          %% 5.3.2.40

%% FIXME: check, if alert_on_msg_delivery is decoded correctly, as it has an empty body
%% alert_on_msg_delivery has no body
get_decoder(?alert_on_msg_delivery, 0 = _Size) ->
    fun(<<>>) -> true end;                                                %% 5.3.2.41

get_decoder(?its_reply_type, 1 = _Size) ->
    fun(<<X: 8/big-unsigned-integer>>) -> X end;                          %% 5.3.2.42

%% TODO: add decoder for its_session_info string
get_decoder(?its_session_info, 2 = _Size) ->
    fun(<<X: 2/binary>>) -> binary_to_list(X) end;                        %% 5.3.2.43

get_decoder(?ussd_service_op, 1 = _Size) ->
    fun(<<X: 1/binary>>) -> binary_to_list(X) end;                        %% 5.3.2.44

get_decoder(_Operation, Size) ->                                          %% unknown optional PDU
    fun(<<X: Size/binary>>) -> X end.




get_encoder(?dest_addr_subunit) ->
    fun(X) -> <<X:16/big-unsigned-integer>> end;                          %% 5.3.2.1

get_encoder(?source_addr_subunit) ->
    fun(X) -> <<X: 8/big-unsigned-integer>> end;                          %% 5.3.2.2

get_encoder(?dest_network_type) ->
    fun(X) -> <<X: 8/big-unsigned-integer>> end;                          %% 5.3.2.3

get_encoder(?source_network_type) ->
    fun(X) -> <<X: 8/big-unsigned-integer>> end;                          %% 5.3.2.4

get_encoder(?dest_bearer_type) ->
    fun(X) -> <<X: 8/big-unsigned-integer>> end;                          %% 5.3.2.5

get_encoder(?source_bearer_type) ->
    fun(X) -> <<X: 8/big-unsigned-integer>> end;                          %% 5.3.2.6

%% FIXME: it is said, that dest_telematics_id should be of 2 bytes, and the source_telematics_id
%% of one byte. Check, if it is a mustake
get_encoder(?dest_telematics_id) ->
    fun(X) -> <<X: 16/big-unsigned-integer>> end;                         %% 5.3.2.7

get_encoder(?source_telematics_id) ->
    fun(X) -> <<X: 8/big-unsigned-integer>> end;                          %% 5.3.2.8

get_encoder(?qos_time_to_live) ->
    fun(X) -> <<X: 32/big-unsigned-integer>> end;                         %% 5.3.2.9

get_encoder(?payload_type) ->
    fun(X) -> <<X: 8/big-unsigned-integer>> end;                          %% 5.3.2.10

get_encoder(?additional_status_info_text) ->
    fun(X) when is_list(X) ->
            esmpp34raw_utils:pack_var_c_octet_string(X, 256)              %% 5.3.2.11
    end;

get_encoder(?receipted_message_id) ->
    fun(X) when is_list(X) ->
            esmpp34raw_utils:pack_var_c_octet_string(X, 65)                    %% 5.3.2.12
    end;

get_encoder(?ms_msg_wait_facilities) ->
    fun(X) -> <<X: 8/big-unsigned-integer>> end;                          %% 5.3.2.13

get_encoder(?privacy_indicator) ->
    fun(X) -> <<X: 8/big-unsigned-integer>> end;                          %% 5.3.2.14

get_encoder(?source_subaddress) ->
    fun(X) when is_list(X); size(X) >=2; size(X) =< 23 ->                 %% 5.3.2.15
            <<X/binary>>
    end;

get_encoder(?dest_subaddress) ->
    fun(X) when is_list(X); size(X) >=2; size(X) =< 23 ->                 %% 5.3.2.16
            <<X/binary>>
    end;

get_encoder(?user_message_reference) ->
    fun(X) -> <<X:16/big-unsigned-integer>> end;                          %% 5.3.2.17

get_encoder(?user_response_code) ->
    fun(X) -> <<X: 8/big-unsigned-integer>> end;                          %% 5.3.2.18

get_encoder(?language_indicator) ->
    fun(X) -> <<X: 8/big-unsigned-integer>> end;                          %% 5.3.2.19

get_encoder(?source_port) ->
    fun(X) -> <<X:16/big-unsigned-integer>> end;                          %% 5.3.2.20

get_encoder(?destination_port) ->
    fun(X) -> <<X:16/big-unsigned-integer>> end;                          %% 5.3.2.21

get_encoder(?sar_msg_ref_num) ->
    fun(X) -> <<X:16/big-unsigned-integer>> end;                          %% 5.3.2.22

get_encoder(?sar_total_segments) ->
    fun(X) -> <<X: 8/big-unsigned-integer>> end;                          %% 5.3.2.23

get_encoder(?sar_segment_seqnum) ->
    fun(X) -> <<X: 8/big-unsigned-integer>> end;                          %% 5.3.2.24

get_encoder(?sc_interface_version) ->
    fun(X) -> <<X: 8/big-unsigned-integer>> end;                          %% 5.3.2.25

get_encoder(?display_time) ->
    fun(X) -> <<X: 8/big-unsigned-integer>> end;                          %% 5.3.2.26

get_encoder(?ms_validity) ->
    fun(X) -> <<X: 8/big-unsigned-integer>> end;                          %% 5.3.2.27

get_encoder(?dpf_result) ->
    fun(X) -> <<X: 8/big-unsigned-integer>> end;                          %% 5.3.2.28

get_encoder(?set_dpf) ->
    fun(X) -> <<X: 8/big-unsigned-integer>> end;                          %% 5.3.2.29

get_encoder(?ms_availability_status) ->
    fun(X) -> <<X: 8/big-unsigned-integer>> end;                          %% 5.3.2.30

get_encoder(?network_error_code) ->
    fun(X) when is_list(X); size(X) == 3 ->
            list_to_binary(X)                                             %% 5.3.2.31
    end;

get_encoder(?message_payload) ->
    fun(X) when is_list(X) -> list_to_binary(X) end;                      %% 5.3.2.32

get_encoder(?delivery_failure_reason) ->
    fun(X) -> <<X: 8/big-unsigned-integer>> end;                          %% 5.3.2.33

get_encoder(?more_messages_to_send) ->
    fun(X) -> <<X: 8/big-unsigned-integer>> end;                          %% 5.3.2.34

get_encoder(?message_state) ->
    fun(X) -> <<X: 8/big-unsigned-integer>> end;                          %% 5.3.2.35

get_encoder(?callback_num) ->
    fun(X) when is_list(X); size(X) >=4; size(X) =< 19 ->
            <<X/binary>>                                                  %% 5.3.2.36
    end;

get_encoder(?callback_num_pres_ind) ->
    fun(X) -> <<X: 8/big-unsigned-integer>> end;                          %% 5.3.2.37

get_encoder(?callback_num_atag) ->
    fun(X) when is_list(X); size(X) =< 65 ->
            <<X/binary>>                                                  %% 5.3.2.38
    end;

get_encoder(?number_of_messages) ->
    fun(X) -> <<X: 8/big-unsigned-integer>> end;                          %% 5.3.2.39

get_encoder(?sms_signal) ->
    fun(X) -> <<X:16/big-unsigned-integer>> end;                          %% 5.3.2.40

%% FIXME: check, if alert_on_msg_delivery is encoded correctly, as it has an empty body
%% alert_on_msg_delivery has no body
get_encoder(?alert_on_msg_delivery) ->
    fun(_) -> <<>> end;                                                   %% 5.3.2.41

get_encoder(?its_reply_type) ->
    fun(X) -> <<X: 8/big-unsigned-integer>> end;                          %% 5.3.2.42

%% TODO: make available the int - encoding too
get_encoder(?its_session_info) ->
    fun(X) when is_list(X); size(X) == 2 -> list_to_binary(X) end;          %% 5.3.2.43

get_encoder(?ussd_service_op) ->
    fun(X) when is_list(X); size(X) == 1 -> list_to_binary(X) end.          %% 5.3.2.44
