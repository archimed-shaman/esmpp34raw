-module(esmpp34raw_data_sm_resp).
-author("Morozov Alexander aka ~ArchimeD~").

-export([
	 unpack/1,
	 pack/1
	]).

-include("esmpp34raw_types.hrl").
-include("esmpp34raw_tags.hrl").
-include("esmpp34raw_utils.hrl").


-spec unpack (binary())           -> #data_sm_resp{}.
-spec pack   (#data_sm_resp{})    -> binary().



unpack(Stream) when is_binary(Stream) ->
    {MessageId, Stream_1}   = esmpp34raw_utils:get_var_c_octet_string(Stream, 65),
    unpack_optional(#data_sm_resp {message_id = MessageId}, Stream_1).



pack(#data_sm_resp{} = Body) ->
    MessageId                = esmpp34raw_utils:pack_var_c_octet_string(Body#data_sm_resp.message_id, 65),
    DeliveryFailureReason    = ?pack_optional(Body#data_sm_resp.delivery_failure_reason,     ?delivery_failure_reason),
    NetworkErrorCode         = ?pack_optional(Body#data_sm_resp.network_error_code,          ?network_error_code),
    AdditionalStatusInfoText = ?pack_optional(Body#data_sm_resp.additional_status_info_text, ?additional_status_info_text),
    DpfResult                = ?pack_optional(Body#data_sm_resp.dpf_result,                  ?dpf_result),
    << MessageId                /binary,
       DeliveryFailureReason    /binary,
       NetworkErrorCode         /binary,
       AdditionalStatusInfoText /binary,
       DpfResult                /binary >>.



?unpack_optional (data_sm_resp);
?define_optional (data_sm_resp, delivery_failure_reason,     ?delivery_failure_reason);
?define_optional (data_sm_resp, network_error_code,          ?network_error_code);
?define_optional (data_sm_resp, additional_status_info_text, ?additional_status_info_text);
?define_optional (data_sm_resp, dpf_result,                  ?dpf_result);
?optional_done   (data_sm_resp).

