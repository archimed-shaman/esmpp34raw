-module(esmpp34raw_deliver_sm_resp).
-author("Morozov Alexander aka ~ArchimeD~").

-export([
         unpack/1,
         pack/1
        ]).

-include("esmpp34raw_types.hrl").



-spec unpack (binary())           -> #deliver_sm_resp{}.
-spec pack   (#deliver_sm_resp{}) -> binary().



unpack(Stream) when is_binary(Stream) ->
    {MessageId, <<>>}   = esmpp34raw_utils:get_var_c_octet_string(Stream, 1),
    #deliver_sm_resp {message_id = MessageId}.



pack(#deliver_sm_resp{message_id = MessageId}) ->
    esmpp34raw_utils:pack_var_c_octet_string(MessageId, 1).
