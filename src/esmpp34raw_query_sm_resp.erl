-module(esmpp34raw_query_sm_resp).
-author("Morozov Alexander aka ~ArchimeD~").

-export([
         unpack/1,
         pack/1
        ]).

-include("esmpp34raw_types.hrl").
-include("esmpp34raw_tags.hrl").



-spec unpack (binary())         -> #query_sm_resp{}.
-spec pack   (#query_sm_resp{}) -> binary().



unpack(Stream) when is_binary(Stream) ->
    {MessageId, Stream_1} = esmpp34raw_utils:get_var_c_octet_string(Stream, 65),
    {FinalDate, Stream_2} = esmpp34raw_utils:get_c_octet_string(Stream_1, [1, 17]),
    << MessageState: 8/big-unsigned-integer,
       ErrorCode:    8/big-unsigned-integer >> = Stream_2,
    #query_sm_resp { message_id    = MessageId,
                     final_date    = FinalDate,
                     message_state = MessageState,
                     error_code    = ErrorCode }.



pack(#query_sm_resp{} = Body) ->
    MessageId    = esmpp34raw_utils:pack_var_c_octet_string(Body#query_sm_resp.message_id, 65),
    FinalDate    = esmpp34raw_utils:pack_var_c_octet_string(Body#query_sm_resp.final_date, 17),
    MessageState = Body#query_sm_resp.message_state,
    ErrorCode    = Body#query_sm_resp.error_code,
    << MessageId      /binary,
       FinalDate      /binary,
       MessageState: 8/big-unsigned-integer,
       ErrorCode:    8/big-unsigned-integer >>.
