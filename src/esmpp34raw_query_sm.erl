-module(esmpp34raw_query_sm).
-author("Morozov Alexander aka ~ArchimeD~").

-export([
         unpack/1,
         pack/1
        ]).

-include("esmpp34raw_types.hrl").
-include("esmpp34raw_tags.hrl").



-spec unpack (binary())    -> #query_sm{}.
-spec pack   (#query_sm{}) -> binary().



unpack(Stream) when is_binary(Stream) ->
    {MessageId, Stream_1} = esmpp34raw_utils:get_var_c_octet_string(Stream, 65),
    << SourceAddrTon: 8/big-unsigned-integer,
       SourceAddrNpi: 8/big-unsigned-integer,
       Stream_2        /binary >> = Stream_1,
    {SourceAddr, <<>>}    = esmpp34raw_utils:get_var_c_octet_string(Stream_2, 21),
    #query_sm { message_id      = MessageId,
                source_addr_ton = SourceAddrTon,
                source_addr_npi = SourceAddrNpi,
                source_addr     = SourceAddr }.



pack(#query_sm{} = Body) ->
    MessageId     = esmpp34raw_utils:pack_var_c_octet_string(Body#query_sm.message_id, 65),
    SourceAddrTon = Body#query_sm.source_addr_ton,
    SourceAddrNpi = Body#query_sm.source_addr_npi,
    SourceAddr    = esmpp34raw_utils:pack_var_c_octet_string(Body#query_sm.source_addr, 21),
    << MessageId       /binary,
       SourceAddrTon: 8/big-unsigned-integer,
       SourceAddrNpi: 8/big-unsigned-integer,
       SourceAddr      /binary >>.
