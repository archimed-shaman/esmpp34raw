-module(esmpp34raw_submit_multi_resp).
-author("Morozov Alexander aka ~ArchimeD~").

-export([
	 unpack/1,
	 pack/1
	]).

-include("esmpp34raw_types.hrl").



-spec unpack                  (binary())                    -> #submit_multi_resp{}.
-spec pack                    (#submit_multi_resp{})        -> binary().
-spec pack_unsuccess_sme_list (binary(), integer(), list()) -> {integer(), binary()}.
-spec get_unsuccess_sme_list  (binary(), integer(), list()) -> {list(), binary()}.


			    
unpack(Stream) when is_binary(Stream) ->
    {MessageId, Stream_1}   = esmpp34raw_utils:get_var_c_octet_string(Stream, 65),
    << NoUnsuccess: 8/big-unsigned-integer,
       Stream_2      /binary>> = Stream_1,
    {UnsuccessSme, <<>>} = get_unsuccess_sme_list(Stream_2, NoUnsuccess, []),
    #submit_multi_resp { message_id    = MessageId,
			 no_unsuccess  = NoUnsuccess,
			 unsuccess_sme = UnsuccessSme}.



pack(#submit_multi_resp{ message_id    = MessageId,
			 no_unsuccess  = NoUnsuccess,
			 unsuccess_sme = UnsuccessSme}) when is_list(UnsuccessSme) ->
    BinMessageId         = esmpp34raw_utils:pack_var_c_octet_string(MessageId, 65),
    {_, BinUnsuccessSme} = case length(UnsuccessSme) /= NoUnsuccess of
			       true -> throw(bad_unsuccess_sme_length);
			       _    -> pack_unsuccess_sme_list(<<>>, NoUnsuccess, UnsuccessSme)
			   end,
    << BinMessageId    /binary,
       NoUnsuccess:   8/big-unsigned-integer,
       BinUnsuccessSme /binary>>.



get_unsuccess_sme_list(Stream, 0, Accumulator) ->
    {lists:reverse(Accumulator), Stream};

get_unsuccess_sme_list(<< DestAddrTon: 8/big-unsigned-integer,
			  DestAddrNpi: 8/big-unsigned-integer,
			  Stream        /binary>>, NoUnsuccess, Accumulator) ->
    {DestinationAddr, Stream_1} = esmpp34raw_utils:get_var_c_octet_string(Stream, 21),
    <<ErrorStatusCode: 32/big-unsigned-integer, Tail/binary>> = Stream_1,
    get_unsuccess_sme_list(Tail, NoUnsuccess - 1, [{ DestAddrTon,
						     DestAddrNpi,
						     DestinationAddr,
						     ErrorStatusCode} | Accumulator]).
    


pack_unsuccess_sme_list(Accumulator, Size, []) ->
    {Size, Accumulator};

pack_unsuccess_sme_list(Accumulator, Size, [{ DestAddrTon,
					      DestAddrNpi,
					      DestinationAddr,
					      ErrorStatusCode} | Rest]) ->
    PackedDestinationAddr = esmpp34raw_utils:pack_var_c_octet_string(DestinationAddr, 21),
    pack_unsuccess_sme_list( << Accumulator           /binary,
				DestAddrTon:         8/big-unsigned-integer,
				DestAddrNpi:         8/big-unsigned-integer,
				PackedDestinationAddr /binary,
				ErrorStatusCode:    32/big-unsigned-integer>>, Size + 1, Rest).

