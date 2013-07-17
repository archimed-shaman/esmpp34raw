-ifndef(esmpp34raw_utils).
-define(esmpp34raw_utils, true).

-include("esmpp34raw_types.hrl").

-spec unpack_optional(pdu_body(), binary()) -> pdu_body().

-define (unpack_optional(Record),
	 unpack_optional(#Record{} = Body, <<>>) ->
		Body).

-define (define_optional(Record, Field, Code),
	 unpack_optional(#Record{} = Body, <<Code:         16/big-unsigned-integer,
					     Length:       16/big-unsigned-integer,
					     BinValue: Length/binary,
					     Rest            /binary
					   >>) ->
		DecodeFun = esmpp34raw_optionals:get_decoder(Code, Length),
		Value = try DecodeFun(BinValue)
			catch
			    Exc:What -> throw({bad_optional, {Exc, What}, Code, Length, BinValue})
			end,
		unpack_optional(Body#Record{Field = Value}, Rest)).

-define (optional_done(Record), 
	 unpack_optional(#Record{} = Body, <<_Tag:       16/big-unsigned-integer, 
					     Length:     16/big-unsigned-integer,
					     _Value: Length/binary,
					     Rest          /binary>>) -> unpack_optional(Body, Rest);
	 unpack_optional(_, _) -> throw(bad_optional)).

-define (pack_optional(Field, Code),
	 esmpp34raw_utils:pack_optional_tlv(Code, Field, esmpp34raw_optionals:get_encoder(Code))).

-endif.
