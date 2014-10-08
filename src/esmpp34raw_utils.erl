-module(esmpp34raw_utils).
-author("Morozov Alexander aka ~ArchimeD~").

-export([
         get_var_c_octet_string/2,
         get_c_octet_string/2,
         pack_var_c_octet_string/2,
         pack_optional_tlv/3
        ]).



-spec get_var_c_octet_string    (binary(),  integer())            -> {string(), binary()};
                                (binary(),  infinity)             -> {string(), binary()}.
-spec get_c_octet_string        (binary(),  [integer()])          -> {string(), binary()}.
-spec pack_var_c_octet_string   (string(),  integer())            -> binary().
-spec pack_optional_tlv         (integer(), any(),    function()) -> binary().
-spec unpack_var_c_octet_string (string(),  binary(), integer())  -> {string(), binary()};
                                (string(),  binary(), infinity)   -> {string(), binary()}.



%% @doc
%% Ejects the c-octet-string from the binary sequence. It is just null-terminated ascii string
%% Arguments:
%%     1) Stream:    binary()  - the input binary sequence
%%     2) MaxLength: integer() - the maximal length of the string (defined in specification)
%% Returns:
%%     1) the tuple of two elements:
%%       1.1) the decoded string
%%       1.2) the rest of the binary stream
%% @end

get_var_c_octet_string(Stream, MaxLength) when is_binary(Stream) ->
    unpack_var_c_octet_string([], Stream, MaxLength).



get_c_octet_string(Stream, PossibleSizes) when is_list(PossibleSizes) ->
    SortedSizes = lists:sort(PossibleSizes),
    unpack_c_octet_string([], 0, Stream, SortedSizes).



%% @doc
%% Encodes the sting as c-octet-string into the binary sequnce
%% Arguments:
%%     1) String:    string()  - the string to be encoded
%%     2) MaxLength: integer() - the maximal length of the string (defined in specification)
%% Returns:
%%     1) the binary sequence
%% @end

pack_var_c_octet_string(String, MaxLength) when length(String) =< MaxLength ->
    BinString = list_to_binary(String),
    <<BinString/binary, 0:8>>.



%% @doc
%% Encodes the optional value into TLV (Tag-Length-Value) binary sequence.
%% Arguments:
%%     1) Tag:   integer()  - the tag of the field to be encoded
%%     2) Value: any()      - the value of the field itself. If undefined, the empty binary is returned
%%     3) F:     function() - the function, that encodes the body of the field
%% Returns:
%%     1) the binary sequence
%% @end

pack_optional_tlv(_Tag, undefined, _F) ->
    <<>>;

pack_optional_tlv(Tag, Value, F) when is_function(F) ->
    BinValue = F(Value),
    Length = byte_size(BinValue),
    <<Tag:           16/big-unsigned-integer,   %% TAG
      Length:        16/big-unsigned-integer,   %% LENGTH
      BinValue         /binary>>.               %% VALUE



unpack_var_c_octet_string(Accumulator, <<0:8, Tail/binary>>, infinity) ->
    {lists:reverse(Accumulator), Tail};

unpack_var_c_octet_string(Accumulator, <<Head:8, Tail/binary>>, infinity) ->
    unpack_var_c_octet_string([Head | Accumulator], Tail, infinity);

%% the final zero is included into the maximal length of the string
unpack_var_c_octet_string(Accumulator, <<0:8, Tail/binary>>, RestLength) when RestLength > 0 ->
    {lists:reverse(Accumulator), Tail};

unpack_var_c_octet_string(Accumulator, <<Head:8, Tail/binary>>, RestLength) when RestLength > 0 ->
    unpack_var_c_octet_string([Head | Accumulator], Tail, RestLength - 1);

unpack_var_c_octet_string(_Accumulator, _Binary, _RestLength) ->
    throw(bad_string).



unpack_c_octet_string(_Accumulator, _Length, _Binary, []) ->
    throw(bad_string);

unpack_c_octet_string(Accumulator, Length, <<0:8, Tail/binary>>, [Value | _Values]) when Length + 1 == Value ->
    {lists:reverse(Accumulator), Tail};

unpack_c_octet_string(_Accumulator, _Length, <<0:8, _/binary>>, _Values) ->
    throw(bad_string);

unpack_c_octet_string(Accumulator, Length, <<Head:8, Tail/binary>>, [Value | Values]) when Length + 1 < Value ->
    unpack_c_octet_string([Head | Accumulator], Length + 1, Tail, [Value | Values]);

unpack_c_octet_string(Accumulator, Length, <<Head:8, Tail/binary>>, [Value | Values]) when Length + 1 == Value ->
    unpack_c_octet_string([Head | Accumulator], Length + 1, Tail, Values);

unpack_c_octet_string(_Accumulator, _Length, _Binary, _RestLength) ->
    throw(bad_string).
