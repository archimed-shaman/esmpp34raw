-module(esmpp34raw_stream).
-author("Morozov Alexander aka ~ArchimeD~").

-export([
	 unpack_sequence/1,
	 pack_single/3
	]).

-include("esmpp34raw_types.hrl").
-include("esmpp34raw_constants.hrl").
-include("esmpp34raw_codes.hrl").


-spec unpack_sequence (binary())                                       -> {list(#pdu{}), list(any()), binary()}.
-spec pack_single     (pdu_body(),   integer(),   integer())           -> binary().

-spec unpack_pdu      (list(#pdu{}), list(any()), binary())            -> {list(#pdu{}), list(any()),  binary()}.
-spec pack_pdu        (pdu_body(),   integer(),   integer())           -> binary().
-spec pack_body       (pdu_body(),   integer(),   integer())           -> {integer(), binary()}.
-spec unpack_body     (integer(),    integer(),   integer(), binary()) -> pdu_body() | binary().
								


%% @doc 
%% Unpacks the binary stream.
%% Arguments:
%%     1) Stream: binary()  - the input binary sequence to be dissected
%% Returns:
%%     1) the tuple of two elements:
%%       1.1) the list of PDUs found
%%       1.2) the rest of the binary stream
%% @end

unpack_sequence(Stream) when is_binary(Stream) ->
    unpack_pdu([], [], Stream).



%% @doc 
%% Packs the single SMPP PDU to the binary sequence. You should couple the several sequences by your own
%% Arguments:
%%     1) PduBody: pdu_body()       - a record, containing a body of one of the allowed SMPP packages
%%     2) CommandStatus: integer()  - the command status value of the PDU
%%     3) SequnceNumber: integer()  - the sequnce number of the PDU
%% Returns:
%%     1) the binary sequnce, containing the encoded PDU
%% @end

pack_single(PduBody, CommandStatus, SequenceNumber) ->
    pack_pdu(PduBody, CommandStatus, SequenceNumber).



%% @doc
%% Unpacks the Protocol Data Units (PDU) from the binary stream. The header is common for the all types,
%% the body is depend on the pdu type.
%% Arguments:
%%     1) PduList: list(#pdu{})     - accumulator of the PDUs found
%%     2) FailedPduList(#pdu{})     - accumulator of the PDUs, which are not able to be parsed
%%     3) Stream: binary()          - the binary stream to be parsed
%% Returns:
%%     1) PduList: list(#pdu{})     - the list of PDUs found
%%     2) FailedPduList(#pdu{})     - the list of unknown PDUs
%%     3) Stream: binary()          - the rest of the binary stream
%% @end

unpack_pdu(PduList, FailedPduList, <<>>) ->
    {lists:reverse(PduList), lists:reverse(FailedPduList), <<>>};

unpack_pdu(PduList, FailedPduList, <<CommandLength:  32/big-unsigned-integer, 
				     CommandId:      32/big-unsigned-integer, 
				     CommandStatus:  32/big-unsigned-integer,
				     SequenceNumber: 32/big-unsigned-integer,
				     Tail              /binary>>) when CommandLength - 16 =< byte_size(Tail) ->
    BodyLength = CommandLength - 16,
    <<Body:BodyLength/binary, Rest/binary>> = Tail,
    try
	UnpackedBody = unpack_body(CommandId, CommandStatus, SequenceNumber, Body),
	Pdu = #pdu {command_length  = CommandLength,
		    command_id      = CommandId,
		    command_status  = CommandStatus,
		    sequence_number = SequenceNumber,
		    body            = UnpackedBody},
	unpack_pdu([Pdu | PduList], FailedPduList, Rest)
    catch
	throw:Cause ->
	    FailedPdu = #pdu {command_length  = CommandLength,
			      command_id      = CommandId,
			      command_status  = CommandStatus,
			      sequence_number = SequenceNumber,
			      body            = #unknown_pdu{error = Cause, binary_sequence = Body}},
	    unpack_pdu(PduList, [FailedPdu | FailedPduList], Rest)
    end;


unpack_pdu(PduList, FailedPduList, Stream) ->
    {lists:reverse(PduList), lists:reverse(FailedPduList), Stream}.



%% @doc 
%% Packs the SMPP PDU to the binary sequence.
%% Arguments:
%%     1) PduBody: pdu_body()       - a record, containing a body of one of the allowed SMPP packages
%%     2) CommandStatus: integer()  - the command status value of the PDU
%%     3) SequnceNumber: integer()  - the sequnce number of the PDU
%% Returns:
%%     1) the binary sequnce, containing the encoded PDU
%% @end

pack_pdu(Body, CommandStatus, SequenceNumber) ->
    %% 3.2 SMPP PDU Format - Overview
    %% The SMPP Header is a mandatory part of every SMPP PDU and must always be present. The
    %% SMPP PDU Body is optional and may not be included with every SMPP PDU.
    {CommandId, PackedBody} = pack_body(Body, CommandStatus, SequenceNumber),
    CommandLength = byte_size(PackedBody) + 16,
    <<CommandLength:     32/big-unsigned-integer, 
      CommandId:         32/big-unsigned-integer, 
      CommandStatus:     32/big-unsigned-integer, 
      SequenceNumber:    32/big-unsigned-integer, 
      PackedBody/binary>>.



%% @doc 
%% Packs the body of the PDU
%% Arguments:
%%     1) Body:          pdu_body() - the record, containing the PDU body
%%     2) CommandStatus: integer()  - the command status value of the PDU
%%     3) SequnceNumber: integer()  - the sequnce number of the PDU
%% Returns:
%%     1) the tuple of two elements:
%%       1.2) the code of the PDU
%%       1.1) the binary sequnce, containing the encoded PDU body
%% @end

%% 4.1 "BIND" Operation

pack_body(#bind_transmitter{} = Body, _CommandStatus, _SequenceNumber) ->
    {?bind_transmitter, esmpp34raw_bind_transmitter:pack(Body)};

pack_body(#bind_transmitter_resp{} = Body, CommandStatus, _SequenceNumber) ->
    %% 4.1.2 "BIND_TRANSMITTER_RESP" Syntax
    %% The body portion of the SMPP bind_transmitter_resp PDU is not returned
    %% if the command_status field contains a non-zero value;
    case CommandStatus of
	?ESME_ROK  -> {?bind_transmitter_resp, esmpp34raw_bind_transmitter_resp:pack(Body)};
	_ErrorCode -> {?bind_transmitter_resp, <<>>}
    end;

pack_body(#bind_receiver{} = Body, _CommandStatus, _SequenceNumber) ->
    {?bind_receiver, esmpp34raw_bind_receiver:pack(Body)};

pack_body(#bind_receiver_resp{} = Body, CommandStatus, _SequenceNumber) ->
    %% 4.1.4 "BIND_RECEIVER_RESP" Syntax
    %% The bind_receiver_resp PDU Body is not returned if the command_status field
    %% contains a non-zero value.
    case CommandStatus of
	?ESME_ROK  -> {?bind_receiver_resp, esmpp34raw_bind_receiver_resp:pack(Body)};
	_ErrorCode -> {?bind_receiver_resp, <<>>}
    end;

pack_body(#bind_transceiver{} = Body, _CommandStatus, _SequenceNumber) ->
    {?bind_transceiver, esmpp34raw_bind_transceiver:pack(Body)};

pack_body(#bind_transceiver_resp{} = Body, _CommandStatus, _SequenceNumber) ->
    {?bind_transceiver_resp, esmpp34raw_bind_transceiver_resp:pack(Body)};

pack_body(#outbind{} = Body, _CommandStatus, _SequenceNumber) ->
    {?outbind, esmpp34raw_outbind:pack(Body)};

%% 4.2 "UNBIND" Operation

pack_body(#unbind{} = _Body, _CommandStatus, _SequenceNumber) ->
    {?unbind, <<>>};

pack_body(#unbind_resp{} = _Body, _CommandStatus, _SequenceNumber) ->
    {?unbind_resp, <<>>};

%% 4.3 "GENERIC_NACK" PDU

pack_body(#generic_nack{} = _Body, _CommandStatus, _SequenceNumber) ->
    {?generic_nack, <<>>};

%% 4.4 "SUBMIT_SM" Operation

pack_body(#submit_sm{} = Body, _CommandStatus, _SequenceNumber) ->
    {?submit_sm, esmpp34raw_submit_sm:pack(Body)};

pack_body(#submit_sm_resp{} = Body, CommandStatus, _SequenceNumber) ->
    %% 4.2.2 "SUBMIT_SM_RESP" Syntax
    %% The submit_sm_resp PDU Body is not returned if the command_status field contains
    %% a non-zero value.
    case CommandStatus of
	?ESME_ROK  -> {?submit_sm_resp, esmpp34raw_submit_sm_resp:pack(Body)};
	_ErrorCode -> {?submit_sm_resp, <<>>}
    end;

%% 4.5 "SUBMIT_MULTI" Operation

pack_body(#submit_multi{} = Body, _CommandStatus, _SequenceNumber) ->
    {?submit_multi, esmpp34raw_submit_multi:pack(Body)};

pack_body(#submit_multi_resp{} = Body, _CommandStatus, _SequenceNumber) ->
    {?submit_multi_resp, esmpp34raw_submit_multi_resp:pack(Body)};

%% 4.6 "DELIVER_SM" Operation

pack_body(#deliver_sm{} = Body, _CommandStatus, _SequenceNumber) ->
    {?deliver_sm, esmpp34raw_deliver_sm:pack(Body)};

pack_body(#deliver_sm_resp{} = Body, _CommandStatus, _SequenceNumber) ->
    {?deliver_sm_resp, esmpp34raw_deliver_sm_resp:pack(Body)};

%% 4.7 "DATA_SM" Operation

pack_body(#data_sm{} = Body, _CommandStatus, _SequenceNumber) ->
    {?data_sm, esmpp34raw_data_sm:pack(Body)};

pack_body(#data_sm_resp{} = Body, _CommandStatus, _SequenceNumber) ->
    {?data_sm_resp, esmpp34raw_data_sm_resp:pack(Body)};

%% 4.8 "QUERY_SM" Operation

pack_body(#query_sm{} = Body, _CommandStatus, _SequenceNumber) ->
    {?query_sm, esmpp34raw_query_sm:pack(Body)};

pack_body(#query_sm_resp{} = Body, _CommandStatus, _SequenceNumber) ->
    {?query_sm_resp, esmpp34raw_query_sm_resp:pack(Body)};

%% 4.9 "CANCEL_SM" Operation

pack_body(#cancel_sm{} = Body, _CommandStatus, _SequenceNumber) ->
    {?cancel_sm, esmpp34raw_cancel_sm:pack(Body)};

pack_body(#cancel_sm_resp{} = _Body, _CommandStatus, _SequenceNumber) ->
    {?query_sm_resp, <<>>};

%% 4.10 "REPLACE_SM" Operation

pack_body(#replace_sm{} = Body, _CommandStatus, _SequenceNumber) ->
    {?replace_sm, esmpp34raw_replace_sm:pack(Body)};

pack_body(#replace_sm_resp{} = _Body, _CommandStatus, _SequenceNumber) ->
    {?replace_sm_resp, <<>>};

%% 4.11 "ENQUIRE_LINK" Operation

pack_body(#enquire_link{} = _Body, _CommandStatus, _SequenceNumber) ->
    {?enquire_link, <<>>};

pack_body(#enquire_link_resp{} = _Body, _CommandStatus, _SequenceNumber) ->
    {?enquire_link_resp, <<>>};

%% 4.12 "ALERT_NOTIFICATION" Operation

pack_body(#alert_notification{} = Body, _CommandStatus, _SequenceNumber) ->
    {?alert_notification, esmpp34raw_alert_notification:pack(Body)}.



%% @doc 
%% Unpacks the body of the PDU from the binary sequence
%% Arguments:
%%     1) Type:          integer() - the type of the PDU
%%     2) CommandStatus: integer() - the command status value of the PDU
%%     3) SequnceNumber: integer() - the sequnce number of the PDU
%%     4) Stream:        binary()  - the binary sequence to be decoded
%% Returns:
%%     1) pdu_body() - the record, which is the body of the decoded PDU
%% @end

%% 4.1 "BIND" Operation

unpack_body(?bind_transmitter, _CommandStatus, _SequenceNumber, Stream) ->
    esmpp34raw_bind_transmitter:unpack(Stream);

unpack_body(?bind_transmitter_resp, CommandStatus, _SequenceNumber, Stream) ->
    %% 4.1.2 "BIND_TRANSMITTER_RESP" Syntax
    %% The body portion of the SMPP bind_transmitter_resp PDU is not returned
    %% if the command_status field contains a non-zero value;
    case CommandStatus of
	?ESME_ROK  -> esmpp34raw_bind_transmitter_resp:unpack(Stream);
	_ErrorCode -> #bind_transmitter_resp{}
    end;

unpack_body(?bind_receiver, _CommandStatus, _SequenceNumber, Stream) ->
    esmpp34raw_bind_receiver:unpack(Stream);

unpack_body(?bind_receiver_resp, CommandStatus, _SequenceNumber, Stream) ->
    %% 4.1.4 "BIND_RECEIVER_RESP" Syntax
    %% The bind_receiver_resp PDU Body is not returned if the command_status field
    %% contains a non-zero value.
    case CommandStatus of
	?ESME_ROK  -> esmpp34raw_bind_receiver_resp:unpack(Stream);
	_ErrorCode -> #bind_receiver_resp{}
    end;

unpack_body(?bind_transceiver, _CommandStatus, _SequenceNumber, Stream) ->
    esmpp34raw_bind_transceiver:unpack(Stream);

unpack_body(?bind_transceiver_resp, _CommandStatus, _SequenceNumber, Stream) ->
    %% TODO: check, if treansceiver body can be scipped on error, like in receiver_resp ot transmitter_resp
    %% SMPP standard does not specify this, but it would be logically
    esmpp34raw_bind_transceiver_resp:unpack(Stream);

unpack_body(?outbind, _CommandStatus, _SequenceNumber, Stream) ->
    esmpp34raw_outbind:unpack(Stream);

%% 4.2 "UNBIND" Operation

unpack_body(?unbind, _CommandStatus, _SequenceNumber, _Stream) ->
    #unbind{};

unpack_body(?unbind_resp, _CommandStatus, _SequenceNumber, _Stream) ->
    #unbind_resp{};

%% 4.3 "GENERIC_NACK" PDU

unpack_body(?generic_nack, _CommandStatus, _SequenceNumber, _Stream) ->
    #generic_nack{};

%% 4.4 "SUBMIT_SM" Operation

unpack_body(?submit_sm, _CommandStatus, _SequenceNumber, Stream) ->
    esmpp34raw_submit_sm:unpack(Stream);

unpack_body(?submit_sm_resp, CommandStatus, _SequenceNumber, Stream) ->
    %% 4.2.2 "SUBMIT_SM_RESP" Syntax
    %% The submit_sm_resp PDU Body is not returned if the command_status field contains
    %% a non-zero value.
    case CommandStatus of
	?ESME_ROK  -> esmpp34raw_submit_sm_resp:unpack(Stream);
	_ErrorCode -> #submit_sm_resp{}
    end;

%% 4.5 "SUBMIT_MULTI" Operation

unpack_body(?submit_multi, _CommandStatus, _SequenceNumber, Stream) ->
    esmpp34raw_submit_multi:unpack(Stream);

unpack_body(?submit_multi_resp, _CommandStatus, _SequenceNumber, Stream) ->
    esmpp34raw_submit_multi_resp:unpack(Stream);

%% 4.6 "DELIVER_SM" Operation

unpack_body(?deliver_sm, _CommandStatus, _SequenceNumber, Stream) ->
    esmpp34raw_deliver_sm:unpack(Stream);

unpack_body(?deliver_sm_resp, _CommandStatus, _SequenceNumber, Stream) ->
    esmpp34raw_deliver_sm_resp:unpack(Stream);

%% 4.7 "DATA_SM" Operation

unpack_body(?data_sm, _CommandStatus, _SequenceNumber, Stream) ->
    esmpp34raw_data_sm:unpack(Stream);

unpack_body(?data_sm_resp, _CommandStatus, _SequenceNumber, Stream) ->
    esmpp34raw_data_sm_resp:unpack(Stream);

%% 4.8 "QUERY_SM" Operation

unpack_body(?query_sm, _CommandStatus, _SequenceNumber, Stream) ->
    esmpp34raw_query_sm:unpack(Stream);

unpack_body(?query_sm_resp, _CommandStatus, _SequenceNumber, Stream) ->
    esmpp34raw_query_sm_resp:unpack(Stream);

%% 4.9 "CANCEL_SM" Operation

unpack_body(?cancel_sm, _CommandStatus, _SequenceNumber, Stream) ->
    esmpp34raw_cancel_sm:unpack(Stream);

unpack_body(?cancel_sm_resp, _CommandStatus, _SequenceNumber, _Stream) ->
    #cancel_sm_resp{};

%% 4.10 "REPLACE_SM" Operation

unpack_body(?replace_sm, _CommandStatus, _SequenceNumber, Stream) ->
    esmpp34raw_replace_sm:unpack(Stream);

unpack_body(?replace_sm_resp, _CommandStatus, _SequenceNumber, _Stream) ->
    #replace_sm_resp{};

%% 4.11 "ENQUIRE_LINK" Operation

unpack_body(?enquire_link, _CommandStatus, _SequenceNumber, _Stream) ->
    #enquire_link{};

unpack_body(?enquire_link_resp, _CommandStatus, _SequenceNumber, _Stream) ->
    #enquire_link_resp{};

%% 4.12 "ALERT_NOTIFICATION" Operation

unpack_body(?alert_notification, _CommandStatus, _SequenceNumber, Stream) ->
    esmpp34raw_alert_notification:unpack(Stream);

%% UNKNOWN

unpack_body(_Unknown, _CommandStatus, _SequenceNumber, <<_Body/binary>>) ->
    throw(unknown_pdu).



