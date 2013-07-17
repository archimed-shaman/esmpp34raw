esmpp34raw
==========

It is the raw encoder/decoder of the SMPP 3.4 stream. In spite of the influence of the smpp34pdu project (https://github.com/essiene/smpp34pdu), this library was developed "from stratch", according to the "Short Message Peer-to-Peer Protocol Specification v3.4".
This library is just a coder and decoder of the binary sequences and does not include any connection logic. Such functionality is planned as the separate project.

Version
-------
1.0.0 alpha

Usage
-----

<pre>
Transmitter = #bind_transmitter{system_id = "testid", password = "passwd", system_type = "VMS", interface_version = 16#34, addr_ton = 1, addr_npi = 1, address_range = "123"}.
BTransmitter = esmpp34raw_stream:pack_single(Transmitter, 0, 1).
esmpp34raw_stream:unpack_sequence(BTransmitter).
</pre>
