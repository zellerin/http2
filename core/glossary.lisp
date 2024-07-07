(in-package #:http2)

(defsection @terms
    (:export nil
     :title "Glossary terms")
  "Everywhere in the documentation, HTTP stands for HTTP/2 as defined in the HTTP2-STANDARD."

  (http2-standard glossary-term)
  (http2-client glossary-term)
  (server glossary-term)
  (connection-endpoint glossary-term)
  (peer glossary-term)
  (connection-term glossary-term)
  (http2-stream glossary-term)
  (receiver glossary-term)
  (sender glossary-term)
  (connection-error glossary-term)
  (http2-stream-error glossary-term)
  (network-stream-term glossary-term))

(define-glossary-term http2-standard
    (:title "Standard")
    "[RFC9113](https://www.rfc-editor.org/rfc/rfc9113.html) is the standard for HTTP/2 that this library aims to implement. It obsoletes RFC7540.")

(define-glossary-term  http2-client
    (:title "HTTP client")
    "The endpoint that initiates an HTTP/2 connection.  Clients send HTTP
      requests and receive HTTP responses.")

(define-glossary-term connection-endpoint
    (:title "endpoint")
    "Either the client or server of the connection.")

(define-glossary-term peer
    (:title "peer")
    "When discussing a particular CONNECTION-ENDPOINT, peer refers to the endpoint
that is remote to the primary subject of discussion.")

(define-glossary-term connection-term
    (:title "HTTP connection")
    "A transport-layer connection between HTTP2-CLIENT and SERVER. This is the
    STANDARD documentation; I assume that they mean the TCP or TLS connection by
    that; though I am not sure TLS rates as tranport-layer.

    This is in the code implemented by NETWORK-STREAM-TERM and additional data
    in [HTTP2-CONNECTION][class]")

(define-glossary-term http2-stream
    (:title "HTTP stream")
    "A bidirectional flow of frames within the HTTP/2 connection.")

(define-glossary-term network-stream-term
    (:title "network stream")
    "The underlying stream (CL one) that the CONNECTION-TERM is established upon. The
HTTP2-STANDARD does not define this terms and explicitly assumes TCP or TLS
connection, but we use other stream for testing as well.")

(define-glossary-term frame
    ()
    "The smallest unit of communication within an HTTP/2 connection, consisting of a
header and a variable-length sequence of octets structured according to the
frame type. Each frame relates either to the CONNECTION-TERM as such or to a
particular HTTP2-STREAM.")

(define-glossary-term connection-error
    (:title "Connection error")
    "An error that affects the entire HTTP/2 connection. When an endpoint detects
[CONNECTION-ERROR][glossary-term], it notifies the PEER by ending GOAWAY frame and the CONNECTION-TERM is closed.

For handling detected errors, see function [CONNECTION-ERROR][function] and condition [CONNECTION-ERROR][condition].

For handling received GOAWAY frames, see generic function DO-GOAWAY and
condition GO-AWAY")

(define-glossary-term receiver () "An endpoint that is receiving frames.")

(define-glossary-term sender () "An endpoint that is transmitting frames.")

(define-glossary-term server
    (:title "HTTP server")
    "The endpoint that accepts an HTTP/2 connection.  Servers receive HTTP requests
and send HTTP responses.")

(define-glossary-term http2-stream-error
    (:title "HTTP stream error")
    "An error on the individual HTTP/2 stream. The section is still usable after it is detected.

See also function [HTTP-STREAM-ERROR][function] and condition
[HTTP-STREAM-ERROR][condition].")
