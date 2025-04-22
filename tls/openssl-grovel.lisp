(in-package #:http2/openssl)

(cc-flags "-I/opt/homebrew/opt/openssl@3/include")
(cc-flags "-L/opt/homebrew/opt/openssl@3/lib")

(include "openssl/ssl.h")
(constant (ssl-error-none "SSL_ERROR_NONE"))
(constant (ssl-error-want-write "SSL_ERROR_WANT_WRITE"))
(constant (ssl-error-want-read "SSL_ERROR_WANT_READ"))
(constant (ssl-error-ssl "SSL_ERROR_SSL"))
(constant (ssl-error-syscall "SSL_ERROR_SYSCALL"))
(constant (ssl-error-zero-return "SSL_ERROR_ZERO_RETURN"))

(constant (ssl-tlsext-err-ok "SSL_TLSEXT_ERR_OK"))
(constant (ssl-tlsext-err-alert-fatal "SSL_TLSEXT_ERR_ALERT_FATAL"))
(constant (ssl-tlsext-err-noack "SSL_TLSEXT_ERR_NOACK"))

(constant (ssl-filetype-pem "SSL_FILETYPE_PEM"))
(constant (tls-1.2-version "TLS1_2_VERSION"))
(constant (ssl-op-all "SSL_OP_ALL"))

(constant (ssl-ctrl-set-min-proto-version "SSL_CTRL_SET_MIN_PROTO_VERSION"))

(constant (bio-flags-should-retry "BIO_FLAGS_SHOULD_RETRY"))
