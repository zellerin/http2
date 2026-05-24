(mgl-pax:define-package #:http2/tests
  (:use #:cl #:fiasco #:http2/server #:http2/client #:mgl-pax))

(mgl-pax:define-package #:http2/tests/frames
  (:use #:cl #:fiasco #:http2/core #:mgl-pax
        #:http2/utils
        #:http2/tests/support)
  (:import-from #:http2/hpack #:compile-headers))

(mgl-pax:define-package #:http2/tests/headers
  (:use #:cl #:fiasco #:http2/core #:mgl-pax
        #:http2/utils
        #:http2/tests/support)
  (:import-from #:http2/core #:get-id-to-use #:get-error-code #:get-debug-data))

(in-package :http2/tests)

(defsection @overview ()
  "There is a test suite HTTP2/TESTS bound to the eponymous package, and there are
several subsets of tests below that. The general form for each subset is

```
(mgl-pax:define-package #:http2/tests/XXX
  (:use #:cl #:fiasco #:http2/XXX #:mgl-pax))

(in-package #:http2/tests/XXX)

(defsection @overview ()
 ...
)

(defsuite
    (http2/tests/utils :bind-to-package #:http2/tests/utils
                       :in http2/tests::http2/tests))
```
"
;  (http2/tests/utils::@overview section)
;  (http2/core::@test-errors section)
  )

(defsuite
    (http2/tests :bind-to-package #:http2/tests))
