(define %lib (ffi-lib "libelektratools" '("1")))

(define ks-from-xml-file! (get-ffi-obj "ksFromXMLfile" %lib
                                       (_fun _ks _string
                                             -> (rv : _int)
                                             -> (raise-on-nonzero #f rv))))
