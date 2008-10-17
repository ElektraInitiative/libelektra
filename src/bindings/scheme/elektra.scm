;; elektra.scm -*- scheme48-package: elektra -*-

(define %lib (ffi-lib "libelektra" '("2")))

(define-values (_size_t _ssize_t)
  (case sizeof:size_t
    ((2) (values _uint16 _int16))
    ((4) (values _uint32 _int32))
    ((8) (values _uint64 _int64))))

(define (uint-type size)
  (case size ((2) _uint16) ((4) _uint32) ((8) _uint64)))

(define (int-type size)
  (case size ((2) _int16) ((4) _int32) ((8) _int64)))

(define _uid_t (uint-type sizeof:uid_t))
(define _gid_t (uint-type sizeof:gid_t))
(define _mode_t (uint-type sizeof:mode_t))
(define _time_t (int-type sizeof:time_t)) ;; Assumes time_t is a signed integer

(define (bytes->string bytes)
  (let ((len (- (u8vector-length bytes) 1)))
    (do ((s (make-string len))
         (i 0 (+ i 1)))
        ((>= i len) s)
      (string-set! s i (ascii->char (u8vector-ref bytes i))))))

(define (assq-ref alist key)
  (cond ((assq key alist) => cdr)
        (else #f)))

;;; Error handling

(define-condition-type &elektra-error &error
  elektra-error?
  (code elektra-error-code))

(define (make-raiser pred)
  (lambda (kdb val)
    (if (pred val)
        (if kdb
            (let ((errno (kdb-get-errno kdb)))
              (raise (condition (&elektra-error (code errno))
                                (&message (message (kdb-str-error errno))))))
            (raise (condition (&elektra-error (code #f))))))))

(define raise-on-neg (make-raiser (lambda (val) (< val 0))))
(define raise-on-false (make-raiser (lambda (val) (not val))))
(define raise-on-zero (make-raiser (lambda (val) (zero? val))))
(define raise-on-nonzero (make-raiser (lambda (val) (not (zero? val)))))

;;; Flags handling

(define (flags->integer alist flags)
  (fold (lambda (x n) (+ n (assq-ref alist x))) 0 flags))

(define (integer->flags alist n)
  (let loop ((n n) (result '()) (opts alist))
    (cond ((or (zero? n) (null? opts))
           result)
          ((zero? (bitwise-and (cdar opts) n))
           (loop n result (cdr opts)))
          (else
           (loop (- n (bitwise-and (cdar opts)))
                 (cons (caar opts) result)
                 (cdr opts))))))

;;; Data structures

(define-record-type kdb
  (make-kdb handle)
  kdb?
  (handle kdb-handle))

(define _kdb (make-ctype _pointer kdb-handle make-kdb))

(define-record-type ks
  (make-ks handle)
  ks?
  (handle ks-handle))

(define _ks (make-ctype _pointer ks-handle make-ks))

(define-record-type key
  (make-key handle)
  key?
  (handle key-handle))

(define-record-discloser key
  (lambda (key)
    `(key ,(key-get-name key))))

(define _key (make-ctype _pointer key-handle make-key))

(define kdb-options-alist `((no-recursive . ,KDB_O_NORECURSIVE)
                            (excl-dir . ,KDB_O_EXCLDIR)
                            (dir-only . ,KDB_O_DIRONLY)
                            (stat-only . ,KDB_O_STATONLY)
                            (inactive . ,KDB_O_INACTIVE)
                            (unsorted . ,KDB_O_NOSORT)
                            (nfollow-link . ,KDB_O_NFOLLOWLINK)
                            (condensed . ,KDB_O_CONDENSED)
                            (numbers . ,KDB_O_NUMBERS)
                            (xml-headers . ,KDB_O_XMLHEADERS)
                            (full-name . ,KDB_O_FULLNAME)
                            (full-ugid . ,KDB_O_FULLUGID)
                            (hierarchy . ,KDB_O_HIER)
                            (no-case . ,KDB_O_NOCASE)
                            (no-span-parent . ,KDB_O_NOSPANPARENT)
                            (all . ,KDB_O_ALL)))

(define (kdb-options->ulong options) (flags->integer kdb-options-alist options))
(define (ulong->kdb-options n)       (integer->flags kdb-options-alist n))

(define _kdb_options (make-ctype _ulong kdb-options->ulong ulong->kdb-options))

(define key-switch-alist `((type . ,KEY_SWITCH_TYPE)
                           (name . ,KEY_SWITCH_NAME)
                           (value . ,KEY_SWITCH_VALUE)
                           (owner . ,KEY_SWITCH_OWNER)
                           (comment . ,KEY_SWITCH_COMMENT)
                           (uid . ,KEY_SWITCH_UID)
                           (gid . ,KEY_SWITCH_GID)
                           (mode . ,KEY_SWITCH_MODE)
                           (needsync . ,KEY_SWITCH_NEEDSYNC)))

(define (key-switches->integer switches) (flags->integer key-switch-alist switches))
(define (integer->key-switches n)        (integer->flags key-switch-alist n))

(define _key_switches (make-ctype _uint32 key-switches->integer integer->key-switches))

;;; KDB methods
(define kdb-open (get-ffi-obj "kdbOpen" %lib
                              (_fun -> (handle : _pointer)
                                    -> (let ((kdb (make-kdb handle)))
                                         (raise-on-false kdb handle)
                                         kdb))))
(define kdb-close (get-ffi-obj "kdbClose" %lib
                               (_fun (kdb : _kdb)
                                     -> (retval : _int) 
                                     -> (raise-on-neg kdb retval))))

(define kdb-get-errno (get-ffi-obj "kdbGetErrno" %lib (_fun _kdb -> _int)))
(define kdb-str-error (get-ffi-obj "kdbStrError" %lib (_fun _int -> _string)))

(define kdb-get-key! (get-ffi-obj "kdbGetKey" %lib
                                  (_fun (kdb : _kdb) _key -> (rv : _int) -> (raise-on-neg kdb rv))))

(define kdb-get-key-child-keys! (get-ffi-obj "kdbGetKeyChildKeys" %lib
                                             (_fun (kdb : _kdb) _key _ks _kdb_options
                                                   -> (rv : _ssize_t)
                                                   -> (raise-on-neg kdb rv))))

(define kdb-set-key (get-ffi-obj "kdbSetKey" %lib
                                 (_fun (kdb : _kdb) _key
                                       -> (rv : _int)
                                       -> (raise-on-nonzero kdb rv))))
(define kdb-set-keys (get-ffi-obj "kdbSetKeys" %lib
                                  (_fun (kdb : _kdb) _ks
                                        -> (rv : _int)
                                        -> (raise-on-nonzero kdb rv))))

(define kdb-remove-key (get-ffi-obj "kdbRemoveKey" %lib
                                    (_fun (kdb : _kdb) _key
                                          -> (rv : _int)
                                          -> (raise-on-nonzero kdb rv))))
(define kdb-rename (get-ffi-obj "kdbRename" %lib
                                (_fun (kdb : _kdb) _key _string
                                      -> (rv : _int)
                                      -> (raise-on-nonzero kdb rv))))
(define kdb-link (get-ffi-obj "kdbLink" %lib
                              (_fun (kdb : _kdb) _string _string
                                    -> (rv : _int)
                                    -> (raise-on-nonzero kdb rv))))
(define kdb-stat-key! (get-ffi-obj "kdbStatKey" %lib
                                   (_fun (kdb : _kdb) _key
                                         -> (rv : _int)
                                         -> (zero? rv))))

;;; KS methods
(define ks-new (get-ffi-obj "ksNew" %lib (_fun -> _ks)))
(define ks-del (get-ffi-obj "ksDel" %lib (_fun _ks -> _void)))
(define ks-next! (get-ffi-obj "ksNext" %lib (_fun _ks
                                                  -> (rv :  _pointer)
                                                  -> (and rv (make-key rv)))))
(define ks-current (get-ffi-obj "ksCurrent" %lib (_fun _ks -> _key)))
(define ks-rewind! (get-ffi-obj "ksRewind" %lib (_fun _ks -> _void)))
(define ks-head (get-ffi-obj "ksHead" %lib (_fun _ks -> _key)))
(define ks-tail (get-ffi-obj "ksTail" %lib (_fun _ks -> _key)))
(define ks-lookup-by-name (get-ffi-obj "ksLookupByName" %lib
                                       (_fun _ks _string (flags : _ulong = 0)
                                             -> (rv : _pointer)
                                             -> (and rv (make-key rv)))))
(define ks-lookup-by-string (get-ffi-obj "ksLookupByString" %lib
                                         (_fun _ks _string _size_t (flags : _ulong = 0)
                                               -> (rv : _pointer)
                                               -> (and rv (make-key rv)))))
(define ks-get-size (get-ffi-obj "ksGetSize" %lib (_fun _ks -> _ssize_t)))
(define ks-append! (get-ffi-obj "ksAppend" %lib (_fun _ks _key -> _ssize_t)))
(define ks-append-keys! (get-ffi-obj "ksAppend" %lib (_fun _ks _ks -> _ssize_t)))
(define ks-insert! (get-ffi-obj "ksInsert" %lib (_fun _ks _key -> _ssize_t)))
(define ks-insert-keys! (get-ffi-obj "ksInsertKeys" %lib (_fun _ks _ks -> _ssize_t)))
(define ks-pop! (get-ffi-obj "ksPop" %lib (_fun _ks -> (rv : _pointer) -> (and rv (make-key rv)))))
(define ks-pop-last! (get-ffi-obj "ksPop" %lib
                                  (_fun _ks -> (rv : _pointer) -> (and rv (make-key rv)))))
(define ks-compare! (get-ffi-obj "ksCompare" %lib (_fun _ks _ks _ks -> _int)))
(define ks-sort! (get-ffi-obj "ksSort" %lib (_fun _ks -> _void)))

;;; Key methods
(define %key-new (get-ffi-obj "keyNew" %lib (_fun (x : _pointer = (and #f)) -> _key)))
(define (key-new properties)
  (let ((key (%key-new)))
    (let loop ((props properties))
      (cond ((null? props) key)
            (else
             (case (caar props)
               ((name) (key-set-name! key (cadar props)))
               ((value) (key-set-value! key (cadar props)))
               ((link) (key-set-link! key (cadar props)))
               ((owner) (key-set-owner! key (cadar props)))
               ((mode) (key-set-mode! key (cadar props)))
               ((uid) (key-set-uid! key (cadar props)))
               ((gid) (key-set-gid! key (cadar props)))
               (else
                (error "invalid key property" (car props))))
             (loop (cdr props)))))))

(define key-del (get-ffi-obj "keyDel" %lib (_fun _key -> _void)))
(define key-set-name! (get-ffi-obj "keySetName" %lib
                                  (_fun _key _string -> (rv : _ssize_t)
                                        -> (raise-on-zero #f rv))))
(define key-add-basename! (get-ffi-obj "keyAddBaseName" %lib
                                  (_fun _key _string -> (rv : _ssize_t)
                                        -> (raise-on-neg #f rv))))
(define key-set-basename! (get-ffi-obj "keySetBaseName" %lib
                                  (_fun _key _string -> (rv : _ssize_t)
                                        -> (raise-on-neg #f rv))))
(define key-get-name (get-ffi-obj "keyName" %lib (_fun _key -> _string)))
(define (make-get-string-proc size-fun get-fun)
  (let ((get-size (get-ffi-obj size-fun %lib
                               (_fun _key -> _ssize_t))))
    (get-ffi-obj get-fun %lib
                 (_fun (key : _key)
                       (name : _pointer = (malloc (get-size key)))
                       (size : _size_t = (get-size key))
                       -> (rv : _ssize_t) -> (bytes->string name)))))

(define key-get-full-name (make-get-string-proc "keyGetFullNameSize" "keyGetFullName"))

(define key-name-is-user? (get-ffi-obj "keyNameIsUser" %lib
                                       (_fun _string -> (rv : _int)
                                             -> (not (zero? rv)))))

(define key-name-is-system? (get-ffi-obj "keyNameIsSystem" %lib
                                         (_fun _string -> (rv : _int)
                                               -> (not (zero? rv)))))

(define key-get-root-name (make-get-string-proc "keyGetRootNameSize"
                                              "keyGetRootName"))
(define key-get-full-root-name (make-get-string-proc "keyGetFullRootNameSize"
                                                   "keyGetFullRootName"))
(define key-get-parent-name (make-get-string-proc "keyGetParentNameSize"
                                                "keyGetParentName"))
(define key-get-base-name (get-ffi-obj "keyBaseName" %lib (_fun _key -> _string)))

(define key-set-string! (get-ffi-obj "keySetString" %lib (_fun _key _string -> _ssize_t)))
(define key-get-string (make-get-string-proc "keyGetValueSize" "keyGetString"))

(define key-get-value key-get-string)
(define (key-set-value! key value)
  (cond ((string? value) (key-set-string! key value))
        (else "Invalid value type")))

(define key-set-link! (get-ffi-obj "keySetLink" %lib (_fun _key _string -> _ssize_t)))
(define key-get-link (make-get-string-proc "keyGetLinkSize" "keyGetLink"))

(define key-set-owner! (get-ffi-obj "keySetOwner" %lib (_fun _key _string -> _ssize_t)))
(define key-get-owner (get-ffi-obj "keyOwner" %lib (_fun _key -> _string)))

(define key-set-comment! (get-ffi-obj "keySetComment" %lib (_fun _key _string -> _ssize_t)))
(define key-get-comment (get-ffi-obj "keyComment" %lib (_fun _key -> _string)))

(define key-set-uid! (get-ffi-obj "keySetUID" %lib
                                  (_fun _key _uid_t -> (rv :  _int)
                                        -> (raise-on-nonzero #f rv))))
(define key-get-uid (get-ffi-obj "keyGetUID" %lib (_fun _key -> _uid_t)))

(define key-set-gid! (get-ffi-obj "keySetGID" %lib
                                  (_fun _key _gid_t -> (rv :  _int)
                                        -> (raise-on-nonzero #f rv))))
(define key-get-gid (get-ffi-obj "keyGetGID" %lib (_fun _key -> _gid_t)))

(define key-set-mode! (get-ffi-obj "keySetMode" %lib (_fun _key _mode_t -> _int)))
(define key-get-mode (get-ffi-obj "keyGetMode" %lib (_fun _key -> _mode_t)))
(define key-set-u-mode! (get-ffi-obj "keySetUMode" %lib (_fun _key _mode_t -> _int)))

(define key-get-mtime (get-ffi-obj "keyGetMTime" %lib (_fun _key -> _time_t)))
(define key-get-atime (get-ffi-obj "keyGetATime" %lib (_fun _key -> _time_t)))
(define key-get-ctime (get-ffi-obj "keyGetCTime" %lib (_fun _key -> _time_t)))

; (define key-set-flag! (get-ffi-obj "keySetFlag" %lib (_fun _key -> _int)))
; (define key-clear-flag! (get-ffi-obj "keyClearFlag" %lib (_fun _key -> _int)))
; (define key-get-flag (get-ffi-obj "keyGetFlag" %lib (_fun _key -> _int)))

(define key-get-namespace (get-ffi-obj "keyGetNamespace" %lib (_fun _key -> _int)))

(define (key-predicate name)
  (get-ffi-obj name %lib (_fun _key -> (rv : _int) -> (not (zero? rv)))))

(define key-is-system? (key-predicate "keyIsSystem"))
(define key-is-user? (key-predicate "keyIsUser"))
(define key-is-link? (key-predicate "keyIsLink"))
(define key-is-dir? (key-predicate "keyIsDir"))
(define key-is-bin? (key-predicate "keyIsBin"))
(define key-is-string? (key-predicate "keyIsString"))
(define key-needs-sync? (key-predicate "keyNeedSync"))
(define key-compare (get-ffi-obj "keyCompare" %lib (_fun _key _key -> _key_switches)))
