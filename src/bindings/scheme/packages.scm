;; packages.scm -*- scheme48-package: (config) -*-

(define-interface elektra-interface
  (export kdb-open
          kdb-close
          kdb-get-key!
          kdb-get-key-child-keys!
          kdb-set-keys
          kdb-set-key
          kdb-remove-key
          kdb-rename
          kdb-link
          kdb-stat-key!
          
          ks-new
          ks-del
          ks-next!
          ks-current
          ks-rewind!
          ks-head
          ks-tail
          ks-lookup-by-name
          ks-lookup-by-string
          ks-get-size
          ks-append!
          ks-append-keys!
          ks-pop!
          ks-pop-last!
          
          key-new
          key-del
          key-set-name!
          key-add-basename!
          key-set-basename!
          key-get-name
          key-get-full-name
          key-name-is-user?
          key-name-is-system?
          key-get-root-name
          key-get-full-root-name
          key-get-parent-name
          key-get-base-name

          key-set-string!
          key-get-string
          key-get-value
          key-set-value!
          
          key-set-link!
          key-get-link

          key-set-owner!
          key-get-owner
          key-set-comment!
          key-get-comment
          
          key-set-uid!
          key-get-uid
          key-set-gid!
          key-get-gid

          key-set-mode!
          key-get-mode
          key-set-u-mode!

          key-get-mtime
          key-get-atime
          key-get-ctime

;           key-set-flag!
;           key-clear-flag!
;           key-get-flag

          key-get-namespace
          key-is-system?
          key-is-user?
          key-is-link?
          key-is-dir?
          key-is-bin?
          key-is-string?
          key-needs-sync?
          
          key-compare))

(define-interface elektra-types-interface
  (export _kdb _key _ks
          raise-on-neg
          raise-on-false
          raise-on-zero
          raise-on-nonzero))

(define-interface elektra-tools-interface
  (export ks-from-xml-file!))

(define-structures ((elektra elektra-interface)
                    (elektra-types elektra-types-interface))
  (open scheme srfi-1
        spells.error
        spells.define-values
        spells.record-types
        spells.ascii spells.byte-vectors spells.bitwise spells.condition
        ffi)
  (files elektra-grovel elektra))

(define-structure elektra-tools elektra-tools-interface
  (open scheme
        ffi
        elektra-types)
  (files elektra-tools))
