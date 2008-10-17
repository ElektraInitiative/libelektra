;; ,open srfi-1 elektra elektra-tools spells.assert
;;

(define (test/key-new)
  ;; empty key
  (let ((key (key-new '())))
    (assert (equal? (key-get-name key) #f))
    (key-del key))

  ;; key with name
  (let ((key (key-new '((name "system/sw/test")))))
    (assert (equal? (key-get-name key) "system/sw/test"))
    (key-del key))

  ;; key with name and value
  (let ((key (key-new '((name "system/sw/test")
                        (value "test")))))
    (assert (key-is-string? key))
    (assert (equal? (key-get-string key) "test"))
    (key-del key))

  ;; key with name + UID/GID
  (let ((key (key-new '((name "system/sw/test") (uid 123) (gid 456)))))
    (assert (equal? (key-get-uid key) 123))
    (assert (equal? (key-get-gid key) 456))
    (key-del key))

  ;; key with name + owner
  (let ((key (key-new '((name "user/test/test") (owner "yl")))))
    (assert (equal? (key-get-owner key) "yl"))
    (key-del key))

  (let ((ks (ks-new)))
    (ks-append! ks (key-new '()))
    (ks-append! ks (key-new '((name "user/sw"))))
    (ks-append! ks (key-new '((name "user/tmp/ex1") (value "some data"))))
    (ks-append! ks (key-new '((name "user/tmp/ex2") (value "some data") (mode #o777) )))
    (ks-append! ks (key-new '((name "user/tmp/ex3") (link "system/mtp/x") (mode #o654))))

    (ks-rewind! ks)

    (let ((key (ks-next! ks)))
      (assert key))
    (let ((key (ks-next! ks)))
      (assert (equal? (key-get-name key) "user/sw")))
    (let ((key (ks-next! ks)))
      (assert (equal? (key-get-name key) "user/tmp/ex1"))
      (assert (equal? (key-get-value key) "some data")))
    (let ((key (ks-next! ks)))
      (assert (equal? (key-get-name key) "user/tmp/ex2"))
      (assert (equal? (key-get-value key) "some data"))
      (assert (equal? (key-get-mode key) #o777)))
    
    (ks-del ks))

  (let ((k1 (key-new '((name "user/1") (value "singlevalue"))))
        (k2 (key-new '((name "user/2") (value "thevalue"))))
        (k3 (key-new '((name "user/3") (value "syskey")))))
    (assert (key-is-string? k1))
    (assert (equal? (key-get-value k1) "singlevalue"))

    (assert (key-is-string? k2))
    (assert (equal? (key-get-value k2) "thevalue"))

    (assert (key-is-string? k3))
    (assert (equal? (key-get-value k3) "syskey"))

    (for-each key-del (list k1 k2 k3))))

(define (delete-keys-recurse kdb root)
  (let ((key (key-new `((name ,root)))))
    (let ((ks (ks-new)))
      ;; fetch all _non_-directory keys
      (kdb-get-key-child-keys! kdb key ks '())
      ;; delete these ...
      (ks-rewind! ks)
      (do ((cur (ks-pop! ks) (ks-pop! ks)))
          ((not cur))
        (kdb-remove-key kdb cur))
      (ks-del ks))
    
    (let ((ks (ks-new)))
      ;; fetch all directory keys
      (kdb-get-key-child-keys! kdb key ks '())
      ;; delete these ...
      (do ((cur (ks-pop-last! ks) (ks-pop-last! ks)))
          ((not cur))
        (kdb-remove-key kdb cur))
      (ks-del ks))

    (key-del key)))

(define (new-test-root kdb)
  (let loop ((i 0))
    (let* ((name (string-append "user/elektra-tests/" (number->string i)))
           (key (key-new `((name ,name))))
           (stat (kdb-stat-key! kdb key)))
      (key-del key)
      (if (not stat)
          name
          (loop (+ i 1))))))

(define (test/key-compare k1 k2 ignore-flags)
  (let ((result (key-compare k1 k2)))
    (assert (lset<= result ignore-flags))))

(define (test/kdb-set-key-and-get-key kdb key)
  (let ((tmp (key-new `((name ,(key-get-name key))))))
    (kdb-set-key kdb key)
    (kdb-get-key! kdb tmp)
    (test/key-compare tmp key (if (key-is-dir? key) '(value) '()))
    (key-del tmp)))

(define (test/kdb-stat-key kdb key)
  (let ((tmp (key-new `((name ,(key-get-name key))))))
    (define (assert-equal getter)
      (assert (equal? (getter key) (getter tmp))))
    (kdb-get-key! kdb key)
    (for-each display `("stat:ing " ,(key-get-name tmp) #\newline))
    (kdb-get-key! kdb tmp)
    (assert-equal key-get-uid)
    (assert-equal key-get-gid)
    (assert-equal key-get-mode)
    (assert-equal key-get-atime)
    (assert-equal key-get-mtime)
    (assert-equal key-get-ctime)
    (key-del tmp)))

;; Rename the tested key to <keyname>-renamed.
;; Then check if <keyname> is removed & <keyname>-renamed
;; exists.
;;
;; NOTE: The tested key must exist.
;; At the end, the key is renamed to its original name
(define (test/kdb-rename kdb key)
  (let* ((new-name (string-append (key-get-name key) "-renamed"))
         (tmp (key-new `((name ,new-name)))))
    (kdb-rename kdb key new-name)
    (kdb-get-key! kdb tmp)
    (assert (not (kdb-stat-key! kdb key)))
    (kdb-rename kdb tmp (key-get-name key))
    (key-del tmp)))

(define (test/kdb-link kdb key)
  (let ((ignore-flags (append '(name type) (if (key-is-dir? key) '(value) '())))
        (tmp (key-new `((name (string-append (key-get-name key) "-linked"))))))
    (kdb-link kdb (key-get-name key) (key-get-name tmp))
    (assert (kdb-stat-key! kdb tmp))
    (assert (equal? (key-get-value tmp) (key-get-name key)))
    (kdb-get-key! kdb key)
    (kdb-get-key! kdb tmp)
    (test/key-compare key tmp ignore-flags)
    (kdb-remove-key kdb tmp)
    (key-del tmp)))

(define (main args)
  (test/key-new)
  
  (let* ((kdb (kdb-open))
         (root (new-test-root kdb))
         (ks (ks-new)))
    (ks-from-xml-file! ks "key.xml")
    (ks-rewind! ks)
    (do ((cur (ks-next! ks) (ks-next! ks)))
        ((not cur))
      (for-each display `("Testing " ,(key-get-comment cur) "..."))
      (test/kdb-set-key-and-get-key kdb cur)
      (test/kdb-stat-key kdb cur)
      ;;(test/kdb-rename kdb cur)
      ;;(test/kdb-link kdb cur)
      (newline))
    ;;(delete-keys-recurse kdb )
    (kdb-close kdb)))
