(library (letloop http)

  (export http-error?
          http-error-message
          http-error-payload
          http-request-read
          http-request-write
          http-response-read
          http-response-write
          )

  (import (chezscheme))

  ;; letloop r999 without the tests
  (define-syntax define-record-type*
    (lambda (stx)
      (syntax-case stx ()
        ((_ <type>
            uid
            (constructor constructor-tag ...)
            predicate?
            (field-tag accessor setter ...) ...)

         (and (for-all identifier?
                       #'(<type> constructor constructor-tag ... predicate?
                                 field-tag ... accessor ... setter ... ...))
              (for-all (lambda (s) (<= 0 (length s) 1))
                       #'((setter ...) ...))
              (for-all (lambda (ct)
                         (memp (lambda (ft) (bound-identifier=? ct ft))
                               #'(field-tag ...)))
                       #'(constructor-tag ...)))
         (with-syntax (((field-clause ...)
                        (map (lambda (clause)
                               (if (= 2 (length clause))
                                   #`(immutable . #,clause)
                                   #`(mutable . #,clause)))
                             #'((field-tag accessor setter ...) ...)))
                       ((unspec-tag ...)
                        (remp (lambda (ft)
                                (memp (lambda (ct) (bound-identifier=? ft ct))
                                      #'(constructor-tag ...)))
                              #'(field-tag ...))))
                      #'(define-record-type (<type> constructor predicate?)
                          (nongenerative uid)
                          (protocol (lambda (ctor)
                                      (lambda (constructor-tag ...)
                                        (define unspec-tag) ...
                                        (ctor field-tag ...))))
                          (fields field-clause ...))))

        ((_ <type> (constructor constructor-tag ...)
            predicate?
            (field-tag accessor setter ...) ...)

         (and (for-all identifier?
                       #'(<type> constructor constructor-tag ... predicate?
                                 field-tag ... accessor ... setter ... ...))
              (for-all (lambda (s) (<= 0 (length s) 1))
                       #'((setter ...) ...))
              (for-all (lambda (ct)
                         (memp (lambda (ft) (bound-identifier=? ct ft))
                               #'(field-tag ...)))
                       #'(constructor-tag ...)))
         (with-syntax (((field-clause ...)
                        (map (lambda (clause)
                               (if (= 2 (length clause))
                                   #`(immutable . #,clause)
                                   #`(mutable . #,clause)))
                             #'((field-tag accessor setter ...) ...)))
                       ((unspec-tag ...)
                        (remp (lambda (ft)
                                (memp (lambda (ct) (bound-identifier=? ft ct))
                                      #'(constructor-tag ...)))
                              #'(field-tag ...))))
                      #'(define-record-type (<type> constructor predicate?)
                          ;; XXX: The following expression sets the
                          ;; record type unique identifier, hence it
                          ;; is preferable that all record types have
                          ;; a different <type>. It is a good idea I
                          ;; can't forsee cases where <type> must be
                          ;; <foobar> instead of <letloop-foobar>,
                          ;; except that record instances are
                          ;; representation in the REPL are slightly
                          ;; longer, and given the uid is not random
                          ;; it is also more readable.
                          (nongenerative <type>)
                          (protocol (lambda (ctor)
                                      (lambda (constructor-tag ...)
                                        (define unspec-tag) ...
                                        (ctor field-tag ...))))
                          (fields field-clause ...)))))))

  (define pk
    (lambda args
      (write args)(newline)
      (flush-output-port)
      (car (reverse args))))

  ;; <http-error>

  (define-record-type* <http-error>
    (make-http-error message payload)
    http-error?
    (message http-error-message)
    (payload http-error-payload))

  ;; helpers

  (define raise-unexpected-end-of-file
    (lambda ()
      (raise (make-http-error "Unexpected end of file" (list 'unexpected-end-of-file)))))

  (define raise-invalid
    (lambda (uid)
      (raise (make-http-error "Invalid" (list 'invalid uid)))))

  (define byte-space 32)
  (define byte-carriage-return 13)
  (define byte-linefeed 10) ;; aka. newline

  (define generator->list
    (lambda (generator)
      (let loop ((out '()))
        (let ((object (generator)))
          (if (eof-object? object)
              (reverse out)
              (loop (cons object out)))))))

  (define every
    (lambda (predicate? objects)
      (if (null? objects)
          #t
          (if (predicate? (car objects))
              (every predicate? (cdr objects))
              #f))))

  (define (bytevector-append . bvs)
    (assert (every bytevector? bvs))
    (let* ((total (apply fx+ (map bytevector-length bvs)))
           (out (make-bytevector total)))
      (let loop ((bvs bvs)
                 (index 0))
        (unless (null? bvs)
          (bytevector-copy! (car bvs) 0 out index (bytevector-length (car bvs)))
          (loop (cdr bvs) (fx+ index (bytevector-length (car bvs))))))
      out))

  (define generator->bytevector
    (lambda (generator n*)
      (lambda ()
        (if (fxzero? n*)
            (eof-object)
            (let loop ((n n*)
                       (out '()))
              (if (fxzero? n)
                  (begin (set! n* 0) (u8-list->bytevector (reverse out)))
                  (loop (fx- n 1) (cons (generator) out))))))))

  (define http-line-read
    (lambda (generator)
      (let loopx ((out '()))
        (let ((byte (generator)))
          (cond
           ((and (fx=? byte byte-linefeed) (fx=? (car out) byte-carriage-return))
            (reverse (cdr out)))
           ;; linefeed may not appear in the middle of request-line,
           ;; response-line, or header line.
           ((and (fx=? byte byte-linefeed) (not (fx=? (car out) byte-carriage-return)))
            (raise-invalid 1))
           ((eof-object? byte)
            (raise-unexpected-end-of-file))
           (else (loopx (cons byte out))))))))


  (define http-headers-read
    (lambda (generator)

      (define massage*
        (lambda (chars)
          (let loop ((chars chars))
            (if (null? chars)
                '()
                (if (char=? (car chars) #\space)
                    (loop (cdr chars))
                    chars)))))

      (define massage
        (lambda (string)
          (let loopx ((chars (reverse (massage* (reverse (massage* (string->list string))))))
                      (key '()))
            (if (null? chars)
                (raise-invalid 5)
                (let ((char (car chars)))
                  (if (char=? char #\:)
                      (cons (string->symbol (string-downcase (list->string (reverse (massage* key)))))
                            (string-downcase (list->string (massage* (reverse (massage* (reverse (cdr chars))))))))
                      (loopx (cdr chars) (cons (car chars) key))))))))

      (let loopy ((out '()))
        (let ((bytes (http-line-read generator)))
          (if (null? bytes)
              (reverse (map (lambda (x) (massage (utf8->string (apply bytevector x)))) out))
              (loopy (cons bytes out)))))))

  (define http-chunked-read
    (lambda (generator)

      (define massage
        (lambda (generator size)
          (let ((bytevector (make-bytevector size)))
            (let loop ((index 0))
              (unless (fx=? index size)
                (bytevector-u8-set! bytevector index (generator))
                (loop (fx+ index 1))))
            bytevector)))

      (define continue
        (lambda ()
          (let ((chunk-size (string->number (utf8->string (u8-list->bytevector (http-line-read generator))) 16)))
            (unless chunk-size
              (raise-invalid 11))
            (if (fxzero? chunk-size)
                (begin
                  (set! continue eof-object)
                  (eof-object))
                (let ((bytevector (massage generator chunk-size)))
                  (http-line-read generator)
                  bytevector)))))

      (lambda ()
        (continue))))

  (define http-body-read
    (lambda (generator headers)
      (let ((content-length (let ((value (assq 'content-length headers)))
                              (if value
                                  (string->number (cdr value))
                                  #f))))
        (if content-length
            (generator->bytevector generator content-length)
            (let ((chunked? (let ((value (assq 'transfer-encoding headers)))
                              (and value (string=? (cdr value) "chunked")))))
              (if chunked?
                  (http-chunked-read generator)
                  eof-object))))))

  ;; http-request-read

  (define http-request-read
    (lambda (generator)
      ;; GENERATOR must yield one byte at a time

      (define request-line-read
        (lambda (generator)

          (define massage
            (lambda (bytes)
              (let loop ((bytes bytes)
                         (chunk '())
                         (out '()))
                (if (null? bytes)
                    (if (null? chunk)
                        (raise-invalid 3)
                        (reverse (cons (utf8->string (apply bytevector (reverse chunk))) out)))
                    (let ((byte (car bytes)))
                      (if (and (fx=? byte byte-space) (not (null? chunk)))
                          (loop (cdr bytes) '() (cons (utf8->string (apply bytevector (reverse chunk))) out))
                          (loop (cdr bytes) (cons byte chunk) out)))))))

          (let ((strings (massage (http-line-read generator))))
            (unless (fx=? (length strings) 3)
              (raise-invalid 2))
            (values (string->symbol (car strings)) (cadr strings) (string->symbol (caddr strings))))))

      (guard (ex (else (values #f #f #f #f #f)))
        (call-with-values (lambda () (request-line-read generator))
          (lambda (method uri version)
            (call-with-values (lambda () (http-headers-read generator))
              (lambda (headers)
                (values method uri version headers (http-body-read generator headers)))))))))

  ;; http-request-write

  (define http-request-write
    (lambda (accumulator method target version headers body)
      (let* ((request-line (format #f "~a ~a ~a\r\n" method target version))
             (headers (apply string-append (map (lambda (x) (format #f "~a: ~a\r\n" (car x) (cdr x))) headers))))
        (accumulator (string->utf8 (string-append request-line headers "\r\n")))
        (let loop ()
          (let ((bytevector (body)))
            (unless (eof-object? bytevector)
              (accumulator bytevector)
              (loop)))))))

  ;; http-response-read

  (define http-response-read
    (lambda (generator)
      ;; GENERATOR must yield one byte at a time

      (define response-line-read
        (lambda (generator)

          (define massage
            (lambda (bytes)
              (let loop ((bytes bytes)
                         (chunk '())
                         (out '()))
                (if (null? bytes)
                    (if (null? chunk)
                        (raise-invalid 7)
                        (reverse (cons (utf8->string (apply bytevector (reverse chunk))) out)))
                    (let ((byte (car bytes)))
                      (if (and (fx=? byte byte-space) (not (null? chunk)))
                          (loop (cdr bytes) '() (cons (utf8->string (apply bytevector (reverse chunk))) out))
                          (loop (cdr bytes) (cons byte chunk) out)))))))

          (let ((strings (massage (http-line-read generator))))
            (cond
             ((fx=? (length strings) 3)
              (values (string->symbol (car strings)) (string->number (cadr strings)) (caddr strings)))
             ((fx=? (length strings) 2)
              (values (string->symbol (car strings)) (string->number (cadr strings)) #f))
             (else (raise-invalid 10))))))

      (call-with-values (lambda () (response-line-read generator))
        (lambda (version code reason)
          (call-with-values (lambda () (http-headers-read generator))
            (lambda (headers)
              (values version code reason headers (http-body-read generator headers))))))))


  ;; http-response-write

  (define http-response-write
    (lambda (accumulator version code reason headers body)

      (define massage*
        (lambda (generator)
          (let loop ((out '()))
            (let ((bytevector (generator)))
              (if (eof-object? bytevector)
                  (apply bytevector-append (reverse out))
                  (loop (cons bytevector out)))))))

      (define tranfer-encoding-chunked?
        (lambda (pair)
          (and (eq? (car pair) 'tranfer-encoding)
               (string=? (cdr pair) "chunked"))))

      (define massage**
        (lambda (headers content-length)
          (cond
           ((null? headers) (list (cons 'content-length content-length)))
           ((tranfer-encoding-chunked? (car headers)) (cons (cons 'content-length content-length) (cdr headers)))
           (else (cons (car headers) (massage** (cdr headers) content-length))))))

      (define massage
        (lambda (headers body)
          (let ((bytevector body))
            (values (massage** headers (bytevector-length bytevector)) bytevector))))

      (assert (or (pair? headers) (null? headers)))
      (assert (bytevector? body))

      (call-with-values (lambda () (massage headers body))
        (lambda (headers body)
          (let* ((response-line (format #f "~a ~a ~a\r\n" version code reason))
                 (headers (apply string-append (map (lambda (x) (format #f "~a: ~a\r\n" (car x) (cdr x))) headers))))
            (accumulator (string->utf8 (string-append response-line headers "\r\n")))
            (accumulator body)))))))
