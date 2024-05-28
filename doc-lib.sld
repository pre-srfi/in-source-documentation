#|* Scheme reader for in-source documentation. *|#
;; SPDX-License-Identifier: MIT
;; SPDX-FileCopyrightText: 2024 Antero Mejr <mail@antr.me>

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

(define-library (doc-lib)
  (import (scheme base)
          (scheme char)
          (scheme read))
  (export make-documentation documentation? documentation-attached?
          documentation-text documentation-content documentation-alist
          read-documentation documentation-format)
  (begin

    #|? This library implements the SRFI for in-source documentation. ?|#

    #|* Create a `documentation` record.

The `attached?` argument is a boolean indicating whether or not the
documentation is attached to an expression. The `content` argument is the string
containing the in-source documentation text, or `#f` if there is no
documentation available. The `content` argument is the expression the in-source
documentation is attached to, or `#f` if the in-source documentation is
unattached. The `alist` argument is an association list containing
implementation-defined information relating to the documentation, such as its
location in the source file.
    *|#
    (define-record-type <documentation>
      (make-documentation attached? text content alist)
      documentation?
      (attached? documentation-attached?)
      (text documentation-text) ;string
      (content documentation-content)
      (alist documentation-alist))

    #|* Read doc objects and Scheme objects from a port.

This procedure returns the next `doc` record or object parsable from the
given textual input port, updating `port` to point to the first character past
the end of the unattached in-source documentation text, the
documentation-attached Scheme object, or non-documented external representation
of the object.
    *|#
    (define (read-documentation . rest)
      (if (null? rest)
          (read-doc-internal (current-input-port))
          (read-doc-internal (car rest))))

    #|* Specify the markup language documentation comments are written in.

The value of the parameter must be a symbol.
    *|#
    (define documentation-format
      (make-parameter 'text (lambda (x)
                              (unless (symbol? x)
                                (error "Not a symbol: " x))
                              x)))

    ;; Additions to the Chibi reader.

    (define (read-doc-internal port)
      (read-with-shared-structure port))

    (define (skip-doc-comment attached?)
      (define (skip-doc-comment2 in . rest)
        (let ((p (peek-char in)))
          (when (eof-object? p)
            (error "EOF while in a documentation comment."))
          (cond ((and (not (null? rest))
                      (eqv? (car rest) #\|)
                      (eqv? p #\#))
                 (when (or (< (length rest) 2)
                           (and attached? (not (eqv? #\* (cadr rest))))
                           (and (not attached?) (not (eqv? #\? (cadr rest)))))
                   (error
                    "Documentation comment ended without matching character."))
                 (read-char in)
                 (list->string (reverse (cddr rest))))
              ((and (not (null? rest))
                    (eqv? (car rest) #\\)
                    (eqv? p #\\))
               (read-char in)
               (apply skip-doc-comment2 in rest))
              ((and (not (null? rest))
                    (eqv? (car rest) #\\)
                    (eqv? p #\#))
               (apply skip-doc-comment2 in (cons (read-char in) (cdr rest))))
              (else
               (apply skip-doc-comment2 in (cons (read-char in) rest))))))
      skip-doc-comment2)

    (define skip-doc-attached (skip-doc-comment #t))
    (define skip-doc-unattached (skip-doc-comment #f))

    ;; Taken from Chibi Scheme's lib/srfi/38.scm reader.
    ;; See "BEGIN EDITS" comment.
    ;;
    ;; This code was written by Alex Shinn in 2009 and placed in the
    ;; Public Domain.  All warranties are disclaimed.

    (define (skip-line in)
      (let ((c (read-char in)))
        (if (not (or (eof-object? c) (eqv? c #\newline)))
            (skip-line in))))

    (define (skip-horizontal-whitespace in)
      (case (peek-char in)
        ((#\space #\tab)
         (read-char in)
         (skip-horizontal-whitespace in))))

    (define (skip-whitespace in)
      (case (peek-char in)
        ((#\space #\tab #\newline #\return)
         (read-char in)
         (skip-whitespace in))))

    (define (skip-whitespace-and-line-comments in)
      (case (peek-char in)
        ((#\space #\tab #\newline #\return)
         (read-char in)
         (skip-whitespace-and-line-comments in))
        ((#\;)
         (skip-line in)
         (skip-whitespace-and-line-comments in))))

    ;; BEGIN EDITS - added read argument
    (define (skip-comment in depth read)
      (case (read-char in)
        ((#\#) (skip-comment in (if (eqv? #\| (read-char in)) (+ depth 1) depth)
                             read))
        ((#\|) (if (eqv? #\# (peek-char in))
                   (if (zero? depth)
                       #f
                       (skip-comment in (- depth 1) read))
                   (skip-comment in depth read)))
        ((#\*) (let ((text (skip-doc-attached in)))
                 (make-documentation #t text (read in) '())))
        ((#\?) (let ((text (skip-doc-unattached in)))
                 (make-documentation #f text #f '())))
        (else (if (eof-object? (peek-char in))
                  (read-incomplete-error "unterminated #| comment")
                  (skip-comment in depth read)))))
    ;; END EDITS

    ;; returns #f if a trailing # was consumed
    (define (skip-whitespace-and-sexp-comments in read)
      (skip-whitespace-and-line-comments in)
      (cond
       ((eqv? #\# (peek-char in))
        (read-char in)
        (cond ((eqv? #\; (peek-char in))
               (read-char in)
               (read in)
               (skip-whitespace-and-sexp-comments in read))
              ;; BEGIN EDITS
              ;; ((eqv? #\| (peek-char in))
              ;;  (skip-comment in 0)
              ;;  (skip-whitespace-and-sexp-comments in read))
              ;; END EDITS
              (else #f)))
       (else
        #t)))

    (define delimiters
      '(#\; #\" #\| #\( #\) #\{ #\} #\space #\tab #\newline #\return))

    (define named-chars
      `(("newline" . #\newline)
        ("return" . #\return)
        ("space" . #\space)
        ("tab" . #\tab)
        ("null" . ,(integer->char 0))
        ("alarm" . ,(integer->char 7))
        ("backspace" . ,(integer->char 8))
        ("escape" . ,(integer->char 27))
        ("delete" . ,(integer->char 127))))

    (define U1 1)
    (define S8 2)
    (define U8 3)
    (define S16 4)
    (define U16 5)
    (define S32 6)
    (define U32 7)
    (define S64 8)
    (define U64 9)
    (define F32 10)
    (define F64 11)
    (define C64 12)
    (define C128 13)
    (define F8 14)
    (define F16 15)

    (define (resolve-uniform-type c prec)
      (or
       (case prec
         ((1) (and (eqv? c #\u) U1))
         ((8) (case c ((#\u) U8) ((#\s) S8) ((#\f) F8) (else #f)))
         ((16) (case c ((#\u) U16) ((#\s) S16) ((#\f) F16) (else #f)))
         ((32) (case c ((#\u) U32) ((#\s) S32) ((#\f) F32) (else #f)))
         ((64) (case c ((#\u) U64) ((#\s) S64) ((#\f) F64) ((#\c) C64) (else #f)))
         ((128) (case c ((#\c) C128) (else #f)))
         (else #f))
       (error "invalid uniform type" c prec)))

    (define read-with-shared-structure
      (let ((read read))
        (lambda o
          (let ((in (if (pair? o) (car o) (current-input-port)))
                (shared '()))
            (define (read-label res)
              (let ((c (peek-char in)))
                (cond
                 ((and (not (eof-object? c))
                       (or (char-numeric? c)
                           (memv (char-downcase c)
                                 '(#\- #\+ #\a #\b #\c #\d #\e #\f #\i))))
                  (read-label (cons (read-char in) res)))
                 ((and (eqv? c #\/) (not (memv #\/ res)))
                  (read-label (cons (read-char in) res)))
                 ((and (eqv? c #\@) (not (memv #\@ res)))
                  (read-label (cons (read-char in) res)))
                 (else
                  (list->string (reverse res))))))
            (define (read-numeric-hashes res)
              (if (eqv? #\# (peek-char in))
                  (let* ((res (cons (read-char in) res))
                         (c (read-char in)))
                    (if (memv c '(#\b #\d #\o #\x #\e #\i))
                        (read-numeric-hashes (cons c res))
                        (error "invalid numeric hash escape #" c)))
                  res))
            (define (read-number base)
              (let* ((str (read-label (read-numeric-hashes '())))
                     (n (string->number str base))
                     (c (peek-char in)))
                (if (or (not n) (not (or (eof-object? c) (memv c delimiters))))
                    (error "read error: invalid number syntax" str c)
                    n)))
            (define (read-float-tail in) ;; called only after a leading period
              (let lp ((res 0.0) (k 0.1))
                (let ((c (peek-char in)))
                  (cond
                   ((char-numeric? c)
                    (lp (+ res (* (- (char->integer (read-char in))
                                     (char->integer #\0))
                                  k))
                        (* k 0.1)))
                   ((or (eof-object? c) (memv c delimiters)) res)
                   (else (error "invalid char in float syntax" c))))))
            (define (read-name c in)
              (let lp ((ls (if (char? c) (list c) '())))
                (let ((c (peek-char in)))
                  (cond ((or (eof-object? c) (memv c delimiters))
                         (list->string (reverse ls)))
                        (else (lp (cons (read-char in) ls)))))))
            (define (read-named-char c in)
              (let ((name (read-name c in)))
                (cond ((assoc name named-chars string-ci=?) => cdr)
                      ((and (or (eqv? c #\x) (eqv? c #\X))
                            (string->number (substring name 1 (string-length name))
                                            16))
                       => integer->char)
                      (else (error "unknown char name" name)))))
            (define (read-type-id in)
              (let ((ch (peek-char in)))
                (cond
                 ((eqv? ch #\#)
                  (read-char in)
                  (let ((id (read in)))
                    (cond ((eq? id 't) #t)
                          ((integer? id) id)
                          (else (error "invalid type identifier" id)))))
                 ((eqv? ch #\")
                  (read in))
                 (else
                  (error "invalid type identifier syntax" ch)))))
            (define (read-escape-sequence)
              (let ((ch (read-char in)))
                (cond
                 ((eof-object? ch) (read-incomplete-error "incomplete escape"))
                 (else
                  (case ch
                    ((#\a) #\alarm) ((#\b) #\backspace)
                    ((#\n) #\newline) ((#\r) #\return)
                    ((#\t) #\tab)
                    ((#\newline) (skip-horizontal-whitespace in) #f)
                    ((#\space #\tab)
                     (skip-line in) (skip-horizontal-whitespace in) #f)
                    ((#\x #\X)
                     (let* ((n (read-number 16))
                            (ch2 (read-char in)))
                       (if (not (and n (eqv? ch2 #\;)))
                           (error "invalid string escape" n ch2)
                           (integer->char n))))
                    (else ch))))))
            (define (read-delimited terminal)
              (let ((out (open-output-string)))
                (let lp ()
                  (let ((ch (read-char in)))
                    (cond
                     ((eof-object? ch) (read-incomplete-error "incomplete string"))
                     ((eqv? ch terminal) (get-output-string out))
                     ((eqv? ch #\\)
                      (let ((ch2 (read-escape-sequence)))
                        (if ch2 (write-char ch2 out))
                        (lp)))
                     (else (write-char ch out) (lp)))))))
            (define (read-object)
              (let ((name (read-name #f in)))
                (skip-whitespace-and-line-comments in)
                (let* ((id (read-type-id in))
                       (type (lookup-type name id)))
                  (let lp ((ls '()))
                    (skip-whitespace-and-line-comments in)
                    (cond
                     ((eof-object? (peek-char in))
                      (error "missing closing }"))
                     ((eqv? #\} (peek-char in))
                      (read-char in)
                      (let ((res ((make-constructor #f type))))
                        (let lp ((ls (reverse ls)) ( i 0))
                          (cond
                           ((null? ls)
                            res)
                           (else
                            (slot-set! type res i (car ls))
                            (lp (cdr ls) (+ i 1)))))))
                     (else (lp (cons (read-one in) ls))))))))
            (define (read-hash in)
              (if (eof-object? (peek-char in))
                  (error "read error: incomplete # found at end of input"))
              (case (char-downcase (peek-char in))
                ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
                 (let* ((str (read-label '()))
                        (n (string->number str)))
                   (if (not n)
                       (error "read error: invalid reference" str))
                   (cond
                    ((eqv? #\= (peek-char in))
                     (if (assv n shared)
                         (error "read error: duplicate label" str))
                     (read-char in)
                     (let* ((cell (list #f))
                            (thunk (lambda () (car cell))))
                       (set! shared (cons (cons n thunk) shared))
                       (let ((x (read-one in)))
                         (if (hole? x)
                             (error "read error: self label reference" n))
                         (set-car! cell x)
                         x)))
                    ((eqv? #\# (peek-char in))
                     (read-char in)
                     (cond
                      ((assv n shared) => cdr)
                      (else (error "read error: unknown reference" n))))
                    (else
                     (error "read error: expected # after #n"
                                 (read-char in))))))
                ((#\;)
                 (read-char in)
                 (read-one in) ;; discard
                 (read-one in))
                ((#\|)
                 ;; BEGIN EDITS
                 ;; (skip-comment in 0)
                 ;; (read-one in))
                 (skip-comment in 0 read-one))
                 ;; END EDITS
                ((#\!)
                 (read-char in)
                 (let ((c (peek-char in)))
                   (cond
                    ((or (char-whitespace? c) (eqv? c #\/))
                     (skip-line in)
                     (read-one in))
                    (else
                     (let ((name (read-name #f in)))
                       (cond
                        ((string-ci=? name "fold-case")
                         (set-port-fold-case! in #t))
                        ((string-ci=? name "no-fold-case")
                         (set-port-fold-case! in #f))
                        (else            ;; assume a #!/bin/bash line
                         (error "unknown #! symbol" name)))
                       (read-one in))))))
                ((#\() (list->vector (read-one in)))
                ((#\') (read-char in) (list 'syntax (read-one in)))
                ((#\`) (read-char in) (list 'quasisyntax (read-one in)))
                ((#\,) (read-char in)
                 (let ((sym (if (eqv? #\@ (peek-char in))
                                (begin (read-char in) 'unsyntax-splicing)
                                'unsyntax)))
                   (list sym (read-one in))))
                ((#\t)
                 (let ((s (read-name #f in)))
                   (or (string-ci=? s "t") (string-ci=? s "true")
                       (error "bad # syntax" s))))
                ((#\f)
                 (let ((s (read-name #f in)))
                   (cond
                    ((or (string-ci=? s "f") (string-ci=? s "false"))
                     #f)
                    ((member s '("f8" "F8"))
                     (list->uvector F8 (read in)))
                    ((member s '("f16" "F16"))
                     (list->uvector F16 (read in)))
                    ((member s '("f32" "F32"))
                     (list->uvector F32 (read in)))
                    ((member s '("f64" "F64"))
                     (list->uvector F64 (read in)))
                    (else
                     (error "bad # syntax" s)))))
                ((#\d) (read-char in) (read in))
                ((#\x) (read-char in) (read-number 16))
                ((#\o) (read-char in) (read-number 8))
                ((#\b) (read-char in) (read-number 2))
                ((#\i) (read-char in) (exact->inexact (read-one in)))
                ((#\e)
                 (let ((s (read-name #\# in)))
                   (or (string->number s)
                       (read-one (open-input-string (substring s 2))))))
                ((#\u #\v #\s #\c)
                 (if (char-ci=? #\v (peek-char in))
                     (read-char in))
                 (let* ((c (char-downcase (read-char in)))
                        (prec (read-number 10))
                        (etype (resolve-uniform-type c prec))
                        (ls (read-one in)))
                   (if (not (list? ls))
                       (error "invalid uniform vector syntax" ls))
                   (list->uvector etype ls)))
                ((#\\)
                 (read-char in)
                 (let* ((c1 (read-char in))
                        (c2 (peek-char in)))
                   (if (or (eof-object? c2) (memv c2 delimiters))
                       c1
                       (read-named-char c1 in))))
                (else
                 (error "unknown # syntax: " (peek-char in)))))
            (define (read-one in)
              (cond
               ((not (skip-whitespace-and-sexp-comments in read-one))
                (read-hash in))
               (else
                (case (peek-char in)
                  ((#\#)
                   (read-char in)
                   (read-hash in))
                  ((#\()
                   (read-char in)
                   (let lp ((res '()))
                     (cond
                      ((not (skip-whitespace-and-sexp-comments in read-one))
                       (lp (cons (read-hash in) res)))
                      (else
                       (let ((c (peek-char in)))
                         (case c
                           ((#\))
                            (read-char in)
                            (reverse res))
                           ((#\.)
                            (read-char in)
                            (cond
                             ((memv (peek-char in) delimiters)
                              (let ((tail (read-one in)))
                                (cond
                                 ((null? res)
                                  (error "dot before any elements in list"))
                                 ((and (skip-whitespace-and-sexp-comments
                                        in read-one)
                                       (eqv? #\) (peek-char in)))
                                  (read-char in)
                                  (append (reverse res) tail))
                                 ((eof-object? (peek-char in))
                                  (read-incomplete-error
                                   "unterminated dotted list"))
                                 (else
                                  (error "expected end of list after dot")))))
                             ((char-numeric? (peek-char in))
                              (lp (cons (read-float-tail in) res)))
                             (else
                              (lp (cons (string->symbol (read-name #\. in)) res)))))
                           (else
                            (if (eof-object? c)
                                (read-incomplete-error "unterminated list")
                                (lp (cons (read-one in) res))))))))))
                  ((#\{)
                   (read-char in)
                   (read-object))
                  ((#\")
                   (read-char in)
                   (read-delimited #\"))
                  ((#\|)
                   (read-char in)
                   (string->symbol (read-delimited #\|)))
                  ((#\') (read-char in) (list 'quote (read-one in)))
                  ((#\`) (read-char in) (list 'quasiquote (read-one in)))
                  ((#\,)
                   (read-char in)
                   (let ((sym (if (eqv? #\@ (peek-char in))
                                  (begin (read-char in) 'unquote-splicing)
                                  'unquote)))
                     (list sym (read-one in))))
                  (else
                   (read in))))))
            ;; body
            (let ((res (read-one in)))
              (if (pair? shared)
                  (patch res))
              res)))))

    (define (hole? x) (procedure? x))
    (define (fill-hole x) (if (hole? x) (fill-hole (x)) x))

    (define (patch x)
      (cond
       ((pair? x)
        (if (hole? (car x)) (set-car! x (fill-hole (car x))) (patch (car x)))
        (if (hole? (cdr x)) (set-cdr! x (fill-hole (cdr x))) (patch (cdr x))))
       ((vector? x)
        (do ((i (- (vector-length x) 1) (- i 1)))
            ((< i 0))
          (let ((elt (vector-ref x i)))
            (if (hole? elt)
                (vector-set! x i (fill-hole elt))
                (patch elt)))))
       (else
        (let* ((type (type-of x))
               (slots (and type (type-slots type))))
          (cond
           (slots
            (let lp ((i 0) (ls slots))
              (cond
               ((pair? ls)
                (let ((elt (slot-ref type x i)))
                  (if (hole? elt)
                      (slot-set! type x i (fill-hole elt))
                      (patch elt))
                  (lp (+ i 1) (cdr ls))))))))))))
))
