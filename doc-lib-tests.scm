;; Tests for in-source documentation reader.
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

(import (scheme base)
        (scheme write)
        (scheme process-context)
        (srfi 1)
        (srfi 64)
        (doc-lib))

(test-begin "doc-lib")

(test-group "documentation record"
  (let ((obj (make-documentation #t "Hello world." '(+ 3 2) '((a . 1)))))
    (test-assert (documentation? obj))
    (test-assert (documentation-attached? obj))
    (test-assert (string=? (documentation-text obj) "Hello world."))
    (test-assert (equal? (documentation-content obj) '(+ 3 2)))
    (test-assert (equal? (documentation-alist obj) '((a . 1))))))

(define attached "#|* The *|\\# number \\\\ 3. *|#3")
(test-group "read-documentation/attached"
  (let ((obj (read-documentation (open-input-string attached))))
    (test-assert (documentation-attached? obj))
    (test-assert (string=? (documentation-text obj) " The *|# number \\ 3. "))
    (test-assert (= 3 (documentation-content obj)))
    (test-assert (list? (documentation-alist obj)))))

(define unattached "#|? Hello world. ?|\\# ?|#")
(test-group "read-documentation/unattached"
  (let ((obj (read-documentation (open-input-string unattached))))
    (test-assert (not (documentation-attached? obj)))
    (test-assert (string=? (documentation-text obj) " Hello world. ?|# "))
    (test-assert (not (documentation-content obj)))
    (test-assert (list? (documentation-alist obj)))))

(define no-documentation "(+ 3 2)")
(test-group "read-documentation/none"
  (let ((obj (read-documentation (open-input-string no-documentation))))
    (test-assert (equal? '(+ 3 2) obj))))

(define empty "#|**|#3")
(define empty2 "#|??|#3")
(test-group "read-documentation/empty"
  (let ((obj (read-documentation (open-input-string empty)))
        (obj2 (read-documentation (open-input-string empty2))))
    (test-assert (documentation-attached? obj))
    (test-assert (string=? (documentation-text obj) ""))
    (test-assert (= 3 (documentation-content obj)))
    (test-assert (list? (documentation-alist obj)))
    (test-assert (not (documentation-attached? obj2)))
    (test-assert (string=? (documentation-text obj2) ""))
    (test-assert (not (documentation-content obj2)))
    (test-assert (list? (documentation-alist obj2)))))

(define invalid "(+ 1 2 #|* An expression is missing after this. *|#)")
(test-group "read-documentation/error"
  (test-error (read-documentation (open-input-string invalid))))

(define nested "#|* Outer *|# (+ 1 #|* Inner *|# (- 2 #|? Innermost ?|# 3))")
(test-group "read-documentation/nested"
  (let* ((outer (read-documentation (open-input-string nested)))
         (inner (third (documentation-content outer)))
         (innermost (third (documentation-content inner))))
    (test-assert (documentation-attached? outer))
    (test-assert (string=? (documentation-text outer) " Outer "))
    (test-assert (eq? '+ (car (documentation-content outer))))
    (test-assert (list? (documentation-alist outer)))

    (test-assert (documentation-attached? inner))
    (test-assert (string=? (documentation-text inner) " Inner "))
    (test-assert (eq? '- (car (documentation-content inner))))
    (test-assert (list? (documentation-alist inner)))

    (test-assert (not (documentation-attached? innermost)))
    (test-assert (string=? (documentation-text innermost) " Innermost "))
    (test-assert (not (documentation-content innermost)))
    (test-assert (list? (documentation-alist innermost)))))

(define weird1 "(1 . #|* hello *|# 2)")
(test-group "read-documentation/improper-attached"
  (let* ((out (read-documentation (open-input-string weird1)))
         (obj (cdr out)))
    (test-assert (= (car out) 1))
    (test-assert (documentation-attached? obj))
    (test-assert (string=? (documentation-text obj) " hello "))
    (test-assert (= 2 (documentation-content obj)))
    (test-assert (list? (documentation-alist obj)))))

(define weird1 "(1 #|? world ?|# 2 . 2)")
(define weird2 "(1 . #|? world ?|# 2)")
(define weird3 "(1 . 2 #|? world ?|#)")
(test-group "read-documentation/improper-unattached"
  (let ((out (read-documentation (open-input-string weird1))))
    (test-assert (not (documentation-attached? (second out))))
    (test-assert (string=? " world " (documentation-text (second out))))
    (test-assert (not (documentation-content (second out))))
    (test-assert (list? (documentation-alist (second out)))))
  (test-error (read-documentation (open-input-string weird2)))
  (test-error (read-documentation (open-input-string weird3))))

(define in-line-comment ";; #|? Hello ?|#\n")
(test-group "read-documentation/in-line-comment"
  (let ((out (read-documentation (open-input-string in-line-comment))))
    (test-assert (eof-object? out))))

(define in-sexp-comment "#;(+ 1 #|? Hello ?|# 2)")
(test-group "read-documentation/in-sexp-comment"
  (let ((out (read-documentation (open-input-string in-sexp-comment))))
    (test-assert (eof-object? out))))

(define in-block-comment "#| hello #|? world ?|# ! |#")
(test-group "read-documentation/in-block-comment"
  (let ((out (read-documentation (open-input-string in-block-comment))))
    (test-assert (eof-object? out))))

(define single-char "#| * ? |#")
(test-group "read-documentation/single-char"
  (let ((out (read-documentation (open-input-string single-char))))
    (test-assert (eof-object? out))))

(test-end)
