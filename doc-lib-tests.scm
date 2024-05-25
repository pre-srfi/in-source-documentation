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
        (srfi 1)
        (srfi 64)
        (doc-lib))

(test-begin "doc-lib")

(test-group "doc record"
  (let ((obj (make-doc #t "Hello world." '(+ 3 2) '((a . 1)))))
    (test-assert (doc? obj))
    (test-assert (doc-attached? obj))
    (test-assert (string=? (doc-text obj) "Hello world."))
    (test-assert (equal? (doc-content obj) '(+ 3 2)))
    (test-assert (equal? (doc-alist obj) '((a . 1))))))

(define attached "#? The ?\\# number \\\\ 3. ?#3")
(test-group "read-doc/attached"
  (let ((obj (read-doc (open-input-string attached))))
    (test-assert (doc-attached? obj))
    (test-assert (string=? (doc-text obj) " The ?# number \\ 3. "))
    (test-assert (= 3 (doc-content obj)))
    (test-assert (list? (doc-alist obj)))))

(define unattached "#* Hello world. *\\# *#")
(test-group "read-doc/unattached"
  (let ((obj (read-doc (open-input-string unattached))))
    (test-assert (not (doc-attached? obj)))
    (test-assert (string=? (doc-text obj) " Hello world. *# "))
    (test-assert (not (doc-content obj)))
    (test-assert (list? (doc-alist obj)))))

(define no-doc "(+ 3 2)")
(test-group "read-doc/none"
  (let ((obj (read-doc (open-input-string no-doc))))
    (test-assert (equal? '(+ 3 2) obj))))

(define invalid "(+ 1 2 #? An expression is missing after this. ?#)")
(test-group "read-doc/error"
  (test-error (read-doc (open-input-string invalid))))

(define nested "#? Outer ?# (+ 1 #? Inner ?# (- 2 #* Innermost *# 3))")
(test-group "read-doc/nested"
  (let* ((outer (read-doc (open-input-string nested)))
         (inner (third (doc-content outer)))
         (innermost (third (doc-content inner))))
    (test-assert (doc-attached? outer))
    (test-assert (string=? (doc-text outer) " Outer "))
    (test-assert (eq? '+ (car (doc-content outer))))
    (test-assert (list? (doc-alist outer)))

    (test-assert (doc-attached? inner))
    (test-assert (string=? (doc-text inner) " Inner "))
    (test-assert (eq? '- (car (doc-content inner))))
    (test-assert (list? (doc-alist inner)))

    (test-assert (not (doc-attached? innermost)))
    (test-assert (string=? (doc-text innermost) " Innermost "))
    (test-assert (not (doc-content innermost)))
    (test-assert (list? (doc-alist innermost)))))

(define weird1 "(1 . #? hello ?# 2)")
(test-group "read-doc/improper-attached"
  (let* ((out (read-doc (open-input-string weird1)))
         (doc (cdr out)))
    (test-assert (= (car out) 1))
    (test-assert (doc-attached? doc))
    (test-assert (string=? (doc-text doc) " hello "))
    (test-assert (= 2 (doc-content doc)))
    (test-assert (list? (doc-alist doc)))))

(define weird1 "(1 #* world *# 2 . 2)")
(define weird2 "(1 . #* world *# 2)")
(define weird3 "(1 . 2 #* world *#)")
(test-group "read-doc/improper-unattached"
  (let ((out (read-doc (open-input-string weird1))))
    (test-assert (not (doc-attached? (second out))))
    (test-assert (string=? " world " (doc-text (second out))))
    (test-assert (not (doc-content (second out))))
    (test-assert (list? (doc-alist (second out)))))
  (test-error (read-doc (open-input-string weird2)))
  (test-error (read-doc (open-input-string weird3))))

(test-end)
