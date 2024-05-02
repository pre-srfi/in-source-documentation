;; Scheme reader for in-source documentation.
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

#| #? This module implements the library component of SRFI 2xx.

Implementations should rename this library to (srfi 2XX).
?# |#
(define-library (doc-lib)
  (import (scheme base)
          (scheme case-lambda))
  (export make-doc doc? doc-attached? doc-content doc-expression doc-alist
          read-doc)
  (begin

    #| #? The doc record type stores documentation and any attached code.

    Fields:
      attached?: Whether the documentation is attached to an
                 expression or not.
      content: The text content of the documentation as a string.
      expression: If attached, the expression the documentation is attached to.
                  Set to #f if unattached.
    ?# |#
    (define-record-type <doc>
      (doc attached content expression)
      doc?
      (attached doc-attached?)
      (content doc-content) ;string
      (expression doc-expression))

    #| #? The read-doc procedure reads a port into 'doc' records.

    Arguments:
      port (optional): A textual input port. Default is (current-input-port).
    ?# |#
    (define read-doc
      (case-lambda
       (() (_read-doc (current-input-port)))
       ((port) (_read-doc port))))

    (define (_get-doc symbol import-set)
      1)

    (define (_read-doc port)
      1)

))
