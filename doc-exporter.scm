#! gosh -I.
#|? Simple plain-text documentation exporter program. ?|#
;; SPDX-License-Identifier: MIT

(import (scheme base)
        (scheme cxr)
        (scheme file)
        (scheme process-context)
        (scheme write)
        (srfi 1)
        (srfi 13)
        (srfi 28)
        (doc-lib))

(define (list-identifier expr)
  (let ((head (car expr)))
    (cond ((eq? head 'define-library)
           (format "* Library ~a\n"
                   (cadr expr)))
          ((eq? head 'define)
           (if (pair? (cadr expr))
               (format "* Procedure ~a\nArguments: ~a\n"
                       (car (cadr expr))
                       (cond ((null? (cdr (cadr expr)))
                              "Thunk")
                             ((pair? (cdr (cadr expr)))
                              (cdr (cadr expr)))
                             (else
                              "Variadic")))
               (format "* Variable ~a\n"
                       (cadr expr))))
          ((eq? head 'define-record-type)
           (format "* Record ~a\nConstructor: ~a\nPredicate: ~a\n"
                   (cadr expr)
                   (caddr expr)
                   (cadddr expr)))
          ((eq? head 'lambda)
           (format "* Lambda with arguments ~a\n" (cadr expr)))
           (else
            (format "* Identifier ~a\n" head)))))

(define (identifier-name doc)
  (if (not (documentation-attached? doc))
      ""
      (let ((expr (documentation-content doc)))
        (cond ((list? expr)
               (list-identifier expr))
              ((pair? expr)
               (format "* ~a\n\n" (car pair)))
              (else
               (format "* ~a\n\n" expr))))))

(define (format-doc-output docs)
  (string-join (map (lambda (doc)
                      (string-append (identifier-name doc) "\n"
                                     (string-trim-both
                                      (documentation-text doc))
                                     "\n"))
                    docs)
               "\
\n********************************************************************************\n"))

;; Fancier exporters will want to infer document structure from the records,
;; instead of just flattening them.
(define (flatten x)
  (filter documentation?
          (cond ((null? x) '())
                ((pair? x) (append (flatten (car x)) (flatten (cdr x))))
                ((and (documentation? x) (documentation-attached? x))
                 (cons x (flatten (documentation-content x))))
                (else (list x)))))

(define (read-all-docs port)
  (let loop ((docs '()))
    (let ((obj (read-documentation port)))
      (if (eof-object? obj)
          (flatten docs)
          (loop (cons obj docs))))))

(define (document-file fpath)
  ;; Exporter programs can parameterize documentation-format based on the value
  ;; of a CLI flag. Exporter libraries can then use the parameterized value.
  ;; documentation-format can also be parameterized by code, by a special string
  ;; inside the documentation text, or be inferred from the documentation text.
  (unless (eq? (documentation-format) 'text)
    (error "Unrecognized documentation format: " (documentation-format)))
  (let ((docs (call-with-input-file fpath
                (lambda (port)
                  (read-all-docs port))))
        (outpath (string-append fpath ".txt")))
    (display (format "Writing ~a.\n" outpath) (current-error-port))
    (call-with-output-file outpath
      (lambda (port)
        (display (format-doc-output docs) port)))))

(define (main args)
  (when (null? args)
    (display "Usage: doc-exporter.scm FILES ...\n")
    (exit 1))
  (for-each document-file args))

(main (cdr (command-line)))
