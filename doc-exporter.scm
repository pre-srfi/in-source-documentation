#! gosh -I.
#|? Simple plain-text documentation exporter program. ?|#
;; SPDX-License-Identifier: MIT

(import (scheme base)
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
           (if (list? (cadr x))
               (format "* Procedure ~a\n"
                       (car (cadr x)))
               (format "* Variable ~a\n"
                       (cadr x))))
          ((eq? head 'define-record-type)
           (format "* Record ~a\nConstructor: ~a\nPredicate: ~a\n"
                   (cadr x)
                   (caddr x)
                   (cadddr x)))
          ((eq? head 'lambda)
           (format "* Lambda with arguments ~a\n" (cadr x)))
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
                      (string-append (identifier-name doc)
                                     (string-trim-both
                                      (documentation-text doc)))))
               "\n"))

;; Fancier exporters will want to infer document structure from the records,
;; instead of just flattening them.
(define (flatten lst)
  (cond ((null? x) '())
        ((pair? x) (append (flatten (car x)) (flatten (cdr x))))
        ((documentation? x) (if (documentation-attached? x)
                                (cons x (flatten (documentation-content x)))
                                (list x)))
        (else (list x))))

(define (read-all-docs port)
  (let loop ((docs '())
             (last #f))
    (when (eof-object? last)
      (flatten docs))
    (let ((obj (read-documentation port)))
      (if (documentation? obj)
          (loop (cons obj docs) obj)
          (loop docs obj)))))

(define (document-file fpath)
  ;; Exporter programs can parameterize documentation-format, based on the value
  ;; of a CLI flag. Exporter libraries can then use the parameterized value.
  ;; document-format can also be parameterized in code, or maybe even be
  ;; inferred from the documentation text.
  (unless (eq? (documentation-format) 'text)
    (error "Unrecognized documentation format: " (documentation-format)))
  (let ((docs (call-with-input-file fpath
                (lambda (port)
                  (read-all-docs port))))
        (outpath (string-append fpath ".txt")))
    (display (format "Writing ~a.\n" outpath) (current-error-port))
    (call-with-output-file outpath
      (lambda (port)
        (display (format-doc-output docs))))))

(define (main args)
  (when (null? args)
    (display "Usage: doc-exporter.scm FILES ...\n")
    (exit 1))
  (for-each document-file args))

(main (cdr (command-line)))
