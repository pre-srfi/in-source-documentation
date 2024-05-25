## Abstract

This specifies reader syntax extensions for in-source documentation. The syntax
`#? ... ?#` adds documentation text to the subsequent expression.
The syntax `#* ... *#` adds documentation text that is not attached
to any particular expression. A Scheme library for reading in-source
documentation is also specified.

## Rationale

In-source documentation systems allow for documentation to be written alongside
the code, in the source code file itself. Then, an exporter program can parse
the source code file, extract the documentation, and use it to generate
typeset documentation in a format such as LaTeX or HTML.

One of the advantages of in-source documentation is that when reading code,
there is no need to simulatenously navigate and switch between a separate
external information source, like an Info page or website. And oftentimes,
reading an object's in-source documentation will make reading its code
unnecessary.

Keeping documentation up-to-date is another task made easier by in-source
documentation. It is quicker to update documentation that is located next to the
modified source code, than having to find and update the correct location
in an external documentation file.

New technologies like the [Language Server
Protocol](https://microsoft.github.io/language-server-protocol) (LSP) allow
in-source documentation to be automatically retrieved and displayed by text
editors and Integrated Development Environments (IDE). LSP clients are included
in an increasing number of editors (including Emacs), but LSP servers are
presently available for only a small number of Scheme implementations.

Introducing a flexible in-source documentation system as an SRFI will
standardize a stable interface that can be used in multiple implementations,
both to generate typeset documentation and to provide a base for new editor
tools and integrations.

This SRFI intentionally does not specify a command-line interface for a program
that exports in-source documentation. Such an interface would be better
specified in a SRFI for package management. It also does not specify a format
for the extracted in-source documentation text, as exporters may choose to
support any number of different markup formats.

In-source documentation is not to be confused with literate programming (LP).
For in-source documentation systems, documentation is extracted from the source
code. For LP systems, source code is extracted (or "tangled", in LP terminology)
from the documentation.

### Existing solutions

Chicken Scheme's [Hahn](https://wiki.call-cc.org/eggref/4/hahn) library provided
an in-source documentation system for Chicken 4, but it has not been updated to
Chicken 5. It uses a custom S-expression-based markup format, and documentation
is placed between the identifier name and body of `define` forms. It
can export to LaTeX and Wiki.

[Clojure](https://clojure.org/) places docstrings between the procedure name and
its argument list. Documentation is accessed in the REPL, using the `(doc
proc-name)` procedure. Documentation export is provided by a number of
external projects. Most parse docstrings as markdown text with WikiLink links,
and export to HTML.

The [SchemeDoc](https://homes.cs.aau.dk/~normark/schemedoc/) library provided an
in-source documentation library for R4RS/R5RS, as part of the larger LAML
library. It used a custom markup system placed inside regular Scheme comments,
and assigned meaning to the number of semicolons preceding the comment text. It
exported to HTML.

Racket's [srcdoc](https://docs.racket-lang.org/scribble/srcdoc.html) module
provides special forms to document procedures, variables, and data structures.
In-source documentation uses the Scribble markup format, and can be exported to
HTML or LaTeX.

## Specification

### Syntax

This SRFI extends the specifications of comments in
[R7RS section 2.2](https://small.r7rs.org/attachment/r7rs.pdf#section.2.2)
as follows:

The sequences `#?` and `#*` indicate the start of a documentation comment. The
documentation comment continues, and may span multiple lines, until a matching
`?#` or `*#` appears, respectively. If the matching closing character sequence
is omitted, an error is signaled. Documentation comments are visible to Scheme
as a single whitespace. Documentation comments may not be nested: a `#*` or `#?`
sequence inside of a documentation comment is ignored.

This SRFI also adds four productions in the specification of lexical structure,
[R7RS section
7.1.1](https://small.r7rs.org/attachment/r7rs.pdf#subsection.7.1.1), as follows:

```
<attached documentation comment> -->
  #? <attached documentation comment text>* ?#

<attached documentation comment text> -->
  <character sequence not containing ?#>

<unattached documentation comment> -->
  #* <unattached documentation comment text>* *#

<unattached documentation comment text> -->
  <character sequence not containing *#>
```

### Documentation library

```
(read-doc [port])
```
The `read-doc` procedure converts in-source documentation text and
external representations of Scheme objects into `doc` records or objects. It
returns the next `doc` record or object parsable from the given textual input
port, updating `port` to point to the first character past the end of
the unattached in-source documentation text, the documentation-attached Scheme
object, or non-documented external representation of the object.

When parsing in-source documentation text, this procedure must recognize the
escape sequences `\#` and `\\`, and convert them to `#` and `\`, respectively.
No other escape sequences should be supported. This procedure shall not strip
whitespace present in the documentation text.

It is an error if the attached `#? ... ?#` syntax is read, but there are no more
objects to read before the expression ends or the port is exhausted. It is an
error if the attached `#? ... ?#` syntax is read, but next read object is a
documentation comment of any kind.

It is an error if the unattached `#* ... *#` syntax appears after the dot in a
dotted list.

If the `port` argument is not supplied, `(current-input-port)` shall be used.

```
(define attached "#? The number 3. ?#3")
(read-doc (open-input-string attached))
;; => (doc #t " The number 3. " 3 '())

(define unattached "#* Hello world. */# *#")
(read-doc (open-input-string unattached))
;; => (doc #f " Hello world. *# " #f '())

(define no-doc "(+ 3 2)")
(read-doc (open-input-string no-doc))
;; => '(+ 3 2)

(define invalid "(+ 1 2 #? An expression is missing after this. ?# )")
(read-doc (open-input-string invalid))
;; => error

(define nested "#? Outer #? (+ 1 #? Inner ?# (+ 2 #* Innermost *# 3))")
(read-doc (open-input-string nested))
;; => (doc #t " Outer "
           `(+ 1 ,(doc #t " Inner "
                       `(+ 2 ,(doc #f " Innermost " #f '()) 3)
                       '()))
           '())
```
<br/>

The following procedures may be automatically implemented using
`define-record-type`:

```
(make-doc attached? text content alist)
```

Create a `doc` record. The `attached?` argument is a boolean indicating whether
or not the documentation is attached to an expression. The `content` argument is
the string containing the in-source documentation text, or `#f` if there is no
documentation available. The `content` argument is the expression the in-source
documentation is attached to, or `#f` if the in-source documentation is
unattached. The `alist` argument is an association list containing
implementation-defined information relating to the documentation, such as its
location in the source file.

```
(make-doc #t "Three plus two." '(+ 3 2)
          '((source-location . (22 42))))
;; => object of type 'doc'
```
<br/>

```
(doc-attached? doc-object)
```

Returns the `attached?` field of a `doc` record, a boolean.

```
(doc-attached? (make-doc #t " Three plus two. " '(+ 3 2) '()))
;; => #t
```
<br/>

```
(doc-text doc-object)
```

Returns the `text` field of a `doc` record, a string.

```
(doc-text (make-doc #t "Three plus two." '(+ 3 2) '()))
;; => "Three plus two."
```
<br/>

```
(doc-content doc-object)
```

Returns the `content` field of a `doc` record. The return value may be of any
readable type. If the return value is a list, it may contain nested `doc`
records.

```
(doc-content (make-doc #t " Three plus two. " '(+ 3 2) '()))
;; => '(+ 3 2)
```
<br/>

```
(doc-alist doc-object)
```

Returns the `alist` field of a `doc` record, an association list.

```
(doc-alist (make-doc #t " Three plus two. " '(+ 3 2)
                     '((source-location . (22 42)))))
;; => '((source-location . (22 42)))
```
<br/>

```
(doc? object)
```

Type predicate that returns `#t` if the supplied `object`
argument is a `doc` record. Otherwise, returns `#f`.

```
(doc? (make-doc #t " Three plus two. " '(+ 3 2) '()))
;; => #t
(doc? "hello world")
;; => #f
```

## Implementation

A conformant sample implementation of the documentation library is provided. It
requires R7RS-small.

Reader support for the syntax extensions is implementation-specific, and is not
provided.

The sample implementation is available [in the
repo](https://github.com/scheme-requests-for-implementation/srfi-2xx) or
[directly](https://srfi.schemers.org/srfi-2xx/doc-lib.sld).

A test suite for the documentation library is available [in the
repo](https://github.com/scheme-requests-for-implementation/srfi-2xx/blob/master/doc-lib-tests.scm)
or [directly](https://srfi.schemers.org/srfi-2xx/doc-lib-tests.scm).

## Acknowledgements

Thank you to Alex Shinn and the Chibi Scheme contributors. The Chibi reader is
used as part of the sample implementation.

Thank you to the members of the SRFI mailing list for their input during the
drafting process.
