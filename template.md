## Abstract

This specifies a convention for in-source documentation within block comments.
The syntax `#|* ... *|#` adds documentation text to the adjacent expression.
The syntax `#|? ... ?|#` adds documentation text that is not attached to any
particular expression. A Scheme library for reading in-source documentation is
also specified.

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
specified in a SRFI for package management. It also does not specify an
interface to control the output of an exporter, as exporter behavior is outside
the scope of this SRFI.

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

The sequences `#|*` and `#|?` indicate the start of a documentation comment. The
documentation comment continues, and may span multiple lines, until a closing
`*|#` or `?|#` appears, respectively.

This SRFI does not change the lexical syntax of block comments, as specified in
[R7RS section 2.2](https://small.r7rs.org/attachment/r7rs.pdf#section.2.2) and
[SRFI 30](https://srfi.schemers.org/srfi-30/srfi-30.html). The reader's behavior
shall not change when a documentation comment sequence is encountered.

The following rules apply only when reading documentation comments:

- An exception shall be raised if the matching closing character sequence is
  omitted.
- If a nested documentation comment appears within a block comment, it shall be
ignored.
- If a nested block comment appears within a documentation comment, the
character sequence shall be read literally as part of the documentation text.
- If a documentation comment appears within a line or S-expression comment, it
shall be ignored.

### Documentation library

```
(read-documentation [port])
```

The `read-documentation` procedure converts in-source documentation text and
external representations of Scheme objects into `documentation` records or
objects. It returns the next `documentation` record or object parsable from the
given textual input port, updating `port` to point to the first character past
the end of the unattached in-source documentation text, the
documentation-attached Scheme object, or non-documented external
representation of the object.

When parsing in-source documentation text, this procedure must recognize the
escape sequences `\#` and `\\`, and convert them to `#` and `\`, respectively.
No other escape sequences should be supported. This procedure shall not strip
whitespace present in the documentation text.

An exception should be raised if an attached documentation comment is read, but
there are no more objects to read before the expression ends or the port is
exhausted. An exception should be raised if an attached documentation comment is
read, but the next object read is a documentation comment of either kind.

It is an error if an unattached documentation comment appears after the dot in a
dotted list.

If the `port` argument is not supplied, `(current-input-port)` shall be used.

```
(define attached "#|* The number 3. *|#3")
(read-documentation (open-input-string attached))
=> [documentation record]
     attached = #t
     text = " The number 3. "
     content = 3
     alist = ()

(define unattached "#|? Hello world. ?|\# ?|#")
(read-documentation (open-input-string unattached))
=> [documentation record]
     attached = #f
     text = " Hello world. "
     content = #f
     alist = ()

(define no-documentation "(+ 3 2)")
(read-documentation (open-input-string no-documentation))
=> (+ 3 2)

(define invalid "(+ 1 2 #|* An expression is missing after this. *|# )")
(read-documentation (open-input-string invalid))
=> error

(define nested "#|* Outer #|*
                (+ 1 #|* Inner *|#
                     (+ 2 #|? Innermost ?|# 3))")
(read-documentation (open-input-string nested))
=> [documentation record]
     attached = #t
     text = " Outer "
     content = (+ 1 [documentation record]
                      attached = #t
                      text = " Inner "
                      content = (+ 2 [documentation record]
                                       attached = #f
                                       content = " Innermost "
                                       content = #f
                                       alist = ()
                                   3)
                      alist = ()
               )
     alist = ()
```
<br/>

```
(documentation-format [symbol])
```

The `documentation-format` parameter specifies the markup language documentation
comments are written in. The value of the parameter must be a symbol. The value
`'text` shall be supported, to indicate that documentation is written as
unformatted plain text. The default value of the parameter is defined by the
implementation. The set of available values is defined by the program being used
to export documentation. However, it is suggested that the following might be
provided:

- `'markdown`
- `'sxml`
- `'jsdoc`

```
(documentation-format 'markdown) ; => 'text
(documentation-format) ; => 'markdown
```
<br/>

The following procedures may be automatically implemented using
`define-record-type`:

```
(make-documentation attached? text content alist)
```

Create a `documentation` record. The `attached?` argument is a boolean
indicating whether or not the documentation is attached to an expression. The
`text` argument is the string containing the documentation comment's text. The
`content` argument is the expression the in-source documentation is attached to,
or `#f` if the in-source documentation is unattached. The `alist` argument is an
association list containing implementation-defined information relating to the
documentation, such as its location in the source file.

```
(make-documentation #t "Three plus two." '(+ 3 2)
                    '((source-location . (22 42))))
=> [documentation record]
     attached = #t
     text = "Three plus two."
     content = (+ 3 2)
     alist = ((source-location . (22 42)))
```
<br/>

```
(documentation-attached? documentation-object)
```

Returns the `attached?` field of a `documentation` record, a boolean.

```
(documentation-attached?
  (make-documentation #t " Three plus two. " '(+ 3 2) '()))
=> #t
```
<br/>

```
(documentation-text documentation-object)
```

Returns the `text` field of a `documentation` record, a string.

```
(documentation-text (make-documentation #t "Three plus two." '(+ 3 2) '()))
=> "Three plus two."
```
<br/>

```
(documentation-content documentation-object)
```

Returns the `content` field of a `documentation` record. The return value may be
of any readable type. If the return value is a list, it may contain nested
`documentation` records.

```
(documentation-content
  (make-documentation #t " Three plus two. " '(+ 3 2) '()))
=> (+ 3 2)
```
<br/>

```
(documentation-alist documentation-object)
```

Returns the `alist` field of a `documentation` record, an association list.

```
(documentation-alist
  (make-documentation #t " Three plus two. " '(+ 3 2)
                      '((source-location . (22 42)))))
=> ((source-location . (22 42)))
```
<br/>

```
(documentation? object)
```

Type predicate that returns `#t` if the supplied `object`
argument is a `documentation` record. Otherwise, returns `#f`.

```
(documentation? (make-documentation #t " Three plus two. " '(+ 3 2) '()))
=> #t
(documentation? "hello world")
=> #f
```

## Implementation

A conformant sample implementation of the documentation library is provided. It
requires R7RS-small.

The sample implementation is available [in the
repo](https://github.com/scheme-requests-for-implementation/srfi-253) or
[directly](https://srfi.schemers.org/srfi-253/doc-lib.sld).

A test suite for the documentation library is available [in the
repo](https://github.com/scheme-requests-for-implementation/srfi-253/blob/master/doc-lib-tests.scm)
or [directly](https://srfi.schemers.org/srfi-253/doc-lib-tests.scm).

A plain-text documentation exporter is available [in the
repo](https://github.com/scheme-requests-for-implementation/srfi-253/blob/master/doc-exporter.scm)
or [directly](https://srfi.schemers.org/srfi-253/doc-exporter.scm).

## Acknowledgements

Thank you to Alex Shinn and the Chibi Scheme contributors. The Chibi reader is
used as part of the sample implementation.

Thank you to the members of the SRFI mailing list for their input during the
drafting process.
