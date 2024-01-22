# replace-with-inflections.el

This package currently provides the following function:

* `query-replace-with-inflections`

This is an "inflection" aware version of `query-replace`.  For
example, replacing `foo_bar` with `baz_quux` will also replace
`foo_bars` with `baz_quuxes`, `FooBar` with `BazQuux`, `FOO_BAR` with
`BAZ_QUUX`, and so on.

Read the docstring for details.

## References

For the term "inflection", refer to the following packages which this
library depends on:

* [inflections](https://github.com/eschulte/jump.el)
* [string-inflection](https://github.com/akicho8/string-inflection)

## Installation

This package is available on [MELPA](http://melpa.org/#/replace-with-inflections)/[MELPA Stable](http://stable.melpa.org/#/replace-with-inflections)

```
M-x package-install replace-with-inflections
```

## Configuration

Here's my suggested settings:

```elisp
(define-key search-map "n" 'query-replace-with-inflections)
```

## Author

Copyright (c) 2017-2024 Akinori MUSHA.

Licensed under the 2-clause BSD license.  See `LICENSE.txt` for
details.

Visit [GitHub Repository](https://github.com/knu/replace-with-inflections.el)
for the latest information.
