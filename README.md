# Ahungry Fleece

## A JSON (and friends) utility library

In Greek mythology, the Golden Fleece (Greek: χρυσόμαλλον δέρας
chrysómallon déras) is the fleece of the gold-hair winged ram,
which was held in Colchis. The fleece is a symbol of authority and
kingship. It figures in the tale of the hero Jason...

In my mythology, the Ahungry Fleece simplifies the programmer's job of
rapid development in one of the greatest languages around (Lisp!)

# Usage

## YAML/JSON ref selection

There is a way to quickly select a node in some given YML or JSON as
such:

```yml
definitions:
  Pet:
    type: object
    required:
    - name
    - petType
```

Say you have that in a file such as pets.yml, you can get the first
element of the required array as such:

```lisp
(use-package :af.lib.hashy)
(defparameter *yml* (hash-from-yaml-file "pets.yml"))
(ref "#/definitions/Pet/required/0" *yml*) ;; returns "name"

;; To set the value (changing 'object' to 'array')
(ref "#/definitions/Pet/type" *yml* "array")
```

The above would also work with json (just use the
`#'hash-from-json-file` call instead).

# License

GPLv3
