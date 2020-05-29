# Ahungry Fleece

## A minimalist utility library for Common Lisp
In Greek mythology, the Golden Fleece (Greek: χρυσόμαλλον δέρας
chrysómallon déras) is the fleece of the gold-hair winged ram,
which was held in Colchis. The fleece is a symbol of authority and
kingship. It figures in the tale of the hero Jason...

In my mythology, the Ahungry Fleece simplifies the programmer's job of
rapid development in one of the greatest languages around (Lisp!)

## TOC
- [Setup](#setup)
  * [Install](#installation)
- [Usage](#usage)
  * [IO](#io)
  * [JSON/YAML](#refpath)
  * [Logging](#logging)
  * [Testing](#testing)
  * [Colorizing](#colorizing)
  * [Clone](#clone)
- [About](#about)
  * [Maintainer](#maintainer)
  * [License](#license)

# Setup
## Installation
### Quicklisp/REPL
Currently the library is not on quicklisp (yet), so you'll have to add
to your local projects directory after cloning it, as such:

```sh
cd ~/some/path
git clone https://github.com/ahungry/ahungry-fleece
```

```lisp
(push #P"~/some/path/ahungry-fleece" ql:*local-project-directories*)
(ql:quickload :ahungry-fleece)
;; Optionally confirm all is working by running the tests
(af.lib.test:main)
```

### Roswell
Simply run:
```sh
ros install ahungry/ahungry-fleece
```

Then you can use it in your roswell image, or run the CLI scripts
directly via:

```
~/.roswell/bin/ahungry-fleece -h
```


# Usage
## IO
You can easily write to and read from files with some of the following
convenience functions.

```lisp
(use-package :af.lib.io)
(file-put-contents "/tmp/helloWorld" "Hello World!")
(file-get-contents "/tmp/helloWorld")
```

Or get directory/file status with:

```lisp
(directory-p "/dev") ;; t
(directory-p "/dev/null") ;; nil
(file-p "/dev") ;; nil
(file-p "/dev/null") ;; t
```

## JSON/YAML
### RefPath
There is a way to quickly select a node in some given YAML or JSON or
HASH-TABLE.

When given some YAML such as this (for example):

```yml
definitions:
  Pet:
    type: object
    required:
    - name
    - petType
```

You can get the first element of the 'required' array as such (assume
that YAML above is in a file named 'pets.yml'):

```lisp
(use-package :af.lib.hashy)
(defparameter *yml* (hash-from-yaml-file "pets.yml"))
(ref "#/definitions/Pet/required/0" *yml*) ;; returns "name"
```

You can also set values by passing in the optional third parameter:

```lisp
;; To set the value (changing 'object' to 'array')
(ref "#/definitions/Pet/type" *yml* "array")
```

The above would also work with json (just use the
`#'hash-from-json-file` call instead).

## Logging
There is also support for a small and easy to use logging interface.

To log using the `*loggy*` singleton instance of the Loggy class, you
can simply:

```lisp
(use-package :af.lib.loggy)

;; Will print only if log level is set to 'debug (default)
(flog 'debug "Hello world")

;; If you want to suppress those calls in the code on a prod instance
(setf (Level *loggy*) 'warn)

;; Will now no longer print at all (using a specific log call, but to
;; singleton again, so equivalent to the #'flog call):
(log-> *loggy* 'debug "Hello world")
```

You can customize the following properties in the Loggy class:
- Level (can use 'debug, 'warn, 'info, 'crit): controls when to invoke
  log output
- Output (default #'format): function called for outputting
- Output-Args (default '("~a~%")): Extra arguments passed to Output
- Output-Stream (default t): The stream target (primarily for format output)

## Testing
A tiny BDD test framework exists, and can be used as such:

```lisp
(use-package :af.lib.testy)
(suite "your.suite"
  (desc "your.suite.function"
    (it "Should add two numbers"
      (and
        (eq 4 (+ 2 2))
        (eq 8 (+ 6 2))))

    (it "Should fail here"
      (eq 10 (+ 2 2)))))
```
which will produce some output such as this when run:

```
your.suite

  your.suite.function

    + Should add two numbers
    - Should fail here

  2 assertions, 1 failures

1 tests, 1 failures
```
![testing](http://ahungry.com/img/test-shot.png)


## Colorizing
You can colorize shell output (via the shell escape codes) by using:

```lisp
(use-package :af.lib.ansi-colors)
(with-color :blue (format t "Blue text!"))
```

To avoid colors in the SLIME REPL, simply set:

```lisp
(setq af.lib.ansi-colors:*colorize-p* nil)
```

## Clone
You can also quickly create/bootstrap a project with a similar
directory / makefile setup as this one, by using:

```lisp
(ahungry-fleece:make-skelly-project "/some/path/here")
```

# About
## Maintainer
You can reach me at Matthew Carter <m@ahungry.com> or file an issue here.

## License
GPLv3
