# JSON for LispWorks

The `json` package is a dead-simple [JSON](http://www.json.org) parser for [LispWorks](http://www.lispworks.com).

It makes heavy use of my [`lexer`](http://github.com/massung/lexer) and [`re`](http://github.com/massung/re) packages, so to understand the code you should start there. But they are really is simple.

## Quickstart

To convert from JSON to a Lisp object use `JSON-DECODE`.

	(json-decode string &optional source) ;=> value

The two parameters are the same parameters that the `tokenize` function takes in the `lexer` package.

JSON arrays are decoded as Lisp vectors and JSON objects are decoded into associative lists. The JSON literals `true`, `false`, and `null` are decoded as the list keywords `:TRUE`, `:FALSE`, and `:NULL`. All other JSON values should translate directly.

To encode a Lisp object as JSON use the `JSON-ENCODE` function.

	(json-encode value) ;=> string

All the same rules for decoding apply to encoding: vectors are encoded as JSON arrays and lists are encoded as objects.

## Examples

	CL-USER > (json-decode "{\"test\": [1,-4e+3,true,{}]}")
	(("test" #(1 -4000 :TRUE NIL)))
	NIL

	CL-USER > (json-encode *)
	"{\"test\":[1,-4000,true,{}]}"
	NIL
