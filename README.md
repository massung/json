# JSON for LispWorks

The `JSON` package is a dead-simple [JSON](http://www.json.org) parser for [LispWorks](http://www.lispworks.com).

It makes heavy use of my [LEXER](http://www.github.com/massung/lexer) package, so to understand the code you should start there. But it really is simple.

# Quickstart

There really is only one function to use: `JSON-DECODE`.

	JSON-DECODE (string &OPTIONAL source) ;=> Lisp term

The two parameters are the same parameters that the `LEXER`  function takes. JSON arrays are decoded as Lisp vectors and JSON objects are decoded into associative lists. The JSON literals `true`, `false`, and `null` are decoded as the list keywords `:TRUE`, `:FALSE`, and `:NULL`.

# Examples

	CL-USER > (json-decode "1")
	1
	NIL

	CL-USER > (json-decode "-4e+3")
	-4000
	NIL

	CL-USER > (json-decode "\"Hello, world!\"")
	"Hello, world!"
	NIL

	CL-USER > (json-decode "[1,2,\"foo\",[10.5]]")
	#(1 2 "foo" #(10.5))

	CL-USER > (json-decode "{ \"foo\" : \"bar\" }")
	(("foo" . "bar"))
	NIL
