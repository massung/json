# JSON for LispWorks

The `json` package is a dead-simple [JSON](http://www.json.org) parser for [LispWorks](http://www.lispworks.com).

It makes heavy use of my [`lexer`](http://github.com/massung/lexer) and [`re`](http://github.com/massung/re) packages, so to understand the code you should start there. But they are really is simple.

## Quickstart

There really is only one function to use: `JSON-DECODE`.

	JSON-DECODE (string &OPTIONAL source) ;=> Lisp term

The two parameters are the same parameters that the `tokenize` function takes in the `lexer` package.

JSON arrays are decoded as Lisp lists and JSON objects are decoded into associative lists. The JSON literals `true`, `false`, and `null` are decoded as the list keywords `:TRUE`, `:FALSE`, and `:NULL`. All other JSON values should translate directly.

There is no obvious difference between `nil` (an empty list) and `nil` (an empty object). It has been my experience that if you are parsing JSON, you know what type a particular value should be and will Do The Right Thing.

## Examples

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
	(1 2 "foo" #(10.5))

	CL-USER > (json-decode "{ \"foo\" : \"bar\" }")
	(("foo" "bar"))
	NIL

# What's Next?

I haven't had a need to encode a Lisp object to JSON, but if I add anything later, that will likely be what it will be. If you do this yourself, please send me a pull request!
