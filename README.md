# JSON for LispWorks

The `JSON` package is a dead-simple [JSON](http://www.json.org) parser for [LispWorks](http://www.lispworks.com).

It makes heavy use of my [LEXER](http://www.github.com/massung/lexer) package, so to understand the code you should start there. But it really is simple.

# Quickstart

There really is only one function to use: `JSON-DECODE`.

	JSON-DECODE (string &OPTIONAL source) ;=> Lisp term

The two parameters are the same parameters that the `LEXER`  function takes. JSON arrays are decoded as Lisp vectors and JSON objects are decoded into associative lists. The JSON literals `true` and `false` are decoded as `T` and `NIL`. The `null` value for JSON is also decoded as the `:NULL` keyword (to distinguish it from `false`).

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

# Code Walk-thru

The [`json-parse-error`](https://github.com/massung/json/blob/master/json.lisp#L30) is the definition of a `JSON-PARSE-ERROR`. What's most important is that it derives from `LEX-ERROR`, which takes a lexer for an `:initarg`. When this error is signaled, it is capable of giving source file and line number information about where the parse error occurred.

The [`*lexer*`](https://github.com/massung/json/blob/master/json.lisp#L34) is a dynamic variable used as the lexer. Since the parse errors are triggered from within the parse grammar, we need to be able to have access to the lexer when creating an instance of `JSON-PARSE-ERROR`. This allows us to do that.

Since JSON has a well-defined, C-like syntax for escaping strings, the [`string-lexer`](https://github.com/massung/json/blob/master/json.lisp#L36) tokenizes strings into characters, handling escape characters as well. The [`unescape-string`](https://github.com/massung/json/blob/master/json.lisp#L46) function just tokenizes a string into an array of characters and formats them back into a Lisp string, properly handling unicode characters.

Our tokenizer for JSON is the [`json-lexer`](https://github.com/massung/json/blob/master/json.lisp#L51). This is the heart of showing off the [LEXER](http://www.github.com/massung/lexer) package. It is a series of regex-style patterns used to tokenize and input string. The token function for this object is sent to the [`json-parser`](https://github.com/massung/json/blob/master/json.lisp#L69)  - a [LispWorks](http://www.lispworks.com) grammar for JSON using the [`PARSERGEN`](http://www.lispworks.com/documentation/lw50/LWRM/html/lwref-433.htm) package.

Finally, the [`json-decode`](https://github.com/massung/json/blob/master/json.lisp#L104) function just pulls everything together, assigning the lexer and then passing the token function to the grammar.
