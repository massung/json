# JSON for LispWorks

The `json` package is a dead-simple [JSON](http://www.json.org) parser for [LispWorks](http://www.lispworks.com).

It makes heavy use of my [`lexer`](http://github.com/massung/lexer) and [`re`](http://github.com/massung/re) packages, so to understand the code you should start there. But they are really is simple.

## Quickstart

To convert from JSON to a Lisp object use `JSON-DECODE`.

	(json-decode string &optional source) ;=> value

The two parameters are the same parameters that the `tokenize` function takes in the `lexer` package.

JSON arrays are decoded as Lisp lists and JSON objects are decoded into a `JSON-OBJECT`, which has a single slot: `json-object-member`, an associative list. The JSON literals `true`, `false`, and `null` are decoded as the list keywords `t`, `nil`, and `nil`. All other JSON values should translate directly.

	CL-USER > (json-decode "{\"test\": [1,-4e+3,true]}")
	#<JSON::JSON-OBJECT 224959A7>

	CL-USER > (json-object-members *)
	(("test" (1 -4000 T)))

## Decoding Into a CLOS Object

If you have a class defined, a JSON a-list can be decoded into an instance of the object type.

	(json-decode-into class string &optional source)

For example:

	CL-USER > (defclass login ()
	            ((|user| :initarg :user :initform "guest")
	             (|pass| :initarg :pass :initform "12345")))
	#<STANDARD-CLASS LOGIN 200A80BF>

	CL-USER > (defclass account ()
	            ((|login|   :type login)
	             (|balance| :initform 0)))
	#<STANDARD-CLASS ACCOUNT 200C7BEF>

Now we have defined two classes: `login` and `account`, where an `account` contains a `login`. Using `json-decode-into`, we can take decode JSON directly into a CLOS object instance.

	CL-USER > (json-decode-into 'account "[{\"login\":{\"user\":\"jeff\"}},{\"login\":{\"user\":\"mark\", \"pass\":\"kd93\"}}]")
	(#<ACCOUNT 21F97D1B> #<ACCOUNT 21F97D03>)

Notice how since the JSON was an array, we got back a list of `account` objects. And, if we look inside, we'll see that since the `|login|` slot was declared with `:type login`, it was further decoded as well.

The symbol name of each slot in a class is what's used to decode into an object instance. The `assoc` is performed with `:test #'string=`. This means it's probably better to use `|symbol|` symbols to clearly get the case sensitivity that is desired for each slot name.

## Decoding Into Core Classes

There are a few "special" classes that are handled uniquely:

* `KEYWORD`

The incoming JSON value should be a string. It will be up-cased and interned into the `keyword` package.

	CL-USER > (json-decode-into 'keyword "\"hello\"")
	:HELLO

* `LIST`

If the incoming JSON value is a `json-object`, and all the members are returned as an a-list. Any values in the a-list that are also objects will be recursively returned as a-lists as well.

	CL-USER > (json-decode-into 'list "{\"a\":{\"b\":10}}")
	(("a" (("b" 10))))

* `HASH-TABLE`

The JSON value should be a `json-object` and all the key/value pairs will be returned in a new `hash-table` of type `EQUAL`. Any child values that are objects will also be compiled into hash tables.

	CL-USER > (json-decode-into 'hash-table "{\"a\":{\"b\":10}}")
	#<EQUAL Hash Table{1} 21E4D39F>

*Note: When using `json-decode-into`, if the object that would be returned is not of the type you requested, and error will be signaled.*

## Encoding to JSON

Encoding back to JSON is done using the `json-encode` generic method. Methods have been implemented for numbers, symbols, strings, vectors, lists, and standard objects.

Vectors and lists both encode to arrays. An empty list (`nil`) encodes to `false`. To encode a JSON object, create an instance of an object and all its slots will be encoded to JSON.

	CL-USER > (json-encode (vector 1 t (make-instance 'login :user "jeff" :pass "vy903k#2")))
	"[1,true,{\"user\":\"jeff\",\"pass\":\"vy903k#2\"}]"
