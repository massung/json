# JSON for LispWorks

The `json` package is a dead-simple [JSON](http://www.json.org) parser for Common Lisp. It makes heavy use of my [`re`](http://github.com/massung/re), [`lexer`](http://github.com/massung/lexer), and [`parse`](http://github.com/massung/parse) packages. So to understand the code you should start there. But they are really is simple.

## Quickstart

Most of the time, you'll be interfacing with two functions:

    (json-decode string &optional source)
    (json-encode value &optional stream)

Using `json-decode`, you can parse a string into a Lisp value.

    CL-USER > (json-decode "10")
    10

    CL-USER > (json-decode "[\"Hello, world!\", 2, true, false]")
    ("Hello, world!" 2 T NIL)

    CL-USER > (json-decode "{ \"foo\" : \"bar\" }")
    #<JSON-OBJECT {"foo":"bar"}>

Similarly, you can encode a Lisp value into JSON.

    CL-UER > (json-encode '(1 2 abc 1/2 "hello"))
    [1,2,"ABC",0.5,"hello"]
    NIL

Many more types [of Lisp objects] are supported for encoding than are for decoding. When decoding, all JSON values are assumed to be string, numbers, lists (arrays), T, nil, or a JSON object. As you can see from above, the symbol `abc` was coerced to a string before being encoded.

All characters, pathnames, and symbols are coerced to strings. All real numeric types are acceptable. All sequences (that aren't strings) are coerced to JSON arrays. Hash tables and `json-object` instances are written out as JSON objects.

## Accessing JSON-OBJECTs

If a `json-object` is decoded (or you create one), the helper functions `json-getf` and `json-setf` can be used to access the members of the object.

    (json-getf object key &optional value)
    (json-setf object key value)

The `json-getf` function behaves similarly to `gethash`. If the key is present, then its value and T are returned. Otherwise `value` is returned along with NIL to indicate the key was not found.

While you can call `json-setf` yourself, `json-getf` is setf-able, and does the same thing. If the key is already present, then its value is overwritten, otherwise a new key/value pair is added to the object.

    CL-USER > (json-decode "{\"foo\":\"bar\"}")
    #<JSON-OBJECT {"foo":"bar"}>

    CL-USER > (setf (json-getf * "foo") 'foobar
                    (json-getf * "hello") "world")
    "world"

    CL-USER > **
    #<JSON-OBJECT {"hello":"world","foo":"FOOBAR"}>

That's it!
