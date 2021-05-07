# JSON for Common Lisp

The `json` package is a dead-simple [JSON](http://www.json.org) parser for Common Lisp.

It is very quick and I personally use it to parse extremely large, genomics JSON-list files (several GB in size).

## Quickstart

Most of the time, you'll be interfacing with two functions:

    (json-decode string &key start end)
    (json-encode value &optional stream)

Using `json-decode`, you can parse a string into a Lisp value. It returns two values: the decoded value and the position within the string at which decoding stopped.

    CL-USER > (json-decode "10")
    10
    2

    CL-USER > (json-decode "[\"Hello, world!\", 2, true, false]")
    ("Hello, world!" 2 T NIL)
    33

    CL-USER > (json-decode "{ \"foo\" : \"bar\" }")
    #<JSON-OBJECT {"foo":"bar"}>
    17

Similarly, you can encode a Lisp value into JSON.

    CL-UER > (json-encode '(1 2 abc 1/2 "hello"))
    [1,2,"ABC",0.5,"hello"]
    NIL

Many more Lisp types are supported for encoding than decoding. When decoding, all JSON values are assumed to be string, numbers, lists (arrays), T, nil, or a `json-object`. As you can see from above, the symbol `abc` was coerced to a string before being encoded.

Similarly, when decoding, it is not possible to distinguish between `false`, `null`, or `[]`. And, I have personally never found this to be problematic.

All characters, pathnames, and symbols are coerced to strings. All real numeric types are acceptable. All sequences (that aren't strings) are coerced to JSON arrays. Hash tables and `json-object` instances are written out as JSON objects.

## Reading JSON from a Stream

The `json-decode` function is actually just a wrapper around `json-read`, which reads a Lisp value from a JSON input stream. It can be quite useful to use `json-read` to parse without first reading into a string or when decoding multiple JSON values from the same input (e.g. a JSON-list file).

    CL-USER > (with-input-from-string (s "10 true 30")
                (print (json-read s))
                (print (json-read s))
                (print (json-read s)))
    10
    T
    30

The `json-read` function - like all read functions - allows for optional signalling of end of file errors and EOF values:

    (json-read stream &optional eof-error-p eof-value)

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

## JSON Reader Macro

There is also a reader macro that can be enabled, which allows for easy, literal creation of JSON objects:

    CL-USER > (json-enable-reader-macro)
    T

    CL-USER > #{:foo 10 :bar "Hello, world!"}
    #<JSON-OBJECT {"FOO":10,"BAR":"Hello, world!"}>

That's it!

## Special Thanks

A big "thank you" here goes to Kaveh Yousefi for finding a bug and providing the fix!
