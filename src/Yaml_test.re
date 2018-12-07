open Jest;
open Expect;
open Yaml;

test("Boolean", () =>
  parse("true") |> expect |> toEqual(Bool(true))
);

test("Boolean", () =>
  parse("false") |> expect |> toEqual(Bool(false))
);

test("Float", () =>
  parse("1") |> expect |> toEqual(Float(1.))
);

test("String", () =>
  parse("iamstring") |> expect |> toEqual(String("iamstring"))
);

test("Array", () =>
  parse("- single item in an array")
  |> expect
  |> toEqual(Array([String("single item in an array")]))
);

test("Array", () =>
  parse("- hello\n- aloha")
  |> expect
  |> toEqual(Array([String("hello"), String("aloha")]))
);

test("Object", () =>
  parse("replicas: 1")
  |> expect
  |> toEqual(Object([("replicas", Float(1.))]))
);

test("More complex object", () =>
  parse("first: 1\nsecond:\n - 1\n - str\nthird:\n here: 2\n there: true")
  |> expect
  |> toEqual(
       Object([
         ("first", Float(1.)),
         ("second", Array([Float(1.), String("str")])),
         ("third", Object([("here", Float(2.)), ("there", Bool(true))])),
       ]),
     )
);

test("Stringify Float", () =>
  stringify(Float(10.)) |> expect |> toEqual("10\n")
);

test("Stringify String", () =>
  stringify(String("ola")) |> expect |> toEqual("ola\n")
);

test("Stringify Array", () =>
  stringify(Array([Float(1.), Float(2.)]))
  |> expect
  |> toEqual("- 1\n- 2\n")
);

test("Stringify Object with options", () =>
  stringifyWithOpts(
    Object([
      ("second", Array([Float(1.), String("str")])),
      ("first", Float(1.)),
      ("third", Object([("here", Float(2.))])),
    ]),
    {"sortKeys": true},
  )
  |> expect
  |> toEqual("first: 1\nsecond:\n  - 1\n  - str\nthird:\n  here: 2\n")
);

test("Comments", () =>
  parse("test: more\n# what\nup: 1\n")
  |> stringify
  |> expect
  |> toEqual("test: more\nup: 1\n")
);

test("Empty yaml", () =>
  parse("") |> stringify |> expect |> toEqual("")
);

test("Only one comment yaml", () =>
  parse("# comment") |> stringify |> expect |> toEqual("")
);