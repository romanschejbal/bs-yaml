type value = [
  | `Null
  | `Bool(bool)
  | `Float(float)
  | `String(string)
  | `Array(list(value))
  | `Object(list((string, value)))
];

[@bs.module "js-yaml"]
external yamlParse: (string, ~options: 'a=?, unit) => Js.Json.t = "safeLoad";
[@bs.module "js-yaml"]
external yamlStringify: (Js.Json.t, ~options: 'a=?, unit) => string =
  "safeDump";

let parseWithOpts = (str, options) => {
  let json =
    switch (yamlParse(str, ~options, ())) {
    | exception _ => Js.Json.null
    | x => x
    };

  let rec p = json =>
    switch (Js.Json.classify(json)) {
    | Js.Json.JSONFalse => `Bool(false)
    | Js.Json.JSONTrue => `Bool(true)
    | Js.Json.JSONNull => `Null
    | Js.Json.JSONNumber(n) => `Float(n)
    | Js.Json.JSONString(s) => `String(s)
    | Js.Json.JSONArray(arr) => `Array(Array.map(p, arr) |> Array.to_list)
    | Js.Json.JSONObject(o) when Js.typeof(o) != "undefined" =>
      `Object(
        Js.Dict.entries(o)
        |> Array.map(((key, value)) => (key, p(value)))
        |> Array.to_list,
      )
    | Js.Json.JSONObject(_) => `Null
    };

  p(json);
};

let parse = str => parseWithOpts(str, None);

let stringifyWithOpts = (yaml, options) => {
  let rec s = yaml =>
    switch (yaml) {
    | `Bool(value) => Js.Json.boolean(value)
    | `Float(number) => Js.Json.number(number)
    | `String(str) => Js.Json.string(str)
    | `Array(arr) => List.map(s, arr) |> Array.of_list |> Js.Json.array
    | `Object(list) =>
      Js.Dict.fromList(List.map(((key, value)) => (key, s(value)), list))
      |> Js.Json.object_
    | `Null => Js.Json.null
    };
  s(yaml)
  |> (
    json =>
      switch (Js.Json.classify(json)) {
      | Js.Json.JSONNull => ""
      | _ => yamlStringify(json, ~options, ())
      }
  );
};

let stringify = yaml => stringifyWithOpts(yaml, None);