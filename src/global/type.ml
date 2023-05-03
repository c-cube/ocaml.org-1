module Build = struct
  type status = Bad | Internal_failure | Not_available | Partial | Good

  let status_of_string = function
    | "bad" -> Ok Bad
    | "internal-failure" -> Ok Internal_failure
    | "not-available" -> Ok Not_available
    | "partial" -> Ok Partial
    | "good" -> Ok Good
    | str -> Error (`Msg (Printf.sprintf "Unreconized status: %s" str))

  let status_to_string = function
    | Bad -> "Error"
    | Internal_failure -> "CI not available"
    | Not_available -> "No information"
    | Partial -> "Partial"
    | Good -> "OCaml"

  let to_string ((v : string), (s : status)) =
    match s with Good -> "OCaml " ^ v | s -> status_to_string s
end
