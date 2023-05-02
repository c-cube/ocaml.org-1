module Status = struct
  type t = Bad | Internal_failure | Not_available | Partial | Good

  let of_string = function
    | "bad" -> Ok Bad
    | "internal-failure" -> Ok Internal_failure
    | "not-available" -> Ok Not_available
    | "partial" -> Ok Partial
    | "good" -> Ok Good
    | str -> Error (`Msg (Printf.sprintf "Unreconized status: %s" str))

  let to_string = function
    | Bad -> "Error"
    | Internal_failure -> "CI not available"
    | Not_available -> "No information"
    | Partial -> "Partial"
    | Good -> "OCaml"
end

let to_string ((v : string), (s : Status.t)) =
  match s with Status.Good -> "OCaml " ^ v | s -> Status.to_string s
