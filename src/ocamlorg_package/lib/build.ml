open Ocamlorg.Import

let error kind str = Error (`Msg (Printf.sprintf "Unreconized %s: %s" kind str))

module Status = struct
  type t = Bad | Internal_failure | Not_available | Partial | Good

  let of_string = function
    | "bad" -> Ok Bad
    | "internal-failure" -> Ok Internal_failure
    | "not-available" -> Ok Not_available
    | "partial" -> Ok Partial
    | "good" -> Ok Good
    | str -> error "status" str

  let to_string = function
    | Bad -> "Build error"
    | Internal_failure -> "CI not available"
    | Not_available -> "No information"
    | Partial -> "Partial"
    | Good -> "Success"
end

let ( let* ) = Result.bind
let ( <@> ) f = Result.fold ~ok:Result.map ~error:(fun e _ -> Error e) f
let ok_cons f x u = Ok List.cons <@> f x <@> u

module Json = struct
  let error kind json = error kind (Yojson.Safe.to_string json)

  let to_build = function
    | `Assoc [ ("compiler", `String version); ("status", `String status) ] ->
        let* status = Status.of_string status in
        Ok (version, status)
    | json -> error "build" json

  let to_release = function
    | `Assoc [ ("name", `String release); ("statuses", `List builds) ] ->
        let* builds = List.fold_right (ok_cons to_build) builds (Ok []) in
        Ok (release, builds)
    | json -> error "release" json

  let to_repo = function
    | `List releases ->
        List.fold_left (Fun.flip (ok_cons to_release)) (Ok []) releases
    | json -> error "repo" json

  let of_string json =
    let of_list u = u |> List.to_seq |> String.Map.of_seq in
    json |> to_repo |> Result.map of_list
end
