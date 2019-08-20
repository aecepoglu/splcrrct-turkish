module Letter : sig
  type t
  val of_chars : char list -> t
  val to_bytes : t -> Bytes.t list
end = struct
  type t = char list
  let of_chars x = x
  let to_bytes x = List.map (Bytes.make 1) x
end

module Uword : sig
  type t
  val of_string : string -> t
  val to_string : t -> string
  val rev : t -> t
  val to_letters : t -> Letter.t list
  val of_letters : Letter.t list -> t
end = struct
  type t = Letter.t list

  let of_string str =
    let n = String.length str in
    let rec aux i j cur acc =
      if i >= n
      then List.rev acc
      else if j > 0
      then (
        let cur' = str.[i] :: cur in
          if j = 1
          then aux (i + 1) (j - 1) [] ((Letter.of_chars (List.rev cur')) :: acc)
          else aux (i + 1) (j - 1) cur' acc
      )
      else (
        let c = str.[i] in
          if      c < '\x80' then aux i 1 [] acc
          else if c < '\xC0' then invalid_arg "char point < xC0"
          else if c < '\xE0' then aux i 2 [] acc
          else if c < '\xF0' then aux i 3 [] acc
          else if c < '\xF8' then aux i 4 [] acc
          else if c < '\xFC' then aux i 5 [] acc
          else if c < '\xFE' then aux i 6 [] acc
          else                    invalid_arg "char point >= xFE"
      )
    in
      aux 0 0 [] []

  let to_string letters = letters
                          |> List.map (Letter.to_bytes)
                          |> List.flatten
                          |> Bytes.concat Bytes.empty
                          |> Bytes.to_string

  let rev x = List.rev x

  let to_letters x = x
  let of_letters x = x
end

let cond_suffixes = ["a"; "e";
                     "i"; "u";
                     "da"; "de";
                     "dan"; "den"]

let posession_suffixes = ["(iu)m";
                          "(iu)n";
                          "(yssn)i"; "(yssn)u";
                          "(iu)m(iu)z";
                          "(iu)n(iu)z";
                          "lari"; "leri"]

let words = ["ak"; "akil"; "ag"; "agda"]

let rec starts_remainders a b acc = match a, b with
| h_a :: t_a, h_b :: t_b when h_a = h_b -> starts_remainders t_a t_b (h_a :: acc)
| x, [] -> Some (x, acc)
| _, _ -> None

let string_of_letters x = x |> Uword.of_letters |> Uword.to_string

let suggest_words (word:string) :string list =
  let _suffixes' = cond_suffixes
                   |> List.map Uword.of_string
                   |> List.map Uword.rev in
  let _word' = word |> Uword.of_string |> Uword.rev |> Uword.to_letters in
    List.to_seq _suffixes'
    |> Seq.map Uword.to_letters
    |> Seq.filter_map (fun x -> starts_remainders _word' x [])
    |> Seq.map (fun (a, b) -> string_of_letters (List.rev a)
                              ^ "-"
                              ^ string_of_letters b)
    |> List.of_seq

let () =
  print_endline "done";
