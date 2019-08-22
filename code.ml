let cond_suffixes = ["lar"; "ler"; "da"; "dan"]

let posession_suffixes = ["(iu)m";
                          "(iu)n";
                          "(yssn)i"; "(yssn)u";
                          "(iu)m(iu)z";
                          "(iu)n(iu)z";
                          "lari"; "leri"]

let words = ["ak"; "akil"; "ag"; "agda"]

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

module type SUFFIXES = sig
  type t
  val from : Uword.t list -> t
  val find_all : t -> Letter.t list -> (Letter.t list * Letter.t list) list
end

let withrmd_starts_with ?(acc0=[]) word prefix =
  let rec aux acc = function
  | a :: t_a, b :: t_b when a = b -> aux (a :: acc) (t_a, t_b)
  | x, [] -> Some (List.rev acc, x)
  | _, _ -> None
  in
    aux (List.rev acc0) (word, prefix)

module SuffixList : SUFFIXES = struct
  type t = Letter.t list list
  let from x =
    x
    |> List.map Uword.to_letters
    |> List.map List.rev
  let find_all t pattern =
    let rec aux results all_results = function
    | pat :: pat_tl -> 
      let results = List.filter_map (withrmd_starts_with pat) t in
        aux results all_results pat_tl
    | [] -> (match results with
            | [] -> all_results
            | l -> (aux [] (l @ all_results) (List.map snd l))
            )
    in
      aux [] [] [List.rev pattern]
end

let string_of_letters x = x |> Uword.of_letters |> Uword.to_string

let suggest_words suffixes (word:string) :string list =
  let suffixes = SuffixList.from (List.map Uword.of_string cond_suffixes) in
  let word' = word |> Uword.of_string |> Uword.to_letters in
    SuffixList.find_all suffixes word'
    |> List.map (fun (a, b) -> List.rev a, List.rev b)
    |> List.map (fun (a, b) -> string_of_letters b ^ "-" ^ string_of_letters a)

let () =
  let _suffixes = cond_suffixes
                  |> List.map Uword.of_string
                  |> SuffixList.from in
  assert (withrmd_starts_with [1; 2; 3; 4] [1; 2] = Some ([1; 2], [3; 4]));
  assert (suggest_words _suffixes "akildan" = ["akil-dan"]);
  assert (suggest_words _suffixes "akillardan" = ["akil-lar-dan"]);
  print_endline "done";
