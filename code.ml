(* Unicode Letter *)
module Letter (*: sig
  type t
  val of_chars : char list -> t
  val to_bytes : t -> Bytes.t list
  val list_to_string : t list -> string
end *)= struct
  type t = char list
  let of_chars x = x
  let to_bytes x = List.map (Bytes.make 1) x
  let list_to_string xs = xs
                     |> List.map to_bytes
                     |> List.flatten
                     |> Bytes.concat Bytes.empty
                     |> Bytes.to_string
end

let letter_tr_i = Letter.of_chars ['\196'; '\177']
let letter_tr_u = Letter.of_chars ['\195'; '\188']
let letter_tr_o = Letter.of_chars ['\195'; '\182']
let letter_tr_g = Letter.of_chars ['\196'; '\159']
let letter_tr_s = Letter.of_chars ['\197'; '\159']
let letter_tr_c = Letter.of_chars ['\195'; '\167']

let enletter_of_trletter = function
| l when l = letter_tr_i -> Letter.of_chars ['i']
| l when l = letter_tr_u -> Letter.of_chars ['u']
| l when l = letter_tr_o -> Letter.of_chars ['o']
| l when l = letter_tr_g -> Letter.of_chars ['g']
| l when l = letter_tr_s -> Letter.of_chars ['s']
| l when l = letter_tr_c -> Letter.of_chars ['c']
| l -> l

type vowel_depth = BackVowel | FrontVowel
type vowel_width = ThinVowel | WideVowel

let thin_vowels = [letter_tr_i;
                   Letter.of_chars ['i'];
                   letter_tr_u;
                   Letter.of_chars ['u'];
                  ]

let chars_of_string str = 
  let n = String.length str in
  let rec aux i acc =
    if i < n
    then aux (i + 1) (str.[i] :: acc)
    else List.rev acc
  in
    aux 0 []


(* Unicode Word *)
module Uword (*: sig
               type t
               val of_string : string -> t
               val to_string : t -> string
               val rev : t -> t
               val to_letters : t -> Letter.t list
               val of_letters : Letter.t list -> t
               end*) = struct
  type t = Letter.t list

  let of_string str =
    let rec aux2 i cur acc = function
    | []                        -> List.rev acc
    | c :: c_tl when i > 0      -> (let cur' = c :: cur in
                                      if i = 1
                                      then aux2 0 []
                                             ((Letter.of_chars (List.rev cur')) :: acc)
                                             c_tl
                                      else aux2 (i - 1) cur'
                                             acc
                                             c_tl
                                   )
    | c :: c_tl as cs when c < '\x80' -> aux2 1 [] acc cs
    | c :: _          when c < '\xC0' -> invalid_arg "char point < xC0"
    | c :: c_tl as cs when c < '\xE0' -> aux2 2 [] acc cs
    | c :: c_tl as cs when c < '\xF0' -> aux2 3 [] acc cs
    | c :: c_tl as cs when c < '\xF8' -> aux2 4 [] acc cs
    | c :: c_tl as cs when c < '\xFC' -> aux2 5 [] acc cs
    | c :: c_tl as cs when c < '\xFE' -> aux2 6 [] acc cs
    | _                         -> invalid_arg "char point >= xFE"
    in
      aux2 0 [] [] (chars_of_string str)

  let to_string = Letter.list_to_string

  let rev x = List.rev x

  let to_letters x = x
  let of_letters x = x
end

module type WORD_DB = sig
  type t
  val of_strings : string list -> t
  val find : t -> Letter.t list -> Letter.t list option
end

module BasicWordDb : WORD_DB = struct
  type t = (Letter.t list, Letter.t list) Hashtbl.t

  let of_strings ws =
    let tbl = Hashtbl.create (List.length ws) in
    let () = List.to_seq ws
             |> Seq.map Uword.of_string
             |> Seq.map Uword.to_letters
             |> Seq.iter (fun ls -> Hashtbl.add tbl
                                      (List.map enletter_of_trletter ls)
                                      ls
                         )
    in
      tbl

  let find = Hashtbl.find_opt
end

type ek = Ek of Uword.t
let letters_of_ek = function
| Ek w -> Uword.to_letters w


module type SUFFIXES = sig
  type t
  val from : ek list -> t
  val find_all : t -> Letter.t list -> (ek list * Letter.t list) list
end

let withrmd_starts_with word prefix =
  let rec aux acc = function
  | a :: t_a, b :: t_b when a = b -> aux (a :: acc) (t_a, t_b)
  | x, [] -> Some (List.rev acc, x)
  | _, _ -> None
  in
    aux [] (word, prefix)

module SuffixList = struct
  type t = (Letter.t list * ek) list
  let from eks =
    List.map (function
             | Ek w as e -> List.rev (Uword.to_letters w), e
             ) eks
  let find_all t pattern =
    let rec aux results all_results = function
    | (eks, pat) :: pat_rest -> 
      let results = List.filter_map (fun (letters, ek) ->
                                      withrmd_starts_with pat letters
                                      |> Option.map (fun (suf, rmd) -> ek :: eks, rmd)
                                    ) t
      in
        aux results all_results pat_rest
    | [] -> (match results with
            | [] -> all_results
            | l -> (aux [] (l @ all_results) l)
            )
    in
      aux [] [] [[], List.rev pattern]
end

let suggest_words suffixes words (word:string) :string list =
  let word' = word |> Uword.of_string |> Uword.to_letters in
    SuffixList.find_all suffixes word'
    |> (function
       | [] -> [[], List.rev word']
       | x -> x
       )
    |> List.map (fun (eks, w) -> List.rev w,
                                 List.map letters_of_ek eks
                )
    |> List.filter_map (fun (w, ss) -> match BasicWordDb.find words w with
                       | Some w' -> Some (w', ss)
                       | None -> None
                       )
    |> List.map (fun (w, ss) -> Letter.list_to_string w
                                ^ "-"
                                ^ (ss
                                   |> List.map Letter.list_to_string
                                   |> String.concat "-"
                                  ))
    |> (function
       | results when not (List.mem word results) -> results @ [word]
       | x -> x
       )

let test expected received msg =
  if expected = received
  then ()
  else print_endline msg

let abc = thin_vowels, ["_m"; "_n"; "(yssn)_"; "(_)m_z"; "(_)n_z"]
                       |> List.map chars_of_string

let suffix_strs = ["lar";
                   "dan"; "den";
                   "(i)m"; "(i)n"; "(yssn)i"; "(i)miz"; "(i)niz"]
let words_list = ["akıl"; "ev"; "bal"]
let words = BasicWordDb.of_strings words_list
let suffixes' = suffix_strs
                |> List.map Uword.of_string
                |> List.map (fun x -> Ek x)
                |> SuffixList.from

let () =
  let suggest = suggest_words suffixes' words in
  let test_suggest a b =
    test (suggest a) b ("Failed " ^ a)
  in
    assert (withrmd_starts_with [1; 2; 3; 4] [1; 2] = Some ([1; 2], [3; 4]));
    test_suggest "akildan"    ["akıl-dan"; "akildan"];
    test_suggest "akillardan" ["akıl-lar-dan"; "akillardan"];
    test_suggest "evden"      ["ev-den"; "evden"];
    test_suggest "akil"       ["akıl-"; "akil"];
    test_suggest "balim"      ["bal-ım"; "balim"];
    (*test_suggest "evim"       ["ev-im"; "evim"];*)
    (*test_suggest "okulum"     ["okul-um"; "okulum"];*)
    (*test_suggest "okuzum"     ["okuz-um"; "okuzum"];*)
    test_suggest "unknown"    ["unknown"];
