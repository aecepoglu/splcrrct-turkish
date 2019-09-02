let chars_of_string str = 
  let n = String.length str in
  let rec aux i acc =
    if i < n
    then aux (i + 1) (str.[i] :: acc)
    else List.rev acc
  in
    aux 0 []

(* Unicode Letter *)
module ULetter : sig
  type t
  val of_chars : char list -> t
  val to_string : t -> string
  val list_to_string : t list -> string
  val tr_of_en : t -> t
  val en_of_tr : t -> t
  val print : Format.formatter -> t -> unit
end = struct
  type t = string
  let of_chars x = x
                   |> List.map (Bytes.make 1)
                   |> Bytes.concat Bytes.empty
                   |> Bytes.to_string
  let to_string x = x
  
  let list_to_string xs = xs
                          |> List.map to_string
                          |> String.concat ""
  
  let tr_of_en = function
  | "i" -> "ı"
  | "o" -> "ö"
  | "u" -> "ü"
  | "c" -> "ç"
  | "g" -> "ğ"
  | x -> x

  let en_of_tr = function
  | "i" -> "ı"
  | "ö" -> "o"
  | "ü" -> "u"
  | "ç" -> "c"
  | "ğ" -> "g"
  | x -> x
  
  let print fmt x = Format.printf "'%s'" (to_string x)
end

(* Unicode Word *)
module UWord : sig
  type t
  val of_string : string -> t
  val to_string : t -> string
  val to_letters : t -> ULetter.t list
  val of_letters : ULetter.t list -> t
  val print : Format.formatter -> t -> unit
end = struct
  type t = ULetter.t list

  let of_string str =
    let rec aux i cur acc = function
    | []                        -> List.rev acc
    | c :: c_tl when i > 0      -> (let cur' = c :: cur in
                                      if i = 1
                                      then aux 0 []
                                             ((ULetter.of_chars (List.rev cur')) :: acc)
                                             c_tl
                                      else aux (i - 1) cur'
                                             acc
                                             c_tl
                                   )
    | c :: c_tl as cs when c < '\x80' -> aux 1 [] acc cs
    | c :: _          when c < '\xC0' -> invalid_arg "char point < xC0"
    | c :: c_tl as cs when c < '\xE0' -> aux 2 [] acc cs
    | c :: c_tl as cs when c < '\xF0' -> aux 3 [] acc cs
    | c :: c_tl as cs when c < '\xF8' -> aux 4 [] acc cs
    | c :: c_tl as cs when c < '\xFC' -> aux 5 [] acc cs
    | c :: c_tl as cs when c < '\xFE' -> aux 6 [] acc cs
    | _                         -> invalid_arg "char point >= xFE"
    in
      aux 0 [] [] (chars_of_string str)

  let to_string = ULetter.list_to_string
  
  let print fmt ls =
    Format.print_char '[';
    List.iteri (fun i l ->
                 Format.print_string (if i > 0 then "; " else "");
                 ULetter.print fmt l
               )
      ls;
    Format.print_char ']'

  let rev x = List.rev x

  let to_letters x = x
  let of_letters x = x
end

module type WORD_DB = sig
  type t
  val of_strings : string list -> t
  val contains : t -> ULetter.t list -> bool
end

module BasicWordDb : WORD_DB = struct
  type t = (ULetter.t list, bool) Hashtbl.t

  let of_strings ws =
    let tbl = Hashtbl.create (List.length ws) in
    let () = List.to_seq ws
             |> Seq.map UWord.of_string
             |> Seq.map UWord.to_letters
             |> Seq.iter (fun ls -> Hashtbl.add tbl ls true)
    in
      tbl

  let contains = Hashtbl.mem
end

(* __^__                                               __^__
  ( ___ )---------------------------------------------( ___ )
   | / |  Vowels In Turkish                            | \ |
   | / |                                               | \ |
   | / |        Unrounded  Unrounded  Rounded  Rounded | \ |
   | / |        Open       Close      Open     Close   | \ |
   | / | Back   a          ı          o        u       | \ |
   | / | Front  e          i          ö        ü       | \ |
   |___|                                               |___|
  (_____)---------------------------------------------(_____) *)

type letter_constraint =
| AfterConsonant

type vowel_depth = Back | Front | AnyDepth
type vowel_roundness = Rounded | Unrounded | AnyRoundness
type vowel_openness = Open | Closed | AnyOpening

type ruly_letter =
| JustLetter of string
| Vowel of vowel_depth * vowel_roundness * vowel_openness
| Optional of letter_constraint * ruly_letter


let build_suffixes () =
  let x = [
    [
      JustLetter "l";
      Vowel (AnyDepth, Unrounded, Open);
      JustLetter "r"
    ];
    [
      Optional (AfterConsonant, Vowel (AnyDepth, AnyRoundness, Closed));
      JustLetter "m";
      Vowel (AnyDepth, AnyRoundness, Closed);
      JustLetter "z"
    ]
  ] in
    x

let is_vowel = function
|  "a" | "e" | "ı" | "i" | "o" | "ö" | "u" | "ü" -> true
| _ -> false

let is_consonant x = not (is_vowel x)

let filter_roundness = function
| AnyRoundness -> (fun _ -> true)
| Rounded -> (function
             | "o" | "ö" | "u" | "ü" -> true
             | _ -> false
             )
| Unrounded -> (function
               |  "a" | "e" | "ı" | "i" -> true
               | _ -> false
               )

let filter_depth = function
| AnyDepth -> (fun _ -> true)
| Back -> (function
          | "a" | "ı" | "o" | "u" -> true
          | _ -> true
          )
| Front -> (function
           |"e" |"i" | "ö" | "ü" -> true
           | _ -> true
           )

let filter_openness = function
| AnyOpening -> (fun _ -> true)
| Open -> (function
          | "a" | "e" | "o" | "ö" -> true
          | _ -> false
          )
| Closed -> (function
            | "ı" | "i" | "u" | "ü" -> true
            | _ -> false
            )

let get_vowels depth roundness openness =
  ["a";"e";"ı";"i";"o";"ö";"u";"ü"]
  |> List.filter (filter_depth depth)
  |> List.filter (filter_roundness roundness)
  |> List.filter (filter_openness openness)

let make_letter_variations ls =
  ls 
  |> List.map (fun c ->
                let c' = ULetter.tr_of_en c in
                  if c = c'
                  then [c]
                  else [c; c']
              )

let gen_all_combos (ass:'a list list) :'a list list= 
  let rec aux pre = function
  | [hd] :: tl           -> aux (hd :: pre) tl
  | (h_hd :: h_tl) :: tl -> aux pre ([h_hd] :: tl)
                            @ aux pre (h_tl :: tl)
  | [] :: tl             -> aux pre tl
  | []                   -> [List.rev pre]
  in
    aux [] ass

let debugstring_of_letters = 
  let rec aux acc = function
  | [] -> String.concat "" (List.rev acc)
  | JustLetter s :: tl -> aux (s :: acc) tl
  | Vowel (depth, roundness, openness) :: tl ->
    aux
      (("("
        ^ (String.concat
             "|"
             (get_vowels depth roundness openness)
          )
        ^ ")"
       ) :: acc) tl
  | Optional (_constraint, v) :: tl -> aux (("?" ^ (aux [] [v])) :: acc) tl
  in
    aux []

let string_of_letters ls =
  let rec aux acc = function
  | [] -> List.rev acc
  | JustLetter s :: tl -> aux ([s] :: acc) tl
  | Vowel (depth, roundness, openness) :: tl ->
    aux 
      ((get_vowels depth roundness openness) :: acc) tl
  | Optional (_constraint, v) :: tl -> aux ((aux [] [v]) @ acc) tl
  in
    aux [] ls
    |> gen_all_combos
    |> List.map (String.concat "")

let build_all_combos lss =
  let rec aux pre sols = function
  | [h_hd] :: tl -> 
    let pre' = h_hd :: pre in
      aux pre' ((pre', tl) :: sols) tl
  | (h_hd :: h_tl) :: tl ->
    let sols' = aux pre sols (h_tl :: tl) in
      aux pre sols' ([h_hd] :: tl)
  | [] :: tl -> sols
  | [] -> sols
  in
    aux [] [] lss
    |> List.map (fun (a, b) -> List.rev a, b)

let suggest_words (words:BasicWordDb.t) (w:string) :string list =
  let _suffixes = build_suffixes ()
                  |> List.map string_of_letters
                  |> List.flatten
                  |> List.cons "" in
  let letters =  w
                 |> UWord.of_string
                 |> UWord.to_letters in
  let _letter_combos = make_letter_variations letters in
    build_all_combos _letter_combos
    |> List.filter (fun (fst, _) -> BasicWordDb.contains words fst)
    |> List.map (fun (fst, snd) -> 
                  let str = ULetter.list_to_string fst in
                    snd
                    |> gen_all_combos
                    |> List.map ULetter.list_to_string
                    |> List.filter (fun x -> List.mem x _suffixes)
                    |> List.map (fun x -> str ^ x)
                )
    |> List.flatten
    |> List.sort String.compare

let words_list = ["akıl"; "cin"; "çin"; "öküz"; "ev"; "oğul"]
let words = BasicWordDb.of_strings words_list
let suggest = suggest_words words

let () =
  let test_suggest x y =
    let y' = suggest x in
      (if y <> y'
       then (
         Printf.printf
           "For \"%s\"; Expected [%s], got [%s]"
           x
           (String.concat ";" y)
           (String.concat ";" y')
       )
       else (
         Printf.printf "\"%s\" OK" x
       )
      );
      print_endline ""
  in
    test_suggest "cin" ["cin"; "çin"];
    test_suggest "ogul" ["oğul"];
    test_suggest "akillar" ["akıllar"];
    test_suggest "evler"   ["evler"];
    test_suggest "okuzler" ["öküzler"];
    test_suggest "unknown" ["unknown"];

