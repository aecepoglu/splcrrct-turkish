let chars_of_string str = 
  let n = String.length str in
  let rec aux i acc =
    if i < n
    then aux (i + 1) (str.[i] :: acc)
    else List.rev acc
  in
    aux 0 []

let combine_safely l1 l2 =
  let rec aux acc = function
  | (h1 :: t1), (h2 :: t2) -> aux ((h1, h2) :: acc) (t1, t2)
  | ([], []) -> Some (List.rev acc)
  | _, _ -> None
  in
    aux [] (l1, l2)

(* __^__                                               __^__
  ( ___ )---------------------------------------------( ___ )
   | / |  Vowels In Turkish                            | \ |
   | / |                                               | \ |
   | / |        Unrounded  Unrounded  Rounded  Rounded | \ |
   | / |        Open       Close      Open     Close   | \ |
   | / | Back   a          ı          o        u       | \ |
   | / | Front  e          i          ö        ü       | \ |
   |___|                                               |___|
  (_____)---------------------------------------------(_____)
*)

(* Unicode Letter *)
module ULetter : sig
  type t
  type vowel_depth = Back | Front | AnyDepth
  type vowel_roundness = Rounded | Unrounded | AnyRoundness
  type vowel_openness = Open | Closed | AnyOpening
  type vowel_categories = vowel_depth * vowel_roundness * vowel_openness
  type complex_t =
      Consonant of t
    | Vowel of t * vowel_categories
  val categorize : t -> vowel_categories
  val of_chars : char list -> t
  val of_string : string -> t
  val to_string : t -> string
  val list_to_string : t list -> string
  val tr_of_en : t -> t
  val en_of_tr : t -> t
  val complexify : t -> complex_t
  val print : Format.formatter -> t -> unit
end = struct
  type t = string

  type vowel_depth = Back | Front | AnyDepth
  type vowel_roundness = Rounded | Unrounded | AnyRoundness
  type vowel_openness = Open | Closed | AnyOpening
  type vowel_categories = vowel_depth * vowel_roundness * vowel_openness

  type complex_t =
      Consonant of t
    | Vowel of t * vowel_categories

  let of_chars x = String.of_seq (List.to_seq x)
  let to_string x = x
  let of_string x = x
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
  
  let categorize = function
  | "a" -> (Back , Unrounded, Open  )
  | "e" -> (Front, Unrounded, Open  )
  | "ı" -> (Back , Unrounded, Closed)
  | "i" -> (Front, Unrounded, Closed)
  | "o" -> (Back , Rounded  , Open  )
  | "ö" -> (Front, Rounded  , Open  )
  | "u" -> (Back , Rounded  , Closed)
  | "ü" -> (Front, Rounded  , Closed)
  | _ -> (AnyDepth, AnyRoundness, AnyOpening)
  
  let complexify c = match c with
  | "a" | "e" | "ı" | "i" | "o" | "ö" | "u" | "ü" -> Vowel (c, categorize c)
  | _ -> Consonant c
  
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

type letter_constraint =
| AfterConsonant

type letter_def =
| JustLetter of ULetter.t
| SuchVowel of ULetter.vowel_categories

let l = ULetter.of_string

let filter_depth = function
| ULetter.AnyDepth -> (fun _ -> true)
| x -> (fun l -> match (ULetter.categorize l) with
       | (x', _, _) -> x = x'
       )

let filter_roundness = function
| ULetter.AnyRoundness -> (fun _ -> true)
| x -> (fun l -> match (ULetter.categorize l) with
       | (_, x', _) -> x = x'
       )

let filter_openness = function
| ULetter.AnyOpening -> (fun _ -> true)
| x -> (fun l -> match (ULetter.categorize l) with
       | (_, _, x') -> x = x'
       )

let get_vowels depth roundness openness =
  ["a";"e";"ı";"i";"o";"ö";"u";"ü"]
  |> List.map ULetter.of_string
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
  | JustLetter l :: tl -> aux (ULetter.to_string l :: acc) tl
  | SuchVowel (depth, roundness, openness) :: tl ->
    aux
      (("("
        ^ (String.concat
             "|"
             (List.map ULetter.to_string (get_vowels depth roundness openness))
          )
        ^ ")"
       ) :: acc) tl
  in
    aux []

let attempt depth0 (lss:ULetter.t list list) (lds:letter_def list) =
  let find (depth1, roundness, openness) depth2 ls =
    ls
    |> List.filter (filter_depth depth1)
    |> List.filter (filter_depth depth2)
    |> List.filter (filter_roundness roundness)
    |> List.filter (filter_openness openness)
    |> (function
       | h :: _ -> Some h
       | [] -> None
       )
  in
  let rec aux depth acc = function
  | [] -> Some (List.rev acc)
  | (JustLetter l, ls) :: tl -> (if List.mem l ls
                                 then aux depth (l :: acc) tl
                                 else None
                                )
  | (SuchVowel (depth', _, _ as cats), ls) :: tl -> (match find cats depth ls with
                                  | Some l -> aux depth' (l :: acc) tl
                                  | None -> None
                                  )
  in
    (match combine_safely lds lss with
    | Some zipped -> aux depth0 [] zipped
    | None -> None
    )

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

let suggest_words (words:BasicWordDb.t) suffixes (w:string) :string list =
  let find_last_vowel ls =
    let rec aux = function
    | h :: t -> (match ULetter.complexify h with
                | Vowel (_, x) -> Some x
                | _ -> aux t
                )
    | [] -> None
    in
      aux (List.rev ls)
  in
  let letters =  w
                 |> UWord.of_string
                 |> UWord.to_letters in
  let _letter_combos = make_letter_variations letters in
    build_all_combos _letter_combos
    |> List.filter (fun (fst, _) -> BasicWordDb.contains words fst)
    |> List.map (fun (fst, snd) -> 
                  let _ends_in_consonant = fst
                                          |> List.rev
                                          |> (fun l -> List.nth l 0)
                                          |> ULetter.complexify
                                          |> (function
                                             | ULetter.Consonant _ -> true
                                             | _ -> false
                                             ) in
                  let (last_depth, _, _) = find_last_vowel fst
                                              |> (function
                                                 | Some x -> x
                                                 | None -> (AnyDepth, AnyRoundness, AnyOpening)
                                                 )
                                                in
                  let str = ULetter.list_to_string fst in
                    List.filter_map (attempt last_depth snd) suffixes
                    |> List.map ULetter.list_to_string
                    |> List.map (fun x -> str ^ x)
                )
    |> List.flatten
    |> List.sort String.compare
    |> (function
       | [] -> [w]
       | l  -> l
       )

let words_list = ["bal"; "cin"; "çin"; "öküz"; "oğul"]
let words = BasicWordDb.of_strings words_list
let suffixes =  [
  [];
  [
    JustLetter (l "l");
    SuchVowel (AnyDepth, Unrounded, Open);
    JustLetter (l "r")
  ];
  [
    SuchVowel (AnyDepth, Unrounded, Open);
  ];
  [
    SuchVowel (AnyDepth, AnyRoundness, Closed);
  ];
  (*[
    Optional (AfterConsonant, SuchVowel (AnyDepth, AnyRoundness, Closed));
    JustLetter (l "m");
    SuchVowel (AnyDepth, AnyRoundness, Closed);
    JustLetter (l "z")
    ]*)
]
let suggest = suggest_words words suffixes

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
    test_suggest "cin"     ["cin"; "çin"];
    test_suggest "ogul"    ["oğul"];
    test_suggest "ogullar" ["oğullar"];
    test_suggest "okuzler" ["öküzler"];
    test_suggest "bali"    ["balı"];
    test_suggest "cini"    ["cini"; "çini"];
    test_suggest "ogulu"   ["oğulu"];
    test_suggest "okuzu"   ["öküzü"];
    test_suggest "ogula"   ["oğula"];
    test_suggest "okuze"   ["öküze"];
    (* halim, 
    *)
    test_suggest "unknown" ["unknown"];

