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

let find_opt_map f =
   let rec aux = function
   | h :: t -> (match f h with
               | None -> aux t
               | x -> x
               )
   | [] -> None
   in
    aux

let matches_alternatives_with_rmd xs xss =
  let rec aux = function
  | h1 :: t1, h2 :: t2 when List.mem h1 h2 -> aux (t1, t2)
  | [], rmd         -> Some rmd
  | _, []           -> None
  | _ :: _, _ :: _  -> None
  in
    aux (xs, xss)

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
  type letter_type = Vowel | Consonant | AnyType
  type category = letter_type * vowel_depth * vowel_roundness * vowel_openness
  val categorize : t -> category
  val of_chars : char list -> t
  val of_string : string -> t
  val to_string : t -> string
  val list_to_string : t list -> string
  val tr_of_en : t -> t
  val en_of_tr : t -> t
  val print : Format.formatter -> t -> unit
end = struct
  type t = string

  type vowel_depth = Back | Front | AnyDepth
  type vowel_roundness = Rounded | Unrounded | AnyRoundness
  type vowel_openness = Open | Closed | AnyOpening
  type letter_type = Vowel | Consonant | AnyType
  type category = letter_type * vowel_depth * vowel_roundness * vowel_openness

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
  | "s" -> "ş"
  | x -> x
  
  let en_of_tr = function
  | "i" -> "ı"
  | "ö" -> "o"
  | "ü" -> "u"
  | "ç" -> "c"
  | "ğ" -> "g"
  | "ş" -> "s"
  | x -> x
  
  let categorize = function
  | "a" -> (Vowel, Back , Unrounded, Open  )
  | "e" -> (Vowel, Front, Unrounded, Open  )
  | "ı" -> (Vowel, Back , Unrounded, Closed)
  | "i" -> (Vowel, Front, Unrounded, Closed)
  | "o" -> (Vowel, Back , Rounded  , Open  )
  | "ö" -> (Vowel, Front, Rounded  , Open  )
  | "u" -> (Vowel, Back , Rounded  , Closed)
  | "ü" -> (Vowel, Front, Rounded  , Closed)
  | _   -> (Consonant, AnyDepth, AnyRoundness, AnyOpening)
  
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

type letter_def = ULetter.letter_type
                  * ULetter.vowel_depth
                  * ULetter.vowel_roundness
                  * ULetter.t list

let l = ULetter.of_string

let make_letter_variations ls =
  ls 
  |> List.map (fun c ->
                let c' = ULetter.tr_of_en c in
                  if c = c'
                  then [c]
                  else [c; c']
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

let depth_matches a b = (match (a, b) with
                        | ULetter.AnyDepth, _
                        | _, ULetter.AnyDepth -> true
                        | x, y                -> x = y
                        )

let roundness_matches a b = (match (a, b) with
                            | ULetter.AnyRoundness, _
                            | _, ULetter.AnyRoundness -> true
                            | x, y                    -> x = y
                            )
let letter_type_matches a b = (match (a, b) with
                              | ULetter.AnyType, _
                              | _, ULetter.AnyType -> true
                              | x, y               -> x = y
                              )

let attempt
      (word:ULetter.t list)
      (goal:ULetter.t list list)
      (all_defs:letter_def list)
  : ULetter.t list option
  =
  let get_last_type l = List.nth_opt (List.rev l) 0
                        |> Option.map ULetter.categorize
                        |> Option.fold
                             ~some: (function (a,_,_,_) -> a)
                             ~none: ULetter.Consonant in
  let last_type = get_last_type word in
  let (d_last, r_last) = (List.rev word)
                         |> find_opt_map
                              (fun l ->
                                 match ULetter.categorize l with
                                 | (Vowel, d, r, _) -> Some (d, r)
                                 | _ -> None
                              )
                         |> Option.value ~default:(ULetter.AnyDepth,
                                                   ULetter.AnyRoundness)
  in
  let chk_depth = depth_matches d_last in
  let chk_roundness = roundness_matches r_last in
  (** not tail-recursive *)
  let rec aux t_last acc rmd defs =
    (match rmd with
    | Some []      -> Some acc
    | Some rmd_lss -> 
        (match defs with
        | (t, d, r, ls) :: def_tl when letter_type_matches t_last t
                                    && chk_depth d
                                    && chk_roundness r ->
            (match aux
                     (get_last_type ls)
                     (acc @ ls)
                     (matches_alternatives_with_rmd ls rmd_lss)
                     all_defs
            with
            | None -> aux t_last acc rmd def_tl
            | x    -> x
            )
        | _ :: def_tl -> aux t_last acc rmd def_tl
        | []          -> None
        )
    | None -> None
    )
in
    aux last_type [] (Some goal) all_defs

let suggest_words (words:BasicWordDb.t) suffixes (w:string) :string list =
  let letters =  w
                 |> UWord.of_string
                 |> UWord.to_letters in
  let _letter_combos = make_letter_variations letters in
    build_all_combos _letter_combos
    |> List.filter (fun (fst, _) -> BasicWordDb.contains words fst)
    |> List.filter_map (fun (fst, snd) -> 
                  let str = ULetter.list_to_string fst in
                    attempt fst snd suffixes
                    |> Option.map (fun x -> str ^ (ULetter.list_to_string x))
                )
    |> List.sort String.compare

let suggest_words_or_fallback words suffixes w =
  match suggest_words words suffixes w with
  | [] -> [w]
  | l -> l

let words_list = [
  "bal";
  "cin";
  "çin";
  "kedi";
  "öküz";
  "oğul";
  "eşik";
]
let words = BasicWordDb.of_strings words_list
let suffixes =
  let open ULetter in
    [
      AnyType,   Back,      AnyRoundness, "lar";
      AnyType,   Front,     AnyRoundness, "ler";
      Consonant, Back,      AnyRoundness, "a";
      Consonant, Front,     AnyRoundness, "e";
      Vowel,     Back,      AnyRoundness, "ya";
      Vowel,     Front,     AnyRoundness, "ye";
      Consonant, Back,      Unrounded,    "ı";
      Consonant, Front,     Unrounded,    "i";
      Consonant, Back,      Rounded,      "u";
      Consonant, Front,     Rounded,      "ü";
      Vowel,     Back,      Unrounded,    "yı";
      Vowel,     Front,     Unrounded,    "yi";
      Vowel,     Back,      Rounded,      "yu";
      Vowel,     Front,     Rounded,      "yü";
      AnyType,   Back,      AnyRoundness, "da";
      AnyType,   Front,     AnyRoundness, "de";
      AnyType,   Back,      AnyRoundness, "dan";
      AnyType,   Front,     AnyRoundness, "den";
      AnyType,   Back,      AnyRoundness, "ta";
      AnyType,   Front,     AnyRoundness, "te";
      AnyType,   Back,      AnyRoundness, "tan";
      AnyType,   Front,     AnyRoundness, "ten";
      Vowel,     AnyDepth,  AnyRoundness, "m";
      Vowel,     AnyDepth,  AnyRoundness, "n";
      Vowel,     Back,      Unrounded,    "sı";
      Vowel,     Front,     Unrounded,    "si";
      Vowel,     Back,      Rounded,      "su";
      Vowel,     Front,     Rounded,      "sü";
    ]
    |> List.map (fun (a, b, c, str) ->
                  let ls = str
                           |> UWord.of_string
                           |> UWord.to_letters
                  in
                    a, b, c, ls
                )


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
    [
      "cin",        ["cin"; "çin"];
      "ogul",       ["oğul"];
      "ogullar",    ["oğullar"];
      "okuzler",    ["öküzler"];
      "ogula",      ["oğula"];
      "okuze",      ["öküze"];
      "kediye",     ["kediye"];
      "bali",       ["balı"];
      "cini",       ["cini"; "çini"];
      "ogulu",      ["oğulu"];
      "okuzu",      ["öküzü"];
      "balda",      ["balda"];
      "kedide",     ["kedide"];
      "balda",      ["balda"];
      "kedide",     ["kedide"];
      "oguldan",    ["oğuldan"];
      "kediden",    ["kediden"];
      "ogullardan", ["oğullardan"];
      "esikten",    ["eşikten"];
      "balim",      ["balım"];
      "cinim",      ["cinim"; "çinim"];
      "ogulum",     ["oğulum"];
      "okuzum",     ["öküzüm"];
      "balin",      ["balın"];
      "cinin",      ["cinin"; "çinin"];
      "ogulun",     ["oğulun"];
      "okuzun",     ["öküzün"];
      "oguldann",   [];
      "ogldan",     [];
      "fallback",   [];
    ]    
    |> List.iter (fun (a, b) -> test_suggest a b);
