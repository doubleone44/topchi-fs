namespace Shared

open Helper
open Pinyin
open Thoth.Json

module Characters = 

    type Hint = string

    type Ideograph =
        | LeftRight
        | AboveBelow
        | LeftMiddleRight
        | AboveMiddleBelow
        | SurroundFull
        | SurroundAbove
        | SurroundBelow
        | SurroundLeft
        | SurroundRight
        | SurroundUpperLeft
        | SurroundUpperRight
        | SurroundLowerLeft
        | Overlaid

    let etymologyGen (etType : string) (hint : string option) (semantic : string option) (phonetic : string option) =
        None

    type Character =
        { Character: char
          Pinyin: Option<Pinyin>
          Definition: List<string>
          Decomposition: Option<Decomposition>
          Etymology: Option<Etymology> }

    and Decomposition =
        | Zero
        | One of Character
        | Two of Option<Ideograph> * Decomposition * Decomposition
        | Three of Option<Ideograph> * Decomposition * Decomposition * Decomposition

    and Etymology =
        | Ideographic of Hint
        | Pictographic of Hint
        | Pictophonetic of Phonetic option * Semantic option * Hint

    and Phonetic = string // to be Character
    and Semantic = string // to be Character

    type Phrase = List<Character>

    type EtyString = 
        { Type: string
          Hint: string
          Semantic: Semantic option     
          Phonetic: Phonetic option }   

    let EtyDecoder : Decoder<EtyString> =
        Decode.object (fun get -> 
        { 
            Type = get.Optional.Field "type" Decode.string
                |> Option.defaultValue ""
            Hint = get.Optional.Field "hint" Decode.string
                |> Option.defaultValue ""
            Semantic = get.Optional.Field "semantic" Decode.string // to be a character match -- for now a string
            Phonetic = get.Optional.Field "phonetic" Decode.string // to be a character match -- for now a string
        }
    )

    let EtyStringToEty etyString =
        match etyString with
        | Some a -> match a.Type with
                    | "ideographic" -> Some(Ideographic a.Hint) 
                    | "pictographic" -> Some(Pictographic a.Hint)
                    | "pictophonetic" -> Some(Pictophonetic (a.Phonetic, a.Semantic, a.Hint) )
                    | _ -> None
        | None -> None

    let BiDirs = ['⿰';'⿱';'⿴';'⿵';'⿶';'⿷';'⿸';'⿹';'⿺';'⿻']
    let TriDirs =  ['⿲';'⿳']

    let matchBidir =
        fun bidir ->
            match bidir with
            | '⿰' -> Some(LeftRight)
            | '⿱' -> Some(AboveBelow)
            | '⿴' -> Some(LeftMiddleRight)
            | '⿵' -> Some(AboveMiddleBelow)
            | '⿶' -> Some(SurroundFull)
            | '⿷' -> Some(SurroundAbove)
            | '⿸' -> Some(SurroundBelow)
            | '⿹' -> Some(SurroundLeft)
            | '⿺' -> Some(SurroundRight)
            | '⿻' -> Some(SurroundUpperLeft)
            | '⿼' -> Some(SurroundUpperRight)
            | '⿽' -> Some(SurroundLowerLeft)
            | '⿾' -> Some(Overlaid)
            | _ -> None 

    let matchTriDir =
        fun triDir ->
            match triDir with
            | '⿲' -> Some(LeftRight)
            | '⿳' -> Some(AboveBelow)
            | _ -> None

    type DecompositionPass = 
        { dec: Decomposition
          chars: List<char> }

    let rec Dec (chars : List<char>): DecompositionPass =
        match chars with
            | [] -> {dec = Zero; chars = []}
            | h::t when BiDirs |> List.contains h -> 
                let ideo = matchBidir h
                let firstDec = Dec (t)
                let restDec = Dec (firstDec.chars)
                { dec = Two(ideo, firstDec.dec, restDec.dec); chars = restDec.chars }
            | h::t when TriDirs |> List.contains h ->
                let ideo = matchTriDir h
                let firstDec = Dec (t)
                let secondDec = Dec (firstDec.chars)
                let restDec = Dec (secondDec.chars)
                { dec = Three(ideo, firstDec.dec, secondDec.dec, restDec.dec); chars = restDec.chars }
    // TODO: add search for pinyin and definition here - possibly have the decomposition pass return a list of definitions and pinyin
            | h::t -> {dec = One({
                Character = h
                Pinyin = None
                Definition = []
                Decomposition = None
                Etymology = None
            }); chars = t}

    let Decompose (decString : string option) : Decomposition option =
        match decString with
            | None -> None
            | Some str ->
                let chars = Seq.toList str
                let dec = Dec (chars)
                Some(dec.dec)
                            
    let decodeCharacter : Decoder<Character> =
        Decode.object (fun get ->
            { Character = (get.Required.Field "character" Decode.string).ToCharArray().[0]

              Definition = ((get.Optional.Field "definition" Decode.string
                        |> Option.defaultValue "").Split(','))
                        |> Seq.toList

              Pinyin = get.Required.Field "pinyin" (Decode.list Decode.string)
                    |> first
                    |> Option.defaultValue ""
                    |> StringToPinyin

              Decomposition = get.Optional.Field "decomposition" Decode.string
                            |> Decompose

              Etymology = get.Optional.Field "etymology" EtyDecoder
                        |> EtyStringToEty
            })


    let DecodeCharacterList (characterJSON):  Result<Character list,string> = 
        Decode.Auto.fromString<List<Character>>(characterJSON)

    let TryCharFromPinyinGeneric (characters : Result<Character list,string>) (pinyin : Pinyin) : Result<Character,string> =
        match characters with
        | Ok li ->
            let res = List.filter (fun c -> c.Pinyin = Some(pinyin)) li |> first 
            match res with 
            | None -> Error ("No character found for pinyin " + pinyin.ToString())
            | Some c -> Ok(c) 
        | Error e -> Error e

