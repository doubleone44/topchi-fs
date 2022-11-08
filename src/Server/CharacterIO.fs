namespace Server

open Shared
open Helper
open Pinyin
open Characters

module CharacterIO = 

    let characterJSON = System.IO.File.ReadAllText "/Data/characters.json"
    let characters = DecodeCharacterList characterJSON

    let TryCharFromPinyin (pinyin: Pinyin) = 
        TryCharFromPinyinGeneric characters pinyin

