module Characters.Tests

open Expecto

open Shared
open Pinyin
open Characters

let sample = """{"手": {
        "character": "手",
        "definition": "hand",
        "pinyin": ["shǒu"],
        "decomposition": "？",
        "etymology": {
            "type": "pictographic",
            "hint": "A hand with the fingers splayed"
        },
        "radical": "手",
        "matches": [null, null, null, null]
    }}"""

let result = {
    Character = '手'
    Pinyin = Some ({
        Initial = Some ({ Rep = "sh"; IPA = "ʂ";   Approximation = "shirt "         ; Explanation = "Similar to shoe but retroflex, or marsh in American English." })
        Final = { Rep = "ou"; IPA = ""; Approximation = ""; Explanation = "" }
        Tone = { Rep = 3; Description = Dipping}
        FinalTone = "ǒu"
    })
    Definition = ["hand"]
    Decomposition = Some Zero
    Etymology = Some (Pictographic ("A hand with the fingers splayed"))
}

let characters = testList "Characters" [
    testCase "Parse example character" <| fun _ ->
        let result = DecodeCharacterList sample
        let expectedResult = result

        Expect.equal result expectedResult "Result should be ok"
]