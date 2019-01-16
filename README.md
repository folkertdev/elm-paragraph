# elm-paragraph

Formats paragraphs by inserting (nearly) optimal linebreaks.

```elm
module Example exposing (result)

import Paragraph

formatted =
    Paragraph.lines
        { maximumWidth = 50
        , optimalWidth = 45
        , stringWidth = String.length
        }
        warAndPeace

result =
    [ "“Well, Prince, so Genoa and Lucca are now just"
    , "family estates of the Buonapartes. But I warn"
    , "you, if you don’t tell me that this means war,"
    , "if you still try to defend the infamies and"
    , "horrors perpetrated by that Antichrist—I really"
    , "believe he is Antichrist—I will have nothing"
    , "more to do with you and you are no longer my"
    , "friend, no longer my ‘faithful slave,’ as you"
    , "call yourself! But how do you do? I see I have"
    , "frightened you—sit down and tell me all the news.”"
    ]

{-| The first paragraph of "War and Peace"
-}
warAndPeace =
    """“Well, Prince, so Genoa and Lucca are now just family
    estates of the Buonapartes. But I warn you, if you don’t
    tell me that this means war, if you still try to defend
    the infamies and horrors perpetrated by that Antichrist—
    I really believe he is Antichrist—I will have nothing
    more to do with you and you are no longer my friend, 
    no longer my ‘faithful slave,’ as you call yourself! 
    But how do you do? I see I have frightened you—
    sit down and tell me all the news.”"""
```

Based on the paper [Bridging the Algorithm Gap: A Linear-time Functional Program for Paragraph Formatting][paper]

[paper]: http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.33.7923&rep=rep1&type=pdf
