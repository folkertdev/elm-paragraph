# elm-paragraph

Split paragraphs into lines at optimal places 

```elm
import Paragraph 

options : Paragraph.Options
options =
    { maximumWidth = 15
    , optimalWidth = 12
    , stringWidth = String.length
    }

-- insert linebreaks
Paragraph.format options
    "A delightful language for reliable webapps"
    --> "A delightful\nlanguage\nfor reliable\nwebapps"

-- split a paragraph into lines at optimal places
Paragraph.lines options
    "A delightful language for reliable webapps"
    --> ["A delightful"
    --> ,"language"
    --> ,"for reliable"
    --> ,"webapps"
    --> ]
```

## What is good input

This package works best for the text of a single paragraph. It doesn't deal so well with symbols like `\n` or `\t`. 
They can be removed with `String.replace "\n" ""`.
Leading and trailing spaces count toward the length of the string. 
Use `String.trim` to trim leading and trailing spaces. 

## Picking a good optimal width

Commonly the optimal width should be slightly smaller than the maximum width. 
The algorithm will have a bit of wiggle room to break earlier than strictly needed, which 
leads to more uniform line length. 

## Picking a string width function

In many cases using `String.length` as the width function is fine. 
Keep in mind that this can give some weird results for non-latin symbols:

```elm
-- string length of the party popper emoji
String.length "🎉"
    --> 2
```

One of my future plans is to use information extracted from font files to be able to limit lines to a given number of pixels. 
I could also imagine there are different creative width functions you could use.

## Notes

The implementation is based on the paper [Bridging the Algorithm Gap: A Linear-time Functional Program for Paragraph Formatting][paper]


[paper]: http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.33.7923&rep=rep1&type=pdf
