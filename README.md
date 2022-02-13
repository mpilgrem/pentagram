# pentagram

Inspired by [Wordle](https://www.nytimes.com/games/wordle/index.html),
`pentagram` provides an app to help with (that is, cheat at) Wordle.

It makes use of a curated subset 'B' of the set of English five letter words
(`setb.wordlist`) and the set of all Wordle solutions to date
(`Wordle.wordlist`, which may be not up to date). There is no guarantee that all
possible Wordle solutions (which are themselves curated) are a member of 'B' and
'B' may contain words that are not possible Wordle solutions.

None of the Wordle solutions to date that have ended with 'S' have been plurals (the solutions being FLOSS, CRASS, ABYSS, REBUS and TRUSS). 'B' excludes plurals ending with 'S'.

Example of use
--------------

A guess RAISE might result in a Wordle state of 'green grey yellow grey grey', indicating an 'R' in the correct place, an 'I' in the wrong place and no 'A', 'S' or 'E'. That can be analysed with:

~~~
> pentagram R [] [I] [] [] ASE
~~~

yielding output:

~~~
[("RIGHT",7),("RIGID",7),("ROBIN",7),("RIGOR",9),("RICIN",11),("RITZY",11)]
~~~

This indicates that if one of RIGHT, RIGID and ROBIN is not the solution, it will yield the most information about other possible solutions.
