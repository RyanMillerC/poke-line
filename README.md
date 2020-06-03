# Poke Mode

> A Pokemon shows position in current buffer in mode-line.

TODO: Add image here

## Pokemon Types to Add

* [] Normal
* [x] Fire
* [x] Water
* [x] Grass
* [] Electric
* [] Ice
* [] Fighting
* [] Poison
* [] Ground
* [] Flying
* [] Psychic
* [x] Bug
* [] Rock
* [] Ghost
* [] Dark
* [] Dragon
* [] Steel
* [] Fairy

## Adding New Pokemon

Images are stored as XPM. These can be easily created from PNG files using
[ImageMagick]().

Pokemon sprites can be downloaded from the PokemonDB
[here](https://img.pokemondb.net/sprites/).

Pokemon sprite images face left, so the image will need to be horizontally
mirrored.

Most sprites have whitespace around them than can be trimmed.

Poke mode images should be 20x30 pixels.

To create an XPM image file that is 20x30 with whitespace trimmed:
`convert pokemon.png -trim -flop -thumbnail 20x30 pokemon.xpm`
