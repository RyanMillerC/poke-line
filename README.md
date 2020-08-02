# poke-mode

> Minor Emacs mode to show position in a buffer using a Pokemon.

![Demo GIF](/docs/demo.gif)

Select from all 890 different Pokemon! Change Pokemon on the fly! Set a
different Pokemon for each mode!

## Installation

I highly recommended installing poke-mode with
[use-package](https://github.com/jwiegley/use-package):

```elisp
(use-package poke-mode
  :ensure t)
```

Alternatively, this package can be installed with package-install
and included in your initial config with:

```elisp
(require 'poke-mode)
```

## Usage

poke-mode can be activated with:

```elisp
(poke-mode 1)
```

The active pokemon can be swapped out with:

```elisp
(set-pokemon "charmander")
```

## Credits

Software was inspired and forked from [nyan-mode](https://github.com/TeMPOraL/nyan-mode).

Pokemon sprites were obtained from [PokemonDB.net](https://img.pokemondb.net/sprites/)
and processed with [this tool](https://github.com/RyanMillerC/poke-position-images).

Pokémon is property of Nintendo/Creatures Inc./GAME FREAK inc. Legal
information on fan projects is available [here](https://www.pokemon.com/us/legal/).
