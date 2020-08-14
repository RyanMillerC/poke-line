# poke-line

[![MELPA](https://melpa.org/packages/poke-line-badge.svg)](https://melpa.org/#/poke-line)

> Minor Emacs mode to show position in a buffer using a Pokemon.

![Demo GIF](/docs/demo.gif)

Select from all 890 different Pokemon! Change Pokemon on the fly! Set a
different Pokemon for each mode!

## Installation

poke-line is available through MELPA. You will need to
[enable MELPA](https://melpa.org/#/getting-started) in your Emacs
configuration before installing poke-line.

I recommend installing poke-line with
[use-package](https://github.com/jwiegley/use-package) in your Emacs
configuration:

```elisp
(use-package poke-line
  :ensure t)
```

Alternatively, this package can be installed with
package-install:

```elisp
M-x package-install RET poke-line
```

## Usage

poke-line can be activated with:

```elisp
(poke-line-mode 1)
```

The chosen Pokemon can be selected with:

```elisp
(poke-line-set-pokemon "charmander")
```

The command above is also interactive so Pokemon can be swapped
on the fly:

```elisp
M-x poke-line-set-pokemon "charmander"
```

Easily set the default Pokemon in your config with use-package:

```elisp
(use-package poke-line
  :ensure t
  :config
  (poke-line-mode 1)
  (poke-line-set-pokemon "gengar"))
```

See [this page](docs/pokemon.md) for a list of available Pokemon.

## Customization

poke-line allows for two custom variables to be set. If either variable is
updated after starting poke-line, `poke-line-refresh` must be called to
refresh their values.

### poke-line-minimum-window-width

Minimum width of the window, below which poke-line-mode will not be displayed. This
is important because poke-line-mode will push out all information from small
windows.

```elisp
(setq poke-line-minimum-window-width 64)
```

### poke-line-bar-length

Length of Poke element bar in units.  Each unit is equal to an 8px image.
Minimum of 3 units are required for poke-line.

```elisp
(setq poke-line-bar-length 32)
```

## Credits

Software was inspired and forked from [nyan-mode](https://github.com/TeMPOraL/nyan-mode).

Pokemon sprites were obtained from [PokemonDB.net](https://img.pokemondb.net/sprites/)
and processed with [this tool](https://github.com/RyanMillerC/poke-position-images).

Pokémon is property of Nintendo/Creatures Inc./GAME FREAK inc. Legal
information on fan projects is available [here](https://www.pokemon.com/us/legal/).
