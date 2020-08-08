#!/bin/bash
#
# Create Elisp file with Pokemon type lookups
#

POKEMON_DATA_URL='https://raw.githubusercontent.com/RyanMillerC/poke-position-images/master/pokemon-data.json'

echo 'Downloading pokemon-data.json...'
curl -s "${POKEMON_DATA_URL}" -o pokemon-data.json

echo 'Creating poke-mode-types.el...'
cat > poke-mode-types.el <<'EOF'
;;; poke-mode-types.el --- Provides Pokemon types used by `poke-mode' -*- lexical-binding: t; -*-

;; Author: Ryan Miller <ryan@devopsmachine.com>
;; URL: https://github.com/RyanMillerC/poke-mode/
;; Version: 1.0.0
;; Keywords: pokemon, fun, mode-line, mouse

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:

;; This package is required by poke-mode to provide Pokemon types

;;; Code:

EOF

get_alist_entry() {
    index="${1}"
    pokemon_name=$(jq -r ".[${index}].name" pokemon-data.json)
    pokemon_type=$(jq -r ".[${index}].type" pokemon-data.json)
    printf "(\"${pokemon_name}\" . \"${pokemon_type}\")"
}

number_of_pokemon=$(jq '. | length' pokemon-data.json)
echo "Processing Pokemon 1 of ${number_of_pokemon}"
printf "(defvar poke-pokemon-types '(" >> poke-mode-types.el
get_alist_entry 0 >> poke-mode-types.el

for ((iterator=1; iterator<number_of_pokemon; iterator++)) ; do
  echo "Processing Pokemon $((iterator+1)) of $((number_of_pokemon))..."
  printf '\n                             ' >> poke-mode-types.el
  get_alist_entry "${iterator}" >> poke-mode-types.el
done

printf '))\n' >> poke-mode-types.el

cat >> poke-mode-types.el <<'EOF'

(provide 'poke-mode-types)

;;; poke-mode-types.el ends here
EOF

echo 'Complete!'
