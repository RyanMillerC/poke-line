#!/bin/bash
#
# Create Elisp file with Pokemon type lookups
#

POKEMON_DATA_URL='https://raw.githubusercontent.com/RyanMillerC/poke-position-images/master/pokemon-data.json'

echo 'Downloading pokemon-data.json...'
curl -s "${POKEMON_DATA_URL}" -o pokemon-data.json

get_alist_entry() {
    index="${1}"
    pokemon_name=$(jq -r ".[${index}].name" pokemon-data.json)
    pokemon_type=$(jq -r ".[${index}].type" pokemon-data.json)
    printf "(\"${pokemon_name}\" . \"${pokemon_type}\")"
}

create_file() {
  number_of_pokemon=$(jq '. | length' pokemon-data.json)
  printf "(setq poke-pokemon-types '("
  get_alist_entry 0

  for ((iterator=1; iterator<number_of_pokemon; iterator++)) ; do
    printf '\n                           '
    get_alist_entry "${iterator}"
  done

  printf ')\n'
}

echo 'Creating pokemon-types.el...'
create_file > pokemon-types.el
echo 'Complete!'
