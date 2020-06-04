#!/bin/bash
#
# Create Elisp file with Pokemon type lookups
#

BASE_API_URL='https://pokeapi.co/api/v2/pokemon'

get_alist_entry() {
    pokemon_id="${1}"
    api_response=$(curl -s "${BASE_API_URL}/${pokemon_id}")
    pokemon_name=$(jq -r '.name' <<< ${api_response})
    pokemon_type=$(jq -r '.types[] | select (.slot==1) | .type.name' <<< ${api_response})
    printf "(\"${pokemon_name}\" . \"${pokemon_type}\")"
}

printf "(setq poke-pokemon-types '("
get_alist_entry 1

for ((pokemon_id=2; pokemon_id<24; pokemon_id++)) ; do
   printf "\n                           "
   get_alist_entry "${pokemon_id}"
done

printf "\n                           "
get_alist_entry 25
printf ')\n'
