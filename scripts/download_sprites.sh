#!/bin/bash
#
# Download Pokemon sprites from PokemonDB.net
#
# Download Pokemon sprites by name from PokemonDB.net into
# local directory.
#

BASE_IMAGE_URL='https://img.pokemondb.net/sprites/sword-shield/icon'
SPRITES_DIRECTORY='./assets/sprites'

for ((pokemon_id=1; pokemon_id<=25; pokemon_id++)) ; do
    api_response=$(curl -s "https://pokeapi.co/api/v2/pokemon/${pokemon_id}")
    pokemon_name=$(jq -r '.name' <<< ${api_response})
    pokemon_type=$(jq -r '.types[] | select (.slot==1) | .type.name' <<< ${api_response})

    output_directory="${SPRITES_DIRECTORY}/${pokemon_type}"
    [[ ! -d "${output_directory}" ]] && mkdir "${output_directory}"
    output_file="${SPRITES_DIRECTORY}/${pokemon_type}/${pokemon_name}.png"

    pokemon_image_url="${BASE_IMAGE_URL}/${pokemon_name}.png"
    echo "Downloading ${pokemon_image_url} -> ${output_file}"
    curl -s "${pokemon_image_url}" -o "${output_file}"
done

echo "Complete!"
