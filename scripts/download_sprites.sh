#!/bin/bash
#
# Download Pokemon sprites from PokemonDB.net
#
# Download Pokemon sprites by name from PokemonDB.net into
# local directory.
#

# TODO: This should be removed and a list should be automated
POKEMON_LIST="charmander charmeleon charizard"

BASE_IMAGE_URL='https://img.pokemondb.net/sprites/sword-shield/icon'
SPRITES_DIRECTORY='./assets/sprites'

for pokemon in ${POKEMON_LIST} ; do
    output_file="${SPRITES_DIRECTORY}/${pokemon}.png"
    pokemon_image_url="${BASE_IMAGE_URL}/${pokemon}.png"
    echo "Downloading ${pokemon_image_url} -> ${output_file}"
    curl -s "${pokemon_image_url}" -o "${output_file}"
done

echo "Complete!"
