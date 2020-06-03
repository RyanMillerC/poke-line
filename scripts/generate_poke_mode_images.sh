#!/bin/bash
#
# Generate XPM files used by poke-mode.
#
# Generates XPM image files by layering cropped Pokemon sprites
# over elemental images to give the effect of them using an
# elemental attack.
#

BASE_DIRECTORY='./assets/base'
SPRITES_DIRECTORY='./assets/sprites'
OUTPUT_DIRECTORY='./assets/output'

[[ ! -d "${OUTPUT_DIRECTORY}" ]] && mkdir "${OUTPUT_DIRECTORY}"

for type_directory in $(find "${SPRITES_DIRECTORY}" ! -path "${SPRITES_DIRECTORY}" -type d) ; do
    pokemon_type="${type_directory##*/}"
    for image in $(find "${type_directory}" -name '*.png') ; do
        image_base_name="${image##*/}"
        temp_file="/tmp/${image_base_name}"
        output_file="${OUTPUT_DIRECTORY}/${image_base_name}"
        echo "Processing ${image} -> ${output_file}"
        convert "${image}" -flop \
                           -trim \
                           -resize '20x30>' \
                           -unsharp 0x.5 \
                           "${temp_file}"
        convert "${BASE_DIRECTORY}/${pokemon_type}_base.png" \
                -gravity center \
                "${temp_file}" \
                -composite "${output_file}"
        rm "${temp_file}"
    done
done

echo "Complete!"
