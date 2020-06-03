#!/bin/bash
#
# Generates element image files from base image files.
#
# Generates PNG element image files by taking a cropped section
# of the matching base image.
#

BASE_DIRECTORY='./assets/base'
ELEMENTS_DIRECTORY='./assets/elements'
OUTPUT_DIRECTORY='./assets/output'

for image in $(find "${BASE_DIRECTORY}" -name '*.png') ; do
    image_base_name="${image##*/}"
    element_type="${image_base_name%%_*}"
    output_file="${OUTPUT_DIRECTORY}/${element_type}.png"
    echo "Processing ${element_type} -> ${output_file}"
    convert "${image}" -crop 8x20+22+0 "${output_file}"
done

echo "Complete!"
