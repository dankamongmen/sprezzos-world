#!/usr/bin/env bash
# This script creates the API documentation files.

echo "Creating API documentation"

if [ -d docs ]; then
    cd docs; doxygen ./configs/doxygen_html_config.txt
else
    exit 0
fi
