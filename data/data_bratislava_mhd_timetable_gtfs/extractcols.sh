#!/bin/bash

for file in *.txt; do
    [ -e "$file" ] || continue

    # First row (column names)
    header=$(head -n 1 "$file")

    # Total number of rows (including header)
    total_rows=$(wc -l < "$file")

    # Data rows (excluding header)
    data_rows=$((total_rows - 1))

    echo "$file:"
    echo "  Columns: $header"
    echo "  Total rows: $total_rows"
    echo "  Data rows: $data_rows"
    echo ""
done