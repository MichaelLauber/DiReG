#!/bin/bash
# Usage: ./create_mapping.sh input.jsonl

if [ "$#" -ne 1 ]; then
  echo "Usage: $0 <jsonl_file>"
  exit 1
fi

input_file="$1"
output_file="motif_tf_mapping.txt"

python <<EOF
import json
with open("$input_file", "r") as infile, open("$output_file", "w") as outfile:
    for line in infile:
        line = line.strip()
        if line:
            try:
                data = json.loads(line)
                motif_name = data.get("name", "")
                tf_name = data.get("original_motif", {}).get("tf", "")
                outfile.write("{}\t{}\n".format(motif_name, tf_name))
            except Exception as e:
                print("Error processing line: {}".format(e))
EOF

echo "Mapping file created: $output_file"
