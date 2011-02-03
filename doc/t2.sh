echo "$1" | (while read FILE ; do
  sed 's/Business\ Shell/SparForte/g;s/BUSH/SparForte/g' < "$FILE" > t.t
  mv t.t "$FILE"
done)

