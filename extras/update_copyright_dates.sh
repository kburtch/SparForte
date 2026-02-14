ls *.ads | (while read FILE ; do

  sed 's/2001-2025/2001-2026/g' < "$FILE" > t.t
  mv t.t "$FILE"
done)

