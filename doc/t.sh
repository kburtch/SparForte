ls *html | (while read FILE ; do
  sed 's/dummy">Packages/packages.html">Packages/g' < "$FILE" > t.t
  mv t.t "$FILE"
done)

