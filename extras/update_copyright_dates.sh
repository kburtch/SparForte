ls *.orig | (while read FILE ; do

  sed 's/2001-2019/2001-2020/g' < "$FILE" > t.t
  mv t.t "$FILE"
done)

