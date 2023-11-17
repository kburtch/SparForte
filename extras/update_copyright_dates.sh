ls *.adb | (while read FILE ; do

  sed 's/2001-2022/2001-2023/g' < "$FILE" > t.t
  mv t.t "$FILE"
done)

