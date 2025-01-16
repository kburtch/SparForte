ls *.adb | (while read FILE ; do

  sed 's/2001-2024/2001-2025/g' < "$FILE" > t.t
  mv t.t "$FILE"
done)

