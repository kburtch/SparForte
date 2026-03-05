# Do .ads then .adb
# Do not forget to update the man page
ls *.ads | (while read FILE ; do

  sed 's/2001-2025/2001-2026/g' < "$FILE" > t.t
  mv t.t "$FILE"
done)

