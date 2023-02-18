for file in "$1"/*.wav; do
	filename=$(echo $file | rev | cut -d'/' -f 1 | rev)
	Scripts/lame.exe -b 312 -h -V 0 "$file" "$2/$filename"
done