mkdir -p "$2"
for file in "$1"/*.wav; do
	filename=$(basename "$file" | cut -d "." -f 1)
	Scripts/lame.exe -b 312 -h -V 0 "$file" "$2/$filename.mp3"
done