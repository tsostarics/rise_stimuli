form nuclear regions
	comment Directory containing TextGrids (and will hold output)
	text tgDir ..\06_ResynthesizedRecordings\TextGrids
	text outputFile nuclear_regions.csv
endform

if right$(tgDir$, 1) <> "\"
	tgDir$ = tgDir$ + "\"
endif

Create Strings as file list: "list", tgDir$ + "*.TextGrid"
numberOfFiles = Get number of strings

# Append directory to output file
outputFile$ = tgDir$ + outputFile$
writeFileLine: outputFile$, "file, tmin, tmax"

for ifile to numberOfFiles
	select Strings list
	filename$ = Get string: ifile
	
	Read from file: tgDir$ + filename$
	filename$ = left$(filename$, length(filename$)-9)
	objname$ = replace$(filename$, " ", "_", 0)

	selectObject: "TextGrid " + objname$
	nIntervals = Get number of intervals: 3
	nucStart = Get start time of interval: 3, nIntervals - 1
	nucEnd = Get end time of interval: 3, nIntervals - 1
	appendFileLine: outputFile$, filename$, ", ", nucStart, ", ", nucEnd
	selectObject: "TextGrid " + objname$
	Remove
endfor
