############################
# Intensity Normalization
# Thomas Sostarics 10/10/20
#
# Normalizes all wav files
# in a directory to have
# the given intensity
############################
#
############################

form RMS Normalization
	comment Directory of sound files with textgrids
	text outDir C:\Users\tsost\Box\Research\QP\Recordings\scripttest
	comment Directory for output sound files
	text saveDir C:\Users\tsost\Box\Research\QP\Recordings\scripttest\out
	comment Please enter a dB amplitude to norm to
	natural dBlevel 70
	comment Enter a suffix if you'd like
	text suffix _rms
endform

# Check for final slash
if right$(outDir$, 1) <> "\"
	outDir$ = outDir$ + "\"
endif
if right$(saveDir$, 1) <> "\"
	saveDir$ = saveDir$ + "\"
endif

filenames = Create Strings as file list: "fileList", outDir$ + "*.wav"
numberOfFiles = Get number of strings

for ifile to numberOfFiles
	selectObject: filenames
	filename$ = Get string: ifile
	Read from file: outDir$ + filename$
	filename$ = left$(filename$, length(filename$)-4)
	objname$ = replace$(filename$, " ", "_", 0)
	Scale intensity: 70
	Save as WAV file: saveDir$ + filename$ + suffix$ + ".wav"
endfor

select all
Remove