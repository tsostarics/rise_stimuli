#######################################
# Resynthesize Alignment Continuum v4
# Thomas Sostarics 08/22/2022
# Last Updated: 08/23/2022
#######################################
# This script is for resynthesizing the
# stimuli in experiment 3, which adds
# a low tonal target & an additional
# alignment manipulation. Each source
# file yields nsteps^2 audio files.
# In this version, the distance between
# the leading low target and the
# high target is held at a constant
# time calculated from the syllable
# start boundary to the earliest
# high target
#######################################

form Resynthesize Continuum
	comment Directories for sound files and TextGrids
	text fromDir ..\altest\input
	text tgFromDir ..\altest\input\TextGrids
	comment Directory to save output sounds, PitchTiers, TextGrids
	text outDir ..\altest\output\
	text ptDir ..\altest\output\PitchTiers
	text tgToDir ..\altest\output\TextGrids
	comment Path to csv file containing bezier curve points
	text bezierFile bezierpoints.csv
	positive nBezierPoints 13
	positive nAlignmentSteps 5
	comment Values from monotonal continuum for calculating boundary tones
	positive origLowHz 70
	positive origHighHz 110
	comment Number of boundary tone steps
	positive nSteps 5
	comment ERB Differentials
	real lowErb -0.25
	real highErb 2
	comment Please enter the pitch range for the manipulation
	natural min 40
	natural max 200
endform

# Double check directory to make sure it ends in a slash
# Note: max and linux users might need to change \ to /
if right$(fromDir$, 1) <> "\"
	fromDir$ = fromDir$ + "\"
endif
if right$(outDir$, 1) <> "\"
	outDir$ = outDir$ + "\"
endif
if right$(tgFromDir$, 1) <> "\"
	tgFromDir$ = tgFromDir$ + "\"
endif
if right$(tgToDir$, 1) <> "\"
	tgToDir$ = tgToDir$ + "\"
endif
if right$(ptDir$, 1) <> "\"
	ptDir$ = ptDir$ + "\"
endif

## Make the continuum values
pavals# = from_to_count# (origLowHz, origHighHz, nSteps)
erbvals# = from_to_count# (lowErb, highErb, nSteps)
btvals# = zero# (nSteps)
alignmentPoints# = to# (nBezierPoints)
alignmentSteps# = to# (nAlignmentSteps) - 1
#writeInfoLine: alignmentSteps#
Read Table from comma-separated file: bezierFile$

xcol$ = Get column label: 2
ycol$ = Get column label: 3

# Calculate boundary tone values in Hz based on ERB differentials
for i from 1 to size (btvals#)
	btvals# [i] = erbToHertz(hertzToErb(origLowHz) + erbvals# [i])
endfor

midPitch = mean (pavals#)

# load files
Create Strings as file list: "list", fromDir$ + "*.wav"
numberOfFiles = Get number of strings

for ifile to numberOfFiles
	select Strings list
	filename$ = Get string: ifile
	
	Read from file: fromDir$ + filename$
	filename$ = left$(filename$, length(filename$)-4)

	Read from file: tgFromDir$ + filename$ + ".TextGrid"
	# Any spaces in the file name needs to be replaced
	# as underscores so praat can reference them in
	# the objects pane
	objname$ = replace$(filename$, " ", "_", 0)

	selectObject: "Sound " + objname$
	To Manipulation: 0.01, min, max

	selectObject: "TextGrid " + objname$
	stressedSylStart = Get start time of interval: 4, 2
	stressedSylEnd = Get end time of interval: 4, 2
	stressedSylDur = stressedSylEnd - stressedSylStart
	nIntervals = Get number of intervals: 4
	btTime = Get start time of interval: 4, nIntervals
 	firstWordStart = Get start time of interval: 1, 2
	firstWordEnd = Get end time of interval: 1, 2
	prenuclearPointTime = (firstWordStart + firstWordEnd) / 2

for stepi from 1 to size (alignmentSteps#)
	# Create the vectors to hold the timepoints and frequency values from the csv
	alignmentVals# = zero# (nBezierPoints)
	freqVals# = zero# (nBezierPoints)
	pointi = 1
	# 1 + 9*0 = 1;  9*1 = 9
	# 1 + 9*1 = 10; 9*2 = 18
	# 1 + 9*2 = 19; 9*3 = 27
	# 1 + 9*3 = 28; 9*3 = 36
	# 1 + 9*4 = 37; 9*3 = 45
	for row from (1+nBezierPoints*alignmentSteps#[stepi]) to (nBezierPoints*alignmentPoints#[stepi])
		selectObject: "Table bezierpoints"
		
		alignmentVals#[pointi] = Get value: row, xcol$
		freqVals#[pointi] = Get value: row, ycol$
		pointi = pointi + 1
	endfor
	#appendInfoLine: alignmentVals#

	for bti from 1 to size (btvals#)
		selectObject: "Sound " + objname$
  		tmax = Get total duration
  		To Manipulation: 0.01, min, max
  		selectObject: "Sound " + objname$
  		Create PitchTier: objname$, 0.0, tmax

  		# Add the new pitch values in this order:
  		#                *
  		#              paval
  		#  mid           O
  		#  O--O         / \
  		#       \___   /   \
  		#            \O     O
  		#        origLowHz  btval
  		#             *
		#             **
  		# *=alignment changes by alignStepPct each iteration, but Hz value does not change
		# **=always trails paval by constant lowAlignmentPct
 		Add point: 0.0, midPitch
		Add point: prenuclearPointTime, midPitch
		for paPoint from 1 to nBezierPoints
			Add point: (alignmentVals#[paPoint]*stressedSylDur +stressedSylStart), freqVals#[paPoint]
		endfor
 		Add point: btTime, btvals#[bti]

		# Perform the resynthesis and save new file
		selectObject: "Manipulation " + objname$
		plusObject: "PitchTier " + objname$
		Replace pitch tier
		selectObject: "Manipulation " + objname$
		Get resynthesis (PSOLA)
		selectObject: "Sound " + objname$
		Save as WAV file: outDir$ + objname$ + "_" + string$(stepi) + "_" + string$(bti) + ".wav"
		
		# Remove the resynthesis manipulation and pitchtier we made
		selectObject: "Manipulation " + objname$
		plusObject: "PitchTier " + objname$
  		Remove
  
		# Create a new manipulation from the resynthesized file
		# so we can extract the new signal's pitch tier
		selectObject: "Sound " + objname$
		To Manipulation: 0.01, min, max
		Extract pitch tier
		selectObject: "PitchTier " + objname$
		Save as text file: ptDir$ + objname$ + "_" + string$(stepi) + "_" + string$(bti) + ".PitchTier"
		
		# Remove the new Sound, PitchTier, and Manipulation we just made
		# Important: if you don't remove the sound object here the later resyntheses
		#            will apply on top of the resynthesized file! like jpeg compression artifacts
		selectObject: "PitchTier " + objname$
		plusObject: "Manipulation " + objname$
		plusObject: "Sound " + objname$
		Remove

		# Copy the TextGrid into the new directory. This will yield many copies
		# of the same TextGrid but honestly it's easier this way, space is cheap
		# and most analyses/wrangling assumes a 1:1 correspondence original
		# wav files and textgrid files
		selectObject: "TextGrid " + objname$
		Save as text file: tgToDir$ + objname$ + "_" + string$(stepi) + "_" + string$(bti) + ".TextGrid"

	endfor
endfor
selectObject: "Sound " + objname$
plusObject: "TextGrid " + objname$
plusObject: "Manipulation " + objname$
Remove
endfor

#writeFileLine: "resynthesis_parameters_exp3.csv", "index, pa_val, bt_val"
#for i from 1 to size (pavals#)
#	appendFileLine: "resynthesis_parameters_exp3.csv", i, ", ", pavals# [i], ", ", btvals# [i]
#endfor

select all
Remove