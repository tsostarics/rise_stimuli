Prerequisites:
 - Must install montreal forced aligner in a conda environment
 - Should download the US MFA dictionary and acoustic files and add to Scripts 
   directory

## Organization
File format as follows:

 - 01: exp1_raw_3syl_01_HLL = [experiment#]_[rawrecording]_[wordlist/set]_[version/session#]_[tune]
 - 02: 3syl_evansville_01_HLL_001 = [wordlist]_[utteranceID]_[version/session#]_[tune]_[take]
 - 03: same as 02, minus the wordlist
 - 04: same as 03
 - 05: ConstantScaling_XX, XX indicates the percentage of the final syllable's
 duration (100 = no duration manipulation, 80 = 80% original duration). Files 
 are otherwise the same as 03
 - 06: branning_01_HHH_010_1_1 = [utteranceID]_[version/session#]_[originaltune]_[take]_[PAval]_[BTval]
 - 06b: branning_01_HHH_010_1_1 = [utteranceID]_[version/session#]_[originaltune]_[take]_[PAval]_[BTval]
 - 06c: branning_01_HHH_010_1_1 = [utteranceID]_[version/session#]_[originaltune]_[take]_[Alignmentval]_[BTval] 
 (-CURVED directroy uses curved onglides, the other uses straight lines)
 

Takes are given WITHIN THE SESSION. Take counting restarts with each session.

## Forced Aligner
Usage Notes:
 - Must run `conda init bash` and then `conda activate aligner` before running
   Scripts/force_align.sh
 - Scripts/force_align.sh should be run from the Recordings directory

The following words were out of vocabulary for the english_us_mfa dictionary.
Provided below are the entries with their phonetic transcriptions.
These lines should be copy-pasted into the english_us_mfa.dict file

weatherman	1.0	0.99	0.0	0.0	w ɜ ð ɚ m æ n

northerner	1.0	0.99	0.0	0.0	n ɒ ɹ ð ɚ n ɚ

middleman	1.0	0.99	0.0	0.0	m ɪ ɾ ɫ̩ m æ n

branning	1.0	0.99	0.0	0.0	b ɹ æ n ɪ ŋ

bronville	1.0	0.99	0.0	0.0	b ɹ ɑ n v ɪ ɫ

greenview	1.0	0.99	0.0	0.0	ɟ ɹ i n vʲ ʉː

 - Note that the 2nd float for these words is .99 because in our corpus it is
   always the last word in an utterance, hence the probability of silence
   following the word will be 1.0, but if you make it exactly 1 python will
   throw an error when trying to execute math.log(1-silence_after_probability). 
   
## Top-level files:

***Do not move or edit any of the files located here***. 
These consist mainly of manually-created files  and shared resources 
that are used with different functions throughout the project.

 - README.md: This readme
 - Praat.exe: Praat executable for the project, v6.2.14 (May 24, 2022)
 - syllabification.csv: Manually specified syllabification for all nuclear
 words
 - _targets.R: Targets make file
 - app.R: Shiny app to play continua more easily
 - run.R: Used with targets
 - Recordings.Rproj: R project file
 - make.sh: Used with targets
 - run.sh: Used with targets
 - .gitignore: git ignore file, mostly for ignoring non-stimulus files. *Do not*
 remove any of the lines here.
 - filelist_for_spectral_measures.txt: List of recordings to use for an older
 version of the spectral measures calculation script
 - wordlist.txt: Word list for the first recording session (3 syllables)
 - wordlist2.txt: Wordlist for the second recording session (3 syllables)
 - wordlist3.txt: Wordlist for the third recording session (2 syllables)
 - praat.xml: Syntax highlighting file for praatscripts
 - _quarto.yml: Quarto config file for the project, forces quarto docs to render
 in the project directory
 
## Directories

Stimulus-related directories:

 - 00_Raw: Raw recordings
 - 01_Mono: Mono recordings with textgrids
 - 02_PossibleRecordings: Selected recordings that are okay enough for further
 analysis. Contains wav files, montreal forced aligned textgrids, pitchtiers, and 
 spectral measures.
 - 03_ChosenRecordings: Recordings selected on the basis of closest duration to
 the average for 2 and 3 syllable words. Contains wav files, fixed MFA textgrids,
 original MFA textgrids, and DurationTiers needed to standardize syllable durations
 - 04_ScaledRecordings: The recordings from the previous directory, but syllable
 durations are scaled to be the same duration. The textgrids are also modified
 to place the boundaries in the correct locations.
 - 05_NudgedRecordings: Contains subdirectories with all the recordings from
 the previous directory, but the final syllable's duration is manipulated to be
 a given percentage of its original duration. E.g., 100 = 100% = no change, 
 80 = 80% of original duration. Note that this is performed after standardizing
 the syllable lengths, so all of the "nudged" syllables also have the same
 duration. These subdirectories carry over to every resynthesis.
 - 06_ResynthesizedRecordings: Resynthesized recordings for Experiment 1:
 Crossed PA and BT continua for monotonal pitch accents
 - 06b: Resynthesized recordings for Experiment 2: Crossed PA and BT continua
 for L+H*, with curved onglides.
 - 06c: Resynthesized Recordings for Experiment 3: Crossed alignment and BT
 continua for L\*+H\* to L\*+H. This one uses straight line interpolations
 for the accent onglides.
 - 06cCURVED: Same as 06c, but the onglide is curved.
 
Non-stimulus related directories:

 - CalculatedFiles: Intermediate files generated from different scripts and
 analyses, which are then used as inputs to other scripts.
 - Figures: Various figures generated by analyses
 - FullSpectralMeasures: Not used
 - Helpers: R files with helper functions sourced by analyses and targets pipelines
 - MidphonFigs: Figures specifically used for MidPhon 2022 submission
 - presentation_stims: Manually created stimulus chains to play in presentations
 - Scripts: Praat scripts and related files
 - SpectralMeasureFiles: Notused
 - test: Testing directory
 - altest: Used to test resynthesis scripts on 2 files
 - Writeups: .qmd files for analyses and figure generation
 - www: Resource directory for app.R
 
 
## Praat scripts

 - praatsauce: Directory containing praatsauce files for spectral measurement
 extraction
 - apply_dt_manipulation.praat: Applies duration tier manipulations
 - draw.praat: Draws multiple sound files and a textgrid to the praat window.
 **Manual execution only**
 - extract_takes.praat: Extracts labeled intervals into their own wav files,
 used to create the files in 02_PossibleRecordings
 - gen_manips_pitchtiers.praat: Creates pitchtiers and/or manipulations for
 all files in a directory, for when I need the pitchtiers saved to disk.
 - INSTALL.praat: Used for installing vocal toolkit only
 - intensity_norm.praat: Norms all files in a directory to the given intensity
 - resynthesize_continua.praat: Creates the continua for Exp 1 (06)
 - resynthesize_alignment_continua3.praat: Creates the continua for Exp 3 with
 straight lines (06c)
 - resynthesize_alignment_continua4.praat: Creates the continua for Exp3 with
 curved lines (06cCURVED)
 - resynthesize_alignment_continua_exp2: Creates the continua for Exp 2 (06b)
 - save_nuclear_regions.praat: Saves timestamps for the nuclear regions given
 a textgrid
 - tg2sepfiles.praat: Extracts textgrid to separate files, not used though. Was
 used as a reference for extract_takes.praat
 - trigger2tg.praat: Northwestern's script for extracting files from presentation
 software

### Other scripts and files

The .zip, .dict, .log files in the Scripts directory are for the montreal
forced aligner; force_align.sh runs the forced aligner.
bezierpoints.csv is used for the curved onglide resynthesis scripts; this is
automatically generated as part of the targets pipeline.
The csv files have information about the resynthesis parameters but aren't
really used.

## Writeups

 - flowchart: Flowchart and details for stimulus creation process
 - midphon_figures: Generates figures for MidPhon 2022 submission
 - raw_recordings_eda: Exploratory data analysis for the raw recordings. This
 also does some calculations for the aggregate durations.
 - resynthesis_analysis: Generates figures for spot-checking the resynthesized
 files. Parameterized for each experiment.
 - spectral_measures_analysis: Exploratory data analysis for spectral measures.
 - retainers: Deprecated chunks that I haven't fully deleted yet