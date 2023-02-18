Praat --run Scripts/extract_takes.praat ../01_Mono/ ../02_PossibleRecordings/
Praat --run Scripts/gen_manips_pitchtiers.praat ../02_PossibleRecordings/ ../02_PossibleRecordings/PitchTiers
conda activate aligner
Scripts/force_align.sh
quarto.cmd render raw_recordings_eda.qmd
quarto.cmd render raw_2syl_eda.qmd
quarto.cmd render Mermaid.qmd