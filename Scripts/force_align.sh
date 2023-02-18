# source C:/Users/tsost/Anaconda3/etc/profile.d/conda.sh
source C:/ProgramData/Anaconda3/etc/profile.d/conda.sh
conda init bash
conda activate aligner
mfa validate 02_PossibleRecordings Scripts/english_us_mfa.dict Scripts/english_mfa.zip > Scripts/mfa_validation.log
mfa align --clean 02_PossibleRecordings Scripts/english_us_mfa.dict Scripts/english_mfa.zip 02_PossibleRecordings/MFA_textgrids > Scripts/mfa_align.log