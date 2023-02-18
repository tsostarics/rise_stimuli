This directory contains files that are calculated as part of 
`raw_recordings_eda.qmd` but are used in other scripts as well.

 - `avg_phone_duration_by_utt`: Average duration of phones within each utterance,
  used when calculating the average syllable durations
  - `chosen_recordings.txt`: The files selected to be used for the scaling
  manipulation based on the nuclear word duration being closest to the average.
  Importantly, one file from each tune is included, so up to 5 recordings of
  each utterance are returned.
  Note that these files are selected by the *word* duration but the duration
  tier works at the level of the *syllable*. The selection process could be
  changed to minimize, within each utterance and tune, the sum of the squared
  syllable deviations.