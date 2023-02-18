get_syllable_durations <- function(avg_phone_durations_path,
                                   syllable_specification) {
  readRDS(avg_phone_durations_path) |>
    left_join(syllable_specification,
              by = c('wordset','utterance','phone_i','phone_label')) |> 
    # Calculate the syllable durations for each utterance 
    # from the averaged phone durations in each session
    group_by(session, wordset, utterance, syllable) |> 
    summarize(syl_duration = sum(avg_duration)) |> 
    # Some words (eg northerner) appeared in different sentences across
    # the sessions. Average the durations of the syllables across sessions.
    # If a word only appears in one session, it will remain the same.
    group_by(wordset, utterance, syllable) |> 
    summarize(syl_duration = mean(syl_duration)) |> 
    # Given the syllable durations for each word, average across the words
    # but within each type of word (2syls, 3syls)
    group_by(wordset, syllable) |> 
    summarize(syl_duration = mean(syl_duration)) |> 
    ungroup() |> 
    left_join(
      group_by(syllable_specification, wordset) |> 
        summarize(utterance = unique(utterance))
    )
}

process_syllable_tiers <- function(chosen_files,
                                   syllable_specification,
                                   syllable_durations){
  # file.remove(list.files("03_ChosenRecordings/FixedTextGrids", full.names = TRUE))
  file.remove(list.files("04_ScaledRecordings/TextGrids", full.names = TRUE))
  file.remove(list.files("03_ChosenRecordings/DurationTiers", full.names = TRUE))
  result <- write_scaling_files(chosen_files, 
                                syllable_specification,
                                syllable_durations)
  if (result != 0)
    stop("Error with write scaling files")
  return("04_ScaledRecordings/TextGrids/")
}

