# library(dplyr) # Data wrangling
# library(purrr) # Iteration
# library(rlang) # Hash function

# Extract the MFA phone labels for each nuclear word (ie pronunciations)

get_file_phones <- function(textgrid_df) {
  textgrid_df |> 
  dplyr::filter(is_nuclear) |> 
  group_by(file, utterance) |> 
  summarize(phones = phone_label)
}

# Generate unique values for each pronunciation. Two words with the same
# string of phones will yield the same hash value.
hash_phones <- function(file_phones){
  file_phones |> 
  split(~file) |> 
  map_dfr(\(file_df) {
    data.frame(file = file_df$file[[1]],
               utterance = file_df$utterance[[1]],
               hash = rlang::hash(file_df$phones))
  })
}

# All words should have only one pronunciation. If there are a few that were
# segmented differently, they'll have different hashes. We'll keep the most
# dominant pronunciation.
find_abnormal_hash_counts <- function(hashed_phones) {
  hashed_phones |> 
  group_by(hash,utterance) |> 
  summarize(n = n()) |> 
  group_by(utterance) |> 
  mutate(n_hashes = n()) |> 
  dplyr::filter(n_hashes != 1) |> 
  ungroup()
}

# Establish which hashes are the dominant ones for each utterance, then
# return the files with non-dominant pronunciations. These should be removed
# from the textgrids in 01_Mono/. Once you've removed things from the textgrids,
# the take numbers WILL change
find_files_to_omit <- function(hashed_phones, abnormal_hash_counts) {
  inner_join(hashed_phones, 
             dplyr::select(abnormal_hash_counts, -utterance), 
             by = "hash") |> 
  group_by(utterance) |> 
  mutate(is_dominant = n == max(n)) |> 
  dplyr::filter(!is_dominant)
}

check_pronunciations <- function(textgrid_df) {
  hashed_phones <- 
    textgrid_df |> 
    get_file_phones() |> 
    hash_phones()
  
  abnormal_hash_counts <- find_abnormal_hash_counts(hashed_phones)
  
  find_files_to_omit(hashed_phones, abnormal_hash_counts)
}