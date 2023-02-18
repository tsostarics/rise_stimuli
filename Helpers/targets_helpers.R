#' Calculate MD5 Checksum on files in directory
#' 
#' Returns the MD5 checksum for every file within a directory.
#' Intended to be used to check of the return value of a target changes but then
#' that didn't work for some reason.
#'
#' @param directory Directory containing files
#' @param .recursive Whether to check subdirectory files
#'
#' @return named vector of md5 checksums
md5sum_dir <- function(directory, .recursive = FALSE) {
  tools::md5sum(list.files(directory, recursive=.recursive, full.names=TRUE))
}


#' Get reverse dependencies of a node
#' 
#' Given the name of a target and the edges dataframe from `targets::tar_network()`
#' return the downstream nodes that depend on the given target. Will recurse
#' through the entire DAG.
#' 
#' @param from Node to search from
#' @param edges Edges dataframe from tar_network
#'
#' @return Character vector of target names
get_reverse_dependencies <- function(from = "write_constant_scale_files", edges) {
  # Base case
  if (!from %in% edges$from)
    return(from)
  
  new_nodes <- edges[edges$from == from,][['to']]
  
  nodes <- lapply(new_nodes, \(x) c(from, get_reverse_dependencies(x, edges)))
  
  unique(c(nodes, recursive=TRUE))
  
}

#' Delete and invalidate reverse dependencies
#' 
#' Targets doesn't play perfectly with praat scripts since the usual workflow
#' is to create large numbers of files. When these files are recreated, targets
#' doesn't flag them as being out of date since the files themselves aren't
#' tracked (too many to do so and also the repository option wouldn't work for 
#' me). To circumvent this, this function will invalidate and delete all of the
#' downstream target information from a given node, which will force them
#' to be rerun.
#' 
#' tar_network() can take a while to run, so it's recommended to save this
#' to a variable if you think you might need to rerun this a few times as
#' you try new things in praat.
#'
#' @param from Node to delete from
#' @param network Output of targets::tar_network()
#'
#' @return
flush_targets <- function(from, network = targets::tar_network()) {
  nodes_to_flush <- get_reverse_dependencies(from, network$edges)
  
  targets::tar_invalidate(all_of(nodes_to_flush))
  targets::tar_delete(all_of(nodes_to_flush))
  
  message(glue::glue("Invalidated and deleted {length(nodes_to_flush)} nodes from {from}"))
  
  nodes_to_flush
}


#' Check manually fixed textgrids
#' 
#' Not used anywhere, intended to check whether there were any manual errors
#' in fixing the textgrid.
#'
#' @param tgdir 
#' @param wavdir 
check_manually_fixed_textgrids <- function(tgdir = "03_ChosenRecordings/FixedTextGrids",
                                           wavdir = "03_ChosenRecordings") {
  tg_files <- gsub(".TextGrid", "", list.files(tgdir), perl = TRUE)
  wav_files <- gsub(".wav", "", list.files(wavdir,pattern='.wav'), perl = TRUE)
  
  if (!all(tg_files %in% wav_files))
    stop("Not all textgrids have associated wav files")
  
  if (length(tg_files) != length(wav_files))
    stop("Number of textgrids does not equal number of wav files")
  
  # Read in all textgrids
  textgrids <- 
    data.frame(file = tg_files,
               path = list.files(tgdir, full.names = TRUE)) |> 
    tidyr::separate(file,
                    into = c('utterance','session','tune','take'),
                    sep = "_",
                    remove = FALSE) |> 
    group_by(file) |> 
    mutate(textgrid = list(tg.read(path, encoding = 'auto')))
  
  # Ensure all textgrids for the same utterance have the same phones
  textgrids |> 
    group_by(file) |> 
    mutate(phones = list(textgrid[[1]]$phones$label)) |> 
    group_by(utterance) |> 
    mutate(match_phones = phones[1]) |> 
    group_by(file) |> 
    mutate(is_same = all(phones[[1]] == match_phones[[1]])) |> 
    group_by(utterance) |>
    summarize(all_same = all(is_same)) 
  
  
  word_check <- 
    textgrids |> 
    group_by(file) |> 
    mutate(phones = list(textgrid[[1]]$words$label)) |> 
    group_by(utterance) |> 
    mutate(match_phones = phones[1]) |> 
    group_by(file) |> 
    mutate(is_same = all(phones[[1]] == match_phones[[1]])) |> 
    group_by(utterance) |>
    summarize(all_same = all(is_same)) |> 
    pluck('all_same') |> 
    all()
  
  
  if (!word_check)
    stop("Phone labelling issues")
  tools::md5sum(textgrids$path)
}
