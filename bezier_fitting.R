library(targets)
library(sosprosody)
library(ggplot2)
library(bezier)
library(tidyr)
library(furrr)
library(future)
library(purrr)
library(dplyr)
library(readr)
source("Helpers/tcog_helpers.R")

tar_load(c(textgrid_df, 
           files_to_omit,
           process_pitchtiers,
           nuclear_words,
           three_syl_words,
           tune_order,
           syllabification_spreadsheet))
tg_df <- 
  textgrid_df |> 
  dplyr::filter(!file %in% files_to_omit$file)

# Extract nuclear regions
nuclear_regions <- get_nuclear_textgrids(tg_df, nuclear_words)

# Preprocess pitchtracks
pitchtier_df <- 
  process_pitchtiers  |> 
  preprocess_pitchtracks(nuclear_regions, 
                         runmed_k = 5, 
                         time_normalize = TRUE,
                         .fromzero = TRUE) |> 
  mutate(tune = factor(tune, levels = tune_order),
         wordset = ifelse(utterance %in% three_syl_words, "syl3", "syl2"))

syls <- read_csv(syllabification_spreadsheet)

# pitchtier_df |> 
#   dplyr::filter(is_nuclear) |> 
#   left_join(syls, by = c('utterance' = 'utterance', 'phone_i' = 'phone_i'))

labeled_pitch_tiers <- 
  process_pitchtiers |>
  left_join(dplyr::select(tg_df, file,word_start, word_end, phone_start, phone_end, phone_label, phone_i, is_nuclear),
            by = join_by(file == file, timepoint <= phone_end, timepoint >= phone_start)) |> 
  dplyr::filter(is_nuclear) |> 
  group_by(file) |> 
  mutate(phone_i = phone_i - (min(phone_i)-1)) |> 
  left_join(dplyr::select(syls, phone_i, utterance, syllable, syl_position_i, syl_position_narrow),
            by = c('phone_i' = 'phone_i', 'utterance' = 'utterance'))


prepped_contours <- 
  labeled_pitch_tiers |> 
  dplyr::filter(!tune %in% c('HHH',"LHH")) |> 
  group_by(file, syllable) |> 
  mutate(syl_end = max(timepoint),
         syl_max = ifelse(syllable == 1, syl_end + .025, 0)) |> 
  group_by(file) |> 
  mutate(syl_max = max(syl_max),
         retain = timepoint <= syl_max) |> 
  dplyr::filter(retain) |> 
  sosprosody::preprocess_pitchtracks(runmed_k = 5,
                                     time_normalize = TRUE,
                                     .fromzero = TRUE)  

peak_trajectories <- 
  prepped_contours |>
  mutate(retain = hz > 60) |>
  group_by(file) |> 
  mutate(retain = all(retain)) |> 
  dplyr::filter(retain) |>
  mutate(peak_height= max(hz_runmed),
         peak_location = hz_runmed == peak_height,
         peak_location = timepoint_norm[peak_location][1],
         retain = timepoint_norm <= peak_location) |> 
  dplyr::filter(retain) |> 
  mutate(t = timepoint_norm) |> 
  dplyr::filter(!utterance %in% three_syl_words)

peak_trajectories
# 
#   
# tstdf <- 
#   dplyr::filter(peak_trajectories, file == 'branning_01_HLL_001') |> 
#   ungroup()
#   
# tstmat <- as.matrix(dplyr::select(tstdf, timepoint_norm, hz_runmed))

# bezier_fit <- 
#   bezierCurveFit(tstmat,
#                min.control.points = 4,
#                max.control.points = 4,
#                fix.start.end = TRUE)
# 
# 
# bezier_points <- bezier::pointsOnBezier(p = bezier_fit$p, n = 20)$points
# bezier(bezier_fit$p[[1]], bezier_fit$p[[2]],)
# 
# bezier_points |>
#   as.data.frame() |> 
#   ggplot(aes(V1, V2))+
#   geom_line() +
#   geom_point() +
#     theme_bw() +
#   geom_line(data= tstdf,
#             aes(x = timepoint_norm, y = hz_runmed), color = 'red')

N_CONTROL_POINTS <- 3
plan(multisession(workers = 10))

bezier_fits <- 
  peak_trajectories |> 
  group_by(file) |> 
  # dplyr::filter(tune == "LSHLL") |>
  split(~file) |> 
  furrr::future_map_dfr(\(x) {
    x |> 
    summarize(bezier_fit = bezierCurveFit(as.matrix(data.frame(timepoint_norm, hz_runmed)),
                                          min.control.points = N_CONTROL_POINTS,
                                          max.control.points = N_CONTROL_POINTS,
                                          maxiter = 4000,
                                          fix.start.end = TRUE) |> list())
  },.progress = TRUE)
  

bezier_curves <- 
  bezier_fits |> 
  group_by(file) |>
  split(~file) |> 
  furrr::future_map_dfr(\(x) {
    mutate(x, bezier_points  = bezier::pointsOnBezier(p = bezier_fit[[1]]$p, n = 45)$points |> list())
  },.progress = TRUE)
  

bezier_points <- 
  bezier_curves |>
  # head(2) |> 
  dplyr::select(-bezier_fit) |> 
  reframe(file, 
            t = bezier_points[[1]][,1],
            hz = bezier_points[[1]][,2])

bezier_control_points <- 
  bezier_fits |>
  group_by(file) |> 
  reframe(file,
          p_t = bezier_fit[[1]]$p[[1]],
          p_hz = bezier_fit[[1]]$p[[2]],
          p_i = seq_len(N_CONTROL_POINTS))


# bezier_curves_plot <- 
  bezier_points |> 
  separate(file,
           into = c('utterance','session','tune','take'),remove = FALSE) |> 
  ggplot(aes(x = t, y = hz, group = file ,color = tune)) +
  geom_line() +
  facet_wrap(~tune) +
  theme_bw() +
  coord_fixed(ratio = 1/45)

average_points <- 
  bezier_control_points |> 
  separate(file,
           into = c('utterance','session','tune','take'),remove = FALSE) |> 
  group_by(tune, p_i) |> 
  summarize(mn_t = mean(p_t),
            sd_t = sd(p_t),
            mn_hz = mean(p_hz),
            sd_t = sd(p_hz))

average_curves <- 
  average_points |> 
  group_by(tune) |> 
  summarize(bezier_fit = bezier::pointsOnBezier(p = as.matrix(data.frame(mn_t, mn_hz)), n = 25)$points |> list()) |> 
  group_by(tune) |> 
  reframe(tune, 
          mn_t = bezier_fit[[1]][,1],
          mn_hz = bezier_fit[[1]][,2])


average_tcogs <- 
  average_curves |> 
  group_by(tune) |> 
  summarize(tcog_t = get_tcog(mn_hz, mn_t),
            tcog_f = get_tcog(mn_t, mn_hz))

empirical_tcogs <- 
  peak_trajectories |> 
  group_by(file, tune) |> 
  summarize(tcog_t = get_tcog(hz_runmed,timepoint_norm),
            tcog_f = get_tcog(timepoint_norm, hz_runmed))

average_empirical_tcog <- 
  empirical_tcogs |> 
  group_by(tune) |>
  summarize(mn_tcog_t = mean(tcog_t),
            mn_tcog_f = mean(tcog_f))

# control_points_plot <- 
  bezier_control_points |> 
  separate(file,
           into = c('utterance','session','tune','take'),remove = FALSE) |> 
  ggplot(aes(x = p_t, y = p_hz, color = factor(p_i)), group = file) +
    geom_line(
      data = bezier_points |> 
        separate(file,
                 into = c('utterance','session','tune','take'),remove = FALSE),
      aes(x = t, y = hz, group = file ,color = tune),
      color = 'gray80'
    ) +
  geom_point()  +
  geom_point(data = average_points,
             aes(x = mn_t,
                 y = mn_hz,
                 group = tune),
             color = 'black',
             shape = 2,
             size = 2) +
  geom_line(data = average_curves,
            aes(x = mn_t, y = mn_hz, group = tune),
            color = 'black') +
  geom_point(data = average_tcogs,
             aes(x = tcog_t, y = tcog_f, group = tune),
             shape = 8, color = 'black') +
    geom_point(data = average_empirical_tcog,
               aes(x = mn_tcog_t, y = mn_tcog_f, group = tune),
               shape = 13, color = 'black') +
    theme_bw() +
  facet_wrap(~tune)+
    coord_fixed(ratio = 1/45) +
    scale_color_brewer(palette = 'Dark2') +
    guides(color=guide_legend(title="Control Point index")) +
    theme(legend.position = 'top') +
    xlab("Stressed Syllable Duration (normalized)") +
    ylab("F0 (Hz)")
