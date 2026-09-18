#######################################################################################################################################
#### resample survey data: all data
####
#######################################################################################################################################

####clear environment
rm(list=ls())

####set wds
 wd = "C:/Users/Derek.Bolser/Documents/Github/Resample-survey-data"
 data = "C:/Users/Derek.Bolser/Documents/Github/Resample-survey-data/code/Results"

library(tidyverse)

# Define species and their corresponding folder and file names
# note: tribble is a row-wise way to create a tibble
species_info <- tribble( 
  ~folder,                ~file,                        ~species,
  "Longnose_skate",       "stratified_resampled_longnose_skate_indices_df.csv",    "Longnose skate",
  "Pacific_ocean_perch",  "stratified_resampled_Pacific_ocean_perch_indices_df.csv",         "Pacific ocean perch",
  "Petrale_sole",         "stratified_resampled_petrale_sole_indices_df.csv",     "Petrale sole",
  "Sablefish",            "stratified_resampled_sablefish_indices_df.csv",   "Sablefish",
  "Shortspine_thornyhead","stratified_resampled_shortspine_thornyhead_indices_df.csv",  "Shortspine thornyhead",
  "Yellowtail_rockfish",  "stratified_resampled_yellowtail_rockfish_indices_df.csv",  "Yellowtail rockfish"
)

figure_dir <- "plots"
data_dir <- "Results"

# Read all data frames and add species column
setwd(wd)
all_indices <- species_info |>
  mutate(path = file.path(data_dir, folder, file)) |>
  mutate(df = map2(path, species, ~ read_csv(.x) |> mutate(species = .y))) |>
  pull(df) |>
  bind_rows()

####plot results ################################################################################################################

#log biomass estimates
ggplot(all_indices, aes(x = as.factor(effort), y = log_est)) +
  geom_boxplot() +
  facet_wrap(~ species, scales = "free_y") +
  labs(x = "Proprotion of effort",
       y = "Log biomass estimate") +
  #theme_minimal() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

ggsave(filename = 'stratified_resampled_log_biomass_indices_boxplot.png',plot = last_plot() , path = "plots", width = 8, height = 8, device = 'png', dpi = 300)

#log(?) SE
ggplot(all_indices, aes(x = as.factor(effort), y = se)) +
  geom_boxplot() +
  facet_wrap(~ species, scales = "free_y") +
  labs(x = "Proprotion of effort",
       y = "Standard error of log biomass estimate") +
  #theme_minimal() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

ggsave(filename = 'stratified_resampled_log_biomass_index_SE_boxplot.png',plot = last_plot() , path = "plots", width = 8, height = 8, device = 'png', dpi = 300)

#biomass estimates
ggplot(all_indices, aes(x = as.factor(effort), y = est)) +
  geom_boxplot() +
  facet_wrap(~ species, scales = "free_y") +
  labs(x = "Proprotion of effort",
       y = "Biomass estimate") +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

ggsave(filename = 'stratified_resampled_biomass_indices_boxplot.png',plot = last_plot() , path = "plots", width = 8, height = 8, device = 'png', dpi = 300)

#### compare with non-stratified resampled indices
species_info_non_stratified <- tribble( 
  ~folder,                ~file,                        ~species,
  "Longnose_skate",       "longnose_indices_df.csv",    "Longnose skate",
  "Pacific_ocean_perch",  "pop_indices_df.csv",         "Pacific ocean perch",
  "Petrale_sole",         "petrale_indices_df.csv",     "Petrale sole",
  "Sablefish",            "sablefish_indices_df.csv",   "Sablefish",
  "Shortspine_thornyhead","shortspine_indices_df.csv",  "Shortspine thornyhead",
  "Yellowtail_rockfish",  "yellowtail_indices_df.csv",  "Yellowtail rockfish"
)

setwd(wd)
all_indices_not_stratified <- species_info_non_stratified |>
  mutate(path = file.path(data_dir, folder, file)) |>
  mutate(df = map2(path, species, ~ read_csv(.x) |> mutate(species = .y))) |>
  pull(df) |>
  bind_rows()

####get summaries of differences
stratified_summary<-all_indices%>%
  group_by(species,effort)%>%
  summarise(est = mean(est,na.rm = T),log_est = mean(log_est,na.rm = T),
            se = mean(se,na.rm = T))

not_stratified_summary<-all_indices_not_stratified%>%
  group_by(species,effort)%>%
  summarise(est = mean(est,na.rm = T),log_est = mean(log_est,na.rm = T),
            se = mean(se,na.rm = T))


difference_summary <- stratified_summary %>%
  left_join(
    not_stratified_summary,
    by = c("species", "effort"),
    suffix = c("_stratified", "_not_stratified")
  ) %>%
  transmute(
    species,
    effort,
    difference_est = est_stratified - est_not_stratified,
    difference_log_est = log_est_stratified - log_est_not_stratified,
    difference_se = se_stratified - se_not_stratified
  )

#make a plot
ggplot(difference_summary, aes(x = as.factor(effort), y = difference_est)) +
  geom_boxplot() +
  facet_wrap(~ species, scales = "free_y") +
  labs(x = "Proprotion of effort",
       y = "Difference between stratified and non-stratified resampled biomass estimates") +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

ggsave(filename = 'difference between stratified and non stratified resampled biomass estimates.png',plot = last_plot() , path = "plots", width = 8, height = 8, device = 'png', dpi = 300)

setwd(data_dir)
write.csv(difference_summary,"summary of differences between stratified and non stratified biomass estimates.csv",row.names = F)


