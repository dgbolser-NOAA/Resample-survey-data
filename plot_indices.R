#######################################################################################################################################
#### resample survey data: all data
####
#######################################################################################################################################

####clear environment
rm(list=ls())

####set wds
# wd = "C:/Users/Derek.Bolser/Documents/Resample_survey_data"
# data = "C:/Users/Derek.Bolser/Documents/Resample_survey_data/code/Results"

library(tidyverse)

# Define species and their corresponding folder and file names
# note: tribble is a row-wise way to create a tibble
species_info <- tribble( 
  ~folder,                ~file,                        ~species,
  "Arrowtooth_flounder",  "arrowtooth_indices_df.csv",  "Arrowtooth flounder",
  "Bocaccio",             "bocaccio_indices_df.csv",    "Bocaccio",
  "Canary_rockfish",      "canary_indices_df.csv",      "Canary rockfish",
  "Darkblotched_rockfish","darkblotched_indices_df.csv","Darkblotched rockfish",
  "Dover_sole",           "dover_indices_df.csv",       "Dover sole",
  "Lingcod_north",        "lingcod_n_indices_df.csv",   "Lingcod (North)",
  "Lingcod_south",        "lingcod_s_indices_df.csv",   "Lingcod (South)",
  "Longnose_skate",       "longnose_indices_df.csv",    "Longnose skate",
  "Pacific_ocean_perch",  "pop_indices_df.csv",         "Pacific ocean perch",
  "Pacific_spiny_dogfish","dogfish_indices_df.csv",     "Pacific spiny dogfish",
  "Petrale_sole",         "petrale_indices_df.csv",     "Petrale sole",
  "Rex_sole",             "rex_indices_df.csv",         "Rex sole",
  "Sablefish",            "sablefish_indices_df.csv",   "Sablefish",
  "Shortspine_thornyhead","shortspine_indices_df.csv",  "Shortspine rockfish",
  "Widow_rockfish",       "widow_indices_df.csv",       "Widow rockfish",
  "Yellowtail_rockfish",  "yellowtail_indices_df.csv",  "Yellowtail rockfish"
)

figure_dir <- "Figures"
data_dir <- "Results"

# Read all data frames and add species column
all_indices <- species_info |>
  mutate(path = file.path(data_dir, folder, file)) |>
  mutate(df = map2(path, species, ~ read_csv(.x) |> mutate(species = .y))) |>
  pull(df) |>
  bind_rows()

####plot results ################################################################################################################

#log biomass estimates
ggplot(all_indices, aes(x = as.factor(effort), y = log_est)) +
  geom_boxplot() +
  facet_wrap(~ species) +
  labs(x = "Proprotion of effort",
       y = "Log biomass estimate") +
  #theme_minimal() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

ggsave(filename = 'all_indices_boxplot_log_biomass.png',plot = last_plot() , path = "Figures", width = 8, height = 8, device = 'png', dpi = 300)

#log(?) SE
ggplot(all_indices, aes(x = as.factor(effort), y = se)) +
  geom_boxplot() +
  facet_wrap(~ species) +
  labs(x = "Proprotion of effort",
       y = "Standard error of log biomass estimate") +
  #theme_minimal() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

ggsave(filename = 'all_indices_boxplot_log_biomass_SE.png',plot = last_plot() , path = "Figures", width = 8, height = 8, device = 'png', dpi = 300)

#biomass estimates
ggplot(all_indices, aes(x = as.factor(effort), y = est)) +
  geom_boxplot() +
  facet_wrap(~ species) +
  labs(x = "Proprotion of effort",
       y = "Biomass estimate") +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

ggsave(filename = 'all_indices_boxplot_biomass.png',plot = last_plot() , path = "Figures", width = 8, height = 8, device = 'png', dpi = 300)
