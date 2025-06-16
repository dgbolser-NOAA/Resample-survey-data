# messy script created during co-working with Elizabeth and Ian
# can be cleaned up later

library(ggplot2)
library(purrr)
library(dplyr)
library(here)
library(r4ss)
library(viridis)
source(model_output_plots.R)


# Plot indices
species_fleet_df <- data.frame(
  name = c("Longnose_skate", "Pacific_ocean_perch", "Petrale_sole",
           "Sablefish", "Shortspine_thornyhead", "Yellowtail_rockfish"),
  fleet = c(5, 8, 4, 7, 6, 6)
)

# Black dotted line is the original, need to figure out how to get ggplot to add that to the legend
plot_effort_vs_og_indices(species_fleet_df, plot_save_dir = here::here("plots"))

# Plot biological composition comparisons
# resampled_dirs <- list.dirs("resampled_models", full.names = TRUE, recursive = FALSE)
# fleet_lookup <- c(
#   "Longnose_skate" = 5,
#   "Pacific_ocean_perch" = 8,
#   "Petrale_sole" = 4,
#   "Sablefish" = 7,
#   "Shortspine_thornyhead" = 6,
#   "Yellowtail_rockfish" = 6
# )
# plot_composition_comparisons(dir_list = resampled_dirs, 
#                              fleet_lookup, 
#                              plot_save_dir = here::here("plots"))


# Plot model results
resampled_dirs <- list.dirs("resampled_models", full.names = TRUE, recursive = FALSE)
all_models <- r4ss::SSgetoutput(dirvec = resampled_dirs)
summaryoutput <- r4ss::SSsummarize(all_models)
summaryoutput$modelnames <- basename(resampled_dirs)

# The following models did not invert the hessian and need to be sorted out
# Make sure that the uncertainty plots are working correctly when there are iterations
# that did not run
# summaryoutput$modelnames[which(summaryoutput$BratioSD |> dpl 
#                                yr::filter(Yr == 2010) == 0)]
# [1] "Pacific_ocean_perch_0.4_9" "Sablefish_0.2_3"
# [3] "Sablefish_0.2_7"           "Sablefish_0.4_6"
# [5] "Sablefish_0.4_8"           "Sablefish_0.4_9"
# [7] "Sablefish_0.8_1"           "Sablefish_0.8_2"
# [9] "Sablefish_0.8_7"           "Sablefish_1_1"
# [11] "Yellowtail_rockfish_0.8_1"

plot_comparisons_ggplot(
    summaryoutput,
    subplots = c(1,2,3,4,5),
    models = "all",
    legendlabels = basename(resampled_dirs),
    show_equilibrium = TRUE,
    summarize_by_species_effort = TRUE,
    plot_save_dir = here::here("plots")
)


### All of Ian's stuff
# temporary stuff for exploring scale of index estimates
all_indices <- read.csv(here::here("Results/Petrale_sole/petrale_indices_df.csv")) |>
  filter(!is.na(se))
petrale_indices <- all_indices |>
  filter(species == "Petrale sole")
petrale_inputs <- r4ss::SS_read("original_models/Petrale_sole")
petrale_index <- petrale_inputs$dat$CPUE  |> dplyr::filter(index == 4)

# Get ratio to determine how much to multiply stock assessment index by to match
# derek's index from sdmTMB
mean(all_indices$est)/mean(petrale_index$obs)

plot(petrale_index$year, petrale_index$obs)
plot(petrale_indices$Year, petrale_indices$est, ylim = c(0, 3e6))
# original index in petrale assessment is off by a factor of about 20
lines(petrale_index$year, 20*petrale_index$obs, col = 2, lwd = 3)

# 100% effort index has very high correlation with the original index for petrale
petrale_new_effort1 <- all_indices |>
  filter(species == "Petrale sole" & 
           effort == 1 & Year <= 2022)
cor(x = petrale_new_effort1$est, y = petrale_index$obs)
# [1] 0.9980627

# look at some assessment model output for petrale and shortspine thornyhead
library(r4ss)
petrale_output <- r4ss::SS_output("original_models/Petrale_sole")
petrale_effort1 <- r4ss::SS_output("resampled_models/Petrale_sole_1_1")
petrale_0.2_3 <- r4ss::SS_output("resampled_models/Petrale_sole_0.2_3")

# get list of directories within the resampled_models folder
resampled_dirs <- list.dirs("resampled_models", full.names = TRUE, recursive = FALSE)
# temporarily filter for directories that contain "Petrale_sole"
petrale_dirs <- resampled_dirs[grepl("Petrale_sole", resampled_dirs)]
petrale_models <- r4ss::SSgetoutput(dirvec = petrale_dirs)
SSplotComparisons(SSsummarize(petrale_models),
                  legendlabels = basename(petrale_dirs))
petrale_summary <- SSsummarize(petrale_models)

shortspine_dirs <- resampled_dirs[grepl("Shortspine", resampled_dirs)]

shortspine_models <- r4ss::SSgetoutput(dirvec = shortspine_dirs)
SSplotComparisons(SSsummarize(shortspine_models),
                  legendlabels = basename(shortspine_dirs))
shortspine_summary <- SSsummarize(shortspine_models)
# look at index uncertainty
SSplotComparisons(SSsummarize(shortspine_models),
                  subplots = 13, indexPlotEach = TRUE)

# average input sample size is much lower for effort = 0.2 model
petrale_effort1$condbase |> dplyr::filter(Fleet == 4) |> pull(Nsamp_in)  |> mean()
# [1] 38.37296
petrale_0.2_1$condbase |> dplyr::filter(Fleet == 4) |> pull(Nsamp_in)  |> mean()
# [1] 9.448802

# adjusted input sample sizes are more similar
petrale_effort1$condbase |> dplyr::filter(Fleet == 4) |> pull(Nsamp_adj)  |> mean()
# [1] 0.190236
petrale_0.2_1$condbase |> dplyr::filter(Fleet == 4) |> pull(Nsamp_adj)  |> mean()
# [1] 0.1372155

# for lengths, the input sample size is about 20% for the 0.2 effort model
petrale_effort1$lendbase |> dplyr::filter(Fleet == 4) |> pull(Nsamp_in)  |> mean()
#[1] 1012.947
petrale_0.2_1$lendbase |> dplyr::filter(Fleet == 4) |> pull(Nsamp_in)  |> mean()
#[1] 215.4211
# adjusted sample sizes are still pretty different
petrale_effort1$lendbase |> dplyr::filter(Fleet == 4) |> pull(Nsamp_adj)  |> mean()
#[1] 71.08765
petrale_0.2_1$lendbase |> dplyr::filter(Fleet == 4) |> pull(Nsamp_adj)  |> mean()
#[1] 42.30675

shortspine_models$replist1$StartTime
shortspine_models$replist2$StartTime

library(purrr)
start_times <- map(paste0("replist", 1:11), ~ shortspine_models[[.x]]$StartTime)
