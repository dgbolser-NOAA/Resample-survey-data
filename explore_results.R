
# temporary stuff for exploring scale of index estimates
petrale_indices <- all_indices |>
  filter(species == "Petrale sole") 
petrale_inputs <- r4ss::SS_read("original_models/Petrale_sole")
petrale_index <- petrale_inputs$dat$CPUE  |> dplyr::filter(index == 4)
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