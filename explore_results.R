# messy script created during co-working with Elizabeth and Ian
# can be cleaned up later
library(ggplot2)
library(purrr)
library(dplyr)
library(here)
library(r4ss)
library(viridis)
library(stringr)
library(tidyr)
library(patchwork)
library(ggtext)
library(patchwork)
library(ss3sim)
source(here::here("model_output_plots.R"))

resampled_dirs <- list.dirs("resampled_models", full.names = TRUE, recursive = FALSE)

# Plot indices -----------------------------------------------------------------
species_fleet_df <- data.frame(
  name = c("Longnose_skate", "Pacific_ocean_perch", "Petrale_sole",
           "Sablefish", "Shortspine_thornyhead", "Yellowtail_rockfish"),
  fleet = c(5, 8, 4, 10, 6, 6)
)

# Black dotted line is the original, need to figure out how to get ggplot to add that to the legend
plot_effort_vs_og_indices(species_fleet_df, plot_save_dir = here::here("plots"))


# Plot biological composition comparisons --------------------------------------
fleet_lookup <- c(
  "Longnose_skate" = 5,
  "Pacific_ocean_perch" = 8,
  "Petrale_sole" = 4,
  "Sablefish" = 10,
  "Shortspine_thornyhead" = 6,
  "Yellowtail_rockfish" = 6
)
plot_composition_comparisons(dir_list = resampled_dirs,
                             fleet_lookup,
                             plot_save_dir = here::here("plots"))


# Plot model results -----------------------------------------------------------
all_models <- r4ss::SSgetoutput(dirvec = resampled_dirs, modelnames = basename(resampled_dirs))
summaryoutput <- r4ss::SSsummarize(all_models, )
summaryoutput$modelnames <- basename(resampled_dirs)


#### TRY KIVA'S VIOLIN PLOTS OF ESTIMATED PARAMETERS ####
extract_ss3sim_style <- function(report, model_name = NA_character_) {
  scalar <- ss3sim:::get_results_scalar(report)
  ts     <- ss3sim:::get_results_timeseries(report)
  dq     <- ss3sim:::get_results_derived(report)
  
  scalar$model_run <- model_name
  ts$model_run     <- model_name
  dq$model_run     <- model_name
  
  list(scalar = scalar, ts = ts, dq = dq)
}

res <- lapply(names(all_models), function(nm) extract_ss3sim_style(all_models[[nm]], nm))

scalar <- dplyr::bind_rows(lapply(res, `[[`, "scalar"))
ts     <- dplyr::bind_rows(lapply(res, `[[`, "ts"))
dq     <- dplyr::bind_rows(lapply(res, `[[`, "dq"))

end_yrs <- tibble::tribble(
  ~species, ~endyr,
  "Longnose_skate", 2018,
  "Pacific_ocean_perch", 2016,
  "Petrale_sole", 2022,
  "Sablefish", 2024,
  "Shortspine_thornyhead", 2022,
  "Yellowtail_rockfish", 2024
)

# End year recruitment
rec_0 <- ts |>
  mutate(model_name = model_run) |>
  tidyr::separate(
    col = model_run,
    into = c("species", "effort", "iter"),
    sep  = "_(?=[^_]+$)|_(?=[^_]+_(?=[^_]+$))",
    remove = FALSE
  ) |>
  left_join(end_yrs, by = "species") |>
  filter(is.na(year) | year == endyr) |>
  select(species, effort, iter, Recruit_0) |>
  group_by(species) |>
  mutate(Recruit_0_effort1 = Recruit_0[effort == 1][1]) |>
  ungroup() |>
  filter(effort != 1) |>
  mutate(effort = factor(effort),
         are = abs((Recruit_0 - Recruit_0_effort1) / Recruit_0_effort1), 
         rel_error = (log(Recruit_0) - log(Recruit_0_effort1)) / log(Recruit_0_effort1)
         ) |>
  group_by(species, effort) |>
  ggplot() +
  geom_violin(
    aes(x = species, y = rel_error, fill = effort),
    width = 0.9,
    alpha = 0.9,
    col = 'white',
    linewidth = 0.6,
    scale = "width"
  ) +
  geom_hline(yintercept = 0, color = 'black') +
  geom_vline(
    xintercept = seq(1.5, 6 - 0.5, by = 1),  # between 1&2, 2&3, ...
    color = "grey80",
    linewidth = 0.6
  ) +
  scale_fill_manual(values = rev(LaCroixColoR::lacroix_palette('Orange', 4))) +
  theme_classic() +
  labs(x = 'Species', y = 'Relative error to 100% effort of \nlog(terminal year recruitment)')

  ggsave("endyr_rec_plot.png", plot = rec_0, path = here::here("plots"))




# Unfished recruitment
unfished_rec <- scalar |>
  mutate(model_name = model_run) |>
  tidyr::separate(
    col = model_run,
    into = c("species", "effort", "iter"),
    sep  = "_(?=[^_]+$)|_(?=[^_]+_(?=[^_]+$))",
    remove = FALSE
  ) |>
  select(species, effort, iter, SR_LN_R0) |>
  group_by(species) |>
  mutate(SR_LN_R0_effort1 = SR_LN_R0[effort == 1][1]) |>
  ungroup() |>
  filter(effort != 1) |>
  mutate(effort = factor(effort),
         are = abs((SR_LN_R0 - SR_LN_R0_effort1) / SR_LN_R0_effort1), 
         rel_error = (log(SR_LN_R0) - log(SR_LN_R0_effort1)) / log(SR_LN_R0_effort1)
  ) |>
  group_by(species, effort) |>
  ggplot() +
  geom_violin(
    aes(x = species, y = rel_error, fill = effort),
    width = 0.9,
    alpha = 0.9,
    col = 'white',
    linewidth = 0.6,
    scale = "width"
  ) +
  geom_hline(yintercept = 0, color = 'black') +
  geom_vline(
    xintercept = seq(1.5, 6 - 0.5, by = 1),  # between 1&2, 2&3, ...
    color = "grey80",
    linewidth = 0.6
  ) +
  scale_fill_manual(values = rev(LaCroixColoR::lacroix_palette('Orange', 4))) +
  theme_classic() +
  labs(x = 'Species', y = 'Relative error to 100% effort of \nlog(unfished recruitment)')
  ggsave("unfished_rec_plot.png", plot = unfished_rec, path = here::here("plots"))

nat_M <- scalar |>
  mutate(model_name = model_run) |>
  tidyr::separate(
    col = model_run,
    into = c("species", "effort", "iter"),
    sep  = "_(?=[^_]+$)|_(?=[^_]+_(?=[^_]+$))",
    remove = FALSE
  ) |>
  select(species, effort, iter, NatM_uniform_Fem_GP_1) |>
  group_by(species) |>
  mutate(NatM_uniform_Fem_GP_1_effort1 = NatM_uniform_Fem_GP_1[effort == 1][1]) |>
  ungroup() |>
  filter(effort != 1) |>
  mutate(effort = factor(effort),
         are = abs((NatM_uniform_Fem_GP_1 - NatM_uniform_Fem_GP_1_effort1) / NatM_uniform_Fem_GP_1_effort1), 
         rel_error = (log(NatM_uniform_Fem_GP_1) - log(NatM_uniform_Fem_GP_1_effort1)) / log(NatM_uniform_Fem_GP_1_effort1)
  ) |>
  group_by(species, effort) |>
  ggplot() +
  geom_violin(
    aes(x = species, y = rel_error, fill = effort),
    width = 0.9,
    alpha = 0.9,
    col = 'white',
    linewidth = 0.6,
    scale = "width"
  ) +
  geom_hline(yintercept = 0, color = 'black') +
  geom_vline(
    xintercept = seq(1.5, 6 - 0.5, by = 1),  # between 1&2, 2&3, ...
    color = "grey80",
    linewidth = 0.6
  ) +
  scale_fill_manual(values = rev(LaCroixColoR::lacroix_palette('Orange', 4))) +
  theme_classic() +
  labs(x = 'Species', y = 'Relative error to 100% effort of \nlog(natural mortality)')
ggsave("nat_M_plot.png", plot = nat_M, path = here::here("plots"))

dq_long <- dq |>
  mutate(model_name = model_run) |>
  tidyr::separate(
    col = model_run,
    into = c("species", "effort", "iter"),
    sep  = "_(?=[^_]+$)|_(?=[^_]+_(?=[^_]+$))",
    remove = FALSE
  ) |>
  pivot_longer(
    cols = starts_with("Value."),
    names_to = "metric",
    values_to = "value",
    names_prefix = "Value."
  ) |>
  filter(!is.na(value)) |>
  group_by(metric, year) |>
  mutate(value_eff1 = value[effort == 1][1]) |>  # baseline within group
  ungroup() |>
  filter(!is.na(value_eff1)) |>
  group_by(metric, species, effort, year) |>
  summarize(
    mare = mean(abs((value - value_eff1) / value_eff1)),
    mre = mean((value - value_eff1) / value_eff1)
  ) |>
  ungroup() |>
  filter(effort != 1)

shade_df <- dq_long %>%
  mutate(year = as.numeric(year)) %>%
  filter(metric == "Bratio") %>%
  group_by(species) %>%
  summarise(max_year = max(year, na.rm = TRUE), .groups = "drop") %>%
  left_join(end_yrs, by = "species") %>%
  transmute(
    species,
    xmin = endyr + 1,
    xmax = max_year,
    ymin = -Inf,
    ymax = Inf
  ) %>%
  # in case some species don't need shading (endyr at/after last year)
  filter(xmin <= xmax)

mare_ts <- dq_long |>
  mutate(year = as.numeric(year)) |>
  filter(metric == "Bratio") |>
  # filter(species == "Longnose_skate") |>
  ggplot() +
  geom_rect(
    data = shade_df,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    inherit.aes = FALSE,
    fill = "grey90",
    color = NA
  ) +
  geom_line(aes(x = year, y = mare, col = factor(effort), group = effort)) +
  scale_color_manual(values = rev(LaCroixColoR::lacroix_palette('Orange', 4))) +
  labs(x = 'Year', color = 'Effort') +
  theme_classic() +
  scale_x_continuous(
    breaks = function(x) seq(floor(min(x, na.rm = TRUE)/40)*40,
                             ceiling(max(x, na.rm = TRUE)/40)*40,
                             by = 40)
  ) +
  facet_wrap(~ species, scales = 'free_y', nrow = 2, strip.position = 'right')
ggsave("mare_ts_Bratio_plot.png", plot = mare_ts, path = here::here("plots"))

# The following models did not invert the hessian and need to be sorted out
# Make sure that the uncertainty plots are working correctly when there are iterations
# that did not run
# summaryoutput$modelnames[which(summaryoutput$BratioSD |> dplyr::filter(Yr == 2010) == 0)]

plot_comparisons_ggplot(
  # Add an option to get results and then plot? Do SSgetoutput and SSsummarize for people?
  # Add input of fleet numbers for each species - important when scaling up 
  summaryoutput,
  all_output = all_models,
  subplots = c(1,2,3,4,5,6,7),
  models = "all",
  legendlabels = basename(resampled_dirs),
  show_equilibrium = TRUE,
  plot_save_dir = here::here("plots")
)


### All of Ian's stuff
# temporary stuff for exploring scale of index estimates
all_indices <- read.csv(here::here("Results/Shortspine_thornyhead/shortspine_indices_df.csv")) |>
  filter(!is.na(se)) |>
  filter(effort == 1.0)
# petrale_indices <- all_indices |>
#   filter(species == "Petrale sole")
st_inputs <- r4ss::SS_read("original_models/Shortspine_thornyhead")
st_index <- st_inputs$dat$CPUE  |> dplyr::filter(index == 6)

# Get ratio to determine how much to multiply stock assessment index by to match
# derek's index from sdmTMB
mean(all_indices$est)/mean(st_index$obs)

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
