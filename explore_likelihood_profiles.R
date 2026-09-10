# this code has eleements from explore_results.R

# likelihood profiles for shortspine models to explore influence of data by type
resampled_dirs <- list.dirs(
  "resampled_models",
  full.names = TRUE,
  recursive = FALSE
)
# temporarily filter for directories that contain "Petrale_sole"
petrale_dirs <- resampled_dirs[grepl("Petrale_sole", resampled_dirs)]
shortspine_dirs <- resampled_dirs[grepl("Shortspine", resampled_dirs)]

petrale_models <- r4ss::SSgetoutput(dirvec = petrale_dirs)
names(petrale_models) <- basename(petrale_dirs)
petrale_summary <- r4ss::SSsummarize(petrale_models)


shortspine_models <- r4ss::SSgetoutput(
  dirvec = shortspine_dirs,
  modelnames = basename(shortspine_dirs)
)
names(shortspine_models) <- basename(shortspine_dirs)
shortspine_summary <- r4ss::SSsummarize(shortspine_models)

SSplotComparisons(shortspine_summary, subplots = 1:7)
subset <- which(
  grepl("_0.2_2", shortspine_summary$modelnames) |
    grepl("_0.8_1", shortspine_summary$modelnames) |
    grepl("_1_1", shortspine_summary$modelnames)
)
SSplotComparisons(
  shortspine_summary,
  subplots = 1:15,
  models = subset,
  legendlabels = shortspine_summary$modelnames[subset]
)
subset <- which(
  grepl("_0.2", shortspine_summary$modelnames)
)
SSplotComparisons(
  shortspine_summary,
  subplots = 1,
  models = subset,
  legendlabels = shortspine_summary$modelnames[subset]
)


# run likelihood profile for 0.2_2 and 0.8_1 models to explore influence of data by type

# copy model files to new directories and run likelihood profile over log(R0)
dir.create("profiles")

dir1 <- file.path(
  "profiles",
  paste0(basename(shortspine_dirs[grepl("_0.2_2", shortspine_dirs)]), "_fine")
)
dir2 <- file.path(
  "profiles",
  paste0(basename(shortspine_dirs[grepl("_0.8_1", shortspine_dirs)]), "_fine")
)

dir.old <- shortspine_dirs[grepl("_0.2_2", shortspine_dirs)]
r4ss::copy_SS_inputs(
  dir.old = dir.old,
  dir.new = dir1
)
dir.old <- shortspine_dirs[grepl("_0.8_1", shortspine_dirs)]
r4ss::copy_SS_inputs(
  dir.old = dir.old,
  dir.new = dir2
)

# get reasonable range for profile
info <- shortspine_models[[1]]$parameters["SR_LN(R0)", c("Value", "Parm_StDev")]
qnorm(
  p = c(0.025, 0.975),
  mean = as.numeric(info["Value"]),
  sd = as.numeric(info["Parm_StDev"])
)

info <- shortspine_models[[13]]$parameters[
  "SR_LN(R0)",
  c("Value", "Parm_StDev")
]
qnorm(
  p = c(0.025, 0.975),
  mean = as.numeric(info["Value"]),
  sd = as.numeric(info["Parm_StDev"])
)

# logR0vec <- seq(9.0, 9.7, by = 0.1)
logR0vec <- seq(9.1, 9.6, by = 0.05)

prof1 <- r4ss::profile(
  dir = dir1,
  newctlfile = "SST_control.ss",
  profilevec = logR0vec,
  string = "SR_LN(R0)",
  extras = "-nohess"
)

prof2 <- r4ss::profile(
  dir = dir2,
  newctlfile = "SST_control.ss",
  profilevec = logR0vec,
  string = "SR_LN(R0)",
  extras = "-nohess"
)

# read profile output
prof_models_0.2 <- r4ss::SSgetoutput(
  dirvec = dir1,
  keyvec = 1:length(logR0vec),
  modelnames = paste0("logR0_", logR0vec)
)
prof_models_0.8 <- r4ss::SSgetoutput(
  dirvec = dir2,
  keyvec = 1:length(logR0vec),
  modelnames = paste0("logR0_", logR0vec)
)
# add MLE model to the list of profile models
# first re-read the MLE models to get the correct model names
mod_Shortspine_thornyhead_0.2_2 <- r4ss::SS_output(
  "resampled_models/Shortspine_thornyhead_0.2_2"
)
mod_Shortspine_thornyhead_0.8_1 <- r4ss::SS_output(
  "resampled_models/Shortspine_thornyhead_0.8_1"
)
prof_models_0.2[["MLE"]] <- mod_Shortspine_thornyhead_0.2_2
prof_models_0.8[["MLE"]] <- mod_Shortspine_thornyhead_0.8_1

prof_summary_0.2 <- r4ss::SSsummarize(prof_models_0.2)
prof_summary_0.8 <- r4ss::SSsummarize(prof_models_0.8)
r4ss::SSplotProfile(
  prof_summary_0.2,
  profile.string = "R0",
  profile.label = "Log recruitment (R0)",
  plotdir = "plots",
  plot = FALSE,
  print = TRUE
)
file.rename(
  "plots/profile_plot_likelihood.png",
  "plots/profile_R0_shortspine_0.2_fine.png"
)
r4ss::SSplotProfile(
  prof_summary_0.8,
  profile.string = "R0",
  profile.label = "Log recruitment (R0)",
  plotdir = "plots",
  plot = FALSE,
  print = TRUE,
  models = "converged"
)
file.rename(
  "plots/profile_plot_likelihood.png",
  "plots/profile_R0_shortspine_0.8_fine.png"
)

r4ss::PinerPlot(
  prof_summary_0.2,
  profile.string = "R0",
  profile.label = "Log recruitment (R0)",
  component = "Surv_like",
  main = "Changes in index likelihood by fleet",
  plotdir = "plots",
  plot = FALSE,
  print = TRUE,
  models = "converged",
  ymax = 12
)
file.rename(
  "plots/profile_plot_likelihood.png",
  "plots/profile_indices_R0_shortspine_0.2_fine.png"
)
r4ss::PinerPlot(
  prof_summary_0.8,
  profile.string = "R0",
  profile.label = "Log recruitment (R0)",
  component = "Surv_like",
  main = "Changes in index likelihood by fleet",
  plotdir = "plots",
  plot = FALSE,
  print = TRUE,
  models = "converged",
  ymax = 12
)
file.rename(
  "plots/profile_plot_likelihood.png",
  "plots/profile_indices_R0_shortspine_0.8_fine.png"
)

r4ss::PinerPlot(
  prof_summary_0.2,
  profile.string = "R0",
  profile.label = "Log recruitment (R0)",
  component = "Length_like",
  main = "Changes in length likelihood by fleet",
  plotdir = "plots",
  plot = FALSE,
  print = TRUE,
  models = "converged",
  ymax = 25
)
file.rename(
  "plots/profile_plot_likelihood.png",
  "plots/profile_length_R0_shortspine_0.2_fine.png"
)
r4ss::PinerPlot(
  prof_summary_0.8,
  profile.string = "R0",
  profile.label = "Log recruitment (R0)",
  component = "Length_like",
  main = "Changes in length likelihood by fleet",
  plotdir = "plots",
  plot = FALSE,
  print = TRUE,
  models = "converged",
  ymax = 25
)
file.rename(
  "plots/profile_plot_likelihood.png",
  "plots/profile_length_R0_shortspine_0.8_fine.png"
)


nwfscDiag::plot_profile(
  mydir = dir1,
  rep = prof_models_0.2,
  para = "SR_LN(R0)",
  profilesummary = prof_summary_0.2
)
nwfscDiag::plot_profile(
  mydir = dir2,
  rep = prof_models_0.8,
  para = "SR_LN(R0)",
  profilesummary = prof_summary_0.8
)

# get maximum gradient component for each model in the profiles
lapply(prof_models_0.2, function(x) x$maximum_gradient_component) |>
  unlist() |>
  round(3)
lapply(prof_models_0.8, function(x) x$maximum_gradient_component) |>
  unlist() |>
  round(3)

# get max gradient for all shortspine models
lapply(shortspine_models, function(x) x$maximum_gradient_component) |>
  unlist() |>
  round(3)

# get maximum gradient component for all models for all species
dirs <- dir("resampled_models", full.names = TRUE)
# function to get maximum gradient which will be the line within the first
# 30 lines of the Report.sso file which has format like
# "Convergence_Level: 3.65129e-05 is_final_gradient"
get_max_gradient <- function(report_file) {
  lines <- readLines(report_file, n = 30)
  conv_line <- grep("Convergence_Level", lines, value = TRUE)
  as.numeric(sub(
    "Convergence_Level: ([0-9.eE+-]+) is_final_gradient",
    "\\1",
    conv_line
  ))
}
# apply the function to all Report.sso files in the resampled_models directories
max_gradients <- sapply(file.path(dirs, "Report.sso"), get_max_gradient)
round(max_gradients, 3) |> table()
#   0 0.001 0.002 0.061 39.42
#  57    18     1     1     1
data.frame(dirs = dirs, max_gradient = max_gradients) |>
  tibble::as_tibble() |>
  dplyr::arrange(desc(max_gradient))
