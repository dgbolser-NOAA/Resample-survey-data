library(purrr)
library(tidyverse)

test_filter <- test_cleanup_bio[names(test_cleanup_bio) %in% sdm_model_filt$model_iter]


sdm_model <- read.csv(list.files(
  here::here("Results","Petrale_sole"),
  pattern = "*._indices_df",
  full.names = TRUE
)) |>
  filter(effort %in% c(0.2, 0.4, 0.8, 1)) |>
  mutate(model_iter =paste0(effort,"_", replicate))

# randomly sample 3 replicates from each effort
sdm_model_reps <- sdm_model |>
  distinct(model_iter, .keep_all = TRUE) |>
  group_by(effort) |>
  slice_sample(n = 3) |>
  ungroup()

sdm_model_filt <- sdm_model |>
  filter(model_iter %in% sdm_model_reps$model_iter)

plan(multisession)

furrr::future_map2(
  .x = catch_filtered,
  .y = bio_filtered,
  .f = put_data_in_models,
  dir = ,
  model_name = ,
  strata = ,
  fleet_number = ,
  species_group = 
)

put_data_in_models <- function(catch_filtered,
                               bio_filtered,
                               dir,
                               model_name,
                               strata,
                               fleet_number,
                               species_group,
                               )
  {
  
  dirs <- list.dirs(file.path(dir, "resampled"), recursive = FALSE)
  if (any(grepl(dirs, catch_filtered$source) == FALSE)) {
    copy_SS_inputs(
      dir.old = file.path(dir, "Models", model_name),
      dir.new = file.path(dir, "resampled", catch_filtered$source),
      create.dir = TRUE,
      overwrite = TRUE,
      use_ss_new = FALSE,
      verbose = TRUE
    )
  }
  
  ss3_inputs <- r4ss::SS_read(dir)
  
  # calculate length compositions from resampled survey data
  len_comp_new <- nwfscSurvey::get_expanded_comps(
    bio_data = bio_filtered,
    catch_data = catch_filtered,
    comp_bins = ss3_inputs$dat$lbin_vector,
    comp_column_name = "Length_cm",
    strata = strata,
    fleet = fleet_number,
    month = 7
  )
  
  # QUESTION: @iantaylor-NOAA - do we need this function, can we just use the
  # input_n param in get_expanded_comps?
  input_n <- nwfscSurvey::get_input_n(
    data = bio_filtered[[i]],
    species_group = species_group
  )
  
  len_comp_new <- len_comp_new$sexed
  # change capitalization and a few headers to match r4ss notation
  names(len_comp_new) <- tolower(names(len_comp_new))
  len_comp_new <- len_comp_new |>
    dplyr::rename(part = "partition", Nsamp = "input_n")
  
  # modify length data
  len_comp_new$Nsamp <- input_n |>
    dplyr::filter(sex_grouped == "sexed") |>
    dplyr::pull(input_n)
  
  # marginal age at length
  if (any(ss3_inputs$dat$agecomp$Lbin_hi == -1)) {
    maal <- nwfscSurvey::get_expanded_comps(
      data = bio_filtered[[i]],
      catch_data = catch_filtered[[i]],
      comp_bins = ss3_inputs$dat$agebin_vector,
      comp_column_name =  "age",
      strata = strata,
      fleet = fleet_number,
      month = 7
    )
    names(maal$female) <- names(ss3_inputs$dat$agecomp)
    names(maal$male) <- names(ss3_inputs$dat$agecomp)
    maal <- rbind(maal$female, maal$male)
    for (y in unique(maal$year)) {
      ageerr_y <- ss3_inputs$dat$agecomp |>
        dplyr::filter(year == y & fleet == fleet_number) |>
        dplyr::pull(ageerr) |>
        unique()
      maal$ageerr[maal$year == y] <- ageerr_y
    }
  }
  
  # conditional-age-at-length comps
  if (any(ss3_inputs$dat$agecomp$Lbin_hi != -1)) {
    caal <- nwfscSurvey::get_raw_caal(
      data = bio_filtered[[i]],
      len_bins = ss3_inputs$dat$lbin_vector,
      age_bins = ss3_inputs$dat$agebin_vector,
      fleet = fleet_number,
      month = 7
    )
    names(caal$female) <- names(ss3_inputs$dat$agecomp)
    names(caal$male) <- names(ss3_inputs$dat$agecomp)
    caal <- rbind(caal$female, caal$male)
    # figure out year-specific ageing error type
    # (petrale may be only species with multiple types due to WDFW ageing the survey fish in a few years)
    for (y in unique(caal$year)) {
      ageerr_y <- ss3_inputs$dat$agecomp |>
        dplyr::filter(year == y & fleet == fleet_number) |>
        dplyr::pull(ageerr) |>
        unique()
      caal$ageerr[caal$year == y] <- ageerr_y
    }
  }
  ss3_inputs$dat$lencomp <- ss3_inputs$dat$lencomp |> 
    dplyr::filter(fleet != fleet_number) |> # leave all other as they were
    tidyverse::bind_rows(len_comp_new) |> # new length comps for WCGBTS fleet
    arrange(fleet)
  
  # update age comps in the model
  ss3_inputs$dat$agecomp <- ss3_inputs$dat$agecomp |> 
    dplyr::filter(abs(fleet) != fleet_number) # leave all other as they were |>
  tidyverse::bind_rows(caal) |> # new marginal age comps for WCGBTS fleet
    tidyverse::bind_rows(maal) |> # new conditional-age-at-length comps for WCGBTS fleet
    arrange(fleet)
  
  #### Add Index Data #### -----------------------------------------------------------------------
  sdm_model_i <- sdm_model |>
    filter(model_iter == bio_filtered[[i]]$source) |>
    filter(Year <= ss3_inputs$dat$endyr) |>
    mutate(month = 7, index = fleet_number) |>
    # QUESTION:
    # Do we need log_est or se?
    # Is est in kg, T, or MT?
    select(Year, month, index, est, se) |>
    rename(year = Year, obs = est, log_se = se)
  
  ss3_inputs$dat$CPUE <-
    rbind(
      ss3_inputs$dat$CPUE |> dplyr::filter(index != fleet_number), # leave all other as they were
      sdm_model_i # new index for WCGBTS fleet
    )
  
  #### Write and Run SS3 #### --------------------------------------------------------------------
  # write the modified SS3 files
  r4ss::SS_write(
    ss3_inputs,
    dir = file.path(dir, "../resampled", catch_filtered[[i]]$source),
    overwrite = TRUE
  )
  
  # run SS3
  r4ss::run(file.path(dir, "../resampled", catch_filtered[[i]]$source))
}