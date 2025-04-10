# Run resampled model

catch <- read.csv(here::here("data/nwfsc_bt_fmp_spp_updated.csv")) |>
  filter(species %in% c(
    "longnose skate",
    "petrale sole",
    "sablefish",
    "shortspine thornyhead",
    "Pacific ocean perch"
  ))
bio <- nwfscSurvey::pull_bio(
  survey = "NWFSC.Combo",
  common_name = c(
    "longnose skate",
    "petrale sole",
    "sablefish",
    "shortspine thornyhead",
    "Pacific ocean perch",
  )
)
save(bio, file = here::here("data", "nwfsc_bt_fmp_spp_updated_bio.rda"))

df_test <- data.frame(
  species_name = "petrale sole",
  original_model_dir = here::here("original_models", "petrale_sole"),
  resampled_model_dir = here::here("resampled_models"),
  sdm_dirs = here::here("Results", "Petrale_sole"),
  lat_filter = NULL,
  depth_filter = "depth_filter_675",
  strata_type = "mid",
  species_group = "flatfish",
  fleet_number = 7
)

run_model(species_name = "petrale sole",
          original_model_dir = here::here("original_models", "petrale_sole"),
          sdm_dirs = here::here("Results", "Petrale_sole"),
          lat_filter = NULL,
          depth_filter = "depth_filter_675",
          strata_type = "mid",
          species_group = "flatfish",
          fleet_number = 7,
          resampled_model_dir = here::here("resampled_models"),
          exe_location = here::here("ss3.exe"),
          catch_df = catch,
          bio_df = bio)

# og_model_dirs <- list.dirs(here::here("original_models"), full.names = TRUE, recursive = FALSE)
# sdm_dirs <- list.dirs(here::here("Results"), full.names = TRUE, recursive = FALSE)
# sdm_dirs <- grep(paste(basename(og_model_dirs), collapse = "|"), sdm_dirs, value = TRUE)
#   
# df <- data.frame(
#   species_name = c("longnose skate", "Pacific ocean perch", "petrale sole", 
#                    "sablefish", "shortspine thornyhead"),
#   original_model_dir = og_model_dirs,
#   sdm_dirs = sdm_dirs,
#   lat_filter = c(NULL, "lat_filter_35", NULL, NULL, NULL),
#   depth_filter = c(NULL, "depth_filter_500", "depth_filter_675", NULL, NULL),
#   strata_type = c("deep", "mid", "mid", "deep", "deep"),
#   species_group = c("all", "all", "flatfish", "all", "thorny"),
#   fleet_number = c(5, 8, 7, 7, 6)
# )
# 
# map(df, ~ run_model(species_name = .x$species_name,
#                     original_model_dir = .x$original_model_dir,
#                     sdm_dirs = .x$sdm_dirs,
#                     lat_filter = .x$lat_filter,
#                     depth_filter = .x$depth_filter,
#                     strata_type = .x$strata_type,
#                     species_group = .x$species_group,
#                     fleet_number = .x$fleet_number,
#                     resampled_model_dir = here::here("resampled_models"),
#                     exe_location = here::here("ss3.exe"),
#                     catch_df = catch,
#                     bio_df = bio))

# TO_DO: run effort level and replicate models in parallel

#' Run the model for a given species
#'
#' This function reads in SS3 inputs, filters catch and biological data for the specified species,
#' calculates length compositions from re-sampled survey data, and updates the SS3 model with new length
#' and age compositions. It then writes the modified SS3 files and runs the SS3 model.
#'
#' @param species_name A string specifying the common name of the species.
#' @param original_model_dir A string specifying the directory where the SS3 inputs are located.
#' @param resampled_model_dir A string specifying the directory where the SS3 inputs are located.
#' @param sdm_dirs A string specifying the directory where the indices are located.
#' @param lat_filter NULL
#' @param depth_filter
#' @param catch_df A data frame containing catch data. Default is `catch`.
#' @param bio_df A data frame containing biological data. Default is `bio`.
#' @param strata_type A string specifying the type of strata to use. Options are
#' "mid" or others. Default is "mid".
#' @param species_group A species group for input data.
#' @param fleet_number An integer specifying the fleet number for the WCGBTS.
#' @param exe_location File path to where the ss3 executable is located.
#' Default is 7.
#'
#' @return This function does not return a value. It writes modified SS3 files
#' and runs the SS3 model.
#'
#' @examples
#' og_dir <- here::here("original_models"),
#' dir.create(here::here("resampled_models"))
#' resamp_dir <- here::here("resampled_models")
#'
#' sdm_dirs <- list.dirs(here::here("Results"), recursive = FALSE)
#'
#' df <- data.frame(
#'       species_name = c("petrale sole", "arrowtooth flounder"),
#'       original_model_dir = og_dir,
#'       resampled_model_dir = resamp_dir,
#'       sdm_dir = sdm_dirs,
#'       lat_filter = c("lat_filter_34", "lat_filter_35"),
#'       depth_filter = c("depth_filter_275", "depth_filter_425"),
#'       strata_type = c("mid", "mid")
#'       species_group = c("flatfish", "flatfish"),
#'       fleet_number = c(7, 7))
#' df_list <- split(df, seq(nrow(df)))
#' map(df_list, ~ run_model(species_name = .x$species_name,
#'                                 strata_type = .x$strata_type,
#'                                 species_group = .x$species_group,
#'                                 fleet_number = .x$fleet_number))
#'
run_model <- function(
  species_name,
  original_model_dir,
  resampled_model_dir,
  sdm_dir,
  catch_df = catch,
  bio_df = bio,
  lat_filter,
  depth_filter,
  strata_type = "mid",
  species_group,
  fleet_number = 7,
  exe_location
) {
  model_name <- basename(original_model_dir)
  
  ss3_inputs_old <- r4ss::SS_read(original_model_dir)
  
  #### Get sdm data frame #### -------------------------------------------------------------------
  sdm_model <- read.csv(list.files(
    sdm_dirs,
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
  
  sdm_model <- sdm_model |>
    filter(model_iter %in% sdm_model_reps$model_iter)
  
  rm(sdm_model_reps)

  #### Get Bio data #### --------------------------------------------------------------------------
  catch_filtered <- cleanup_by_species(catch_df, species = species_name)
  catch_filtered[names(catch_filtered) %in% sdm_model_filt$model_iter]
  catch_filtered <- lapply(catch_filtered, function(df) {
    df <- df[df$Year <= ss3_inputs_old$dat$endyr, ]
    return(df)
  })

  bio_filtered <- cleanup_by_species(bio_df, species = species_name)
  bio_filtered[names(bio_filtered) %in% sdm_model_filt$model_iter]
  bio_filtered <- lapply(bio_filtered, function(df) {
    df <- df[df$Year <= ss3_inputs_old$dat$endyr, ]
    return(df)
    })

  # apply lat and depth filters
  if (lat_filter == "lat_filter_35") {
    catch_filtered <- lapply(catch_filtered, lat_filter_35)
    bio_filtered <- lapply(bio_filtered, lat_filter_35)
  } else {
    catch_filtered <- catch_filtered
    bio_filtered <- bio_filtered
  }

  if (depth_filter == "depth_filter_500") {
    catch_filtered <- lapply(catch_filtered, depth_filter_500)
    bio_filtered <- lapply(bio_filtered, depth_filter_500)
  } else if (depth_filter == "depth_filter_675") {
    catch_filtered <- lapply(catch_filtered, depth_filter_675)
    bio_filtered <- lapply(bio_filtered, depth_filter_675)
  } else {
    catch_filtered <- catch_filtered
    bio_filtered <- bio_filtered
  }
  
  
  # choose correct strata
  if (strata_type == "mid") {
    strata <- nwfscSurvey::CreateStrataDF.fn(
      names = c("shallow_s", "mid_s", "shallow_n", "mid_n"),
      depths.shallow = c(55, 183, 55, 183),
      depths.deep = c(183, 549, 183, 549),
      lats.south = c(32, 32, 42, 42),
      lats.north = c(42, 42, 49, 49)
    )
  } else {
    strata <- nwfscSurvey::CreateStrataDF.fn(
      names = c("shallow_s", "mid_s", "shallow_n", "mid_n"),
      depths.shallow = c(55, 183, 549, 55, 183, 549),
      depths.deep = c(183, 549, 1280, 183, 549, 1280),
      lats.south = c(32, 32, 32, 42, 42, 42),
      lats.north = c(42, 42, 42, 49, 49, 49)
    )
  }
    
  for (i in 1:length(catch_filtered)) {
    # read in SS3 inputs
    # if replicate/effort folder doesn't exist
    dirs <- list.dirs(resampled_model_dir, recursive = FALSE)
    new_dir <- file.path(resampled_model_dir, paste0(model_name, "_", catch_filtered[[i]]$source))
  
    if (any(grepl(dirs, paste0(model_name, "_", catch_filtered[[i]]$source)) == FALSE)) {
      copy_SS_inputs(
        dir.old = file.path(original_model_dir, model_name),
        dir.new = new_dir,
        create.dir = TRUE,
        overwrite = TRUE,
        use_ss_new = FALSE,
        verbose = TRUE
      )
    }
    
    ss3_inputs <- r4ss::SS_read(new_dir)
    
    # calculate length compositions from resampled survey data
    len_comp_new <- nwfscSurvey::get_expanded_comps(
      bio_data = bio_filtered[[i]],
      catch_data = catch_filtered[[i]],
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
      maal <- maal |>
        dplyr::rename(part = "partition", Nsamp = "input_n")
      
      maal$Nsamp <- input_n |>
        dplyr::filter(sex_grouped == "sexed") |>
        dplyr::pull(input_n)
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
      caal <- caal |>
        dplyr::rename(part = "partition", Nsamp = "input_n")
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
        dir = new_dir,
        overwrite = TRUE
      )
      
      # run SS3
      r4ss::run(new_dir, exe = exe_location)
  }
}