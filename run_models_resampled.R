# Run resampled model

catch <- read.csv(here::here("data/nwfsc_bt_fmp_spp_updated.csv"))
bio <- nwfscSurvey::pull_bio(
  survey = "NWFSC.Combo",
  common_name = c(
    "Pacific spiny dogfish",
    "longnose skate",
    "arrowtooth flounder",
    "petrale sole",
    "rex sole",
    "Dover sole",
    "sablefish",
    "shortspine thornyhead",
    "Pacific ocean perch",
    "darkblotched rockfish",
    "widow rockfish",
    "yellowtail rockfish",
    "bocaccio",
    "canary rockfish",
    "lingcod"
  )
)
save(bio, file = here::here("data", "nwfsc_bt_fmp_spp_updated_bio.rda"))


### only do 4 sampling effort (20, 40, 50, 100) with 3 random replicates out of the 10
### and only do this for 3-5 species

# TO_DO: randomly draw 3 replicates out of 10
# TO_DO: select 4 sampling efforts (20, 40, 50, 100)
# TO_DO: select all these first then have it all work in parallel

#' Run the model for a given species
#'
#' This function reads in SS3 inputs, filters catch and biological data for the specified species,
#' calculates length compositions from re-sampled survey data, and updates the SS3 model with new length
#' and age compositions. It then writes the modified SS3 files and runs the SS3 model.
#'
#' @param species_name A string specifying the common name of the species.
#' @param dir A string specifying the directory where the SS3 inputs are located.
#' @param sdm_dirs A string specifying the directory where the indices are located.
#' @param model_name A string specifying the name of the model in the Models folder.
#' @param lat_filter
#' @param depth_filter
#' @param catch_df A data frame containing catch data. Default is `catch`.
#' @param bio_df A data frame containing biological data. Default is `bio`.
#' @param strata_type A string specifying the type of strata to use. Options are
#' "mid" or others. Default is "mid".
#' @param species_group A species group for input data.
#' @param wcgbts_fleet_number An integer specifying the fleet number for the WCGBTS.
#' Default is 7.
#'
#' @return This function does not return a value. It writes modified SS3 files
#' and runs the SS3 model.
#'
#' @examples
#' dirs <- list.dirs(here::here("models"), recursive = FALSE)
#' dirs <- lapply(dirs, function(x) file.path(x, "original")
#'
#' sdm_dirs <- list.dirs(here::here("Results"), recursive = FALSE)
#'
#' df <- data.frame(
#'       species_name = c("petrale sole", "arrowtooth flounder"),
#'       dir = dirs,
#'       sdm_dir = sdm_dirs,
#'       lat_filter = c("lat_filter_34", "lat_filter_35"),
#'       depth_filter = c("depth_filter_275", "depth_filter_425"),
#'       strata_type = c("mid", "mid")
#'       species_group = c("flatfish", "flatfish"),
#'       fleet_number = c(7, 7))
#' df_list <- split(df, seq(nrow(df)))
#' map(df_list, ~ run_model(species_name = .x$species_name,
#'                                 dir = .x$dir,
#'                                 strata_type = .x$strata_type,
#'                                 species_group = .x$species_group,
#'                                 fleet_number = .x$fleet_number))
#'
run_model <- function(
  species_name,
  dir,
  model_name,
  sdm_dir,
  catch_df = catch,
  lat_filter,
  depth_filter,
  bio_df = bio,
  strata_type = "mid",
  species_group,
  fleet_number = 7
) {
  
  # if resampled folder doesn't exist, create it
  dirs <- list.dirs(dirname(dir), recursive = FALSE)
  if (any(grepl(dirs, resampled) == FALSE)) {
    dir.create(file.path(dir, "resampled"))
  }
  
  #### Get sdm data frame #### -------------------------------------------------------------------
  list.files(sdm_dirs)
  
  sdm_model <- read.csv(list.files(
    sdm_dirs,
    pattern = "*._indices_df",
    full.names = TRUE
  ))
  
  sdm_model$model_iter <- paste0(sdm_model$effort,"_", sdm_model$replicate)

  #### Get Bio data #### --------------------------------------------------------------------------
  catch_filtered <- cleanup_by_species(catch_df, species = species_name) |>
    filter(Year <= ss3_inputs$dat$endyr)

  bio_filtered <- cleanup_by_species(bio_df, species = species_name) |>
    filter(Year <= ss3_inputs$dat$endyr)

  # apply lat and depth filters
  if (lat_filter == "lat_filter_335") {
    catch_filtered <- lapply(catch_filtered, lat_filter_335)
    bio_filtered <- lapply(bio_filtered, lat_filter_335)
  } else if (lat_filter == "lat_filter_34") {
    catch_filtered <- lapply(catch_filtered, lat_filter_34)
    bio_filtered <- lapply(bio_filtered, lat_filter_34)
  } else if (lat_filter == "lat_filter_34_max") {
    catch_filtered <- lapply(catch_filtered, lat_filter_34_max)
    bio_filtered <- lapply(bio_filtered, lat_filter_34_max)
  } else if (lat_filter == "lat_filter_35") {
    catch_filtered <- lapply(catch_filtered, lat_filter_35)
    bio_filtered <- lapply(bio_filtered, lat_filter_35)
  } else {
    catch_filtered <- catch_filtered
    bio_filtered <- bio_filtered
  }

  if (depth_filter == "depth_filter_275") {
    catch_filtered <- lapply(catch_filtered, depth_filter_275)
    bio_filtered <- lapply(bio_filtered, depth_filter_275)
  } else if (depth_filter == "depth_filter_425") {
    catch_filtered <- lapply(catch_filtered, depth_filter_425)
    bio_filtered <- lapply(bio_filtered, depth_filter_425)
  } else if (depth_filter == "depth_filter_450") {
    catch_filtered <- lapply(catch_filtered, depth_filter_450)
    bio_filtered <- lapply(bio_filtered, depth_filter_450)
  } else if (depth_filter == "depth_filter_500") {
    catch_filtered <- lapply(catch_filtered, depth_filter_500)
    bio_filtered <- lapply(bio_filtered, depth_filter_500)
  } else if (depth_filter == "depth_filter_675") {
    catch_filtered <- lapply(catch_filtered, depth_filter_675)
    bio_filtered <- lapply(bio_filtered, depth_filter_675)
  } else if (depth_filter == "depth_filter_700") {
    catch_filtered <- lapply(catch_filtered, depth_filter_700)
    bio_filtered <- lapply(bio_filtered, depth_filter_700)
  } else {
    catch_filtered <- catch_filtered
    bio_filtered <- bio_filtered
  }
  
  # make the names file
  model_iter <- as.list(names(catch_filtered))
  
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
    dirs <- list.dirs(file.path(dir, "resampled"), recursive = FALSE)
      
    if (any(grepl(dirs, catch_filtered[[i]]$source) == FALSE)) {
      copy_SS_inputs(
        dir.old = file.path(dir, "Models", model_name),
        dir.new = file.path(dir, "resampled", catch_filtered[[i]]$source),
        create.dir = TRUE,
        overwrite = TRUE,
        use_ss_new = FALSE,
        verbose = TRUE
      )
    }
    
    ss3_inputs <- r4ss::SS_read(dir[[i]])
    
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

    ###TO_DO: need to add maal
    
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
      ss3_inputs$dat$lencomp <-
        rbind(
          ss3_inputs$dat$lencomp |> dplyr::filter(fleet != fleet_number), # leave all other as they were
          len_comp_new # new length comps for WCGBTS fleet
        )
      # update age comps in the model
      ss3_inputs$dat$agecomp <-
        rbind(
          ss3_inputs$dat$agecomp |> dplyr::filter(abs(fleet) != fleet_number), # leave all other as they were
          caal # new conditional-age-at-length comps for WCGBTS fleet
        )
 
      #### Add Index Data #### -----------------------------------------------------------------------
      # TO DO:
      # Insert function at some point to go through effort and replicate of indices
      
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
  for (i in 1:length(catch_filtered)) {
    # read in SS3 inputs
    # if replicate/effort folder doesn't exist
    dirs <- list.dirs(file.path(dir, "resampled"), recursive = FALSE)
    if (any(grepl(dirs, catch_filtered[[i]]$source) == FALSE)) {
      copy_SS_inputs(
        dir.old = file.path(dir, "Models", model_name),
        dir.new = file.path(dir, "resampled", catch_filtered[[i]]$source),
        create.dir = TRUE,
        overwrite = TRUE,
        use_ss_new = FALSE,
        verbose = TRUE
      )
    }
    
    ss3_inputs <- r4ss::SS_read(dir[[i]])
    
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

    ###TO_DO: need to add maal
    
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
      ss3_inputs$dat$lencomp <-
        rbind(
          ss3_inputs$dat$lencomp |> dplyr::filter(fleet != fleet_number), # leave all other as they were
          len_comp_new # new length comps for WCGBTS fleet
        )
      # update age comps in the model
      ss3_inputs$dat$agecomp <-
        rbind(
          ss3_inputs$dat$agecomp |> dplyr::filter(abs(fleet) != fleet_number), # leave all other as they were
          caal # new conditional-age-at-length comps for WCGBTS fleet
        )
 
      #### Add Index Data #### -----------------------------------------------------------------------
      # TO DO:
      # Insert function at some point to go through effort and replicate of indices
      
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
}
