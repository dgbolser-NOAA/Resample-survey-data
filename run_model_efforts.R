#' Run the models of a given species for each of the effort levels and replicates
#'
#' This function reads in SS3 inputs, filters catch and biological data for the specified species,
#' and age compositions. It then writes the modified SS3 files and runs the SS3 model.
#' calculates length compositions from re-sampled survey data, and updates the SS3 model with new length
#' @param catch_filtered A data frame containing catch data. Default is `catch`.
#' @param bio_filtered A data frame containing biological data. Default is `bio`.
#' @param original_model_dir A string specifying the directory where the SS3 inputs are located.
#' @param resampled_model_dir A string specifying the directory where the SS3 inputs are located.
#' @param sdm_model_filt data frame of the the sdms for only the sampling efforts and replicates wanted
#' @param model_name A string specifying the name of the model in the Models folder.
#' @param strata A string specifying the type of strata to use. Options are
#' "mid" or others. Default is "mid".
#' @param fleet_number An integer specifying the fleet number for the WCGBTS.
#' @param species_group A species group for input data.
#'
#' @return This function does not return a value. It writes modified SS3 files
#' and runs the SS3 model.
#'
#' @examples
#' This function is meant to be run exclusively within run_models_resampled()
#' plan(multisession,  workers = 11)
#' furrr::future_map2(.x = catch_filtered, 
#'                    .y = bio_filtered,
#'                    .f = run_model_efforts,
#'                     resampled_model_dir,
#'                     original_model_dir,
#'                     model_name = model_name,
#'                     sdm_model_filt = sdm_model_filt,
#'                     strata = strata,
#'                     fleet_number = fleet_number,
#'                     species_group = species_group
#'                     )
#'

run_model_efforts <- function(catch_filtered,
                               bio_filtered,
                               resampled_model_dir,
                               original_model_dir,
                               sdm_model_filt,
                               model_name,
                               strata,
                               fleet_number,
                               species_group
                               )
  {
    # read in SS3 inputs
    # if replicate/effort folder doesn't exist
    dirs <- list.dirs(resampled_model_dir, recursive = FALSE)
    model_iter <- unique(catch_filtered$source)
    
    new_dir <- file.path(resampled_model_dir, paste0(model_name, "_", model_iter))
    
    if (length(dirs) != 0) {
      full_model_name <- paste0(model_name, "_", model_iter)
      matches <- grepl(full_model_name, dirs)
      if (any(matches) == FALSE){
        r4ss::copy_SS_inputs(
          dir.old = file.path(original_model_dir),
          dir.new = new_dir,
          create.dir = TRUE,
          overwrite = TRUE,
          use_ss_new = FALSE,
          verbose = TRUE
        )
      }
    }
    
    if (length(dirs) == 0) {
      r4ss::copy_SS_inputs(
        dir.old = file.path(original_model_dir),
        dir.new = new_dir,
        create.dir = TRUE,
        overwrite = TRUE,
        use_ss_new = FALSE,
        verbose = TRUE
      )
    }
    
    
    ss3_inputs <- r4ss::SS_read(new_dir)
    
    # calculate length compositions from resampled survey data
    if(length(row.names(ss3_inputs$dat$agecomp |> filter(fleet == fleet_number)))){
      len_comp_new <- nwfscSurvey::get_expanded_comps(
        bio_data = bio_filtered,
        catch_data = catch_filtered,
        comp_bins = ss3_inputs$dat$lbin_vector,
        comp_column_name = "Length_cm",
        strata = strata,
        fleet = fleet_number,
        month = 7
      )
      
      len_comp_new <- len_comp_new$sexed
      len_comp_new <- len_comp_new |>
        dplyr::rename(part = "partition", Nsamp = "input_n")
      
      # Add length comp back into data file
      ss3_inputs$dat$lencomp <- ss3_inputs$dat$lencomp |> 
        dplyr::filter(fleet != fleet_number) |> # leave all other as they were
        dplyr::bind_rows(len_comp_new) |> # new length comps for WCGBTS fleet
        arrange(fleet)
      
      # marginal age at length
      if (any(ss3_inputs$dat$agecomp$Lbin_hi == -1)) {
        maal <- nwfscSurvey::get_expanded_comps(
          bio_data = bio_filtered,
          catch_data = catch_filtered,
          comp_bins = ss3_inputs$dat$agebin_vector,
          comp_column_name =  "age",
          strata = strata,
          fleet = fleet_number,
          month = 7
        )
        maal <- maal$sexed
        maal <- maal |>
          dplyr::rename(part = "partition", Nsamp = "input_n")
        
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
          data = bio_filtered,
          len_bins = ss3_inputs$dat$lbin_vector,
          age_bins = ss3_inputs$dat$agebin_vector,
          fleet = fleet_number,
          month = 7
        )
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
      
      ages <- data.frame()
      if (exists("caal")) { ages <- dplyr::bind_rows(ages, caal) }
      if (exists("maal")) { ages <- dplyr::bind_rows(ages, maal) }
      
      # update age comps in the model
      ss3_inputs$dat$agecomp <- ss3_inputs$dat$agecomp |> 
        dplyr::filter(fleet != fleet_number) |>
        bind_rows(ages) |> 
        arrange(fleet)
    }
    
    #### Add Index Data #### -----------------------------------------------------------------------
    sdm_model_i <- sdm_model_filt |>
      filter(model_iter == unique(bio_filtered$source)) |>
      filter(Year <= ss3_inputs$dat$endyr) |>
      mutate(month = 7, index = fleet_number) |>
      # QUESTION:
      # Do we need log_est or se?
      # Is est in kg, T, or MT?
      select(Year, month, index, est, se) |>
      rename(year = Year, obs = est, se_log = se)
    
    ss3_inputs$dat$CPUE <-
      rbind(
        ss3_inputs$dat$CPUE |> dplyr::filter(index != fleet_number), # leave all other as they were
        sdm_model_i # new index for WCGBTS fleet
      ) |>
      arrange(index)
    
    #### Write and Run SS3 #### --------------------------------------------------------------------
    # write the modified SS3 files
    r4ss::SS_write(
      ss3_inputs,
      dir = new_dir,
      overwrite = TRUE
    )
    
    # download exe if it isn't in the file path
    if(file.exists(file.path(new_dir, "ss3")) == FALSE) {
      get_ss3_exe(new_dir)
    }
    # run SS3 
    r4ss::run(new_dir, skipfinished = FALSE, extras = "-nohess")
    
    replist <- r4ss::SS_output(new_dir)
    
    r4ss::tune_comps(
      replist,
      niters_tuning = 2, 
      option = "Francis",
      dir = new_dir,
      exe = "ss3"
    )
}