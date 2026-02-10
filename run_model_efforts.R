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
#'                     fleet_number = fleet_number
#'                     )
#'

run_model_efforts <- function(catch_filtered,
                              bio_filtered,
                              resampled_model_dir,
                              original_model_dir,
                              sdm_model_filt,
                              model_name,
                              strata,
                              fleet_number
                              )
  {
    # read in SS3 inputs
    dirs <- list.dirs(resampled_model_dir, recursive = FALSE)
    model_iter <- unique(catch_filtered$source)
    
    message("Starting ", model_name, model_iter)
    
    new_dir <- file.path(resampled_model_dir, paste0(model_name, "_", model_iter))
    
    r4ss::copy_SS_inputs(
      dir.old = file.path(original_model_dir),
      dir.new = new_dir,
      create.dir = TRUE,
      overwrite = TRUE,
      use_ss_new = FALSE,
      verbose = FALSE
      )
    
    ss3_inputs <- r4ss::SS_read(new_dir, verbose = FALSE)
    
    message("Insert resampled biology for: ", model_name, model_iter)
    
    # determine the number of sexes to use when pulling data
    if(ss3_inputs$dat$Nsexes == 2){
      n_sexes <- TRUE
    } else {
      n_sexes <- FALSE
    }
    
    # calculate length compositions from resampled survey data
    if(length(row.names(ss3_inputs$dat$lencomp |> filter(abs(fleet) == fleet_number))) > 1){
      len_comp <- nwfscSurvey::get_expanded_comps(
        bio_data = bio_filtered,
        catch_data = catch_filtered,
        comp_bins = ss3_inputs$dat$lbin_vector,
        comp_column_name = "Length_cm",
        strata = strata,
        fleet = fleet_number,
        month = 7,
        two_sex_comps = n_sexes
      )
      
      # Create new len_comp data frame to add length comps to
      len_comp_new <- data.frame()
      
      # If it is a two sex model, do the following
      if(ss3_inputs$dat$Nsexes == 2){
        # Get sex types so that we know what all needs to be included
        sex_type <- ss3_inputs$dat$lencomp |> 
          filter(abs(fleet) == fleet_number) |>
          pull(sex)
        
        # Get the fleet for combined sex (sex = 3), this is important because sometimes fleets are 
        # included but are negative
        sexed_combined_fleet <- ss3_inputs$dat$lencomp |> 
          filter(sex == 3) |>
          filter(abs(fleet) == fleet_number) |> 
          pull(fleet) |>
          unique()
        
        # if unsexed length comps exist, include them this time as well
        if (0 %in% sex_type) {
          # Get unsexed (sex = 0) fleet number
          unsexed_fleet <- ss3_inputs$dat$lencomp |> 
            filter(abs(fleet) == fleet_number) |>
            filter(sex == 0) |> 
            pull(fleet) |>
            unique()
          names(len_comp$unsexed) <- names(len_comp$sexed)
          len_comp_new <- rbind(
            len_comp_new,
            len_comp$unsexed |> mutate(fleet = unsexed_fleet),
            len_comp$sexed |> mutate(fleet = sexed_combined_fleet)
          )
          # If just male and female exists, also include them
           if (all(c(1, 2) %in% sex_type)) {
            # Get the fleet for each separate sex, this is important because sometimes fleets are 
            # included but are negative
            female_fleet <- ss3_inputs$dat$lencomp |> 
              filter(abs(fleet) == fleet_number) |>
              filter(sex == 1) |> 
              pull(fleet) |>
              unique()
            male_fleet <- ss3_inputs$dat$lencomp |> 
              filter(abs(fleet) == fleet_number) |>
              filter(sex == 2) |> 
              pull(fleet) |>
              unique()
            
            f <- len_comp$sexed |>
              mutate(across(matches("^m(\\d+)$"), ~0)) |>
              mutate(fleet = female_fleet)
            m <- len_comp$sexed |>
              mutate(across(matches("^f(\\d+)$"), ~0)) |>
              mutate(fleet = male_fleet)
            
            len_comp_new <- rbind(len_comp_new, f, m)
            }
          } else {
            len_comp_new <- rbind(len_comp_new, len_comp$sexed |> mutate(fleet = sexed_combined_fleet))
          }
      }
       else {
        len_comp_new <- rbind(len_comp_new, len_comp$unsexed)
      }
      
      yrs_include <- ss3_inputs$dat$lencomp |> 
        dplyr::filter(abs(fleet) == fleet_number)
      
      len_comp_new <- len_comp_new |>
        dplyr::rename(part = "partition", Nsamp = "input_n") |>
        filter(year %in% yrs_include$year)
      colnames(len_comp_new) <- colnames(ss3_inputs$dat$lencomp)
      
      # Add length comp back into data file
      ss3_inputs$dat$lencomp <- ss3_inputs$dat$lencomp |> 
        dplyr::filter(fleet != fleet_number) |> # leave all other as they were
        dplyr::bind_rows(len_comp_new) |> # new length comps for WCGBTS fleet
        arrange(abs(fleet))
    }
      
    # ages
    if(!is.null(ss3_inputs$dat$agecomp)){
      if(length(row.names(ss3_inputs$dat$agecomp |> filter(abs(fleet) == fleet_number))) > 1){
        # marginal age at length
        if (length(row.names(ss3_inputs$dat$agecomp |> filter(abs(fleet) == fleet_number, Lbin_hi == -1))) > 1){
          maal <- nwfscSurvey::get_expanded_comps(
            bio_data = bio_filtered,
            catch_data = catch_filtered,
            comp_bins = ss3_inputs$dat$agebin_vector,
            comp_column_name =  "age",
            strata = strata,
            fleet = fleet_number,
            month = 7,
            two_sex_comps = n_sexes
          )
          maal_new <- data.frame()
          
          if(ss3_inputs$dat$Nsexes == 2){
            sex_type <- ss3_inputs$dat$agecomp |> 
              filter(Lbin_hi == -1) |>
              filter(abs(fleet) == fleet_number) |>
              pull(sex)
            
            sexed_combined_fleet <- ss3_inputs$dat$agecomp |> 
              filter(Lbin_hi == -1) |>
              filter(abs(fleet) == fleet_number) |>
              filter(sex == 3) |> 
              pull(fleet) |>
              unique()
            
            if (0 %in% sex_type) {
              unsexed_fleet <- ss3_inputs$dat$agecomp |> 
                filter(Lbin_hi == -1) |>
                filter(abs(fleet) == fleet_number) |>
                filter(sex == 0) |> 
                pull(fleet) |>
                unique()
              names(maal$unsexed) <- names(maal$sexed)
              maal_new <- rbind(
                maal_new,
                maal$unsexed |> mutate(fleet = unsexed_fleet),
                maal$sexed |> mutate(fleet = sexed_combined_fleet)
              )
              
              # See if there are any maal for individual sexes
              any_ind_sexes <- ss3_inputs$dat$agecomp |> 
                filter(Lbin_hi == -1) |>
                filter(abs(fleet) == fleet_number) |>
                filter(sex %in% c(1,2)) |> 
                pull(fleet) |>
                unique()
              
               if (length(any_ind_sexes) > 0) {
                female_fleet <- ss3_inputs$dat$agecomp |> 
                  filter(Lbin_hi == -1) |>
                  filter(abs(fleet) == fleet_number) |>
                  filter(sex == 1) |> 
                  pull(fleet) |>
                  unique()
                male_fleet <- ss3_inputs$dat$agecomp |> 
                  filter(Lbin_hi == -1) |>
                  filter(abs(fleet) == fleet_number) |>
                  filter(sex == 2) |> 
                  pull(fleet) |>
                  unique()
                  
                f <- maal$sexed |>
                  mutate(across(matches("^m(\\d+)$"), ~0)) |>
                  mutate(fleet = female_fleet)
                m <- maal$sexed |>
                  mutate(across(matches("^f(\\d+)$"), ~0)) |>
                  mutate(fleet = male_fleet)
                
                maal_new <- rbind(maal_new, f, m)
              } 
          } else {
            maal_new <- rbind(maal_new, maal$sexed |> mutate(fleet = sexed_combined_fleet))
            }
          } else {
            maal_new <- rbind(maal_new, maal$unsexed)
          }
          
          maal <- maal_new |>
            dplyr::rename(part = "partition", Nsamp = "input_n")
          
          yrs_include <- ss3_inputs$dat$agecomp |> 
            dplyr::filter(abs(fleet) == fleet_number,
                          Lbin_hi == -1)
          
          maal <- maal |>
            dplyr::filter(year %in% yrs_include$year)
          
          for (y in unique(maal$year)) {
            ageerr_y <- ss3_inputs$dat$agecomp |>
              dplyr::filter(year == y & abs(fleet) == fleet_number) |>
              dplyr::filter(Lbin_hi == -1) |>
              dplyr::select(ageerr, fleet) |>
              unique()
            
            idx <- which(maal$year == y)
            maal$ageerr[idx] <- rep(ageerr_y$ageerr, length(idx))
            maal$fleet[idx] <- rep(ageerr_y$fleet, length(idx))
          }
          
          colnames(maal) <- colnames(ss3_inputs$dat$agecomp)
        }
        
        # conditional-age-at-length comps 
        if (length(row.names(ss3_inputs$dat$agecomp |> filter(abs(fleet) == fleet_number, Lbin_hi != -1))) > 1) {
          caal_fleet <- ss3_inputs$dat$agecomp |>
            filter(Lbin_hi != -1) |>
            filter(abs(fleet) == fleet_number) |>
            pull(fleet) |>
            unique()
          
          caal <- nwfscSurvey::get_raw_caal(
            data = bio_filtered,
            len_bins = ss3_inputs$dat$lbin_vector,
            age_bins = ss3_inputs$dat$agebin_vector,
            fleet = caal_fleet,
            month = 7
          )
          caal <- caal |>
            dplyr::rename(part = "partition", Nsamp = "input_n")
          
          yrs_include <- ss3_inputs$dat$agecomp |> 
            dplyr::filter(abs(fleet) == fleet_number,
                          Lbin_hi != -1)
          
          caal <- caal |>
            dplyr::filter(year %in% yrs_include$year)
          
          for (y in unique(caal$year)) {
            ageerr_y <- ss3_inputs$dat$agecomp |>
              dplyr::filter(year == y & abs(fleet) == fleet_number) |>
              dplyr::filter(Lbin_hi != -1) |>
              dplyr::select(ageerr, fleet) |>
              unique()
    
            idx <- which(caal$year == y)
            caal$ageerr[idx] <- rep(ageerr_y$ageerr, length(idx))
            caal$fleet[idx] <- rep(ageerr_y$fleet, length(idx))
          }
          
          if(abs(ss3_inputs$dat$Nsexes) == 1){
            caal <- caal |>
              dplyr::arrange(year, Lbin_lo) |>
              tidyr::pivot_longer(cols = 10:length(colnames(caal)), names_to = "age", values_to = "count") |>
              dplyr::mutate(Nsamp = case_when(
                grepl("m", age) ~ 0,
                grepl("f", age) ~ Nsamp
              )) |>
              mutate(age = as.numeric(stringr::str_remove_all(age, "f|m"))) |>
              dplyr::group_by(year, Lbin_lo, age) |>
              dplyr::summarize(month = unique(month),
                               fleet = unique(fleet),
                               sex = 0,
                               partition = unique(part),
                               ageerr = unique(ageerr),
                               Lbin_hi = unique(Lbin_hi),
                               Nsamp = sum(Nsamp),
                               count = sum(count)) |>
              dplyr::select(year, month, fleet, sex, partition, ageerr, Lbin_lo, Lbin_hi, Nsamp, age, count) |>
              tidyr::pivot_wider(names_from = "age", values_from = "count")
          }
          colnames(caal) <- colnames(ss3_inputs$dat$agecomp)
        }
        
        ages <- data.frame()
        if (exists("caal")) { ages <- dplyr::bind_rows(ages, caal) }
        if (exists("maal")) { ages <- dplyr::bind_rows(ages, maal) }
        ages <- ages |>
          dplyr::mutate(ageerr = as.integer(ageerr))
        
        # update age comps in the model
        ss3_inputs$dat$agecomp <- ss3_inputs$dat$agecomp |> 
          dplyr::filter(abs(fleet) != fleet_number) |>
          bind_rows(ages) |> 
          arrange(Lbin_hi, abs(fleet))
      }
    }
    
    # Weight Length Relationship
    # re-estimate W-L parameters
    wl <- estimate_weight_length_fixed(bio_filtered)

    # parse "effort_replicate" like "0.2_3"
    effort_val <- suppressWarnings(as.numeric(sub("_.*$", "", model_iter)))
    iter_val   <- suppressWarnings(as.integer(sub("^.*_", "", model_iter)))
    
    bio_filt_wl <- bio_filtered |>
      dplyr::filter(
        !is.na(Length_cm),
        Length_cm > 0,
        !is.na(Weight_kg),
        Weight_kg > 0
      )
    
    # not actually using the w-l recalculated in the models (thus far) because 
    # some models use combo of those plus triennial and trying to recalc that is 
    # not working for me as pull_bio isn't working for me.
    wl_out <- wl |>
      dplyr::mutate(
        species = species,
        effort = effort_val,
        iteration = iter_val,
        model_iter = model_iter,     # optional but often handy to keep
        model_name = model_name,  # optional
        two_sexes = n_sexes,
        l_max = max(bio_filt_wl$Length_cm)
      ) |>
      dplyr::select(species, effort, iteration, model_iter, model_name, dplyr::everything())
    

    #### Add Index Data #### -----------------------------------------------------------------------
    message("Insert resampled index for: ", model_name, model_iter)
    sdm_model_i <- sdm_model_filt |>
      dplyr::filter(model_iter == unique(bio_filtered$source)) |>
      dplyr::filter(Year <= ss3_inputs$dat$endyr) |>
      dplyr::mutate(month = 7, index = fleet_number) |>
      dplyr::select(Year, month, index, est, se) |>
      dplyr::rename(year = Year, obs = est, se_log = se)
    
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
      overwrite = TRUE,
      verbose = FALSE
    )
    
    # download exe if it isn't in the file path
    # if(file.exists(file.path(new_dir, "ss3")) == FALSE) {
      get_ss3_exe(version = "v3.30.23.2", new_dir)
    # }
    
    # run SS3 
    message("Running model for ", model_name, model_iter)
    r4ss::run(new_dir, skipfinished = FALSE, extras = "-nohess")
    
    replist <- r4ss::SS_output(new_dir)
    
    message("Tuning comps for ", model_name, model_iter)
    
    r4ss::tune_comps(
      replist,
      niters_tuning = 2, 
      option = "Francis",
      dir = new_dir,
      exe = "ss3"
    )
    
    # return the weight-length relationship dataframe so that I can plot it later
    return(wl_out)
    
    message("Finished running for ", model_name, model_iter)
}