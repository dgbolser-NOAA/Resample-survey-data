# Run resampled model

# Is there a way to tell the models to not downweight surveys that are less important? 
# I mean it's not what would actually be done in an assessment but it might be a worthwhile comparison if it could be done.
# I agree that it would be a useful comparison. We could do that by updating the index observations but leaving the uncertainty 
# unchanged, and similarly update the age and length comps but leave the Nsamp the same as before.

#### Load in required packages #### --------------------------------------------
library(here)
library(nwfscSurvey)
library(dplyr)
library(tidyverse)
library(r4ss)
library(purrr)
library(furrr)
source("cleanup_by_species.R")
source("smaller_functions.R")
source("run_model_efforts.R")


#### Load in catch and bio data from nwfscSurvey package #### ------------------
catch <- read.csv(here::here("data/nwfsc_bt_fmp_spp_updated.csv")) |>
  filter(Common_name %in% c(
    "longnose skate",
    "petrale sole",
    "sablefish",
    "shortspine thornyhead",
    "Pacific ocean perch",
    "yellowtail rockfish"
  ))

# Last pulled 6/5/2025
# bio <- nwfscSurvey::pull_bio(
#   survey = "NWFSC.Combo",
#   common_name = c(
#     "longnose skate",
#     "petrale sole",
#     "sablefish",
#     "shortspine thornyhead",
#     "Pacific ocean perch",
#     "yellowtail rockfish"
#   )
# )
# saveRDS(bio, file = here::here("data", "nwfsc_bt_fmp_spp_updated_bio.rds"))
# rm(bio)

bio <- readRDS(here::here("data", "nwfsc_bt_fmp_spp_updated_bio.rds"))

set.seed(49)

# dir.create(here::here("resampled_models"))
og_model_dir <- list.dirs(here::here("original_models"), full.names = TRUE, recursive = FALSE)
sdm_dir <- list.dirs(here::here("Results"), full.names = TRUE, recursive = FALSE)
sdm_dir <- grep(paste(basename(og_model_dir), collapse = "|"), sdm_dir, value = TRUE)
resampled_model_dir <- here::here("resampled_models")

df <- data.frame(
  species_name = c("longnose skate", "Pacific ocean perch", "petrale sole",
                   "sablefish", "shortspine thornyhead", "yellowtail rockfish"),
  scientific_name = c("Raja rhina", "Sebastes alutus", "Eopsetta jordani",
                  "Anoplopoma fimbria", "Sebastolobus alascanus", "Sebastes flavidus"),
  original_model_dir = og_model_dir,
  sdm_dir = sdm_dir,
  lat_filter = c(NA, "lat_filter_35", NA, NA, NA, "lat_filter_335"),
  depth_filter = c(NA, "depth_filter_500", "depth_filter_675", NA, NA, "depth_filter_425"),
  strata_type = c("deep", "mid", "mid", "deep", "deep", "mid"),
  fleet_number = c(5, 8, 4, 7, 6, 6)
)

df_list <- split(df, seq(nrow(df)))

purrr::map(df_list, ~ run_model(species_name = .x$species_name,
                    scientific_name = .x$scientific_name,
                    original_model_dir = .x$original_model_dir,
                    sdm_dir = .x$sdm_dir,
                    lat_filter = .x$lat_filter,
                    depth_filter = .x$depth_filter,
                    strata_type = .x$strata_type,
                    fleet_number = .x$fleet_number,
                    resampled_model_dir = resampled_model_dir,
                    catch_df = catch,
                    bio_df = bio))

# run just the ith species
# i <- 6
# i <- 2
# i <- 4 # sablefish
# run_model(species_name = df_list[[i]]$species_name,
#           scientific_name = df_list[[i]]$scientific_name,
#           original_model_dir = df_list[[i]]$original_model_dir,
#           sdm_dir = df_list[[i]]$sdm_dir,
#           lat_filter = df_list[[i]]$lat_filter,
#           depth_filter = df_list[[i]]$depth_filter,
#           strata_type = df_list[[i]]$strata_type,
#           fleet_number = df_list[[i]]$fleet_number,
#           resampled_model_dir = resampled_model_dir,
#           catch_df = catch,
#           bio_df = bio)
# 
# species_name <- df_list[[i]]$species_name
# scientific_name <- df_list[[i]]$scientific_name
# original_model_dir <- df_list[[i]]$original_model_dir
# sdm_dir <- df_list[[i]]$sdm_dir
# lat_filter <- df_list[[i]]$lat_filter
# depth_filter <- df_list[[i]]$depth_filter
# strata_type <- df_list[[i]]$strata_type
# fleet_number <- df_list[[i]]$fleet_number
# resampled_model_dir <- resampled_model_dir
# catch_df <- catch
# bio_df <- bio

#' Run the model for a given species
#'
#' This function reads in SS3 inputs, filters catch and biological data for the specified species,
#' calculates length compositions from re-sampled survey data, and updates the SS3 model with new length
#' and age compositions. It then writes the modified SS3 files and runs the SS3 model.
#'
#' @param species_name A string specifying the common name of the species.
#' @param scientific_name A string specifying the scientific name of the species.
#' @param original_model_dir A string specifying the directory where the SS3 inputs are located.
#' @param resampled_model_dir A string specifying the directory where the SS3 inputs are located.
#' @param sdm_dir A string specifying the directory where the indices are located.
#' @param lat_filter NULL
#' @param depth_filter NULL
#' @param catch_df A data frame containing catch data. Default is `catch`.
#' @param bio_df A data frame containing biological data. Default is `bio`.
#' @param strata_type A string specifying the type of strata to use. Options are
#' "mid" or others. Default is "mid".
#' @param fleet_number An integer specifying the fleet number for the WCGBTS.
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
#' sdm_dir <- list.dirs(here::here("Results"), recursive = FALSE)
#'
#' df <- data.frame(
#'       species_name = c("petrale sole", "arrowtooth flounder"),
#'       original_model_dir = og_dir,
#'       resampled_model_dir = resamp_dir,
#'       sdm_dir = sdm_dir,
#'       lat_filter = c("lat_filter_34", "lat_filter_35"),
#'       depth_filter = c("depth_filter_275", "depth_filter_425"),
#'       strata_type = c("mid", "mid")
#'       fleet_number = c(7, 7))
#' df_list <- split(df, seq(nrow(df)))
#' map(df_list, ~ run_model(species_name = .x$species_name,
#'                                 strata_type = .x$strata_type,
#'                                 species_group = .x$species_group,
#'                                 fleet_number = .x$fleet_number))
#'
run_model <- function(
  species_name,
  scientific_name,
  original_model_dir,
  resampled_model_dir,
  sdm_dir,
  catch_df = catch,
  bio_df = bio,
  lat_filter = NA,
  depth_filter = NA,
  strata_type = "mid",
  fleet_number = 4
  ) {
  model_name <- basename(original_model_dir)
  
  message("Starting ", model_name)
  
  ss3_inputs_old <- r4ss::SS_read(original_model_dir)
  
  #### Get sdm data frame #### -------------------------------------------------------------------
  sdm_model <- read.csv(list.files(
    sdm_dir,
    pattern = "*._indices_df",
    full.names = TRUE
  )) |>
    filter(effort %in% c(0.2, 0.4, 0.8, 1)) |>
    filter(effort != 0.1) |>
    mutate(model_iter = paste0(effort,"_", replicate)) |>
    filter(!is.na(se))
  
  rescale_num <- resampled_mean_index/og_mean_index
  
  # randomly sample 3 replicates from each effort
  sdm_model_reps <- sdm_model |>
    distinct(model_iter, .keep_all = TRUE) |>
    group_by(effort) |>
    slice_sample(n = 3) |>
    ungroup()
  
  resampled_mean_index <- mean(sdm_model |>
                                 filter(effort == 1) |>
                                 pull(est))
  
  og_mean_index <- mean(ss3_inputs_old$dat$CPUE |>
                          filter(index == fleet_number) |>
                          pull(obs))
  
  rescale_num <- resampled_mean_index/og_mean_index
  
  sdm_model_filt <- sdm_model |>
    filter(model_iter %in% sdm_model_reps$model_iter) |>
    mutate(est = est/rescale_num)
  
  rm(sdm_model_reps, sdm_model, resampled_mean_index, og_mean_index, rescale_num)

  #### Get Bio data #### --------------------------------------------------------------------------
  catch_filtered <- cleanup_by_species(catch_df, species = species_name)
  catch_filtered <- catch_filtered[names(catch_filtered) %in% sdm_model_filt$model_iter]
  catch_filtered <- lapply(catch_filtered, function(df) {
    df <- df[df$Year <= ss3_inputs_old$dat$endyr, ]
    return(df)
  })
  
  bio_filtered <- lapply(catch_filtered, function(catch_data) {
    replicate_id <- unique(catch_data$source) # Get replicate ID
    bio_df$Trawl_id <- as.double(bio_df$Trawl_id)
    bio_df <- bio_df |>
              filter(Year <= ss3_inputs_old$dat$endyr, 
                     Scientific_name == scientific_name) # Filter bio data for the species and years
    matched_bio <- bio_df[bio_df$Trawl_id %in% catch_data$Trawl_id, ] # Filter bio data based on tow IDs
    matched_bio <- matched_bio |>
      mutate(source = replicate_id) # Add replicate ID as a column
    return(matched_bio)
  })

  # apply lat and depth filters
  if (is.null(lat_filter) || is.na(lat_filter)) {
    catch_filtered <- catch_filtered
    bio_filtered <- bio_filtered
  } else if (lat_filter == "lat_filter_35") {
    catch_filtered <- lapply(catch_filtered, lat_filter_35)
    bio_filtered <- lapply(bio_filtered, lat_filter_35)
  } else if (lat_filter == "lat_filter_335") {
    catch_filtered <- lapply(catch_filtered, lat_filter_335)
    bio_filtered <- lapply(bio_filtered, lat_filter_335)
  } else {
    catch_filtered <- catch_filtered
    bio_filtered <- bio_filtered
  }

  if (is.null(depth_filter) || is.na(depth_filter)) {
    catch_filtered <- catch_filtered
    bio_filtered <- bio_filtered
  } else if (depth_filter == "depth_filter_500") {
    catch_filtered <- lapply(catch_filtered, depth_filter_500)
    bio_filtered <- lapply(bio_filtered, depth_filter_500)
  } else if (depth_filter == "depth_filter_675") {
    catch_filtered <- lapply(catch_filtered, depth_filter_675)
    bio_filtered <- lapply(bio_filtered, depth_filter_675)
  } else if (depth_filter == "depth_filter_425") {
    catch_filtered <- lapply(catch_filtered, depth_filter_425)
    bio_filtered <- lapply(bio_filtered, depth_filter_425)
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
      names = c("shallow_s", "mid_s", "deep_s", "shallow_n", "mid_n", "deep_n"),
      depths.shallow = c(55, 183, 549, 55, 183, 549),
      depths.deep = c(183, 549, 1280, 183, 549, 1280),
      lats.south = c(32, 32, 32, 42, 42, 42),
      lats.north = c(42, 42, 42, 49, 49, 49)
    )
  }
    
  plan(multisession, workers = 11)
  
  furrr::future_map2(.x = catch_filtered, 
                     .y = bio_filtered,
                     .f = run_model_efforts,
                     resampled_model_dir,
                     original_model_dir,
                     sdm_model_filt = sdm_model_filt,
                     model_name = model_name,
                     strata = strata,
                     fleet_number = fleet_number
  )
  # run_model_efforts(
  # catch_filtered <- catch_filtered[[1]]
  # bio_filtered <- bio_filtered[[1]]
  # resampled_model_dir <- resampled_model_dir
  # original_model_dir <- original_model_dir
  # sdm_model_filt <- sdm_model_filt
  # model_name <- model_name
  # strata <- strata
  # fleet_number <- fleet_number
  # )
  plan(sequential)
  
  message("Finished ", model_name)
}
