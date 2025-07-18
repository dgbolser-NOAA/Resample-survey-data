#' Species distribution model function
#'
#' Function to create a mesh, fit the sdmTMB model, and get the index.
#' Exports fit.rds and index.rds files to the designated species folder.
#' Used for arrowtooth flounder, bocaccio, dover sole, lingcod north, lingcod
#' south, longnose skate, pacific ocean perch (pop), pacific spiny dogfish, rex
#' sole, yellowtail.
#'
#' @param x speciesname_df[[i]] which is a data frame from a list of data frames
#' created from the cleanup_by_species() function and any further post-processing
#' of depth filters (see the smaller_function.R file for those).
#' @param y speciesname_files[[i]] which is an item in a list created from
#' names(speciesname_df)
#' @import sdmTMB
#'
species_sdm_fn <- function(x, y, z) {
  # make mesh
  mesh <- sdmTMB::make_mesh(x, xy_cols = c("longitude", "latitude"), n_knots = 500)
  
  # fit model
  fit <- sdmTMB::sdmTMB(
    catch_weight ~ 0 + fyear + pass_scaled,
    data = x,
    mesh = mesh,
    family = delta_gamma(),
    time = "Year",
    offset = log(x$effort),
    control = sdmTMB::sdmTMBcontrol(newton_loops = 3),
    anisotropy = TRUE,
    spatiotemporal = as.list(c("iid", "iid"))
  )
  
  # get the index
  predictions <- predict(fit, newdata = z, return_tmb_object = TRUE)
  index <- sdmTMB::get_index(predictions, area = z$area_km2_WCGBTS, bias_correct = TRUE)
  
  # save file
  saveRDS(fit, paste0("fit_", y, ".rds"))
  saveRDS(index, paste0("index_", y, ".rds"))
}

#### new function for the updated yellowtail configuration. 
yellowtail_sdm_fn <- function(x, y, z) {
  # make mesh
  mesh <- sdmTMB::make_mesh(x, xy_cols = c("longitude", "latitude"), n_knots = 400)
  
  # fit model
  fit <- sdmTMB::sdmTMB(
    catch_weight ~ 0 + fyear + pass_scaled,
    data = x,
    mesh = mesh,
    family = delta_lognormal(),
    time = "Year",
    offset = log(x$effort),
    control = sdmTMB::sdmTMBcontrol(newton_loops = 3),
    anisotropy = TRUE,
    spatiotemporal = as.list(c("iid", "off"))
  )
  
  # get the index
  predictions <- predict(fit, newdata = z, return_tmb_object = TRUE)
  index <- sdmTMB::get_index(predictions, area = z$area_km2_WCGBTS, bias_correct = TRUE)
  
  # save file
  saveRDS(fit, paste0("fit_", y, ".rds"))
  saveRDS(index, paste0("index_", y, ".rds"))
}


#' Species distribution model function using delta_lognormal family
#'
#' Function to create a mesh, fit the sdmTMB model, and get the index.
#' Exports fit.rds and index.rds files to the designated species folder.
#' Used for petrale sole, sablefish.
#'
#' @param x speciesname_df[[i]] which is a data frame from a list of data frames
#' created from the cleanup_by_species() function and any further post-processing
#' of depth filters (see the smaller_function.R file for those).
#' @param y speciesname_files[[i]] which is an item in a list created from
#' names(speciesname_df)
#' @import sdmTMB
#'
species_sdm_lognormal_fn <- function(x, y, z) {
  # make mesh
  mesh <- sdmTMB::make_mesh(x, xy_cols = c("longitude", "latitude"), n_knots = 500)
  
  # fit model
  fit <- sdmTMB::sdmTMB(
    catch_weight ~ 0 + fyear + pass_scaled,
    data = x,
    mesh = mesh,
    family = delta_lognormal(),
    time = "Year",
    offset = log(x$effort),
    control = sdmTMB::sdmTMBcontrol(newton_loops = 3),
    anisotropy = TRUE,
    spatiotemporal = as.list(c("iid", "iid"))
  )
  
  # get the index
  predictions <- predict(fit, newdata = z, return_tmb_object = TRUE)
  index <- sdmTMB::get_index(predictions, area = z$area_km2_WCGBTS, bias_correct = TRUE) 
  # save file
  saveRDS(fit, paste0("fit_", y, ".rds"))
  saveRDS(index, paste0("index_", y, ".rds"))
}

#' Canary species distribution model function
#'
#' Function to create a mesh, fit the sdmTMB model, and get the index.
#' Exports fit.rds and index.rds files to the designated species folder.
#' Different from species_sdm_lognormal_fn in that the spatiotemporal
#' list is c("iid", "off").
#'
#' @param x speciesname_df[[i]] which is a data frame from a list of data frames
#' created from the cleanup_by_species() function and any further post-processing
#' of depth filters (see the smaller_function.R file for those).
#' @param y speciesname_files[[i]] which is an item in a list created from
#' names(speciesname_df)
#' @import sdmTMB
#'
canary_sdm_fn <- function(x, y, z) {
  # make mesh
  mesh <- sdmTMB::make_mesh(x, xy_cols = c("longitude", "latitude"), n_knots = 200)
  
  # fit model
  fit <- sdmTMB::sdmTMB(
    catch_weight ~ 0 + fyear + pass_scaled,
    data = x,
    mesh = mesh,
    family = delta_lognormal(),
    time = "Year",
    offset = log(x$effort),
    control = sdmTMB::sdmTMBcontrol(newton_loops = 3),
    anisotropy = FALSE,
    spatiotemporal = as.list(c("iid", "off"))
  )
  
  # get the index
  predictions <- predict(fit, newdata = z, return_tmb_object = TRUE)
  index <- sdmTMB::get_index(predictions, area = z$area_km2_WCGBTS, bias_correct = TRUE)
  
  # save file
  saveRDS(fit, paste0("fit_", y, ".rds"))
  saveRDS(index, paste0("index_", y, ".rds"))
}

#' Darkbloched species distribution model function
#'
#' Function to create a mesh, fit the sdmTMB model, and get the index.
#' Exports fit.rds and index.rds files to the designated species folder.
#' Different from species_sdm_lognormal_fn in that the spatiotemporal
#' list is c("off", "iid").
#'
#' @param x speciesname_df[[i]] which is a data frame from a list of data frames
#' created from the cleanup_by_species() function and any further post-processing
#' of depth filters (see the smaller_function.R file for those).
#' @param y speciesname_files[[i]] which is an item in a list created from
#' names(speciesname_df)
#' @import sdmTMB
#'
darkblotched_sdm_fn <- function(x, y, z) {
  # make mesh
  mesh <- sdmTMB::make_mesh(x, xy_cols = c("longitude", "latitude"), n_knots = 250)
  
  # fit model
  fit <- sdmTMB::sdmTMB(
    catch_weight ~ 0 + fyear + pass_scaled,
    data = x,
    mesh = mesh,
    family = delta_lognormal(),
    time = "Year",
    offset = log(x$effort),
    control = sdmTMB::sdmTMBcontrol(newton_loops = 3),
    anisotropy = TRUE,
    spatiotemporal = as.list(c("off", "iid"))
  )
  
  # get the index
  predictions <- predict(fit, newdata = z, return_tmb_object = TRUE)
  index <- sdmTMB::get_index(predictions, area = z$area_km2_WCGBTS, bias_correct = TRUE)
  
  # save file
  saveRDS(fit, paste0("fit_", y, ".rds"))
  saveRDS(index, paste0("index_", y, ".rds"))
}

#' Shortspine species distribution model function
#'
#' Function to create a mesh, fit the sdmTMB model, and get the index.
#' Exports fit.rds and index.rds files to the designated species folder.
#' Different from species_sdm_lognormal_fn in that the model contains
#' Depth_m and Depth_m^2
#'
#' @param x speciesname_df[[i]] which is a data frame from a list of data frames
#' created from the cleanup_by_species() function and any further post-processing
#' of depth filters (see the smaller_function.R file for those).
#' @param y speciesname_files[[i]] which is an item in a list created from
#' names(speciesname_df)
#' @import sdmTMB
#'
shortspine_sdm_fn <- function(x, y, z) {
  # make mesh
  mesh <- sdmTMB::make_mesh(x, xy_cols = c("longitude", "latitude"), n_knots = 350)
  
  # fit model
  fit <- sdmTMB::sdmTMB(
    catch_weight ~ 0 + fyear + pass_scaled + depth_scaled + depth_scaled_squared,
    data = x,
    mesh = mesh,
    family = delta_lognormal(),
    time = "Year",
    offset = log(x$effort),
    control = sdmTMB::sdmTMBcontrol(newton_loops = 3),
    anisotropy = TRUE,
    spatiotemporal = as.list(c("iid", "iid"))
  )
  
  # get the index
  predictions <- predict(fit, newdata = z, return_tmb_object = TRUE)
  index <- sdmTMB::get_index(predictions, area = z$area_km2_WCGBTS, bias_correct = TRUE)
  
  # save file
  saveRDS(fit, paste0("fit_", y, ".rds"))
  saveRDS(index, paste0("index_", y, ".rds"))
}


#' Widow species distribution model function
#'
#' Function to create a mesh, fit the sdmTMB model, and get the index.
#' Exports fit.rds and index.rds files to the designated species folder.
#' Different from species_sdm_fn in that the mesh is 200 n_knots and the
#' spatiotemporal list is c("off", "off")
#'
#' @param x speciesname_df[[i]] which is a data frame from a list of data frames
#' created from the cleanup_by_species() function and any further post-processing
#' of depth filters (see the smaller_function.R file for those).
#' @param y speciesname_files[[i]] which is an item in a list created from
#' names(speciesname_df)
#' @import sdmTMB
#'
widow_sdm_fn <- function(x, y, z) {
  # make mesh
  mesh <- sdmTMB::make_mesh(x, xy_cols = c("longitude", "latitude"), n_knots = 200)
  
  # fit model
  fit <- sdmTMB::sdmTMB(
    catch_weight ~ 0 + fyear + pass_scaled, 
    data = x,
    mesh = mesh,
    family = delta_gamma(),
    time = "Year",
    offset = log(x$effort),
    control = sdmTMB::sdmTMBcontrol(newton_loops = 3),
    anisotropy = TRUE,
    spatiotemporal = as.list(c("off", "off"))
  )
  
  # get the index
  predictions <- predict(fit, newdata = z, return_tmb_object = TRUE)
  index <- sdmTMB::get_index(predictions, area = z$area_km2_WCGBTS, bias_correct = TRUE)
  
  # save file
  saveRDS(fit, paste0("fit_", y, ".rds"))
  saveRDS(index, paste0("index_", y, ".rds"))
}