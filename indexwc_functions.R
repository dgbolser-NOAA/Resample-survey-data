##### locally defined and modified indexwc functions

#### resampling function: an alternative to cleanup_by_species to work within the indexwc workflow. ######################
resample_data<-function(df) {
  catch_split <- split(df,df$year)
  
  tows <- lapply(catch_split, tow_fn)
  
  # Assign random 1s and 0s based on the specified proportions to a list of dataframes
  props <- as.data.frame(seq(0.2, 1.0, by = 0.2))
  names(props) <- "trawl_id"
  
  # match the structure of the catch data
  props <- rep(props, length(tows))
  
  tows_assigned <- map2(tows, props, include_or_exclude)
  
  # remove replicates of the 1 effort level
  tows_assigned <- lapply(tows_assigned, function(x) {
    x <- x[1:13]
    return(x)
  })
  
  tows_assigned_resampled <- purrr::map(tows_assigned, function(x) {
    purrr::map(x, function(y) {
      y[y$RandomAssignment == 1, ]
    })
  })
  
  tows_assigned_resampled <- unlist(tows_assigned_resampled, recursive = F)
  
  alldata_resampled <- join_dfs(tows_assigned_resampled, df, "trawl_id")
  
  names(alldata_resampled) <- substr(names(alldata_resampled), 6, 50) # it would be good to replace 50 with a logical indicating the end
  
  species_all_yrs <- alldata_resampled %>%
    bind_rows(.id = "source")
  
  species_all_yrs <- split(species_all_yrs, species_all_yrs$source)
  
  rm("catch_split", "tows", "props", "tows_assigned", "alldata_resampled")
  
  return(species_all_yrs)
}

########## define format common name as it isn't in the package ######################################################
format_common_name <- function(x) {
  tolower(
    gsub(
      pattern = "\\s",
      replacement = "_",
      x = gsub(
        pattern = "[[]:punctuation:]]|\\.",
        replacement = "",
        x = x
      )
    )
  )
}

############ define format formula as it isn't in the package ########################################################
format_formula <- function(x) {
  # For delta models, users can input a list.
  # So, call this function on each list member.
  if (inherits(x, "list") && length(x) == 2) {
    x <- purrr::map(x, format_formula)
  }
  # This is the typical behavior that should be used and will be called
  # above when this function calls itself to go from character to formula.
  if (inherits(x, "character")) {
    stopifnot(length(x) == 1)
    if (length(x) == 3 && x[1] == "~") {
      x <- paste(x[2], x[1], x[3])
    }
    x <- as.formula(paste(x, collapse = ""))
  }
  return(x)
}

##### is depth in formula is also called and needs to be defined #######################################################
is_depth_in_formula <- function(x, delta_lgl) {
  purrr::map_lgl(
    .x = if (length(x) != 2 && delta_lgl) {
      list(x, x)
    } else {
      x
    },
    .f = ~ any(grepl(pattern = "depth_scaled", .x))
  )
}

##### alter run_sdmTMB to handle many resampled datasets with the same configuration ####################################
#run sdmtmb
run_sdmtmb_batches <- function(dir_main = getwd(),
                                 data,
                                 data_name,
                                 family,
                                 formula,
                                anisotropy,
                                 n_knots,
                                 share_range,
                                 sdmtmb_control = sdmTMB::sdmTMBcontrol(newton_loops = 3),
                                 ...) {
  # Checks
  stopifnot(inherits(family, "family"))
  stopifnot(all(
    c(
      "year", "fyear", "survey_name", "common_name",
      "catch_weight", "effort", "x", "y"
    ) %in%
      colnames(data)
  ))
  # Create directory structure
  if(!is.null(dir_main)) {
    
    dir_new <- data |>
      dplyr::group_by(.data$survey_name, .data$common_name) |>
      dplyr::count() |>
      dplyr::mutate(
        common_without = format_common_name(.data$common_name),
        survey_without = format_common_name(.data$survey_name),
        directory = fs::path(
          dir_main,
          .data$common_without,
          .data$survey_without,
          indexwc::format_family(family)
        )
      ) |>
      dplyr::pull(.data$directory)
    stopifnot(length(dir_new) == 1)
    dir_data <- fs::path(dir_new,"data")
    fs::dir_create(dir_data)
    save(data, file = file.path(dir_data, paste0("data_",data_name,".rdata")))
  }
  formula <- format_formula(formula)
  cli::cli_inform(c(
    "*" = "Running sdmTMB for {data[1, 'common_name']}"
  ))
 
  # Create and save mesh
  mesh <- sdmTMB::make_mesh(
    data = data,
    xy_cols = c("x", "y"),
    n_knots = n_knots
  )
  # Fit model
  fit <- sdmTMB::sdmTMB(
    formula = formula,
    time = "year",
    offset = log(data$effort),
    data = data,
    mesh = mesh,
    family = family,
    control = sdmtmb_control,
    share_range = share_range,
    do_fit = TRUE,
    ...
  )
  # Refit if hessian not positive definite
  if (!fit[["pos_def_hessian"]]) {
    fit <- sdmTMB::run_extra_optimization(fit)
  }
  # Save model output
  if(!is.null(dir_main)) {
    saveRDS(fit, file = fs::path(dir_data,paste0("fit_",data_name,".rds")))
  }
  # Attach mesh for downstream use
  fit$mesh <- mesh
  return(fit)
  
  #create prediction grid
  # Create prediction grid; replaced the creation of data truncated, ranges, and a different grid using indexwc function. 
  pred_grid <- sdmTMB::replicate_df(california_current_grid,
                                    time_name = "year",
                                    time_values = unique(fit$data$year))
  
  pred_grid$fyear <- as.factor(pred_grid$year)
  
  # # get the index
  index <- calc_index_areas(
    data = fit$data,
    fit = fit,
    prediction_grid = pred_grid,
    boundaries = c("Coastwide"),
    dir = dir_data
  )

  # # save file
  saveRDS(index, file = fs::path(dir_data,paste0("index_",data_name,".rds")))
}
