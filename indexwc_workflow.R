#### indexwc workflow, modified to include resampling procedure ####
#### clear environment
rm(list = ls())

#install
#pak::pak("pfmc-assessments/indexwc")

#load packages
library(indexwc)
library(tidyverse)
library(sdmTMB)
library(furrr)

#setwds
wd <- "/home/user"
basedir<-file.path(wd,'Resample-survey-data')
datadir<-file.path(basedir,'data')
longnose<-file.path(basedir,'longnose_skate/wcgbts/delta_gamma/data/')
pop<-file.path(basedir,'pacific_ocean_perch/wcgbts/delta_gamma/data/')
petrale<-file.path(basedir,'petrale_sole/wcgbts/delta_lognormal/data/')
sablefish<-file.path(basedir,'sablefish/wcgbts/delta_gamma/data/')
shortspine<-file.path(basedir,'shortspine_thornyhead/wcgbts/delta_lognormal/data/')
yellowtail<-file.path(basedir,'yellowtail_rockfish/wcgbts/delta_lognormal/data/')

setwd(basedir)

#load functions
source(file.path(basedir, "smaller_functions.R")) #need to edit to specify the code directory if running locally
source(file.path(basedir, "indexwc_functions.R")) #need to edit to specify the code directory if running locally

#get rid of memory limits
options(future.globals.maxSize = 1 * 1024^4)  # Allow up to 1 TB for globals

#verify use of ropenblas upon starting new VM session. 
extSoftVersion()["BLAS"] #should be: "opt/OpenBlas/lib/libopenblas_haswellp-r0.3.13.so" or a newer version. If not, uncomment the code below
ropenblas::ropenblas(x = "0.3.32")

#get configuration and cc grid
load("configuration.rda")
setwd(datadir)
load("california_current_grid.rda")

#filter out non-focal species
focal<- c("Pacific ocean perch", "sablefish", "petrale sole", "longnose skate", "yellowtail rockfish", "shortspine thornyhead")
configuration <- configuration[configuration$species %in% focal, ]

#remove triennial configurations
configuration<- configuration[configuration$source == "NWFSC.Combo",]

#remove alternate configurations
configuration<-configuration[c(1:3,5,6,9),] #selecting the config without split_Mendocino for yellowtail. 

# Download the data and filter the data based upon species-specific
# depths and latitudes in the configuration file ############################################################
setwd(basedir)

data <- configuration |>
  dplyr::rowwise() |>
  dplyr::mutate(
    data_raw = list(
      format_data(eval(parse(text = fxn)))),  
    data_filtered = list(data_raw |> resample_data() )
  ) |>
  dplyr::ungroup()

#### unnest data and get the names of the dfs
data<-data|> tidyr::unnest_longer(data_filtered, indices_include = TRUE)
data$data_name<- paste0(data$species,"_",data$data_filtered_id) #get df names. need to add spp name to get unique IDs
data$data_name <- gsub("[^0-9A-Za-z.-]", "_", data$data_name)

#### get the prediction grid
#depth must be negative
california_current_grid<- california_current_grid[!california_current_grid$depth< 0,] #remove land values
california_current_grid$depth<- -abs(california_current_grid$depth) #make depths negative

##### Run the model across all species in the configuration file
run_model_safely <- purrr::safely(run_sdmtmb_batches) #necessary to run past errors; indexwc::run_sdmtmb changed to reference resampled version

index_run <- data |>
  dplyr::mutate( 
    family = purrr::map(family, .f = ~ eval(parse(text = .x))), 
    
    results = purrr::pmap(
      .l = list(
        data = data_filtered,
        data_name = data_name,
        formula = formula,
        family = family,
        anisotropy = anisotropy,
        n_knots = knots,
        share_range = share_range,
        spatiotemporal = purrr::map2(spatiotemporal1, spatiotemporal2, list)
      ),
      .f = run_model_safely #run_sdmtmb_batches
    )
  )

####test the calc index areas function.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              #### calculate indices
###test shortspine.
# setwd(shortspine)
# fit<-readRDS("fit_shortspine_thornyhead_1_1.rds")
# 
# test<-data$data_filtered[[65]]
# 
# ####pred_grid
# pred_grid <- sdmTMB::replicate_df(california_current_grid,
#   time_name = "year",
#   time_values = unique(fit$data$year))
# 
# pred_grid$fyear <- as.factor(pred_grid$year)
# pred_grid$depth_scaled<- scale(pred_grid$depth)
# pred_grid$depth_scaled_squared<- pred_grid$depth_scaled^2
# 
# ####calc index areas: indexwc procedure
# index <- calc_index_areas(
#   data = fit$data,
#   fit = fit,
#   prediction_grid = pred_grid,
#   boundaries = c("Coastwide"),
#   dir = shortspine
# )
# 
# write.csv(
#   index[["indices"]],
#   file = file.path(shortspine,paste0("indices_","shortspine_thornyhead_1_1_test",".csv")),
#   row.names = FALSE
# )

# function to calculate indices for all fits in one directory
calc_indices <- function(dir) {
  
  # list all .rds files
  fit_files <- list.files(dir, pattern = "fit_.*rds$", full.names = TRUE)
  
  map(fit_files, function(f) {
    fit_obj <- readRDS(f)

    data_name<- paste0(fit_obj$data$common_name[[1]],"_",fit_obj$data$source[[1]]) 
    data_name <- gsub("[^0-9A-Za-z.-]", "_", data_name)
    
    # skip NULL or broken fits
    if (is.null(fit_obj)) {
      message("Skipping NULL fit: ", basename(f))
      return(NULL)
    }
    
    cli::cli_inform(c(
      "*" = "Calculating index for {fit_obj$data[1, 'common_name']}"
    ))
    
    grid <- sdmTMB::replicate_df(california_current_grid,
                                      time_name = "year",
                                      time_values = unique(fit_obj$data$year))
    
    grid$fyear <- as.factor(grid$year)
    grid$depth_scaled<- scale(grid$depth)
    grid$depth_scaled_squared<- grid$depth_scaled^2
    
    # run calc_index_areas using the $data stored in the fit; add the offset?
    tryCatch({
      index<- calc_index_areas(
        data = fit_obj$data,
        fit = fit_obj,
        prediction_grid = grid,
        boundaries = "Coastwide",
        dir = dir
      )
      
      write.csv(
        index[["indices"]],
        file = file.path(dir,paste0("indices_",data_name,".csv")),
        row.names = FALSE
      )
      #saveRDS(index, file = fs::path(dir,paste0("index_",data_name,".rds")))
      #return(index)
      
    }, error = function(e) {
      message("Error in ", data_name, ": ", conditionMessage(e))
      return(NULL)
    })
    
  })

}

##### get all the indices. This does not work for some reason.
#dirs<-c(longnose, petrale, pop, sablefish, shortspine, yellowtail)
# # 
# plan(multisession, workers = 6)
# all_indices <- future_map(dirs, calc_indices)
# names(all_indices) <- basename(dirs)

#run the function on each directory individually. 
longnose_indices<- calc_indices(longnose) #done
petrale_indices<- calc_indices(petrale) #done
pop_indices<- calc_indices(pop) #done
sablefish_indices<- calc_indices(sablefish) #done
shortspine_indices<- calc_indices(shortspine) #done
yellowtail_indices<- calc_indices(yellowtail) #done

#####try running diagnostics. 
# setwd(longnose)
# check_sd <- function(fit) {
#   tryCatch({
#     rep <- fit$sd_report
#     return(!is.null(rep) && length(rep$value) > 0)
#   }, error = function(e) FALSE)
# }
# 
# sapply(fit_petrale, check_sd)
