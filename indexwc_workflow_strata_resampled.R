#### indexwc workflow, modified to include resampling procedure ####
#### clear environment
rm(list = ls())

#install
# pak::pak("tidyverse")
# pak::pak("furrr")
# pak::pak("sdmTMB")
# pak::pak("pfmc-assessments/indexwc")
# pak::pak("ropenblas")

#load packages
library(indexwc)
library(tidyverse)
library(sdmTMB)
library(furrr)
library(purrr)

#setwds
wd <- "/home/user"
basedir<-file.path(wd,'Resample-survey-data')
#redefine basedir to reference full archive. 
#basedir<-file.path(wd,'Resample-survey-data-archive')
datadir<-file.path(basedir,'data')
longnose<-file.path(basedir,'longnose_skate/wcgbts/delta_gamma/data/')
pop<-file.path(basedir,'pacific_ocean_perch/wcgbts/delta_gamma/data/')
petrale<-file.path(basedir,'petrale_sole/wcgbts/delta_lognormal/data/')
sablefish<-file.path(basedir,'sablefish/wcgbts/delta_gamma/data/')
#shortspine<-file.path(basedir,'shortspine_thornyhead/wcgbts/delta_lognormal/data/')
shortspine<-file.path(basedir,'shortspine_thornyhead/wcgbts/delta_gamma/data/') #formerly defined as shortspine_dg
yellowtail<-file.path(basedir,'yellowtail_rockfish/wcgbts/delta_lognormal/data/')

setwd(basedir)

#load functions
source(file.path(basedir, "smaller_functions.R")) #need to edit to specify the code directory if running locally
source(file.path(basedir, "indexwc_functions.R")) #need to edit to specify the code directory if running locally

#get rid of memory limits
options(future.globals.maxSize = 1 * 1024^4)  # Allow up to 1 TB for globals

#verify use of ropenblas upon starting new VM session. 
extSoftVersion()["BLAS"] #should be: "opt/OpenBlas/lib/libopenblas_haswellp-r0.3.34.so" or a newer version. If not, uncomment the code below
ropenblas::ropenblas(x = "0.3.34")

#get configuration and cc grid
setwd(datadir)
load("configuration.rda") 
load("california_current_grid.rda") 

#filter out non-focal species
focal<- c("Pacific ocean perch", "sablefish", "petrale sole", "longnose skate", "yellowtail rockfish", "shortspine thornyhead")
configuration <- configuration[configuration$species %in% focal, ]

#remove triennial configurations
configuration<- configuration[configuration$source == "NWFSC.Combo",]

#remove alternate configurations
configuration<-configuration[c(1:3,5,6,9),] #selecting the config without split_Mendocino for yellowtail. 

#alter the shortspine configuration to reflect most recent configuration in assessment. Depth and depth^2 were not used for 
#NWFSC combo dataset. 
#filter for shortspine only
#configuration<-configuration[configuration$species == "shortspine thornyhead",]

#configuration$formula[configuration$species == "shortspine thornyhead"]<- "catch_weight ~ 0 + fyear + pass_scaled"

#try out delta gamma for shortspine
configuration$family[configuration$species == "shortspine thornyhead"]<- "sdmTMB::delta_gamma()"

#specify a different number of knots
#configuration$knots[configuration$species == "shortspine thornyhead"]<- 400

# Download the data and filter the data based upon species-specific
# depths and latitudes in the configuration file ############################################################
# define strata to explore resampling by strata
strata <- nwfscSurvey::create_strata(
  names = paste(
    sep = "_",
    rep(times = 4, c("55m-183m", "183m-549m", "549m-900m", "900m-1280m")),
    rep(each = 4, c("32-34.5", "34.5-42.0", "42.0-46.0", "46.0-49"))
  ),
  depths_shallow = rep(times = 4, x = c(55, 183, 549, 900)),
  depths_deep = rep(times = 4, x = c(183, 549, 900, 1280)),
  lats_south = rep(each = 4, x = c(32, 34.5, 42, 46.0)),
  lats_north = rep(each = 4, x = c(34.5, 42.0, 46.0, 49))
)

###make depths negative
# strata <- strata %>%
#   mutate(
#     Depth_m.1 = -Depth_m.1,
#     Depth_m.2 = -Depth_m.2
#   )

####pull data based on configurations
setwd(basedir)

data <- configuration |>
  dplyr::rowwise() |>
  dplyr::mutate(
    data_raw = list(
      format_data(eval(parse(text = fxn)))
    ),
    data_filtered = list(
      resample_by_strata(data_raw, strata)
    )
  ) |>
  dplyr::ungroup()

#### unnest data and get the names of the dfs
data<-data|> tidyr::unnest_longer(data_filtered, indices_include = TRUE) 
data$data_name<- paste0(data$species,"_",data$data_filtered_id) #get df names. need to add spp name to get unique IDs
data$data_name <- gsub("[^0-9A-Za-z.-]", "_", data$data_name)
data$data_filtered <- lapply(
  data$data_filtered,
  function(df) {
    df$source <- stringr::str_extract(
      df$source,
      "(0\\.2|0\\.4|0\\.6|0\\.8|1)(?:_(?:[1-9]|10))$"
    )
    df
  }
)

#### get the prediction grid
#depth must be negative
california_current_grid<- california_current_grid[!california_current_grid$depth< 0,] #remove land values
california_current_grid$depth<- -abs(california_current_grid$depth) #make depths negative

##### Run the model across all species in the configuration file
run_model_safely <- purrr::safely(run_sdmtmb_batches) #necessary to run past errors; indexwc::run_sdmtmb changed to reference resampled version

#rerun shortspine with updated configuration
#data<-data[data$species == "shortspine thornyhead",]
#chase down fits that didn't converge. 
#data<-data[c(4,7,10,11),] #200 knots for 0.4_1, 0.6_1, 0.8_1
#data<- data[13,] #test delta gamma family for full effort model. 
#setwd(basedir)
#0.4_1 is singular. 
#data<-data[4,] #150 kts for 0.8_2

#run models
setwd(basedir)
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
###calculate the remaining shortspine indices. 0.6_1 and 0.8_1 have non positive definite hessian. 
#setwd(shortspine_dg)
#fit<-readRDS("fit_shortspine_thornyhead_1_1.rds")

#test<-data$data_filtered[[65]]

####pred_grid
pred_grid <- sdmTMB::replicate_df(california_current_grid,
                                  time_name = "year",
                                  time_values = unique(fit$data$year))

pred_grid$fyear <- as.factor(pred_grid$year)
pred_grid$depth_scaled<- scale(pred_grid$depth)
pred_grid$depth_scaled_squared<- pred_grid$depth_scaled^2

####calc index areas: indexwc procedure
index <- calc_index_areas(
  data = fit$data,
  fit = fit,
  prediction_grid = pred_grid,
  boundaries = c("Coastwide"),
  dir = shortspine_dg
)

write.csv(
  index[["indices"]],
  file = file.path(shortspine,paste0("indices_","shortspine_thornyhead_delta_gamma_1_1",".csv")),
  row.names = FALSE
)

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

#run the function on each directory individually. 
setwd(basedir)
longnose_indices<- calc_indices(longnose) #done
petrale_indices<- calc_indices(petrale) #done
pop_indices<- calc_indices(pop) #done
sablefish_indices<- calc_indices(sablefish) #done
shortspine_indices<- calc_indices(shortspine) #changed to _dg for testing
yellowtail_indices<- calc_indices(yellowtail) #done

####run summary and formatting functions
##### process fit files; is this necessary? The functions need updating. 
#process_and_save_fits(longnose,"longnose_skate")
#process_and_save_fits(pop,"Pacific_ocean_perch")
#process_and_save_fits(petrale,"petrale_sole")
#process_and_save_fits(sablefish,"sablefish")
#process_and_save_fits(shortspine,"shortspine_thornyhead")
#process_and_save_fits(yellowtail,"yellowtail_rockfish")

##### process index files
process_indexwc_indices(longnose,"longnose_skate")
process_indexwc_indices(pop,"Pacific_ocean_perch")
process_indexwc_indices(petrale,"petrale_sole")
process_indexwc_indices(sablefish,"sablefish")
process_indexwc_indices(shortspine,"shortspine_thornyhead")
process_indexwc_indices(yellowtail,"yellowtail_rockfish")

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
