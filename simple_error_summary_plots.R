#make some simple plots to visualize error
#lead in taken from explore_results.r and not all these libraries are needed. 
library(ggplot2)
library(purrr)
library(dplyr)
library(here)
library(r4ss)
library(viridis)
library(stringr)
library(tidyr)
library(patchwork)
library(ggtext)
library(patchwork)
library(ss3sim)

#remotes::install_github("r4ss/r4ss")
#Derek had trouble with the here package so defined directories specifically. 
basedir<- "C:/Users/Derek.Bolser/Documents/Resample-survey-data-update"
plots<- "C:/Users/Derek.Bolser/Documents/Resample-survey-data-update/plots"
results<- "C:/Users/Derek.Bolser/Documents/Resample-survey-data-update/Results/summaries"
setwd(basedir)

resampled_dirs <- list.dirs("resampled_models", full.names = TRUE, recursive = FALSE)

# extract model results -----------------------------------------------------------
all_models <- r4ss::SSgetoutput(dirvec = resampled_dirs,modelnames = basename(resampled_dirs))
summaryoutput <- r4ss::SSsummarize(all_models, )
summaryoutput$modelnames <- basename(resampled_dirs)

#extract OFL sigma as it isn't in the summary
ofl_sigma_df <- imap_dfr(
  all_models,
  ~ tibble(
    model = .y,
    OFL_sigma = .x$OFL_sigma
  )
)

ofl_sigma_df <- ofl_sigma_df %>%
  mutate(
    species_name = str_extract(model, "^.*(?=_(?:0\\.\\d|1)_\\d+$)"),
    effort_level = as.numeric(str_extract(model, "(?<=_)(?:0\\.\\d|1)(?=_\\d+$)")),
    replicate    = as.integer(str_extract(model, "\\d+$"))
  )

ofl_sigma_df<-ofl_sigma_df[,2:5]

# examine ssb sd ----------------------------------------------------------------------
SSB_SD<-summaryoutput$SpawnBioSD 

#transpose df
ssbsd<-as.data.frame(t(SSB_SD))

#get the terminal years
ssbsd<-ssbsd[,143:151]

#make the label the column names
colnames(ssbsd) <- as.character(unlist(ssbsd[61, ]))
ssbsd<-ssbsd[1:60,]

#bring in row names
ssbsd<- tibble::rownames_to_column(ssbsd, var = "model")

#extract information from names
ssbsd <- ssbsd %>%
  mutate(
    species_name = str_extract(model, "^.*(?=_(?:0\\.\\d|1)_\\d+$)"),
    effort_level = as.numeric(str_extract(model, "(?<=_)(?:0\\.\\d|1)(?=_\\d+$)")),
    replicate    = as.integer(str_extract(model, "\\d+$"))
  )

#extract only the terminal year estimates 
ssbsd <- ssbsd %>%
  mutate(
    end_year_SSB_SD = case_when(
      species_name == "Longnose_skate"        ~ SSB_2018,
      species_name == "Pacific_ocean_perch"   ~ SSB_2016,
      species_name == "Petrale_sole"          ~ SSB_2022,
      species_name == "Sablefish"             ~ SSB_2024,
      species_name == "Shortspine_thornyhead" ~ SSB_2022,
      species_name == "Yellowtail_rockfish"   ~ SSB_2024
    )
  )

#trim df to only essential columns
ssbsd<-ssbsd[,11:14]

# examine SSB ----------------------------------------------------------------------
SSB<-summaryoutput$SpawnBio

#transpose df
ssb<-as.data.frame(t(SSB))

#get the terminal years
ssb<-ssb[,143:151]

#make the label the column names
colnames(ssb) <- as.character(unlist(ssb[61, ]))
ssb<-ssb[1:60,]

#bring in row names
ssb<- tibble::rownames_to_column(ssb, var = "model")

#extract information from names
ssb <- ssb %>%
  mutate(
    species_name = str_extract(model, "^.*(?=_(?:0\\.\\d|1)_\\d+$)"),
    effort_level = as.numeric(str_extract(model, "(?<=_)(?:0\\.\\d|1)(?=_\\d+$)")),
    replicate    = as.integer(str_extract(model, "\\d+$"))
  )

#extract only the terminal year estimates 
ssb <- ssb %>%
  mutate(
    end_year_SSB = case_when(
      species_name == "Longnose_skate"        ~ SSB_2018,
      species_name == "Pacific_ocean_perch"   ~ SSB_2016,
      species_name == "Petrale_sole"          ~ SSB_2022,
      species_name == "Sablefish"             ~ SSB_2024,
      species_name == "Shortspine_thornyhead" ~ SSB_2022,
      species_name == "Yellowtail_rockfish"   ~ SSB_2024
    )
  )

#trim df to only essential columns
ssb<-ssb[,11:14]

#join dfs to calculate CVs --------------------------------------------------
cvdf<- left_join(ssb,ssbsd,by = c("species_name","effort_level","replicate"))

cvdf$end_year_SSB<-as.numeric(cvdf$end_year_SSB)
cvdf$end_year_SSB_SD<-as.numeric(cvdf$end_year_SSB_SD)

cvdf$ssb_cv<-cvdf$end_year_SSB_SD/cvdf$end_year_SSB

cv_summaries <- cvdf %>%
  group_by(species_name, effort_level) %>%
  summarise(
    mean_ssb_cv = mean(ssb_cv, na.rm = TRUE),
    ssb_cv_sd = sd(ssb_cv, na.rm = TRUE)
  )

# calculate relative error statistics ------------------------------------------
redf<-cvdf %>%
  group_by(species_name) %>%
  mutate(
    ref_ssb = end_year_SSB_SD[effort_level == 1.0][1],
    relative_error = (end_year_SSB_SD - ref_ssb) / ref_ssb
  ) %>%
  ungroup() %>%
  select(-ref_ssb)

redf$MARE<- abs(redf$relative_error)

# examine the change in CV -----------------------------------------------------
edf<-redf %>%
  group_by(species_name) %>%
  mutate(
    ref_cv = ssb_cv[effort_level == 1.0][1],
    cv_dif = abs(ssb_cv - ref_cv)
  ) %>%
  ungroup() %>%
  select(-ref_cv)

#join with OFL sigma df
edf<- left_join(edf,ofl_sigma_df,by = c("species_name","effort_level","replicate"))

#write csvs
setwd(results)
#write.csv(edf,"west_coast_bts_effort_uncertainty_summary_statistics.csv",row.names = F)

#plots ------------------------------------------------------------------------
species_labels <- c(
  Longnose_skate = "Longnose skate (2018)",
  Pacific_ocean_perch = "Pacific ocean perch (2016)",
  Petrale_sole = "Petrale sole (2022)",
  Sablefish = "Sablefish (2024)",
  Shortspine_thornyhead = "Shortspine thornyhead (2022)",
  Yellowtail_rockfish = "Yellowtail rockfish (2024)"
)

#OFL sigma -----------------------------------------------------------------------
ggplot(edf,
       aes(x = factor(effort_level),
           y = OFL_sigma,
           fill = species_name)) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap(~ species_name, labeller = as_labeller(species_labels)) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(
    x = "Proportion of historical survey effort",
    y = "Terminal year overfishing limit sigma",
    fill = "Species"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none"
  )

setwd(plots)
ggsave(filename = 'OFL_sigma_panel_boxplot.png',plot = last_plot() , path = plots, width = 9.5, height = 6.5, device = 'png', dpi = 300)

#CVs --------------------------------------------------------
#panel with colors
species_labels <- c(
  Longnose_skate = "Longnose skate (2018)",
  Pacific_ocean_perch = "Pacific ocean perch (2016)",
  Petrale_sole = "Petrale sole (2022)",
  Sablefish = "Sablefish (2024)",
  Shortspine_thornyhead = "Shortspine thornyhead (2022)",
  Yellowtail_rockfish = "Yellowtail rockfish (2024)"
)

ggplot(redf,
       aes(x = factor(effort_level),
           y = ssb_cv,
           fill = species_name)) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap(~ species_name, labeller = as_labeller(species_labels)) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(
    x = "Proportion of historical survey effort",
    y = "Coeffecient of variation in terminal year spawning stock biomass estimate",
    fill = "Species"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none"
  )

setwd(plots)
#ggsave(filename = 'CV_panel_boxplot.png',plot = last_plot() , path = plots, width = 9.5, height = 6.5, device = 'png', dpi = 300)

#MARE plot -------------------------------------------------------
ggplot(redf %>% dplyr::filter(effort_level != 1),
       aes(x = factor(effort_level),
           y = MARE,
           fill = species_name)) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap(~ species_name, labeller = as_labeller(species_labels)) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(
    x = "Proportion of historical survey effort",
    y = "Mean absolute relative error in terminal year spawning stock biomass estimate",
    fill = "Species"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none"
  )

setwd(plots)
#ggsave(filename = 'MARE_panel_boxplot.png',plot = last_plot() , path = plots, width = 9.5, height = 6.5, device = 'png', dpi = 300)
