# Plotting functions
plot_effort_vs_og_indices <- function(species_fleet_df) {
  # Use pmap to iterate over the data frame rows
  results <- purrr::pmap(
    species_fleet_df,
    function(name, fleet) {
      og_mod <- r4ss::SS_read(here("original_models", name))
      og_index <- og_mod$dat$CPUE |>
        filter(index == fleet) |>
        select(year, obs, se_log) |>
        rename(se = se_log) |>
        mutate(effort = "original model",
               replicate = NA,
               effort_rep = "original model")
      
      i_files <- list.files(here("Results", name), recursive = TRUE, full.names = TRUE)
      i_csv <- i_files[grepl(".*_indices_df.csv$", i_files)]
      if (length(i_csv) == 0) return(NULL)
      
      effort_indices <- read.csv(i_csv, header = TRUE) |>
        filter(!is.na(se)) |>
        filter(effort %in% c(0.2, 0.4, 0.8, 1)) |>
        select(Year, est, se, effort, replicate) |>
        rename(year = Year, obs = est) |>
        mutate(effort = as.character(effort),
               effort_rep = paste0(effort,"_",replicate)) |>
        filter(year <= og_mod$dat$endyr)
      
      bind_rows(effort_indices, og_index) |>
        mutate(species = tolower(gsub("_"," ",name)))
    }
  )
  
  all_indices <- bind_rows(results) |>
    dplyr::mutate( # will remove this once scale of indices is fixed
      obs = dplyr::case_when(
        effort == "original model" & species == "longnose skate" ~ obs * 18,
        effort == "original model" & species %in% c("petrale sole", "longnose skate", "sablefish") ~ obs * 20,
        effort == "original model" & species == "pacific ocean perch" ~ obs * 30,
        effort == "original model" & species == "shortspine thornyhead" ~ obs * 25,
        effort == "original model"& species == "yellowtail rockfish" ~ obs * 100,
        TRUE ~ obs
      )
    )
  
  effort_summary <- all_indices %>%
    dplyr::filter(effort !="original model") %>%
    dplyr::group_by(species, year, effort) %>%
    dplyr::summarise(
      mean_obs = mean(obs, na.rm = TRUE),
      se_obs = sd(obs, na.rm = TRUE) / sqrt(dplyr::n()),
      .groups = "drop"
    )
  
  original_model_data <- all_indices %>%
    dplyr::filter(effort == "original model")
  
  # Plot with facets for species
  p <- ggplot() +
    # SE ribbon
    geom_ribbon(
      data = effort_summary,
      aes(x = year, ymin = mean_obs - se_obs, ymax = mean_obs + se_obs, fill = effort, group = effort),
      alpha = 0.2
    ) +
    # Mean points
    geom_line(
      data = effort_summary,
      aes(x = year, y = mean_obs, color = effort, group = effort)
    ) +
    # Original model as line
    geom_line(
      data = original_model_data,
      aes(x = year, y = obs, color = effort, group = effort),
      linetype = "dotted",
      size = 0.5
    ) +
    facet_wrap(~species, scales = "free_y") +
    theme_bw() +
    labs(
      x = "Year",
      y = "Index",
      fill = "Model/Effort",
      color = "Model/Effort"
    )
  
  list(data = all_indices, plot = p)
}



plot_comparisons_ggplot <- function(
  summaryoutput,
  subplots = c(1,2,3,4),
  models = "all",
  endyrvec = NULL,
  legendlabels = NULL,
  uncertainty = TRUE,
  rescale = TRUE,
  show_equilibrium = TRUE,
  summarize_by_species_effort = TRUE
  ) {
    # Helper for extracting species and effort from model name
  extract_species_effort <- function(model_name) {
    # Split the name by underscore
    parts <- strsplit(model_name, "_")[[1]]
    n <- length(parts)
    # If last part is numeric and the second last is numeric, then replicate is present
    if (n >= 3 && grepl("^[0-9.]+$", parts[n-1]) && grepl("^[0-9]+$", parts[n])) {
      # e.g. yellowtail_rockfish_0.2_2 or yellowtail_0.2_2
      species <- paste(parts[1:(n-2)], collapse = "_")
      effort <- as.numeric(parts[n-1])
    } else if (n >= 2 && grepl("^[0-9.]+$", parts[n])) {
      # e.g. yellowtail_rockfish_0.2 or yellowtail_0.2
      species <- paste(parts[1:(n-1)], collapse = "_")
      effort <- as.numeric(parts[n])
    } else {
      species <- NA
      effort <- NA
    }
    list(species = species, effort = effort)
  }
    
    # Helper to melt wide summary tables to long
    melt_model_table <- function(tbl, value_name = "value") {
      tbl_long <- tidyr::pivot_longer(
        tbl,
        cols = -1,      # everything except first column (Yr or Label)
        names_to = "model",
        values_to = value_name
      )
      tbl_long
    }
    
    plots <- list()
    n <- summaryoutput[["n"]]
    if (models[1] == "all") models <- 1:n
    nlines <- length(models)
    model_names <- if (!is.null(legendlabels)) legendlabels else paste("model", models)
    
    # Extract species and effort for each model
    parse_res <- lapply(model_names, extract_species_effort)
    species_vec <- sapply(parse_res, function(x) x$species)
    effort_vec <- sapply(parse_res, function(x) x$effort)
    
    species_effort_table <- data.frame(
      model = model_names,
      species = species_vec,
      effort = effort_vec,
      stringsAsFactors = FALSE
    )
    
    # --------- Spawning Biomass (subplots 1 and 2) ----------
    if (any(subplots %in% c(1,2))) {
      dat <- summaryoutput[["SpawnBio"]]
      dat_lower <- summaryoutput[["SpawnBioLower"]]
      dat_upper <- summaryoutput[["SpawnBioUpper"]]
      # Convert to long
      dat_long <- melt_model_table(dat, "value")
      dat_long$lower <- melt_model_table(dat_lower, "lower")$lower
      dat_long$upper <- melt_model_table(dat_upper, "upper")$upper
      dat_long$model <- factor(dat_long$model, labels=model_names)
      dat_long <- left_join(dat_long, species_effort_table, by = "model")
      dat_long_plot <- dplyr::filter(dat_long, Yr > min(Yr) & Yr > sort(unique(Yr))[2])
      
      if (summarize_by_species_effort) {
        # Summarize across replicates for each species and effort
        dat_summ <- dat_long_plot %>%
          group_by(Yr, species, effort) %>%
          summarize(
            mean_val = mean(value, na.rm = TRUE),
            se_val = sd(value, na.rm = TRUE) / sqrt(sum(!is.na(value))),
            mean_lower = mean(lower, na.rm = TRUE),
            mean_upper = mean(upper, na.rm = TRUE)
          ) %>%
          ungroup()
        p <- ggplot(dat_summ, aes(x=Yr, y=mean_val, color=factor(effort), group=effort, fill=factor(effort))) +
          geom_line(size=1) +
          geom_point(size=1.5) +
          labs(x="Year", y="Spawning biomass (t)", color="Effort", fill="Effort") +
          theme_minimal() +
          facet_wrap(~species, scales="free_y")
        if (uncertainty) {
          p <- p + geom_ribbon(aes(ymin=mean_val-se_val, ymax=mean_val+se_val), alpha=0.2, color=NA)
        }
      } else {
        p <- ggplot(dat_long_plot, aes(x=Yr, y=value, color=model, fill=model, group=model)) +
          geom_line(size=1) +
          geom_point(size=1.5) +
          labs(x="Year", y="Spawning biomass (t)", color="Model", fill="Model") +
          theme_minimal()
        if (uncertainty) {
          p <- p + geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.2, color=NA)
        }
      }
      plots$spawning_biomass <- p + ggtitle("Spawning Biomass Comparison")
    }
    
    # --- Repeat similar logic for other subplots (biomass ratio, etc.) as desired ---
    # Make sure to use facet_wrap(~species) and color/group by effort when summarizing
    
    return(plots)
  }