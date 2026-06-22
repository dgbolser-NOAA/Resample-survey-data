# Plotting functions
# Indices Plots
plot_effort_vs_og_indices <- function(species_fleet_df, plot_save_dir) {
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
        filter(effort %in% c(0.2, 0.4, 0.6, 0.8, 1)) |>
        select(Year, est, se, effort, replicate) |>
        rename(year = Year, obs = est) |>
        mutate(effort = as.character(effort),
               effort_rep = paste0(effort,"_",replicate)) |>
        filter(year <= og_mod$dat$endyr)
      
      bind_rows(effort_indices, og_index) |>
        mutate(species = gsub("_"," ",name))
    }
  )
  
  # Hopefully don't have to do this
  all_indices <- bind_rows(results)
  
  effort_summary <- all_indices |>
    dplyr::group_by(species, year, effort) |>
    dplyr::summarise(
      mean_obs = mean(obs, na.rm = TRUE),
      se_obs = sd(obs, na.rm = TRUE) / sqrt(dplyr::n()),
      .groups = "drop"
    ) |>
    mutate(se_obs = case_when(
      effort == 1 ~ NA_real_,
      effort == "original_model" ~ NA_real_,
      TRUE ~ se_obs
    ))
  
  effort_levels <- c("0.2", "0.4", "0.6", "0.8", "1", "original model")
  effort_colors <- c(
    "0.2" = viridis(5)[1],
    "0.4" = viridis(5)[2],
    "0.6" = viridis(5)[3],
    "0.8" = viridis(5)[4],
    "1" = viridis(5)[5],
    "original model" = "black"
  )
  effort_fills <- c(
    "0.2" = viridis(5)[1],
    "0.4" = viridis(5)[2],
    "0.6" = viridis(5)[3],
    "0.8" = viridis(5)[4],
    "1" = "transparent",
    "original model" = "transparent"
  )
  
  effort_summary$effort <- factor(effort_summary$effort, levels = effort_levels)
  
  # Plot with facets for species
  p <- ggplot(effort_summary, aes(x = year)) +
    geom_ribbon(
      aes(ymin = mean_obs - se_obs, ymax = mean_obs + se_obs, fill = effort, group = effort),
      alpha = 0.2
    ) +
    geom_line(
      aes(y = mean_obs, color = effort, linetype = effort, group = effort)
    ) +
    facet_wrap(~species, scales = "free_y") +
    theme_bw() +
    labs(
      x = "Year",
      y = "Index"
    ) +
    expand_limits(y = 0) +
    scale_linetype_manual(values = c("0.2" = "solid", "0.4" = "solid", "0.6" = "solid", "0.8" = "solid", "1" = "solid", "original model" = "dashed"), name = "Model/Effort") +
    scale_color_manual(values = effort_colors, name = "Model/Effort") +
    scale_fill_manual(values = effort_fills, name = "Model/Effort") +
    scale_y_continuous(labels = function(x) format(x, trim = TRUE, scientific = FALSE)) +
    theme(strip.text = element_text(size = 12, face = "bold"), 
          panel.grid.minor = element_blank(),
          axis.title.x = element_text(face = "bold"),
          axis.title.y = element_text(face = "bold"),
          legend.title = element_text(face = "bold"),
          plot.background = element_rect(fill = "transparent", colour = NA),
          panel.background = element_rect(fill = "transparent", colour = NA),
          legend.background = element_rect(fill = "transparent", colour = NA))
  
  ggplot2::ggsave(
    filename = file.path(plot_save_dir, "effort_indices.png"),
    plot = p,
    width = 1130, # Set width in pixels
    height = 505, # Set height in pixels
    units = "px", # Specify units as pixels
    dpi = 100, # Use a standard DPI when using pixel dimensions
    bg = "transparent"
  )
  
  list(data = all_indices, plot = p)
}


# All the assessment output plots and length/age weights comparison
# Go through and filter recruits and recruitment devs plots to only show years like 1990 onward
plot_comparisons_ggplot <- function(
    summaryoutput,
    all_output, # result from r4ss::SSgetoutput()
    subplots = c(1,2,3,4,5,6,7),
    models = "all",
    endyrvec = NULL,
    legendlabels = NULL,
    rescale = TRUE,
    show_equilibrium = TRUE,
    plot_save_dir
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
      cols = -matches("Label|Yr"), # everything except first column (Yr or Label)
      names_to = "model",
      values_to = value_name
    )
    tbl_long
  }
  
  plots <- list()
  n <- summaryoutput[["n"]]
  if (models[1] == "all") models <- 1:n
  nlines <- length(models)
  model_names <- if (!is.null(legendlabels)){legendlabels} else
    if(any(grepl("replist", summaryoutput$modelnames))) {
      paste("model", models)
    } else {
      summaryoutput$modelnames
    }
  
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
  
  summaryoutput <- purrr::map(summaryoutput, function(x) {
    if (is.data.frame(x) && ncol(x) == length(summaryoutput$modelnames)) {
      colnames(x) <- summaryoutput$modelnames
    }
    x
  })
  
  summaryoutput <- modify_if(summaryoutput, is.data.frame, ~{
    old_names <- colnames(.)
    # Find which columns match the pattern
    matches <- grepl("replist", old_names)
    # Rename only matching columns using the replacements vector
    old_names[matches] <- summaryoutput$modelnames[seq_len(sum(matches))]
    colnames(.) <- old_names
    .
  })
  
  effort_colors <- c(
    "0.2" = viridis(5)[1],
    "0.4" = viridis(5)[2],
    "0.6" = viridis(5)[3],
    "0.8" = viridis(5)[4],
    "1" = viridis(5)[5]
  )
  effort_fills <- c(
    "0.2" = viridis(5)[1],
    "0.4" = viridis(5)[2],
    "0.6" = viridis(5)[3],
    "0.8" = viridis(5)[4],
    "1" = "transparent"
  )
  
  # --------- OFL and OFL SD --------------------------
  # ℹ Define the lookup table for filtering upfront
  OFL_YEAR_FILTER <- data.frame(
    species = c("Longnose skate", "Pacific ocean perch", "Petrale sole", 
                "Sablefish", "Shortspine thornyhead", "Yellowtail rockfish"),
    Yr = c(2019, 2017, 2023, 2025, 2023, 2025)
  )
  
  # 1. Function to process the raw data and add species/effort columns (CORRECTED)
  process_quants <- function(data_table, value_name) {
    
    # Remove the problematic first_col/last_col search
    # first_col <- names(data_table)[grepl("skate", names(data_table))][1]
    # last_col <- names(data_table)[grepl("rockfish", names(data_table))][length(names(data_table))]
    
    data_table |>
      dplyr::filter(grepl("OFL", Label)) |>
      tidyr::pivot_longer(
        # FIX: Use regex to safely select all model columns. 
        # Matches columns that contain '_number_' (e.g., '_0.2_').
        cols = matches("_[0-9.]+_"), 
        names_to = "model", 
        values_to = value_name
      ) |>
      filter(!is.na(!!sym(value_name))) |>
      # Use mutate to create species and effort columns efficiently
      mutate(
        species = stringr::str_replace(model, "(_[0-9.]+.*)$", "") |> 
          stringr::str_replace_all("_", " ") |> stringr::str_trim(),
        effort = purrr::map_chr(stringr::str_split(model, "_"), ~ .x[length(.x) - 1])
      )
  }
  
  # 2. Process both OFL and OFL_SD tables
  ofl_base <- process_quants(summaryoutput$quants, "OFL")
  ofl_sd_base <- process_quants(summaryoutput$quantsSD, "OFL_SD") |> 
    select(Yr, model, OFL_SD)
  
  # 3. Join, Filter by Year, and Final Pivot Longer (All in one pipe)
  ofl_all_filtered <- ofl_base |> 
    full_join(ofl_sd_base, by = c("Yr", "model")) |>
    # Apply the species/year filter using a semi-join (much cleaner than multiple ORs)
    semi_join(OFL_YEAR_FILTER, by = c("species", "Yr")) |>
    select(model, species, effort, Yr, OFL, OFL_SD) |>
    tidyr::pivot_longer(cols = OFL:OFL_SD, names_to = "metric", values_to = "value")
  
  # Define the standard ggplot theme settings (reduces repetition in the plot function)
  OFL_PLOT_THEME <- theme(
    strip.text = element_text(size = 12, face = "bold"), 
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    plot.background = element_rect(fill = "transparent", colour = NA),
    panel.background = element_rect(fill = "transparent", colour = NA),
    legend.background = element_rect(fill = "transparent", colour = NA)
  )
  
  # Function to calculate mean/SE and generate the plot for a single metric
  plot_metric <- function(data, plot_metric_name, y_axis_label) {
    data |>
      filter(metric == plot_metric_name) |>
      # Convert and group/summarise in the same pipe
      mutate(clean_value = as.numeric(value)) |>
      group_by(species, effort) |>
      summarise(
        valid_n = sum(!is.na(clean_value)),
        value = mean(clean_value, na.rm = TRUE),
        # Calculate Standard Error (SE) using the valid count
        se = if (valid_n >= 2) {
          sd(clean_value, na.rm = TRUE) / sqrt(valid_n)
        } else {
          0
        },
        .groups = 'drop' 
      ) |>
      ggplot(aes(x = effort, y = value, group = species)) +
      geom_point(size = 2) +
      geom_line(linewidth = 1) +
      geom_errorbar(aes(ymin = value - se, ymax = value + se), width = 0.05) +
      labs(x = "Effort Level", y = y_axis_label) +
      theme_bw() +
      facet_wrap(~species, scales = "free_y", nrow = 1) +
      OFL_PLOT_THEME
  }
  
  # 3. Execute the plotting function for both metrics
  p <- plot_metric(ofl_all_filtered, "OFL", "OFL")
  p_sd <- plot_metric(ofl_all_filtered, "OFL_SD", "OFL Standard Deviation")
  
  # 4. Combine plots using patchwork
  combined <- p / p_sd + 
    plot_annotation(title = "OFL and OFL Standard Deviation (SE)") &
    labs(x = "Effort Level")
  combined
  
  plots$ofl_sd <- combined
  
  ggsave(
    filename = file.path(plot_save_dir, "ofl_sd.png"),
    plot = combined,
    width = 1130, # Set width in pixels
    height = 505, # Set height in pixels
    units = "px", # Specify units as pixels
    dpi = 100, # Use a standard DPI when using pixel dimensions
    bg = "transparent"
  )
  
  # --------- Spawning Biomass --------------------------
  if (any(subplots == 1)) {
    # All units are biomass so don't need to worry about anything in numbers
    # but would to use this plot for r4ss
    dat <- summaryoutput[["SpawnBio"]]
    dat_lower <- summaryoutput[["SpawnBioLower"]]
    dat_upper <- summaryoutput[["SpawnBioUpper"]]
    # Convert to long
    dat_long <- melt_model_table(dat, "value")
    dat_long$lower <- melt_model_table(dat_lower, "lower")$lower
    dat_long$upper <- melt_model_table(dat_upper, "upper")$upper
    dat_long$model <- factor(dat_long$model, labels=model_names)
    # Get model index of endyrs to remove any years after endyr (any forecast yrs)
    dat_long <- left_join(dat_long, species_effort_table, by = "model") |>
      group_by(model) |>
      mutate(model_index = cur_group_id()) |>
      ungroup()
    dat_long$endyr <- summaryoutput$endyrs[dat_long$model_index]
    dat_long <- dat_long |>
      filter(Yr <= endyr) |>
      filter(!is.na(value))
    dat_long_plot <- dplyr::filter(dat_long, Yr > min(Yr) & Yr > sort(unique(Yr))[2])
    
    # Summarize across replicates for each species and effort
    dat_summ <- dat_long_plot |>
      mutate(species = gsub("_"," ", species)) |>
      group_by(Yr, species, effort) |>
      summarize(
        mean_val = mean(value, na.rm = TRUE),
        se_val = sd(value, na.rm = TRUE) / sqrt(sum(!is.na(value))),
        mean_lower = mean(lower, na.rm = TRUE), # should this be the value used for the ribbon?
        mean_upper = mean(upper, na.rm = TRUE) # should this be the value used for the ribbon?
      ) |>
      ungroup()
    
    p <- ggplot(dat_summ, aes(x=Yr, y=mean_val, color=factor(effort), group=effort, fill=factor(effort))) +
      geom_ribbon(aes(ymin=mean_val-se_val, ymax=mean_val+se_val), alpha=0.2, color=NA) +
      geom_line(linewidth = 1) +
      labs(x="Year", y="Spawning biomass (t)", color="Effort", fill="Effort") +
      theme_bw() +
      facet_wrap(~species, scales="free_y") + 
      scale_color_manual(values = effort_colors, name = "Model/Effort") +
      scale_fill_manual(values = effort_fills, name = "Model/Effort") + 
      ggtitle("Spawning Biomass Comparison") +
      scale_x_continuous(breaks = seq(1875,2025, by = 20)) +
      theme(strip.text = element_text(size = 12, face = "bold"), 
            panel.grid.minor = element_blank(),
            axis.title.x = element_text(face = "bold"),
            axis.title.y = element_text(face = "bold"),
            legend.title = element_text(face = "bold"),
            plot.background = element_rect(fill = "transparent", colour = NA),
            panel.background = element_rect(fill = "transparent", colour = NA),
            legend.background = element_rect(fill = "transparent", colour = NA))
    
    plots$spawning_biomass <- p
    
    ggsave(
      filename = file.path(plot_save_dir, "spawning_biomass.png"),
      plot = p,
      width = 1130, # Set width in pixels
      height = 505, # Set height in pixels
      units = "px", # Specify units as pixels
      dpi = 100, # Use a standard DPI when using pixel dimensions
      bg = "transparent"
    )
    
    # Model uncertainty plot
    dat_end <- dat_summ %>%
      group_by(species, effort) %>%
      filter(Yr == max(Yr)) %>%
      ungroup()
    p2 <- ggplot(dat_end, aes(x=factor(effort), y=mean_val, color=factor(effort), fill=factor(effort))) +
      geom_point(size=3, position=position_dodge(width=0.5)) +
      geom_errorbar(aes(ymin=mean_lower, ymax=mean_upper), width=0.2, position=position_dodge(width=0.5)) +
      labs(x="Effort", y="Spawning biomass (t)", color="Effort", fill="Effort") +
      theme_bw() +
      facet_wrap(~species, scales="free_y") +
      scale_color_manual(values = effort_colors, name = "Model/Effort") +
      scale_fill_manual(values = effort_fills, name = "Model/Effort") +
      ggtitle("Uncertainty in End Year Spawning Biomass") +
      scale_y_continuous(labels = function(x) format(x, trim = TRUE, scientific = FALSE)) +
      theme(strip.text = element_text(size = 12, face = "bold"), 
            panel.grid.minor = element_blank(),
            axis.title.x = element_text(face = "bold"),
            axis.title.y = element_text(face = "bold"),
            legend.title = element_text(face = "bold"),
            plot.background = element_rect(fill = "transparent", colour = NA),
            panel.background = element_rect(fill = "transparent", colour = NA),
            legend.background = element_rect(fill = "transparent", colour = NA))
    
    plots$spawning_biomass_uncertainty <- p2
    
    ggsave(
      filename = file.path(plot_save_dir, "spawning_biomass_uncertainty.png"),
      plot = p2,
      width = 1130, # Set width in pixels
      height = 505, # Set height in pixels
      units = "px", # Specify units as pixels
      dpi = 100, # Use a standard DPI when using pixel dimensions
      bg = "transparent"
    )
  }
  
  # --------- Summary Biomass --------------------------
  if (any(subplots == 2)) {
    dat <- summaryoutput[["SmryBio"]]
    dat_lower <- summaryoutput[["SmryBioLower"]]
    dat_upper <- summaryoutput[["SmryBioUpper"]]
    # Convert to long
    dat_long <- melt_model_table(dat, "value")
    dat_long$lower <- melt_model_table(dat_lower, "lower")$lower
    dat_long$upper <- melt_model_table(dat_upper, "upper")$upper
    dat_long$model <- factor(dat_long$model, labels=model_names)
    dat_long <- left_join(dat_long, species_effort_table, by = "model") |>
      group_by(model) |>
      mutate(model_index = cur_group_id()) |>
      ungroup()
    dat_long$endyr <- summaryoutput$endyrs[dat_long$model_index]
    dat_long <- dat_long |>
      filter(Yr <= endyr)
    dat_long_plot <- dplyr::filter(dat_long, Yr > min(Yr) & Yr > sort(unique(Yr))[2])
    
    # Summarize across replicates for each species and effort
    dat_summ <- dat_long_plot |>
      group_by(Yr, species, effort) |>
      summarize(
        mean_val = mean(value, na.rm = TRUE),
        se_val = sd(value, na.rm = TRUE) / sqrt(sum(!is.na(value))),
        mean_lower = mean(lower, na.rm = TRUE),
        mean_upper = mean(upper, na.rm = TRUE)
      ) |>
      ungroup()
    
    p <- ggplot(dat_summ, aes(x=Yr, y=mean_val, color=factor(effort), group=effort, fill=factor(effort))) +
      geom_line(linewidth = 1) +
      labs(x="Year", y="Summary biomass (t)", color="Effort", fill="Effort") +
      theme_bw() +
      facet_wrap(~species, scales="free_y") + 
      geom_ribbon(aes(ymin=mean_val-se_val, ymax=mean_val+se_val), alpha=0.2, color=NA) +
      scale_color_manual(values = effort_colors, name = "Model/Effort") +
      scale_fill_manual(values = effort_fills, name = "Model/Effort") +
      ggtitle("Summary Biomass Comparison") +
      scale_x_continuous(breaks = seq(1875,2025, by = 20)) +
      scale_y_continuous(labels = function(x) format(x, trim = TRUE, scientific = FALSE)) +
      theme(strip.text = element_text(size = 12, face = "bold"), 
            panel.grid.minor = element_blank(),
            axis.title.x = element_text(face = "bold"),
            axis.title.y = element_text(face = "bold"),
            legend.title = element_text(face = "bold"),
            plot.background = element_rect(fill = "transparent", colour = NA),
            panel.background = element_rect(fill = "transparent", colour = NA),
            legend.background = element_rect(fill = "transparent", colour = NA))
    
    plots$summary_biomass <- p
    
    ggsave(
      filename = file.path(plot_save_dir, "summary_biomass.png"),
      plot = p,
      width = 1130, # Set width in pixels
      height = 505, # Set height in pixels
      units = "px", # Specify units as pixels
      dpi = 100, # Use a standard DPI when using pixel dimensions
      bg = "transparent"
    )
    
    # Model uncertainty plot
    dat_end <- dat_summ %>%
      group_by(species, effort) %>%
      filter(Yr == max(Yr)) %>%
      ungroup()
    p2 <- ggplot(dat_end, aes(x=factor(effort), y=mean_val, color=factor(effort), fill=factor(effort))) +
      geom_point(size=3, position=position_dodge(width=0.5)) +
      geom_errorbar(aes(ymin=mean_lower, ymax=mean_upper), width=0.2, position=position_dodge(width=0.5)) +
      labs(x="Effort", y="Summary biomass (t)", color="Effort", fill="Effort") +
      theme_bw() +
      facet_wrap(~species, scales="free_y") +
      scale_color_manual(values = effort_colors, name = "Model/Effort") +
      scale_fill_manual(values = effort_fills, name = "Model/Effort") +
      ggtitle("Uncertainty in End Year Summary Biomass") +
      scale_y_continuous(labels = function(x) format(x, trim = TRUE, scientific = FALSE)) +
      theme(strip.text = element_text(size = 12, face = "bold"), 
            panel.grid.minor = element_blank(),
            axis.title.x = element_text(face = "bold"),
            axis.title.y = element_text(face = "bold"),
            legend.title = element_text(face = "bold"),
            plot.background = element_rect(fill = "transparent", colour = NA),
            panel.background = element_rect(fill = "transparent", colour = NA),
            legend.background = element_rect(fill = "transparent", colour = NA))
    
    
    plots$summary_biomass_uncertainty <- p2
    
    ggsave(
      filename = file.path(plot_save_dir, "summary_biomass_uncertainty.png"),
      plot = p2,
      width = 1130, # Set width in pixels
      height = 505, # Set height in pixels
      units = "px", # Specify units as pixels
      dpi = 100, # Use a standard DPI when using pixel dimensions
      bg = "transparent"
    )
  }
  
  # --------- Bratio --------------------------
  if (any(subplots == 3)) {
    dat <- summaryoutput[["Bratio"]]
    dat_lower <- summaryoutput[["BratioLower"]]
    dat_upper <- summaryoutput[["BratioUpper"]]
    # Convert to long
    dat_long <- melt_model_table(dat, "value")
    dat_long$lower <- melt_model_table(dat_lower, "lower")$lower
    dat_long$upper <- melt_model_table(dat_upper, "upper")$upper
    dat_long$model <- factor(dat_long$model, labels=model_names)
    dat_long <- left_join(dat_long, species_effort_table, by = "model") |>
      group_by(model) |>
      mutate(model_index = cur_group_id()) |>
      ungroup()
    dat_long$endyr <- summaryoutput$endyrs[dat_long$model_index]
    dat_long <- dat_long |>
      filter(Yr <= endyr)
    dat_long_plot <- dplyr::filter(dat_long, Yr > min(Yr) & Yr > sort(unique(Yr))[2])
    
    # Summarize across replicates for each species and effort
    dat_summ <- dat_long_plot |>
      group_by(Yr, species, effort) |>
      summarize(
        mean_val = mean(value, na.rm = TRUE),
        se_val = sd(value, na.rm = TRUE) / sqrt(sum(!is.na(value))),
        mean_lower = mean(lower, na.rm = TRUE), # should this be the value used for the ribbon?
        mean_upper = mean(upper, na.rm = TRUE) # should this be the value used for the ribbon?
      ) |>
      ungroup()
    
    p <- ggplot(dat_summ, aes(x=Yr, y=mean_val, color=factor(effort), group=effort, fill=factor(effort))) +
      geom_line(linewidth = 1) +
      labs(x="Year", y="Fraction of unfished", color="Effort", fill="Effort") +
      geom_line(linewidth = 1) +
      theme_bw() +
      facet_wrap(~species, scales="free_y") + 
      geom_ribbon(aes(ymin=mean_val-se_val, ymax=mean_val+se_val), alpha=0.2, color=NA) +
      scale_color_manual(values = effort_colors, name = "Model/Effort") +
      scale_fill_manual(values = effort_fills, name = "Model/Effort") +
      ggtitle("Fraction of Unfished Comparison") +
      scale_x_continuous(breaks = seq(1875,2025, by = 20)) +
      scale_y_continuous(labels = function(x) format(x, trim = TRUE, scientific = FALSE)) +
      theme(strip.text = element_text(size = 12, face = "bold"), 
            panel.grid.minor = element_blank(),
            axis.title.x = element_text(face = "bold"),
            axis.title.y = element_text(face = "bold"),
            legend.title = element_text(face = "bold"),
            plot.background = element_rect(fill = "transparent", colour = NA),
            panel.background = element_rect(fill = "transparent", colour = NA),
            legend.background = element_rect(fill = "transparent", colour = NA))
    
    plots$bratio <- p
    
    ggsave(
      filename = file.path(plot_save_dir, "fraction_unfished.png"),
      plot = p,
      width = 1130, # Set width in pixels
      height = 505, # Set height in pixels
      units = "px", # Specify units as pixels
      dpi = 100, # Use a standard DPI when using pixel dimensions
      bg = "transparent"
    )
    
    # Model uncertainty plot
    dat_end <- dat_summ %>%
      group_by(species, effort) %>%
      filter(Yr == max(Yr)) %>%
      ungroup()
    p2 <- ggplot(dat_end, aes(x=factor(effort), y=mean_val, color=factor(effort), fill=factor(effort))) +
      geom_point(size=3, position=position_dodge(width=0.5)) +
      geom_errorbar(aes(ymin=mean_lower, ymax=mean_upper), width=0.2, position=position_dodge(width=0.5)) +
      labs(x="Effort", y="Fraction of unfished", color="Effort", fill="Effort") +
      theme_bw() +
      facet_wrap(~species, scales="free_y") +
      scale_color_manual(values = effort_colors, name = "Model/Effort") +
      scale_fill_manual(values = effort_fills, name = "Model/Effort") +
      ggtitle("Uncertainty in End Year Fraction of Unfished") +
      scale_y_continuous(labels = function(x) format(x, trim = TRUE, scientific = FALSE)) +
      theme(strip.text = element_text(size = 12, face = "bold"), 
            panel.grid.minor = element_blank(),
            axis.title.x = element_text(face = "bold"),
            axis.title.y = element_text(face = "bold"),
            legend.title = element_text(face = "bold"),
            plot.background = element_rect(fill = "transparent", colour = NA),
            panel.background = element_rect(fill = "transparent", colour = NA),
            legend.background = element_rect(fill = "transparent", colour = NA))
    
    
    plots$bratio_uncertainty <- p2
    
    ggsave(
      filename = file.path(plot_save_dir, "fraction_unfished_uncertainty.png"),
      plot = p2,
      width = 1130, # Set width in pixels
      height = 505, # Set height in pixels
      units = "px", # Specify units as pixels
      dpi = 100, # Use a standard DPI when using pixel dimensions
      bg = "transparent"
    )
  }
  
  # --------- Recruitment --------------------------
  if (any(subplots == 4)) {
    dat <- summaryoutput[["recruits"]]
    dat_lower <- summaryoutput[["recruitsLower"]]
    dat_upper <- summaryoutput[["recruitsUpper"]]
    # Convert to long
    dat_long <- melt_model_table(dat, "value")
    dat_long$lower <- melt_model_table(dat_lower, "lower")$lower
    dat_long$upper <- melt_model_table(dat_upper, "upper")$upper
    dat_long$model <- factor(dat_long$model, labels=model_names)
    dat_long <- left_join(dat_long, species_effort_table, by = "model") |>
      group_by(model) |>
      mutate(model_index = cur_group_id()) |>
      ungroup()
    dat_long$endyr <- summaryoutput$endyrs[dat_long$model_index]
    dat_long <- dat_long |>
      filter(Yr <= endyr)
    dat_long_plot <- dplyr::filter(dat_long, Yr > min(Yr) & Yr > sort(unique(Yr))[2])
    
    # Adjust units if needed
    ylab <- "Age-0 recruits (1,000s)"
    yunits <- 1
    maxrec <- max(dat_long_plot$value, na.rm = TRUE)
    if (maxrec > 1e3 && maxrec < 1e6) {
      yunits <- 1e3
      dat_long_plot$value <- dat_long_plot$value / yunits
      dat_long_plot$lower <- dat_long_plot$lower / yunits
      dat_long_plot$upper <- dat_long_plot$upper / yunits
      ylab <- gsub("1,000s", "millions", ylab)
    }
    
    # Summarize across replicates for each species and effort
    dat_summ <- dat_long_plot |>
      group_by(Yr, species, effort) |>
      summarize(
        mean_val = mean(value, na.rm = TRUE),
        se_val = sd(value, na.rm = TRUE) / sqrt(sum(!is.na(value))),
        mean_lower = mean(lower, na.rm = TRUE), # should this be the value used for the ribbon?
        mean_upper = mean(upper, na.rm = TRUE) # should this be the value used for the ribbon?
      ) |>
      ungroup()
    
    # Filter 1990 onwards just for plot of recruits
    dat_summ_filt_years <- dat_summ |>
      filter(Yr >=1990)
    
    p <- ggplot(dat_summ_filt_years, aes(x=Yr, y=mean_val, color=factor(effort), group=effort, fill=factor(effort))) +
      # geom_line() +
      geom_point() +
      labs(x="Year", y=ylab, color="Effort", fill="Effort") +
      theme_bw() +
      facet_wrap(~species, scales="free_y") + 
      geom_errorbar(aes(ymin=mean_val-se_val, ymax=mean_val+se_val), width = 0.2, alpha = 0.5) +
      scale_color_manual(values = effort_colors, name = "Model/Effort") +
      scale_fill_manual(values = effort_fills, name = "Model/Effort") +
      ggtitle("Recruits Comparison") +
      scale_x_continuous(breaks = seq(1875,2025, by = 20)) +
      theme(strip.text = element_text(size = 12, face = "bold"), 
            panel.grid.minor = element_blank(),
            axis.title.x = element_text(face = "bold"),
            axis.title.y = element_text(face = "bold"),
            legend.title = element_text(face = "bold"),
            plot.background = element_rect(fill = "transparent", colour = NA),
            panel.background = element_rect(fill = "transparent", colour = NA),
            legend.background = element_rect(fill = "transparent", colour = NA))
    
    plots$recruits <- p
    
    ggsave(
      filename = file.path(plot_save_dir, "Recruits.png"),
      plot = p,
      width = 1130, # Set width in pixels
      height = 505, # Set height in pixels
      units = "px", # Specify units as pixels
      dpi = 100, # Use a standard DPI when using pixel dimensions
      bg = "transparent"
    )
    
    # Model uncertainty plot
    dat_end <- dat_summ %>%
      group_by(species, effort) %>%
      filter(Yr == max(Yr)) %>%
      ungroup()
    p2 <- ggplot(dat_end, aes(x=factor(effort), y=mean_val, color=factor(effort), fill=factor(effort))) +
      geom_point(size=3, position=position_dodge(width=0.5)) +
      geom_errorbar(aes(ymin=mean_lower, ymax=mean_upper), width=0.2, position=position_dodge(width=0.5)) +
      labs(x="Effort", y=ylab, color="Effort", fill="Effort") +
      theme_bw() +
      facet_wrap(~species, scales="free_y") +
      scale_color_manual(values = effort_colors, name = "Model/Effort") +
      scale_fill_manual(values = effort_fills, name = "Model/Effort") +
      ggtitle("Uncertainty in End Year Recruits") +
      scale_y_continuous(labels = function(x) format(x, trim = TRUE, scientific = FALSE)) +
      theme(strip.text = element_text(size = 12, face = "bold"), 
            panel.grid.minor = element_blank(),
            axis.title.x = element_text(face = "bold"),
            axis.title.y = element_text(face = "bold"),
            legend.title = element_text(face = "bold"),
            plot.background = element_rect(fill = "transparent", colour = NA),
            panel.background = element_rect(fill = "transparent", colour = NA),
            legend.background = element_rect(fill = "transparent", colour = NA))
    
    
    plots$recruits_uncertainty <- p2
    
    ggsave(
      filename = file.path(plot_save_dir, "recruits_uncertainty.png"),
      plot = p2,
      width = 1130, # Set width in pixels
      height = 505, # Set height in pixels
      units = "px", # Specify units as pixels
      dpi = 100, # Use a standard DPI when using pixel dimensions
      bg = "transparent"
    )
  }
  
  # --------- Recruitment Deviations --------------------------
  if (any(subplots == 5)) {
    dat <- summaryoutput[["recdevs"]] |> select(where(is.numeric)) |> select(where(~ any(abs(.) != 0, na.rm = TRUE)))
    dat_lower <- summaryoutput[["recdevsLower"]] |> select(where(is.numeric)) |> select(where(~ any(abs(.) != 0, na.rm = TRUE)))
    dat_upper <- summaryoutput[["recdevsUpper"]] |> select(where(is.numeric)) |> select(where(~ any(abs(.) != 0, na.rm = TRUE)))
    # Convert to long
    dat_long <- melt_model_table(dat, "value")
    dat_long$lower <- melt_model_table(dat_lower, "lower")$lower
    dat_long$upper <- melt_model_table(dat_upper, "upper")$upper
    present_models <- intersect(model_names, unique(dat_long$model))
    dat_long$model <- factor(dat_long$model, levels = present_models)
    dat_long <- left_join(dat_long, species_effort_table, by = "model") |>
      filter(!is.na(value)) |>
      group_by(model) |>
      mutate(model_index = cur_group_id()) |>
      ungroup()
    dat_long$endyr <- summaryoutput$endyrs[dat_long$model_index]
    dat_long <- dat_long |>
      filter(Yr <= endyr)
    dat_long_plot <- dplyr::filter(dat_long, Yr > min(Yr) & Yr > sort(unique(Yr))[2])
    
    # Summarize across replicates for each species and effort
    dat_summ <- dat_long_plot |>
      group_by(Yr, species, effort) |>
      summarize(
        mean_val = mean(value, na.rm = TRUE),
        se_val = sd(value, na.rm = TRUE) / sqrt(sum(!is.na(value))),
        mean_lower = mean(lower, na.rm = TRUE), # should this be the value used for the ribbon?
        mean_upper = mean(upper, na.rm = TRUE) # should this be the value used for the ribbon?
      ) |>
      ungroup()
    
    # Filter 1990 onwards just for plot of recruits
    dat_summ_filt_years <- dat_summ |>
      filter(Yr >=1990)
    
    p <- ggplot(dat_summ_filt_years, aes(x=Yr, y=mean_val, color=factor(effort), group=effort, fill=factor(effort))) +
      # geom_line() +
      geom_errorbar(aes(ymin=mean_lower, ymax=mean_upper), width = 0.2, alpha = 0.2)+
      geom_point() +
      labs(x="Year", y="Recruitment deviations", color="Effort", fill="Effort") +
      theme_bw() +
      facet_wrap(~species, scales="free_y") +
      scale_color_manual(values = effort_colors, name = "Model/Effort") +
      scale_fill_manual(values = effort_fills, name = "Model/Effort") +
      ggtitle("Recruitment Deviations Comparison") +
      scale_x_continuous(breaks = seq(1875,2025, by = 20)) +
      scale_y_continuous(labels = function(x) format(x, trim = TRUE, scientific = FALSE)) +
      theme(strip.text = element_text(size = 12, face = "bold"), 
            panel.grid.minor = element_blank(),
            axis.title.x = element_text(face = "bold"),
            axis.title.y = element_text(face = "bold"),
            legend.title = element_text(face = "bold"),
            plot.background = element_rect(fill = "transparent", colour = NA),
            panel.background = element_rect(fill = "transparent", colour = NA),
            legend.background = element_rect(fill = "transparent", colour = NA))
    
    plots$bratio <- p
    
    ggsave(
      filename = file.path(plot_save_dir, "recdevs.png"),
      plot = p,
      width = 1130, # Set width in pixels
      height = 505, # Set height in pixels
      units = "px", # Specify units as pixels
      dpi = 100, # Use a standard DPI when using pixel dimensions
      bg = "transparent"
    )
    
    dat_end <- dat_summ %>%
      group_by(species, effort) %>%
      filter(Yr == max(Yr)) %>%
      ungroup()
    p2 <- ggplot(dat_end, aes(x=factor(effort), y=mean_val, color=factor(effort), fill=factor(effort))) +
      geom_point(size=3, position=position_dodge(width=0.5)) +
      geom_errorbar(aes(ymin=mean_lower, ymax=mean_upper), width=0.2, position=position_dodge(width=0.5)) +
      labs(x="Effort", y="Recruitment deviations", color="Effort", fill="Effort") +
      theme_bw() +
      facet_wrap(~species, scales="free_y") +
      scale_color_manual(values = effort_colors, name = "Model/Effort") +
      scale_fill_manual(values = effort_fills, name = "Model/Effort") +
      ggtitle("Uncertainty in End Year Recruitment Deviations") +
      scale_y_continuous(labels = function(x) format(x, trim = TRUE, scientific = FALSE)) +
      theme(strip.text = element_text(size = 12, face = "bold"), 
            panel.grid.minor = element_blank(),
            axis.title.x = element_text(face = "bold"),
            axis.title.y = element_text(face = "bold"),
            legend.title = element_text(face = "bold"),
            plot.background = element_rect(fill = "transparent", colour = NA),
            panel.background = element_rect(fill = "transparent", colour = NA),
            legend.background = element_rect(fill = "transparent", colour = NA))
    
    
    plots$recdevs_uncertainty <- p2
    
    ggsave(
      filename = file.path(plot_save_dir, "recdevs_uncertainty.png"),
      plot = p2,
      width = 1130, # Set width in pixels
      height = 505, # Set height in pixels
      units = "px", # Specify units as pixels
      dpi = 100, # Use a standard DPI when using pixel dimensions
      bg = "transparent"
    )
  }
  
  
  # Length composition Ratio plot
  if (any(subplots == 6)) {
    names(all_output) <- legendlabels
    len_comps <- purrr::map(all_output, "Length_Comp_Fit_Summary")
    len_comps <- purrr::map(len_comps, ~.x[c("Fleet", "Fleet_name", "N", "mean_Nsamp_in", "mean_Nsamp_adj")])
    df_lencomps <- map_dfr(len_comps, ~as_tibble(.x), .id = "source")
    df_sep <- df_lencomps |>
      tidyr::extract(
        source,
        into = c("species", "effort", "replicate"),
        regex = "(.+)_(.+)_(.+)"
      ) |>
      mutate(
        effort = as.numeric(effort),
        replicate = as.integer(replicate)
      )
    df_wcgbts <- df_sep |>
      filter(Fleet_name %in% c("5_WCGBT", "WCGBT", "NWFSCcombo", "WCGBTS", "NWCBO")) |>
      group_by(species, effort, replicate) |>
      mutate(N_x_mean_Nsamp_adj_WCGBTS = N * mean_Nsamp_adj,
             Fleet_name = "WCGBTS") |>
      select(-Fleet, -N, -mean_Nsamp_in, -mean_Nsamp_adj, -Fleet_name)
    
    df_else <- df_sep |>
      group_by(species, effort, replicate) |>
      mutate(N_x_mean_Nsamp_adj = N * mean_Nsamp_adj) |>
      summarize(
        Fleet_name = "all fleets",
        N_x_mean_Nsamp_adj_all = sum(N_x_mean_Nsamp_adj, na.rm = TRUE),
      )|>
      select(-Fleet_name)
    
    df_combined <- df_wcgbts |>
      inner_join(df_else, by = c("species", "effort", "replicate")) |>
      mutate(species = gsub("_"," ", species)) |>
      mutate(ratio = N_x_mean_Nsamp_adj_WCGBTS / N_x_mean_Nsamp_adj_all)
    
    df_summary <- df_combined |>
      group_by(species, effort) |>
      summarise(
        mean_ratio = mean(ratio, na.rm = TRUE),
        se = sd(ratio, na.rm = TRUE) / sqrt(n()),
        .groups = "drop"
      )
    
    p3 <- ggplot(df_combined, aes(x = as.factor(effort), y = ratio)) +
      geom_boxplot(width = 0.4, aes(fill = as.factor(effort))) +
      facet_wrap(~ species) +
      scale_fill_manual(values = c("#440154FF", "#3B528BFF", "#21908CFF", "#5DC863FF", "#FDE725FF")) +
      labs(x = "Effort", y = "Ratio of length composition of WCGBTS to all fleets", fill = "Effort") +
      theme_bw() +
      theme(strip.text = element_text(size = 12, face = "bold"), 
            panel.grid.minor = element_blank(),
            axis.title.x = element_text(face = "bold"),
            axis.title.y = element_text(face = "bold"),
            legend.title = element_text(face = "bold"),
            plot.background = element_rect(fill = "transparent", colour = NA),
            panel.background = element_rect(fill = "transparent", colour = NA),
            legend.background = element_rect(fill = "transparent", colour = NA))
    
    plots$length_comp_ratio <- p3
    
    ggsave(
      filename = file.path(plot_save_dir, "length_comp_ratio.png"),
      plot = p3,
      width = 1130, # Set width in pixels
      height = 505, # Set height in pixels
      units = "px", # Specify units as pixels
      dpi = 100, # Use a standard DPI when using pixel dimensions
      bg = "transparent"
    )
  }
  # Age comps ratio
  # need to go back code for the missing values
  if (any(subplots == 6)) {
    age_comps <- purrr::map(all_output, "Age_Comp_Fit_Summary")
    age_comps <- purrr::map(len_comps, ~.x[c("Fleet", "Fleet_name", "N", "mean_Nsamp_in", "mean_Nsamp_adj")])
    df_agecomps <- map_dfr(age_comps, ~as_tibble(.x), .id = "source")
    df_sep <- df_agecomps |>
      tidyr::extract(
        source,
        into = c("species", "effort", "replicate"),
        regex = "(.+)_(.+)_(.+)"
      ) |>
      mutate(
        effort = as.numeric(effort),
        replicate = as.integer(replicate)
      )
    
    df_wcgbts <- df_sep |>
      filter(Fleet_name %in% c("5_WCGBT", "WCGBT", "NWFSCcombo", "WCGBTS", "NWCBO")) |>
      group_by(species, effort, replicate) |>
      mutate(N_x_mean_Nsamp_adj_WCGBTS = N * mean_Nsamp_adj,
             Fleet_name = "WCGBTS") |>
      select(-Fleet, -N, -mean_Nsamp_in, -mean_Nsamp_adj, -Fleet_name)
    
    df_else <- df_sep |>
      group_by(species, effort, replicate) |>
      mutate(N_x_mean_Nsamp_adj = N * mean_Nsamp_adj) |>
      summarize(
        Fleet_name = "all fleets",
        N_x_mean_Nsamp_adj_all = sum(N_x_mean_Nsamp_adj, na.rm = TRUE),
      )|>
      select(-Fleet_name)
    
    df_combined <- df_wcgbts |>
      inner_join(df_else, by = c("species", "effort", "replicate")) |>
      mutate(species = gsub("_"," ", species)) |>
      mutate(ratio = N_x_mean_Nsamp_adj_WCGBTS / N_x_mean_Nsamp_adj_all)
    
    df_summary <- df_combined |>
      group_by(species, effort) |>
      summarise(
        mean_ratio = mean(ratio, na.rm = TRUE),
        se = sd(ratio, na.rm = TRUE) / sqrt(n()),
        .groups = "drop"
      )
    
    p4 <- ggplot(df_combined, aes(x = as.factor(effort), y = ratio)) +
      geom_boxplot(width = 0.4, aes(fill = as.factor(effort))) +
      facet_wrap(~ species) +
      scale_fill_manual(values = c("#440154FF", "#3B528BFF", "#21908CFF", "#5DC863FF", "#FDE725FF")) +
      labs(x = "Effort", y = "Ratio of age composition of WCGBTS to all fleets", fill = "Effort") +
      theme_bw() +
      theme(strip.text = element_text(size = 12, face = "bold"), 
            panel.grid.minor = element_blank(),
            axis.title.x = element_text(face = "bold"),
            axis.title.y = element_text(face = "bold"),
            legend.title = element_text(face = "bold"),
            plot.background = element_rect(fill = "transparent", colour = NA),
            panel.background = element_rect(fill = "transparent", colour = NA),
            legend.background = element_rect(fill = "transparent", colour = NA))
    
    plots$age_comp_ratio <- p4
    
    ggsave(
      filename = file.path(plot_save_dir, "age_comp_ratio.png"),
      plot = p4,
      width = 1130, # Set width in pixels
      height = 505, # Set height in pixels
      units = "px", # Specify units as pixels
      dpi = 100, # Use a standard DPI when using pixel dimensions
      bg = "transparent"
    )
  }
  
  return(plots)
}

# Length and weight dot plots
plot_composition_comparisons <- function(dir_list, fleet_lookup, plot_save_dir){
  inputs <- setNames(
    lapply(dir_list, r4ss::SS_read),
    basename(dir_list)
  )
  
  extract_model_info <- function(model_name) {
    # Split by "_" and find the last 2 are effort and replicate, rest is species
    parts <- str_split(model_name, "_", simplify = TRUE)
    n <- ncol(parts)
    list(
      model = model_name,
      species = paste(parts[1:(n-2)], collapse = "_"),
      effort = parts[n-1],
      replicate = parts[n]
    )
  }
  
  lencomps <- imap_dfr(
    inputs,
    ~ {
      info <- extract_model_info(.y)
      fleet_num <- fleet_lookup[[info$species]]
      lencomp_df <- .x$dat$lencomp |> filter(abs(fleet) == fleet_num)
      
      if(length(lencomp_df$year) > 0) {
        # Check for f# columns
        fm_cols <- grep("^[fm]\\d+$", names(lencomp_df), value = TRUE)
        if (length(fm_cols) > 0) {
          # Pivot longer, extract the number, sum across f/m for each number
          lencomp_df <- lencomp_df |>
            pivot_longer(cols = all_of(fm_cols), names_to = "fm_col", values_to = "fmval") |>
            mutate(l_col = paste0("l", gsub("^[fm]", "", fm_col))) |>
            group_by(across(-c(fm_col, fmval, l_col)), l_col) |>
            summarise(lval = sum(fmval, na.rm = TRUE), .groups = "drop") |>
            pivot_wider(names_from = l_col, values_from = lval) |>
            # reattach non-fm columns (if any)
            left_join(lencomp_df |> select(-all_of(fm_cols)), by = setdiff(names(lencomp_df), fm_cols))
        }
        
        # Continue as before, using the new l# columns if present
        lcols <- grep("^l\\d+$", names(lencomp_df), value = TRUE)
        lencomp_df |>
          select(year, all_of(lcols)) |>
          pivot_longer(cols = all_of(lcols), names_to = "length", values_to = "freq") |>
          mutate(
            length = gsub("^l", "", length),
            length = as.numeric(length),
            species = gsub("_", " ", info$species),
            effort = info$effort,
            replicate = info$replicate
          ) %>%
          group_by(species, effort, year) |>
          mutate(freq = freq / sum(freq)) |>
          ungroup()
      }
    }
    )
    
  if(length(lencomps$year) > 0) {
    length_comparison_plot <- lencomps |>
      filter(freq > 0) |>
      ggplot(aes(x = year, y = length, col = effort, size = freq)) +
      geom_point(position = position_dodge(0.9)) +
      facet_wrap(~species) +
      theme_bw() +
      labs(x="Year", y="Length (cm)", color="Effort") +
      guides(size = "none") +
      scale_color_manual(values = effort_colors, name = "Effort") +
      theme(strip.text = element_text(size = 12, face = "bold"), 
            panel.grid.minor = element_blank(),
            axis.title.x = element_text(face = "bold"),
            axis.title.y = element_text(face = "bold"),
            legend.title = element_text(face = "bold"),
            plot.background = element_rect(fill = "transparent", colour = NA),
            panel.background = element_rect(fill = "transparent", colour = NA),
            legend.background = element_rect(fill = "transparent", colour = NA))
    
    length_comparison_plot.1 <- lencomps |>
      filter(freq > 0) |>
      filter(species == "Sablefish") |>
      ggplot(aes(x = year, y = length, col = effort, size = freq)) +
      geom_point(position = position_dodge(0.9)) +
      theme_bw() +
      labs(x="Year", y="Length (cm)", color="Effort") +
      guides(size = "none", color = "none") +
      scale_color_manual(values = effort_colors, name = "Effort") +
      ggtitle("Sablefish") +
      theme(panel.grid.minor = element_blank(),
            axis.title.x = element_text(face = "bold"),
            axis.title.y = element_text(face = "bold"),
            title = element_text(face = "bold"),
            plot.title = element_text(vjust = -8),
            plot.background = element_rect(fill = "transparent", colour = NA),
            panel.background = element_rect(fill = "transparent", colour = NA),
            legend.background = element_rect(fill = "transparent", colour = NA))
      
    combined <- length_comparison_plot + length_comparison_plot.1 + plot_layout(widths = c(2,1))
    
    ggsave(
      filename = file.path(plot_save_dir, "length_comparisons.png"),
      plot = length_comparison_plot,
      width = 1130, # Set width in pixels
      height = 505, # Set height in pixels
      units = "px", # Specify units as pixels
      dpi = 100, # Use a standard DPI when using pixel dimensions
      bg = "transparent"
    )
    
    ggsave(
      filename = file.path(plot_save_dir, "length_comparisons_zoom.png"),
      plot = combined,
      width = 1130, # Set width in pixels
      height = 505, # Set height in pixels
      units = "px", # Specify units as pixels
      dpi = 100, # Use a standard DPI when using pixel dimensions
      bg = "transparent"
    )
  }
  
  agecomps <- imap_dfr(
    inputs,
    ~ {
      if(!is.null(.x$dat$agecomp)) {
        info <- extract_model_info(.y)
        fleet_num <- fleet_lookup[[info$species]]
        agecomp_df <- .x$dat$agecomp |> filter(abs(fleet) == fleet_num)
        
        # Check for f# or m# columns
        fm_cols <- grep("^[fm]\\d+$", names(agecomp_df), value = TRUE)
        if (length(fm_cols) > 0) {
          # Pivot longer, extract the number, sum across f/m for each number
          agecomp_df <- agecomp_df |>
            pivot_longer(cols = all_of(fm_cols), names_to = "fm_col", values_to = "fmval") |>
            mutate(a_col = paste0("a", gsub("^[fm]", "", fm_col))) |>
            group_by(across(-c(fm_col, fmval, a_col)), a_col) |>
            summarise(aval = sum(fmval, na.rm = TRUE), .groups = "drop") |>
            pivot_wider(names_from = a_col, values_from = aval) |>
            # reattach non-fm columns (if any)
            left_join(agecomp_df |> select(-all_of(fm_cols)), by = setdiff(names(agecomp_df), fm_cols))
        }
        acols <- grep("^a\\d+$", names(agecomp_df), value = TRUE)
        agecomp_df |>
          select(year, all_of(acols)) |>
          pivot_longer(cols = all_of(acols), names_to = "age", values_to = "freq") |>
          mutate(
            age = gsub("^a", "", age),
            age = as.numeric(age),
            species = gsub("_", " ", info$species),
            effort = info$effort,
            replicate = info$replicate
          ) |>
          group_by(species, effort, year) |>
          mutate(freq = freq / sum(freq)) |>
          ungroup()
      }
    }
  )
  
  age_comparison_plot <- agecomps |>
    filter(freq > 0) |>
    ggplot(aes(x = year, y = age, col = effort, size = freq)) +
    geom_point(position = position_dodge(0.5)) +
    facet_wrap(~species) +
    theme_bw() +
    labs(x="Year", y="Age", color="Effort")  +
    scale_color_manual(values = effort_colors, name = "Effort") +
    guides(size = "none") +
    theme(strip.text = element_text(size = 12, face = "bold"), 
          panel.grid.minor = element_blank(),
          axis.title.x = element_text(face = "bold"),
          axis.title.y = element_text(face = "bold"),
          legend.title = element_text(face = "bold"),
          plot.background = element_rect(fill = "transparent", colour = NA),
          panel.background = element_rect(fill = "transparent", colour = NA),
          legend.background = element_rect(fill = "transparent", colour = NA))
  
  age_comparison_plot.1 <- agecomps |>
    filter(freq > 0) |>
    filter(species == "Sablefish") |>
    ggplot(aes(x = year, y = age, col = effort, size = freq)) +
    geom_point(position = position_dodge(0.9)) +
    theme_bw() +
    labs(x="Year", y="Age", color="Effort") +
    guides(size = "none", color = "none") +
    scale_color_manual(values = effort_colors, name = "Effort") +
    ggtitle("Sablefish") +
    theme(panel.grid.minor = element_blank(),
          axis.title.x = element_text(face = "bold"),
          axis.title.y = element_text(face = "bold"),
          title = element_text(face = "bold"),
          plot.title = element_text(vjust = -8),
          plot.background = element_rect(fill = "transparent", colour = NA),
          panel.background = element_rect(fill = "transparent", colour = NA),
          legend.background = element_rect(fill = "transparent", colour = NA))
  
  combined <- age_comparison_plot + age_comparison_plot.1 + plot_layout(widths = c(2,1))
  
  ggsave(
    filename = file.path(plot_save_dir, "age_comparisons.png"),
    plot = age_comparison_plot,
    width = 1130, # Set width in pixels
    height = 505, # Set height in pixels
    units = "px", # Specify units as pixels
    dpi = 100, # Use a standard DPI when using pixel dimensions
    bg = "transparent"
  )
  
  
  ggsave(
    filename = file.path(plot_save_dir, "age_comparisons_zoom.png"),
    plot = combined,
    width = 1130, # Set width in pixels
    height = 505, # Set height in pixels
    units = "px", # Specify units as pixels
    dpi = 100, # Use a standard DPI when using pixel dimensions
    bg = "transparent"
  )
}

  #' Plot weight-length curves from wl_df (no raw points)
  #'
  #' @param wl_df Data frame output from run_model() (bound from run_model_efforts()).
  #'   Must contain columns: species, effort, iteration (optional), sex, A, B.
  #'   If it contains multiple species/efforts/iterations they will be faceted.
  #' @param dir Optional directory to save the plot. If NULL, prints to device.
  #' @param add_save_name Optional string prepended to the saved file name.
  #' @param facet_vars Which columns to facet by (any of c("species","effort","iteration")).
  #' @param height,width,dpi ggsave parameters.
  #'
  #' @return A ggplot object (invisibly if saved).
  #' @export
plot_weight_length <- function(
    wl_df,
    dir = plot_save_dir,
    height = 7,
    width = 9,
    dpi = 300
) {
  stopifnot(is.data.frame(wl_df))
  
  # Ensure a well-defined l_max per (species, effort) panel
  lmax_panel <- wl_df |>
    dplyr::mutate(effort = factor(effort, levels = c(1.0, 0.8, 0.6, 0.4, 0.2))) |>
    dplyr::group_by(species, effort) |>
    dplyr::summarize(l_max_panel = max(l_max, na.rm = TRUE), .groups = "drop")
  
  wl_df <- wl_df |>
    dplyr::mutate(effort = factor(effort, levels = c(1.0, 0.8, 0.6, 0.4, 0.2))) |>
    dplyr::left_join(lmax_panel, by = c("species", "effort"))
  
  # Per-iteration curves (needed to compute SE ribbon over predicted curves)
  iter_curve <- wl_df |>
    dplyr::group_by(species, effort, iteration, sex) |>
    dplyr::reframe(
      plot_length = seq(0, unique(l_max_panel), by = 1),
      plot_weight = A[1] * plot_length^(B[1])
    ) |>
    dplyr::ungroup()
  
  # Mean curve + SE ribbon at each length
  summary_curve <- iter_curve |>
    dplyr::group_by(species, effort, sex, plot_length) |>
    dplyr::summarize(
      n_iter = dplyr::n_distinct(iteration),
      mean_weight = mean(plot_weight, na.rm = TRUE),
      se_weight = stats::sd(plot_weight, na.rm = TRUE) / sqrt(n_iter),
      ymin = mean_weight - se_weight,
      ymax = mean_weight + se_weight,
      .groups = "drop"
    )
  
  colors <- c(F = "#414487FF", M = "#22A884FF")
  
  # Plot: facet ONLY by species; show effort as different colors
  p <- ggplot2::ggplot(
    summary_curve,
    ggplot2::aes(x = plot_length, y = mean_weight)
  ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = ymin, ymax = ymax, fill = effort),
      alpha = 0.20,
      color = NA
    ) +
    ggplot2::geom_line(
      ggplot2::aes(color = effort, linetype = sex),
      linewidth = 1.0
    ) +
    # NOTE: removed geom_text() entirely (this is what printed A and B)
    ggplot2::facet_wrap(~species) +
    ggplot2::scale_color_brewer(palette = "Dark2", name = "Effort") +
    ggplot2::scale_fill_brewer(palette = "Dark2", name = "Effort") +
    ggplot2::scale_linetype_discrete(name = "Sex") +
    ggplot2::labs(x = "Length (cm)", y = "Weight (kg)") +
    ggplot2::theme_bw()
  
  if (!is.null(dir)) {
    plotdir <- file.path(dir, "plots")
    if (!dir.exists(plotdir)) dir.create(plotdir, recursive = TRUE)
    
    plot_name <- file.path(
      plotdir,
      paste0(
        add_save_name,
        ifelse(is.null(add_save_name) || add_save_name == "", "", "_"),
        "weight_length_mean_se_ribbon.png"
      )
    )
    
    ggplot2::ggsave(plot_name, p, height = height, width = width, units = "in", dpi = dpi)
    return(invisible(p))
  }
  
  p
}
  
  
  # Compare Ms for those that are estimated
  # longnose skate M is estimated
  # PoP M is fixed
  # Petrale sole M is estimated
  # Sablefish M is estimated
  # shortspine thornyhead M fixed
  # yellowtail rockfish M is estimated
  
  # Compare growth curves for those that are estimated
  # shortspine thornyhead is the only growth curve not estimated - no ages to be able to estimate
  