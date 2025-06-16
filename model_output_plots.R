# Plotting functions
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
  
  effort_levels <- c("0.2", "0.4", "0.8", "1", "original model")
  effort_colors <- c(
    "0.2" = viridis(7)[1],
    "0.4" = viridis(7)[3],
    "0.8" = viridis(7)[5],
    "1" = viridis(7)[7],
    "original model" = "black"
  )
  effort_fills <- effort_colors
  effort_fills["original model"] <- "NA"
  
  effort_summary$effort <- factor(effort_summary$effort, levels = effort_levels)
  original_model_data$effort <- factor(original_model_data$effort, levels = effort_levels)
  
  # Plot with facets for species
  library(ggplot2)
  p <- ggplot() +
    geom_ribbon(
      data = effort_summary,
      aes(x = year, ymin = mean_obs - se_obs, ymax = mean_obs + se_obs, fill = effort, group = effort),
      alpha = 0.2
    ) +
    geom_line(
      data = effort_summary,
      aes(x = year, y = mean_obs, color = effort, group = effort)
    ) +
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
      y = "Index"
    ) +
    scale_color_manual(values = effort_colors, name = "Model/Effort") +
    scale_fill_manual(values = effort_fills, name = "Model/Effort")
  
  ggplot2::ggsave(
    filename = file.path(plot_save_dir, "effort_indices.png"),
    plot = p
  )
  
  list(data = all_indices, plot = p)
}



plot_comparisons_ggplot <- function(
  summaryoutput,
  subplots = c(1,2,3,4,5),
  models = "all",
  endyrvec = NULL,
  legendlabels = NULL,
  rescale = TRUE,
  show_equilibrium = TRUE,
  summarize_by_species_effort = TRUE,
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
      "0.2" = viridis(7)[1],
      "0.4" = viridis(7)[3],
      "0.8" = viridis(7)[5],
      "1" = viridis(7)[7]
    )
    effort_fills <- effort_colors
    
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
      dat_long <- left_join(dat_long, species_effort_table, by = "model")
      dat_long_plot <- dplyr::filter(dat_long, Yr > min(Yr) & Yr > sort(unique(Yr))[2])
      
      if (summarize_by_species_effort) {
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
          geom_line() +
          labs(x="Year", y="Spawning biomass (t)", color="Effort", fill="Effort") +
          theme_minimal() +
          facet_wrap(~species, scales="free_y") + 
          # geom_ribbon(aes(ymin=mean_lower, ymax=mean_upper), alpha=0.2, color=NA)
          geom_ribbon(aes(ymin=mean_val-se_val, ymax=mean_val+se_val), alpha=0.2, color=NA) +
          scale_color_manual(values = effort_colors, name = "Model/Effort") +
          scale_fill_manual(values = effort_fills, name = "Model/Effort") + 
          ggtitle("Spawning Biomass Comparison")
      } else {
        p <- ggplot(dat_long_plot, aes(x=Yr, y=value, color=model, fill=model, group=model)) +
          geom_line() +
          labs(x="Year", y="Spawning biomass (t)", color="Model", fill="Model") +
          theme_minimal() + 
          geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.2, color=NA) +
          scale_color_manual(values = effort_colors, name = "Model/Effort") +
          scale_fill_manual(values = effort_fills, name = "Model/Effort") + 
          ggtitle("Spawning Biomass Comparison")
        }
      plots$spawning_biomass <- p
      
      ggsave(
        filename = file.path(plot_save_dir, "spawning_biomass.png"),
        plot = p
        # , width = png_width, height = png_height, dpi = png_dpi
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
      dat_long <- left_join(dat_long, species_effort_table, by = "model")
      dat_long_plot <- dplyr::filter(dat_long, Yr > min(Yr) & Yr > sort(unique(Yr))[2])
      
      if (summarize_by_species_effort) {
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
          geom_line() +
          labs(x="Year", y="Summary biomass (t)", color="Effort", fill="Effort") +
          theme_minimal() +
          facet_wrap(~species, scales="free_y") + 
          geom_ribbon(aes(ymin=mean_val-se_val, ymax=mean_val+se_val), alpha=0.2, color=NA) +
          scale_color_manual(values = effort_colors, name = "Model/Effort") +
          scale_fill_manual(values = effort_fills, name = "Model/Effort") +
          ggtitle("Summary Biomass Comparison")
      } else {
        p <- ggplot(dat_long_plot, aes(x=Yr, y=value, color=model, fill=model, group=model)) +
          geom_line() +
          labs(x="Year", y="Summary biomass (t)", color="Model", fill="Model") +
          theme_minimal() + 
          geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.2, color=NA) +
          scale_color_manual(values = effort_colors, name = "Model/Effort") +
          scale_fill_manual(values = effort_fills, name = "Model/Effort") +
          ggtitle("Summary Biomass Comparison")
      }
      plots$summary_biomass <- p
      
      ggsave(
        filename = file.path(plot_save_dir, "summary_biomass.png"),
        plot = p
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
      dat_long <- left_join(dat_long, species_effort_table, by = "model")
      dat_long_plot <- dplyr::filter(dat_long, Yr > min(Yr) & Yr > sort(unique(Yr))[2])
      
      if (summarize_by_species_effort) {
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
          geom_line() +
          labs(x="Year", y="Fraction of unfished", color="Effort", fill="Effort") +
          theme_minimal() +
          facet_wrap(~species, scales="free_y") + 
          geom_ribbon(aes(ymin=mean_val-se_val, ymax=mean_val+se_val), alpha=0.2, color=NA) +
          scale_color_manual(values = effort_colors, name = "Model/Effort") +
          scale_fill_manual(values = effort_fills, name = "Model/Effort") +
          ggtitle("Fraction of Unfished Comparison")
      } else {
        p <- ggplot(dat_long_plot, aes(x=Yr, y=value, color=model, fill=model, group=model)) +
          geom_line() +
          labs(x="Year", y="Fraction of unfished", color="Model", fill="Model") +
          theme_minimal() + 
          geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.2, color=NA) +
          scale_color_manual(values = effort_colors, name = "Model/Effort") +
          scale_fill_manual(values = effort_fills, name = "Model/Effort") +
          ggtitle("Fraction of Unfished Comparison")
      }
      plots$bratio <- p
      
      ggsave(
        filename = file.path(plot_save_dir, "fraction_unfished.png"),
        plot = p
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
      dat_long <- left_join(dat_long, species_effort_table, by = "model")
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
      
      if (summarize_by_species_effort) {
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
          # geom_line() +
          geom_point() +
          labs(x="Year", y=ylab, color="Effort", fill="Effort") +
          theme_minimal() +
          facet_wrap(~species, scales="free_y") + 
          geom_errorbar(aes(ymin=mean_val-se_val, ymax=mean_val+se_val), width = 0.2, alpha = 0.5) +
          scale_color_manual(values = effort_colors, name = "Model/Effort") +
          scale_fill_manual(values = effort_fills, name = "Model/Effort") +
          ggtitle("Recruits Comparison")
      } else {
        p <- ggplot(dat_long_plot, aes(x=Yr, y=value, color=model, fill=model, group=model)) +
          geom_line() +
          geom_point() +
          labs(x="Year", y=ylab, color="Model", fill="Model") +
          theme_minimal() + 
          geom_errorbar(aes(ymin=lower, ymax=upper), width = 0.2, alpha=0.5)+
          scale_color_manual(values = effort_colors, name = "Model/Effort") +
          scale_fill_manual(values = effort_fills, name = "Model/Effort") +
          ggtitle("Recruits Comparison")
      }
      plots$bratio <- p
      
      ggsave(
        filename = file.path(plot_save_dir, "Recruits.png"),
        plot = p
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
        filter(!is.na(value))
      dat_long_plot <- dplyr::filter(dat_long, Yr > min(Yr) & Yr > sort(unique(Yr))[2])
      
      if (summarize_by_species_effort) {
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
          # geom_line() +
          geom_errorbar(aes(ymin=mean_lower, ymax=mean_upper), width = 0.2, alpha = 0.2)+
          geom_point() +
          labs(x="Year", y="Recruitment deviations", color="Effort", fill="Effort") +
          theme_minimal() +
          facet_wrap(~species, scales="free_y") +
          scale_color_manual(values = effort_colors, name = "Model/Effort") +
          scale_fill_manual(values = effort_fills, name = "Model/Effort") +
          ggtitle("Recruitment Deviations Comparison")
      } else {
        p <- ggplot(dat_long_plot, aes(x=Yr, y=value, color=model, fill=model, group=model)) +
          geom_line() +
          geom_point() +
          labs(x="Year", y="Recruitment deviations", color="Model", fill="Model") +
          theme_minimal() + 
          geom_errorbar(aes(ymin=lower, ymax=upper), width=0.2, alpha=0.5) +
          scale_color_manual(values = effort_colors, name = "Model/Effort") +
          scale_fill_manual(values = effort_fills, name = "Model/Effort") +
          ggtitle("Recruitment Deviations Comparison")
      }
      plots$bratio <- p
      
      ggsave(
        filename = file.path(plot_save_dir, "recdevs.png"),
        plot = p
      )
    }
    ...
    
    return(plots)
}


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
      if (is.null(fleet_num)) {
        stop(paste("Fleet not found for species:", info$species))
      }
      colnames(.x$dat$lencomp)
      .x$dat$lencomp |>
        filter(abs(fleet) == fleet_num) |>
        select(year, matches("^l\\d+$")) |>
        pivot_longer(cols = c(-year), names_to = "length", values_to = "freq") |>
        mutate(
          length = gsub("^l", "", length),
          length = as.numeric(length),
          species = info$species,
          effort = info$effort,
          replicate = info$replicate
        ) %>%
        group_by(species, effort, year) |>
        mutate(freq = freq / sum(freq)) |>
        ungroup()
    }
  )
  
  length_comparison_plot <- lencomps |>
    filter(freq > 0) |>
    ggplot(aes(x = year, y = length, col = effort, size = freq)) +
    geom_point(position = position_dodge(0.5)) +
    facet_wrap(~species) +
    theme_minimal() +
    labs(x="Year", y="Length (cm)", color="Effort", size="Frequency") +
    scale_color_manual(values = effort_colors, name = "Model/Effort")
  
  ggsave(
    filename = file.path(plot_save_dir, "length_comparisons.png"),
    plot = length_comparison_plot
  )
  
  agecomps <- imap_dfr(
    inputs,
    ~ {
      info <- extract_model_info(.y)
      fleet_num <- fleet_lookup[[info$species]]
      if (is.null(fleet_num)) {
        stop(paste("Fleet not found for species:", info$species))
      }
      .x$dat$agecomp %>%
        filter(abs(fleet) == fleet_num) |>
        select(year, matches("^a\\d+$")) |>
        pivot_longer(cols = c(-year), names_to = "age", values_to = "freq") |>
        mutate(
          age = gsub("^a", "", age),
          age = as.numeric(age),
          species = info$species,
          effort = info$effort,
          replicate = info$replicate
        ) %>%
        group_by(species, effort, year) |>
        mutate(freq = freq / sum(freq)) |>
        ungroup()
    }
  )
  
  age_comparison_plot <- agecomps |>
    filter(freq > 0) |>
    ggplot(aes(x = year, y = age, col = effort, size = freq)) +
    geom_point(position = position_dodge(0.5)) +
    facet_wrap(~species) +
    theme_minimal() +
    labs(x="Year", y="Age", color="Effort", size="Frequency")  +
    scale_color_manual(values = effort_colors, name = "Model/Effort")
  
  ggsave(
    filename = file.path(plot_save_dir, "age_comparisons.png"),
    plot = age_comparison_plot
    # , width = png_width, height = png_height, dpi = png_dpi
  )
}