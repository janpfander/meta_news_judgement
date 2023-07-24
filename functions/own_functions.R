# Function to round numbers (generally used on tidy output of models)
round_numbers <- function(x) mutate_if(x, is.numeric, round, 2)

# Function for calculating effect sizes.
# We make measure an argument so that we can change it for later robustness checks.

calculate_effect_sizes <- function(effect, measure = "SMCC", data = meta_wide, 
                                   measure_between = "SMD") {
  
  if(effect == "accuracy") {
    
    # Within participant studies
    # "SMCC" for the standardized mean change using change score standardization 
    # use escalc function
    within_participants <- escalc(measure= {{measure}}, 
                                  # diff = true (m1i) - fake (m2i)
                                  m2i= mean_accuracy_fake, 
                                  sd2i=sd_accuracy_fake, 
                                  ni=n_observations, 
                                  m1i=mean_accuracy_true,
                                  sd1i=sd_accuracy_true, 
                                  data = data %>% 
                                    # restrict to within studies only
                                    filter(design == "within"), 
                                  ri = cor, 
                                  # in case outcome is "SMD"
                                  n1i=n_observations,
                                  n2i=n_observations
    )
    
    # Without the following bit, it would not be possible to run the function on 
    # a subset of data that has no between studies
    if(nrow(data %>% filter(design == "between")) != 0) {
      
      # Between participant studies.
      # we use the escalc function from the metafor package
      # standardized mean difference (SMD)
      between_participants <- escalc(measure= {{measure_between}},
                                     # diff = true (m1i) - fake (m2i)
                                     m2i= mean_accuracy_fake,
                                     sd2i=sd_accuracy_fake, n2i=n_observations,
                                     m1i=mean_accuracy_true, sd1i=sd_accuracy_true,
                                     n1i=n_observations,
                                     data = data %>%
                                       # restrict to between studies only
                                       filter(design == "between")
      )
      
      # Re-unite both designs in a single data frame
      accuracy_effect <- rbind(within_participants, between_participants)  
      return(accuracy_effect)
      
    } else {
      # Alternative for data without any between studies
      return(within_participants)
    }
  }
  
  if(effect == "error") {
    # Within participant studies
    
    # "SMCC" for the standardized mean change using change score standardization 
    # use escalc function
    within_participants <- escalc(measure= {{measure}}, 
                                  # diff = true (m1i) - fake (m2i)
                                  m2i= error_fake, 
                                  sd2i=sd_accuracy_fake, 
                                  ni=n_observations, 
                                  m1i=error_true,
                                  sd1i=sd_accuracy_true, 
                                  data = data %>% 
                                    # restrict to within studies only
                                    filter(design == "within"), 
                                  ri = cor, 
                                  # in case outcome is "SMD"
                                  n1i=n_observations,
                                  n2i=n_observations)
    
    # Without the following bit, it would not be possible to run the function on 
    # a subset of data that has no between studies
    if(nrow(data %>% filter(design == "between")) != 0) {
      
      # Between participant studies. 
      # we use the escalc function from the metafor package
      # standardized mean difference (SMD)
      between_participants <- escalc(measure={{measure_between}},
                                     # diff = true (m1i) - fake (m2i)
                                     m2i= error_fake,
                                     sd2i=sd_accuracy_fake, n2i=n_observations,
                                     m1i=error_true, sd1i=sd_accuracy_true,
                                     n1i=n_observations,
                                     data = data %>%
                                       # restrict to between studies only
                                       filter(design == "between")
      )
      
      # Re-unite both designs in a single data frame 
      error_effect <- rbind(within_participants, between_participants)
      return(error_effect) 
      
    } else {
      # Alternative for data without any between studies
      return(within_participants)
    }
  } 
}

# Function to calculate meta models
calculate_models <- function(data, robust = TRUE, ...) {
  
  # Multilevel random effect model for accuracy
  model <-  metafor::rma.mv(yi, vi, random = ~ 1 | unique_sample_id / observation_id, data=data)
  
  return(model)
  
  if(robust == TRUE) {
    # with robust standard errors clustered at the sample level 
    robust_model <- robust(model, cluster = data$unique_sample_id)
    
    return(robust_model)
  }
}

# Function that calculates models by scale
# returns a vector of lists with with all models 
models_by_scale <- function(return_as = "list") {
  
  # define existing scale types
  scales <- meta_wide %>% 
    # exclude scale with only one single observation
    # (impossible to run a meta model)^
    filter(!accuracy_scale %in% c("100")) %>% 
    reframe(unique(accuracy_scale)) %>% pull()
  
  # calculate (non-standardized) effect sizes/mean differences
  effect_MD_accuracy <- calculate_effect_sizes(effect = "accuracy", 
                                               measure = "MC",
                                               measure_between = "MD",
                                               data = meta_wide)
  effect_MD_error <- calculate_effect_sizes(effect = "error", 
                                            measure = "MC",
                                            measure_between = "MD",
                                            data = meta_wide)
  if(return_as == "list") {
    
    result <- scales %>%
      # make a loop that returns the model results for each scale type
      # and appends it to a list
      map(function(x) {
        
        # check progress
        print(paste0("Running model for scale type: ", x))
        
        reduced <- meta_wide %>% filter(accuracy_scale == x)
        
        # calculate the meta model
        model_accuracy <- calculate_models(data=effect_MD_accuracy %>% 
                                             # reduce data frame to respective scale only
                                             filter(accuracy_scale == x), 
                                           robust = TRUE)
        model_error <- calculate_models(data=effect_MD_error %>% 
                                          # reduce data frame to respective scale only
                                          filter(accuracy_scale == x),
                                        robust = TRUE)
        
        # define names for identification later
        name_accuracy <- paste0("Accuracy ", "(", x,")")
        name_error <- paste0("Error ", "(", x,")")
        return(setNames(list(model_accuracy, model_error), c(name_accuracy, name_error)))
      })
    
    # So fare we have a list of lists (of lists, i.e. model results).
    # We put the to top levels on the same level.
    result <- unlist(result, recursive = FALSE)
    
    return(result)
  }
  
  if(return_as == "data_frame") {
    
    result <- scales %>%
      # make a loop that returns the model results for each scale type
      # and appends it to a list
      map_df(function(x) {
        
        # check progress
        print(paste0("Running model for scale type: ", x))
        
        reduced <- meta_wide %>% filter(accuracy_scale == x)
        
        # calculate the meta model
        model_accuracy <- calculate_models(data=effect_MD_accuracy %>% 
                                             # reduce data frame to respective scale only
                                             filter(accuracy_scale == x), 
                                           robust = TRUE) %>% 
          tidy() %>% 
          mutate(scale = x,
                 outcome = "accuracy")
        model_error <- calculate_models(data=effect_MD_error %>% 
                                          # reduce data frame to respective scale only
                                          filter(accuracy_scale == x),
                                        robust = TRUE) %>% 
          tidy() %>% 
          mutate(scale = x,
                 outcome = "error")
        
        return(bind_rows(model_accuracy, model_error) %>% 
                 mutate_if(is.numeric, round, digits = 4))
      })
    
    return(result)
  }
}

# Function for plotting the moderators
moderator_plots <- function(data, name, common_plot = TRUE) {
  # define moderators
  moderators <- c("news_family_grouped", "country_grouped","political_concordance", 
                  "news_format_grouped", 
                  "news_source", "accuracy_scale_grouped", "perfect_symetry")
  
  # empty list
  plot_list <- list()
  
  # make plots fro each moderator
  for (moderator in moderators) {
    
    plot_data <- data %>%
      mutate(across(.data[[moderator]])) %>% 
      drop_na(.data[[moderator]])
    
    plot <- ggplot(plot_data,
                   aes(x = .data[[moderator]], y = yi)) +
      geom_half_boxplot(aes(color = .data[[moderator]]), side = "l", size = 0.5, nudge = 0.05, 
                        outlier.shape = NA, alpha = 0.7) +
      geom_half_violin(aes(fill = .data[[moderator]]), side = "r", nudge = 0.05, 
                       alpha = 0.7) +
      geom_half_point(aes(color = .data[[moderator]]), side = "l", 
                      transformation_params = list(height = 0, width = 0.1, seed = 1),
                      alpha = 0.7) +
      # add line of 0
      geom_hline(yintercept = 0, 
                 linewidth = 0.5, linetype = "24", color = "grey") +
      # colors 
      scale_color_viridis_d(option = "turbo", begin = 0.1, end = 0.9) +
      scale_fill_viridis_d(option = "turbo", begin = 0.1,, end = 0.9) +
      # labels and scales
      guides(color = "none", fill = "none") +
      labs(x = NULL, y = paste0("SMCC (", name, ")")) +
      plot_theme + 
      theme(
        # Bold title
        axis.title = element_text(size = rel(0.8), face = "plain")
      ) +
      coord_flip()
    
    plot_list[[moderator]] <- plot
  }
  if(common_plot == TRUE) {
    # make common plot
    plot <- ggpubr::ggarrange(plotlist = plot_list,
                              labels = c("(a) News Family", "(b) Country", "(c) Political Concordance",
                                         "(d) News Format", "(e) News Source", "(f) Accuracy Scale", 
                                         "(g) Symmetry"),
                              font.label = list(size = 10), 
                              ncol = 2, 
                              nrow = 4) %>%
      ggpubr::annotate_figure(top = ggpubr::text_grob(name, face = "bold",
                                                      color = "red",
                                                      size = 14))
    
    return(plot)
  } else {
    # Assign names to the elements of plot_list
    names(plot_list) <- moderators
    return(plot_list) 
  }
}

# Function for plotting concordance
plot_concordance <- function(outcome) {
  
  if(outcome == "accuracy") {
    
    ## plot effects
    concordance_plot <- moderator_plots(accuracy_effect, "Discernment", common_plot = FALSE)[[3]]
    
    # retrieve data from models to plot predictions
    intercept <- coef(moderator_models_accuracy$Concordance)[[1]]
    concordance_model <- moderator_models_accuracy$Concordance 
  }
  
  if(outcome == "error") {
    
    ## plot effects
    concordance_plot <- moderator_plots(error_effect, "Response bias", common_plot = FALSE)[[3]]
    
    # retrieve data from models
    intercept <- coef(moderator_models_error$Concordance)[[1]]
    concordance_model <- moderator_models_error$Concordance 
  }
  
  predictions <- concordance_model %>% 
    tidy(conf.int=TRUE) %>% 
    mutate(
      political_concordance = ifelse(term == "intercept", "concordant", "discordant"), 
      # make sure to add the value of the intercept to have the predicted value
      # instead of the differential
      across(c(estimate, conf.low, conf.high), ~ifelse(term == "intercept", .x, 
                                                       .x + intercept))
    )
  
  # Add model predictions to effect plot
  result <- concordance_plot + 
    geom_pointrange(data = predictions, 
                    aes(y = estimate, x = political_concordance, 
                        ymin = conf.low, ymax = conf.high), 
                    nudge = 0.5 )
  
  return(result)
  
}

# Function for plotting the share of true news
plot_share_true <- function(data, name) {
  # Make a factor variable
  data <- data %>% 
    mutate(
      share_true_news = round(share_true_news, digits = 2),
      share_true_news_factor = ordered(share_true_news)
    )
  
  # Get unique factor levels
  factor_levels <- unique(data$share_true_news_factor)
  
  # Separate data for multiple observations 
  data_multiple <- data %>% 
    group_by(share_true_news_factor) %>% 
    filter(n() > 1) %>% 
    ungroup()
  
  # Make plot
  plot <- ggplot() +
    # Add violin and box plot for factor levels with multiple observations
    geom_half_boxplot(
      data = data_multiple,
      aes(x = share_true_news_factor, y = yi, color = share_true_news_factor),
      side = "l",
      size = 0.5,
      nudge = 0.05,
      outlier.shape = NA,
      alpha = 0.7
    ) +
    # Add points for factor levels with a single observation
    geom_half_point(
      data = data,
      aes(x = share_true_news_factor, y = yi, color = share_true_news_factor),
      side = "l",
      transformation_params = list(height = 0, width = 0.1, seed = 1),
      alpha = 0.7
    ) +
    # Add line of 0
    geom_hline(yintercept = 0, 
               linewidth = 0.5, linetype = "24", color = "grey") +
    # Colors 
    scale_color_viridis_d(
      option = "turbo",
      begin = 0.1,
      end = 0.9,
      breaks = factor_levels
    ) +
    scale_fill_viridis_d(
      option = "turbo",
      begin = 0.1,
      end = 0.9,
      breaks = factor_levels
    ) +
    # Labels and scales
    scale_y_continuous(limits = c(-1.5, 3)) +
    guides(color = "none", fill = "none") +
    labs(x = "Share of true news", y = paste0("SMCC (", name, ")")) +
    plot_theme + 
    theme(axis.title = element_text(size = rel(0.8), face = "plain"),
          axis.text.x = element_text(angle = 45, hjust = 1))
  
  return(plot)
}


# Functions for identifying outliers (defined as outside of interquartile range +/- 
# 1.5 times Interquartile range)
identify_outliers <- function(data){
  
  # Define critical outlier thresholds
  outlier_threshhold <- data %>%
    summarize(lower_threshold = quantile(yi, 0.25) - 1.5 * IQR(yi),
              upper_threshold = quantile(yi, 0.75) + 1.5 * IQR(yi))
  
  # Create the outlier indicator variable
  data <- data %>%
    mutate(outlier = if_else(yi < outlier_threshhold$lower_threshold | 
                               yi > outlier_threshhold$upper_threshold,
                             TRUE, FALSE))
  
  return(data)
}

# Function that calculates effects for different correlations, 
# for either accuracy or error, and returns the main average effect for the mixed meta
# model for each correlation value in a data frame
robustness_correlation_values <- function(correlations, effect, measure = "SMCC") {
  
  if (effect == "accuracy") {
    
    results <- correlations %>% 
      # make a loop that for each correlation adds the main effect size in the 
      # mixed meta model to a data frame
      map_df(function(x) {
        
        # Keep track
        print(paste0("Currently estimating model for corrrelation value ", x))
        
        # Step 1: Add correlation to data frame
        meta_wide <- meta_wide %>% 
          mutate(cor = x)
        
        # Step 2: Calculate effect sizes
        
        # Within participant studies
        # "SMCC" for the standardized mean change using change score standardization 
        # use escalc function
        within_participants <- escalc(measure= {{measure}}, 
                                      # diff = true (m1i) - fake (m2i)
                                      m2i= mean_accuracy_fake, 
                                      sd2i=sd_accuracy_fake, 
                                      ni=n_observations, 
                                      m1i=mean_accuracy_true,
                                      sd1i=sd_accuracy_true, 
                                      data = meta_wide %>% 
                                        # restrict to within studies only
                                        filter(design == "within"), 
                                      ri = cor)
        
        # Between participant studies. 
        # we use the escalc function from the metafor package
        # standardized mean difference (SMD)
        between_participants <- escalc(measure="SMD",
                                       # diff = true (m1i) - fake (m2i)
                                       m2i= mean_accuracy_fake,
                                       sd2i=sd_accuracy_fake, n2i=n_observations,
                                       m1i=mean_accuracy_true, sd1i=sd_accuracy_true,
                                       n1i=n_observations,
                                       data = meta_wide %>%
                                         # restrict to between studies only
                                         filter(design == "between"))
        
        # Re-unite both designs in a single data frame 
        accuracy_effect <- rbind(within_participants, between_participants)
        
        # Step 3: calculate model
        # Multilevel random effect model for accuracy
        model_accuracy <-  metafor::rma.mv(yi, vi, 
                                           random = ~ 1 | unique_sample_id / observation_id,
                                           data=accuracy_effect)
        
        
        # robust standard errors clustered at the sample level
        robust_model_accuracy <- robust(model_accuracy, 
                                        cluster = accuracy_effect$unique_sample_id)
        
        # Step 4: return estimate of interest
        return(tidy(robust_model_accuracy) %>% 
                 mutate(imputed_correlation = x, 
                        effect = "accuracy") %>% 
                 mutate_if(is.numeric, round, 5)
        ) 
      }
      )
  } 
  
  else {
    
    results <- correlations %>% 
      # make a loop that for each correlation adds the main effect size in the 
      # mixed meta model to a data frame
      map_df(function(x) {
        
        # Keep track
        print(paste0("Currently estimating model for corrrelation value ", x))
        
        # Step 1: Add correlation to data frame
        meta_wide <- meta_wide %>% 
          mutate(cor = x)
        
        # Step 2: Calculate effect sizes
        
        
        # Within participant studies
        
        # "SMCC" for the standardized mean change using change score standardization 
        # use escalc function
        within_participants <- escalc(measure={{measure}}, 
                                      # diff = true (m1i) - fake (m2i)
                                      m2i= error_fake, 
                                      sd2i=sd_accuracy_fake, 
                                      ni=n_observations, 
                                      m1i=error_true,
                                      sd1i=sd_accuracy_true, 
                                      data = meta_wide %>% 
                                        # restrict to within studies only
                                        filter(design == "within"), 
                                      ri = cor)
        
        # Between participant studies. 
        # we use the escalc function from the metafor package
        # standardized mean difference (SMD)
        between_participants <- escalc(measure="SMD",
                                       # diff = true (m1i) - fake (m2i)
                                       m2i= error_fake,
                                       sd2i=sd_accuracy_fake, n2i=n_observations,
                                       m1i=error_true, sd1i=sd_accuracy_true,
                                       n1i=n_observations,
                                       data = meta_wide %>%
                                         # restrict to between studies only
                                         filter(design == "between"))
        
        # Re-unite both designs in a single data frame 
        error_effect <- rbind(within_participants, between_participants)
        
        # Step 3: calculate model
        # Multilevel random effect model for accuracy
        model_error <-  metafor::rma.mv(yi, vi, 
                                        random = ~ 1 | unique_sample_id / observation_id,
                                        data=error_effect)
        
        
        # robust standard errors clustered at the sample level
        robust_model_error <- robust(model_error, 
                                     cluster = error_effect$unique_sample_id)
        
        # Step 4: return estimate of interest
        return(tidy(robust_model_error) %>% 
                 mutate(imputed_correlation = x, 
                        effect = "error") %>% 
                 mutate_if(is.numeric, round, 5)
        ) 
      }
      )
  }
  
}

# Function that returns the main average effect assuming *independence*
robustness_independence <- function(effect = "accuracy", ...) {
  
  if (effect == "accuracy") {
    # Calculate effect assuming *independence*
    independence_effect <- escalc(measure="SMD",
                                  # diff = true (m1i) - fake (m2i)
                                  m2i= mean_accuracy_fake,
                                  sd2i=sd_accuracy_fake, n2i=n_observations,
                                  m1i=mean_accuracy_true, sd1i=sd_accuracy_true,
                                  n1i=n_observations,
                                  data = meta_wide)
    # Multilevel random effect model 
    model_independence <-  metafor::rma.mv(yi, vi, 
                                           random = ~ 1 | unique_sample_id / observation_id,
                                           data=independence_effect)
    
    
    # robust standard errors clustered at the sample level
    robust_model_independence <- robust(model_independence, 
                                        cluster = independence_effect$unique_sample_id)
    
    # Step 4: return estimate of interest
    return(tidy(robust_model_independence, conf.int = TRUE) %>% 
             mutate_if(is.numeric, round, 5))
    
  }
  
  if (effect == "error") {
    # Calculate effect assuming *independence*
    independence_effect <- escalc(measure="SMD",
                                  # diff = true (m1i) - fake (m2i)
                                  m2i= error_fake,
                                  sd2i=sd_accuracy_fake, n2i=n_observations,
                                  m1i=error_true, sd1i=sd_accuracy_true,
                                  n1i=n_observations,
                                  data = meta_wide)
    # Multilevel random effect model 
    model_independence <-  metafor::rma.mv(yi, vi, 
                                           random = ~ 1 | unique_sample_id / observation_id,
                                           data=independence_effect)
    
    
    # robust standard errors clustered at the sample level
    robust_model_independence <- robust(model_independence, 
                                        cluster = independence_effect$unique_sample_id)
    
    # Step 4: return estimate of interest
    return(tidy(robust_model_independence, conf.int = TRUE) %>% 
             mutate_if(is.numeric, round, 5))
    
  }
  
}

# Function that returns plots to compare model results from different imputations of correlations 
# (and a version assuming independence)
robustness_plot <- function(effect, ...){
  
  
  # calculate effect as function of correlation value for error
  effect_by_correlation <- robustness_correlation_values(correlations, effect = effect, ...)
  
  # calculate effect assuming independence for error
  effect_independence <- robustness_independence(effect = effect)
  
  # get data and main model results from environment
  main_model_name <- paste0("robust_model_", effect)
  data_name <- paste0(effect, "_effect")
  
  main_model <- get(main_model_name)
  data <- get(data_name)
  
  # get tidy version of main model (to be used as reference)
  main_model <-  tidy(main_model) %>% 
    mutate_if(is.numeric, round, 5) %>% 
    # add the value of the correlation
    mutate(imputed_correlation = mean(data$cor))
  
  
  # plot distribution of standard error
  SE_plot <- ggplot(effect_by_correlation, 
                    aes(x = imputed_correlation, y = std.error,
                        fill = "a", color = "a")) + 
    geom_point(alpha = 0.6) + 
    # add point with average correlation effect
    geom_point(data = main_model, aes(x = imputed_correlation, y = std.error), 
               color = "red", fill = "red")  +
    geom_text(data = main_model, aes(x = imputed_correlation, y = 0.9*std.error, 
                                     label =  paste0("SE assuming \n avearage correlation (", round(
                                       imputed_correlation, digits = 2), ")", "\n",
                                       "= ", round(std.error, digits = 4))),
              color = 'red', nudge_x = 0.08, 
              # since we didn't pre-compute means, ggplot would print 
              # mean(distance_by_walk) for each observation which makes the text
              # super bold - so we tell it to check for overlap and remove it
              check_overlap = T
    ) + 
    # add h_line with effect assuming independence
    geom_hline(data = effect_independence, aes(yintercept = std.error), 
               linetype='dotted', 
               color = 'darkorange') +
    geom_text(data = effect_independence, aes(y = 0.9*std.error, x = 0, 
                                              label =  paste0("SE assuming \n independence",
                                                              "\n", "= ", 
                                                              round(std.error, digits = 4))),
              color = 'darkorange', nudge_x = -0.02,
              check_overlap = T
    ) +
    # colors 
    scale_color_viridis_d(option = "plasma") +
    scale_fill_viridis_d(option = "plasma") +
    # labels and scales
    guides(color = "none", fill = "none") +
    labs(x = "Imputed Correlation", y = "SE",
         title = paste0("Average ", effect, " Standard errors"), 
         subtitle = "as function of possible intra-sample \n correlations") +
    plot_theme
  
  # scatter plot effect by correlation
  effect_plot <- ggplot(effect_by_correlation,
                        aes(x = imputed_correlation, y = estimate, 
                            fill = "a", color = "a")) + 
    geom_point(alpha = 0.6) + 
    # add point with average correlation effect
    geom_point(data = main_model, aes(x = imputed_correlation, y = estimate), 
               color = "red", fill = "red")  +
    geom_text(data = main_model, aes(x = imputed_correlation, y = 0.9*estimate, 
                                     label =  paste0("effect assuming \n avearage correlation (", round(
                                       imputed_correlation, digits = 2), ")", "\n",
                                       "= ", round(estimate, digits = 2))),
              color = 'red', nudge_x = 0.08,  
              # since we didn't pre-compute means, ggplot would print 
              # mean(distance_by_walk) for each observation which makes the text
              # super bold - so we tell it to check for overlap and remove it
              check_overlap = T
    ) + 
    # add h_line with effect assuming independence
    geom_hline(data = effect_independence, aes(yintercept = estimate), 
               linetype='dotted', 
               color = 'darkorange') +
    geom_text(data = effect_independence, aes(y = 0.9*estimate, x = 0, 
                                              label =  paste0("effect assuming \n independence",
                                                              "\n", "= ", 
                                                              round(estimate, digits = 2))),
              color = 'darkorange', 
              check_overlap = T
    ) +
    # colors 
    scale_color_viridis_d(option = "plasma") +
    scale_fill_viridis_d(option = "plasma") +
    # labels and scales
    guides(color = "none", fill = "none") +
    labs(x = "Imputed Correlation", y = "Estimated effect",
         title = paste0("Average ", effect, " effects"), 
         subtitle = "as function of possible intra-sample \n correlations") +
    plot_theme
  
  return(list(effect_plot, SE_plot))
}  

# Function for the main descriptive plot 
plot_descriptive <- function(return = "plot"){
  # make plot data with standardized accuracy/error
  data <- meta_wide %>% 
    # rename
    rename(accuracy_fake = mean_accuracy_fake, 
           accuracy_true = mean_accuracy_true) %>%
    mutate(
      # standardize_accuracy
      across(c(accuracy_fake, accuracy_true), 
             ~ifelse(
               # Binary and 0 to 1 scale are already on the wanted scale
               accuracy_scale == "binary" |
                 accuracy_scale == "1", .x, 
               # for all other numeric scales
               (.x-1)  / (accuracy_scale_numeric - 1)), 
             .names = "std_{col}"),
      # standardize_accuracy
      across(starts_with("error"), 
             ~ifelse(
               # Binary and 0 to 1 scale are already on the wanted scale
               accuracy_scale == "binary" |
                 accuracy_scale == "1", .x, 
               # for all other numeric scales
               .x  / (accuracy_scale_numeric - 1)), 
             .names = "std_{col}"), 
      # calculate discernment measure
      discernment = std_accuracy_true - std_accuracy_fake,
    ) %>% 
    pivot_longer(starts_with("std_accuracy"),
                 names_prefix = "std_",
                 names_to = "outcome_veracity", 
                 values_to = "value") %>% 
    select("outcome_veracity", "value", "discernment", "std_error_true", "std_error_fake") %>% 
    separate_wider_delim(outcome_veracity, "_", names = c("outcome", "veracity")) %>% 
    # change label for veracity: fake to false 
    mutate(veracity = ifelse(veracity == "fake", "false", veracity)) %>% 
    # select only accuracy
    filter(outcome == "accuracy")
  
  # summarized data_means
  means <- data %>% 
    filter(outcome == "accuracy") %>% 
    group_by(veracity, outcome) %>% 
    summarize(mean_value = mean(value), 
              discernment = mean(discernment),
              error_fake = mean(std_error_fake), 
              error_true = mean(std_error_true)) %>% 
    mutate(optimum = ifelse(veracity == "false", 0, 1),
           # define where to draw the horizontal line on the y-axis
           x_position_means = ifelse(veracity == "true", mean_value + 0.08,
                                     mean_value - 0.08),
           x_position_optimum = ifelse(veracity == "true", optimum - 0.05,
                                       optimum + 0.05),
           label_optimum = ifelse(veracity == "true", "Optimum\nTrue",
                                  "Optimum\nFalse"),
           y_error_lines = 1,
           y_discernment_line = 3.5,
           error = ifelse(veracity == "false", mean_value, 
                          optimum - mean_value)) %>% 
    mutate_if(is.numeric, round, digits = 2)
  
  if(return == "data") {
    return(means)
  }
  
  if(return == "plot") {
    
    plot <- ggplot(data, aes(x = value, fill = veracity)) +
      geom_density(alpha = 0.5) +
      # add mean lines with colors
      geom_vline(data = means,
                 aes(xintercept = mean_value, color = veracity),
                 linetype = "dashed") +
      # labels for means
      geom_label(data = means,
                 aes(x = x_position_means, y = 4.5, label = paste0("mean=", mean_value)),
                 alpha = 0.6, size = 3, show.legend = FALSE) +
      # optima
      # add line at 0
      geom_vline(xintercept = 0, 
                 linewidth = 0.5, linetype = "24", color = "grey") +
      # add line at 1
      geom_vline(xintercept = 1, 
                 linewidth = 0.5, linetype = "24", color = "grey") +
      # labels for optima
      geom_label(inherit.aes = FALSE, data = means,
                 aes(x = x_position_optimum, y = 1.75, label = label_optimum),
                 alpha = 0.6,
                 color = "grey50", size = 3, show.legend = FALSE) +
      # discernment
      geom_segment(data = means, aes(x = error_fake, 
                                     y = y_discernment_line, 
                                     xend = 1-error_true, 
                                     yend = y_discernment_line),
                   color = "black", linetype = "solid", 
                   arrow = arrow(length = unit(0.3, "cm"), 
                                 ends = "both")
      ) + 
      geom_text(inherit.aes = FALSE, data = means, 
                aes(x = error_fake + ((1-error_true - error_fake)/2), 
                    y = y_discernment_line, label = paste0("Discernment\n(", discernment, ")")), 
                nudge_y = 0.01, size = 3,
                color = "black", check_overlap = TRUE) +
      # error
      geom_segment(data = means, aes(x = mean_value, y = y_error_lines, 
                                     xend = optimum, yend = y_error_lines),
                   color = grey(0.3), linetype = "solid", 
                   arrow = arrow(length = unit(0.3, "cm"), 
                                 ends = "both")
      ) + 
      geom_text(inherit.aes = FALSE, data = means, 
                aes(x = mean_value + (optimum - mean_value)/1.5, 
                    y = y_error_lines, label = paste0("Error\n(", error, ")")), 
                nudge_y = 0.01, size = 3,
                color = grey(0.3), check_overlap = TRUE) +
      # scale
      scale_x_continuous(breaks = seq(from = 0, to = 1, by = 0.2)) +
      # colors 
      scale_color_viridis_d(option = "plasma", end = 0.9)+
      scale_fill_viridis_d(option = "plasma", end = 0.9) +
      # labels and scales
      labs(x = "Accuracy scores \n (scaled to range from 0 to 1)", y = "Density") +
      plot_theme + 
      theme(legend.position = "top")  +
      coord_cartesian(xlim = c(0, 1))
    
    return(plot) 
  }
}

# Function to plot distributions for accuracy
plot_standardized_distributions <- function(format = "density", outcome_filter,
                                            veracity_filter) {
  
  # make plot data with standardized accuracy/error
  data <- meta_wide %>% 
    # rename
    rename(accuracy_fake = mean_accuracy_fake, 
           accuracy_true = mean_accuracy_true) %>%
    mutate(
      # standardize_accuracy
      across(c(accuracy_fake, accuracy_true), 
             ~ifelse(
               # Binary and 0 to 1 scale are already on the wanted scale
               accuracy_scale == "binary" |
                 accuracy_scale == "1", .x, 
               # for all other numeric scales
               (.x-1)  / (accuracy_scale_numeric - 1)), 
             .names = "std_{col}"),
      # standardize_accuracy
      across(starts_with("error"), 
             ~ifelse(
               # Binary and 0 to 1 scale are already on the wanted scale
               accuracy_scale == "binary" |
                 accuracy_scale == "1", .x, 
               # for all other numeric scales
               .x  / (accuracy_scale_numeric - 1)), 
             .names = "std_{col}")
    ) %>% 
    pivot_longer(starts_with("std"),
                 names_prefix = "std_",
                 names_to = "outcome_veracity", 
                 values_to = "value") %>% 
    select("outcome_veracity", "value") %>% 
    separate_wider_delim(outcome_veracity, "_", names = c("outcome", "veracity")) 
  
  # filter outcome and veracity
    data <- data %>% filter(outcome == outcome_filter & veracity == veracity_filter)

  # summarized data_means
  means <- data %>% group_by(veracity, outcome) %>% summarize(mean_value = mean(value))
  
  if(format == "histogram") {
    geom <- geom_histogram(alpha = 0.5, position = "identity")
    labs <- labs(x = "Standardized scores \n (scale from 0 to 1)", y = "Count")
    # add labels with mean values
    labels <- ggrepel::geom_label_repel(data = means,
                                        aes(x = mean_value, label = paste0("mean=",round(mean_value, 2))),
                                        alpha = 0.6,
                                        nudge_x = -0.1,
                                        y = 25, size = 3, show.legend = FALSE) 
  }
  
  if(format == "density") {
    
    # Calculate nudge_y based on veracity
    means <- means %>%
      mutate(y = ifelse(veracity == "true", 3, 4))
    
    
    geom <- geom_density(alpha = 0.5)
    labs <- labs(x = "Standardized scores \n (scale from 0 to 1)", y = "Density")
    # add labels with mean values
    labels <- geom_label(data = means,
                         aes(x = mean_value, label = paste0("mean=",round(mean_value, 2))),
                         alpha = 0.6,
                         nudge_x = -0.15,
                         y = means$y, size = 3, show.legend = FALSE) 
  }
  
  plot <- ggplot(data, aes(x = value, fill = veracity)) +
    geom +
    # add mean lines with colors
    geom_vline(data = means,
               aes(xintercept = mean_value, color = veracity),
               linetype = "dashed") +
    labels +
    # add line at 0
    geom_vline(xintercept = 0, 
               linewidth = 0.5, linetype = "24", color = "grey") +
    # add line at 1
    geom_vline(xintercept = 1, 
               linewidth = 0.5, linetype = "24", color = "grey") +
    # scale
    scale_x_continuous(breaks = seq(from = 0, to = 1, by = 0.2)) +
    # colors 
    scale_color_viridis_d(option = "plasma", end = 0.9)+
    scale_fill_viridis_d(option = "plasma", end = 0.9) +
    # labels and scales
    labs +
    plot_theme + 
    theme(legend.position = "top") +
    facet_wrap(~outcome)
  
  return(plot)
}

# Function for world map plots
plot_map <- function(fill, data = accuracy_effect){
  # Download "Admin 0 – Countries" from
  # https://www.naturalearthdata.com/downloads/110m-cultural-vectors/
  world_map <- read_sf("data/ne_110m_admin_0_countries/ne_110m_admin_0_countries.shp") %>% 
    # remove Antarctica
    filter(ISO_A3 != "ATA")
  
  # make summary data to plot on map
  participants_by_country = data %>% 
    group_by(country_mapcompatible, unique_sample_id) %>% 
    summarise(participants_per_sample = max(n_subj)) %>% 
    group_by(country_mapcompatible) %>% 
    summarize(participants = sum(participants_per_sample)) %>% 
    # uneven numbers may occur where we didn't have information on how many 
    # participants in treatment vs. control group and assumed an even split 
    # (i.e. dividing overal n by number of condition)
    mutate_if(is.numeric, round, digits = 0)
  
  map_data <- data %>% 
    group_by(country_mapcompatible) %>% 
    summarise(papers = n_distinct(paperID), 
              samples = n_distinct(unique_sample_id),
              effects = n_distinct(observation_id), 
              yi = mean(yi)
    ) %>% 
    left_join(participants_by_country) %>% 
    # make cutoff versions that allow for better plotting
    mutate(papers_cut = case_when(papers == 1 ~ "1", 
                                  papers <= 5 ~ "2-5", 
                                  papers <= 10 ~ "5-10", 
                                  papers > 10 ~ as.character(max(papers))), 
           papers_cut = fct_relevel(as.factor(papers_cut), "1", "2-5","5-10", as.character(max(papers))),
           samples_cut = case_when(samples == 1 ~ "1", 
                                   samples <= 5 ~ "2-5", 
                                   samples <= 10 ~ "5-10", 
                                   samples > 10 ~ as.character(max(samples))), 
           samples_cut = fct_relevel(as.factor(samples_cut), "1", "2-5","5-10", as.character(max(samples))),
           effects_cut = case_when(effects == 1 ~ "1", 
                                   effects <= 5 ~ "2-5", 
                                   effects <= 10 ~ "5-10", 
                                   effects <= 15 ~ "10-15",
                                   effects > 15 ~ as.character(max(effects))), 
           effects_cut = fct_relevel(as.factor(effects_cut), "1", "2-5","5-10", "10-15", as.character(max(effects))),
           participants_cut = case_when(participants < 500 ~ "< 500", 
                                        participants < 1000 ~ "500 - 1'000",  
                                        participants < 2000 ~ "1'000 - 2'000",
                                        participants < 5000 ~ "2'000 - 5'000",
                                        participants < 10000 ~ "5'000 - 10'000",
                                        participants < 15000 ~ "10'000 - 15'000",
                                        participants > 15000 ~ as.character(max(participants))),
           participants_cut = fct_relevel(as.factor(participants_cut), "< 500", 
                                          "500 - 1'000","1'000 - 2'000", "2'000 - 5'000", 
                                          "5'000 - 10'000", "10'000 - 15'000", 
                                          as.character(max(participants))),
           participants_num = ifelse(participants <=5000, participants, 5001)
    )
  
  # combine geo data and summarized data
  map <- left_join(world_map, map_data, by = c("ADMIN" = "country_mapcompatible"))
  
  if (fill == "papers") {
    # papers
    papers <- ggplot() + 
      geom_sf(data = map, 
              aes(fill = papers_cut),
              size = 0.25) +
      coord_sf(crs = st_crs("ESRI:54030")) +  # Robinson
      scale_fill_viridis_d(option = "plasma", begin = 0.2, end = 0.9) +
      labs(fill = "Papers") +
      plot_theme +
      theme_void() +
      theme(legend.position = "bottom")
    return(papers)
  }
  
  if (fill == "samples") {
    # samples
    samples <- ggplot() + 
      geom_sf(data = map, 
              aes(fill = samples_cut),
              size = 0.25, 
              alpha = 0.6) +
      coord_sf(crs = st_crs("ESRI:54030")) +  # Robinson
      scale_fill_viridis_d(option = "plasma", begin = 0.8, end = 1, direction = -1) +
      labs(fill = "Samples") +
      plot_theme +
      theme_void() +
      theme(legend.position = "bottom") 
    return(samples)
  }
  
  if (fill == "participants_numeric") {
    # participants
    participants <- ggplot() + 
      geom_sf(data = map , 
              aes(fill = participants_num),
              size = 0.25) +
      coord_sf(crs = st_crs("ESRI:54030")) +  # Robinson
      scale_fill_viridis_c(option = "plasma", begin = 0.2, end = 0.9, 
                           limits = c(1, 5001), 
                           breaks = c(1, seq(1000, 4000, by = 1000), 5001), 
                           labels = c(1, seq(1000, 4000, by = 1000), ">5000")
      ) +
      labs(fill = "Participants") +
      theme_void() +
      theme(legend.position = "bottom", 
            legend.key.width = unit(2, "cm"))
    return(participants)
  }
  
  if (fill == "discernment") {
    # participants
    discernment <- ggplot() + 
      geom_sf(data = map , 
              aes(fill = yi),
              size = 0.25) +
      coord_sf(crs = st_crs("ESRI:54030")) +  # Robinson
      scale_fill_viridis_c(option = "rocket", begin = 0.65, end = 1, 
                           direction = -1,na.value = "white",
                           breaks = c(0, seq(0.5, 2, by = 0.5)), 
                           labels = c(0, seq(0.5, 2, by = 0.5))) +
      labs(fill = "Discernment") +
      theme_void() +
      theme(legend.position = "bottom", 
            legend.key.width = unit(2, "cm"))
    return(discernment)
  }
  
  if (fill == "response bias") {
    # participants
    bias <- ggplot() + 
      geom_sf(data = map , 
              aes(fill = yi),
              size = 0.25) +
      coord_sf(crs = st_crs("ESRI:54030")) +  # Robinson
      scale_fill_gradient2(low = "#2C7BB6", mid = "grey", high = "#D7191C",
                           midpoint = 0, na.value = "white") +
      labs(fill = "Response bias") +
      theme_void() +
      theme(legend.position = "bottom", 
            legend.key.width = unit(2, "cm"))
    return(bias)
  }
  
  if (fill == "participants") {
    
    # Define the number of colors in the scale
    n_colors <- 20000
    
    # Generate the color scale
    palette <- colorRampPalette(c("#FCFDBFCC", "black"), interpolate = "linear")(n_colors)
    
    # # alternative from viridis
    # palette <- viridis(n_colors, 
    #                    alpha = 0.8, begin = 0, end = 1, direction = -1, option = "rocket")
    
    # assign (more or less) proportional color
    colors <- c(palette[1], palette[1000], palette[2000], palette[3500], palette[7500], palette[12500], palette[length(palette)])
    
    
    participants <- ggplot() + 
      geom_sf(data = map , 
              aes(fill = participants_cut),
              size = 0.25) +
      coord_sf(crs = st_crs("ESRI:54030")) +  # Robinson
      scale_fill_manual(values = colors, 
                        na.value = "white") +
      labs(fill = "Participants") +
      theme_void() +
      theme(legend.position = "bottom")
    return(participants)
  }
  
  if(fill == "effects") {
    
    # Define the number of colors in the scale
    n_colors <- 15
    
    # generate color scale
    palette <- viridis(n_colors,
                       alpha = 0.8, begin = 0.65, end = 1, direction = -1, option = "rocket")
    
    # # Manual Alternative 
    # # Define the start and end points for the color scale
    # start_color <- "#FFFFE5"  # Very light yellow
    # end_color <- "#FFD700"    # Dark yellow, almost orange
    # 
    # # Generate the color scale
    # palette <- colorRampPalette(c(start_color, end_color), interpolate = "linear")(n_colors)
    
    # assign (more or less) proportional color
    colors <- c(palette[1], palette[5], palette[12], palette[15], "black")
    
    
    effects <- ggplot() + 
      geom_sf(data = map , 
              aes(fill = effects_cut),
              size = 0.25) +
      coord_sf(crs = st_crs("ESRI:54030")) +  # Robinson
      scale_fill_manual(values = colors, 
                        na.value = "white", 
                        guide = guide_legend(nrow = 1)) +
      labs(fill = "Effect sizes") +
      theme_void() +
      theme(legend.position = "bottom",
            legend.text = element_text(size = 10),  # Adjust the text size
            legend.key.height = unit(0.5, "lines"),  # Set the height of the legend keys
            legend.box.spacing = unit(0.1, "lines")  # Adjust the spacing between the legend keys
            )
    return(effects)
  }
}

# Function for calculating models for moderator variables
calculate_moderator_models <- function(data, list_report = FALSE) {
  
  # define names for models (make sure they are in the same order as the)
  # models below)
  
  names <- c("Country", "Concordance", "Family", "Format", "Source", "Scale", 
             "Symmetrie", "All")
  
  # separate models per moderator 
  moderator_models <- list(
    
    accuracy_country <-  robust(metafor::rma.mv(yi, vi, 
                                                mods = ~country_grouped,
                                                random = ~ 1 | unique_sample_id / 
                                                  observation_id, data=data), 
                                cluster = data$unique_sample_id
    ),
    
    accuracy_concordance <-  robust(metafor::rma.mv(yi, vi, 
                                                    mods = ~political_concordance,
                                                    random = ~ 1 | unique_sample_id / 
                                                      observation_id, data=data), 
                                    cluster = data$unique_sample_id
    ),
    
    accuracy_news_family <-  robust(metafor::rma.mv(yi, vi, 
                                                    mods = ~news_family_grouped,
                                                    random = ~ 1 | unique_sample_id / 
                                                      observation_id, data=data),
                                    cluster = data$unique_sample_id
    ),
    
    accuracy_news_format <-  robust(metafor::rma.mv(yi, vi, 
                                                    mods = ~news_format_grouped,
                                                    random = ~ 1 | unique_sample_id / 
                                                      observation_id, data=data),
                                    cluster = data$unique_sample_id
    ),
    
    accuracy_news_source <-  robust(metafor::rma.mv(yi, vi, 
                                                    mods = ~news_source,
                                                    random = ~ 1 | unique_sample_id / 
                                                      observation_id, data=data),
                                    cluster = data$unique_sample_id
    ),
    
    accuracy_scale_grouped <-  robust(metafor::rma.mv(yi, vi, 
                                                      mods = ~accuracy_scale_grouped,
                                                      random = ~ 1 | unique_sample_id / 
                                                        observation_id, data=data),
                                      cluster = data$unique_sample_id
    ),
    
    accuracy_perfect_symetry <- robust(metafor::rma.mv(yi, vi, 
                                                       mods = ~perfect_symetry,
                                                       random = ~ 1 | unique_sample_id / 
                                                         observation_id, data=data),
                                       cluster = data$unique_sample_id
    ),
    
    accuracy_all <- robust(metafor::rma.mv(yi, vi, 
                                           mods = ~country_grouped + political_concordance +
                                             news_family_grouped + 
                                             news_format_grouped + news_source +
                                             perfect_symetry,
                                           random = ~ 1 | unique_sample_id / 
                                             observation_id, data=data),
                           cluster = data$unique_sample_id
    )
    
  )
  
  moderator_models <- setNames(moderator_models, names)
  
  if(list_report == FALSE) {
    
    return(moderator_models)
  }
  
  if(list_report == TRUE) {
    
    # make more convenient lists for reporting
    result_list <- list()  # Create an empty data frame to store the results
    
    for (i in names(moderator_models)) {
      result <- moderator_models[[i]] %>% 
        tidy(conf.int = TRUE) %>% 
        round_numbers() %>% 
        mutate(ci = glue::glue("[{conf.low}, {conf.high}]")) %>% 
        mutate(moderator = i) %>% 
        split(.$term)
      
      # append result to list
      result_list[[i]] <- result
    }
    return(result_list)
  }
}

# Function for splitting data along several variables (useful for inline reporting)
# taken from here: https://www.tjmahr.com/lists-knitr-secret-weapon/
super_split <- function(.data, ...) {
  dots <- rlang::enquos(...)
  for (var in seq_along(dots)) {
    var_name <- rlang::as_name(dots[[var]])
    .data <- purrr::map_depth(
      .x = .data,
      .depth = var - 1,
      .f = function(xs) split(xs, xs[var_name])
    )
  }
  .data
}

# Function for plotting p-curve
# (use either accuracy_effect or error_effect as data input)
plot_pcurve <- function(data){
  
  # Determine plot name
  name_data <- deparse(substitute(data))
  
  if(str_detect(name_data, "accuracy")){
    name <- "(a) Discernment"
  } else { name <- "(b) Response bias"}
  
  # Create p-curve function compatible data
  p_curve_data <- data %>% 
    mutate(TE = yi, 
           seTE = sqrt(vi), 
           studlab = observation_id)
  
  # save p-curve plot 
  p_curve <- pcurve(p_curve_data)
  
  # Extract data to put into ggplot. 
  p_curve_plot <- p_curve$PlotData 
  
  # long format data works better in ggplot 
  p_curve_plot <- pivot_longer(p_curve_plot, cols = - `p-value`) %>%  
    mutate(value = round(value, digits =1))
  
  # Generates new p_curve plot from p_curve model results
  p_curve <- ggplot(p_curve_plot , aes(x = `p-value`, y = value, color = name)) + 
    geom_line(aes(linetype = name), size = 1.5) + theme_classic() + 
    geom_point(data = filter(p_curve_plot ,  name == "Observed (blue)")) + 
    geom_text( data = filter(p_curve_plot ,  name == "Observed (blue)"), 
               aes(label = paste(value, "%", sep = "")), 
               size = 3.25, color = "Black",    vjust = -1.25 ) + 
    ylab("Percentage of test results") + 
    scale_color_manual(values=c('grey40', 'indianred3', "grey10"), 
                       name = "Curve", 
                       labels = c("Curve under null of 0% power", 
                                  "Observed p-curve", 
                                  "Curve under null of 30% power")) +
    ggtitle(name)  + 
    plot_theme +
    theme(legend.position = c(.90, .95),
          legend.justification = c("right", "top"),
          legend.box.just = "right",
          legend.margin = margin(6, 6, 6, 6),
          legend.text = element_text(size = 8),
          plot.title = element_text(hjust = 0.5), 
          axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12)) +
    scale_linetype_manual(values=c("dotted", "solid", "twodash")) + 
    guides(linetype = FALSE) +ylim(NA, 105) + xlim(0.009,NA)
  
  return(p_curve) 
}



