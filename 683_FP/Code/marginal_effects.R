##### Alianna Casas 
##### POL 683 
##### Marginal Effects for Results Section
##### Updated: 13 Dec 2024


# Load Libraries

install.packages("brant")
library(brant)
library(readxl)
library(foreign)
library(tidyverse)
library(ggeffects)
library(Hmisc)
library(lmtest)
library(dplyr)
library(haven)
library(MASS)
library(ordinal)
library(sandwich)
library(ggeffects)
library(knitr)
library(psych)
library(stargazer)



# Working Directory

setwd("/Users/aliannacasas/Desktop/683_FP")


## MARGINAL EFFECTS FOR THE LOGISTIC REG MODELS ##

  # I want to calculate the marginal effects for each variable which I think will take too much code to do it separately.

  # Attempting to make a function for this:





  # List logistic regression models

  models <- list(preorgpar_nc = preorgpar_nc,preorgmvt_nc = preorgmvt_nc,
                 preorgyou_nc = preorgyou_nc,preorgrel_nc = preorgrel_nc, preorgreb_nc = preorgreb_nc)

  # Define ranges for variables
  
  var_ranges <- list(
    preorgpar = c(0, 1), 
    preorgmvt = c(0, 1), 
    preorgyou = c(0, 1), 
    preorgrel = c(0, 1), 
    preorgreb = c(0, 1), 
    leftist = c(0, 1),    
    lngdppc = seq(min(final_data$lngdppc, na.rm = TRUE), max(final_data$lngdppc, na.rm = TRUE), length.out = 100),
    fertility_start = seq(min(final_data$fertility_start, na.rm = TRUE), max(final_data$fertility_start, na.rm = TRUE), length.out = 100),
    duration = seq(min(final_data$duration, na.rm = TRUE), max(final_data$duration, na.rm = TRUE), length.out = 100),
    forced_recruit = c(0, 1)
  )

  # Creating a function for ME for each model and vars included
  
    calculate_marginal_effects <- function(model, data, variable, values) {
      
    # New data, setting at mean
      
      newdata <- data.frame(lapply(data, function(x) if (is.numeric(x)) mean(x, na.rm = TRUE) else x[1]))
    
    # Store values
      
      marginal_effects <- numeric(length(values) - 1)
    
    # Loop for calculating the differences
      
      for (i in seq_along(values)[-length(values)]) {
        
      # Setting values 
        
      newdata1 <- newdata
      newdata2 <- newdata
      newdata1[[variable]] <- values[i]
      newdata2[[variable]] <- values[i + 1]
      
      # Predicted probs.
      
      prob1 <- predict(model, newdata = newdata1, type = "response")
      prob2 <- predict(model, newdata = newdata2, type = "response")
      
      # Taking the difference
      marginal_effects[i] <- mean(prob2 - prob1)
    }
    
    # Average ME across all differences
      
    return(mean(marginal_effects, na.rm = TRUE))
  }
  
  # Results 
    
    all_results <- list()
  
  # Loop each model
  
    for (model_name in names(models)) {
      model <- models[[model_name]] 
    
    # Loop  each variable
    
    for (var in names(var_ranges)) {
      var_values <- var_ranges[[var]]  
      
      # Try calculating marginal effects
      
      tryCatch({
        marginal_effect <- calculate_marginal_effects(model, final_data, var, var_values)
        
        # Results
        
        all_results <- append(all_results, list(data.frame(
          model = model_name,
          variable = var,
          marginal_effect = marginal_effect
        )))
      }, error = function(e) {
        warning(paste("Error in model", model_name, "variable", var, ":", e$message))
      })
    }
  }
  
# COMBINE AND VIEW RESULTS
    
  all_results <- bind_rows(all_results)

  print(all_results)




