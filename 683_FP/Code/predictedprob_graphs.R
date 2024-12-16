##### Alianna Casas 
##### POL 683 
##### Predicted Probabilities for Results Section
##### Updated: 15 Dec 2024


# Load Libraries

library(readxl)
library(foreign)
library(tidyverse)
library(ggeffects)
library(lmtest)
library(dplyr)
library(haven)
library(ggeffects)
library(ggplot2)
library(MASS)
library(ordinal)
library(sandwich)
library(ggeffects)
library(knitr)
library(psych)
library(stargazer)
library(wesanderson)




# Working Directory

setwd("/Users/aliannacasas/Desktop/683_FP")


  # Predicted probabilities


      # Politically-motivated

      predictions_political <- bind_rows(
        ggpredict(preorgpar_nc, terms = "preorgpar[all]", 
                  condition = c(leftist = 0, lngdppc = 7.54, duration = 10.53, forced_recruit = 1, fertility_start = 5.55)) %>%  
          mutate(organization = "Political Party"),
        
        ggpredict(preorgmvt_nc, terms = "preorgmvt[all]", 
                  condition = c(leftist = 0, lngdppc = 7.54, duration = 10.53, forced_recruit = 1, fertility_start = 5.55)) %>%  
          mutate(organization = "Movement")
      )


      # Non-politically motivated
      
      predictions_np <- bind_rows(
        ggpredict(preorgyou_nc, terms = "preorgyou[all]", condition = c(leftist = 0, lngdppc = 7.54, duration = 10.53, forced_recruit = 1, fertility_start = 5.55)) %>%
          mutate(organization = "Youth"),
        
        ggpredict(preorgrel_nc, terms = "preorgrel[all]", condition = c(leftist = 0, lngdppc = 7.54, duration = 10.53, forced_recruit = 1, fertility_start = 5.55)) %>%
          mutate(organization = "Religious"))


      # Former Rebels
  
    predictions_rebel <- bind_rows(ggpredict(preorgreb_nc, terms = "preorgreb[all]", condition = c(leftist = 0, lngdppc = 7.54, duration = 10.53, forced_recruit = 1, fertility_start = 5.55)) %>%
        mutate(organization = "Former Rebels"))

    
    # Predicted Prob Plots by Motivation
    
    # But first, Wes Andersen:
    
    grand_budapest_palette <- wes_palette("GrandBudapest2", n = length(unique(predictions$organization)), type = "continuous")
    
    
    
    # Political #
    
    
    # Omit NAs for faceted graph
    
    predictions_political <- na.omit(predictions_political)
    
    # Re-ordering so "Political Party" graph is displayed first:
   
    predictions_political$organization <- factor(predictions_political$organization, 
                                                 levels = c("Political Party", "Movement"))
    # Create graph as an object
    
    pol <- ggplot(predictions_political, aes(x = x, y = predicted, color = organization)) +
      geom_line(size = 1) +                                  
      geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA) +  
      labs(
        title = "Predicted Probabilities of Noncombat Role", 
        x = "Politically Motivated Parent Organizations", 
        y = "Predicted Probability"
      ) + 
      theme_minimal(base_size = 14) + 
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"),
        panel.grid.major = element_line(color = "grey80"),
        panel.grid.minor = element_blank(),
        legend.position = "none"
      ) + 
      facet_wrap(~organization, scales = "free_y", nrow = 1) + 
      scale_color_manual(values = grand_budapest_palette, drop = TRUE)
    

    

        # Non-Political #
        
    # Omit NAs for faceted graph
  
    predictions_np <- na.omit(predictions_np)
    
    
    # Re-ordering so "preorgyou" graph is displayed first:
    
    predictions_np$organization <- factor(predictions_np$organization, 
                                          levels = c("youth", setdiff(unique(predictions_np$organization), "youth")))
    
    

  
    # Create graph as an object 
    
    nonpol <- ggplot(predictions_np, aes(x = x, y = predicted, color = organization)) +
          geom_line(size = 1) +                                 
          geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA) + 
          labs(
            title = "Predicted Probabilities of Noncombat Role", 
            x = "Non-Politically Motivated Parent Organizations", 
            y = "Predicted Probability"
          ) + 
          theme_minimal(base_size = 14) + 
          theme(
            plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
            plot.subtitle = element_text(hjust = 0.5, size = 12),
            axis.title.x = element_text(size = 14, face = "bold"),
            axis.title.y = element_text(size = 14, face = "bold"),
            panel.grid.major = element_line(color = "grey80"),
            panel.grid.minor = element_blank(),
            legend.position = "none"
          ) + 
          facet_wrap(~organization, scales = "free_y", nrow = 1, , drop = TRUE) + 
          scale_color_manual(values = grand_budapest_palette)
        
        
        
        # Former Rebels #
        
        
  rebel <- ggplot(predictions_rebel, aes(x = x, y = predicted, color = organization)) +
          geom_line(size = 1) +                                 
          geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA) + 
          labs(
            title = "Predicted Probabilities of Noncombat Role", 
            x = "Former Rebel Group (Parent Organization Type)", 
            y = "Predicted Probability"
          ) + 
          theme_minimal(base_size = 14) + 
          theme(
            plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
            plot.subtitle = element_text(hjust = 0.5, size = 12),
            axis.title.x = element_text(size = 14, face = "bold"),
            axis.title.y = element_text(size = 14, face = "bold"),
            panel.grid.major = element_line(color = "grey80"),
            panel.grid.minor = element_blank(),
            legend.position = "none"
          ) + scale_color_manual(values = grand_budapest_palette)
    
    
pol
nonpol
rebel

