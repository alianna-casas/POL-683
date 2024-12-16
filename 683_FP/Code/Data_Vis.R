##### Alianna Casas 
##### POL 683 
##### Data Visualizations
##### Updated: 11 Dec 2024




# Load Libraries

library(readxl)
library(foreign)
library(tidyverse)
library(ggeffects)
library(lmtest)
library(dplyr)
library(haven)
library(MASS)
library(ordinal)
library(sandwich)
library(ggeffects)
library(knitr)
library(wesanderson)


# Working Directory

setwd("/Users/aliannacasas/Desktop/683_FP")


# Load in cleaned data

final_data <- read.csv("final_data.csv")

# Plotting noncombat participation by specific role

# Reshape to plot noncombat participation by role (clandestine, logistical, and outreach roles)


  #Reshape data for the faceted graph
    
    graph_data <- final_data %>%
      pivot_longer(
        cols = c(noncombat_clandestine, noncombat_outreach, noncombat_logistics),
        names_to = "category",
        values_to = "value")


  # Create custom labels and graph data #
    
   # Custom
    
    custom_labels <- c(
      noncombat_clandestine = "Clandestine",
      noncombat_outreach = "Outreach",
      noncombat_logistics = "Logistics")


  # And Graph #
    
    
    ggplot(graph_data, aes(x = value, fill = category)) +
      geom_bar(color = "black") +
      scale_fill_manual(values = wes_palette("GrandBudapest2", n = 3)) +
      labs(
        title = "Presence of Noncombat Participation",
        x = "Type of Noncombat Roles",
        y = "Count"
      ) +
      theme_minimal() +
      facet_wrap(~ category, labeller = labeller(category = custom_labels), scales = "free_y") +
      theme(legend.position = "none",       
        plot.title = element_text(hjust = 0.5))

      # Majority of these groups have logistic and outreach support #
      
      
# Creating a similar graph for the prevalence of female participation in noncombat roles

    # Set as factor 
    
    merge_waar$noncombat_prev_best <- as.factor(merge_waar$noncombat_prev_best)

    # Create the bar plot
    
    ggplot(na.omit(final_data), aes(x = noncombat_prev_best, fill = noncombat_prev_best)) +
      geom_bar(color = "black",na.rm = TRUE) +
      scale_fill_manual(values = wes_palette("GrandBudapest2")) +
      theme_minimal() +
      labs(
        title = "Prevalence of Women in Noncombat Roles",
        x = "Category",
        y = "Count",
        fill = "Prevalence" 
      ) +
      theme(
        plot.title = element_text(hjust = 0.5))


# Make a graph for female mobilization across roles in RG
    
    # Reshape 
    
    final_data_long <- merge_waar %>%
      pivot_longer(
        cols = c(noncombat, frontline, lead), 
        names_to = "role_type", 
        values_to = "mobilized"
      ) %>%
    filter(!is.na(mobilized))  # Remove rows with NA values
    
    custom_labels <- c(
      frontline = "Frontline Roles",
      lead = "Leadership Roles",
      noncombat = "Noncombat Roles")
    
    # Graph?
    
    ggplot(final_data_long, aes(x = as.factor(mobilized), fill = as.factor(mobilized))) +
      geom_bar(color = "black") +
      scale_fill_manual(values = wes_palette("GrandBudapest2")) +
      theme_minimal() +
      labs(
        title = "Female Mobilization Across Roles",
        x = "Mobilized (0 = No, 1 = Yes)",
        y = "Number of Rebel Groups",
        fill = "Mobilization"
      ) +
      facet_wrap(~ role_type, scales = "fixed", labeller = labeller(role_type = custom_labels)) +
      theme(
        plot.title = element_text(hjust = 0.5))
    




