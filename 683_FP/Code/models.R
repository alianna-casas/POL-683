##### Alianna Casas 
##### POL 683 
##### Logistic Regression and Generalized Ordered Logistic Regression Models
##### Updated: 16 Dec 2024


# Load Libraries

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

################################################################

# Read and set data as a dataframe

# Reading in 

final_data <- read.csv("final_data.csv")

# DF

final_data <- as.data.frame(final_data)

# Generate descriptive statistics


describe(final_data[, c("noncombat", "noncombat_clandestine", 
                "noncombat_logistics", "noncombat_outreach", "leftist", 
                "lngdppc", "fertility_start", "duration", "forced_recruit", 
                "preorgpar", "preorgmvt", "preorgyou", "preorglab", "preorgrel")])


## LOGIT REGRESSION ANALYSIS ##

  final_data <- as.data.frame(final_data)
  
  # Poltical Parties #
  
  preorgpar_nc <- glm(noncombat ~  preorgpar + leftist + lngdppc + fertility_start  + duration + forced_recruit, family = "binomial"(link=logit) , data = final_data)

    # Clustered SE
    
    clustered_par_nc <- vcovCL(preorgpar, cluster = ~ccode.x)
    se_par_nc <- sqrt(diag(clustered_par_nc))
    
  
  # Non-Party Political Movements #
    
  preorgmvt_nc <- glm(noncombat ~  preorgmvt + leftist + lngdppc + fertility_start  + duration + forced_recruit, family = "binomial"(link=logit) , data = final_data)


    # Clustered SE
    
    clustered_mvt_nc <- vcovCL(preorgmvt_nc, cluster = ~ccode.x)
    se_mvt_nc <- sqrt(diag(clustered_mvt_nc))


  # Youth #
  
  preorgyou_nc <- glm(noncombat ~  preorgyou + leftist + lngdppc + fertility_start  + duration + forced_recruit, family = "binomial"(link=logit), data = final_data)

    # Clustered SE
    
    clustered_you_nc <- vcovCL(preorgyou_nc, cluster = ~ccode.x)
    se_you_nc <- sqrt(diag(clustered_you_nc))

  # Religious #
    
  preorgrel_nc <- glm(noncombat ~  preorgrel + leftist + lngdppc + fertility_start + duration + forced_recruit, family = "binomial"(link=logit), data = final_data)

    # Clustered SE
    
    clustered_rel_nc <- vcovCL(preorgrel_nc, cluster = ~ccode.x)
    se_rel_nc <- sqrt(diag(clustered_rel_nc))

  # Former Rebel #
      
  preorgreb_nc <- glm(noncombat ~  preorgreb + leftist + lngdppc + fertility_start + duration + forced_recruit, family = "binomial"(link=logit), data = final_data)

    # Clustered SE
    
    clustered_reb_nc <- vcovCL(preorgreb_nc, cluster = ~ccode.x)
    se_reb_nc <- sqrt(diag(clustered_reb_nc))


# Noncombat Prevalence Output #

  # I had to output and combine these manually- thank you stargazer for being a pain on a Sunday night.

    stargazer(preorgpar_nc, preorgmvt_nc, preorgyou_nc, preorgrel_nc,
              type = "latex", 
              title = "Generalized Ordered Logit Regression Results", 
              model.names = TRUE, 
              se = list(se_par_nc, se_mvt_nc, se_you_nc, se_rel_nc), 
              digits = 3)

  stargazer(preorgreb_nc, type = "latex", title = "Generalized Ordered Logit Regression Results", 
            model.names = TRUE, 
            se = list(se_reb_nc),  
                      digits = 3)
  
  
## AIC & BIC ##
  
  # AIC - comparing model fit #
  
      AIC(preorgpar_nc) # Political
      
      AIC(preorgmvt_nc) # Non-Party Political
      
      AIC(preorgyou_nc) # Youth - lowest AIC value
      
      AIC(preorgrel_nc) # Religious
      
      AIC(preorgreb_nc) # Former Rebel
      
      
  # BIC - comparing model fit #
      
      BIC(preorgpar_nc) # Political
      
      BIC(preorgmvt_nc) # Non-Party Political
      
      BIC(preorgyou_nc) # Youth - likewise, lowest BIC value
      
      BIC(preorgrel_nc) # Religious
      
      BIC(preorgreb_nc) # Former Rebel
  

#############################################################################
#############################################################################
#############################################################################  
  
  ## ORDERED LOGITISTIC REGRESSION ANALYSIS (ROBUSTNESS CHECK) ##
  
  ## Looking at how different forms of preorgs predict the prevalence noncombat participation ##
  
  # Making sure that noncombat is a factor for the ordered logits
  
  final_data$noncombat_prev_best <- as.factor(final_data$noncombat_prev_best)
  
  
  # Poltical Parties #
  
  preorgpar <- polr(noncombat_prev_best ~  preorgpar + leftist + lngdppc + fertility_start  + duration + forced_recruit , data = final_data, Hess = TRUE)
  coeftest(preorgpar, vcov. = vcovCL(preorgpar, cluster = final_data$ccode.x, type = "HC0"))
  
  # Brant ~ Doesn't hold for preorgpar or duration #
  
  brant_test <- brant(preorgpar)
  
  
  # Non-Party Political Movements #
  
  preorgmvt <- polr(noncombat_prev_best ~  preorgmvt + leftist + lngdppc + fertility_start  + duration + forced_recruit , data = final_data, Hess = TRUE)
  coeftest(preorgmvt, vcov. = vcovCL(preorgmvt, cluster = final_data$ccode.x, type = "HC0"))
  
  # Brant ~ doesn't hold for duration #
  
  brant_test <- brant(preorgmvt)
  
  
  # Youth #
  
  preorgyou <- polr(noncombat_prev_best ~  preorgyou + leftist + lngdppc + fertility_start  + duration + forced_recruit , data = final_data, Hess = TRUE)
  coeftest(preorgyou, vcov. = vcovCL(preorgyou, cluster = final_data$ccode.x, type = "HC0"))
  
  # Brant ~ doesn't hold for duration #
  
  brant_test <- brant(preorgyou)
  
  # Religious #
  
  preorgrel <- polr(noncombat_prev_best ~  preorgrel + leftist + lngdppc + fertility_start + duration + forced_recruit, data = final_data, Hess = TRUE)
  coeftest(preorgrel, vcov. = vcovCL(preorgrel, cluster = final_data$ccode.x, type = "HC0"))
  
  # Brant  ~ doesn't hold for duration  #
  
  brant_test <- brant(preorgrel)
  
  # Former Rebel #
  
  preorgreb <- polr(noncombat_prev_best ~  preorgreb + leftist + lngdppc + fertility_start + duration + forced_recruit, data = final_data, Hess = TRUE)
  coeftest(preorgreb, vcov. = vcovCL(preorgreb, cluster = final_data$ccode.x, type = "HC0"))
  
  # Brant ~ doesn't hold for duration #
  
  brant_test <- brant(preorgreb)
  
  
  # Using a generalized ordered logit model to relax assumptions of the PLA
  
  # Poltical Parties #
  
  preorgpar <- clm(noncombat_prev_best ~  preorgpar + leftist + lngdppc + fertility_start  + duration + forced_recruit , data = final_data)
  
  # Clustered SE
  
  clustered_par <- vcovCL(preorgpar, cluster = ~ccode.x)
  se_par <- sqrt(diag(clustered_par))
  
  
  # Non-Party Political Movements #
  
  preorgmvt <- clm(noncombat_prev_best ~  preorgmvt + leftist + lngdppc + fertility_start  + duration + forced_recruit , data = final_data)
  
  # Clustered SE
  
  clustered_mvt <- vcovCL(preorgmvt, cluster = ~ccode.x)
  se_mvt <- sqrt(diag(clustered_mvt))
  
  
  # Youth #
  
  preorgyou <- clm(noncombat_prev_best ~  preorgyou + leftist + lngdppc + fertility_start  + duration + forced_recruit , data = final_data)
  
  # Clustered SE
  
  clustered_you <- vcovCL(preorgyou, cluster = ~ccode.x)
  se_you <- sqrt(diag(clustered_you))

  # Religious ~ doesn't hold for duration #
  
  preorgrel <- clm(noncombat_prev_best ~  preorgrel + leftist + lngdppc + fertility_start + duration + forced_recruit, data = final_data)
  
  # Clustered SE
  
  clustered_rel <- vcovCL(preorgrel, cluster = ~ccode.x)
  se_rel <- sqrt(diag(clustered_rel))
  
  
  # Former Rebel #
  
  preorgreb <- clm(noncombat_prev_best ~  preorgreb + leftist + lngdppc + fertility_start + duration + forced_recruit, data = final_data)
  
  # Clustered SE
  
  clustered_reb <- vcovCL(preorgreb, cluster = ~ccode.x)
  se_reb <- sqrt(diag(clustered_reb))
  
  
  # Noncombat Prevalence Output #

  stargazer(preorgpar, preorgmvt, preorgyou, preorgrel, preorgreb, 
            type = "latex", 
            title = "Generalized Ordered Logit Regression Results", 
            model.names = TRUE, 
            se = list(se_par, se_mvt, se_you, se_rel, se_reb), 
            digits = 3)    
