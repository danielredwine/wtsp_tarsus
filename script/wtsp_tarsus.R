# Load all libraries needed
library(ggplot2)
library(dplyr)
library(lme4) #GLM packagees 
library(lmerTest)
library(ggfortify)
library(MuMIn)
library(asbio)
library(GGally)
library(performance)
library(patchwork) # Easy way to arrange ggplot
library(olsrr) # Easy way to look at regression plots
library(RColorBrewer) # colors are fun 
library(emmeans)
library(tidyverse)

wtsp_data <- read.csv("data/wtsp_data_23_oct_23.csv") 
#load in dataset, ../ moves up one folder

# Select the variables I need and clean up dataset 
wtsp_data_clean <- wtsp_data %>% # name new object
  filter(New_Recap == "N") %>% # filter to only include new birds to avoid repeated measure
  # If some recaps didn't have tarsus initially may want to include
  select(SampleID, PCRsex, Wing, BirdMass, Tarsus) %>% # Select variables 
  filter(PCRsex == "M" | PCRsex == "F" ) %>% # make sure only M/F
  filter(Tarsus != "NA") # filter out any blanks

# Ensure that all variables are factors/numeric as appropriate
wtsp_data_clean$SampleID <- as.factor(wtsp_data_clean$SampleID)
wtsp_data_clean$PCRsex <- as.factor(wtsp_data_clean$PCRsex)
wtsp_data_clean$Wing <- as.numeric(wtsp_data_clean$Wing)
wtsp_data_clean$BirdMass <- as.numeric(wtsp_data_clean$BirdMass)
wtsp_data_clean$Tarsus <- as.numeric(wtsp_data_clean$Tarsus)

wing_tarsus_line <- ggplot(wtsp_data_clean, aes(Wing, Tarsus)) + 
  geom_point() +
  geom_smooth(method="lm") +
  theme_bw() +
  ylab("Tarsus Length (mm)") +
  xlab("Wing Chord Length (mm)")

ggsave("wing_vs_tarsus_plot.png", path ="../script/output") #../ moves up a level

mass_tarsus_line <- ggplot(wtsp_data_clean, aes(BirdMass, Tarsus)) + 
  geom_point() +
  geom_smooth(method="lm") +
  theme_bw() +
  ylab("Tarsus Length (mm)") +
  xlab("Mass (g)")

ggsave("mass_vs_tarsus_plot.png", path ="../script/output") #../ moves up a level

sex_tarsus_box <- ggplot(wtsp_data_clean, aes(PCRsex, Tarsus, fill = PCRsex)) + 
  geom_boxplot() +
  theme_bw() +
  xlab("Sex") +
  ylab("Tarsus Length (mm)") +
  theme(legend.position  = "none") +
  scale_fill_brewer(palette = "Blues")

ggsave("sex_vs_tarsus_boxplot.png", path ="../script/output") #../ moves up a level

sex_wing_box <- ggplot(wtsp_data_clean, aes(PCRsex, Wing, fill = PCRsex)) + 
  geom_boxplot() +
  theme_bw() +
  xlab("Sex") +
  ylab("Wing Chord Length (mm)") +
  theme(legend.position  = "none") +
  scale_fill_brewer(palette = "Purples")

ggsave("sex_vs_wing_boxplot.png", path ="../script/output") # up one level works for Rmd and script


wing_tarsus_line / sex_tarsus_box | mass_tarsus_line / sex_wing_box
# patchwork notation for arranging plots
# | puts plots next to eachother while / puts plots over another
# wing tarsus line over sex tarsus box next to mass tarsus line over sex wing box

w_t_model <- lm(Tarsus ~ Wing, data=wtsp_data_clean) # make the model 
summary(w_t_model) # model summary 
coef(w_t_model) # coefficients for slope and r squared

performance::check_model(w_t_model) # checking model assumptions
