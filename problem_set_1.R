# STRT 6601 Applied Economics & Modeling ----

# Problem Set 1

# SAXA 3
# Mike Johnson | Kris Lederer | Sebastian Martinez | Ryan Mathis | Khushi Patel

## Set up ----

# Import libraries
library(tidyverse)

# Read data
df = read.csv('ae98.csv')

## Question 1: Provide a table with summary statistics for the relevant variables.

# Create table with summary stats
summary_stats = df %>% 
  summarise_all(list(
    n = ~sum(!is.na(.)),
    mean = ~mean(., na.rm = TRUE),
    sd = ~sd(., na.rm = TRUE),
    min = ~min(., na.rm = TRUE),
    max = ~max(., na.rm = TRUE)
  )) %>% 
  pivot_longer(everything(), # Move columns to rows
               names_to = c("variable", "stat"), # Split variable and stat  
               names_pattern = "(.*)_(.*)") %>%
  pivot_wider(names_from = stat, values_from = value) %>% # Move stat to columns
  mutate(across(where(is.numeric), ~round(., 4))) # Rounding

# Create table with proportions for binary data

binary_vars = c("momworked", "morekids", "moreths", "blackm", "hispm", "whitem", "samesex", "twins2")

binary_summary = df %>% 
  select(binary_vars) %>% 
  summarize_all(mean, na.rm = TRUE) %>% 
  pivot_longer(everything(), names_to = "Variable", values_to = "Proportion") %>% 
  mutate(Proportion = round(Proportion, 2) * 100)


  
