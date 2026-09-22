	# Exercise 03: Importing and Tidying Data
	# Estimated time: 30 minutes
	#
	# In this exercise, you will work with fish-count data from two parts
	# of an estuary. The observations are split across two CSV files.
	#
	# Skills practiced:
	# - read_csv()
	# - glimpse() and names()
	# - bind_rows()
	# - pivot_longer()
	# - separate_wider_regex()
	# - mutate() and recode()
	# - group_by() and summarize()
	# - write_csv()
	
	library(tidyverse)
	library(here)
	
	
	# -------------------------------------------------------------------------
	# Part 1: Import and inspect the data
	# Suggested time: 6 minutes
	# -------------------------------------------------------------------------
	
	# Create a path to the folder containing the two CSV files.
	
	data_file_path <- here(
	  "week-03",
	  "exercises",
	  "data")
	
	
	# Import both files.
	# In these files, a period (.) represents a missing value.
	
	upper_bay <- read_csv(
	  file.path(
	    data_file_path,
	    "upper-bay-fish-counts.csv"
	  ),
	  na = c("", "NA", "."))
	
	lower_bay <- read_csv(
	  file.path(
	    data_file_path,
	    "lower-bay-fish-counts.csv"
	  ),
	  na = c("", "NA", "."))
	
	
	# Inspect both data frames.
	
	glimpse(lower_bay)
	glimpse(upper_bay)
	
	names(upper_bay)
	names(lower_bay)
	
	
	# Answer in comments:
	#
	# 1. Do the two files contain the same variables?
	
	# Yes, they each have listed: region, site, survey_year, codj, coda, etc. 
	
	# 2. What does one row currently represent?
	
	# One row represents one site during one year in one region. 
	
	# 3. Which column names appear to contain more than one variable?
	All of the ones after survey_year
	
	# -------------------------------------------------------------------------
	# Part 2: Combine the tables
	# Suggested time: 4 minutes
	# -------------------------------------------------------------------------
	
	# The two tables contain the same variables but observations from
	# different regions. Combine them into one data frame named fish_wide.
	
	fish_wide <- bind_rows(
	  upper_bay,
	  lower_bay
	)
	
	fish_wide
	
	
	# Check the dimensions of the combined data frame.
	
	dim(fish_wide)
	
	
	# Answer in a comment:
	# 12  9
	# What does bind_rows() do?
	 it merges the tables into one 
	
	
	# -------------------------------------------------------------------------
	# Part 3: Pivot the count columns longer
	# Suggested time: 7 minutes
	# -------------------------------------------------------------------------
	
	# The columns `codj` through `pola` contain fish counts.
	# Their names encode both species and life stage.
	#
	# Pivot these columns into:
	# - a new column named species_stage
	# - a new column named count
	#
	# Drop rows in which count is missing.
	
	fish_long <- fish_wide |>
	  pivot_longer(
	    cols = codj:pola,
	    names_to = "species_stage",
	    values_to = "count",
	    values_drop_na = TRUE
	  )
	
	fish_long
	
	
	# Inspect the result.
	
	glimpse(fish_long)
	
	
	# Answer in comments:
	#
	# 1. What does one row represent before the pivot?
	# one row is one site, one year, and one region combined
	# 2. What does one row represent after the pivot?
	# After the pivot, it includes the species stage and count
	# 3. Why does species_stage still violate the rules of tidy data?
	because its still combining species and life stage
	
	
	# -------------------------------------------------------------------------
	# Part 4: Separate the encoded variables
	# Suggested time: 7 minutes
	# -------------------------------------------------------------------------
	
	# Each value in species_stage contains:
	# - a three-letter species code: `cod`, `had`, or `pol`
	# - a one-letter life-stage code: `j` or `a`
	#
	# Use `separate_wider_regex()` to create columns named species
	# and life_stage.
	
	fish_tidy <- fish_long |>
	  separate_wider_regex(
	    species_stage,
	    patterns = c(
	      species = "[a-z]{3}",
	      life_stage = "[ja]"
	    )
	  )
	
	fish_tidy
	
	
	# Replace the abbreviated codes with readable labels.
	
	fish_tidy <- fish_tidy |>
	  mutate(
	    species = recode(
	      species,
	      cod = "Atlantic cod",
	      had = "Haddock",
	      pol = "Pollock"
	    ),
	    life_stage = recode(
	      life_stage,
	      j = "juvenile",
	      a = "adult"
	    )
	  )
	
	fish_tidy
	
	
	# Answer in a comment:
	#
	# What does one row represent in fish_tidy?
	# One row represents the count for one species and life stage at one site and in one region, during one survey year
	
	
	# -------------------------------------------------------------------------
	# Part 5: Summarize and export the tidy data
	# Suggested time: 6 minutes
	# -------------------------------------------------------------------------
	
	# Calculate the total number of fish observed for each species
	# and life stage.
	
	fish_summary <- fish_tidy |>
	  group_by(
	    species,
	    life_stage
	  ) |>
	  summarize(
	    total_count = sum(count),
	    .groups = "drop" # optional, removes any groupings that may affect later code
	  )
	
	fish_summary
	
	
	# Create a path for an output file and export the tidy data.
	
	output_file <- file.path(
	  data_file_path,
	  "fish-counts-tidy.csv"
	)
	
	write_csv(
	  fish_tidy,
	  output_file
	)
	
	file.exists(output_file)
	
	
	# Answer in comments:
	#
	# 1. Which species had the largest total count?
	# Pollock
	# 2. Were more juveniles or adults observed overall?
	# there were more juveniles
	
	
	# -------------------------------------------------------------------------
	# Create a small data frame by hand
	# -------------------------------------------------------------------------
	
	# Use tribble() to create a data frame named species_key containing:
	#
	# Atlantic cod | Gadus morhua
	# Haddock      | Melanogrammus aeglefinus
	# Pollock      | Pollachius virens
	
	species_key <- tribble(
	  ~common_name, ~scientific_name,
	  "Atlantic cod", "Gadus morhua",
	  "Haddock", "Melanogrammus",
	  "Pollock", "Pollachius virens",
	)
	
	species_key
	str(species_key)
