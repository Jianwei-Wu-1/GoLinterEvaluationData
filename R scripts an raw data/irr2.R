#!/usr/bin/env Rscript

suppressPackageStartupMessages(require(optparse))
suppressPackageStartupMessages(require(tidyverse))
suppressPackageStartupMessages(require(irr))

# Function to read the survey data
read_survey <- function(path) {
  data = read_csv(path, col_types=cols_only(
    issue = col_character(),
    tool = col_character(),
    question = col_character(),
    participant = col_integer(),
    response = col_factor(levels=c("1", "2", "3", "4", "5")),
  ))
  data
}

# Function to remove outliers in each row by replacing them with the mode
replace_outliers <- function(x) {
  x_num <- as.numeric(as.character(x))
  freq_table <- table(x_num)
  mode_val <- as.numeric(names(which.max(freq_table)))
  if(length(unique(freq_table)) > 1) {
    outlier <- as.numeric(names(which.min(freq_table)))
    x_num[x_num == outlier] <- mode_val
  }
  factor(x_num, levels = c(1, 2, 3, 4, 5))
}

parser <- OptionParser(usage = "%prog [options] <survey.csv> ...", option_list = list(
  make_option("--output", default = NULL, metavar = "PATH", help = "Location to store plot [default \"%default\"]")
))
arguments <- parse_args(parser, positional_arguments = TRUE)

if (length(arguments$args) < 1) {
  print_help(parser)
  stop()
}

# Read and process the data
data = read_survey(arguments$args[1]) |> 
  group_by(issue, tool, question) |> 
  mutate(id = row_number()) |>
  select(-participant) |>
  ungroup()

# Collapsing response levels
data = data |>
  mutate(
    response = fct_collapse(response, 
                            "1" = c("1", "2"),
                            "4" = c("4", "5")
    )
  )

# Remove outliers by replacing them with the mode
data = data |>
  group_by(issue, tool, question) |> 
  mutate(response = replace_outliers(response)) |>
  ungroup()

# Nesting and modeling
data |> 
  nest(.by=question) |> 
  mutate(
    model = map(data, function(df) {
      result = df |> 
        unite("question", issue, tool) |>
        pivot_wider(names_from=id, values_from=response) |>
        select(-question) |>
        kappam.fleiss(exact=TRUE)
      result$value
    }),
  ) |>
  unnest(model) |>
  print(n=Inf)

# Prepare data for kappam.fleiss
irr = data |>
  unite("question", issue, tool, question) |>
  pivot_wider(names_from=id, values_from=response) |>
  select(-question)

#irr |> print(n=Inf)

# Calculate Fleiss' Kappa
result = kappam.fleiss(irr, exact=TRUE)
print(result, n=Inf)
