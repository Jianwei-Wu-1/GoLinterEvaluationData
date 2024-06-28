#!/usr/bin/env Rscript

suppressPackageStartupMessages(require(optparse))
suppressPackageStartupMessages(require(tidyverse))
suppressPackageStartupMessages(require(irr))


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

parser <- OptionParser(usage = "%prog [options] <survey.csv> ...", option_list = list(
    make_option("--output", default = NULL, metavar = "PATH", help = "Location to store plot [default \"%default\"]")
))
arguments <- parse_args(parser, positional_arguments = TRUE)

if (length(arguments$args) < 1) {
  print_help(parser)
  stop()
}


data = read_survey(arguments$args[1]) |> 
  group_by(issue, tool, question) |> 
  mutate(id = row_number()) |>
  select(-participant) |>
  ungroup()

data = data |>
  mutate(
    response = fct_collapse(response, 
      "1" = c("1", "2"),
      "4" = c("4", "5")
    )
  )

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

irr = data |>
  unite("question", issue, tool, question) |>
  pivot_wider(names_from=id, values_from=response) |>
  select(-question)

 # irr |> print(n=Inf)

result = kappam.fleiss(irr, exact=TRUE)
print(result)