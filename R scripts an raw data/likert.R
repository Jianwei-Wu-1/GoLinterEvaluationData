#!/usr/bin/env Rscript

suppressPackageStartupMessages(require(optparse))
suppressPackageStartupMessages(require(tidyverse))
suppressPackageStartupMessages(require(likert))

read_survey <- function(path) {
  data = read_csv(path, col_types=cols_only(
    issue = col_character(),
    category = col_character(),
    tool = col_character(),
    question = col_character(),
    participant = col_integer(),
    response = col_factor(levels=c("1", "2", "3", "4", "5"))
  ))
  
  data |> mutate(
    response = fct_recode(response, 
                          "Strongly disagree" = "1", 
                          "Disagree" = "2", 
                          "Neither disagree nor agree" = "3", 
                          "Agree" = "4", 
                          "Strongly agree" = "5")
  )
}

parser <- OptionParser(usage = "%prog [options] <survey.csv> ...", option_list = list(
  make_option("--output", default = NULL, metavar = "PATH", help = "Location to store plot [default \"%default\"]")
))
arguments <- parse_args(parser, positional_arguments = TRUE)

if (length(arguments$args) < 1) {
  print_help(parser)
  stop()
}

if (is.null(arguments$options$output)) {
  arguments$options$output = gsub("\\.csv$", ".pdf", arguments$args[1])
}

data = read_survey(arguments$args[1]) |>
  group_by(category, tool, question, response) |>
  count()

plot = ggplot(data, aes(fill = response, y = tool, x = n)) +
  geom_bar(position = "fill", stat = "identity") +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_fill_brewer(palette = "RdBu", name = "Response") +
  facet_grid(
    cols = vars(question),
    rows = vars(category),
    scales = "free",
    space = "free"
  ) +
  scale_x_continuous(labels = scales::percent) +
  theme_bw() +
  theme(
    legend.position = "bottom"
  ) +
  labs(x = "Percentage of responses", y = "Tool")

ggsave(plot, file = arguments$options$output, width = 8, height = 3.5)
