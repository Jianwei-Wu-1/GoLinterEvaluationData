#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(optparse))
suppressPackageStartupMessages(library(forcats))

d1 <- read_csv("data.csv", col_types = cols(
  Project = col_character(),
  IssueCategory = col_double(),
  staticcheck = col_character(),
  govet = col_character(),
  revive = col_character(),
  gosec = col_character(),
  errcheck = col_character()
))


d2 <- d1 %>% pivot_longer(
    cols = c(staticcheck, govet, revive, gosec, errcheck),
    names_to = "linter",
    values_to = "detected",
) %>% rename(category = IssueCategory, issue = Project)


write_csv(d2, "data2.csv", append=FALSE)