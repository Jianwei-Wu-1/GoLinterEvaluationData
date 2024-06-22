#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(optparse))


df <- read_csv("data2.csv", col_types=cols(
  project = col_character(),
  issue = col_integer(),
  category = col_integer(),
  linter = col_character(),
  detected = col_logical()
))


df <- df %>% 
  unite(col=issue, project, issue)

summary <- df %>% 
  group_by(issue, category) %>% 
  summarize(detected=any(detected), .groups="drop_last") %>%
  mutate(linter="(any)")

df <- bind_rows(df, summary) %>%
  mutate(linter = fct_relevel(linter, "(any)", after=Inf))

plot <- ggplot(df, aes(x=issue, y=fct_rev(linter), fill=detected)) +
  geom_tile() +
  scale_fill_manual(values = c(NA, "#2b8cbe")) +
  labs(
    x="Issue",
    y="Linter",
    fill="Detected"
  ) +
  facet_grid(
    cols=vars(category), # Changed from "category" to "issue"
    rows=vars(linter),
    scales="free", 
    space="free",
  ) +
  theme_bw() +
  theme(
    strip.text.y = element_blank(),
    axis.text.x = element_blank()
  )

ggsave("plot.pdf", plot, width=10, height=2.5)
