# 01_priority_inflation.R
# Run AFTER sourcing `import waitlist data NIJZ.R` locally.
# Uses only local NIJZ_db; no raw data is uploaded to GitHub.
# Outputs in local results/ and figures/ (review before publication).

library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(scales)

stopifnot(exists("NIJZ_db"))
required <- c("date", "var", "priority", "value", "VZS šifra")
missing_columns <- setdiff(required, names(NIJZ_db))
if (length(missing_columns)) stop("Missing columns: ", paste(missing_columns, collapse=", "))

dir.create("results", showWarnings = FALSE)
dir.create("figures", showWarnings = FALSE)

# Important methodological choices:
# - 'Čakajoči VSI' = waiting-list STOCK at end of month.
# - 'Realizirani' = monthly activity FLOW.
# - NA may mean zero or unavailable: keep NA distinct from zero.
# - These are observational indicators, not causal evidence of priority erosion.
# - 2020–2025 is the abstract's study period; 2026 can be studied separately.
# - Aug 2024 changed the operational definition of 'Čakajoči NAD';
#   never pool its values across that break as a methodologically stable series.
# - Do not calculate national mean waiting times by simply averaging
#   provider-level means; weight by corresponding realized counts after
#   verifying exactly aligned definitions and observation coverage.

priority_levels <- c("Redno", "Hitro", "Zelo hitro")
valid_vars <- c("Čakajoči VSI", "Realizirani")

source_counts <- NIJZ_db |>
  mutate(
    date = as.Date(date),
    priority = as.character(priority),
    value = suppressWarnings(as.numeric(value))
  ) |>
  filter(
    date >= as.Date("2020-01-01"),
    date <= as.Date("2025-12-31"),
    priority %in% priority_levels,
    var %in% valid_vars
  )

if (nrow(source_counts) == 0) stop("No records matching the study period and variable labels.")

# Inspect missingness by month/variable before interpreting the trend.
missingness <- source_counts |>
  group_by(date, var, priority) |>
  summarise(
    n_rows = n(),
    n_missing = sum(is.na(value)),
    .groups = "drop"
  )
write_csv(missingness, "results/priority_data_completeness.csv")

# Aggregate within date and priority. All-missing groups remain NA.
monthly <- source_counts |>
  group_by(date, var, priority) |>
  summarise(
    count = if (all(is.na(value))) NA_real_ else sum(value, na.rm=TRUE),
    n_missing = sum(is.na(value)),
    .groups = "drop"
  ) |>
  group_by(date, var) |>
  mutate(
    denominator = if (any(is.na(count))) NA_real_ else sum(count),
    share = if_else(!is.na(denominator) & denominator > 0,
                    count / denominator, NA_real_)
  ) |>
  ungroup()

# Flag incomplete date-variable panels (missing urgency categories).
category_coverage <- monthly |>
  group_by(date, var) |>
  summarise(n_categories = n_distinct(priority), .groups="drop")
monthly <- monthly |>
  left_join(category_coverage, by=c("date", "var")) |>
  mutate(
    share = if_else(n_categories == length(priority_levels) &
                      n_missing == 0L & is.finite(share),
                    share, NA_real_)
  )
# NOTE: This conservative rule drops a category's share if its own counts are
# incomplete. For a fully complete comparison across categories, use
# `complete_panel` below.
complete_panel <- monthly |>
  group_by(date, var) |>
  mutate(
    panel_complete = n() == length(priority_levels) &&
      all(n_missing == 0) && all(!is.na(count)),
    share = if_else(panel_complete, share, NA_real_)
  ) |>
  ungroup()

write_csv(complete_panel, "results/priority_shares_monthly.csv")

p <- complete_panel |>
  filter(var == "Čakajoči VSI") |>
  ggplot(aes(x=date, y=share, colour=priority)) +
  geom_line(linewidth=0.9, na.rm=TRUE) +
  scale_y_continuous(labels=percent_format(accuracy=1)) +
  scale_x_date(date_breaks="1 year", date_labels="%Y") +
  labs(
    title="Urgency composition of the waiting-list stock",
    subtitle="Monthly end-of-month shares, Slovenia, 2020–2025",
    x=NULL, y="Share of patients", colour="Urgency"
  ) +
  theme_minimal(base_size=13)

ggsave("figures/priority_trends_stock.svg", p, width=11, height=6, bg="white")
ggsave("figures/priority_trends_stock.png", p, width=11, height=6, dpi=180, bg="white")

p_flow <- complete_panel |>
  filter(var == "Realizirani") |>
  ggplot(aes(x=date, y=share, colour=priority)) +
  geom_line(linewidth=0.9, na.rm=TRUE) +
  scale_y_continuous(labels=percent_format(accuracy=1)) +
  scale_x_date(date_breaks="1 year", date_labels="%Y") +
  labs(
    title="Urgency composition of realized services",
    subtitle="Monthly shares of realized activity, Slovenia, 2020–2025",
    x=NULL, y="Share of services", colour="Urgency"
  ) +
  theme_minimal(base_size=13)

ggsave("figures/priority_trends_flow.svg", p_flow, width=11, height=6, bg="white")
ggsave("figures/priority_trends_flow.png", p_flow, width=11, height=6, dpi=180, bg="white")

message("Outputs saved locally in results/ and figures/. Review missingness before publishing.")
