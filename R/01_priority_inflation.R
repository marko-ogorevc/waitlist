# 01_priority_inflation.R
# Run AFTER sourcing `import waitlist data NIJZ.R` locally.
# Uses only local NIJZ_db; no raw data is uploaded to GitHub.
# Outputs in local results/ and figures/ (review before publication).

library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(scales)

extrafont::loadfonts()
f_family = "Poppins"

stopifnot(exists("NIJZ_db"))

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


priority_data <- NIJZ_db |> 
  filter(
     TIP_VZS %in% c(
      "Prvi pregled",
      "Diagnostični postopek",
      "Terapevtski postopek"
    ),
    
    !grepl("fizioterapevtska", `VZS naziv`, ignore.case = TRUE)
  )

# chart waiting list ####
overview_chart_wait_all <- 
  priority_data %>%
  
  filter(
    var == "Čakajoči VSI"
  )    |> 
  
  mutate(
    type = factor(TIP_VZS, 
                  levels = c("Prvi pregled", "Diagnostični postopek", "Terapevtski postopek"),
                  labels = c("First specialist consultation", "Diagnostic procedure", "Therapeutic procedure")),
    
    priority_eng = factor(priority,
                          levels = c("Zelo hitro", "Hitro", "Redno"),
                          labels = c("Urgent", "Semi-urgent", "Routine")
    )
  ) %>% 
  
  group_by(date, priority_eng, type) %>% 
  summarise(
    value = sum(value, na.rm = TRUE)
  ) |> 
  group_by(
    date, type
  ) |> 
  mutate(
    value = value / sum(value, na.rm = TRUE)
  ) |> 
  ungroup() |> 
  ggplot(
    aes(x = date, y = value)
  )+
  
  geom_line(aes(color = priority_eng))+
  geom_text(aes(color = priority_eng, 
                label = ifelse(date %in% c(min(date), max(date)),
                               scales::percent(value, big.mark = ",", decimal.mark =".", accuracy = 0.1),
                               NA_character_)),
            show.legend = FALSE)+
  theme_minimal()+
  scale_color_manual(
    values = c("#cd403b", "#748394", "#12989c" ), 
    name = element_blank())+
  
  scale_x_date(name = element_blank(), breaks = "2 years", date_labels = "%b\n%Y", 
               limits = as.Date(c("2019-06-01", "2027-03-31"))) +
  
  scale_y_continuous(name = element_blank(),
                     labels = function(y)  scales::percent(y, big.mark = ",", decimal.mark =".", accuracy = 0.1), 
                     limits = c(NA, NA)
  )+
  
  facet_wrap(
    vars(type),
    scales = "free"
  )+
  
  theme(legend.position = "bottom",
        legend.spacing.y = unit(2, "char"),
        legend.key.height = unit(1.5, "char"),
        legend.key.width = unit(4, "char"),
        text = element_text(
          family = f_family, 
          size = 16),
        axis.line.x = element_line(arrow = grid::arrow(length = unit(0.3, "cm"), 
                                                       ends = "last")
        ),
        axis.title.x = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank()
  )+
  labs(
    subtitle = "Main types of healthcare services monitored through waiting list tracking",
    title = "Share of Patients on Waiting Lists by Priority",
    caption = "Source: The National Institute of Public Health (NIJZ), 2026.\nVisualization: Dr. Marko Ogorevc (IER, DZIIZ)"
  )


# chart realizied ####
overview_chart_realized <- 
  priority_data %>%
  
  filter(
    var == "Realizirani"
  )    |> 
  
  mutate(
    type = factor(TIP_VZS, 
                  levels = c("Prvi pregled", "Diagnostični postopek", "Terapevtski postopek"),
                  labels = c("First specialist consultation", "Diagnostic procedure", "Therapeutic procedure")),
    
    priority_eng = factor(priority,
                          levels = c("Zelo hitro", "Hitro", "Redno"),
                          labels = c("Urgent", "Semi-urgent", "Routine")
    )
  ) %>% 
  
  group_by(date, priority_eng, type) %>% 
  summarise(
    value = sum(value, na.rm = TRUE)
  ) |> 
  group_by(
    date, type
  ) |> 
  mutate(
    value = value / sum(value, na.rm = TRUE)
  ) |> 
  ungroup() |> 
  ggplot(
    aes(x = date, y = value)
  )+
  
  geom_line(aes(color = priority_eng))+
  geom_text(aes(color = priority_eng, 
                label = ifelse(date %in% c(min(date), max(date)),
                               scales::percent(value, big.mark = ",", decimal.mark =".", accuracy = 0.1),
                               NA_character_)),
            show.legend = FALSE)+
  theme_minimal()+
  scale_color_manual(
    values = c("#cd403b", "#748394", "#12989c" ), 
    name = element_blank())+
  
  scale_x_date(name = element_blank(), breaks = "2 years", date_labels = "%b\n%Y", 
               limits = as.Date(c("2019-06-01", "2027-03-31"))) +
  
  scale_y_continuous(name = element_blank(),
                     labels = function(y)  scales::percent(y, big.mark = ",", decimal.mark =".", accuracy = 0.1), 
                     limits = c(NA, NA)
  )+
  
  facet_wrap(
    vars(type),
    scales = "free"
  )+
  
  theme(legend.position = "bottom",
        legend.spacing.y = unit(2, "char"),
        legend.key.height = unit(1.5, "char"),
        legend.key.width = unit(4, "char"),
        text = element_text(
          family = f_family, 
          size = 16),
        axis.line.x = element_line(arrow = grid::arrow(length = unit(0.3, "cm"), 
                                                       ends = "last")
        ),
        axis.title.x = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank()
  )+
  labs(
    subtitle = "Main types of healthcare services monitored through waiting list tracking",
    title = "Share of Completed Healthcare Services by Priority Level",
    caption = "Source: The National Institute of Public Health (NIJZ), 2026.\nVisualization: Dr. Marko Ogorevc (IER, DZIIZ)"
  )

# wating times ####

waiting_times <- priority_data |>
  filter(var %in% c("ČD realizirana", "Realizirani")) |>
  select(
    date, `VZS šifra`, `RIZDDZ šifra izvajalca`, `Naziv izvajalca`,
    TIP_VZS, priority, var, value
  ) |>
  pivot_wider(
    names_from = var,
    values_from = value
  )


waiting_times |>
  summarise(
    n_negative = sum(`ČD realizirana` < 0, na.rm = TRUE),
    n_zero = sum(`ČD realizirana` == 0, na.rm = TRUE),
    min_wait = min(`ČD realizirana`, na.rm = TRUE),
    realized_negative = sum(
      Realizirani[`ČD realizirana` < 0],
      na.rm = TRUE
    )
  )

waiting_times |>
  filter(`ČD realizirana` <= 0) |>
  mutate(
    year = lubridate::year(date),
    status = if_else(
      `ČD realizirana` < 0,
      "Negativna",
      "Ničelna"
    )
  ) |>
  group_by(year, priority, status) |>
  summarise(
    n = n(),
    realized = sum(Realizirani, na.rm = TRUE),
    .groups = "drop"
  ) |> 
  ggplot(
    aes( 
      x = year, y = n)
  )+
  geom_line(aes(color = status, lty = priority))


waiting_times_valid <- waiting_times |>
  filter(
    !is.na(`ČD realizirana`),
    `ČD realizirana` > 0,
    !is.na(Realizirani),
    Realizirani > 0
  )

waiting_times_monthly <- waiting_times_valid |>
  group_by(date, TIP_VZS, priority) |>
  summarise(
    mean_wait = weighted.mean(
      `ČD realizirana`,
      w = Realizirani
    ),
    n_realized = sum(Realizirani),
    .groups = "drop"
  )


overview_chart_waiting_times <- 
  waiting_times_monthly %>%
  
 
  mutate(
    type = factor(TIP_VZS, 
                  levels = c("Prvi pregled", "Diagnostični postopek", "Terapevtski postopek"),
                  labels = c("First specialist consultation", "Diagnostic procedure", "Therapeutic procedure")),
    
    priority_eng = factor(priority,
                          levels = c("Zelo hitro", "Hitro", "Redno"),
                          labels = c("Urgent", "Semi-urgent", "Routine")
    )
  ) %>% 
  
   ungroup() |> 
  ggplot(
    aes(x = date, y = mean_wait)
  )+
  
  geom_line(aes(color = priority_eng))+
  # geom_text(aes(color = priority_eng, 
  #               label = ifelse(date %in% c(min(date), max(date)),
  #                              scales::percent(value, big.mark = ",", decimal.mark =".", accuracy = 0.1),
  #                              NA_character_)),
  #           show.legend = FALSE)+
  theme_minimal()+
  scale_color_manual(
    values = c("#cd403b", "#748394", "#12989c" ), 
    name = element_blank())+
  
  scale_x_date(name = element_blank(), breaks = "2 years", date_labels = "%b\n%Y", 
               limits = as.Date(c("2019-06-01", "2027-03-31"))) +
  
  scale_y_continuous(name = "Days",
                     limits = c(NA, NA)
  )+
  
  facet_wrap(
    vars(type),
    scales = "free_y"
  )+
  
  theme(legend.position = "bottom",
        legend.spacing.y = unit(2, "char"),
        legend.key.height = unit(1.5, "char"),
        legend.key.width = unit(4, "char"),
        text = element_text(
          family = f_family, 
          size = 16),
        axis.line.x = element_line(arrow = grid::arrow(length = unit(0.3, "cm"), 
                                                       ends = "last")
        ),
        axis.title.x = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank()
  )+
  labs(
    subtitle = "Actual waiting times for completed healthcare services, Slovenia, 2020–2026",
    title = "Mean Realized Waiting Time by Priority Level",
    caption = "Source: The National Institute of Public Health (NIJZ), 2026.\nVisualization: Dr. Marko Ogorevc (IER, DZIIZ)"
  )

# razmerje ####

(
  overview_chart_priority_erosion <-
    waiting_times_monthly %>%
    
    filter(
      TIP_VZS %in% c(
        "Prvi pregled",
        "Diagnostični postopek",
        "Terapevtski postopek"
      )
    ) %>%
    
    select(date, TIP_VZS, priority, mean_wait) %>%
    
    pivot_wider(
      names_from = priority,
      values_from = mean_wait
    ) %>%
    
    mutate(
      ratio = `Zelo hitro` / Redno,
      
      type = factor(
        TIP_VZS,
        levels = c(
          "Prvi pregled",
          "Diagnostični postopek",
          "Terapevtski postopek"
        ),
        labels = c(
          "First specialist consultation",
          "Diagnostic procedure",
          "Therapeutic procedure"
        )
      )
    ) %>%
    
    filter(
      is.finite(ratio),
      Redno > 0
    ) %>%
    
    ggplot(
      aes(x = date, y = ratio)
    ) +
    
    geom_hline(
      yintercept = 1,
      linetype = "dashed",
      color = "grey60"
    ) +
    
    geom_line(
      color = "#cd403b",
      linewidth = 0.8
    ) +
    
    theme_minimal() +
    
    scale_x_date(
      name = element_blank(),
      breaks = "2 years",
      date_labels = "%b\n%Y",
      limits = as.Date(c("2019-06-01", "2027-03-31"))
    ) +
    
    scale_y_continuous(
      name = "Waiting time ratio",
      labels = scales::label_number(accuracy = 0.01),
      limits = c(0, NA)
    ) +
    
    facet_wrap(
      vars(type),
      scales = "free_y"
    ) +
    
    theme(
      legend.position = "bottom",
      legend.spacing.y = unit(2, "char"),
      legend.key.height = unit(1.5, "char"),
      legend.key.width = unit(4, "char"),
      
      text = element_text(
        family = f_family,
        size = 16
      ),
      
      axis.line.x = element_line(
        arrow = grid::arrow(
          length = unit(0.3, "cm"),
          ends = "last"
        )
      ),
      
      axis.title.x = element_blank(),
      axis.line.y = element_blank(),
      axis.ticks.y = element_blank()
    ) +
    
    labs(
      title = "Relative Waiting Time by Priority Level",
      subtitle = "Ratio of urgent to routine realized waiting times, Slovenia, 2020–2026",
      caption = "Source: The National Institute of Public Health (NIJZ), 2026.\nVisualization: Dr. Marko Ogorevc (IER, DZIIZ)"
    )
)


# 

(
  overview_chart_waiting_time_gap <-
    waiting_times_monthly %>%
    
    filter(
      TIP_VZS %in% c(
        "Prvi pregled",
        "Diagnostični postopek",
        "Terapevtski postopek"
      )
    ) %>%
    
    select(date, TIP_VZS, priority, mean_wait) %>%
    
    pivot_wider(
      names_from = priority,
      values_from = mean_wait
    ) %>%
    
    mutate(
      gap = Redno - `Zelo hitro`,
      
      type = factor(
        TIP_VZS,
        levels = c(
          "Prvi pregled",
          "Diagnostični postopek",
          "Terapevtski postopek"
        ),
        labels = c(
          "First specialist consultation",
          "Diagnostic procedure",
          "Therapeutic procedure"
        )
      )
    ) %>%
    
    filter(is.finite(gap)) %>%
    
    ggplot(
      aes(x = date, y = gap)
    ) +
    
    geom_hline(
      yintercept = 0,
      linetype = "dashed",
      color = "grey60"
    ) +
    
    geom_line(
      color = "#cd403b",
      linewidth = 0.8
    ) +
    
    theme_minimal() +
    
    scale_x_date(
      name = element_blank(),
      breaks = "2 years",
      date_labels = "%b\n%Y",
      limits = as.Date(c("2019-06-01", "2027-03-31"))
    ) +
    
    scale_y_continuous(
      name = "Difference in waiting time (days)",
      labels = scales::label_number(accuracy = 1)
    ) +
    
    facet_wrap(
      vars(type),
      scales = "free_y"
    ) +
    
    theme(
      legend.position = "bottom",
      legend.spacing.y = unit(2, "char"),
      legend.key.height = unit(1.5, "char"),
      legend.key.width = unit(4, "char"),
      
      text = element_text(
        family = f_family,
        size = 16
      ),
      
      axis.line.x = element_line(
        arrow = grid::arrow(
          length = unit(0.3, "cm"),
          ends = "last"
        )
      ),
      
      axis.title.x = element_blank(),
      axis.line.y = element_blank(),
      axis.ticks.y = element_blank()
    ) +
    
    labs(
      title = "Absolute Waiting Time Gap Between Priority Levels",
      subtitle = "Difference between routine and urgent realized waiting times, Slovenia, 2020–2026",
      caption = "Source: The National Institute of Public Health (NIJZ), 2026.\nVisualization: Dr. Marko Ogorevc (IER, DZIIZ)"
    )
)


# 

first_appointment <- priority_data |>
  filter(var %in% c("ČD prvi termin", "Vpisani")) |>
  select(
    date,
    `VZS šifra`,
    `RIZDDZ šifra izvajalca`,
    `Naziv izvajalca`,
    TIP_VZS,
    priority,
    var,
    value
  ) |>
  pivot_wider(
    names_from = var,
    values_from = value
  )

first_appointment_monthly <- first_appointment |>
  filter(
    !is.na(`ČD prvi termin`),
    `ČD prvi termin` >= 0,
    !is.na(Vpisani),
    Vpisani > 0
  ) |>
  group_by(date, TIP_VZS, priority) |>
  summarise(
    mean_wait = weighted.mean(
      `ČD prvi termin`,
      w = Vpisani
    ),
    n_enrolled = sum(Vpisani),
    .groups = "drop"
  )



(
  overview_chart_first_appointment <-
    first_appointment_monthly %>%
    
    filter(
      TIP_VZS %in% c(
        "Prvi pregled",
        "Diagnostični postopek",
        "Terapevtski postopek"
      )
    ) %>%
    
    mutate(
      type = factor(
        TIP_VZS,
        levels = c(
          "Prvi pregled",
          "Diagnostični postopek",
          "Terapevtski postopek"
        ),
        labels = c(
          "First specialist consultation",
          "Diagnostic procedure",
          "Therapeutic procedure"
        )
      ),
      
      priority_eng = factor(
        priority,
        levels = c("Zelo hitro", "Hitro", "Redno"),
        labels = c("Urgent", "Semi-urgent", "Routine")
      )
    ) %>%
    
    ggplot(
      aes(x = date, y = mean_wait)
    ) +
    
    geom_line(
      aes(color = priority_eng),
      linewidth = 0.8
    ) +
    
    theme_minimal() +
    
    scale_color_manual(
      values = c("#cd403b", "#748394", "#12989c"),
      name = element_blank()
    ) +
    
    scale_x_date(
      name = element_blank(),
      breaks = "2 years",
      date_labels = "%b\n%Y",
      limits = as.Date(c("2019-06-01", "2027-03-31"))
    ) +
    
    scale_y_continuous(
      name = "Days",
      labels = scales::label_number(accuracy = 1)
    ) +
    
    facet_wrap(
      vars(type),
      scales = "free_y"
    ) +
    
    theme(
      legend.position = "bottom",
      legend.spacing.y = unit(2, "char"),
      legend.key.height = unit(1.5, "char"),
      legend.key.width = unit(4, "char"),
      
      text = element_text(
        family = f_family,
        size = 16
      ),
      
      axis.line.x = element_line(
        arrow = grid::arrow(
          length = unit(0.3, "cm"),
          ends = "last"
        )
      ),
      
      axis.title.x = element_blank(),
      axis.line.y = element_blank(),
      axis.ticks.y = element_blank()
    ) +
    
    labs(
      title = "Mean Reported Waiting Time to the First Available or Indicative Appointment",
      subtitle = "Provider-reported waiting times weighted by new appointments, Slovenia, 2020–2026",
      caption = "Source: The National Institute of Public Health (NIJZ), 2026.\nVisualization: Dr. Marko Ogorevc (IER, DZIIZ)"
    )
)


# ####


(
  overview_chart_waiting_times_comparison <-
    
    bind_rows(
      waiting_times_monthly |>
        transmute(
          date, TIP_VZS, priority,
          indicator = "Realized waiting time",
          mean_wait
        ),
      
      first_appointment_monthly |>
        transmute(
          date, TIP_VZS, priority,
          indicator = "Reported appointment wait",
          mean_wait
        )
    ) |>
    
    filter(
      priority == "Zelo hitro",
      TIP_VZS %in% c(
        "Prvi pregled",
        "Diagnostični postopek",
        "Terapevtski postopek"
      )
    ) |>
    
    mutate(
      type = factor(
        TIP_VZS,
        levels = c(
          "Prvi pregled",
          "Diagnostični postopek",
          "Terapevtski postopek"
        ),
        labels = c(
          "First specialist consultation",
          "Diagnostic procedure",
          "Therapeutic procedure"
        )
      ),
      
      indicator = factor(
        indicator,
        levels = c(
          "Realized waiting time",
          "Reported appointment wait"
        )
      )
    ) |>
    
    ggplot(
      aes(x = date, y = mean_wait)
    ) +
    
    geom_line(
      aes(color = indicator),
      linewidth = 0.8
    ) +
    
    theme_minimal() +
    
    scale_color_manual(
      values = c("#cd403b", "#12989c"),
      name = element_blank()
    ) +
    
    scale_x_date(
      name = element_blank(),
      breaks = "2 years",
      date_labels = "%b\n%Y",
      limits = as.Date(c("2019-06-01", "2027-03-31"))
    ) +
    
    scale_y_continuous(
      name = "Days",
      labels = scales::label_number(accuracy = 1)
    ) +
    
    facet_wrap(
      vars(type),
      scales = "free_y"
    ) +
    
    theme(
      legend.position = "bottom",
      legend.spacing.y = unit(2, "char"),
      legend.key.height = unit(1.5, "char"),
      legend.key.width = unit(4, "char"),
      
      text = element_text(
        family = f_family,
        size = 16
      ),
      
      axis.line.x = element_line(
        arrow = grid::arrow(
          length = unit(0.3, "cm"),
          ends = "last"
        )
      ),
      
      axis.title.x = element_blank(),
      axis.line.y = element_blank(),
      axis.ticks.y = element_blank()
    ) +
    
    labs(
      title = "Realized vs. Reported Waiting Times",
      subtitle = "Urgent priority, Slovenia, 2020–2026",
      caption = "Source: The National Institute of Public Health (NIJZ), 2026.\nVisualization: Dr. Marko Ogorevc (IER, DZIIZ)"
    )
)


# ####

priority_vzs <- priority_data |>
  filter(var == "Čakajoči VSI") |>
  group_by(date, `VZS šifra`, TIP_VZS, priority) |>
  summarise(
    n_waiting = sum(value, na.rm = TRUE),
    .groups = "drop"
  ) |>
  group_by(date, `VZS šifra`) |>
  mutate(
    total_waiting = sum(n_waiting),
    share = if_else(
      total_waiting > 0,
      n_waiting / total_waiting,
      NA_real_
    )
  ) |>
  ungroup() |>
  filter(priority == "Zelo hitro")


priority_vzs |>
  summarise(
    n_vzs = n_distinct(`VZS šifra`),
    first_date = min(date),
    last_date = max(date),
    missing_shares = sum(is.na(share))
  )

priority_vzs_comparison <- priority_vzs |>
  filter(
    lubridate::year(date) %in% c(2020, 2026),
    lubridate::month(date) <= 9,
    !is.na(share)
  ) |>
  mutate(year = lubridate::year(date)) |>
  group_by(`VZS šifra`, TIP_VZS, year) |>
  summarise(
    n_months = n(),
    share_urgent = sum(n_waiting) / sum(total_waiting),
    avg_waiting = mean(total_waiting),
    .groups = "drop"
  ) |>
  filter(n_months >= 6) |>
  select(`VZS šifra`, TIP_VZS, year, share_urgent, avg_waiting) |>
  pivot_wider(
    names_from = year,
    values_from = c(share_urgent, avg_waiting)
  ) |>
  filter(
    !is.na(share_urgent_2020),
    !is.na(share_urgent_2026)
  ) |>
  mutate(
    change_pp = 100 * (share_urgent_2026 - share_urgent_2020)
  )

priority_vzs_comparison |>
  summarise(
    n_vzs = n(),
    increased = sum(change_pp > 0),
    decreased = sum(change_pp < 0),
    unchanged = sum(change_pp == 0),
    pct_increased = mean(change_pp > 0) * 100
  )

(
  priority_inflation_distribution <-
    priority_vzs_comparison |>
    ggplot(aes(x = change_pp)) +
    
    geom_histogram(
      binwidth = 5,
      fill = "#12989c",
      color = "white"
    ) +
    
    geom_vline(
      xintercept = 0,
      linetype = "dashed",
      color = "#cd403b"
    ) +
    
    theme_minimal() +
    
    scale_x_continuous(
      name = "Change in urgent priority share (percentage points)"
    ) +
    
    scale_y_continuous(
      name = "Number of healthcare services"
    ) +
    
    theme(
      text = element_text(
        family = f_family,
        size = 16
      )
    ) +
    
    labs(
      title = "Priority Inflation Across Healthcare Services",
      subtitle = "Change in urgent priority share, Jan–Sep 2020 vs. Jan–Sep 2026",
      caption = "Source: The National Institute of Public Health (NIJZ), 2026.\nVisualization: Dr. Marko Ogorevc (IER, DZIIZ)"
    )
)


priority_vzs_comparison |>
  mutate(
    size = pmin(avg_waiting_2020, avg_waiting_2026),
    group = case_when(
      size < 10 ~ "< 10",
      size < 50 ~ "10–49",
      size < 100 ~ "50–99",
      size < 500 ~ "100–499",
      TRUE ~ "500+"
    ),
    group = factor(
      group,
      levels = c("< 10", "10–49", "50–99", "100–499", "500+")
    )
  ) |>
  group_by(group) |>
  summarise(
    n_vzs = n(),
    pct_increased = 100 * mean(change_pp > 0),
    median_change = median(change_pp),
    .groups = "drop"
  )


# #####

priority_provider <- priority_data |>
  filter(
    var == "Čakajoči VSI",
    lubridate::year(date) %in% c(2020, 2026),
    lubridate::month(date) <= 9
  ) |>
  group_by(
    date,
    `VZS šifra`,
    `RIZDDZ šifra izvajalca`,
    `Naziv izvajalca`,
    priority
  ) |>
  summarise(
    n_waiting = sum(value, na.rm = TRUE),
    .groups = "drop"
  ) |>
  group_by(
    date,
    `VZS šifra`,
    `RIZDDZ šifra izvajalca`,
    `Naziv izvajalca`
  ) |>
  mutate(
    total_waiting = sum(n_waiting),
    share_urgent = if_else(
      total_waiting > 0,
      n_waiting / total_waiting,
      NA_real_
    )
  ) |>
  ungroup() |>
  filter(priority == "Zelo hitro")


priority_provider |>
  filter(!is.na(share_urgent)) |>
  mutate(year = lubridate::year(date)) |>
  group_by(year) |>
  summarise(
    n_providers = n_distinct(
      `RIZDDZ šifra izvajalca`,
      `Naziv izvajalca`
    ),
    n_vzs = n_distinct(`VZS šifra`),
    n_combinations = n_distinct(
      `VZS šifra`,
      `RIZDDZ šifra izvajalca`,
      `Naziv izvajalca`
    ),
    .groups = "drop"
  )


priority_provider_comparison <- priority_provider |>
  filter(
    !is.na(share_urgent)
  ) |>
  mutate(year = lubridate::year(date)) |>
  group_by(
    `VZS šifra`,
    `RIZDDZ šifra izvajalca`,
    `Naziv izvajalca`,
    year
  ) |>
  summarise(
    n_months = n(),
    share_urgent = sum(n_waiting) / sum(total_waiting),
    avg_waiting = mean(total_waiting),
    .groups = "drop"
  ) |>
  filter(n_months >= 6) |>
  pivot_wider(
    names_from = year,
    values_from = c(share_urgent, avg_waiting)
  ) |>
  filter(
    !is.na(share_urgent_2020),
    !is.na(share_urgent_2026)
  ) |>
  mutate(
    change_pp = 100 *
      (share_urgent_2026 - share_urgent_2020)
  )


priority_provider_comparison |>
  summarise(
    n_combinations = n(),
    increased = sum(change_pp > 0),
    decreased = sum(change_pp < 0),
    unchanged = sum(change_pp == 0),
    pct_increased = 100 * mean(change_pp > 0),
    median_change = median(change_pp)
  )


# ####

waiting_times_vzs <- waiting_times_valid |>
  filter(priority == "Zelo hitro") |>
  group_by(date, `VZS šifra`) |>
  summarise(
    mean_wait = weighted.mean(
      `ČD realizirana`,
      w = Realizirani
    ),
    n_realized = sum(Realizirani),
    .groups = "drop"
  )

priority_erosion_data <- priority_vzs |>
  filter(priority == "Zelo hitro") |>
  select(
    date, `VZS šifra`, TIP_VZS,
    share, total_waiting
  ) |>
  left_join(
    waiting_times_vzs,
    by = c("date", "VZS šifra")
  )


priority_erosion_data |>
  summarise(
    n = n(),
    n_vzs = n_distinct(`VZS šifra`),
    n_missing_wait = sum(is.na(mean_wait)),
    pct_complete = 100 * mean(!is.na(mean_wait))
  )


waiting_times |>
  filter(priority == "Zelo hitro") |>
  mutate(
    status = case_when(
      is.na(Realizirani) | Realizirani == 0 ~ "Brez realizacij",
      is.na(`ČD realizirana`) ~ "Manjkajoča ČD",
      `ČD realizirana` <= 0 ~ "Neveljavna ali ničelna ČD",
      TRUE ~ "Veljavni podatki"
    )
  ) |>
  count(status)

priority_erosion_data |>
  filter(
    !is.na(share),
    !is.na(mean_wait)
  ) |>
  group_by(`VZS šifra`) |>
  summarise(
    n_months = n_distinct(date),
    .groups = "drop"
  ) |>
  summarise(
    n_vzs = n(),
    median_months = median(n_months),
    vzs_24m = sum(n_months >= 24),
    vzs_48m = sum(n_months >= 48),
    vzs_60m = sum(n_months >= 60)
  )


# panel model ####

# install.packages("fixest")
library(fixest)

panel_data <- priority_erosion_data |>
  filter(
    !is.na(share),
    !is.na(mean_wait),
    mean_wait > 0,
    n_realized > 0
  ) |>
  group_by(`VZS šifra`) |>
  filter(n_distinct(date) >= 48) |>
  ungroup() |>
  mutate(
    log_wait = log1p(mean_wait),
    share_urgent_pp = share * 100,
    month = factor(date)
  )

model_1 <- feols(
  log_wait ~ share_urgent_pp | `VZS šifra` + date,
  data = panel_data,
  vcov = ~ `VZS šifra`
)

summary(model_1)


priority_new <- priority_data |>
  filter(var == "Vpisani") |>
  group_by(date, `VZS šifra`, TIP_VZS, priority) |>
  summarise(
    n_new = sum(value, na.rm = TRUE),
    .groups = "drop"
  ) |>
  group_by(date, `VZS šifra`) |>
  mutate(
    total_new = sum(n_new),
    share_new = if_else(
      total_new > 0,
      n_new / total_new,
      NA_real_
    )
  ) |>
  ungroup() |>
  filter(priority == "Zelo hitro")


panel_new <- priority_new |>
  select(
    date, `VZS šifra`, TIP_VZS,
    share_new, total_new
  ) |>
  left_join(
    waiting_times_vzs,
    by = c("date", "VZS šifra")
  )

panel_new <- panel_new |>
  mutate(
    month_id = lubridate::year(date) * 12 +
      lubridate::month(date)
  ) |>
  left_join(
    priority_new |>
      mutate(
        month_id = lubridate::year(date) * 12 +
          lubridate::month(date) + 1
      ) |>
      select(
        `VZS šifra`,
        month_id,
        share_new_lag1 = share_new
      ),
    by = c("VZS šifra", "month_id")
  )

panel_new |>
  summarise(
    n = n(),
    n_vzs = n_distinct(`VZS šifra`),
    complete_current = sum(
      !is.na(share_new) & !is.na(mean_wait)
    ),
    complete_lagged = sum(
      !is.na(share_new_lag1) & !is.na(mean_wait)
    )
  )


panel_new_model <- panel_new |>
  filter(
    !is.na(share_new_lag1),
    !is.na(mean_wait),
    mean_wait > 0,
    n_realized > 0,
    total_new > 0
  ) |>
  group_by(`VZS šifra`) |>
  filter(n_distinct(date) >= 48) |>
  ungroup() |>
  mutate(
    log_wait = log1p(mean_wait),
    share_new_lag1_pp = share_new_lag1 * 100
  )


model_2 <- feols(
  log_wait ~ share_new_lag1_pp |
    `VZS šifra` + date,
  data = panel_new_model,
  vcov = ~ `VZS šifra`
)

summary(model_2)
