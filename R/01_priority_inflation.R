# =============================================================================
# WAITING LISTS: PRIORITY INFLATION AND ACCESS INDICATORS
# =============================================================================
# Input: NIJZ_db, created locally by 'import waitlist data NIJZ.R'.
# Period: all available months (currently January 2020–September 2026).
# Unit of observation: VZS x provider x month x priority x measure.
# Source data remain local; commit only code or reviewed aggregate outputs.
#
# ANALYTICAL SAMPLE
# - First consultations, diagnostics, and therapeutic procedures.
# - Excludes control consultations, start-of-treatment codes and physiotherapy.
#   This is an intentional sample definition, not all monitored services.
# - Waiting-list entries are not necessarily unique individuals.
#
# INTERPRETATION / DATA QUALITY
# - 'Čakajoči VSI': end-of-month waiting-list stock.
# - 'Vpisani': monthly new registrations (flow).
# - 'Realizirani': completed services (flow).
# - 'ČD realizirana': provider-level mean realized waiting time.
# - 'ČD prvi termin': monthly mean of daily reported first/indicative dates.
#   Actual available dates and indicative dates cannot be separated here.
# - NA may represent zero activity or missing reporting; summations with
#   na.rm = TRUE are retained from the exploratory analysis and need a
#   missingness sensitivity check before publication.
# - A reporting-definition break affects 'Čakajoči NAD' in August 2024;
#   this script does not use that measure.
# - Administrative urgency-code reporting anomalies remain possible.
# - Descriptive associations / fixed-effects regressions do NOT establish
#   inappropriate triage or a causal effect on waiting times.
#
# SECTION INDEX
# 1. Sample and priority composition
# 2. Realized waiting times and relative/absolute gaps
# 3. Reported appointment waits and comparison
# 4. Priority inflation across VZS and providers
# 5. Exploratory fixed-effects models
# =============================================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(fixest)

extrafont::loadfonts()
f_family <- "Poppins"

stopifnot(exists("NIJZ_db"))

# 1. ANALYTICAL SAMPLE AND PRIORITY COMPOSITION ------------------------------
priority_data <- NIJZ_db |> 
  filter(
     TIP_VZS %in% c(
      "Prvi pregled",
      "Diagnostični postopek",
      "Terapevtski postopek"
    ),
    
    !grepl("fizioterapevtska", `VZS naziv`, ignore.case = TRUE)
  )

# 1.1 Waiting-list stock: monthly urgency shares -------------------------------
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


# 1.2 Completed services: monthly urgency shares -------------------------------
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

# 2. REALIZED WAITING TIMES ----------------------------------------------------
# Include provider name in the key: RIZDDZ code alone is not unique enough.
# Provider-level averages must be weighted by their corresponding completed
# services (Realizirani) before aggregating by service type and urgency.

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


# 2.1 Data-quality check: negative and zero realized waiting times ------------
# Negative durations are invalid. Zero durations can be legitimate, but this
# exploratory analysis excludes both and should be checked for sensitivity.
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


# 2.2 Weighted monthly means --------------------------------------------------
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


# 2.3 Chart: weighted realized waiting times ---------------------------------
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

# 2.4 Chart: urgent / routine realized-wait ratio ------------------------------
# A rising ratio toward 1 signals less relative differentiation, not
# necessarily deterioration in absolute urgency advantage.

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


# 2.5 Chart: routine minus urgent gap in days ---------------------------------
# Absolute and relative gaps may move in opposite directions.
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


# 3. REPORTED FIRST / INDICATIVE APPOINTMENT WAIT -----------------------------
# The published monthly number is an average of daily dates posted by each
# provider and VZS, mixing actual first available and indicative appointments.
# Vpisani is an enrollment-weight used as a proxy; this does NOT estimate
# patient-experienced waiting time.
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
  # 3.1 Chart: enrollment-weighted reported appointment wait ------------------
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


# 3.2 Chart: realized versus reported waiting times, urgent only --------------
# Distinct definitions and distinct weights: diagnostic comparison only.
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


# 4. PRIORITY INFLATION ACROSS HEALTHCARE SERVICES ---------------------------
# End-of-month stock; service-month proportions of 'Zelo hitro'.
# Months with no waiting entries have undefined shares and are excluded.
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

# 4.1 Comparable VZS: January–September 2020 versus 2026 ---------------------
# At least six months with positive waiting-list totals in EACH period.
# Period shares are ratios of summed monthly counts, not simple monthly means.
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

# 4.2 Histogram: percentage-point changes across VZS ------------------------
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


# 4.3 Robustness: grouping by minimum mean stock in the two periods ----------
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


# 4.4 Stable provider–VZS pairs ------------------------------------------------
# Use RIZDDZ code AND provider name. Renamed providers may fail to match.
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


# 5. EXPLORATORY FIXED-EFFECTS REGRESSIONS ----------------------------------
# Outcome: log(1 + mean realized wait), urgent category; VZS and date FE.
# Standard errors clustered by VZS. Models are ASSOCIATIONAL, not causal.
# Means exclude zero / negative durations; evaluate selection and missingness.
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


# 5.1 Model 1: contemporaneous urgent share in waiting-list stock -------------

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
    share_urgent_pp = share * 100
  )

model_1 <- feols(
  log_wait ~ share_urgent_pp | `VZS šifra` + date,
  data = panel_data,
  vcov = ~ `VZS šifra`
)

summary(model_1)


# 5.2 New registrations (flow) and month-lag construction --------------------
# Calendar month ID prevents a missing month from being treated as the
# previous month's observation. Lagged share remains observational.
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


# 5.3 Model 2: urgent share among new registrations, lagged one month --------
# The two models currently use different estimation samples; coefficients
# should not be compared as if they had identical observations.
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
