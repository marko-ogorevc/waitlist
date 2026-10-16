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
# 5. Provider-level comparison and quarterly panel
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
  geom_text(
    data = \(d) d |>
      group_by(type, priority_eng) |>
      filter(date %in% range(date)) |>
      mutate(
        label_date = if_else(
          date == min(date),
          date - lubridate::days(60),
          date + lubridate::days(60)
        ),
        hjust_label = if_else(date == min(date), 1, 0)
      ) |>
      ungroup(),
    
    aes(
      x = label_date,
      y = value,
      color = priority_eng,
      label = scales::percent(value, accuracy = 0.1),
      hjust = hjust_label
    ),
    show.legend = FALSE
    )+
  theme_minimal()+
  scale_color_manual(
    values = c("#cd403b", "#748394", "#12989c" ), 
    name = element_blank())+
  
  scale_x_date(name = element_blank(), breaks = "2 years", date_labels = "%b\n%Y", 
               limits = as.Date(c("2019-01-01", "2027-12-31"))) +
  
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
  geom_text(
    data = \(d) d |>
      group_by(type, priority_eng) |>
      filter(date %in% range(date)) |>
      mutate(
        label_date = if_else(
          date == min(date),
          date - lubridate::days(60),
          date + lubridate::days(60)
        ),
        hjust_label = if_else(date == min(date), 1, 0)
      ) |>
      ungroup(),
    
    aes(
      x = label_date,
      y = value,
      color = priority_eng,
      label = scales::percent(value, accuracy = 0.1),
      hjust = hjust_label
    ),
    show.legend = FALSE
  )+
  theme_minimal()+
  scale_color_manual(
    values = c("#cd403b", "#748394", "#12989c" ), 
    name = element_blank())+
  
  scale_x_date(name = element_blank(), breaks = "2 years", date_labels = "%b\n%Y", 
               limits = as.Date(c("2019-01-01", "2027-12-31"))) +
  
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


# 2.1 QUALITY AUDIT: REALIZED WAITING TIMES ----------------------------------
# A record is a provider × VZS × month × urgency aggregated mean (not a patient).
# Negative realized mean = logically invalid; zero and 0 < wait < 1 day may
# be legitimate. A very short group MEAN does not mean all patients waited
# less than one day. Missing is not interchangeable with zero.
# Main models below retain ONLY strictly positive means and completions > 0.
# Tables also show completed-service weights; counts are not unique patients.

waiting_time_quality <- waiting_times |>
  mutate(
    year = lubridate::year(date),
    valid_volume = !is.na(Realizirani) & is.finite(Realizirani) & Realizirani > 0,
    wait_class = case_when(
      is.na(`ČD realizirana`) ~ "Missing",
      !is.finite(`ČD realizirana`) ~ "Non-finite",
      `ČD realizirana` < 0 ~ "Negative",
      `ČD realizirana` == 0 ~ "Zero",
      `ČD realizirana` < 1 ~ "Between 0 and 1 day",
      `ČD realizirana` < 7 ~ "1 to under 7 days",
      TRUE ~ "7 days or more"
    ),
    wait_class = factor(
      wait_class,
      levels = c("Negative", "Zero", "Between 0 and 1 day",
                 "1 to under 7 days", "7 days or more",
                 "Missing", "Non-finite")
    )
  )

# Count records regardless of completion reporting; count completions only
# where volume is valid. Invalid volume is flagged rather than used as weight.
waiting_quality_overall <- waiting_time_quality |>
  group_by(wait_class) |>
  summarise(
    n_records = n(),
    n_valid_volume_records = sum(valid_volume),
    n_completions = sum(Realizirani[valid_volume]),
    n_missing_or_invalid_volume = sum(!valid_volume),
    .groups = "drop"
  )

waiting_quality_year <- waiting_time_quality |>
  group_by(year, wait_class) |>
  summarise(
    n_records = n(),
    n_completions = sum(Realizirani[valid_volume]),
    .groups = "drop"
  )

waiting_quality_priority <- waiting_time_quality |>
  filter(valid_volume) |>
  group_by(priority, wait_class) |>
  summarise(
    n_records = n(),
    n_completions = sum(Realizirani),
    .groups = "drop"
  ) |>
  group_by(priority) |>
  mutate(
    pct_records = 100 * n_records / sum(n_records),
    pct_completions = 100 * n_completions / sum(n_completions)
  ) |>
  ungroup()

# Optional inspection of logically impossible records, excluding details
# from published presentations to avoid disclosing provider-level reporting.
waiting_quality_negative <- waiting_time_quality |>
  filter(wait_class == "Negative") |>
  summarise(
    n_records = n(),
    min_wait = if (n() > 0) min(`ČD realizirana`) else NA_real_,
    n_completions = sum(Realizirani[valid_volume])
  )

print(waiting_quality_overall)
print(waiting_quality_negative)
print(waiting_quality_priority)

# 2.1a Visual diagnostic by reporting year (counts, not patient counts).
overview_chart_wait_quality <- waiting_quality_year |>
  filter(wait_class %in% c("Negative", "Zero", "Between 0 and 1 day")) |>
  ggplot(aes(x = factor(year), y = n_records, fill = wait_class)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.7) +
  scale_fill_manual(
    values = c("Negative" = "#cd403b",
               "Zero" = "#748394",
               "Between 0 and 1 day" = "#12989c"),
    drop = FALSE
  ) +
  labs(
    x = "Year", y = "Provider–service–month–priority records",
    fill = NULL,
    title = "Data Quality: Implausible and Short Realized Waiting Times",
    subtitle = "Reported mean waiting times per provider, VZS, month and priority",
    caption = "Source: NIJZ. Zero and short waits can be valid; negative waits are invalid."
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

# Sensitivity to zero waits should be tested separately: ratios and log(wait)
# cannot include a zero numerator or denominator without redefining outcome.


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
  
  geom_text(
    data = \(d) d |>
      group_by(type, priority_eng) |>
      filter(date %in% range(date)) |>
      mutate(
        label_date = if_else(
          date == min(date),
          date - lubridate::days(60),
          date + lubridate::days(60)
        ),
        hjust_label = if_else(date == min(date), 1, 0)
      ) |>
      ungroup(),
    
    aes(
      x = label_date,
      y = mean_wait,
      color = priority_eng,
      label = scales::number(mean_wait, accuracy = 0.1),
      hjust = hjust_label
    ),
    show.legend = FALSE
  )+
  
  theme_minimal()+
  scale_color_manual(
    values = c("#cd403b", "#748394", "#12989c" ), 
    name = element_blank())+
  
  scale_x_date(name = element_blank(), breaks = "2 years", date_labels = "%b\n%Y", 
               limits = as.Date(c("2019-01-01", "2027-12-31"))) +
  
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
    
    geom_text(
      data = \(d) d |>
        group_by(type) |>
        filter(date %in% range(date)) |>
        mutate(
          label_date = if_else(
            date == min(date),
            date - lubridate::days(60),
            date + lubridate::days(60)
          ),
          hjust_label = if_else(date == min(date), 1, 0)
        ) |>
        ungroup(),
      
      aes(
        x = label_date,
        y = ratio,
        # color = priority_eng,
        label = scales::percent(ratio, accuracy = 0.1),
        hjust = hjust_label
      ),
      show.legend = FALSE
    )+
    
    theme_minimal() +
    
    scale_x_date(
      name = element_blank(),
      breaks = "2 years",
      date_labels = "%b\n%Y",
      limits = as.Date(c("2019-01-01", "2027-12-31"))
    ) +
    
    scale_y_continuous(
      name = "Waiting time ratio\n(Urgent / Routine)",
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
    
    
    geom_text(
      data = \(d) d |>
        group_by(type) |>
        filter(date %in% range(date)) |>
        mutate(
          label_date = if_else(
            date == min(date),
            date - lubridate::days(60),
            date + lubridate::days(60)
          ),
          hjust_label = if_else(date == min(date), 1, 0)
        ) |>
        ungroup(),
      
      aes(
        x = label_date,
        y = gap,
        label = scales::number(gap, accuracy = 0.1),
        hjust = hjust_label
      ),
      color = "#cd403b",
      show.legend = FALSE
    )+
    
    theme_minimal() +
    
    scale_x_date(
      name = element_blank(),
      breaks = "2 years",
      date_labels = "%b\n%Y",
      limits = as.Date(c("2019-01-01", "2027-12-31"))
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
    
    geom_text(
      data = \(d) d |>
        group_by(type, priority_eng) |>
        filter(date %in% range(date)) |>
        mutate(
          label_date = if_else(
            date == min(date),
            date - lubridate::days(60),
            date + lubridate::days(60)
          ),
          hjust_label = if_else(date == min(date), 1, 0)
        ) |>
        ungroup(),
      
      aes(
        x = label_date,
        y = mean_wait,
        label = scales::number(mean_wait, accuracy = 0.1),
        hjust = hjust_label, 
        color = priority_eng
      ),
     
      show.legend = FALSE
    )+
    
    theme_minimal() +
    
    scale_color_manual(
      values = c("#cd403b", "#748394", "#12989c"),
      name = element_blank()
    ) +
    
    scale_x_date(
      name = element_blank(),
      breaks = "2 years",
      date_labels = "%b\n%Y",
      limits = as.Date(c("2019-01-01", "2027-12-31"))
    ) +
    
    scale_y_continuous(
      name = "Days",
      labels = scales::label_number(accuracy = 1)
    ) +
    
    facet_wrap(
      vars(type)
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
    
    geom_text(
      data = \(d) d |>
        group_by(type, indicator) |>
        filter(date %in% range(date)) |>
        mutate(
          label_date = if_else(
            date == min(date),
            date - lubridate::days(60),
            date + lubridate::days(60)
          ),
          hjust_label = if_else(date == min(date), 1, 0)
        ) |>
        ungroup(),
      
      aes(
        x = label_date,
        y = mean_wait,
        label = scales::number(mean_wait, accuracy = 0.1),
        hjust = hjust_label, 
        color = indicator
        ),
      
      
      show.legend = FALSE
    )+
    
    theme_minimal() +
    
    scale_color_manual(
      values = c("#cd403b", "#12989c"),
      name = element_blank()
    ) +
    
    scale_x_date(
      name = element_blank(),
      breaks = "2 years",
      date_labels = "%b\n%Y",
      limits = as.Date(c("2019-01-01", "2027-12-31"))
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



# 5. PROVIDER × VZS COMPARISONS: RELATIVE WAITING-TIME DIFFERENTIATION -------
# Replaces preliminary VZS-only regressions. Unit: provider × service × period.
# Main pairwise criterion: at least 10 completed services in EACH priority
# in each included month; at least 6 valid months in Jan–Sep of BOTH years.
# Sensitivity: >=50 completions in EACH priority per calendar quarter,
# and >=2 valid quarters in Q1–Q3 of BOTH years.
# Selected, higher-volume services are not representative of all VZS.
# Excluding zero mean waits is an analytical choice requiring sensitivity work.
# Provider names are retained in identifiers; changes of name can break a pair.

provider_panel <- waiting_times_valid |>
  filter(priority %in% c("Zelo hitro", "Hitro", "Redno")) |>
  group_by(date, `RIZDDZ šifra izvajalca`, `Naziv izvajalca`,
           `VZS šifra`, TIP_VZS, priority) |>
  summarise(
    mean_wait = weighted.mean(`ČD realizirana`, Realizirani),
    n_realized = sum(Realizirani),
    .groups = "drop"
  ) |>
  pivot_wider(names_from = priority, values_from = c(mean_wait, n_realized))

# Helper: all pairwise comparisons, numerator = more urgent priority.
make_priority_pairs <- function(wide_data, time_col, min_n) {
  key_cols <- c(time_col, "RIZDDZ šifra izvajalca",
                "Naziv izvajalca", "VZS šifra", "TIP_VZS")
  make_one <- function(label, high, low) {
    wide_data |>
      transmute(
        across(all_of(key_cols)),
        comparison = label,
        wait_high = .data[[paste0("mean_wait_", high)]],
        wait_low = .data[[paste0("mean_wait_", low)]],
        n_high = .data[[paste0("n_realized_", high)]],
        n_low = .data[[paste0("n_realized_", low)]]
      )
  }
  bind_rows(
    make_one("Urgent / Routine", "Zelo hitro", "Redno"),
    make_one("Semi-urgent / Routine", "Hitro", "Redno"),
    make_one("Urgent / Semi-urgent", "Zelo hitro", "Hitro")
  ) |>
    filter(is.finite(wait_high), is.finite(wait_low),
           wait_high > 0, wait_low > 0,
           n_high >= min_n, n_low >= min_n) |>
    mutate(relative_wait = wait_high / wait_low,
           absolute_gap = wait_low - wait_high)
}

provider_keys <- c("RIZDDZ šifra izvajalca",
                   "Naziv izvajalca", "VZS šifra")

# 5.1 Monthly comparison: 2020 vs 2026 --------------------------------------
priority_pairs <- make_priority_pairs(provider_panel, "date", 10)

pair_coverage <- priority_pairs |>
  filter(lubridate::year(date) %in% c(2020, 2026),
         lubridate::month(date) <= 9) |>
  mutate(year = lubridate::year(date)) |>
  count(comparison, across(all_of(provider_keys)), year, name = "n_months") |>
  filter(n_months >= 6) |>
  select(-n_months) |>
  pivot_wider(names_from = year, values_from = year, names_prefix = "year_") |>
  filter(!is.na(year_2020), !is.na(year_2026))

common_pairs <- pair_coverage |>
  count(across(all_of(provider_keys)), name = "n_comparisons") |>
  filter(n_comparisons == 3) |>
  select(all_of(provider_keys))

# Annual means of provider-level waiting times are weighted by completed care.
# First aggregate numerator and denominator separately; THEN form their ratio.
pair_change <- priority_pairs |>
  semi_join(pair_coverage, by = c("comparison", provider_keys)) |>
  filter(lubridate::year(date) %in% c(2020, 2026),
         lubridate::month(date) <= 9) |>
  mutate(year = lubridate::year(date)) |>
  group_by(comparison, across(all_of(provider_keys)), year) |>
  summarise(wait_high = weighted.mean(wait_high, n_high),
            wait_low = weighted.mean(wait_low, n_low),
            .groups = "drop") |>
  mutate(ratio = wait_high / wait_low,
         gap = wait_low - wait_high) |>
  select(comparison, all_of(provider_keys), year, ratio, gap) |>
  pivot_wider(names_from = year, values_from = c(ratio, gap),
              names_sep = "_") |>
  mutate(change_ratio = ratio_2026 - ratio_2020,
         change_gap = gap_2026 - gap_2020)

# Helper for paired comparisons. The signed-rank p-value describes the
# location shift of the paired DIFFERENCES; it is not a causal test.
summarise_pair_change <- function(dat) {
  dat |>
    group_by(comparison) |>
    summarise(
      n = n(),
      median_2020 = median(ratio_2020),
      median_2026 = median(ratio_2026),
      median_change = median(change_ratio),
      pct_increased = 100 * mean(change_ratio > 0),
      median_gap_change = median(change_gap),
      p_value = wilcox.test(change_ratio, mu = 0, exact = FALSE)$p.value,
      .groups = "drop"
    )
}
pair_summary <- summarise_pair_change(pair_change)
pair_summary_common <- pair_change |>
  semi_join(common_pairs, by = provider_keys) |>
  summarise_pair_change()
print(pair_summary)
print(pair_summary_common)

# 5.2 Quarterly sensitivity analysis: >=50 in each priority ------------------
provider_quarterly <- waiting_times_valid |>
  filter(priority %in% c("Zelo hitro", "Hitro", "Redno")) |>
  mutate(quarter = lubridate::floor_date(date, "quarter")) |>
  group_by(quarter, `RIZDDZ šifra izvajalca`, `Naziv izvajalca`,
           `VZS šifra`, TIP_VZS, priority) |>
  summarise(mean_wait = weighted.mean(`ČD realizirana`, Realizirani),
            n_realized = sum(Realizirani), .groups = "drop") |>
  pivot_wider(names_from = priority, values_from = c(mean_wait, n_realized))

quarterly_pairs <- make_priority_pairs(provider_quarterly, "quarter", 50)

quarterly_coverage <- quarterly_pairs |>
  filter(lubridate::year(quarter) %in% c(2020, 2026),
         lubridate::quarter(quarter) <= 3) |>
  mutate(year = lubridate::year(quarter)) |>
  count(comparison, across(all_of(provider_keys)), year, name = "n_quarters") |>
  filter(n_quarters >= 2) |>
  select(-n_quarters) |>
  pivot_wider(names_from = year, values_from = year, names_prefix = "year_") |>
  filter(!is.na(year_2020), !is.na(year_2026))

common_quarterly <- quarterly_coverage |>
  count(across(all_of(provider_keys)), name = "n_comparisons") |>
  filter(n_comparisons == 3) |>
  select(all_of(provider_keys))

quarterly_change <- quarterly_pairs |>
  semi_join(common_quarterly, by = provider_keys) |>
  filter(lubridate::year(quarter) %in% c(2020, 2026),
         lubridate::quarter(quarter) <= 3) |>
  mutate(year = lubridate::year(quarter)) |>
  group_by(comparison, across(all_of(provider_keys)), year) |>
  summarise(wait_high = weighted.mean(wait_high, n_high),
            wait_low = weighted.mean(wait_low, n_low),
            .groups = "drop") |>
  mutate(ratio = wait_high / wait_low, gap = wait_low - wait_high) |>
  select(comparison, all_of(provider_keys), year, ratio, gap) |>
  pivot_wider(names_from = year, values_from = c(ratio, gap),
              names_sep = "_") |>
  mutate(change_ratio = ratio_2026 - ratio_2020,
         change_gap = gap_2026 - gap_2020)

quarterly_summary <- summarise_pair_change(quarterly_change)
print(quarterly_summary)

# 5.3 Quarterly FE model: new registrations and realized waiting-time ratio --
# FE: provider–VZS and calendar quarter. Clustered SE: provider–VZS.
# Contemporaneous share is endogenous to system changes; results describe
# associations rather than causal capacity reallocation.
# Denominator checks are advisable: sum(..., na.rm=TRUE) may hide missingness.
priority_new_quarterly <- priority_data |>
  filter(var == "Vpisani") |>
  mutate(quarter = lubridate::floor_date(date, "quarter")) |>
  group_by(quarter, `RIZDDZ šifra izvajalca`, `Naziv izvajalca`,
           `VZS šifra`, priority) |>
  summarise(n_new = sum(value, na.rm = TRUE), .groups = "drop") |>
  group_by(quarter, across(all_of(provider_keys))) |>
  mutate(total_new = sum(n_new),
         share_new = if_else(total_new > 0, n_new / total_new, NA_real_)) |>
  ungroup() |>
  filter(priority == "Zelo hitro")

panel_quarterly <- quarterly_pairs |>
  filter(comparison == "Urgent / Routine") |>
  left_join(
    priority_new_quarterly |>
      select(quarter, all_of(provider_keys), share_new, total_new),
    by = c("quarter", provider_keys)
  ) |>
  mutate(
    provider_vzs = interaction(`RIZDDZ šifra izvajalca`,
                               `Naziv izvajalca`, `VZS šifra`, drop = TRUE),
    log_ratio = log(relative_wait),
    share_new_pp = share_new * 100
  ) |>
  filter(is.finite(log_ratio), is.finite(share_new_pp), total_new > 0)

# Actual previous CALENDAR quarter, not the previous observed quarter in
# the filtered panel.
lagged_registration <- priority_new_quarterly |>
  mutate(quarter_id = lubridate::year(quarter) * 4 +
                        lubridate::quarter(quarter) + 1) |>
  select(quarter_id, all_of(provider_keys),
         share_new_lag1 = share_new)

panel_lag_common <- panel_quarterly |>
  mutate(quarter_id = lubridate::year(quarter) * 4 +
                        lubridate::quarter(quarter)) |>
  left_join(lagged_registration,
            by = c("quarter_id", provider_keys)) |>
  mutate(share_new_lag1_pp = share_new_lag1 * 100) |>
  filter(is.finite(share_new_lag1_pp))

model_current <- feols(
  log_ratio ~ share_new_pp | provider_vzs + quarter,
  data = panel_lag_common, vcov = ~ provider_vzs
)
model_lagged <- feols(
  log_ratio ~ share_new_lag1_pp | provider_vzs + quarter,
  data = panel_lag_common, vcov = ~ provider_vzs
)
model_combined <- feols(
  log_ratio ~ share_new_pp + share_new_lag1_pp | provider_vzs + quarter,
  data = panel_lag_common, vcov = ~ provider_vzs
)
# Disaggregating log ratio: coefficient difference should equal ratio effect.
model_urgent <- feols(
  log(wait_high) ~ share_new_pp | provider_vzs + quarter,
  data = panel_lag_common, vcov = ~ provider_vzs
)
model_routine <- feols(
  log(wait_low) ~ share_new_pp | provider_vzs + quarter,
  data = panel_lag_common, vcov = ~ provider_vzs
)

# Sensitivity: volumes can themselves respond to prioritization decisions.
panel_extended <- panel_lag_common |>
  mutate(log_new = log1p(total_new),
         log_realized = log1p(n_high + n_low))
model_extended <- feols(
  log_ratio ~ share_new_pp + log_new + log_realized |
    provider_vzs + quarter,
  data = panel_extended, vcov = ~ provider_vzs
)
print(etable(model_current, model_lagged, model_combined,
             digits = 4, fitstat = ~ n + wr2))
print(etable(model_urgent, model_routine, model_current,
             digits = 4, fitstat = ~ n + wr2))
print(etable(model_current, model_extended,
             digits = 4, fitstat = ~ n + wr2))

# Slides report previously verified aggregates, NOT raw NIJZ observations.
# After running locally, compare these outputs to those in presentation.qmd.
