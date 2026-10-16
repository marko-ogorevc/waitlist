library(tidyverse)
library(readxl)
library(gridExtra)
library(extrafont)
library(knitr)
library(kableExtra)

my_font = "Poppins"

root <- "F:/MZ backup/MARKO/"
# root <- "//ad.sigov.si/DAT/MZ/DDE/MARKO/"

sifrant_VZS <- read_excel(
  str_c(root, "/Šifrant VZS/Sifrant-VZS_verzija-18.1.xls"),
  col_types = c("text"),
  sheet = 2
) 

sifrant_VZS_edit <- sifrant_VZS |> 
  select(contains("VZS"), -"Nadrejeni VZS", contains("TIP", ignore.case = TRUE),  "Predmet spremljanja čakalnih seznamov") |> 
  mutate(
    tip_naročanje = case_when(
      `Tip VZS po naročanju` == 1 ~ "preventivni pregled",
      `Tip VZS po naročanju` == 2 ~ "kurativni pregled",
      `Tip VZS po naročanju` == 3 ~ "diagnostični postopek",
      `Tip VZS po naročanju` == 4 ~ "terapevtski postopek",
      `Tip VZS po naročanju` == 5 ~ "elektronski posvet" ,
      `Tip VZS po naročanju` == 6 ~ "nacionalni razpis",
      `Tip VZS po naročanju` == 7 ~ "patronaža",
      TRUE ~ "OSTALO"
    ),
    tip_kurativni = case_when(
      `Tip kurativnega pregleda` == 1 ~ "prvi",
      `Tip kurativnega pregleda` == 2 ~ "kontrolni",
      `Tip kurativnega pregleda` == 3 ~ "začetek zdravljenja",
      `Tip kurativnega pregleda` == 4 ~ "videokonferenca",
      TRUE ~ "OSTALO"
    )
  ) |> 
  mutate(
    across(
      .cols = c("Skupina VZS", "Tip VZS po naročanju", "Tip kurativnega pregleda"),
      .fns = as.numeric
    )
  )

NIJZ_2020 <- read_excel(
  str_c(root, "/NIJZ zahtevki cakajoci/Izpis stanja po VZS in izvajalcih v letih 2020-2021.xlsx"),
  sheet = 1
) |> 
  separate(Obdobje, into = c("month", "year"), remove = FALSE) |> 
  mutate(
    month  = case_when(
      month == "januar" ~ "01",
      month == "februar" ~ "02",
      month == "marec" ~ "03",
      month == "april" ~ "04",
      month == "maj" ~ "05",
      month == "junij" ~ "06",
      month == "julij" ~ "07",
      month == "avgust" ~ "08",
      month == "september" ~ "09",
      month == "oktober" ~ "10",
      month == "november" ~ "11",
      month == "december" ~ "12"
    ),
    date = as.Date(paste(year, month, "01", sep = "-"))+months(1)-days(1)
    
  ) |> 
  select(-c("Obdobje", "year", "month", "VZS Naziv"))



NIJZ_2021 <- read_excel(
  str_c(root, "/NIJZ zahtevki cakajoci/Izpis stanja po VZS in izvajalcih v letih 2020-2021.xlsx"),
  sheet = 2
) |> 
  separate(Obdobje, into = c("month", "year"), remove = FALSE) |> 
  mutate(
    month  = case_when(
      month == "januar" ~ "01",
      month == "februar" ~ "02",
      month == "marec" ~ "03",
      month == "april" ~ "04",
      month == "maj" ~ "05",
      month == "junij" ~ "06",
      month == "julij" ~ "07",
      month == "avgust" ~ "08",
      month == "september" ~ "09",
      month == "oktober" ~ "10",
      month == "november" ~ "11",
      month == "december" ~ "12"
    ),
    date = as.Date(paste(year, month, "01", sep = "-"))+months(1)-days(1)
    
  ) |> 
  select(-c("Obdobje", "year", "month", "VZS Naziv"))


NIJZ_data_plus <- read_excel(
  str_c(root, "/NIJZ zahtevki cakajoci/Prikaz+stanja+april+2024-maj+2024+po+VZS+in+izvajalcih_ver3.xlsx")
) |> 
  select(
    -"VZS naziv"
  )

NIJZ_data_jun <- read_excel(
  str_c(root, "/NIJZ zahtevki cakajoci/Prikaz stanja april 2024-julij 2024 po VZS in izvajalcih_ver4.xlsx")
)|> 
  select(
    -"VZS naziv"
  )


NIJZ_data_aug <- read_excel(
  str_c(root, "/NIJZ zahtevki cakajoci/Prikaz+stanja+april+2024-maj+2024+po+VZS+in+izvajalcih_ver5.xlsx")
) |> 
  mutate(
    Mesec = "August 2024"
  ) |> 
  select(
    -"VZS naziv"
  )


NIJZ_data_okt <- read_excel(
  str_c(root, "/NIJZ zahtevki cakajoci/Prikaz stanja september 2024-oktober 2024 po VZS in izvajalcih_ver6.xlsx")
) |> 
  mutate(
    Mesec = ifelse(Mesec == "september", "September", "October"),
    Mesec = paste(Mesec, Leto)
  ) |> 
  select(-Leto) |> 
  select(
    -"VZS naziv"
  )


NIJZ_data_01 <- read_excel(
  str_c(root, "/NIJZ zahtevki cakajoci/Prikaz+stanja+januar+2023-april+2024+po+VZS+in+izvajalcih_ver2.xlsx")
) |> 
  select(
    -"VZS naziv"
  )

NIJZ_data_02 <- read_excel(
  str_c(root, "/NIJZ zahtevki cakajoci/Prikaz stanja v letu 2022 po vseh VZS in izvajalcih.xlsx")
) |> 
  select(
    -"VZS naziv"
  )

NIJZ_data_okt <- read_excel(
  str_c(root, "/NIJZ zahtevki cakajoci/Prikaz stanja september 2024-oktober 2024 po VZS in izvajalcih_ver6.xlsx")
) |> 
  mutate(
    Mesec = ifelse(Mesec == "september", "September", "October"),
    Mesec = paste(Mesec, Leto)
  ) |> 
  select(-Leto) |> 
  select(
    -"VZS naziv"
  )

NIJZ_data_dec <- read_excel(
  str_c(root, "/NIJZ zahtevki cakajoci/Prikaz stanja november in december 2024 po VZS in izvajalcih_ver6.xlsx")
) |> 
  mutate(
    Mesec = ifelse(month(Mesec) == 11, "November 2024", "December 2024")
  ) |> 
  select(
    -"VZS naziv"
  )


NIJZ_data_feb <- read_excel(
  str_c(root, "/NIJZ zahtevki cakajoci/Prikaz stanja januar in februar 2025 po VZS in izvajalcih.xlsx")
) |> 
  select(
    -"VZS naziv"
  )

NIJZ_data_april <- read_excel(
  str_c(root, "/NIJZ zahtevki cakajoci/Prikaz stanja marec in april 2025 po VZS in izvajalcih.xlsx")
) |> 
  select(
    -"VZS naziv"
  )

NIJZ_maj_junij <- read_excel(
  str_c(root, "/NIJZ zahtevki cakajoci/Prikaz stanja maj in junij 2025 po VZS in izvajalcih.xlsx")
) |> 
  select(
    -"VZS naziv"
  )

NIJZ_julij_avgust <- read_excel(
  str_c(root, "/NIJZ zahtevki cakajoci/Prikaz stanja julij in avgust 2025 po VZS in izvajalcih.xlsx")
) |> 
  select(
    -"VZS naziv"
  ) |> 
  left_join(
    NIJZ_maj_junij |> distinct(`RIZDDZ šifra izvajalca`, `Naziv izvajalca`) |> distinct(`RIZDDZ šifra izvajalca`, .keep_all = TRUE)
  )

NIJZ_sept_oktober <- read_excel(
  str_c(root, "/NIJZ zahtevki cakajoci/Prikaz stanja september in oktober 2025 po VZS in izvajalcih.xlsx")
)|> 
  left_join(
    NIJZ_maj_junij |> distinct(`RIZDDZ šifra izvajalca`, `Naziv izvajalca`) |> distinct(`RIZDDZ šifra izvajalca`, .keep_all = TRUE)
  )

NIJZ_november_december <- read_excel(
  str_c(root, "/NIJZ zahtevki cakajoci/Prikaz stanja november in december 2025 po VZS in izvajalcih.xlsx")
)|> 
  left_join(
    NIJZ_maj_junij |> distinct(`RIZDDZ šifra izvajalca`, `Naziv izvajalca`) |> distinct(`RIZDDZ šifra izvajalca`, .keep_all = TRUE)
  )

NIJZ_januar_februar_2026 <- read_excel(
  str_c(root, "/NIJZ zahtevki cakajoci/Prikaz stanja januar in februar 2026 po VZS in izvajalcih.xlsx")
)|> 
  left_join(
    NIJZ_maj_junij |> distinct(`RIZDDZ šifra izvajalca`, `Naziv izvajalca`) |> distinct(`RIZDDZ šifra izvajalca`, .keep_all = TRUE)
  )

NIJZ_marec_april_2026 <- read_excel(
  str_c(root, "/NIJZ zahtevki cakajoci/Prikaz stanja marec in april 2026 po VZS in izvajalcih.xlsx")
)|> 
  left_join(
    NIJZ_maj_junij |> distinct(`RIZDDZ šifra izvajalca`, `Naziv izvajalca`) |> distinct(`RIZDDZ šifra izvajalca`, .keep_all = TRUE)
  )

NIJZ_maj_september_2026 <- read_excel(
  str_c(root, "/NIJZ zahtevki cakajoci/Prikaz stanja maj, junij, julij, avgust in september 2026 po VZS in izvajalcih.xlsx")
)|> 
  left_join(
    NIJZ_maj_junij |> distinct(`RIZDDZ šifra izvajalca`, `Naziv izvajalca`) |> distinct(`RIZDDZ šifra izvajalca`, .keep_all = TRUE)
  )


NIJZ_data <- NIJZ_data_01  |>  
  bind_rows(NIJZ_data_02) |> 
  bind_rows(NIJZ_data_aug) |>
  bind_rows(NIJZ_data_okt) |> 
  bind_rows(NIJZ_data_dec) |> 
  bind_rows(NIJZ_data_feb ) |> 
  bind_rows(NIJZ_maj_september_2026|> rename(Mesec = Obdobje)) |> 
  separate(Mesec, into = c("month", "year"), remove = FALSE) |> 
  mutate(
    month  = case_when(
      month == "January" ~ "01",
      month == "February" ~ "02",
      month == "March" ~ "03",
      month == "April" ~ "04",
      month == "May" ~ "05",
      month == "June" ~ "06",
      month == "July" ~ "07",
      month == "August" ~ "08",
      month == "September" ~ "09",
      month == "October" ~ "10",
      month == "November" ~ "11",
      month == "December" ~ "12"
    ),
    date = as.Date(paste(year, month, "01", sep = "-"))+months(1)-days(1)
    
  ) |> 
  select(-c("Mesec", "year", "month")) |> 
  
  bind_rows(
    NIJZ_2020
  ) |> 
  
  bind_rows(
    NIJZ_2021
  ) |> 
  
  
  bind_rows(NIJZ_data_jun |> 
              mutate(date = as.Date(Mesec)+months(1)-days(1)) |> 
              select(-"Mesec")) |> 
  bind_rows(NIJZ_data_april|> 
              mutate(
                date = as.Date(Mesec)+months(1)-days(1)
              ) |> 
              select(-Mesec)
  ) |> 
  bind_rows(NIJZ_maj_junij|> 
              mutate(
                date = as.Date(Mesec)+months(1)-days(1)
              ) |> 
              select(-Mesec)
  ) |> 
  
  bind_rows(NIJZ_julij_avgust|> 
              mutate(
                date = as.Date(Mesec)+months(1)-days(1)
              ) |> 
              select(-Mesec)
  ) |> 
  
  bind_rows(
    NIJZ_sept_oktober |> 
      mutate(
        date = as.Date(Obdobje)+months(1)-days(1)
      ) |> 
      select(-Obdobje)
  ) |> 
  
  
  bind_rows(
    NIJZ_november_december |> 
      mutate(
        date = as.Date(Obdobje)+months(1)-days(1)
      ) |> 
      select(-Obdobje)
  ) |> 
  
  
  bind_rows(
    NIJZ_januar_februar_2026 |> 
      mutate(
        date = as.Date(Obdobje)+months(1)-days(1)
      ) |> 
      select(-Obdobje)
  ) |> 
  
  bind_rows(
    NIJZ_marec_april_2026 |> 
      mutate(
        date = as.Date(Obdobje)+months(1)-days(1)
      ) |> 
      select(-Obdobje)
  ) |> 
  
  left_join(sifrant_VZS_edit, by = c("VZS šifra" = "Šifra VZS")) |> 
  filter(
    `Predmet spremljanja čakalnih seznamov` == "DA"
  ) |>
  select(
    - `Predmet spremljanja čakalnih seznamov`
  )



NIJZ_db <-
  NIJZ_data |> 
  pivot_longer(
    -c(contains("VZS"), contains("izvajalca"), "date", contains("tip", ignore.case = TRUE))
  ) 



nijz_var_prior<-
  NIJZ_db |> 
  distinct(name) |> 
  mutate(
    var = case_when(
      grepl("Povprečna ČD", name) ~ "ČD prvi termin",
      grepl("Povprečna realizirana ČD", name) ~ "ČD realizirana",
      grepl("Število vpisanih", name) ~ "Vpisani",
      grepl("Število realiziranih", name) ~ "Realizirani",
      grepl("Število čakajočih NAD dop", name) ~ "Čakajoči NAD",
      grepl("Število čakajočih", name) ~ "Čakajoči VSI"
    ),
    priority = case_when(
      grepl("Zelo hitro", name) ~ "Zelo hitro",
      grepl("Hitro", name) ~ "Hitro",
      grepl("Redno", name) ~ "Redno"
    ),
    
    priority = factor(priority, 
                      levels = c("Redno", "Hitro", "Zelo hitro"))
  )

NIJZ_db <-
  NIJZ_db |> 
  left_join(nijz_var_prior) 

NIJZ_db <-
  NIJZ_db |> 
  mutate(
  TIP_VZS = case_when(
    `Tip VZS po naročanju` == 1 ~ "Preventivni pregled",
    
    `Tip VZS po naročanju` == 2 &
      `Tip kurativnega pregleda` == 1 ~ "Prvi pregled",
    
    `Tip VZS po naročanju` == 2 &
      `Tip kurativnega pregleda` == 2 ~ "Kontrolni pregled",
    
    `Tip VZS po naročanju` == 2 &
      `Tip kurativnega pregleda` == 3 ~ "Začetek zdravljenja",
    
    `Tip VZS po naročanju` == 2 &
      `Tip kurativnega pregleda` == 4 ~ "Videopregled",
    
    `Tip VZS po naročanju` == 2 &
      is.na(`Tip kurativnega pregleda`) ~ "Kurativni pregled - neopredeljen",
    
    `Tip VZS po naročanju` == 3 ~ "Diagnostični postopek",
    `Tip VZS po naročanju` == 4 ~ "Terapevtski postopek",
    `Tip VZS po naročanju` == 5 ~ "ePosvet",
    `Tip VZS po naročanju` == 6 ~ "Nacionalni razpis",
    `Tip VZS po naročanju` == 7 ~ "Patronaža",
    
    TRUE ~ "Drugo"
  ) 
  )|> 
  
  select(
    "date",
    "VZS šifra", 
    "VZS naziv" = "Naziv VZS",
    "TIP_VZS",
    "RIZDDZ šifra izvajalca",
    "Naziv izvajalca",
    "var",
    "priority",
    "value"
  )



