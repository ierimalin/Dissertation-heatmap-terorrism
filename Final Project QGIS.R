
#CLEANING FOR MAPPING TERRORIST ATTACKS AND TRANSITIONAL JUSTICE MECHANISMS AROUND THE WORLD


#1. Cleaning GTD & preparing for QGIS
library(readxl)
library(tidyverse)
gtd_2021 = read_excel("globalterrorismdb_2021Jan-June_1222dist.xlsx")
gtd_2020 = read_excel("globalterrorismdb_0522dist.xlsx")

merged_data = bind_rows(gtd_2020, gtd_2021)


gtd_clean = merged_data |>
  select(iyear, imonth, iday, latitude, longitude)

gtd_clean$date = as.Date(paste(gtd_clean$iyear, gtd_clean$imonth, gtd_clean$iday), format = "%Y %m %d")

gtd_clean = gtd_clean |> filter(!is.na(longitude) & !is.na(date)) #the cases of NA longitude are the same with the cases of NA latitude.


write_csv(gtd_clean, "cleaned_gtd_mapping.csv")


#2. Cleaning TJET & preparing for QGIS