
#CLEANING FOR MAPPING TERRORIST ATTACKS AND TRANSITIONAL JUSTICE MECHANISMS AROUND THE WORLD


#1. Cleaning GTD & preparing for QGIS
library(readxl)
library(readr)
library(tidyverse)
gtd_2021 = read_excel("globalterrorismdb_2021Jan-June_1222dist.xlsx")
gtd_2020 = read_excel("globalterrorismdb_0522dist.xlsx")

merged_data = bind_rows(gtd_2020, gtd_2021)


gtd_clean = merged_data |>
  select(iyear, imonth, iday, latitude, longitude, crit1, crit2, nkill, nwound)

gtd_clean$date = as.Date(paste(gtd_clean$iyear, gtd_clean$imonth, gtd_clean$iday), format = "%Y %m %d")

gtd_clean = gtd_clean |> filter(!is.na(longitude) & !is.na(date) & crit1 == 1 & crit2 == 1) |> mutate(weightcasualties = nkill + (nwound / 2)) #the cases of NA longitude are the same with the cases of NA latitude.


write_csv(gtd_clean, "cleaned_gtd_mapping.csv")


#2. Cleaning TJET & preparing for QGIS
#a. preparing amnesties data

tjet_amnesty <- read_csv("tjet_amnesties.csv")

tjet_amnesty = tjet_amnesty |> select(country, amnestyID, amnestyYear, fitsPostAutocraticTJ, fitsConflictTJ) |> filter(fitsPostAutocraticTJ == 1 | fitsConflictTJ == 1) # this method is used instead of just taking the tjet database because the original does not discriminate between transitional and non-stransitional mechanisms, and we only want those either or. 
tjet_amnesty = tjet_amnesty |> select(country, amnestyYear) |> rename(year = amnestyYear)
tjet_amnesty$amnesty = 1



#b. preparing prosecution data

tjet_trials = read_csv("tjet_trials.csv")

tjet_trials = tjet_trials |> select(country_Trial, trialID, yearStart, fitsPostAutocraticTJ, fitsConflictTJ, conviction) |> filter(fitsPostAutocraticTJ == 1 | fitsConflictTJ == 1) |> filter(conviction == 1) |>  select(country_Trial, yearStart) |> rename(year = yearStart)

tjet_trials$trial = 1


#c. preparing truth commissions data

tjet_tcs = read_csv("tjet_tcs.csv")

tjet_tcs = tjet_tcs |> select(country, truthcommissionID, yearBeginOperation, fitsPostAutocraticTJ, fitsConflictTJ) |> filter(fitsPostAutocraticTJ == 1 | fitsConflictTJ == 1) |> select(country, yearBeginOperation) |> rename(year = yearBeginOperation)

tjet_tcs$tcs = 1


#d. putting it all together

tjet = tjet_amnesty |> full_join(tjet_trials, by = c("country" = "country_Trial", "year")) |> full_join(tjet_tcs, by = c("country", "year"))
            

tjet[is.na(tjet$amnesty), "amnesty"] = 0
tjet[is.na(tjet$trial), "trial"] = 0
tjet[is.na(tjet$tcs), "tcs"] = 0

tjet = tjet |> distinct(country, year, .keep_all = TRUE)

tjet$decade = ifelse(tjet$year >= 1970 & tjet$year <= 1980, "1970-1980",
                      ifelse(tjet$year >= 1981 & tjet$year <= 1990, "1981-1990", 
                             ifelse(tjet$year >= 1991 & tjet$year <= 2000, "1991-2000", 
                                    ifelse(tjet$year >= 2001 & tjet$year <= 2010, "2001-2010", 
                                           ifelse(tjet$year >= 2011 & tjet$year <= 2021, "2011-2021", NA)))))

tjet$tjm = ifelse(tjet$amnesty == 1, 1, ifelse(tjet$trial == 1, 1, ifelse(tjet$tcs == 1, 1, NA)))

tjet = tjet |>
  mutate(
    tjm_1970_1980 = ifelse(decade == "1970-1980" & tjm == 1, 1, 0),
    tjm_1981_1990 = ifelse(decade == "1981-1990" & tjm == 1, 1, 0),
    tjm_1991_2000 = ifelse(decade == "1991-2000" & tjm == 1, 1, 0),
    tjm_2001_2010 = ifelse(decade == "2001-2010" & tjm == 1, 1, 0),
    tjm_2011_2021 = ifelse(decade == "2011-2021" & tjm == 1, 1, 0)
  )


#We only need this for QGIS 
tjet_country_decades = tjet |>
  group_by(country) |>
  summarise(
    tjm_1970_1980 = max(tjm_1970_1980),
    tjm_1981_1990 = max(tjm_1981_1990),
    tjm_1991_2000 = max(tjm_1991_2000),
    tjm_2001_2010 = max(tjm_2001_2010),
    tjm_2011_2021 = max(tjm_2011_2021),
    .groups = "drop"  
  )


write_csv(tjet_country_decades, "tjet_decade_tjm.csv")


#3. Combining TJET and the ne_50_admin for countries with TJMs for every decade


