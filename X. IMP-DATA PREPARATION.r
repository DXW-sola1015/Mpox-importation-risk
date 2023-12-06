#------ load packages ------
library(dplyr)
library(stringr)
library(tidyr)
Sys.setlocale("LC_TIME", "US")

#------ select destination ------
tmp_date = "2022-09-13"
read.csv(paste("../../DATA/global_health/", tmp_date, ".csv", sep = "")) -> tmp_monkeypox

tmp_monkeypox %>%
  mutate(Country = str_trim(Country),
         Date_confirmation = as.Date(Date_confirmation)) %>%
  filter(Country != "China", Date_confirmation >= "2022-05-01") %>%
  mutate(x1 = str_detect(Travel_history_country, Country),
         x2 = str_detect(Travel_history_location, Country),
         x3 = str_detect(Travel_history_location, "within") | 
           str_detect(Travel_history_country, "within"),
         domestic = x1 | x2 | x3) -> tmp_monkeypox

tmp_monkeypox %>%
  filter(!(Travel_history..Y.N.NA. == "Y" & domestic == F)) %>%
  dplyr::select(Country_ISO3, Date_confirmation) -> tmp_case

tmp_case -> tmp_confirm

tmp_monkeypox %>%
  filter(Travel_history..Y.N.NA. == "Y" & domestic == F) -> tmp_case

tmp_case %>%
  group_by(Country_ISO3) %>%
  summarise(n = n_distinct(Date_confirmation)) %>%
  filter(n >= 3) -> tmp_selected

readxl::read_excel("../../DATA/ALPHA2-3.xlsx") %>%
  dplyr::select(Alpha2, Alpha3) -> tmp_iso2_iso3

tmp_selected %>%
  left_join(tmp_iso2_iso3, by = c("Country_ISO3" = "Alpha3")) -> tmp_selected
c("CN", tmp_selected$Alpha2) -> tmp_selected

#------ select origin ------
readxl::read_excel("../../DATA/MONKEYPOX-COUNTRY.xlsx", sheet = "AP") -> tmp_airport1
readxl::read_excel("../../DATA/MONKEYPOX-THREE.xlsx", sheet = "AP") -> tmp_airport2

tmp_airport1 %>%
  rename(AP = `AP-CODE`) %>%
  dplyr::select(ISO2, REGION, AP) -> tmp_airport1

tmp_airport2 %>%
  rename(AP = `AP-CODE`) %>%
  mutate(REGION = str_remove(REGION, ", .*")) %>%
  dplyr::select(ISO2, REGION, AP) -> tmp_airport2

bind_rows(tmp_airport1, tmp_airport2) -> tmp_airport

readxl::read_excel("../../DATA/MONKEYPOX-COUNTRY.xlsx", sheet = "LINK") -> tmp_airport1
readxl::read_excel("../../DATA/MONKEYPOX-THREE.xlsx", sheet = "LINK") -> tmp_airport2

tmp_airport1 %>%
  filter(!is.na(POP)) %>%
  dplyr::select(ISO2, REGION, CASE, POP) -> tmp_airport1

tmp_airport2 %>%
  mutate(REGION = str_remove(REGION, ", .*")) %>%
  dplyr::select(ISO2, REGION, CASE, POP) -> tmp_airport2

bind_rows(tmp_airport1, tmp_airport2) -> tmp_link

tmp_airport %>%
  left_join(tmp_link) %>%
  filter(!is.na(POP)) -> tmp

tmp %>%
  group_by(ISO2) %>%
  summarise(TOTAL = sum(CASE)) %>%
  right_join(tmp) %>%
  rename(Dep.Airport.Code = AP,
         Dep.Country.Code = ISO2) -> tmp

length(unique(tmp$Dep.Airport.Code))
nrow(tmp)

#------ oag ------
# readRDS("../../DATA/OAG.rds") -> tmp_OAG
readRDS("../../DATA/OAG_DATA4MPOX.rds") -> tmp_OAG

tmp_OAG %>%
  mutate(year = as.integer(str_sub(time, 1, 4)),
         month = as.integer(str_sub(time, 6, 7))) %>%
  filter(year %in% c(2019, 2022),
         Arr.Country.Code != Dep.Country.Code,
         Dep.Airport.Code %in% tmp$Dep.Airport.Code) %>%
  dplyr::select(!time) %>%
  dplyr::select(Dep.Airport.Code, Dep.Country.Code, Arr.Airport.Code, Arr.Country.Code,
                volume, year, month) %>%
  mutate(Arr.Country.Code = if_else(Arr.Country.Code %in% tmp_selected, Arr.Country.Code, "OTHER")) -> tmp_OAG

read.csv("../../DATA/AIRPORT_CITY_CODE_CHINA.csv") -> tmp_des_city
tmp_des_city %>%
  dplyr::select(Arr.Airport.Code, Arr.City.Code) %>%
  distinct() -> tmp_des_city

tmp_OAG %>%
  left_join(tmp) %>%
  left_join(tmp_des_city) %>%
  mutate(Arr.City.Code = if_else(is.na(Arr.City.Code), Arr.Country.Code, Arr.City.Code)) %>%
  group_by(Dep.Country.Code, REGION, Arr.Country.Code, Arr.City.Code, year, month) %>%
  summarise(volume = sum(volume),
            pop = mean(POP),
            CASE = mean(CASE)) -> tmp_fin

data.frame(date = seq(as.Date("2018-01-01"), as.Date("2023-01-01"), 1)) %>%
  mutate(year = as.integer(format(date, "%Y")),
         month = as.integer(format(date, "%m"))) %>%
  group_by(year, month) %>%
  summarise(n = n()) -> tmp_month

tmp_fin %>%
  left_join(tmp_month) %>%
  mutate(volume = volume/n) %>%
  dplyr::select(!n) -> tmp_fin

tmp_fin %>%
  group_by(Dep.Country.Code, REGION, year, month) %>%
  summarise(n = sum(volume),
            pop = mean(pop),
            CASE = mean(CASE)) %>%
  mutate(volume = pop - n,
         Arr.Country.Code = "A0",
         Arr.City.Code = "A0") %>%
  dplyr::select(!n) %>%
  bind_rows(tmp_fin) -> tmp_OAG_fin

saveRDS(tmp_OAG_fin, "../../DATA/IMP_OAG.rds")

#------ case pop ------
tmp_confirm %>%
  left_join(tmp_iso2_iso3, by = c("Country_ISO3" = "Alpha3")) %>%
  rename(Dep.Country.Code = Alpha2) %>%
  dplyr::select(!Country_ISO3) -> tmp_fin

expand_grid(tmp_fin,
            year = c(2019, 2022)) %>%
  rename(date = Date_confirmation) -> tmp_fin

saveRDS(tmp_fin, "../../DATA/IMP_LINELIST.rds")
