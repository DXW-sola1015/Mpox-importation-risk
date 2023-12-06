#------ load packages ------
library(dplyr)
library(stringr)

#------ read ------
list.files("../RESULT/SIM/", full.names = T) -> tmp_files

tmp_data <- c()
for (i in tmp_files) {
  read.csv(i) -> tmp
  tmp$sim <- as.integer(str_extract(i, "[0-9]+"))
  bind_rows(tmp_data, tmp) -> tmp_data
}

readxl::read_excel("../../DATA/ALPHA2-3.xlsx") %>%
  dplyr::select(Alpha2, Alpha3) %>%
  rename(vbs = Alpha2) -> tmp_iso2_iso3

tmp_data %>%
  left_join(tmp_iso2_iso3) %>%
  mutate(date = as.Date(date)) %>%
  rename(ARR = Alpha3) %>%
  left_join(tmp_iso2_iso3, by = c("Dep.Country.Code" = "vbs")) %>%
  rename(DEP = Alpha3) -> tmp_data_fin

#------ CN ts ------
tmp_data_fin %>%
  filter(nchar(vbs) == 3) %>%
  group_by(year, date, sim) %>%
  summarise(n = n()) -> tmp_ts

expand.grid(date = seq(min(tmp_data_fin$date), max(tmp_data_fin$date), 1),
            year = c(2019, 2022),
            sim = 1:200) %>%
  left_join(tmp_ts) -> tmp_ts
tmp_ts$n[is.na(tmp_ts$n)] <- 0

tmp_ts %>% 
  group_by(sim, year) %>%
  mutate(n_cum = cumsum(n)) %>%
  group_by(year, date) %>%
  summarise(meidan = quantile(n_cum, .5),
            mean = mean(n_cum),
            lci = quantile(n_cum, .025),
            uci = quantile(n_cum, .975)) -> tmp_ts

#------ CN ORI ------
tmp_data_fin %>%
  filter(nchar(vbs) == 3) %>%
  group_by(year, DEP, sim) %>%
  summarise(n = sum(n)) -> tmp_FROM

expand.grid(year = c(2019, 2022),
            DEP = unique(tmp_data_fin$DEP),
            sim = 1:200) %>%
  left_join(tmp_FROM) -> tmp_FROM

tmp_FROM$n[is.na(tmp_FROM$n)] <- 0

tmp_FROM %>% 
  group_by(year, DEP) %>%
  summarise(meidan = quantile(n, .5),
            mean = mean(n),
            lci = quantile(n, .025),
            uci = quantile(n, .975)) -> tmp_FROM

#------ CN DES ----
tmp_data_fin %>%
  filter(nchar(vbs) == 3) %>%
  group_by(year, vbs, sim) %>%
  summarise(n = sum(n)) -> tmp_DES

expand.grid(year = c(2019, 2022),
            vbs = unique(tmp_data_fin$vbs),
            sim = 1:200) %>%
  left_join(tmp_DES) -> tmp_DES
tmp_DES$n[is.na(tmp_DES$n)] <- 0

tmp_DES %>% 
  filter(nchar(vbs) == 3) %>%
  group_by(year, vbs) %>%
  summarise(meidan = quantile(n, .5),
            mean = mean(n),
            lci = quantile(n, .025),
            uci = quantile(n, .975)) -> tmp_DES

#------ validation ------
tmp_data_fin %>%
  filter(!is.na(ARR)) %>%
  group_by(year, sim, ARR, date) %>%
  summarise(n = sum(n)) -> tmp_validation

expand.grid(date = seq(min(tmp_data_fin$date), max(tmp_data_fin$date), 1),
            ARR = unique(tmp_data_fin$ARR),
            year = c(2019, 2022),
            sim = 1:200) %>%
  left_join(tmp_validation) -> tmp_validation
tmp_validation$n[is.na(tmp_validation$n)] <- 0

tmp_validation %>%
  filter(!is.na(ARR)) %>%
  group_by(sim, year, ARR) %>%
  mutate(n_cum = cumsum(n)) %>%
  group_by(ARR, year, date) %>%
  summarise(meidan = quantile(n_cum, .5),
            mean = mean(n_cum),
            lci = quantile(n_cum, .025),
            uci = quantile(n_cum, .975)) -> tmp_validation

tmp_FROM %>%
  mutate(DEP = if_else(is.na(DEP), "USA", DEP)) -> tmp_FROM
list(a = tmp_ts, b = tmp_FROM, c = tmp_DES) -> tmp_out

saveRDS(tmp_out, "../RESULT/DATA4FIG2.rds")

saveRDS(tmp_validation, "../RESULT/DATA4VALIDATION.rds")
