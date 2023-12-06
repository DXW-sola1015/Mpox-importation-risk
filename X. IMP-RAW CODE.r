#------ load packages ------
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(sf)
library(grid)
library(gamlss)
Sys.setlocale("LC_TIME", "US")

args <- commandArgs(trailingOnly = TRUE)
input_seed <- as.integer(args[1])
x1 <- as.integer(args[2])
x2 <- as.integer(args[3])
input_sim <- x1:x2

# x1 = 1
# x2 = 1
# input_seed = 0
# input_sim = x1:x2

#------ onset to report ------
readRDS("../../DATA/O2R_ZINBI.rds") -> tmp2

#------ incubation period ------ 
par_incubation = c(2.090741, 0.4458967)

#------ link -------
readRDS("../../DATA/IMP_LINELIST.rds") -> tmp_left_fin

# readRDS("../../DATA/IMP_OAG2.rds") -> tmp_test
readRDS("../../DATA/IMP_OAG.rds") %>%
  rename(p = volume,
         vbs = Arr.City.Code) -> tmp_p

tmp_p %>%
  ungroup() %>%
  dplyr::select(Dep.Country.Code, REGION, CASE) %>%
  distinct() %>%
  mutate(CASE = as.integer(round(CASE))) -> tmp_cs

tmp_cs %>%
  group_by(Dep.Country.Code) %>%
  summarise(TOTAL = sum(CASE)) %>%
  right_join(tmp_cs) -> tmp_total

#------ sample ------
tmp_left_fin %>%
  filter(year == 2019) %>%
  group_by(Dep.Country.Code) %>%
  summarise(n = n()) -> tmp_n

tmp_n %>%
  right_join(tmp_total) %>%
  mutate(CASE = if_else(n >= TOTAL, CASE, as.integer(floor(n*CASE/TOTAL)))) %>%
  dplyr::select(!TOTAL) -> tmp_cs_fin

tmp_cs_fin %>%
  group_by(Dep.Country.Code) %>%
  summarise(TOTAL = sum(CASE)) %>%
  left_join(tmp_n) %>%
  mutate(CASE = n - TOTAL,
         REGION = "OTHER") %>%
  bind_rows(tmp_cs_fin) -> tmp_sp

tmp_sp %>%
  group_by(Dep.Country.Code) %>%
  summarise(n = list(data.frame(r = REGION, p = CASE))) -> tmp_sp

tmp_left_fin %>%
  filter(Dep.Country.Code %in% tmp_sp$Dep.Country.Code) %>%
  left_join(tmp_sp) -> tmp_data0

#------ SIM ------
set.seed(input_seed)

# t1 <- Sys.time()
for (sim in input_sim) {
  
  func_sp <- function(x) {
    sample(x$r, 1, prob = x$p)
  }
  
  sapply(tmp_data0$n, func_sp) -> tmp_data0$REGION
  
  tmp_data0 %>%
    dplyr::select(!n) %>%
    mutate(o2r = NA, incu = NA) %>%
    filter(REGION != "OTHER") %>%
    mutate(id = 1:n()) -> tmp_left_fin
  
  tmp_left_fin$o2r <- rZINBI(nrow(tmp_left_fin), mu = tmp2$estimate[1], sigma = tmp2$estimate[2], nu = tmp2$estimate[3])
  tmp_left_fin$incu <- as.integer(round(rlnorm(nrow(tmp_left_fin), par_incubation[1], par_incubation[2])))
  
  tmp_left_fin %>%
    mutate(d1 = date - o2r - incu,
           d2 = date - o2r,
           month = as.integer(format(d1, "%m"))) -> tmp_left_fin
  max(tmp_left_fin$incu) + 1 -> tmp_max
  
  tmp_left_fin -> tmp_input
  
  tmp_result <- c()
  for (i in 1:(tmp_max + 1)) {
    
    tmp_input %>%
      left_join(tmp_p) %>%
      filter(!is.na(p)) %>%
      group_by(year, id) %>%
      summarise(vbs = sample(vbs, 1, prob = p)) %>%
      right_join(tmp_input) %>%
      mutate(d1 = d1 + 1,
             month = as.integer(format(d1, "%m"))) -> tmp_test
    
    tmp_test %>%
      filter(d1 <= d2,
             vbs == "A0") -> tmp_remain

    tmp_test %>%
      filter(!(d1 <= d2 & vbs == "A0")) -> tmp0
    bind_rows(tmp_result, tmp0) -> tmp_result
    
    if (nrow(tmp_remain) == 0) break
    
    tmp_remain %>%
      dplyr::select(!vbs) -> tmp_input
  }
  
  tmp_result %>%
    filter(vbs != "A0") %>%
    mutate(date = d2) %>%
    group_by(year, Dep.Country.Code, vbs, date) %>%
    summarise(n = n()) -> tmp_result
  
  write.csv(tmp_result, paste("../RESULT/SIM/sim", sim, ".csv", sep = ""), row.names = F)
}
# t2 <- Sys.time()
# t2 - t1

# tmp_result %>%
#   filter(nchar(vbs) == 3) %>%
#   group_by(year, date) %>%
#   summarise(n = sum(n)) %>%
#   group_by(year) %>%
#   mutate(n_cum = cumsum(n)) -> tmp_test
