rm(list = ls())

#------ load packages ------
library(gamlss)
library(dplyr)

#------ read data and set paramter ------
input_seed <- 0
input_sim <- 1:10
par_incubation = c(2.090741, 0.4458967)

readRDS("DATA/O2R_ZINBI.rds") -> tmp2
readRDS("DATA/Mpox.RDS") -> tmp_data0
readRDS("DATA/RISK.RDS") -> tmp_p

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
    # i = 1
    names(tmp_input)
    names(tmp_p)
    
    tmp_input %>%
      left_join(tmp_p, relationship = "many-to-many") %>%
      filter(!is.na(p)) %>%
      group_by(id) %>%
      summarise(vbs = sample(Arr.Country.Code, 1, prob = p)) %>%
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
    group_by(Dep.Country.Code, vbs, date) %>%
    summarise(n = n()) -> tmp_result
  
  write.csv(tmp_result, paste("RESULT/SIM1/sim", sim, ".csv", sep = ""), row.names = F)
}
