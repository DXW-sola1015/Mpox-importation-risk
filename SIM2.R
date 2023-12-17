rm(list = ls())

#------ load packages ------
library(dplyr)

#------ set pars ------
n_sim = 1000
N = 100
p_rash = .9
par_incubation = c(2.090741, 0.4458967)
sen_pcr = .95

#----- estimate distribution ------
c(0.025, .5, .975) -> x_q
c(7, 12, 24) -> y_est

nls(x_q ~ pgamma(y_est, a, b), start = list(a = 15, b = 1)) -> fit_gamma
nls(x_q ~ plnorm(y_est, a, b), start = list(a = 1, b = 1)) -> fit_lnorm
nls(x_q ~ pweibull(y_est, a, b), start = list(a = 1, b = 10)) -> fit_weibull
c("fit_gamma", "fit_lnorm", "fit_weibull") -> tmp_selected
c("rgamma", "rlnorm", "rweibull") -> tmp_func_selected

tmp_residual <- c()
tmp_coef <- c()
for (i in tmp_selected) {
  get(i) -> tmp_fit
  summary(tmp_fit) -> tmp_fit
  c(tmp_residual, sum(tmp_fit$residuals^2)) -> tmp_residual
  data.frame(a = tmp_fit$coefficients[1, 1],
             b = tmp_fit$coefficients[2, 1]) -> tmp_fit_coef
  rbind(tmp_coef, tmp_fit_coef) -> tmp_coef
}

tmp_coef[which.min(tmp_residual), ] -> tmp_coef

tmp_rash_duration <- function(x, dist_s, par_s) {
  # x = 10
  # dist_s = tmp_func_selected[which.min(tmp_residual)]
  # par_s = tmp_coef
  
  eval(call(dist_s, x, par_s$a, par_s$b))
}

#------ func ------
tmp_generate_infection <- function(input_N = N, input_sim = n_sim) {
  # input_N = 100
  # input_sim = n_sim
  
  tmp_return <- c()
  for (i in 1:input_sim) {
    # assign state for 100 cases
    data.frame(id = 1:input_N,
               state = rbinom(N, 1, p_rash)) -> tmp_data
    
    # generate date for fever and duration
    round(rlnorm(input_N, par_incubation[1], par_incubation[2])) -> tmp_data$start_rash
    tmp_rash_duration(input_N, tmp_func_selected[which.min(tmp_residual)],
                      tmp_coef) -> tmp_duration
    round(tmp_duration) + tmp_data$start_rash - 1 -> tmp_data$end_rash
    
    # generate date for boarding
    apply(tmp_data, 1, function(x) sample(1:x[3], 1)) -> tmp_data$t
    
    # remove unnecessary symptom
    tmp_data$start_rash[tmp_data$state == 0] <- NA
    tmp_data$end_rash[tmp_data$state == 0] <- NA
    
    # sample(0:9, input_N, replace = T) -> tmp_data$t
    
    tmp_data$sim <- i
    
    bind_rows(tmp_return, tmp_data) -> tmp_return
  }
  
  return(tmp_return)
}

tmp_func_sr <- function(p_sr, input_N = N, input_sim = n_sim) {
  # p_sr = .5
  # input_N = N
  # input_sim = n_sim
  
  tmp_return <- c()
  for (i in 1:input_sim) {
    c(tmp_return, rbinom(input_N, 1, p_sr)) -> tmp_return
  }
  
  return(tmp_return)
}

tmp_test_rash <- function(input_data = tmp_data, sen_pcr) {
  # input_data = tmp_data
  # sen_pcr = sen_pcr
  tmp_rash <- rep(0, nrow(input_data))
  input_data %>%
    mutate(rnum = 1:n()) %>%
    filter(t >= start_rash, t <= end_rash) -> tmp_selected
  tmp_rash[tmp_selected$rnum] <- 1
  
  tmp_rash & input_data$state -> tmp_rash_new
  ifelse(tmp_rash_new == T, 1, 0) -> tmp_rash_new
  
  rbinom(1:nrow(input_data), tmp_rash_new, sen_pcr) -> tmp_pos
  tmp_pos == 0 & tmp_rash_new == 1 -> tmp_neg
  ifelse(tmp_pos == 1, T, F) -> tmp_pos
  !tmp_neg & !tmp_pos -> tmp_NT
  data.frame(pos = tmp_pos, neg = tmp_neg, NT = tmp_NT) -> tmp_return
  
  return(tmp_return)
}

#------ quarantine for t days ------
set.seed(1015)
tmp_save <- c()
for (p_sr in seq(.1, 1, .1)) {
  # p_sr = .1
  tmp_generate_infection() -> tmp_data
  tmp_func_sr(p_sr = p_sr) -> tmp_data$sr
  
  tmp_test_rash(input_data = tmp_data, sen_pcr = sen_pcr) -> tmp_rash_detection
  ifelse(tmp_data$sr == 1, T, F) -> tmp_sr
  
  tmp_data[tmp_rash_detection$pos, ] -> tmp_caught
  tmp_caught$sce = 2
  tmp_data[tmp_rash_detection$NT & tmp_sr, ] -> tmp_NT
  tmp_NT -> tmp_data_raw
  
  tmp_caught1 <- c()
  tmp_caught2 <- tmp_caught
  for (day_t in 0:21) {
    # day_t = 0
    if (day_t == 0) {
    } else {
      tmp_data$t <- tmp_data$t + 1
      tmp_data_raw$t <- tmp_data_raw$t + 1
    }
    
    tmp_test_rash(input_data = tmp_data, sen_pcr = sen_pcr) -> tmp_rash_detection
    ifelse(tmp_data$sr == 1, T, F) -> tmp_sr
    tmp_data[tmp_rash_detection$pos, ] -> tmp_caught
    if (nrow(tmp_caught) != 0) {
      tmp_caught$sce = 1
      bind_rows(tmp_caught1, tmp_caught) -> tmp_caught1
    }
    tmp_data[tmp_rash_detection$NT | (tmp_sr & tmp_rash_detection$neg), ] -> tmp_data
    
    tmp_test_rash(input_data = tmp_data_raw, sen_pcr = sen_pcr) -> tmp_rash_detection
    tmp_data_raw[tmp_rash_detection$pos, ] -> tmp_caught
    if (nrow(tmp_caught) != 0) {
      tmp_caught$sce = 2
      bind_rows(tmp_caught2, tmp_caught) -> tmp_caught2
    }
    tmp_data_raw[tmp_rash_detection$NT, ] -> tmp_data_raw
    
    bind_rows(tmp_caught1, tmp_caught2) %>%
      group_by(sce, sim) %>%
      summarise(n = 100 - n()) %>%
      group_by(sce) %>%
      summarise(m = mean(n),
                # m = quantile(n, .5),
                lci_95 = quantile(n, .025),
                lci_50 = quantile(n, .25),
                uci_95 = quantile(n, .975),
                uci_50 = quantile(n, .75)) %>%
      mutate(day_t = day_t, p_sr = p_sr) -> tmp_out
    bind_rows(tmp_save, tmp_out) -> tmp_save
  }
}

saveRDS(tmp_save, "RESULT/SIM2.RDS")