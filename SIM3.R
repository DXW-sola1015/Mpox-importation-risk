rm(list = ls())

#------ out break prob ------
expand.grid(R0 = seq(1.1, 2, 0.1),
            k = seq(0.1, 1.2, 0.1),
            T0 = 1:10) -> tmp_prob

tmp_prob$prob <- -tmp_prob$T0*log(tmp_prob$R0)/(0.334 + .689/tmp_prob$k + 
                                                  .408/tmp_prob$R0 - 
                                                  .507/(tmp_prob$k*tmp_prob$R0) - 
                                                  .356/tmp_prob$R0^2 + 
                                                  .467/(tmp_prob$k*tmp_prob$R0^2))
1 - exp(tmp_prob$prob) -> tmp_prob$prob

saveRDS(tmp_prob, "RESULT/SIM3.RDS")
