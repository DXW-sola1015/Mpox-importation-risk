#------ load packages ------
library(rstudioapi)
rm(list = ls())

#------ remove files ------
list.files("../RESULT/SIM", full.names = T) -> tmp_files
if (length(tmp_files) != 0) {
  file.remove(tmp_files)  
}

#------ scripts ------
scripts <- c()
for (i in 1:10) {
  c(scripts, paste('Rscript "X. IMP-RAW CODE.r"', i, 1 + (i - 1)*20, i*20)) -> scripts
}
# shell(scripts[1])

#------ batch ------
t1 <- Sys.time()
n_max = 5
sleep_time = 1

tmp_terminal <- c()
n_s <- length(scripts)
i = 1
while (i <= n_s) {
  while (length(tmp_terminal) < n_max) {
    if (i > n_s) break
    terminalExecute(scripts[i], show = F) -> tmp_id
    c(tmp_terminal, tmp_id) -> tmp_terminal
    i <- i + 1
  }
  
  if (i <= n_s) {
    num_exit <- 0
    while (num_exit == 0) {
      terminal_exited <- c()
      for (j in 1:length(tmp_terminal)) {
        c(terminal_exited, !is.null(terminalExitCode(tmp_terminal[j]))) -> terminal_exited
      }
      sum(terminal_exited) -> num_exit
      Sys.sleep(sleep_time)
    }
    
    tmp_terminal[terminal_exited] -> terminal_need_2_kill
    tmp_terminal[!terminal_exited] -> tmp_terminal
    for (j in terminal_need_2_kill) {
      terminalKill(j)
    }
  } else {
    num_running <- length(tmp_terminal)
    while (num_running != 0) {
      for (j in 1:num_running) {
        if (!is.null(terminalExitCode(tmp_terminal[j]))) {
          terminalKill(tmp_terminal[j])
          tmp_terminal[-j] -> tmp_terminal
          num_running <- num_running - 1
        }
      }
      Sys.sleep(sleep_time)
    }
  }
}
t2 <- Sys.time()
t2 - t1
