library(RSpectra)
library(here)

tick <- proc.time()[3]

Wstar.eigen <- eigs_sym(Wstar, round(nrow(Wstar) * 99/100), opts = list(retvec = FALSE))

tock <- proc.time()[3]

(tock-tick)/60 # time in minutes


saveRDS(Wstar.eigen, file = here("intermediary_data/eig_vals.rds"))