library(RSpectra)
library(here)

tick <- proc.time()[3]

Wstar.eigen <- eigs_sym(Wstar, round(nrow(Wstar) * 1/25), opts = list(retvec = FALSE))

tock <- proc.time()[3]

(tock-tick)/60 # time in minutes


saveRDS(Wstar.eigen, file = here("intermediary_data/eig_vals.rds"))



# 18.45 mins to find 1/100 of the eigenvalues

# 32 minutes to find 1.5/100 of the eigenvalues
