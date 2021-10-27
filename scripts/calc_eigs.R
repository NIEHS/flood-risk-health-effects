library(RSpectra)
library(here)

tick <- proc.time()[3]

Wstar.eigen <- eigs_sym(Wstar, round(nrow(Wstar) * 1/25), opts = list(retvec = FALSE))

tock <- proc.time()[3]

(tock-tick)/60 # time in minutes


saveRDS(Wstar.eigen, file = here("intermediary_data/eig_vals.rds"))



# 18.45 mins to find 1/100 of the eigenvalues

# 32 minutes to find 1.5/100 of the eigenvalues

# 8 hours and 24 minutes to find 1/25 of the eigenvalues



# Wstar.eigen <- eigs_sym(Wstar, round(nrow(Wstar) * 1/100), which = "SM", opts = list(retvec = FALSE))
# saved in eig_vals_lower -- took 3 hours




plot(eig_vals2$values)



interp <- approxfun(x = 1:length(eig_vals2$values), y = eig_vals2$values)

lines(interp(1:length(eig_vals2$values)), col = "red")



x_idx <- c(1:length(eig_vals2$values), (71825 - 717):71825)
c_eig_vals <- c(eig_vals2$values, eig_vals_lower$values)

plot(x_idx, c_eig_vals)

interp2 <- approxfun(x = x_idx, y = c_eig_vals)

lines(interp2(x_idx), col = "red")

