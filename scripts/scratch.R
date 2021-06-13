# diagnosis:

aberrant_rows <- c()

for (i in 1:nrow(countyadj)) {
  
  if (!identical(countyadj[i, ], countyadj[, i])) {
    
    aberrant_rows <- c(aberrant_rows, i)
    
  }
  
}

