te=sapply(1:200, function(x) 3*x^2+3*x+1)
ye=list()
for (j in 1:length(te)) {
  ye[[j]]=te[j]-(1:floor(te[j]^(1/3)))^3
}

ye[which(unlist(lapply(ye, function(x) any(x^(1/3)==floor(x^(1/3))))))]
