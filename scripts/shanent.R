shanent=function(x){
s=x
s=unlist(strsplit(s, ""))
l=length(s)
p=table(s)/l
H=-sum(sapply(p, function(x) x*log2(x)))
return(H)
}

shanent.b=function(x){
  s=x
  s=unlist(strsplit(s, ""))
  l1=length(unique(s))
  p=1/l1
  l2=length(s)
  p=dbinom(x=table(s), size =l2, prob = p)
  H=-sum(sapply(p, function(x) x*log2(x)))
  return(H)
}

AA=c("L","I","M","V", "F", "A", "Y","W", "K","R", "H", "E", "D", "Q", "N", "T", "C",  "S", "P", "G", "-")

redalph=list(
  h=c("L","I","M","V","A"),
  a=c("F", "Y","W"),
  b=c("K","R", "H"),
  n=c("E", "D"),
  p=c("Q", "N"),
  o=c("T", "C",  "S"),
  t=c("P", "G"),
  d=c("-")
)

ral=names(redalph)

randstr=function(len, alph){
  paste(alph[round(runif(len, min = 1, max = length(alph)))], collapse = "")
}


inredalph=function(s, redalph){
  s=unlist(strsplit(s, ""))
  paste(sapply(s, function(y) names(which(sapply(redalph, function(x) y %in% x)))), collapse = "")
}

#---
#readfile
library(readr)
test <- read_table2("~/papers/R/seqdata/test.aln",
col_names = FALSE)

#make aln to matrix
aln_nam=test$X1
seq_dat=test$X2

seq_dat=strsplit(seq_dat, "")
m=length(seq_dat)
n=length(seq_dat[[1]])
seq_dat=matrix(data=unlist(seq_dat), ncol=n, nrow =m, byrow = T)

#random background
n=nrow(seq_dat)
mrredent= mean(replicate(100, shanent(randstr(n,ral))))
mrent= mean(replicate(100, shanent(randstr(n,AA))))
rtot=mrredent+mrent

#entropies in 2 alphabets
H2=sapply(1:ncol(seq_dat), function(x) shanent(paste(seq_dat[,x],"")))
H1=sapply(1:ncol(seq_dat), function(x) shanent(inredalph(seq_dat[,x],redalph)))
indx=(H1+H2)/rtot #normalized index.

#bounds of conservation: empirical
cbounds=rep(NA, 13)
te=paste(sapply(redalph[c("a","h")], function(x) randstr(n/2,x)), collapse = "")
cbounds[1]=(shanent(inredalph(te,redalph))+shanent(te))/rtot
te=paste(sapply(redalph[c("p","o")], function(x) randstr(n/2,x)), collapse = "")
cbounds[2]=(shanent(inredalph(te,redalph))+shanent(te))/rtot
te=paste(sapply(redalph[c("p","n")], function(x) randstr(n/2,x)), collapse = "")
cbounds[3]=(shanent(inredalph(te,redalph))+shanent(te))/rtot
te=paste(sapply(redalph[c("n", "a","h")], function(x) randstr(n/3,x)), collapse = "")
cbounds[4]=(shanent(inredalph(te,redalph))+shanent(te))/rtot
te=paste(sapply(redalph[c("b", "a","h")], function(x) randstr(n/3,x)), collapse = "")
cbounds[5]=(shanent(inredalph(te,redalph))+shanent(te))/rtot
te=paste(sapply(redalph[c("n", "b")], function(x) randstr(n/3,x)), collapse = "")
cbounds[6]=(shanent(inredalph(te,redalph))+shanent(te))/rtot
te=paste(sapply(redalph[c("b")], function(x) randstr(n,x)), collapse = "")
cbounds[7]=(shanent(inredalph(te,redalph))+shanent(te))/rtot
te=paste(sapply(redalph[c("a")], function(x) randstr(n/2,x)), collapse = "")
cbounds[8]=(shanent(inredalph(te,redalph))+shanent(te))/rtot
te=paste(sapply(redalph[c("h")], function(x) randstr(n/2,x)), collapse = "")
cbounds[9]=(shanent(inredalph(te,redalph))+shanent(te))/rtot
te=paste(sapply(redalph[c("n")], function(x) randstr(n/2,x)), collapse = "")
cbounds[10]=(shanent(inredalph(te,redalph))+shanent(te))/rtot
te=paste(sapply(redalph[c("b")], function(x) randstr(n/3,x)), collapse = "")
cbounds[11]=(shanent(inredalph(te,redalph))+shanent(te))/rtot
te=paste(sapply(redalph[c("t")], function(x) randstr(n/3,x)), collapse = "")
cbounds[12]=(shanent(inredalph(te,redalph))+shanent(te))/rtot
te=paste(sapply(redalph[c("o")], function(x) randstr(n,x)), collapse = "")
cbounds[13]=(shanent(inredalph(te,redalph))+shanent(te))/rtot

#plot
barplot(indx, names.arg = seq_dat[4,])
abline(h=c(mean(cbounds[1:3]), mean(cbounds[4:6]), mean(cbounds[7:13])), col="blue", lwd=2, lty=2)