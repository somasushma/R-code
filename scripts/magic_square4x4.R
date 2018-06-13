# how to make 4 x 4 magic square
# first make below basic square
m=matrix(data=c(8,11,14,1,13,2,7,12,3,16,9,6,10,5,4,15), nrow = 4, byrow = T)
s1=sum(m[,1])
#choose sum (e.g. 64 for kaumara square)
s2=64
d=s2-s1
a1=floor(d/4)
a2=floor(d/4)+ d %% 4

#make new matrix with sum=s2
m1=matrix(sapply(1:16, function(x) ifelse(m[x]<13,m[x]+a1, m[x]+a2)), nrow = 4, byrow = T)

#reversed version
m2=matrix(rev(m1), nrow=4, byrow = T)

#another version (for kaumAra)
m3=apply(m2, 1, rev)

#print
prmatrix(m3, collab = rep("",4), rowlab = rep("",4))

#complementary matrix
mc=17-m

#new matrix with sum=s1 from complementary matrix
mc1=matrix(sapply(1:16, function(x) ifelse(mc[x]<13,mc[x]+a1, mc[x]+a2)), nrow = 4, byrow = T)

#reversed version of this variant (can be used for kaumAra yantra)
mc2=matrix(rev(mc1), nrow=4, byrow = T)

#another
mc3=apply(mc2, 1, rev)
