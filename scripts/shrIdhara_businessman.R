n=23
a=3
r=2
j=n
k=1
jbox=c()
pbox=c()
while(j>0){
  jbox[k]=j
  if(j/2==floor(j/2)){
    pbox[k]="s"
    j=j/2
  } else {
    j=j-1
    pbox[k]="m"
  }
  k=k+1
}
pbox=rev(pbox)
s=1
for (j in pbox) {
  if(j=="m"){
    s=s*r
  } else{
    s=s^2
  }
}
s=a*s