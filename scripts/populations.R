library("readr")
library("dplyr")
g25 = read_csv("R/Dataexperiments/data/Global_25_PCA.csv")
g25s = read_csv("R/Dataexperiments/data/Global25_scaled.csv")
g25=data.frame(g25)
pops=strsplit(x=g25$Pop,split = ":" )
indv=unlist(lapply(pops, function(x) x[1]))
pops=unique(indv)
colr=sapply(indv, function(x) match(x, pops))
write.table(x = pops, file = "~/R/Dataexperiments/data/pops.txt")

#Indian ethogenesis---------------
j=2
par(mar=c(2,2,2,1), mgp=c(1,.4,0))
plot(x=g25[,j], y=g25[,(j+1)], pch=16, cex=.5, col=gray.colors(30)[colr %% 30], main = substitute(paste("PC", j, ",", k), list(j=j-1, k=j)), xlab = j-1, ylab = j)

points(g25[grep("Udegram|Aligram|Loebanr|Saidu|Barikot|Butkara|Arkotkila", g25$Pop),j], g25[grep("Udegram|Aligram|Loebanr|Saidu|Barikot|Butkara|Arkotkila", g25$Pop),j+1], pch=1, cex=1, col="blue") #Gandhara Grave+Swat
points(g25[grep("Brahmin|Iyer", g25$Pop, ignore.case = T),j], g25[grep("Brahmin|Iyer", g25$Pop, ignore.case = T),(j+1)], pch=16, col="red")
points(g25[grep("Yamnaya", g25$Pop, ignore.case = T),j], g25[grep("Yamnaya", g25$Pop, ignore.case = T),j+1], pch=16, col="darkviolet")
points(g25[grep("Sintashta|Srubnaya|Poltavka|Potapovka_MLBA|Kazakh_Mys", g25$Pop, ignore.case = T),j], g25[grep("Sintashta|Srubnaya|Poltavka|Potapovka_MLBA|Kazakh_Mys", g25$Pop, ignore.case = T),j+1], pch=16, col="darkred")
points(g25[grep("Han|Japanese", g25$Pop),j], g25[grep("Han|Japanese", g25$Pop),j+1], pch=16, col="darkgreen")
points(g25[grep("Alan", g25$Pop),j], g25[grep("Alan", g25$Pop),j+1], pch=16, col="darkblue")

points(g25[grep("Onge", g25$Pop),j], g25[grep("Onge", g25$Pop),j+1], pch=16, col="orange")
points(g25[grep("Paniya", g25$Pop),j], g25[grep("Paniya", g25$Pop),j+1], pch=16, col="black")

points(g25[grep("Turkmeni", g25$Pop),j], g25[grep("Turkmeni", g25$Pop),j+1], pch=15, col="darkblue") #Turkmenistan IA
points(g25[grep("Kalash", g25$Pop),j], g25[grep("Kalash", g25$Pop),j+1], pch=8, col="blue") #Turkmenistan IA
points(g25[grep("Ror", g25$Pop),j], g25[grep("Ror", g25$Pop),j+1], pch=4, col="orangered4") #Ror
points(g25[grep("Sarmatian", g25$Pop),c(j,j+k)], pch=16, cex=1, col="green") #Sarmatian

#labels 2,3
text(x=0,y=-.04,labels = "brAh", col="red")
text(x=-0.02226465,y=-0.01911738,labels = "brAh.E", col="red", pos=4)
text(x=-0.04199393,y=0.001367171,labels = "Jap/Han", col="darkgreen", pos = 4)
text(x=0.0112972,y=-0.005385978,labels = "Alans/T'stan IA", col="darkblue", pos = 4)
text(x=0.006749008,y=0.02005088,labels = "Steppe BA", col="darkred")
text(x=-0.02331969,y=-0.03555004,labels = "Onge", col="orange", pos=4)
text(x=-0.01761788,y=-0.05493501,labels = "Paniya", col="black", pos=4)
text(x=0.006338866,y=-0.02294417,labels = "Kalash", col="blue")
text(x=-0.0005307815,y=-0.01664123,labels = "Ror", col="orangered4")
text(x=0.002825712,y=0.004293535,labels = "Sarmat", col="green")

#clines
segments(x0=0.009175605, x1=-0.011292210, y0=-0.008312343, y1= -0.048831236, lty=3, col="black", lwd = 2)
segments(x0=0.001820451, x1=0.004534657, y0=-0.01776675, y1= 0.00812032, lty=3, col="blue", lwd = 2)

#legends for 1,2


legend(x="bottomleft", legend = c("brAhmaNa", "Han/Jap", "Onge" , "Ancient NW", "Kalash", "Ror", "Sarmatian", "Alan", "Yamnaya", "MLBA steppe", "Paniya"), pch=c(16, 16, 16,1, 8, 4, 16,16,16,16, 16), col=c("red", "darkgreen", "orange", "blue", "blue", "orangered4", "green", "darkblue","darkviolet","darkred", "black"), ncol = 2)

#Botai
points(g25[grep("Botai", g25$Pop),j], g25[grep("Botai", g25$Pop),j+1], pch=8, col="green") 

#CHG
points(g25[grep("CHG", g25$Pop),j], g25[grep("CHG", g25$Pop),j+1], pch=8, col="green") 

#BMAC
points(g25[grep("Gonur", g25$Pop),j], g25[grep("Gonur", g25$Pop),j+1], pch=8, col="green") 

#Greeks
points(g25[grep("Greek|Greec|Mycen", g25$Pop),j], g25[grep("Greek|Greec|Mycen", g25$Pop),j+1], pch=8, col="green") 

#Lithuanians
points(g25[grep("Lithuania", g25$Pop),j], g25[grep("Lithuania", g25$Pop),j+1], pch=8, col="green") 



#Mongols, Huns------------
j=3
k=1
par(mar=c(2,2,2,1), mgp=c(1,.4,0))
plot(x=g25[,c(j,j+k)], pch=16, cex=.5, col="gray65", main = substitute(paste("PC", j, ",", k), list(j=j-1, k=j)), xlab = j-1, ylab = j)

#Mongols/Huns
points(g25[grep("Han", g25$Pop),c(j,j+k)], pch=16, cex=1, col="darkgreen") #Han
points(g25[grep("Korean", g25$Pop),c(j,j+k)], pch=13, cex=1, col="darkgreen") #Korean
points(g25[grep("Japanese", g25$Pop),c(j,j+k)], pch=16, cex=1, col="darkviolet") #Jap
points(g25[grep("Mongoli", g25$Pop),c(j,j+k)], pch=16, cex=1, col="blue") #Mongols
te=g25[-grep("Kalmykia", g25$Pop),]
points(g25[grep("Kalmyk", te$Pop),c(j,j+k)], pch=16, cex=1, col="blue") #Kalmyk
points(g25[grep("Hazara", g25$Pop),c(j,j+k)], pch=16, cex=1, col="black") #Hazara
points(g25[grep("Hun_|Hun-", g25$Pop),c(j,j+k)], pch=16, cex=1, col="darkblue") #Huns
points(g25[grep("Xiong", g25$Pop),c(j,j+k)], pch=16, cex=1, col="cyan") #Xiongnu
points(g25[grep("Saka", g25$Pop),c(j,j+k)], pch=1, cex=1, col="red") #Saka
points(g25[grep("Sarmatian", g25$Pop),c(j,j+k)], pch=16, cex=1, col="green") #Sarmatian
points(g25[grep("Alan", g25$Pop),c(j,j+k)], pch=16, cex=1, col="red") #Alans
points(g25[grep("Tuvinian", g25$Pop),c(j,j+k)], pch=8, cex=1, col="blue") #Tuvinian
points(g25[grep("Yakut|Sakha", g25$Pop),c(j,j+k)], pch=4, cex=1, col="blue") #Yakut
points(g25[grep("Wusun", g25$Pop),c(j,j+k)], pch=4, cex=1, col="red") #Wusun
points(g25[grep("Hovsgol", g25$Pop),c(j,j+k)], pch=5, cex=1, col="blue") #Hovsgol Bronze age
points(g25[grep("Okunevo", g25$Pop),c(j,j+k)], pch=18, cex=1, col="blue") #Okunevo
points(g25[grep("Buryat", g25$Pop),c(j,j+k)], pch=15, cex=1, col="blue") #Buryat


legend(x="bottomleft", legend = c("Han", "Kor", "Jap", "Mon","Hazar" , "Hun", "Xiong", "Saka", "Sarmti", "Alan", "Tuvan", "Yakut", "Wusun", "Hovsgol", "Okunevo","Buryat"), pch=c(16, 13, rep(16,5),1,16,16, 8, 4, 4,5,18,15), col=c("darkgreen", "darkgreen", "darkviolet", "blue", "black", "darkblue", "cyan", "red","green","red", "blue", "blue", "red", "blue", "blue", "blue"), ncol = 2)

#clines
segments(x0=-0.032182042, x1=0.001051782, y0=0.01712452, y1=-0.02136843, lty=3, col="lightblue", lwd = 2)
segments(x0=0.009914134, x1=-0.014668345, y0=-0.007637028, y1= 0.020501093, lty=3, col="red", lwd = 2)
segments(x0=0.009914134, x1=-0.031232504, y0=-0.007637028, y1= 0.017349623, lty=3, col="red", lwd = 2)
segments(x0=0.009175605, x1=-0.011292210, y0=-0.008312343, y1= -0.048831236, lty=3, col="black", lwd = 2)

#text
text(x=-0.0007417899, y=-0.0380262, labels = "Indians")


#niShAda/Austroasiatic-----------
j=3
par(mar=c(2,2,2,1), mgp=c(1,.4,0))
plot(x=g25[,j], y=g25[,(j+1)], pch=16, cex=.5, col=gray.colors(30)[colr %% 30], main = substitute(paste("PC", j, ",", k), list(j=j-1, k=j)), xlab = j-1, ylab = j)

points(g25[grep("Bonda|Santhal|Korwa|Bihor|Gadaba|Bhumij|Juang|Khonda", g25$Pop),c(j,j+k)], pch=16, cex=1, col="darkblue") #Austroasiatic
points(g25[grep("Onge|Jarawa", g25$Pop),c(j,j+k)], pch=16, cex=1, col="orangered") #Onge/Jarawa
points(g25[grep("Paniya", g25$Pop),c(j,j+k)], pch=16, cex=1, col="black")
points(g25[grep("Gond|Asur", g25$Pop),c(j,j+k)], pch=1, cex=1, col="violet")
points(g25[grep("Irula|Malayan|Pulliyar|Kadar", g25$Pop),c(j,j+k)], pch=16, cex=1, col="darkgreen")

points(g25[grep("Australian", g25$Pop),c(j,j+k)], pch=16, cex=1, col="orange")
points(g25[grep("Nasoi", g25$Pop),c(j,j+k)], pch=16, cex=1, col="orange")
points(g25[grep("Papuan", g25$Pop),c(j,j+k)], pch=16, cex=1, col="orange")

points(g25[grep("Cambodian", g25$Pop),c(j,j+k)], pch=16, cex=1, col="cyan4")
points(g25[grep("Vietnam", g25$Pop),c(j,j+k)], pch=16, cex=1, col="cyan4")
points(g25[grep("Nui_Nap", g25$Pop),c(j,j+k)], pch=16, cex=1, col="cyan4")
points(g25[grep("Man_Bac", g25$Pop),c(j,j+k)], pch=16, cex=1, col="cyan4")
points(g25[grep("Thai|Dai", g25$Pop),c(j,j+k)], pch=16, cex=1, col="cyan2")
points(g25[grep("Atayal", g25$Pop),c(j,j+k)], pch=16, cex=1, col="blue")

#legend
legend(x="bottomright", legend = c("Ind.Aus.As","Andaman","Paniya", "Gond/Asur", "Tam.tribes", "Aus/Papuans","E.Aus.As" , "Kra-Dai", "Atayal"), pch=c(rep(16,3), 1, rep(16,5)), col=c("darkblue", "orangered", "black", "violet","darkgreen",  "orange", "cyan4", "cyan2", "blue"), ncol = 3)

#clines
segments(x0=-0.01878741, x1= -0.04200896, y0=-0.05130739, y1= -0.01213913, lty=3, col="black", lwd = 2) #Austric cline
segments(x0= -0.022707934  , x1= 0.008957808, y0=-0.05400865, y1= -0.03037263, lty=3, col="blue", lwd = 2) #Indian hunter gatherer-Iranian farmer

#text
text(x=-0.035575283, y=-0.0384, labels = "Austroasiatic cline")
text(x=-0.0027, y=-.049, labels = "ASI cline", col = "blue")

#ASI cline/Iranian farmer-----------
j=3
par(mar=c(2,2,2,1), mgp=c(1,.4,0))
plot(x=g25[,j], y=g25[,(j+1)], pch=16, cex=.5, col=gray.colors(30)[colr %% 30], main = substitute(paste("PC", j, ",", k), list(j=j-1, k=j)), xlab = j-1, ylab = j)

points(g25[grep("Gonur", g25$Pop),c(j,j+k)], pch=16, cex=1, col="blue") #Gonur
points(g25[grep("Gonur1_BA_o", g25$Pop),c(j,j+k)], pch=16, cex=1, col="orangered") #Gonur outlier
points(g25[grep("Shahr_I_Sokhta", g25$Pop),c(j,j+k)], pch=16, cex=1, col="cyan4") 
points(g25[grep("Namazga", g25$Pop),c(j,j+k)], pch=16, cex=1, col="skyblue")
points(g25[grep("CHG", g25$Pop),c(j,j+k)], pch=16, cex=1, col="hotpink")
points(g25[grep("Dzharkutan", g25$Pop),c(j,j+k)], pch=4, cex=1, col="cyan2")
points(g25[grep("Sappali", g25$Pop),c(j,j+k)], pch=1, cex=1, col="darkblue")
points(g25[grep("Hissar", g25$Pop),c(j,j+k)], pch=1, cex=1, col="darkblue")

points(g25[grep("Balochi", g25$Pop),c(j,j+k)], pch=1, cex=1, col="darksalmon")
points(g25[grep("Brahui", g25$Pop),c(j,j+k)], pch=1, cex=1, col="darkseagreen")

points(g25[grep("Maratha", g25$Pop),c(j,j+k)], pch=16, cex=1, col="blueviolet")
points(g25[grep("Velamas", g25$Pop),c(j,j+k)], pch=16, cex=1, col="blueviolet")
points(g25[grep("Yadava|Gupta|Relli|Piramalai|Chamar|Chenchu|Kapu|Kurumba|Kanjar|Dharkar", g25$Pop),c(j,j+k)], pch=16, cex=1, col="blueviolet")

points(g25[grep("Brahmin|Iyer", g25$Pop),c(j,j+k)], pch=16, cex=1, col="burlywood")
points(g25[grep("Kalash", g25$Pop),c(j,j+k)], pch=1, cex=1, col="red")

points(g25[grep("Onge|Jarawa", g25$Pop),c(j,j+k)], pch=16, cex=1, col="green") #Onge/Jarawa
points(g25[grep("Paniya", g25$Pop),c(j,j+k)], pch=16, cex=1, col="black")
points(g25[grep("Gond|Asur", g25$Pop),c(j,j+k)], pch=1, cex=1, col="black")
points(g25[grep("Irula|Malayan|Pulliyar|Kadar", g25$Pop),c(j,j+k)], pch=16, cex=1, col="black")
points(g25[grep("Australian", g25$Pop),c(j,j+k)], pch=16, cex=1, col="darkgreen")

points(g25[grep("Sintashta", g25$Pop),c(j,j+k)], pch=16, cex=1, col="orange")
points(g25[grep("Alan", g25$Pop),c(j,j+k)], pch=8, cex=1, col="red")

#legend
legend(x="bottomleft", legend = c("Gonur","Shahr_I_Sokhta","Namazga", "Dzharkutan", "Sappali/Hissar", "Balochi","Brahui" , "Mid/Ser Castes", "brAh", "Kalash", "tribes"), pch=c(16,16,1,4,1,1,1,16,16,16), col=c("blue", "cyan4", "skyblue", "cyan2","darkblue",  "darksalmon",  "darkseagreen", "blueviolet", "burlywood", "red","black"), ncol = 1)

text(x=0.0112972,y=-0.005385978,labels = "Alans", col="red", pos = 4)
text(x=0.006749008,y=0.02005088,labels = "Sintashta", col="orange")
text(x=-0.02331969,y=-0.03555004,labels = "Onge/Jarawa", col="green", pos=4)
text(x=-0.02341162,y=-0.06053669,labels = "Australian", col="darkgreen")
text(x=0.0106,y=-0.0224,labels = "CHG", col="hotpink", pos=4)
text(x=.00383,y=-0.0103,labels = "Gonur_o", col="orangered", pos=2)
text(x=-0.00602059,y=-0.05333334,labels = "ASI cline", col="blue")

#clines
segments(x0= -0.0221  , x1= 0.00785, y0=-0.056, y1= -0.033, lty=3, col="blue", lwd = 3) #Indian hunter gatherer-Iranian farmer

#Wusun, Brahmins Kangju etc----------
j=2
k=1
par(mar=c(2,2,2,1), mgp=c(1,.4,0))
plot(x=g25[,j], y=g25[,(j+1)], pch=16, cex=.5, col=gray.colors(30)[colr %% 30], main = substitute(paste("PC", j, ",", k), list(j=j-1, k=j)), xlab = j-1, ylab = j, xlim=c(-.003,.013), ylim=c(-.019,.0078))

points(g25[grep("Brahmin|Iyer", g25$Pop),c(j,j+k)], pch=16, cex=1, col="burlywood")
points(g25[grep("Kalash", g25$Pop),c(j,j+k)], pch=16, cex=1, col="red")
points(g25[grep("Wusun", g25$Pop),c(j,j+k)], pch=16, cex=1, col="orange")
points(g25[grep("Kangju", g25$Pop),c(j,j+k)], pch=16, cex=1, col="blueviolet")

#legend
legend(x="topleft", legend = c("brAh","Kalash","Wusun","Kangju"), pch=c(16,16,16,16), col=c("burlywood", "red", "orange", "blueviolet"), ncol = 1)

#getting some pop in PCA-----------
g25$Pop[which(g25$PC2 < (0.0005) & g25$PC2 > (-.0017) & g25$PC3 > (-.046) & g25$PC3 < (-.042))]
