source("functions.R")
source("privacy.R")

## Polish ods

SD2011M <- read.csv("SD2011M2.csv")

vars <- c("sex", "age", "edu", "marital", "income",
          "ls", "trust", "nofriend", "smoke", "alcabuse", 
          "wkabint", "englang", "height", "weight")
ods <- SD2011M[, vars]

for(v in vars){
  if(is.factor(ods[,v])){
    ods[,v] <- as.character(ods[,v])
    ods[,v] <- factor(ods[,v],levels(SD2011[,v]))
  }
}

ods$wkabint <- as.character(ods$wkabint)
ods$wkabint[ods$wkabint == "YES, TO EU COUNTRY" | 
              ods$wkabint == "YES, TO NON-EU COUNTRY"] <- "YES"
ods$wkabint <- factor(ods$wkabint,c("YES","NO"))

ods$marital <- as.character(ods$marital)
ods$marital[ods$marital == "LEGALLY SEPARATED" | 
              ods$marital == "DE FACTO SEPARATED"] <- "DIVORCED"
ods$marital <- factor(ods$marital,c("SINGLE","MARRIED","WIDOWED","DIVORCED"))

varsTest <- c("sex", "age", "height", "weight", "edu", "englang",
              "marital", "wkabint", "income", "ls", "trust",
              "nofriend", "smoke", "alcabuse")

polishRelations <- matrix(0,nrow = 14, ncol = 14, dimnames = list(colnames(ods),colnames(ods)))
polishRelations[c(3,5,9:14),1] <- 1
polishRelations[c(3:12),2] <- 1
polishRelations[c(4,5,12),3] <- 1
polishRelations[c(5,6,8:12),4] <- 1
polishRelations[c(6,8:13),5] <- 1
polishRelations[c(8:11,13,14),6] <- 1
polishRelations[c(8,12),7] <- 1
polishRelations[c(11),8] <- 1
polishRelations[c(10),9] <- 1
#polishRelations[c(),10] <- 1
polishRelations[c(12),11] <- 1
#polishRelations[c(),12] <- 1
polishRelations[c(14),13] <- 1
#polishRelations[c(),14] <- 1
polishRelations <- polishRelations + t(polishRelations)


## Adult ods

adultODS <- read.obs("adultdata.csv")

adultODSM <- adultODS

for(i in c(1:ncol(adultODS))){
  remAD <- is.na(adultODSM[,i])
  adultODSM <- adultODSM[!remAD,]
}

adultODSM <- adultODSM[,-c(3,5)]
eduLev <- levels(adultODSM$education)[c(14,4,5,6,7,1,2,3,12,15,16,9,8,10,13,11)]
adultODSM$education <- factor(adultODSM$education,eduLev)
adultODSM$relationship <- factor(adultODSM$relationship,
                                 c("Not-in-family","Other-relative","Own-child","Unmarried","Wife","Husband"))

Europe <- c("England","France","Germany","Greece","Holand-Netherlands",
            "Hungary","Ireland","Italy","Poland","Portugal","Scotland",
            "Yugoslavia")
Asia <- c("Cambodia","China","Hong","India","Iran","Japan","Laos",
          "Philippines","South","Taiwan","Thailand","Vietnam")
OtherUS <- c("Outlying-US(Guam-USVI-etc)","Puerto-Rico")
SouthAmerica <- c("Columbia","Cuba","Dominican-Republic","Ecuador",
                  "El-Salvador","Guatemala","Haiti","Honduras",
                  "Jamaica","Nicaragua","Peru","Trinadad&Tobago")

adultODSM2 <- adultODSM
adultODSM2$native.country <- as.character(adultODSM2$native.country)
for(cs in Europe){
  adultODSM2$native.country[adultODSM2$native.country == cs] <- "Europe"
}
for(cs in Asia){
  adultODSM2$native.country[adultODSM2$native.country == cs] <- "Asia"
}
for(cs in OtherUS){
  adultODSM2$native.country[adultODSM2$native.country == cs] <- "OtherUS"
}
for(cs in SouthAmerica){
  adultODSM2$native.country[adultODSM2$native.country == cs] <- "South-America"
}

adultODSM2$native.country <- factor(adultODSM2$native.country,
                                    c("United-States","OtherUS","Canada","Mexico",
                                      "South-America","Europe","Asia"))

adultODS10K <- adultODSM2[1:10000,]

LH2Order <- c(3,5,2,4,12,6,7,8,13,1,9,10,11)

adultRelations <- matrix(0,nrow = 13, ncol = 13, dimnames = list(colnames(adultODSM2),colnames(adultODSM2)))
adultRelations[c(2:6,11,13),1] <- 1
adultRelations[c(3:5,7:13),2] <- 1
adultRelations[c(4,5,7:13),3] <- 1
adultRelations[c(6,7,9:13),4] <- 1
adultRelations[c(7:13),5] <- 1
adultRelations[c(8,11:13),6] <- 1
adultRelations[c(9:13),7] <- 1
adultRelations[c(11:13),8] <- 1
adultRelations[c(12,13),9] <- 1
adultRelations[c(12,13),10] <- 1
adultRelations[c(12,13),11] <- 1
adultRelations[c(13),12] <- 1
#adultRelations[c(),13] <- 1
adultRelations <- adultRelations + t(adultRelations)

## Avila ods

avilaODS <- read.csv("avila-tr.txt",header = FALSE)
colnames(avilaODS) <- c("Int_Dist","Up_Marg","Lo_Marg","Exploit","Row_Num","Mod_Ratio","Int_Spac","Weight","Peak_Num","MR/IS","Class")

CM <- c(1,2,3,4,5,11,6,7,8,9,10)

avilaRelations <- matrix(0,nrow = 11, ncol = 11, dimnames = list(colnames(avilaODS),colnames(avilaODS)))
corAv <- cor(avilaODS[,1:10],method = "spearman") > 0.05
diag(corAv) <- FALSE
corAv <- rbind(cbind(corAv,TRUE),TRUE)
corAv[[11,11]] <- FALSE
avilaRelations[corAv] <- 1


## Play

load("sdsAdultM.RData")

sigma <- rep.int(0.1,13)
sigma[c(1,9,10,11)] <- c(1,1,1,1)

time3 <- list()
time3 <- c(time3,list(Sys.time()))
tst <- findPar(adultODSM2[c(1:1000,10001:11000),],"sdsAdultDecM",sigma,100)
time3 <- c(time3,list(Sys.time()))

time2 <- list()
time2 <- c(time2,list(Sys.time()))
tst5 <- findIncl(adultODSM2[c(1:100,10001:10100),],"sdsAdultDecM",sigma,100)
time2 <- c(time2,list(Sys.time()))



time <- list()
time <- c(time,list(Sys.time()))
tst1 <- findPar(adultODSM2[c(1:1000,10001:11000),],"sdsAdultDecM",sigma,100)
time <- c(time,list(Sys.time()))
tst2 <- findPar(adultODSM2[c(1:10000,10001:20000),],"sdsAdultDecM",sigma,100)
time <- c(time,list(Sys.time()))
tst3 <- findPar(adultODSM2[c(1:1000,10001:11000),],"sdsAdultDecM",sigma,1000)
time <- c(time,list(Sys.time()))
tst4 <- findPar(adultODSM2[c(1:10000,10001:20000),],"sdsAdultDecM",sigma,1000)
time <- c(time,list(Sys.time()))



sigma <- rep.int(0.1,13)
sigma[c(1,9,10,11)] <- c(1,1,1,1)
tstS1 <- findPar(adultODSM2[c(1:10000,10001:20000),],"sdsAdultDecM",sigma,1000)
gc()
sigma <- rep.int(0.1,13)
sigma[c(1,9,10,11)] <- c(1,100,100,1)
tstS2 <- findPar(adultODSM2[c(1:10000,10001:20000),],"sdsAdultDecM",sigma,1000)
gc()
sigma <- rep.int(0.1,13)
sigma[c(1,9,10,11)] <- c(5,5,5,5)
tstS3 <- findPar(adultODSM2[c(1:10000,10001:20000),],"sdsAdultDecM",sigma,1000)
gc()
sigma <- rep.int(0.1,13)
sigma[c(1,9,10,11)] <- c(5,500,500,5)
tstS4 <- findPar(adultODSM2[c(1:10000,10001:20000),],"sdsAdultDecM",sigma,1000)
gc()
sigma <- rep.int(0.01,13)
sigma[c(1,9,10,11)] <- c(1,1,1,1)
tstS5 <- findPar(adultODSM2[c(1:10000,10001:20000),],"sdsAdultDecM",sigma,1000)
gc()
sigma <- rep.int(0.01,13)
sigma[c(1,9,10,11)] <- c(1,100,100,1)
tstS6 <- findPar(adultODSM2[c(1:10000,10001:20000),],"sdsAdultDecM",sigma,1000)
gc()
sigma <- rep.int(0.01,13)
sigma[c(1,9,10,11)] <- c(5,5,5,5)
tstS7 <- findPar(adultODSM2[c(1:10000,10001:20000),],"sdsAdultDecM",sigma,1000)
gc()
sigma <- rep.int(0.01,13)
sigma[c(1,9,10,11)] <- c(5,500,500,5)
tstS8 <- findPar(adultODSM2[c(1:10000,10001:20000),],"sdsAdultDecM",sigma,1000)
gc()
sigma <- rep.int(0.1,13)
tstS9 <- findPar(adultODSM2[c(1:10000,10001:20000),],"sdsAdultDecM",sigma,1000)
gc()
sigma <- rep.int(0.01,13)
tstS10 <- findPar(adultODSM2[c(1:10000,10001:20000),],"sdsAdultDecM",sigma,1000)
gc()
sigma <- rep.int(1,13)
tstS11 <- findPar(adultODSM2[c(1:10000,10001:20000),],"sdsAdultDecM",sigma,1000)
gc()


pN <- 10^seq(-11,1,1)
aboM <- matrix(NA,4*11,length(pN))

for(i in c(1:11)){
  tstP <- eval(as.name(paste0("tstS",i)))
  for(j in c(1:length(pN))){
    sum1 <- sum(tstP[c(1:10000)] < pN[[j]])
    sum2 <- sum(tstP[c(10001:20000)] < pN[[j]])
    aboM[c(4*i-3,4*i-2,4*i-1,4*i),j] <- c(1-sum1/10000,1-sum2/10000,(sum2-sum1)/10000,sum2/(sum2+sum1))
  }
}

for(i in c(1:11)){
  tstP <- eval(as.name(paste0("tstS",i)))
  sum1 <- sum(tstP[c(1:10000)])
  sum2 <- sum(tstP[c(10001:20000)])
  print(sum2/(sum2+sum1))
}

lst = data.frame(Syn = factor(c("0.1,1,1","0.1,1,100","0.1,5,5","0.1,5,500","0.01,1,1","0.01,1,100","0.01,5,5","0.01,5,500","0.1,0.1,0.1","0.01,0.01,0.01","1,1,1"),
                              levels = c("0.1,1,1","0.1,1,100","0.1,5,5","0.1,5,500","0.01,1,1","0.01,1,100","0.01,5,5","0.01,5,500","0.1,0.1,0.1","0.01,0.01,0.01","1,1,1")) ,
                 Clr = c("darkorange","darkred","navy","red","blue","tomato","steelblue","forestgreen","limegreen","grey","black"),
                 Lntp = replicate(11,1),
                 Shp = replicate(11,19))

plt1D <- createPlot(t(aboM[seq(1,41,4),]),pN,lst) + labs(title = "True Positive Dec")
plt2D <- createPlot(t(aboM[seq(2,42,4),]),pN,lst) + labs(title = "False Positive Dec")
plt3D <- createPlot(t(aboM[seq(3,43,4),]),pN,lst) + labs(title = "Difference Dec")
plt4D <- createPlot(t(aboM[seq(4,44,4),]),pN,lst) + labs(title = "True Ratio Dec")

sigma <- rep.int(0.1,13)
sigma[c(1,9,10,11)] <- c(1,1,1,1)
tstSS1 <- findPar(adultODSM2[c(1:10000,10001:20000),],"sdsAdultParM",sigma,100)
gc()
sigma <- rep.int(0.1,13)
sigma[c(1,9,10,11)] <- c(1,100,100,1)
tstSS2 <- findPar(adultODSM2[c(1:10000,10001:20000),],"sdsAdultParM",sigma,100)
gc()
sigma <- rep.int(0.1,13)
sigma[c(1,9,10,11)] <- c(5,5,5,5)
tstSS3 <- findPar(adultODSM2[c(1:10000,10001:20000),],"sdsAdultParM",sigma,100)
gc()
sigma <- rep.int(0.1,13)
sigma[c(1,9,10,11)] <- c(5,500,500,5)
tstSS4 <- findPar(adultODSM2[c(1:10000,10001:20000),],"sdsAdultParM",sigma,100)
gc()
sigma <- rep.int(0.01,13)
sigma[c(1,9,10,11)] <- c(1,1,1,1)
tstSS5 <- findPar(adultODSM2[c(1:10000,10001:20000),],"sdsAdultParM",sigma,100)
gc()
sigma <- rep.int(0.01,13)
sigma[c(1,9,10,11)] <- c(1,100,100,1)
tstSS6 <- findPar(adultODSM2[c(1:10000,10001:20000),],"sdsAdultParM",sigma,100)
gc()
sigma <- rep.int(0.01,13)
sigma[c(1,9,10,11)] <- c(5,5,5,5)
tstSS7 <- findPar(adultODSM2[c(1:10000,10001:20000),],"sdsAdultParM",sigma,100)
gc()
sigma <- rep.int(0.01,13)
sigma[c(1,9,10,11)] <- c(5,500,500,5)
tstSS8 <- findPar(adultODSM2[c(1:10000,10001:20000),],"sdsAdultParM",sigma,100)
gc()
sigma <- rep.int(0.1,13)
tstSS9 <- findPar(adultODSM2[c(1:10000,10001:20000),],"sdsAdultParM",sigma,100)
gc()
sigma <- rep.int(0.01,13)
tstSS10 <- findPar(adultODSM2[c(1:10000,10001:20000),],"sdsAdultParM",sigma,100)
gc()
sigma <- rep.int(1,13)
tstSS11 <- findPar(adultODSM2[c(1:10000,10001:20000),],"sdsAdultParM",sigma,100)
gc()

pN <- 10^seq(-11,1,1)
aboM2 <- matrix(NA,4*11,length(pN))

for(i in c(1:11)){
  tstP <- eval(as.name(paste0("tstSS",i)))
  for(j in c(1:length(pN))){
    sum1 <- sum(tstP[c(1:10000)] < pN[[j]])
    sum2 <- sum(tstP[c(10001:20000)] < pN[[j]])
    aboM2[c(4*i-3,4*i-2,4*i-1,4*i),j] <- c(1-sum1/10000,1-sum2/10000,(sum2-sum1)/10000,sum2/(sum2+sum1))
  }
}

plt1P <- createPlot(t(aboM2[seq(1,41,4),]),pN,lst) + labs(title = "True Positive Par")
plt2P <- createPlot(t(aboM2[seq(2,42,4),]),pN,lst) + labs(title = "False Positive Par")
plt3P <- createPlot(t(aboM2[seq(3,43,4),]),pN,lst) + labs(title = "Difference Par")
plt4P <- createPlot(t(aboM2[seq(4,44,4),]),pN,lst) + labs(title = "True Ratio Par")

for(i in c(1:11)){
  tstP <- eval(as.name(paste0("tstSS",i)))
  sum1 <- sum(tstP[c(1:10000)])
  sum2 <- sum(tstP[c(10001:20000)])
  print(sum2/(sum2+sum1))
}

sigma <- rep.int(0.1,13)
sigma[c(1,9,10,11)] <- c(1,1,1,1)
tstSC1 <- findPar(adultODSM2[c(1:10000,10001:20000),],"sdsAdultCatMC",sigma,1000)
gc()
sigma <- rep.int(0.1,13)
sigma[c(1,9,10,11)] <- c(1,100,100,1)
tstSC2 <- findPar(adultODSM2[c(1:10000,10001:20000),],"sdsAdultCatMC",sigma,1000)
gc()
sigma <- rep.int(0.1,13)
sigma[c(1,9,10,11)] <- c(5,5,5,5)
tstSC3 <- findPar(adultODSM2[c(1:10000,10001:20000),],"sdsAdultCatMC",sigma,1000)
gc()
sigma <- rep.int(0.1,13)
sigma[c(1,9,10,11)] <- c(5,500,500,5)
tstSC4 <- findPar(adultODSM2[c(1:10000,10001:20000),],"sdsAdultCatMC",sigma,1000)
gc()
sigma <- rep.int(0.01,13)
sigma[c(1,9,10,11)] <- c(1,1,1,1)
tstSC5 <- findPar(adultODSM2[c(1:10000,10001:20000),],"sdsAdultCatMC",sigma,1000)
gc()
sigma <- rep.int(0.01,13)
sigma[c(1,9,10,11)] <- c(1,100,100,1)
tstSC6 <- findPar(adultODSM2[c(1:10000,10001:20000),],"sdsAdultCatMC",sigma,1000)
gc()
sigma <- rep.int(0.01,13)
sigma[c(1,9,10,11)] <- c(5,5,5,5)
tstSC7 <- findPar(adultODSM2[c(1:10000,10001:20000),],"sdsAdultCatMC",sigma,1000)
gc()
sigma <- rep.int(0.01,13)
sigma[c(1,9,10,11)] <- c(5,500,500,5)
tstSC8 <- findPar(adultODSM2[c(1:10000,10001:20000),],"sdsAdultCatMC",sigma,1000)
gc()
sigma <- rep.int(0.1,13)
tstSC9 <- findPar(adultODSM2[c(1:10000,10001:20000),],"sdsAdultCatMC",sigma,1000)
gc()
sigma <- rep.int(0.01,13)
tstSC10 <- findPar(adultODSM2[c(1:10000,10001:20000),],"sdsAdultCatMC",sigma,1000)
gc()
sigma <- rep.int(1,13)
tstSC11 <- findPar(adultODSM2[c(1:10000,10001:20000),],"sdsAdultCatMC",sigma,1000)
gc()


pN <- 10^seq(-11,1,1)
aboM3 <- matrix(NA,4*11,length(pN))

for(i in c(1:11)){
  tstP <- eval(as.name(paste0("tstSC",i)))
  for(j in c(1:length(pN))){
    sum1 <- sum(tstP[c(1:10000)] < pN[[j]])
    sum2 <- sum(tstP[c(10001:20000)] < pN[[j]])
    aboM3[c(4*i-3,4*i-2,4*i-1,4*i),j] <- c(1-sum1/10000,1-sum2/10000,(sum2-sum1)/10000,sum2/(sum2+sum1))
  }
}

plt1C <- createPlot(t(aboM3[seq(1,41,4),]),pN,lst) + labs(title = "True Positive CAC")
plt2C <- createPlot(t(aboM3[seq(2,42,4),]),pN,lst) + labs(title = "False Positive CAC")
plt3C <- createPlot(t(aboM3[seq(3,43,4),]),pN,lst) + labs(title = "Difference CAC")
plt4C <- createPlot(t(aboM3[seq(4,44,4),]),pN,lst) + labs(title = "True Ratio CAC")

for(i in c(1:11)){
  tstP <- eval(as.name(paste0("tstSC",i)))
  sum1 <- sum(tstP[c(1:10000)])
  sum2 <- sum(tstP[c(10001:20000)])
  print(sum2/(sum2+sum1))
}

plots <- c(paste0("plt",paste0(rep(c("1","2","3","4"),3),c(rep(c("D"),4),rep(c("P"),4),rep(c("C"),4)))))
for(c in plots){
  t.plot <- eval(as.name(c))
  ggsave(paste0("plots/",c,".pdf"),t.plot,device = "pdf")
}

tstS <- findPar(adultODSM2[c(1:10000,10001:20000),],"sdsAdultDecM",sigma,c(1:1000),TRUE,c(1:10000))
tstSC <- findPar(adultODSM2[c(1:10000,10001:20000),],"sdsAdultCatMC",sigma,c(1:1000),TRUE,c(1:10000))

c(sum(tstS[c(1:10000)] < 1e-6),sum(tstS[c(10001:20000)] < 1e-6))

## Create Smooth

load("sdsAdultM.RData")
sdsAdultDecMS <- createSmooth(sdsAdultDecM,adultODS10K)
sdsAdultDecMTS <- createSmooth(sdsAdultDecMT,adultODS10K)
rm(list = c("sdsAdultParM","sdsAdultParMT","sdsAdultDecM","sdsAdultDecMT"))
save(list = paste0("sdsAdult","DecM",c("","T"),"S"),file = "sdsAdultMSm.RData")
rm(list = paste0("sdsAdult","DecM",c("","T"),"S"))
gc()

load("sdsAdultMH.RData")
sdsAdultDecMHS <- createSmooth(sdsAdultDecMH,adultODS10K)
sdsAdultDecMHTS <- createSmooth(sdsAdultDecMHT,adultODS10K)
rm(list = c("sdsAdultParMH","sdsAdultParMHT","sdsAdultDecMH","sdsAdultDecMHT"))
save(list = paste0("sdsAdult","DecMH",c("","T"),"S"),file = "sdsAdultMHSm.RData")
rm(list = paste0("sdsAdult","DecMH",c("","T"),"S"))
gc()

load("sdsAdultML.RData")
sdsAdultDecMLS <- createSmooth(sdsAdultDecML,adultODS10K)
sdsAdultDecMLTS <- createSmooth(sdsAdultDecMLT,adultODS10K)
rm(list = c("sdsAdultParML","sdsAdultParMLT","sdsAdultDecML","sdsAdultDecMLT"))
save(list = paste0("sdsAdult","DecML",c("","T"),"S"),file = "sdsAdultMLSm.RData")
rm(list = paste0("sdsAdult","DecML",c("","T"),"S"))
gc()

load("sdsAdultMS.RData")
sdsAdultCatMPS <- createSmooth(sdsAdultCatMP,adultODS10K)
sdsAdultCatMPTS <- createSmooth(sdsAdultCatMPT,adultODS10K)
sdsAdultCatMCS <- createSmooth(sdsAdultCatMC,adultODS10K)
sdsAdultCatMCTS <- createSmooth(sdsAdultCatMCT,adultODS10K)
rm(list = c("sdsAdultCatMP","sdsAdultCatMPT","sdsAdultCatMC","sdsAdultCatMCT"))
save(list = paste0("sdsAdult","CatM",c("P","PT","C","CT"),"S"),file = "sdsAdultMSSm.RData")
rm(list = paste0("sdsAdult","CatM",c("P","PT","C","CT"),"S"))
gc()


## Testing
sigma1 <- rep.int(0.1,13)
sigma1[c(1,9,10,11)] <- c(1,1,1,1)
sigma2 <- rep.int(0.1,13)
sigma3 <- rep.int(1,13)
sigmaL <- list(sigma1,sigma2,sigma3)

#mcL <- list(c(1),c(1:10),c(1:100),c(1:1000))
#mcL2 <- list(c(1),c(1:10),c(1:100))

load("sdsAdultM.RData")

MIP <- multiPar(adultODSM2[c(1:10000,10001:20000),],"sdsAdultParM",sigmaL,c(1:100))
MIPT <- multiPar(adultODSM2[c(1:10000,10001:20000),],"sdsAdultParMT",sigmaL,c(1:100))
MID <- multiPar(adultODSM2[c(1:10000,10001:20000),],"sdsAdultDecM",sigmaL,c(1:1000))
MIDT <- multiPar(adultODSM2[c(1:10000,10001:20000),],"sdsAdultDecMT",sigmaL,c(1:1000))

rm(list = c("sdsAdultParM","sdsAdultParMT","sdsAdultDecM","sdsAdultDecMT"))
gc()

load("sdsAdultMH.RData")

MIPH <- multiPar(adultODSM2[c(1:10000,10001:20000),],"sdsAdultParMH",sigmaL,c(1:100))
MIPHT <- multiPar(adultODSM2[c(1:10000,10001:20000),],"sdsAdultParMHT",sigmaL,c(1:100))
MIDH <- multiPar(adultODSM2[c(1:10000,10001:20000),],"sdsAdultDecMH",sigmaL,c(1:1000))
MIDHT <- multiPar(adultODSM2[c(1:10000,10001:20000),],"sdsAdultDecMHT",sigmaL,c(1:1000))

rm(list = c("sdsAdultParMH","sdsAdultParMHT","sdsAdultDecMH","sdsAdultDecMHT"))
gc()

load("sdsAdultML.RData")

MIPL <- multiPar(adultODSM2[c(1:10000,10001:20000),],"sdsAdultParML",sigmaL,c(1:100))
MIPLT <- multiPar(adultODSM2[c(1:10000,10001:20000),],"sdsAdultParMLT",sigmaL,c(1:100))
MIDL <- multiPar(adultODSM2[c(1:10000,10001:20000),],"sdsAdultDecML",sigmaL,c(1:1000))
MIDLT <- multiPar(adultODSM2[c(1:10000,10001:20000),],"sdsAdultDecMLT",sigmaL,c(1:1000))

rm(list = c("sdsAdultParML","sdsAdultParMLT","sdsAdultDecML","sdsAdultDecMLT"))
gc()

load("sdsAdultMS.RData")

MICP <- multiPar(adultODSM2[c(1:10000,10001:20000),],"sdsAdultCatMP",sigmaL,c(1:1000))
MICPT <- multiPar(adultODSM2[c(1:10000,10001:20000),],"sdsAdultCatMPT",sigmaL,c(1:1000))
MICC <- multiPar(adultODSM2[c(1:10000,10001:20000),],"sdsAdultCatMC",sigmaL,c(1:1000))
MICCT <- multiPar(adultODSM2[c(1:10000,10001:20000),],"sdsAdultCatMCT",sigmaL,c(1:1000))

rm(list = c("sdsAdultCatMP","sdsAdultCatMPT","sdsAdultCatMC","sdsAdultCatMCT"))
gc()

save(list = c(paste0("MI",c("P","PT","D","DT")),paste0("MI",c("PH","PHT","DH","DHT")),
              paste0("MI",c("PL","PLT","DL","DLT")),paste0("MI",c("CP","CPT","CC","CCT"))),
     file = "MIN.RData")



load("sdsAdultMSm.RData")

MIDS <- multiPar(adultODSM2[c(1:10000,10001:20000),],"sdsAdultDecMS",sigmaL,c(1:1000))
MIDTS <- multiPar(adultODSM2[c(1:10000,10001:20000),],"sdsAdultDecMTS",sigmaL,c(1:1000))

rm(list = c("sdsAdultDecMS","sdsAdultDecMTS"))
gc()

load("sdsAdultMHSm.RData")

MIDHS <- multiPar(adultODSM2[c(1:10000,10001:20000),],"sdsAdultDecMHS",sigmaL,c(1:1000))
MIDHTS <- multiPar(adultODSM2[c(1:10000,10001:20000),],"sdsAdultDecMHTS",sigmaL,c(1:1000))

rm(list = c("sdsAdultDecMHS","sdsAdultDecMHTS"))
gc()

load("sdsAdultMLSm.RData")

MIDLS <- multiPar(adultODSM2[c(1:10000,10001:20000),],"sdsAdultDecMLS",sigmaL,c(1:1000))
MIDLTS <- multiPar(adultODSM2[c(1:10000,10001:20000),],"sdsAdultDecMLTS",sigmaL,c(1:1000))

rm(list = c("sdsAdultDecMLS","sdsAdultDecMLTS"))
gc()

load("sdsAdultMSSm.RData")

MICPS <- multiPar(adultODSM2[c(1:10000,10001:20000),],"sdsAdultCatMPS",sigmaL,c(1:1000))
MICPTS <- multiPar(adultODSM2[c(1:10000,10001:20000),],"sdsAdultCatMPTS",sigmaL,c(1:1000))
MICCS <- multiPar(adultODSM2[c(1:10000,10001:20000),],"sdsAdultCatMCS",sigmaL,c(1:1000))
MICCTS <- multiPar(adultODSM2[c(1:10000,10001:20000),],"sdsAdultCatMCTS",sigmaL,c(1:1000))

rm(list = c("sdsAdultCatMPS","sdsAdultCatMPTS","sdsAdultCatMCS","sdsAdultCatMCTS"))
gc()

save(list = c(paste0("MI",c("D","DT"),"S"),paste0("MI",c("DH","DHT"),"S"),
              paste0("MI",c("DL","DLT"),"S"),paste0("MI",c("CP","CPT","CC","CCT"),"S")),
     file = "MIS.RData")




load("sdsAdultMSe.RData")

MIPSe <- multiPar(adultODSM2[c(1:10000,10001:20000),],"sdsAdultParMSe",sigmaL,c(1:100))
MIPTSe <- multiPar(adultODSM2[c(1:10000,10001:20000),],"sdsAdultParMTSe",sigmaL,c(1:100))
MIDSe <- multiPar(adultODSM2[c(1:10000,10001:20000),],"sdsAdultDecMSe",sigmaL,c(1:1000))
MIDTSe <- multiPar(adultODSM2[c(1:10000,10001:20000),],"sdsAdultDecMTSe",sigmaL,c(1:1000))

rm(list = c("sdsAdultParMSe","sdsAdultParMTSe","sdsAdultDecMSe","sdsAdultDecMTSe"))
gc()

load("sdsAdultMHSe.RData")

MIPHSe <- multiPar(adultODSM2[c(1:10000,10001:20000),],"sdsAdultParMHSe",sigmaL,c(1:100))
MIPHTSe <- multiPar(adultODSM2[c(1:10000,10001:20000),],"sdsAdultParMHTSe",sigmaL,c(1:100))
MIDHSe <- multiPar(adultODSM2[c(1:10000,10001:20000),],"sdsAdultDecMHSe",sigmaL,c(1:1000))
MIDHTSe <- multiPar(adultODSM2[c(1:10000,10001:20000),],"sdsAdultDecMHTSe",sigmaL,c(1:1000))

rm(list = c("sdsAdultParMHSe","sdsAdultParMHTSe","sdsAdultDecMHSe","sdsAdultDecMHTSe"))
gc()

load("sdsAdultMLSe.RData")

MIPLSe <- multiPar(adultODSM2[c(1:10000,10001:20000),],"sdsAdultParMLSe",sigmaL,c(1:100))
MIPLTSe <- multiPar(adultODSM2[c(1:10000,10001:20000),],"sdsAdultParMLTSe",sigmaL,c(1:100))
MIDLSe <- multiPar(adultODSM2[c(1:10000,10001:20000),],"sdsAdultDecMLSe",sigmaL,c(1:1000))
MIDLTSe <- multiPar(adultODSM2[c(1:10000,10001:20000),],"sdsAdultDecMLTSe",sigmaL,c(1:1000))

rm(list = c("sdsAdultParMLSe","sdsAdultParMLTSe","sdsAdultDecMLSe","sdsAdultDecMLTSe"))
gc()

load("sdsAdultMSSe.RData")

MICPSe <- multiPar(adultODSM2[c(1:10000,10001:20000),],"sdsAdultCatMPSe",sigmaL,c(1:1000))
MICPTSe <- multiPar(adultODSM2[c(1:10000,10001:20000),],"sdsAdultCatMPTSe",sigmaL,c(1:1000))
MICCSe <- multiPar(adultODSM2[c(1:10000,10001:20000),],"sdsAdultCatMCSe",sigmaL,c(1:1000))
MICCTSe <- multiPar(adultODSM2[c(1:10000,10001:20000),],"sdsAdultCatMCTSe",sigmaL,c(1:1000))

rm(list = c("sdsAdultCatMPSe","sdsAdultCatMPTSe","sdsAdultCatMCSe","sdsAdultCatMCTSe"))
gc()

save(list = c(paste0("MI",c("P","PT","D","DT"),"Se"),paste0("MI",c("PH","PHT","DH","DHT"),"Se"),
              paste0("MI",c("PL","PLT","DL","DLT"),"Se"),paste0("MI",c("CP","CPT","CC","CCT"),"Se")),
     file = "MISe.RData")



sigma1 <- rep.int(0.1,14)
sigma1[c(2,5,8,13,14)] <- c(1,1,1,1,1)
sigma2 <- rep.int(0.1,14)
sigma3 <- rep.int(1,14)
sigmaL <- list(sigma1,sigma2,sigma3)


load("sdsPolishMH.RData")

MIPP <- multiPar(ods[c(1:2500,2501:5000),],"sdsPolishParMH",sigmaL,c(1:1000))
MIPPT <- multiPar(ods[c(1:2500,2501:5000),],"sdsPolishParMTH",sigmaL,c(1:1000))
MIPD <- multiPar(ods[c(1:2500,2501:5000),],"sdsPolishDecMH",sigmaL,c(1:1000))
MIPDT <- multiPar(ods[c(1:2500,2501:5000),],"sdsPolishDecMTH",sigmaL,c(1:1000))

rm(list = c("sdsPolishParMH","sdsPolishParMTH","sdsPolishDecMH","sdsPolishDecMTH"))
gc()

load("sdsPolishMSH.RData")

MIPCP <- multiPar(ods[c(1:2500,2501:5000),],"sdsPolishCatMPH",sigmaL,c(1:1000))
MIPCPT <- multiPar(ods[c(1:2500,2501:5000),],"sdsPolishCatMPTH",sigmaL,c(1:1000))
MIPCC <- multiPar(ods[c(1:2500,2501:5000),],"sdsPolishCatMCH",sigmaL,c(1:1000))
MIPCCT <- multiPar(ods[c(1:2500,2501:5000),],"sdsPolishCatMCTH",sigmaL,c(1:1000))

rm(list = c("sdsPolishCatMPH","sdsPolishCatMPTH","sdsPolishCatMCH","sdsPolishCatMCTH"))
gc()

save(list = c(paste0("MIP",c("P","PT","D","DT")),paste0("MIP",c("CP","CPT","CC","CCT"))),
     file = "MIP.RData")


sigma1 <- rep.int(0.01,11)
sigma2 <- rep.int(0.001,11)
sigma3 <- rep.int(0.1,11)
sigmaL <- list(sigma1,sigma2,sigma3)

load("sdsAvilaMH.RData")

MIAP <- multiPar(avilaODS[c(1:5215,5216:10430),],"sdsAvilaParMH",sigmaL,c(1:1000))
MIAPT <- multiPar(avilaODS[c(1:5215,5216:10430),],"sdsAvilaParMTH",sigmaL,c(1:1000))
MIAD <- multiPar(avilaODS[c(1:5215,5216:10430),],"sdsAvilaDecMH",sigmaL,c(1:1000))
MIADT <- multiPar(avilaODS[c(1:5215,5216:10430),],"sdsAvilaDecMTH",sigmaL,c(1:1000))

rm(list = c("sdsAvilaParMH","sdsAvilaParMTH","sdsAvilaDecMH","sdsAvilaDecMTH"))
gc()

load("sdsAvilaMSH.RData")

MIACP <- multiPar(avilaODS[c(1:5215,5216:10430),],"sdsAvilaCatMPH",sigmaL,c(1:1000))
MIACPT <- multiPar(avilaODS[c(1:5215,5216:10430),],"sdsAvilaCatMPTH",sigmaL,c(1:1000))
MIACC <- multiPar(avilaODS[c(1:5215,5216:10430),],"sdsAvilaCatMCH",sigmaL,c(1:1000))
MIACCT <- multiPar(avilaODS[c(1:5215,5216:10430),],"sdsAvilaCatMCTH",sigmaL,c(1:1000))

rm(list = c("sdsAvilaCatMPH","sdsAvilaCatMPTH","sdsAvilaCatMCH","sdsAvilaCatMCTH"))
gc()

save(list = c(paste0("MIA",c("P","PT","D","DT")),paste0("MIA",c("CP","CPT","CC","CCT"))),
     file = "MIA.RData")


## Plot
lst = data.frame(Syn = factor(c("Medium 1","Medium 10","Medium 100","Medium 1000","High 1","High 10","High 100","High 1000","Low 1","Low 10","Low 100","Low 1000"),
                              levels = c("Medium 1","Medium 10","Medium 100","Medium 1000","High 1","High 10","High 100","High 1000","Low 1","Low 10","Low 100","Low 1000")) ,
                 Clr = c("darkorange","darkred","navy","red","blue","tomato","steelblue","forestgreen","limegreen","grey","black","purple"),
                 Lntp = replicate(12,1),
                 Shp = replicate(12,19))
pN <- 10^seq(-50,20,1)
pN <- c(rbind(pN,2*pN,3*pN,5*pN))[1:(4*length(pN)-3)]

load("MIN.RData")
ml <- NULL
ml <- c(c(1:9),c(1:9)*10,c(1:9)*100,1000)
ml <- c(1:100)

data <- expand.grid(X = c(1:length(pN)), Y = ml)
#data$Z <- c(aboP[[1]][c(101:200),])
#ggplot(data, aes(X, Y, fill= Z)) + geom_tile() + scale_fill_gradient(low="white", high="blue")

aboP <- multiMIPar(MIP,pN,10000,10000,100,ml)

plt1P <- createPlot(t(aboP[[1]]),pN,lst[-seq(4,12,4),]) + labs(title = "True Positive P") + scale_y_continuous(breaks = seq(0,1,0.05))
plt2P <- createPlot(t(aboP[[2]]),pN,lst[-seq(4,12,4),]) + labs(title = "False Positive P") + scale_y_continuous(breaks = seq(0,1,0.05))
plt3P <- createPlot(t(aboP[[3]]),pN,lst[-seq(4,12,4),]) + labs(title = "Difference P")
plt4P <- createPlot(t(aboP[[4]]),pN,lst[-seq(4,12,4),]) + labs(title = "False Ratio P")

aboPT <- multiMIPar(MIPT,pN,10000,10000,100,ml)

plt1PT <- createPlot(t(aboPT[[1]]),pN,lst[-seq(4,12,4),]) + labs(title = "True Positive PT") + scale_y_continuous(breaks = seq(0,1,0.05))
plt2PT <- createPlot(t(aboPT[[2]]),pN,lst[-seq(4,12,4),]) + labs(title = "False Positive PT") + scale_y_continuous(breaks = seq(0,1,0.05))
plt3PT <- createPlot(t(aboPT[[3]]),pN,lst[-seq(4,12,4),]) + labs(title = "Difference PT")
plt4PT <- createPlot(t(aboPT[[4]]),pN,lst[-seq(4,12,4),]) + labs(title = "False Ratio PT")

time <- list()
time <- c(time,list(Sys.time()))
aboD <- multiMIPar(MID,pN,10000,10000,100,ml)
time <- c(time,list(Sys.time()))

plt1D <- createPlot(t(aboD[[1]]),pN,lst) + labs(title = "True Positive D") + scale_y_continuous(breaks = seq(0,1,0.05))
plt2D <- createPlot(t(aboD[[2]]),pN,lst) + labs(title = "False Positive D") + scale_y_continuous(breaks = seq(0,1,0.05))
plt3D <- createPlot(t(aboD[[3]]),pN,lst) + labs(title = "Difference D")
plt4D <- createPlot(t(aboD[[4]]),pN,lst) + labs(title = "False Ratio D")

aboDT <- multiMIPar(MIDT,pN,10000,10000,100,ml)

plt1DT <- createPlot(t(aboDT[[1]]),pN,lst) + labs(title = "True Positive DT") + scale_y_continuous(breaks = seq(0,1,0.05))
plt2DT <- createPlot(t(aboDT[[2]]),pN,lst) + labs(title = "False Positive DT") + scale_y_continuous(breaks = seq(0,1,0.05))
plt3DT <- createPlot(t(aboDT[[3]]),pN,lst) + labs(title = "Difference DT")
plt4DT <- createPlot(t(aboDT[[4]]),pN,lst) + labs(title = "False Ratio DT")

plots <- c(paste0("plt",paste0(rep(c("1","2","3","4"),4),c(rep(c("D"),4),rep(c("P"),4),rep(c("DT"),4),rep(c("PT"),4)))))
for(c in plots){
  t.plot <- eval(as.name(c))
  ggsave(paste0("plots/",c,".pdf"),t.plot,device = "pdf")
}


aboPH <- multiMIPar(MIPH,pN,10000,10000,100,ml)

plt1PH <- createPlot(t(aboPH[[1]]),pN,lst[-seq(4,12,4),]) + labs(title = "True Positive PH") + scale_y_continuous(breaks = seq(0,1,0.05))
plt2PH <- createPlot(t(aboPH[[2]]),pN,lst[-seq(4,12,4),]) + labs(title = "False Positive PH") + scale_y_continuous(breaks = seq(0,1,0.05))
plt3PH <- createPlot(t(aboPH[[3]]),pN,lst[-seq(4,12,4),]) + labs(title = "Difference PH")
plt4PH <- createPlot(t(aboPH[[4]]),pN,lst[-seq(4,12,4),]) + labs(title = "False Ratio PH")

aboPHT <- multiMIPar(MIPHT,pN,10000,10000,100,ml)

plt1PHT <- createPlot(t(aboPHT[[1]]),pN,lst[-seq(4,12,4),]) + labs(title = "True Positive PHT") + scale_y_continuous(breaks = seq(0,1,0.05))
plt2PHT <- createPlot(t(aboPHT[[2]]),pN,lst[-seq(4,12,4),]) + labs(title = "False Positive PHT") + scale_y_continuous(breaks = seq(0,1,0.05))
plt3PHT <- createPlot(t(aboPHT[[3]]),pN,lst[-seq(4,12,4),]) + labs(title = "Difference PHT")
plt4PHT <- createPlot(t(aboPHT[[4]]),pN,lst[-seq(4,12,4),]) + labs(title = "False Ratio PHT")

aboDH <- multiMIPar(MIDH,pN,10000,10000,100,ml)

plt1DH <- createPlot(t(aboDH[[1]]),pN,lst) + labs(title = "True Positive DH") + scale_y_continuous(breaks = seq(0,1,0.05))
plt2DH <- createPlot(t(aboDH[[2]]),pN,lst) + labs(title = "False Positive DH") + scale_y_continuous(breaks = seq(0,1,0.05))
plt3DH <- createPlot(t(aboDH[[3]]),pN,lst) + labs(title = "Difference DH")
plt4DH <- createPlot(t(aboDH[[4]]),pN,lst) + labs(title = "False Ratio DH")

aboDHT <- multiMIPar(MIDHT,pN,10000,10000,100,ml)

plt1DHT <- createPlot(t(aboDHT[[1]]),pN,lst) + labs(title = "True Positive DHT") + scale_y_continuous(breaks = seq(0,1,0.05))
plt2DHT <- createPlot(t(aboDHT[[2]]),pN,lst) + labs(title = "False Positive DHT") + scale_y_continuous(breaks = seq(0,1,0.05))
plt3DHT <- createPlot(t(aboDHT[[3]]),pN,lst) + labs(title = "Difference DHT")
plt4DHT <- createPlot(t(aboDHT[[4]]),pN,lst) + labs(title = "False Ratio DHT")

plots <- c(paste0("plt",paste0(rep(c("1","2","3","4"),4),c(rep(c("DH"),4),rep(c("PH"),4),rep(c("DHT"),4),rep(c("PHT"),4)))))
for(c in plots){
  t.plot <- eval(as.name(c))
  ggsave(paste0("plots/",c,".pdf"),t.plot,device = "pdf")
}

aboPL <- multiMIPar(MIPL,pN,10000,10000,100,ml)

plt1PL <- createPlot(t(aboPL[[1]]),pN,lst[-seq(4,12,4),]) + labs(title = "True Positive PL") + scale_y_continuous(breaks = seq(0,1,0.05))
plt2PL <- createPlot(t(aboPL[[2]]),pN,lst[-seq(4,12,4),]) + labs(title = "False Positive PL") + scale_y_continuous(breaks = seq(0,1,0.05))
plt3PL <- createPlot(t(aboPL[[3]]),pN,lst[-seq(4,12,4),]) + labs(title = "Difference PL")
plt4PL <- createPlot(t(aboPL[[4]]),pN,lst[-seq(4,12,4),]) + labs(title = "False Ratio PL")

aboPLT <- multiMIPar(MIPLT,pN,10000,10000,100,ml)

plt1PLT <- createPlot(t(aboPLT[[1]]),pN,lst[-seq(4,12,4),]) + labs(title = "True Positive PLT") + scale_y_continuous(breaks = seq(0,1,0.05))
plt2PLT <- createPlot(t(aboPLT[[2]]),pN,lst[-seq(4,12,4),]) + labs(title = "False Positive PLT") + scale_y_continuous(breaks = seq(0,1,0.05))
plt3PLT <- createPlot(t(aboPLT[[3]]),pN,lst[-seq(4,12,4),]) + labs(title = "Difference PLT")
plt4PLT <- createPlot(t(aboPLT[[4]]),pN,lst[-seq(4,12,4),]) + labs(title = "False Ratio PLT")

aboDL <- multiMIPar(MIDL,pN,10000,10000,100,ml)

plt1DL <- createPlot(t(aboDL[[1]]),pN,lst) + labs(title = "True Positive DL") + scale_y_continuous(breaks = seq(0,1,0.05))
plt2DL <- createPlot(t(aboDL[[2]]),pN,lst) + labs(title = "False Positive DL") + scale_y_continuous(breaks = seq(0,1,0.05))
plt3DL <- createPlot(t(aboDL[[3]]),pN,lst) + labs(title = "Difference DL")
plt4DL <- createPlot(t(aboDL[[4]]),pN,lst) + labs(title = "False Ratio DL")

aboDLT <- multiMIPar(MIDLT,pN,10000,10000,100,ml)

plt1DLT <- createPlot(t(aboDLT[[1]]),pN,lst) + labs(title = "True Positive DLT") + scale_y_continuous(breaks = seq(0,1,0.05))
plt2DLT <- createPlot(t(aboDLT[[2]]),pN,lst) + labs(title = "False Positive DLT") + scale_y_continuous(breaks = seq(0,1,0.05))
plt3DLT <- createPlot(t(aboDLT[[3]]),pN,lst) + labs(title = "Difference DLT")
plt4DLT <- createPlot(t(aboDLT[[4]]),pN,lst) + labs(title = "False Ratio DLT")

plots <- c(paste0("plt",paste0(rep(c("1","2","3","4"),4),c(rep(c("DL"),4),rep(c("PL"),4),rep(c("DLT"),4),rep(c("PLT"),4)))))
for(c in plots){
  t.plot <- eval(as.name(c))
  ggsave(paste0("plots/",c,".pdf"),t.plot,device = "pdf")
}

aboCP <- multiMIPar(MICP,pN,10000,10000,100,ml)

plt1CP <- createPlot(t(aboCP[[1]]),pN,lst) + labs(title = "True Positive CP") + scale_y_continuous(breaks = seq(0,1,0.05))
plt2CP <- createPlot(t(aboCP[[2]]),pN,lst) + labs(title = "False Positive CP") + scale_y_continuous(breaks = seq(0,1,0.05))
plt3CP <- createPlot(t(aboCP[[3]]),pN,lst) + labs(title = "Difference CP")
plt4CP <- createPlot(t(aboCP[[4]]),pN,lst) + labs(title = "False Ratio CP")

aboCPT <- multiMIPar(MICPT,pN,10000,10000,100,ml)

plt1CPT <- createPlot(t(aboCPT[[1]]),pN,lst) + labs(title = "True Positive CPT") + scale_y_continuous(breaks = seq(0,1,0.05))
plt2CPT <- createPlot(t(aboCPT[[2]]),pN,lst) + labs(title = "False Positive CPT") + scale_y_continuous(breaks = seq(0,1,0.05))
plt3CPT <- createPlot(t(aboCPT[[3]]),pN,lst) + labs(title = "Difference CPT")
plt4CPT <- createPlot(t(aboCPT[[4]]),pN,lst) + labs(title = "False Ratio CPT")

aboCC <- multiMIPar(MICC,pN,10000,10000,100,ml)

plt1CC <- createPlot(t(aboCC[[1]]),pN,lst) + labs(title = "True Positive CC") + scale_y_continuous(breaks = seq(0,1,0.05))
plt2CC <- createPlot(t(aboCC[[2]]),pN,lst) + labs(title = "False Positive CC") + scale_y_continuous(breaks = seq(0,1,0.05))
plt3CC <- createPlot(t(aboCC[[3]]),pN,lst) + labs(title = "Difference CC")
plt4CC <- createPlot(t(aboCC[[4]]),pN,lst) + labs(title = "False Ratio CC")

aboCCT <- multiMIPar(MICCT,pN,10000,10000,100,ml)

plt1CCT <- createPlot(t(aboCCT[[1]]),pN,lst) + labs(title = "True Positive CCT") + scale_y_continuous(breaks = seq(0,1,0.05))
plt2CCT <- createPlot(t(aboCCT[[2]]),pN,lst) + labs(title = "False Positive CCT") + scale_y_continuous(breaks = seq(0,1,0.05))
plt3CCT <- createPlot(t(aboCCT[[3]]),pN,lst) + labs(title = "Difference CCT")
plt4CCT <- createPlot(t(aboCCT[[4]]),pN,lst) + labs(title = "False Ratio CCT")

plots <- c(paste0("plt",paste0(rep(c("1","2","3","4"),4),c(rep(c("CC"),4),rep(c("CP"),4),rep(c("CCT"),4),rep(c("CPT"),4)))))
for(c in plots){
  t.plot <- eval(as.name(c))
  ggsave(paste0("plots/",c,".pdf"),t.plot,device = "pdf")
}


## Smooth

load("MIS.RData")


aboDS <- multiMIPar(MIDS,pN,10000,10000,100,ml)

plt1DS <- createPlot(t(aboDS[[1]]),pN,lst) + labs(title = "True Positive DS") + scale_y_continuous(breaks = seq(0,1,0.05))
plt2DS <- createPlot(t(aboDS[[2]]),pN,lst) + labs(title = "False Positive DS") + scale_y_continuous(breaks = seq(0,1,0.05))
plt3DS <- createPlot(t(aboDS[[3]]),pN,lst) + labs(title = "Difference DS")
plt4DS <- createPlot(t(aboDS[[4]]),pN,lst) + labs(title = "False Ratio DS")

aboDTS <- multiMIPar(MIDTS,pN,10000,10000,100,ml)

plt1DTS <- createPlot(t(aboDTS[[1]]),pN,lst) + labs(title = "True Positive DTS") + scale_y_continuous(breaks = seq(0,1,0.05))
plt2DTS <- createPlot(t(aboDTS[[2]]),pN,lst) + labs(title = "False Positive DTS") + scale_y_continuous(breaks = seq(0,1,0.05))
plt3DTS <- createPlot(t(aboDTS[[3]]),pN,lst) + labs(title = "Difference DTS")
plt4DTS <- createPlot(t(aboDTS[[4]]),pN,lst) + labs(title = "False Ratio DTS")

plots <- c(paste0("plt",paste0(rep(c("1","2","3","4"),2),c(rep(c("DS"),4),rep(c("DTS"),4)))))
for(c in plots){
  t.plot <- eval(as.name(c))
  ggsave(paste0("plots/",c,".pdf"),t.plot,device = "pdf")
}


aboDHS <- multiMIPar(MIDHS,pN,10000,10000,100,ml)

plt1DHS <- createPlot(t(aboDHS[[1]]),pN,lst) + labs(title = "True Positive DHS") + scale_y_continuous(breaks = seq(0,1,0.05))
plt2DHS <- createPlot(t(aboDHS[[2]]),pN,lst) + labs(title = "False Positive DHS") + scale_y_continuous(breaks = seq(0,1,0.05))
plt3DHS <- createPlot(t(aboDHS[[3]]),pN,lst) + labs(title = "Difference DHS")
plt4DHS <- createPlot(t(aboDHS[[4]]),pN,lst) + labs(title = "False Ratio DHS")

aboDHTS <- multiMIPar(MIDHTS,pN,10000,10000,100,ml)

plt1DHTS <- createPlot(t(aboDHTS[[1]]),pN,lst) + labs(title = "True Positive DHTS") + scale_y_continuous(breaks = seq(0,1,0.05))
plt2DHTS <- createPlot(t(aboDHTS[[2]]),pN,lst) + labs(title = "False Positive DHTS") + scale_y_continuous(breaks = seq(0,1,0.05))
plt3DHTS <- createPlot(t(aboDHTS[[3]]),pN,lst) + labs(title = "Difference DHTS")
plt4DHTS <- createPlot(t(aboDHTS[[4]]),pN,lst) + labs(title = "False Ratio DHTS")

plots <- c(paste0("plt",paste0(rep(c("1","2","3","4"),2),c(rep(c("DHS"),4),rep(c("DHTS"),4)))))
for(c in plots){
  t.plot <- eval(as.name(c))
  ggsave(paste0("plots/",c,".pdf"),t.plot,device = "pdf")
}


aboDLS <- multiMIPar(MIDLS,pN,10000,10000,100,ml)

plt1DLS <- createPlot(t(aboDLS[[1]]),pN,lst) + labs(title = "True Positive DLS") + scale_y_continuous(breaks = seq(0,1,0.05))
plt2DLS <- createPlot(t(aboDLS[[2]]),pN,lst) + labs(title = "False Positive DLS") + scale_y_continuous(breaks = seq(0,1,0.05))
plt3DLS <- createPlot(t(aboDLS[[3]]),pN,lst) + labs(title = "Difference DLS")
plt4DLS <- createPlot(t(aboDLS[[4]]),pN,lst) + labs(title = "False Ratio DLS")

aboDLTS <- multiMIPar(MIDLTS,pN,10000,10000,100,ml)

plt1DLTS <- createPlot(t(aboDLTS[[1]]),pN,lst) + labs(title = "True Positive DLTS") + scale_y_continuous(breaks = seq(0,1,0.05))
plt2DLTS <- createPlot(t(aboDLTS[[2]]),pN,lst) + labs(title = "False Positive DLTS") + scale_y_continuous(breaks = seq(0,1,0.05))
plt3DLTS <- createPlot(t(aboDLTS[[3]]),pN,lst) + labs(title = "Difference DLTS")
plt4DLTS <- createPlot(t(aboDLTS[[4]]),pN,lst) + labs(title = "False Ratio DLTS")

plots <- c(paste0("plt",paste0(rep(c("1","2","3","4"),2),c(rep(c("DLS"),4),rep(c("DLTS"),4)))))
for(c in plots){
  t.plot <- eval(as.name(c))
  ggsave(paste0("plots/",c,".pdf"),t.plot,device = "pdf")
}

aboCPS <- multiMIPar(MICPS,pN,10000,10000,100,ml)

plt1CPS <- createPlot(t(aboCPS[[1]]),pN,lst) + labs(title = "True Positive CPS") + scale_y_continuous(breaks = seq(0,1,0.05))
plt2CPS <- createPlot(t(aboCPS[[2]]),pN,lst) + labs(title = "False Positive CPS") + scale_y_continuous(breaks = seq(0,1,0.05))
plt3CPS <- createPlot(t(aboCPS[[3]]),pN,lst) + labs(title = "Difference CPS")
plt4CPS <- createPlot(t(aboCPS[[4]]),pN,lst) + labs(title = "False Ratio CPS")

aboCPTS <- multiMIPar(MICPTS,pN,10000,10000,100,ml)

plt1CPTS <- createPlot(t(aboCPTS[[1]]),pN,lst) + labs(title = "True Positive CPTS") + scale_y_continuous(breaks = seq(0,1,0.05))
plt2CPTS <- createPlot(t(aboCPTS[[2]]),pN,lst) + labs(title = "False Positive CPTS") + scale_y_continuous(breaks = seq(0,1,0.05))
plt3CPTS <- createPlot(t(aboCPTS[[3]]),pN,lst) + labs(title = "Difference CPTS")
plt4CPTS <- createPlot(t(aboCPTS[[4]]),pN,lst) + labs(title = "False Ratio CPTS")

aboCCS <- multiMIPar(MICCS,pN,10000,10000,100,ml)

plt1CCS <- createPlot(t(aboCCS[[1]]),pN,lst) + labs(title = "True Positive CCS") + scale_y_continuous(breaks = seq(0,1,0.05))
plt2CCS <- createPlot(t(aboCCS[[2]]),pN,lst) + labs(title = "False Positive CCS") + scale_y_continuous(breaks = seq(0,1,0.05))
plt3CCS <- createPlot(t(aboCCS[[3]]),pN,lst) + labs(title = "Difference CCS")
plt4CCS <- createPlot(t(aboCCS[[4]]),pN,lst) + labs(title = "False Ratio CCS")

aboCCTS <- multiMIPar(MICCTS,pN,10000,10000,100,ml)

plt1CCTS <- createPlot(t(aboCCTS[[1]]),pN,lst) + labs(title = "True Positive CCTS") + scale_y_continuous(breaks = seq(0,1,0.05))
plt2CCTS <- createPlot(t(aboCCTS[[2]]),pN,lst) + labs(title = "False Positive CCTS") + scale_y_continuous(breaks = seq(0,1,0.05))
plt3CCTS <- createPlot(t(aboCCTS[[3]]),pN,lst) + labs(title = "Difference CCTS")
plt4CCTS <- createPlot(t(aboCCTS[[4]]),pN,lst) + labs(title = "False Ratio CCTS")

plots <- c(paste0("plt",paste0(rep(c("1","2","3","4"),4),c(rep(c("CCS"),4),rep(c("CPS"),4),rep(c("CCTS"),4),rep(c("CPTS"),4)))))
for(c in plots){
  t.plot <- eval(as.name(c))
  ggsave(paste0("plots/",c,".pdf"),t.plot,device = "pdf")
}

## Selective

load("MISe.RData")

aboPSe <- multiMIPar(MIPSe,pN,10000,10000,100,ml)

plt1PSe <- createPlot(t(aboPSe[[1]]),pN,lst[-seq(4,12,4),]) + labs(title = "True Positive PSe") + scale_y_continuous(breaks = seq(0,1,0.05))
plt2PSe <- createPlot(t(aboPSe[[2]]),pN,lst[-seq(4,12,4),]) + labs(title = "False Positive PSe") + scale_y_continuous(breaks = seq(0,1,0.05))
plt3PSe <- createPlot(t(aboPSe[[3]]),pN,lst[-seq(4,12,4),]) + labs(title = "Difference PSe")
plt4PSe <- createPlot(t(aboPSe[[4]]),pN,lst[-seq(4,12,4),]) + labs(title = "False Ratio PSe")

aboPTSe <- multiMIPar(MIPTSe,pN,10000,10000,100,ml)

plt1PTSe <- createPlot(t(aboPTSe[[1]]),pN,lst[-seq(4,12,4),]) + labs(title = "True Positive PTSe") + scale_y_continuous(breaks = seq(0,1,0.05))
plt2PTSe <- createPlot(t(aboPTSe[[2]]),pN,lst[-seq(4,12,4),]) + labs(title = "False Positive PTSe") + scale_y_continuous(breaks = seq(0,1,0.05))
plt3PTSe <- createPlot(t(aboPTSe[[3]]),pN,lst[-seq(4,12,4),]) + labs(title = "Difference PTSe")
plt4PTSe <- createPlot(t(aboPTSe[[4]]),pN,lst[-seq(4,12,4),]) + labs(title = "False Ratio PTSe")

time <- list()
time <- c(time,list(Sys.time()))
aboDSe <- multiMIPar(MIDSe,pN,10000,10000,100,ml)
time <- c(time,list(Sys.time()))

plt1DSe <- createPlot(t(aboDSe[[1]]),pN,lst) + labs(title = "True Positive DSe") + scale_y_continuous(breaks = seq(0,1,0.05))
plt2DSe <- createPlot(t(aboDSe[[2]]),pN,lst) + labs(title = "False Positive DSe") + scale_y_continuous(breaks = seq(0,1,0.05))
plt3DSe <- createPlot(t(aboDSe[[3]]),pN,lst) + labs(title = "Difference DSe")
plt4DSe <- createPlot(t(aboDSe[[4]]),pN,lst) + labs(title = "False Ratio DSe")

aboDTSe <- multiMIPar(MIDTSe,pN,10000,10000,100,ml)

plt1DTSe <- createPlot(t(aboDTSe[[1]]),pN,lst) + labs(title = "True Positive DTSe") + scale_y_continuous(breaks = seq(0,1,0.05))
plt2DTSe <- createPlot(t(aboDTSe[[2]]),pN,lst) + labs(title = "False Positive DTSe") + scale_y_continuous(breaks = seq(0,1,0.05))
plt3DTSe <- createPlot(t(aboDTSe[[3]]),pN,lst) + labs(title = "Difference DTSe")
plt4DTSe <- createPlot(t(aboDTSe[[4]]),pN,lst) + labs(title = "False Ratio DTSe")

plots <- c(paste0("plt",paste0(rep(c("1","2","3","4"),4),c(rep(c("DSe"),4),rep(c("PSe"),4),rep(c("DTSe"),4),rep(c("PTSe"),4)))))
for(c in plots){
  t.plot <- eval(as.name(c))
  ggsave(paste0("plots/",c,".pdf"),t.plot,device = "pdf")
}


aboPHSe <- multiMIPar(MIPHSe,pN,10000,10000,100,ml)

plt1PHSe <- createPlot(t(aboPHSe[[1]]),pN,lst[-seq(4,12,4),]) + labs(title = "True Positive PHSe") + scale_y_continuous(breaks = seq(0,1,0.05))
plt2PHSe <- createPlot(t(aboPHSe[[2]]),pN,lst[-seq(4,12,4),]) + labs(title = "False Positive PHSe") + scale_y_continuous(breaks = seq(0,1,0.05))
plt3PHSe <- createPlot(t(aboPHSe[[3]]),pN,lst[-seq(4,12,4),]) + labs(title = "Difference PHSe")
plt4PHSe <- createPlot(t(aboPHSe[[4]]),pN,lst[-seq(4,12,4),]) + labs(title = "False Ratio PHSe")

aboPHTSe <- multiMIPar(MIPHTSe,pN,10000,10000,100,ml)

plt1PHTSe <- createPlot(t(aboPHTSe[[1]]),pN,lst[-seq(4,12,4),]) + labs(title = "True Positive PHTSe") + scale_y_continuous(breaks = seq(0,1,0.05))
plt2PHTSe <- createPlot(t(aboPHTSe[[2]]),pN,lst[-seq(4,12,4),]) + labs(title = "False Positive PHTSe") + scale_y_continuous(breaks = seq(0,1,0.05))
plt3PHTSe <- createPlot(t(aboPHTSe[[3]]),pN,lst[-seq(4,12,4),]) + labs(title = "Difference PHTSe")
plt4PHTSe <- createPlot(t(aboPHTSe[[4]]),pN,lst[-seq(4,12,4),]) + labs(title = "False Ratio PHTSe")

aboDHSe <- multiMIPar(MIDHSe,pN,10000,10000,100,ml)

plt1DHSe <- createPlot(t(aboDHSe[[1]]),pN,lst) + labs(title = "True Positive DHSe") + scale_y_continuous(breaks = seq(0,1,0.05))
plt2DHSe <- createPlot(t(aboDHSe[[2]]),pN,lst) + labs(title = "False Positive DHSe") + scale_y_continuous(breaks = seq(0,1,0.05))
plt3DHSe <- createPlot(t(aboDHSe[[3]]),pN,lst) + labs(title = "Difference DHSe")
plt4DHSe <- createPlot(t(aboDHSe[[4]]),pN,lst) + labs(title = "False Ratio DHSe")

aboDHTSe <- multiMIPar(MIDHTSe,pN,10000,10000,100,ml)

plt1DHTSe <- createPlot(t(aboDHTSe[[1]]),pN,lst) + labs(title = "True Positive DHTSe") + scale_y_continuous(breaks = seq(0,1,0.05))
plt2DHTSe <- createPlot(t(aboDHTSe[[2]]),pN,lst) + labs(title = "False Positive DHTSe") + scale_y_continuous(breaks = seq(0,1,0.05))
plt3DHTSe <- createPlot(t(aboDHTSe[[3]]),pN,lst) + labs(title = "Difference DHTSe")
plt4DHTSe <- createPlot(t(aboDHTSe[[4]]),pN,lst) + labs(title = "False Ratio DHTSe")

plots <- c(paste0("plt",paste0(rep(c("1","2","3","4"),4),c(rep(c("DHSe"),4),rep(c("PHSe"),4),rep(c("DHTSe"),4),rep(c("PHTSe"),4)))))
for(c in plots){
  t.plot <- eval(as.name(c))
  ggsave(paste0("plots/",c,".pdf"),t.plot,device = "pdf")
}

aboPLSe <- multiMIPar(MIPLSe,pN,10000,10000,100,ml)

plt1PLSe <- createPlot(t(aboPLSe[[1]]),pN,lst[-seq(4,12,4),]) + labs(title = "True Positive PLSe") + scale_y_continuous(breaks = seq(0,1,0.05))
plt2PLSe <- createPlot(t(aboPLSe[[2]]),pN,lst[-seq(4,12,4),]) + labs(title = "False Positive PLSe") + scale_y_continuous(breaks = seq(0,1,0.05))
plt3PLSe <- createPlot(t(aboPLSe[[3]]),pN,lst[-seq(4,12,4),]) + labs(title = "Difference PLSe")
plt4PLSe <- createPlot(t(aboPLSe[[4]]),pN,lst[-seq(4,12,4),]) + labs(title = "False Ratio PLSe")

aboPLTSe <- multiMIPar(MIPLTSe,pN,10000,10000,100,ml)

plt1PLTSe <- createPlot(t(aboPLTSe[[1]]),pN,lst[-seq(4,12,4),]) + labs(title = "True Positive PLTSe") + scale_y_continuous(breaks = seq(0,1,0.05))
plt2PLTSe <- createPlot(t(aboPLTSe[[2]]),pN,lst[-seq(4,12,4),]) + labs(title = "False Positive PLTSe") + scale_y_continuous(breaks = seq(0,1,0.05))
plt3PLTSe <- createPlot(t(aboPLTSe[[3]]),pN,lst[-seq(4,12,4),]) + labs(title = "Difference PLTSe")
plt4PLTSe <- createPlot(t(aboPLTSe[[4]]),pN,lst[-seq(4,12,4),]) + labs(title = "False Ratio PLTSe")

aboDLSe <- multiMIPar(MIDLSe,pN,10000,10000,100,ml)

plt1DLSe <- createPlot(t(aboDLSe[[1]]),pN,lst) + labs(title = "True Positive DLSe") + scale_y_continuous(breaks = seq(0,1,0.05))
plt2DLSe <- createPlot(t(aboDLSe[[2]]),pN,lst) + labs(title = "False Positive DLSe") + scale_y_continuous(breaks = seq(0,1,0.05))
plt3DLSe <- createPlot(t(aboDLSe[[3]]),pN,lst) + labs(title = "Difference DLSe")
plt4DLSe <- createPlot(t(aboDLSe[[4]]),pN,lst) + labs(title = "False Ratio DLSe")

aboDLTSe <- multiMIPar(MIDLTSe,pN,10000,10000,100,ml)

plt1DLTSe <- createPlot(t(aboDLTSe[[1]]),pN,lst) + labs(title = "True Positive DLTSe") + scale_y_continuous(breaks = seq(0,1,0.05))
plt2DLTSe <- createPlot(t(aboDLTSe[[2]]),pN,lst) + labs(title = "False Positive DLTSe") + scale_y_continuous(breaks = seq(0,1,0.05))
plt3DLTSe <- createPlot(t(aboDLTSe[[3]]),pN,lst) + labs(title = "Difference DLTSe")
plt4DLTSe <- createPlot(t(aboDLTSe[[4]]),pN,lst) + labs(title = "False Ratio DLTSe")

plots <- c(paste0("plt",paste0(rep(c("1","2","3","4"),4),c(rep(c("DLSe"),4),rep(c("PLSe"),4),rep(c("DLTSe"),4),rep(c("PLTSe"),4)))))
for(c in plots){
  t.plot <- eval(as.name(c))
  ggsave(paste0("plots/",c,".pdf"),t.plot,device = "pdf")
}

aboCPSe <- multiMIPar(MICPSe,pN,10000,10000,100,ml)

plt1CPSe <- createPlot(t(aboCPSe[[1]]),pN,lst) + labs(title = "True Positive CPSe") + scale_y_continuous(breaks = seq(0,1,0.05))
plt2CPSe <- createPlot(t(aboCPSe[[2]]),pN,lst) + labs(title = "False Positive CPSe") + scale_y_continuous(breaks = seq(0,1,0.05))
plt3CPSe <- createPlot(t(aboCPSe[[3]]),pN,lst) + labs(title = "Difference CPSe")
plt4CPSe <- createPlot(t(aboCPSe[[4]]),pN,lst) + labs(title = "False Ratio CPSe")

aboCPTSe <- multiMIPar(MICPTSe,pN,10000,10000,100,ml)

plt1CPTSe <- createPlot(t(aboCPTSe[[1]]),pN,lst) + labs(title = "True Positive CPTSe") + scale_y_continuous(breaks = seq(0,1,0.05))
plt2CPTSe <- createPlot(t(aboCPTSe[[2]]),pN,lst) + labs(title = "False Positive CPTSe") + scale_y_continuous(breaks = seq(0,1,0.05))
plt3CPTSe <- createPlot(t(aboCPTSe[[3]]),pN,lst) + labs(title = "Difference CPTSe")
plt4CPTSe <- createPlot(t(aboCPTSe[[4]]),pN,lst) + labs(title = "False Ratio CPTSe")

aboCCSe <- multiMIPar(MICCSe,pN,10000,10000,100,ml)

plt1CCSe <- createPlot(t(aboCCSe[[1]]),pN,lst) + labs(title = "True Positive CCSe") + scale_y_continuous(breaks = seq(0,1,0.05))
plt2CCSe <- createPlot(t(aboCCSe[[2]]),pN,lst) + labs(title = "False Positive CCSe") + scale_y_continuous(breaks = seq(0,1,0.05))
plt3CCSe <- createPlot(t(aboCCSe[[3]]),pN,lst) + labs(title = "Difference CCSe")
plt4CCSe <- createPlot(t(aboCCSe[[4]]),pN,lst) + labs(title = "False Ratio CCSe")

aboCCTSe <- multiMIPar(MICCTSe,pN,10000,10000,100,ml)

plt1CCTSe <- createPlot(t(aboCCTSe[[1]]),pN,lst) + labs(title = "True Positive CCTSe") + scale_y_continuous(breaks = seq(0,1,0.05))
plt2CCTSe <- createPlot(t(aboCCTSe[[2]]),pN,lst) + labs(title = "False Positive CCTSe") + scale_y_continuous(breaks = seq(0,1,0.05))
plt3CCTSe <- createPlot(t(aboCCTSe[[3]]),pN,lst) + labs(title = "Difference CCTSe")
plt4CCTSe <- createPlot(t(aboCCTSe[[4]]),pN,lst) + labs(title = "False Ratio CCTSe")

plots <- c(paste0("plt",paste0(rep(c("1","2","3","4"),4),c(rep(c("CCSe"),4),rep(c("CPSe"),4),rep(c("CCTSe"),4),rep(c("CPTSe"),4)))))
for(c in plots){
  t.plot <- eval(as.name(c))
  ggsave(paste0("plots/",c,".pdf"),t.plot,device = "pdf")
}



## Polish

load("MIP.RData")

aboPP <- multiMIPar(MIPP,pN,2500,2500,100,ml)

plt1PP <- createPlot(t(aboPP[[1]]),pN,lst) + labs(title = "True Positive PP") + scale_y_continuous(breaks = seq(0,1,0.05))
plt2PP <- createPlot(t(aboPP[[2]]),pN,lst) + labs(title = "False Positive PP") + scale_y_continuous(breaks = seq(0,1,0.05))
plt3PP <- createPlot(t(aboPP[[3]]),pN,lst) + labs(title = "Difference PP")
plt4PP <- createPlot(t(aboPP[[4]]),pN,lst) + labs(title = "False Ratio PP")

aboPPT <- multiMIPar(MIPPT,pN,2500,2500,100,ml)

plt1PPT <- createPlot(t(aboPPT[[1]]),pN,lst) + labs(title = "True Positive PPT") + scale_y_continuous(breaks = seq(0,1,0.05))
plt2PPT <- createPlot(t(aboPPT[[2]]),pN,lst) + labs(title = "False Positive PPT") + scale_y_continuous(breaks = seq(0,1,0.05))
plt3PPT <- createPlot(t(aboPPT[[3]]),pN,lst) + labs(title = "Difference PPT")
plt4PPT <- createPlot(t(aboPPT[[4]]),pN,lst) + labs(title = "False Ratio PPT")

aboPD <- multiMIPar(MIPD,pN,2500,2500,100,ml)

plt1PD <- createPlot(t(aboPD[[1]]),pN,lst) + labs(title = "True Positive PD") + scale_y_continuous(breaks = seq(0,1,0.05))
plt2PD <- createPlot(t(aboPD[[2]]),pN,lst) + labs(title = "False Positive PD") + scale_y_continuous(breaks = seq(0,1,0.05))
plt3PD <- createPlot(t(aboPD[[3]]),pN,lst) + labs(title = "Difference PD")
plt4PD <- createPlot(t(aboPD[[4]]),pN,lst) + labs(title = "False Ratio PD")

aboPDT <- multiMIPar(MIPDT,pN,2500,2500,100,ml)

plt1PDT <- createPlot(t(aboPDT[[1]]),pN,lst) + labs(title = "True Positive PDT") + scale_y_continuous(breaks = seq(0,1,0.05))
plt2PDT <- createPlot(t(aboPDT[[2]]),pN,lst) + labs(title = "False Positive PDT") + scale_y_continuous(breaks = seq(0,1,0.05))
plt3PDT <- createPlot(t(aboPDT[[3]]),pN,lst) + labs(title = "Difference PDT")
plt4PDT <- createPlot(t(aboPDT[[4]]),pN,lst) + labs(title = "False Ratio PDT")

plots <- c(paste0("plt",paste0(rep(c("1","2","3","4"),4),c(rep(c("PD"),4),rep(c("PP"),4),rep(c("PDT"),4),rep(c("PPT"),4)))))
for(c in plots){
  t.plot <- eval(as.name(c))
  ggsave(paste0("plots/",c,".pdf"),t.plot,device = "pdf")
}


aboPCP <- multiMIPar(MIPCP,pN,2500,2500,100,ml)

plt1PCP <- createPlot(t(aboPCP[[1]]),pN,lst) + labs(title = "True Positive PCP") + scale_y_continuous(breaks = seq(0,1,0.05))
plt2PCP <- createPlot(t(aboPCP[[2]]),pN,lst) + labs(title = "False Positive PCP") + scale_y_continuous(breaks = seq(0,1,0.05))
plt3PCP <- createPlot(t(aboPCP[[3]]),pN,lst) + labs(title = "Difference PCP")
plt4PCP <- createPlot(t(aboPCP[[4]]),pN,lst) + labs(title = "False Ratio PCP")

aboPCPT <- multiMIPar(MIPCPT,pN,2500,2500,100,ml)

plt1PCPT <- createPlot(t(aboPCPT[[1]]),pN,lst) + labs(title = "True Positive PCPT") + scale_y_continuous(breaks = seq(0,1,0.05))
plt2PCPT <- createPlot(t(aboPCPT[[2]]),pN,lst) + labs(title = "False Positive PCPT") + scale_y_continuous(breaks = seq(0,1,0.05))
plt3PCPT <- createPlot(t(aboPCPT[[3]]),pN,lst) + labs(title = "Difference PCPT")
plt4PCPT <- createPlot(t(aboPCPT[[4]]),pN,lst) + labs(title = "False Ratio PCPT")

aboPCC <- multiMIPar(MIPCC,pN,2500,2500,100,ml)

plt1PCC <- createPlot(t(aboPCC[[1]]),pN,lst) + labs(title = "True Positive PCC") + scale_y_continuous(breaks = seq(0,1,0.05))
plt2PCC <- createPlot(t(aboPCC[[2]]),pN,lst) + labs(title = "False Positive PCC") + scale_y_continuous(breaks = seq(0,1,0.05))
plt3PCC <- createPlot(t(aboPCC[[3]]),pN,lst) + labs(title = "Difference PCC")
plt4PCC <- createPlot(t(aboPCC[[4]]),pN,lst) + labs(title = "False Ratio PCC")

aboPCCT <- multiMIPar(MIPCCT,pN,2500,2500,100,ml)

plt1PCCT <- createPlot(t(aboPCCT[[1]]),pN,lst) + labs(title = "True Positive PCCT") + scale_y_continuous(breaks = seq(0,1,0.05))
plt2PCCT <- createPlot(t(aboPCCT[[2]]),pN,lst) + labs(title = "False Positive PCCT") + scale_y_continuous(breaks = seq(0,1,0.05))
plt3PCCT <- createPlot(t(aboPCCT[[3]]),pN,lst) + labs(title = "Difference PCCT")
plt4PCCT <- createPlot(t(aboPCCT[[4]]),pN,lst) + labs(title = "False Ratio PCCT")

plots <- c(paste0("plt",paste0(rep(c("1","2","3","4"),4),c(rep(c("PCC"),4),rep(c("PCP"),4),rep(c("PCCT"),4),rep(c("PCPT"),4)))))
for(c in plots){
  t.plot <- eval(as.name(c))
  ggsave(paste0("plots/",c,".pdf"),t.plot,device = "pdf")
}


## Avila

load("MIA.RData")

aboAP <- multiMIPar(MIAP,pN,5215,5215,100,ml)

plt1AP <- createPlot(t(aboAP[[1]]),pN,lst) + labs(title = "True Positive AP") + scale_y_continuous(breaks = seq(0,1,0.05))
plt2AP <- createPlot(t(aboAP[[2]]),pN,lst) + labs(title = "False Positive AP") + scale_y_continuous(breaks = seq(0,1,0.05))
plt3AP <- createPlot(t(aboAP[[3]]),pN,lst) + labs(title = "Difference AP")
plt4AP <- createPlot(t(aboAP[[4]]),pN,lst) + labs(title = "False Ratio AP")

aboAPT <- multiMIPar(MIAPT,pN,5215,5215,100,ml)

plt1APT <- createPlot(t(aboAPT[[1]]),pN,lst) + labs(title = "True Positive APT") + scale_y_continuous(breaks = seq(0,1,0.05))
plt2APT <- createPlot(t(aboAPT[[2]]),pN,lst) + labs(title = "False Positive APT") + scale_y_continuous(breaks = seq(0,1,0.05))
plt3APT <- createPlot(t(aboAPT[[3]]),pN,lst) + labs(title = "Difference APT")
plt4APT <- createPlot(t(aboAPT[[4]]),pN,lst) + labs(title = "False Ratio APT")

aboAD <- multiMIPar(MIAD,pN,5215,5215,100,ml)

plt1AD <- createPlot(t(aboAD[[1]]),pN,lst) + labs(title = "True Positive AD") + scale_y_continuous(breaks = seq(0,1,0.05))
plt2AD <- createPlot(t(aboAD[[2]]),pN,lst) + labs(title = "False Positive AD") + scale_y_continuous(breaks = seq(0,1,0.05))
plt3AD <- createPlot(t(aboAD[[3]]),pN,lst) + labs(title = "Difference AD")
plt4AD <- createPlot(t(aboAD[[4]]),pN,lst) + labs(title = "False Ratio AD")

aboADT <- multiMIPar(MIADT,pN,5215,5215,100,ml)

plt1ADT <- createPlot(t(aboADT[[1]]),pN,lst) + labs(title = "True Positive ADT") + scale_y_continuous(breaks = seq(0,1,0.05))
plt2ADT <- createPlot(t(aboADT[[2]]),pN,lst) + labs(title = "False Positive ADT") + scale_y_continuous(breaks = seq(0,1,0.05))
plt3ADT <- createPlot(t(aboADT[[3]]),pN,lst) + labs(title = "Difference ADT")
plt4ADT <- createPlot(t(aboADT[[4]]),pN,lst) + labs(title = "False Ratio ADT")

plots <- c(paste0("plt",paste0(rep(c("1","2","3","4"),4),c(rep(c("AD"),4),rep(c("AP"),4),rep(c("ADT"),4),rep(c("APT"),4)))))
for(c in plots){
  t.plot <- eval(as.name(c))
  ggsave(paste0("plots/",c,".pdf"),t.plot,device = "pdf")
}


aboACP <- multiMIPar(MIACP,pN,5215,5215,100,ml)

plt1ACP <- createPlot(t(aboACP[[1]]),pN,lst) + labs(title = "True Positive ACP") + scale_y_continuous(breaks = seq(0,1,0.05))
plt2ACP <- createPlot(t(aboACP[[2]]),pN,lst) + labs(title = "False Positive ACP") + scale_y_continuous(breaks = seq(0,1,0.05))
plt3ACP <- createPlot(t(aboACP[[3]]),pN,lst) + labs(title = "Difference ACP")
plt4ACP <- createPlot(t(aboACP[[4]]),pN,lst) + labs(title = "False Ratio ACP")

aboACPT <- multiMIPar(MIACPT,pN,5215,5215,100,ml)

plt1ACPT <- createPlot(t(aboACPT[[1]]),pN,lst) + labs(title = "True Positive ACPT") + scale_y_continuous(breaks = seq(0,1,0.05))
plt2ACPT <- createPlot(t(aboACPT[[2]]),pN,lst) + labs(title = "False Positive ACPT") + scale_y_continuous(breaks = seq(0,1,0.05))
plt3ACPT <- createPlot(t(aboACPT[[3]]),pN,lst) + labs(title = "Difference ACPT")
plt4ACPT <- createPlot(t(aboACPT[[4]]),pN,lst) + labs(title = "False Ratio ACPT")

aboACC <- multiMIPar(MIACC,pN,5215,5215,100,ml)

plt1ACC <- createPlot(t(aboACC[[1]]),pN,lst) + labs(title = "True Positive ACC") + scale_y_continuous(breaks = seq(0,1,0.05))
plt2ACC <- createPlot(t(aboACC[[2]]),pN,lst) + labs(title = "False Positive ACC") + scale_y_continuous(breaks = seq(0,1,0.05))
plt3ACC <- createPlot(t(aboACC[[3]]),pN,lst) + labs(title = "Difference ACC")
plt4ACC <- createPlot(t(aboACC[[4]]),pN,lst) + labs(title = "False Ratio ACC")

aboACCT <- multiMIPar(MIACCT,pN,5215,5215,100,ml)

plt1ACCT <- createPlot(t(aboACCT[[1]]),pN,lst) + labs(title = "True Positive ACCT") + scale_y_continuous(breaks = seq(0,1,0.05))
plt2ACCT <- createPlot(t(aboACCT[[2]]),pN,lst) + labs(title = "False Positive ACCT") + scale_y_continuous(breaks = seq(0,1,0.05))
plt3ACCT <- createPlot(t(aboACCT[[3]]),pN,lst) + labs(title = "Difference ACCT")
plt4ACCT <- createPlot(t(aboACCT[[4]]),pN,lst) + labs(title = "False Ratio ACCT")

plots <- c(paste0("plt",paste0(rep(c("1","2","3","4"),4),c(rep(c("ACC"),4),rep(c("ACP"),4),rep(c("ACCT"),4),rep(c("ACPT"),4)))))
for(c in plots){
  t.plot <- eval(as.name(c))
  ggsave(paste0("plots/",c,".pdf"),t.plot,device = "pdf")
}

## Maximum Knowledge

sigma1 <- rep.int(0.1,13)
sigma1[c(1,9,10,11)] <- c(1,1,1,1)
sigma2 <- rep.int(0.1,13)
sigma3 <- rep.int(1,13)
sigmaL <- rep(list(sigma1,sigma2,sigma3),times = 5)

rankbL <- c(sapply(c(0:4),rep,times = 3))

sel1 <- c(1:13)
selLp <- lapply(sapply(c(1:15),function(x) list(x)),function(x) sel1)
selL <- selLp


load("sdsAdultM.RData")

MKP <- multiMK(adultODS10K,"sdsAdultParM",sigmaL,c(1:100),rankbL,selL)
MKD <- multiMK(adultODS10K,"sdsAdultDecM",sigmaL,c(1:100),rankbL,selL)
MKPT <- multiMK(adultODS10K,"sdsAdultParMT",sigmaL,c(1:100),rankbL,selL)
MKDT <- multiMK(adultODS10K,"sdsAdultDecMT",sigmaL,c(1:100),rankbL,selL)

save(list = paste0("MK",c("P","PT","D","DT")),
     file = "MK/MK.RData")

MEP <- findEst("sdsAdultParM","MKP",c(1:100),c(1:13))
MED <- findEst("sdsAdultDecM","MKD",c(1:100),c(1:13))
MEPT <- findEst("sdsAdultParMT","MKPT",c(1:100),c(1:13))
MEDT <- findEst("sdsAdultDecMT","MKDT",c(1:100),c(1:13))

rm(list = c("sdsAdultParM","sdsAdultDecM","sdsAdultParMT","sdsAdultDecMT"))
rm(list = paste0("MK",c("P","PT","D","DT")))

save(list = paste0("ME",c("P","PT","D","DT")),
     file = "MK/ME.RData")

MRP <- getAll("MEP",adultODS10K,c(1:13))
MRD <- getAll("MED",adultODS10K,c(1:13))
MRPT <- getAll("MEPT",adultODS10K,c(1:13))
MRDT <- getAll("MEDT",adultODS10K,c(1:13))

rm(list = paste0("ME",c("P","PT","D","DT")))


load("sdsAdultMH.RData")

MKPH <- multiMK(adultODS10K,"sdsAdultParMH",sigmaL,c(1:100),rankbL,selL)
MKDH <- multiMK(adultODS10K,"sdsAdultDecMH",sigmaL,c(1:100),rankbL,selL)
MKPHT <- multiMK(adultODS10K,"sdsAdultParMHT",sigmaL,c(1:100),rankbL,selL)
MKDHT <- multiMK(adultODS10K,"sdsAdultDecMHT",sigmaL,c(1:100),rankbL,selL)

save(list = paste0("MK",c("PH","PHT","DH","DHT")),
     file = "MK/MKH.RData")

MEPH <- findEst("sdsAdultParMH","MKPH",c(1:100),c(1:13))
MEDH <- findEst("sdsAdultDecMH","MKDH",c(1:100),c(1:13))
MEPHT <- findEst("sdsAdultParMHT","MKPHT",c(1:100),c(1:13))
MEDHT <- findEst("sdsAdultDecMHT","MKDHT",c(1:100),c(1:13))

rm(list = c("sdsAdultParMH","sdsAdultDecMH","sdsAdultParMHT","sdsAdultDecMHT"))
rm(list = paste0("MK",c("PH","PHT","DH","DHT")))

save(list = paste0("ME",c("PH","PHT","DH","DHT")),
     file = "MK/MEH.RData")

MRPH <- getAll("MEPH",adultODS10K,c(1:13))
MRDH <- getAll("MEDH",adultODS10K,c(1:13))
MRPHT <- getAll("MEPHT",adultODS10K,c(1:13))
MRDHT <- getAll("MEDHT",adultODS10K,c(1:13))

rm(list = paste0("ME",c("PH","PHT","DH","DHT")))


load("sdsAdultML.RData")

MKPL <- multiMK(adultODS10K,"sdsAdultParML",sigmaL,c(1:100),rankbL,selL)
MKDL <- multiMK(adultODS10K,"sdsAdultDecML",sigmaL,c(1:100),rankbL,selL)
MKPLT <- multiMK(adultODS10K,"sdsAdultParMLT",sigmaL,c(1:100),rankbL,selL)
MKDLT <- multiMK(adultODS10K,"sdsAdultDecMLT",sigmaL,c(1:100),rankbL,selL)

save(list = paste0("MK",c("PL","PLT","DL","DLT")),
     file = "MK/MKL.RData")

MEPL <- findEst("sdsAdultParML","MKPL",c(1:100),c(1:13))
MEDL <- findEst("sdsAdultDecML","MKDL",c(1:100),c(1:13))
MEPLT <- findEst("sdsAdultParMLT","MKPLT",c(1:100),c(1:13))
MEDLT <- findEst("sdsAdultDecMLT","MKDLT",c(1:100),c(1:13))

rm(list = c("sdsAdultParML","sdsAdultDecML","sdsAdultParMLT","sdsAdultDecMLT"))
rm(list = paste0("MK",c("PL","PLT","DL","DLT")))

save(list = paste0("ME",c("PL","PLT","DL","DLT")),
     file = "MK/MEL.RData")

MRPL <- getAll("MEPL",adultODS10K,c(1:13))
MRDL <- getAll("MEDL",adultODS10K,c(1:13))
MRPLT <- getAll("MEPLT",adultODS10K,c(1:13))
MRDLT <- getAll("MEDLT",adultODS10K,c(1:13))

rm(list = paste0("ME",c("PL","PLT","DL","DLT")))


load("sdsAdultMS.RData")

MKCP <- multiMK(adultODS10K,"sdsAdultCatMP",sigmaL,c(1:100),rankbL,selL)
MKCPT <- multiMK(adultODS10K,"sdsAdultCatMPT",sigmaL,c(1:100),rankbL,selL)
MKCC <- multiMK(adultODS10K,"sdsAdultCatMC",sigmaL,c(1:100),rankbL,selL)
MKCCT <- multiMK(adultODS10K,"sdsAdultCatMCT",sigmaL,c(1:100),rankbL,selL)

save(list = paste0("MK",c("CP","CPT","CC","CCT")),
     file = "MK/MKS.RData")

MECP <- findEst("sdsAdultCatMP","MKCP",c(1:100),c(1:13))
MECPT <- findEst("sdsAdultCatMP","MKCPT",c(1:100),c(1:13))
MECC <- findEst("sdsAdultCatMC","MKCC",c(1:100),c(1:13))
MECCT <- findEst("sdsAdultCatMCT","MKCCT",c(1:100),c(1:13))

rm(list = c("sdsAdultCatMP","sdsAdultCatMPT","sdsAdultCatMC","sdsAdultCatMCT"))
rm(list = paste0("MK",c("CP","CPT","CC","CCT")))

save(list = paste0("ME",c("CP","CPT","CC","CCT")),
     file = "MK/MES.RData")

MRCP <- getAll("MECP",adultODS10K,c(1:13))
MRCPT <- getAll("MECPT",adultODS10K,c(1:13))
MRCC <- getAll("MECC",adultODS10K,c(1:13))
MRCCT <- getAll("MECCT",adultODS10K,c(1:13))

rm(list = paste0("ME",c("CP","CPT","CC","CCT")))

## Smoothing

load("sdsAdultMSm.RData")


MKDS <- multiMK(adultODS10K,"sdsAdultDecMS",sigmaL,c(1:100),rankbL,selL)
MKDTS <- multiMK(adultODS10K,"sdsAdultDecMTS",sigmaL,c(1:100),rankbL,selL)

save(list = paste0("MK",c("DS","DTS")),
     file = "MK/MKSm.RData")

MEDS <- findEst("sdsAdultDecMS","MKDS",c(1:100),c(1:13))
MEDTS <- findEst("sdsAdultDecMTS","MKDTS",c(1:100),c(1:13))

rm(list = c("sdsAdultDecMS","sdsAdultDecMTS"))
rm(list = paste0("MK",c("DS","DTS")))

save(list = paste0("ME",c("DS","DTS")),
     file = "MK/MESm.RData")

MRDS <- getAll("MEDS",adultODS10K,c(1:13))
MRDTS <- getAll("MEDTS",adultODS10K,c(1:13))

rm(list = paste0("ME",c("DS","DTS")))


load("sdsAdultMHSm.RData")

MKDHS <- multiMK(adultODS10K,"sdsAdultDecMHS",sigmaL,c(1:100),rankbL,selL)
MKDHTS <- multiMK(adultODS10K,"sdsAdultDecMHTS",sigmaL,c(1:100),rankbL,selL)

save(list = paste0("MK",c("DH","DHT"),"S"),
     file = "MK/MKHSm.RData")

MEDHS <- findEst("sdsAdultDecMHS","MKDHS",c(1:100),c(1:13))
MEDHTS <- findEst("sdsAdultDecMHTS","MKDHTS",c(1:100),c(1:13))

rm(list = c("sdsAdultDecMHS","sdsAdultDecMHTS"))
rm(list = paste0("MK",c("DH","DHT"),"S"))

save(list = paste0("ME",c("DH","DHT"),"S"),
     file = "MK/MEHSm.RData")

MRDHS <- getAll("MEDHS",adultODS10K,c(1:13))
MRDHTS <- getAll("MEDHTS",adultODS10K,c(1:13))

rm(list = paste0("ME",c("DH","DHT"),"S"))


load("sdsAdultMLSm.RData")

MKDLS <- multiMK(adultODS10K,"sdsAdultDecMLS",sigmaL,c(1:100),rankbL,selL)
MKDLTS <- multiMK(adultODS10K,"sdsAdultDecMLTS",sigmaL,c(1:100),rankbL,selL)

save(list = paste0("MK",c("DL","DLT"),"S"),
     file = "MK/MKLSm.RData")

MEDLS <- findEst("sdsAdultDecMLS","MKDLS",c(1:100),c(1:13))
MEDLTS <- findEst("sdsAdultDecMLTS","MKDLTS",c(1:100),c(1:13))

rm(list = c("sdsAdultDecMLS","sdsAdultDecMLTS"))
rm(list = paste0("MK",c("DLS","DLTS")))

save(list = paste0("ME",c("DL","DLT"),"S"),
     file = "MK/MELSm.RData")

MRDLS <- getAll("MEDLS",adultODS10K,c(1:13))
MRDLTS <- getAll("MEDLTS",adultODS10K,c(1:13))

rm(list = paste0("ME",c("DLS","DLTS")))


load("sdsAdultMSSm.RData")

MKCPS <- multiMK(adultODS10K,"sdsAdultCatMPS",sigmaL,c(1:100),rankbL,selL)
MKCPTS <- multiMK(adultODS10K,"sdsAdultCatMPTS",sigmaL,c(1:100),rankbL,selL)
MKCCS <- multiMK(adultODS10K,"sdsAdultCatMCS",sigmaL,c(1:100),rankbL,selL)
MKCCTS <- multiMK(adultODS10K,"sdsAdultCatMCTS",sigmaL,c(1:100),rankbL,selL)

save(list = paste0("MK",c("CP","CPT","CC","CCT"),"S"),
     file = "MK/MKSSm.RData")

MECPS <- findEst("sdsAdultCatMPS","MKCPS",c(1:100),c(1:13))
MECPTS <- findEst("sdsAdultCatMPS","MKCPTS",c(1:100),c(1:13))
MECCS <- findEst("sdsAdultCatMCS","MKCCS",c(1:100),c(1:13))
MECCTS <- findEst("sdsAdultCatMCTS","MKCCTS",c(1:100),c(1:13))

rm(list = c("sdsAdultCatMPS","sdsAdultCatMPTS","sdsAdultCatMCS","sdsAdultCatMCTS"))
rm(list = paste0("MK",c("CP","CPT","CC","CCT"),"S"))

save(list = paste0("ME",c("CP","CPT","CC","CCT"),"S"),
     file = "MK/MESSm.RData")

MRCPS <- getAll("MECPS",adultODS10K,c(1:13))
MRCPTS <- getAll("MECPTS",adultODS10K,c(1:13))
MRCCS <- getAll("MECCS",adultODS10K,c(1:13))
MRCCTS <- getAll("MECCTS",adultODS10K,c(1:13))

rm(list = paste0("ME",c("CP","CPT","CC","CCT"),"S"))

## Selective

load("sdsAdultMSe.RData")

MKPSe <- multiMK(adultODS10K,"sdsAdultParMSe",sigmaL,c(1:100),rankbL,selL)
MKDSe <- multiMK(adultODS10K,"sdsAdultDecMSe",sigmaL,c(1:100),rankbL,selL)
MKPTSe <- multiMK(adultODS10K,"sdsAdultParMTSe",sigmaL,c(1:100),rankbL,selL)
MKDTSe <- multiMK(adultODS10K,"sdsAdultDecMTSe",sigmaL,c(1:100),rankbL,selL)

save(list = paste0("MK",c("P","PT","D","DT"),"Se"),
     file = "MK/MKSe.RData")

MEPSe <- findEst("sdsAdultParMSe","MKPSe",c(1:100),c(1:13))
MEDSe <- findEst("sdsAdultDecMSe","MKDSe",c(1:100),c(1:13))
MEPTSe <- findEst("sdsAdultParMTSe","MKPTSe",c(1:100),c(1:13))
MEDTSe <- findEst("sdsAdultDecMTSe","MKDTSe",c(1:100),c(1:13))

rm(list = c("sdsAdultParMSe","sdsAdultDecMSe","sdsAdultParMTSe","sdsAdultDecMTSe"))
rm(list = paste0("MK",c("P","PT","D","DT"),"Se"))

save(list = paste0("ME",c("P","PT","D","DT"),"Se"),
     file = "MK/MESe.RData")

MRPSe <- getAll("MEPSe",adultODS10K,c(1:13))
MRDSe <- getAll("MEDSe",adultODS10K,c(1:13))
MRPTSe <- getAll("MEPTSe",adultODS10K,c(1:13))
MRDTSe <- getAll("MEDTSe",adultODS10K,c(1:13))

rm(list = paste0("ME",c("P","PT","D","DT"),"Se"))


load("sdsAdultMHSe.RData")

MKPHSe <- multiMK(adultODS10K,"sdsAdultParMHSe",sigmaL,c(1:100),rankbL,selL)
MKDHSe <- multiMK(adultODS10K,"sdsAdultDecMHSe",sigmaL,c(1:100),rankbL,selL)
MKPHTSe <- multiMK(adultODS10K,"sdsAdultParMHTSe",sigmaL,c(1:100),rankbL,selL)
MKDHTSe <- multiMK(adultODS10K,"sdsAdultDecMHTSe",sigmaL,c(1:100),rankbL,selL)

save(list = paste0("MK",c("PH","PHT","DH","DHT"),"Se"),
     file = "MK/MKHSe.RData")

MEPHSe <- findEst("sdsAdultParMHSe","MKPHSe",c(1:100),c(1:13))
MEDHSe <- findEst("sdsAdultDecMHSe","MKDHSe",c(1:100),c(1:13))
MEPHTSe <- findEst("sdsAdultParMHTSe","MKPHTSe",c(1:100),c(1:13))
MEDHTSe <- findEst("sdsAdultDecMHTSe","MKDHTSe",c(1:100),c(1:13))

rm(list = c("sdsAdultParMHSe","sdsAdultDecMHSe","sdsAdultParMHTSe","sdsAdultDecMHTSe"))
rm(list = paste0("MK",c("PH","PHT","DH","DHT"),"Se"))

save(list = paste0("ME",c("PH","PHT","DH","DHT"),"Se"),
     file = "MK/MEHSe.RData")

MRPHSe <- getAll("MEPHSe",adultODS10K,c(1:13))
MRDHSe <- getAll("MEDHSe",adultODS10K,c(1:13))
MRPHTSe <- getAll("MEPHTSe",adultODS10K,c(1:13))
MRDHTSe <- getAll("MEDHTSe",adultODS10K,c(1:13))

rm(list = paste0("ME",c("PH","PHT","DH","DHT"),"Se"))


load("sdsAdultMLSe.RData")

MKPLSe <- multiMK(adultODS10K,"sdsAdultParMLSe",sigmaL,c(1:100),rankbL,selL)
MKDLSe <- multiMK(adultODS10K,"sdsAdultDecMLSe",sigmaL,c(1:100),rankbL,selL)
MKPLTSe <- multiMK(adultODS10K,"sdsAdultParMLTSe",sigmaL,c(1:100),rankbL,selL)
MKDLTSe <- multiMK(adultODS10K,"sdsAdultDecMLTSe",sigmaL,c(1:100),rankbL,selL)

save(list = paste0("MK",c("PL","PLT","DL","DLT"),"Se"),
     file = "MK/MKLSe.RData")

MEPLSe <- findEst("sdsAdultParMLSe","MKPLSe",c(1:100),c(1:13))
MEDLSe <- findEst("sdsAdultDecMLSe","MKDLSe",c(1:100),c(1:13))
MEPLTSe <- findEst("sdsAdultParMLTSe","MKPLTSe",c(1:100),c(1:13))
MEDLTSe <- findEst("sdsAdultDecMLTSe","MKDLTSe",c(1:100),c(1:13))

rm(list = c("sdsAdultParMLSe","sdsAdultDecMLSe","sdsAdultParMLTSe","sdsAdultDecMLTSe"))
rm(list = paste0("MK",c("PL","PLT","DL","DLT"),"Se"))

save(list = paste0("ME",c("PL","PLT","DL","DLT"),"Se"),
     file = "MK/MELSe.RData")

MRPLSe <- getAll("MEPLSe",adultODS10K,c(1:13))
MRDLSe <- getAll("MEDLSe",adultODS10K,c(1:13))
MRPLTSe <- getAll("MEPLTSe",adultODS10K,c(1:13))
MRDLTSe <- getAll("MEDLTSe",adultODS10K,c(1:13))

rm(list = paste0("ME",c("PL","PLT","DL","DLT"),"Se"))


load("sdsAdultMSSe.RData")

MKCPSe <- multiMK(adultODS10K,"sdsAdultCatMPSe",sigmaL,c(1:100),rankbL,selL)
MKCPTSe <- multiMK(adultODS10K,"sdsAdultCatMPTSe",sigmaL,c(1:100),rankbL,selL)
MKCCSe <- multiMK(adultODS10K,"sdsAdultCatMCSe",sigmaL,c(1:100),rankbL,selL)
MKCCTSe <- multiMK(adultODS10K,"sdsAdultCatMCTSe",sigmaL,c(1:100),rankbL,selL)

save(list = paste0("MK",c("CP","CPT","CC","CCT"),"Se"),
     file = "MK/MKSSe.RData")

MECPSe <- findEst("sdsAdultCatMPSe","MKCPSe",c(1:100),c(1:13))
MECPTSe <- findEst("sdsAdultCatMPSe","MKCPTSe",c(1:100),c(1:13))
MECCSe <- findEst("sdsAdultCatMCSe","MKCCSe",c(1:100),c(1:13))
MECCTSe <- findEst("sdsAdultCatMCTSe","MKCCTSe",c(1:100),c(1:13))

rm(list = c("sdsAdultCatMPSe","sdsAdultCatMPTSe","sdsAdultCatMCSe","sdsAdultCatMCTSe"))
rm(list = paste0("MK",c("CP","CPT","CC","CCT"),"Se"))

save(list = paste0("ME",c("CP","CPT","CC","CCT"),"Se"),
     file = "MK/MESSe.RData")

MRCPSe <- getAll("MECPSe",adultODS10K,c(1:13))
MRCPTSe <- getAll("MECPTSe",adultODS10K,c(1:13))
MRCCSe <- getAll("MECCSe",adultODS10K,c(1:13))
MRCCTSe <- getAll("MECCTSe",adultODS10K,c(1:13))

rm(list = paste0("ME",c("CP","CPT","CC","CCT"),"Se"))

MRL <- c(pasteC(list("MR",c("P","D"),c("","H","L"),c("","T"),c("","Se"))),
         pasteC(list("MR","D",c("","H","L"),c("","T"),"S")),
         pasteC(list("MRC",c("P","C"),c("","T"),c("","S","Se"))))


## Polish and Avila

sigma1 <- rep.int(0.1,14)
sigma1[c(2,5,8,13,14)] <- c(1,1,1,1,1)
sigma2 <- rep.int(0.1,14)
sigma3 <- rep.int(1,14)
sigmaL <- rep(list(sigma1,sigma2,sigma3),times = 5)

rankbL <- c(sapply(c(0:4),rep,times = 3))

sel1 <- c(1:14)
selLp <- lapply(sapply(c(1:15),function(x) list(x)),function(x) sel1)
selL <- selLp


load("sdsPolishMH.RData")

MKPP <- multiMK(ods[c(1:2500),],"sdsPolishParMH",sigmaL,c(1:100),rankbL,selL)
MKPPT <- multiMK(ods[c(1:2500),],"sdsPolishParMTH",sigmaL,c(1:100),rankbL,selL)
MKPD <- multiMK(ods[c(1:2500),],"sdsPolishDecMH",sigmaL,c(1:100),rankbL,selL)
MKPDT <- multiMK(ods[c(1:2500),],"sdsPolishDecMTH",sigmaL,c(1:100),rankbL,selL)

save(list = paste0("MKP",c("P","PT","D","DT")),
     file = "MK/MKP.RData")

MEPP <- findEst("sdsPolishParMH","MKPP",c(1:100),c(1:14))
MEPPT <- findEst("sdsPolishParMTH","MKPPT",c(1:100),c(1:14))
MEPD <- findEst("sdsPolishDecMH","MKPD",c(1:100),c(1:14))
MEPDT <- findEst("sdsPolishDecMTH","MKPDT",c(1:100),c(1:14))

rm(list = c("sdsPolishParMH","sdsPolishParMTH","sdsPolishDecMH","sdsPolishDecMTH"))
rm(list = paste0("MKP",c("P","PT","D","DT")))

save(list = paste0("MEP",c("P","PT","D","DT")),
     file = "MK/MEP.RData")

MRPP <- getAll("MEPP",ods[c(1:2500),],c(1:14))
MRPPT <- getAll("MEPPT",ods[c(1:2500),],c(1:14))
MRPD <- getAll("MEPD",ods[c(1:2500),],c(1:14))
MRPDT <- getAll("MEPDT",ods[c(1:2500),],c(1:14))

rm(list = paste0("MEP",c("P","PT","D","DT")))


load("sdsPolishMSH.RData")

MKPCP <- multiMK(ods[c(1:2500),],"sdsPolishCatMPH",sigmaL,c(1:100),rankbL,selL)
MKPCPT <- multiMK(ods[c(1:2500),],"sdsPolishCatMPTH",sigmaL,c(1:100),rankbL,selL)
MKPCC <- multiMK(ods[c(1:2500),],"sdsPolishCatMCH",sigmaL,c(1:100),rankbL,selL)
MKPCCT <- multiMK(ods[c(1:2500),],"sdsPolishCatMCTH",sigmaL,c(1:100),rankbL,selL)

save(list = paste0("MKP",c("CP","CPT","CC","CCT")),
     file = "MK/MKSP.RData")

MEPCP <- findEst("sdsPolishCatMPH","MKPCP",c(1:100),c(1:14))
MEPCPT <- findEst("sdsPolishCatMPTH","MKPCPT",c(1:100),c(1:14))
MEPCC <- findEst("sdsPolishCatMCH","MKPCC",c(1:100),c(1:14))
MEPCCT <- findEst("sdsPolishCatMCTH","MKPCCT",c(1:100),c(1:14))

rm(list = c("sdsPolishCatMPH","sdsPolishCatMPTH","sdsPolishCatMCH","sdsPolishCatMCTH"))
rm(list = paste0("MKP",c("CP","CPT","CC","CCT")))

save(list = paste0("MEP",c("CP","CPT","CC","CCT")),
     file = "MK/MESP.RData")

MRPCP <- getAll("MEPCP",ods[c(1:2500),],c(1:14))
MRPCPT <- getAll("MEPCPT",ods[c(1:2500),],c(1:14))
MRPCC <- getAll("MEPCC",ods[c(1:2500),],c(1:14))
MRPCCT <- getAll("MEPCCT",ods[c(1:2500),],c(1:14))

rm(list = paste0("MEP",c("CP","CPT","CC","CCT")))



sigma1 <- rep.int(0.01,11)
sigma2 <- rep.int(0.001,11)
sigma3 <- rep.int(0.1,11)
sigmaL <- rep(list(sigma1,sigma2,sigma3),times = 5)

rankbL <- c(sapply(c(0:4),rep,times = 3))

sel1 <- c(1:11)
selLp <- lapply(sapply(c(1:15),function(x) list(x)),function(x) sel1)
selL <- selLp

load("sdsAvilaMH.RData")

MKAP <- multiMK(avilaODS[c(1:5215),],"sdsAvilaParMH",sigmaL,c(1:100),rankbL,selL)
MKAPT <- multiMK(avilaODS[c(1:5215),],"sdsAvilaParMTH",sigmaL,c(1:100),rankbL,selL)
MKAD <- multiMK(avilaODS[c(1:5215),],"sdsAvilaDecMH",sigmaL,c(1:100),rankbL,selL)
MKADT <- multiMK(avilaODS[c(1:5215),],"sdsAvilaDecMTH",sigmaL,c(1:100),rankbL,selL)

save(list = paste0("MKA",c("P","PT","D","DT")),
     file = "MK/MKA.RData")

MEAP <- findEst("sdsAvilaParMH","MKAP",c(1:100),c(1:11))
MEAPT <- findEst("sdsAvilaParMTH","MKAPT",c(1:100),c(1:11))
MEAD <- findEst("sdsAvilaDecMH","MKAD",c(1:100),c(1:11))
MEADT <- findEst("sdsAvilaDecMTH","MKADT",c(1:100),c(1:11))

rm(list = c("sdsAvilaParMH","sdsAvilaParMTH","sdsAvilaDecMH","sdsAvilaDecMTH"))
rm(list = paste0("MKA",c("P","PT","D","DT")))

save(list = paste0("MEA",c("P","PT","D","DT")),
     file = "MK/MEA.RData")

MRAP <- getAll("MEAP",avilaODS[c(1:5215),],c(1:11))
MRAPT <- getAll("MEAPT",avilaODS[c(1:5215),],c(1:11))
MRAD <- getAll("MEAD",avilaODS[c(1:5215),],c(1:11))
MRADT <- getAll("MEADT",avilaODS[c(1:5215),],c(1:11))

rm(list = paste0("MEA",c("P","PT","D","DT")))



load("sdsAvilaMSH.RData")

MKACP <- multiMK(avilaODS[c(1:5215),],"sdsAvilaCatMPH",sigmaL,c(1:100),rankbL,selL)
MKACPT <- multiMK(avilaODS[c(1:5215),],"sdsAvilaCatMPTH",sigmaL,c(1:100),rankbL,selL)
MKACC <- multiMK(avilaODS[c(1:5215),],"sdsAvilaCatMCH",sigmaL,c(1:100),rankbL,selL)
MKACCT <- multiMK(avilaODS[c(1:5215),],"sdsAvilaCatMCTH",sigmaL,c(1:100),rankbL,selL)

save(list = paste0("MKA",c("CP","CPT","CC","CCT")),
     file = "MK/MKSA.RData")

MEACP <- findEst("sdsAvilaCatMPH","MKACP",c(1:100),c(1:11))
MEACPT <- findEst("sdsAvilaCatMPTH","MKACPT",c(1:100),c(1:11))
MEACC <- findEst("sdsAvilaCatMCH","MKACC",c(1:100),c(1:11))
MEACCT <- findEst("sdsAvilaCatMCTH","MKACCT",c(1:100),c(1:11))

rm(list = c("sdsAvilaCatMPH","sdsAvilaCatMPTH","sdsAvilaCatMCH","sdsAvilaCatMCTH"))
rm(list = paste0("MKA",c("CP","CPT","CC","CCT")))

save(list = paste0("MEA",c("CP","CPT","CC","CCT")),
     file = "MK/MESA.RData")

MRACP <- getAll("MEACP",avilaODS[c(1:5215),],c(1:11))
MRACPT <- getAll("MEACPT",avilaODS[c(1:5215),],c(1:11))
MRACC <- getAll("MEACC",avilaODS[c(1:5215),],c(1:11))
MRACCT <- getAll("MEACCT",avilaODS[c(1:5215),],c(1:11))

rm(list = paste0("MEA",c("CP","CPT","CC","CCT")))

MRL <- c(pasteC(list("MR",c("P","A"),c("P","D","CP","CC"),c("","T"))))


## Maximum Knowledge 2

sigma1 <- rep.int(0.1,13)
sigma1[c(1,9,10,11)] <- c(1,1,1,1)
sigma3 <- rep.int(1,13)
sigmaL <- c(rep(list(sigma3),times = 13),rep(list(sigma1),times = 13))

rankbL <- rep.int(4,26)

sel1 <- c(1:13)
selLp <- lapply(sapply(sel1,function(x) list(x)),function(x) sel1[-x])
selL <- c(selLp,selLp)

load("sdsAdultM.RData")

MKP <- multiMK(adultODS10K,"sdsAdultParM",sigmaL,c(1:100),rankbL,selL)
MKD <- multiMK(adultODS10K,"sdsAdultDecM",sigmaL,c(1:100),rankbL,selL)
MKPT <- multiMK(adultODS10K,"sdsAdultParMT",sigmaL,c(1:100),rankbL,selL)
MKDT <- multiMK(adultODS10K,"sdsAdultDecMT",sigmaL,c(1:100),rankbL,selL)

save(list = paste0("MK",c("P","PT","D","DT")),
     file = "MK/MK2.RData")

MEP <- findEst("sdsAdultParM","MKP",c(1:100),c(1:13))
MED <- findEst("sdsAdultDecM","MKD",c(1:100),c(1:13))
MEPT <- findEst("sdsAdultParMT","MKPT",c(1:100),c(1:13))
MEDT <- findEst("sdsAdultDecMT","MKDT",c(1:100),c(1:13))

rm(list = c("sdsAdultParM","sdsAdultDecM","sdsAdultParMT","sdsAdultDecMT"))
rm(list = paste0("MK",c("P","PT","D","DT")))

save(list = paste0("ME",c("P","PT","D","DT")),
     file = "MK/ME2.RData")

MRP <- getAll("MEP",adultODS10K,c(1:13))
MRD <- getAll("MED",adultODS10K,c(1:13))
MRPT <- getAll("MEPT",adultODS10K,c(1:13))
MRDT <- getAll("MEDT",adultODS10K,c(1:13))

rm(list = paste0("ME",c("P","PT","D","DT")))


load("sdsAdultMH.RData")

MKPH <- multiMK(adultODS10K,"sdsAdultParMH",sigmaL,c(1:100),rankbL,selL)
MKDH <- multiMK(adultODS10K,"sdsAdultDecMH",sigmaL,c(1:100),rankbL,selL)
MKPHT <- multiMK(adultODS10K,"sdsAdultParMHT",sigmaL,c(1:100),rankbL,selL)
MKDHT <- multiMK(adultODS10K,"sdsAdultDecMHT",sigmaL,c(1:100),rankbL,selL)

save(list = paste0("MK",c("PH","PHT","DH","DHT")),
     file = "MK/MKH2.RData")

MEPH <- findEst("sdsAdultParMH","MKPH",c(1:100),c(1:13))
MEDH <- findEst("sdsAdultDecMH","MKDH",c(1:100),c(1:13))
MEPHT <- findEst("sdsAdultParMHT","MKPHT",c(1:100),c(1:13))
MEDHT <- findEst("sdsAdultDecMHT","MKDHT",c(1:100),c(1:13))

rm(list = c("sdsAdultParMH","sdsAdultDecMH","sdsAdultParMHT","sdsAdultDecMHT"))
rm(list = paste0("MK",c("PH","PHT","DH","DHT")))

save(list = paste0("ME",c("PH","PHT","DH","DHT")),
     file = "MK/MEH2.RData")

MRPH <- getAll("MEPH",adultODS10K,c(1:13))
MRDH <- getAll("MEDH",adultODS10K,c(1:13))
MRPHT <- getAll("MEPHT",adultODS10K,c(1:13))
MRDHT <- getAll("MEDHT",adultODS10K,c(1:13))

rm(list = paste0("ME",c("PH","PHT","DH","DHT")))


load("sdsAdultML.RData")

MKPL <- multiMK(adultODS10K,"sdsAdultParML",sigmaL,c(1:100),rankbL,selL)
MKDL <- multiMK(adultODS10K,"sdsAdultDecML",sigmaL,c(1:100),rankbL,selL)
MKPLT <- multiMK(adultODS10K,"sdsAdultParMLT",sigmaL,c(1:100),rankbL,selL)
MKDLT <- multiMK(adultODS10K,"sdsAdultDecMLT",sigmaL,c(1:100),rankbL,selL)

save(list = paste0("MK",c("PL","PLT","DL","DLT")),
     file = "MK/MKL2.RData")

MEPL <- findEst("sdsAdultParML","MKPL",c(1:100),c(1:13))
MEDL <- findEst("sdsAdultDecML","MKDL",c(1:100),c(1:13))
MEPLT <- findEst("sdsAdultParMLT","MKPLT",c(1:100),c(1:13))
MEDLT <- findEst("sdsAdultDecMLT","MKDLT",c(1:100),c(1:13))

rm(list = c("sdsAdultParML","sdsAdultDecML","sdsAdultParMLT","sdsAdultDecMLT"))
rm(list = paste0("MK",c("PL","PLT","DL","DLT")))

save(list = paste0("ME",c("PL","PLT","DL","DLT")),
     file = "MK/MEL2.RData")

MRPL <- getAll("MEPL",adultODS10K,c(1:13))
MRDL <- getAll("MEDL",adultODS10K,c(1:13))
MRPLT <- getAll("MEPLT",adultODS10K,c(1:13))
MRDLT <- getAll("MEDLT",adultODS10K,c(1:13))

rm(list = paste0("ME",c("PL","PLT","DL","DLT")))


load("sdsAdultMS.RData")

MKCP <- multiMK(adultODS10K,"sdsAdultCatMP",sigmaL,c(1:100),rankbL,selL)
MKCPT <- multiMK(adultODS10K,"sdsAdultCatMPT",sigmaL,c(1:100),rankbL,selL)
MKCC <- multiMK(adultODS10K,"sdsAdultCatMC",sigmaL,c(1:100),rankbL,selL)
MKCCT <- multiMK(adultODS10K,"sdsAdultCatMCT",sigmaL,c(1:100),rankbL,selL)

save(list = paste0("MK",c("CP","CPT","CC","CCT")),
     file = "MK/MKS2.RData")

MECP <- findEst("sdsAdultCatMP","MKCP",c(1:100),c(1:13))
MECPT <- findEst("sdsAdultCatMP","MKCPT",c(1:100),c(1:13))
MECC <- findEst("sdsAdultCatMC","MKCC",c(1:100),c(1:13))
MECCT <- findEst("sdsAdultCatMCT","MKCCT",c(1:100),c(1:13))

rm(list = c("sdsAdultCatMP","sdsAdultCatMPT","sdsAdultCatMC","sdsAdultCatMCT"))
rm(list = paste0("MK",c("CP","CPT","CC","CCT")))

save(list = paste0("ME",c("CP","CPT","CC","CCT")),
     file = "MK/MES2.RData")

MRCP <- getAll("MECP",adultODS10K,c(1:13))
MRCPT <- getAll("MECPT",adultODS10K,c(1:13))
MRCC <- getAll("MECC",adultODS10K,c(1:13))
MRCCT <- getAll("MECCT",adultODS10K,c(1:13))

rm(list = paste0("ME",c("CP","CPT","CC","CCT")))

## Smoothing

load("sdsAdultMSm.RData")


MKDS <- multiMK(adultODS10K,"sdsAdultDecMS",sigmaL,c(1:100),rankbL,selL)
MKDTS <- multiMK(adultODS10K,"sdsAdultDecMTS",sigmaL,c(1:100),rankbL,selL)

save(list = paste0("MK",c("DS","DTS")),
     file = "MK/MKSm2.RData")

MEDS <- findEst("sdsAdultDecMS","MKDS",c(1:100),c(1:13))
MEDTS <- findEst("sdsAdultDecMTS","MKDTS",c(1:100),c(1:13))

rm(list = c("sdsAdultDecMS","sdsAdultDecMTS"))
rm(list = paste0("MK",c("DS","DTS")))

save(list = paste0("ME",c("DS","DTS")),
     file = "MK/MESm2.RData")

MRDS <- getAll("MEDS",adultODS10K,c(1:13))
MRDTS <- getAll("MEDTS",adultODS10K,c(1:13))

rm(list = paste0("ME",c("DS","DTS")))


load("sdsAdultMHSm.RData")

MKDHS <- multiMK(adultODS10K,"sdsAdultDecMHS",sigmaL,c(1:100),rankbL,selL)
MKDHTS <- multiMK(adultODS10K,"sdsAdultDecMHTS",sigmaL,c(1:100),rankbL,selL)

save(list = paste0("MK",c("DH","DHT"),"S"),
     file = "MK/MKHSm2.RData")

MEDHS <- findEst("sdsAdultDecMHS","MKDHS",c(1:100),c(1:13))
MEDHTS <- findEst("sdsAdultDecMHTS","MKDHTS",c(1:100),c(1:13))

rm(list = c("sdsAdultDecMHS","sdsAdultDecMHTS"))
rm(list = paste0("MK",c("DH","DHT"),"S"))

save(list = paste0("ME",c("DH","DHT"),"S"),
     file = "MK/MEHSm2.RData")

MRDHS <- getAll("MEDHS",adultODS10K,c(1:13))
MRDHTS <- getAll("MEDHTS",adultODS10K,c(1:13))

rm(list = paste0("ME",c("DH","DHT"),"S"))


load("sdsAdultMLSm.RData")

MKDLS <- multiMK(adultODS10K,"sdsAdultDecMLS",sigmaL,c(1:100),rankbL,selL)
MKDLTS <- multiMK(adultODS10K,"sdsAdultDecMLTS",sigmaL,c(1:100),rankbL,selL)

save(list = paste0("MK",c("DL","DLT"),"S"),
     file = "MK/MKLSm2.RData")

MEDLS <- findEst("sdsAdultDecMLS","MKDLS",c(1:100),c(1:13))
MEDLTS <- findEst("sdsAdultDecMLTS","MKDLTS",c(1:100),c(1:13))

rm(list = c("sdsAdultDecMLS","sdsAdultDecMLTS"))
rm(list = paste0("MK",c("DLS","DLTS")))

save(list = paste0("ME",c("DL","DLT"),"S"),
     file = "MK/MELSm2.RData")

MRDLS <- getAll("MEDLS",adultODS10K,c(1:13))
MRDLTS <- getAll("MEDLTS",adultODS10K,c(1:13))

rm(list = paste0("ME",c("DLS","DLTS")))


load("sdsAdultMSSm.RData")

MKCPS <- multiMK(adultODS10K,"sdsAdultCatMPS",sigmaL,c(1:100),rankbL,selL)
MKCPTS <- multiMK(adultODS10K,"sdsAdultCatMPTS",sigmaL,c(1:100),rankbL,selL)
MKCCS <- multiMK(adultODS10K,"sdsAdultCatMCS",sigmaL,c(1:100),rankbL,selL)
MKCCTS <- multiMK(adultODS10K,"sdsAdultCatMCTS",sigmaL,c(1:100),rankbL,selL)

save(list = paste0("MK",c("CP","CPT","CC","CCT"),"S"),
     file = "MK/MKSSm2.RData")

MECPS <- findEst("sdsAdultCatMPS","MKCPS",c(1:100),c(1:13))
MECPTS <- findEst("sdsAdultCatMPS","MKCPTS",c(1:100),c(1:13))
MECCS <- findEst("sdsAdultCatMCS","MKCCS",c(1:100),c(1:13))
MECCTS <- findEst("sdsAdultCatMCTS","MKCCTS",c(1:100),c(1:13))

rm(list = c("sdsAdultCatMPS","sdsAdultCatMPTS","sdsAdultCatMCS","sdsAdultCatMCTS"))
rm(list = paste0("MK",c("CP","CPT","CC","CCT"),"S"))

save(list = paste0("ME",c("CP","CPT","CC","CCT"),"S"),
     file = "MK/MESSm2.RData")

MRCPS <- getAll("MECPS",adultODS10K,c(1:13))
MRCPTS <- getAll("MECPTS",adultODS10K,c(1:13))
MRCCS <- getAll("MECCS",adultODS10K,c(1:13))
MRCCTS <- getAll("MECCTS",adultODS10K,c(1:13))

rm(list = paste0("ME",c("CP","CPT","CC","CCT"),"S"))

## Selective

load("sdsAdultMSe.RData")

MKPSe <- multiMK(adultODS10K,"sdsAdultParMSe",sigmaL,c(1:100),rankbL,selL)
MKDSe <- multiMK(adultODS10K,"sdsAdultDecMSe",sigmaL,c(1:100),rankbL,selL)
MKPTSe <- multiMK(adultODS10K,"sdsAdultParMTSe",sigmaL,c(1:100),rankbL,selL)
MKDTSe <- multiMK(adultODS10K,"sdsAdultDecMTSe",sigmaL,c(1:100),rankbL,selL)

save(list = paste0("MK",c("P","PT","D","DT"),"Se"),
     file = "MK/MKSe2.RData")

MEPSe <- findEst("sdsAdultParMSe","MKPSe",c(1:100),c(1:13))
MEDSe <- findEst("sdsAdultDecMSe","MKDSe",c(1:100),c(1:13))
MEPTSe <- findEst("sdsAdultParMTSe","MKPTSe",c(1:100),c(1:13))
MEDTSe <- findEst("sdsAdultDecMTSe","MKDTSe",c(1:100),c(1:13))

rm(list = c("sdsAdultParMSe","sdsAdultDecMSe","sdsAdultParMTSe","sdsAdultDecMTSe"))
rm(list = paste0("MK",c("P","PT","D","DT"),"Se"))

save(list = paste0("ME",c("P","PT","D","DT"),"Se"),
     file = "MK/MESe2.RData")

MRPSe <- getAll("MEPSe",adultODS10K,c(1:13))
MRDSe <- getAll("MEDSe",adultODS10K,c(1:13))
MRPTSe <- getAll("MEPTSe",adultODS10K,c(1:13))
MRDTSe <- getAll("MEDTSe",adultODS10K,c(1:13))

rm(list = paste0("ME",c("P","PT","D","DT"),"Se"))


load("sdsAdultMHSe.RData")

MKPHSe <- multiMK(adultODS10K,"sdsAdultParMHSe",sigmaL,c(1:100),rankbL,selL)
MKDHSe <- multiMK(adultODS10K,"sdsAdultDecMHSe",sigmaL,c(1:100),rankbL,selL)
MKPHTSe <- multiMK(adultODS10K,"sdsAdultParMHTSe",sigmaL,c(1:100),rankbL,selL)
MKDHTSe <- multiMK(adultODS10K,"sdsAdultDecMHTSe",sigmaL,c(1:100),rankbL,selL)

save(list = paste0("MK",c("PH","PHT","DH","DHT"),"Se"),
     file = "MK/MKHSe2.RData")

MEPHSe <- findEst("sdsAdultParMHSe","MKPHSe",c(1:100),c(1:13))
MEDHSe <- findEst("sdsAdultDecMHSe","MKDHSe",c(1:100),c(1:13))
MEPHTSe <- findEst("sdsAdultParMHTSe","MKPHTSe",c(1:100),c(1:13))
MEDHTSe <- findEst("sdsAdultDecMHTSe","MKDHTSe",c(1:100),c(1:13))

rm(list = c("sdsAdultParMHSe","sdsAdultDecMHSe","sdsAdultParMHTSe","sdsAdultDecMHTSe"))
rm(list = paste0("MK",c("PH","PHT","DH","DHT"),"Se"))

save(list = paste0("ME",c("PH","PHT","DH","DHT"),"Se"),
     file = "MK/MEHSe2.RData")

MRPHSe <- getAll("MEPHSe",adultODS10K,c(1:13))
MRDHSe <- getAll("MEDHSe",adultODS10K,c(1:13))
MRPHTSe <- getAll("MEPHTSe",adultODS10K,c(1:13))
MRDHTSe <- getAll("MEDHTSe",adultODS10K,c(1:13))

rm(list = paste0("ME",c("PH","PHT","DH","DHT"),"Se"))


load("sdsAdultMLSe.RData")

MKPLSe <- multiMK(adultODS10K,"sdsAdultParMLSe",sigmaL,c(1:100),rankbL,selL)
MKDLSe <- multiMK(adultODS10K,"sdsAdultDecMLSe",sigmaL,c(1:100),rankbL,selL)
MKPLTSe <- multiMK(adultODS10K,"sdsAdultParMLTSe",sigmaL,c(1:100),rankbL,selL)
MKDLTSe <- multiMK(adultODS10K,"sdsAdultDecMLTSe",sigmaL,c(1:100),rankbL,selL)

save(list = paste0("MK",c("PL","PLT","DL","DLT"),"Se"),
     file = "MK/MKLSe2.RData")

MEPLSe <- findEst("sdsAdultParMLSe","MKPLSe",c(1:100),c(1:13))
MEDLSe <- findEst("sdsAdultDecMLSe","MKDLSe",c(1:100),c(1:13))
MEPLTSe <- findEst("sdsAdultParMLTSe","MKPLTSe",c(1:100),c(1:13))
MEDLTSe <- findEst("sdsAdultDecMLTSe","MKDLTSe",c(1:100),c(1:13))

rm(list = c("sdsAdultParMLSe","sdsAdultDecMLSe","sdsAdultParMLTSe","sdsAdultDecMLTSe"))
rm(list = paste0("MK",c("PL","PLT","DL","DLT"),"Se"))

save(list = paste0("ME",c("PL","PLT","DL","DLT"),"Se"),
     file = "MK/MELSe2.RData")

MRPLSe <- getAll("MEPLSe",adultODS10K,c(1:13))
MRDLSe <- getAll("MEDLSe",adultODS10K,c(1:13))
MRPLTSe <- getAll("MEPLTSe",adultODS10K,c(1:13))
MRDLTSe <- getAll("MEDLTSe",adultODS10K,c(1:13))

rm(list = paste0("ME",c("PL","PLT","DL","DLT"),"Se"))


load("sdsAdultMSSe.RData")

MKCPSe <- multiMK(adultODS10K,"sdsAdultCatMPSe",sigmaL,c(1:100),rankbL,selL)
MKCPTSe <- multiMK(adultODS10K,"sdsAdultCatMPTSe",sigmaL,c(1:100),rankbL,selL)
MKCCSe <- multiMK(adultODS10K,"sdsAdultCatMCSe",sigmaL,c(1:100),rankbL,selL)
MKCCTSe <- multiMK(adultODS10K,"sdsAdultCatMCTSe",sigmaL,c(1:100),rankbL,selL)

save(list = paste0("MK",c("CP","CPT","CC","CCT"),"Se"),
     file = "MK/MKSSe2.RData")

MECPSe <- findEst("sdsAdultCatMPSe","MKCPSe",c(1:100),c(1:13))
MECPTSe <- findEst("sdsAdultCatMPSe","MKCPTSe",c(1:100),c(1:13))
MECCSe <- findEst("sdsAdultCatMCSe","MKCCSe",c(1:100),c(1:13))
MECCTSe <- findEst("sdsAdultCatMCTSe","MKCCTSe",c(1:100),c(1:13))

rm(list = c("sdsAdultCatMPSe","sdsAdultCatMPTSe","sdsAdultCatMCSe","sdsAdultCatMCTSe"))
rm(list = paste0("MK",c("CP","CPT","CC","CCT"),"Se"))

save(list = paste0("ME",c("CP","CPT","CC","CCT"),"Se"),
     file = "MK/MESSe2.RData")

MRCPSe <- getAll("MECPSe",adultODS10K,c(1:13))
MRCPTSe <- getAll("MECPTSe",adultODS10K,c(1:13))
MRCCSe <- getAll("MECCSe",adultODS10K,c(1:13))
MRCCTSe <- getAll("MECCTSe",adultODS10K,c(1:13))

rm(list = paste0("ME",c("CP","CPT","CC","CCT"),"Se"))

MRL <- c(pasteC(list("MR",c("P","D"),c("","H","L"),c("","T"),c("","Se"))),
         pasteC(list("MR","D",c("","H","L"),c("","T"),"S")),
         pasteC(list("MRC",c("P","C"),c("","T"),c("","S","Se"))))


## Polish and Avila

sigma1 <- rep.int(0.1,14)
sigma1[c(2,5,8,13,14)] <- c(1,1,1,1,1)
sigma3 <- rep.int(1,14)
sigmaL <- c(rep(list(sigma3),times = 14),rep(list(sigma1),times = 14))

rankbL <- rep.int(4,28)

sel1 <- c(1:14)
selLp <- lapply(sapply(sel1,function(x) list(x)),function(x) sel1[-x])
selL <- c(selLp,selLp)


load("sdsPolishMH.RData")

MKPP <- multiMK(ods[c(1:2500),],"sdsPolishParMH",sigmaL,c(1:100),rankbL,selL)
MKPPT <- multiMK(ods[c(1:2500),],"sdsPolishParMTH",sigmaL,c(1:100),rankbL,selL)
MKPD <- multiMK(ods[c(1:2500),],"sdsPolishDecMH",sigmaL,c(1:100),rankbL,selL)
MKPDT <- multiMK(ods[c(1:2500),],"sdsPolishDecMTH",sigmaL,c(1:100),rankbL,selL)

save(list = paste0("MKP",c("P","PT","D","DT")),
     file = "MK/MKP2.RData")

MEPP <- findEst("sdsPolishParMH","MKPP",c(1:100),c(1:14))
MEPPT <- findEst("sdsPolishParMTH","MKPPT",c(1:100),c(1:14))
MEPD <- findEst("sdsPolishDecMH","MKPD",c(1:100),c(1:14))
MEPDT <- findEst("sdsPolishDecMTH","MKPDT",c(1:100),c(1:14))

rm(list = c("sdsPolishParMH","sdsPolishParMTH","sdsPolishDecMH","sdsPolishDecMTH"))
rm(list = paste0("MKP",c("P","PT","D","DT")))

save(list = paste0("MEP",c("P","PT","D","DT")),
     file = "MK/MEP2.RData")

MRPP <- getAll("MEPP",ods[c(1:2500),],c(1:14))
MRPPT <- getAll("MEPPT",ods[c(1:2500),],c(1:14))
MRPD <- getAll("MEPD",ods[c(1:2500),],c(1:14))
MRPDT <- getAll("MEPDT",ods[c(1:2500),],c(1:14))

rm(list = paste0("MEP",c("P","PT","D","DT")))


load("sdsPolishMSH.RData")

MKPCP <- multiMK(ods[c(1:2500),],"sdsPolishCatMPH",sigmaL,c(1:100),rankbL,selL)
MKPCPT <- multiMK(ods[c(1:2500),],"sdsPolishCatMPTH",sigmaL,c(1:100),rankbL,selL)
MKPCC <- multiMK(ods[c(1:2500),],"sdsPolishCatMCH",sigmaL,c(1:100),rankbL,selL)
MKPCCT <- multiMK(ods[c(1:2500),],"sdsPolishCatMCTH",sigmaL,c(1:100),rankbL,selL)

save(list = paste0("MKP",c("CP","CPT","CC","CCT")),
     file = "MK/MKSP2.RData")

MEPCP <- findEst("sdsPolishCatMPH","MKPCP",c(1:100),c(1:14))
MEPCPT <- findEst("sdsPolishCatMPTH","MKPCPT",c(1:100),c(1:14))
MEPCC <- findEst("sdsPolishCatMCH","MKPCC",c(1:100),c(1:14))
MEPCCT <- findEst("sdsPolishCatMCTH","MKPCCT",c(1:100),c(1:14))

rm(list = c("sdsPolishCatMPH","sdsPolishCatMPTH","sdsPolishCatMCH","sdsPolishCatMCTH"))
rm(list = paste0("MKP",c("CP","CPT","CC","CCT")))

save(list = paste0("MEP",c("CP","CPT","CC","CCT")),
     file = "MK/MESP2.RData")

MRPCP <- getAll("MEPCP",ods[c(1:2500),],c(1:14))
MRPCPT <- getAll("MEPCPT",ods[c(1:2500),],c(1:14))
MRPCC <- getAll("MEPCC",ods[c(1:2500),],c(1:14))
MRPCCT <- getAll("MEPCCT",ods[c(1:2500),],c(1:14))

rm(list = paste0("MEP",c("CP","CPT","CC","CCT")))



sigma1 <- rep.int(0.01,11)
sigma2 <- rep.int(0.001,11)
sigma3 <- rep.int(0.1,11)
sigmaS <- rep.int(1,11)
sigmaL <- rep(list(sigmaS),times = 11)

rankbL <- rep.int(4,11)

sel1 <- c(1:11)
selLp <- lapply(sapply(sel1,function(x) list(x)),function(x) sel1[-x])
selL <- selLp

load("sdsAvilaMH.RData")

MKAP <- multiMK(avilaODS[c(1:5215),],"sdsAvilaParMH",sigmaL,c(1:100),rankbL,selL)
MKAPT <- multiMK(avilaODS[c(1:5215),],"sdsAvilaParMTH",sigmaL,c(1:100),rankbL,selL)
MKAD <- multiMK(avilaODS[c(1:5215),],"sdsAvilaDecMH",sigmaL,c(1:100),rankbL,selL)
MKADT <- multiMK(avilaODS[c(1:5215),],"sdsAvilaDecMTH",sigmaL,c(1:100),rankbL,selL)

save(list = paste0("MKA",c("P","PT","D","DT")),
     file = "MK/MKA2.RData")

MEAP <- findEst("sdsAvilaParMH","MKAP",c(1:100),c(1:11))
MEAPT <- findEst("sdsAvilaParMTH","MKAPT",c(1:100),c(1:11))
MEAD <- findEst("sdsAvilaDecMH","MKAD",c(1:100),c(1:11))
MEADT <- findEst("sdsAvilaDecMTH","MKADT",c(1:100),c(1:11))

rm(list = c("sdsAvilaParMH","sdsAvilaParMTH","sdsAvilaDecMH","sdsAvilaDecMTH"))
rm(list = paste0("MKA",c("P","PT","D","DT")))

save(list = paste0("MEA",c("P","PT","D","DT")),
     file = "MK/MEA2.RData")

MRAP <- getAll("MEAP",avilaODS[c(1:5215),],c(1:11))
MRAPT <- getAll("MEAPT",avilaODS[c(1:5215),],c(1:11))
MRAD <- getAll("MEAD",avilaODS[c(1:5215),],c(1:11))
MRADT <- getAll("MEADT",avilaODS[c(1:5215),],c(1:11))

rm(list = paste0("MEA",c("P","PT","D","DT")))



load("sdsAvilaMSH.RData")

MKACP <- multiMK(avilaODS[c(1:5215),],"sdsAvilaCatMPH",sigmaL,c(1:100),rankbL,selL)
MKACPT <- multiMK(avilaODS[c(1:5215),],"sdsAvilaCatMPTH",sigmaL,c(1:100),rankbL,selL)
MKACC <- multiMK(avilaODS[c(1:5215),],"sdsAvilaCatMCH",sigmaL,c(1:100),rankbL,selL)
MKACCT <- multiMK(avilaODS[c(1:5215),],"sdsAvilaCatMCTH",sigmaL,c(1:100),rankbL,selL)

save(list = paste0("MKA",c("CP","CPT","CC","CCT")),
     file = "MK/MKSA2.RData")

MEACP <- findEst("sdsAvilaCatMPH","MKACP",c(1:100),c(1:11))
MEACPT <- findEst("sdsAvilaCatMPTH","MKACPT",c(1:100),c(1:11))
MEACC <- findEst("sdsAvilaCatMCH","MKACC",c(1:100),c(1:11))
MEACCT <- findEst("sdsAvilaCatMCTH","MKACCT",c(1:100),c(1:11))

rm(list = c("sdsAvilaCatMPH","sdsAvilaCatMPTH","sdsAvilaCatMCH","sdsAvilaCatMCTH"))
rm(list = paste0("MKA",c("CP","CPT","CC","CCT")))

save(list = paste0("MEA",c("CP","CPT","CC","CCT")),
     file = "MK/MESA2.RData")

MRACP <- getAll("MEACP",avilaODS[c(1:5215),],c(1:11))
MRACPT <- getAll("MEACPT",avilaODS[c(1:5215),],c(1:11))
MRACC <- getAll("MEACC",avilaODS[c(1:5215),],c(1:11))
MRACCT <- getAll("MEACCT",avilaODS[c(1:5215),],c(1:11))

rm(list = paste0("MEA",c("CP","CPT","CC","CCT")))

MRL <- c(pasteC(list("MR",c("P","A"),c("P","D","CP","CC"),c("","T"))))


## Maximum Knowledge Baseline

sigma1 <- rep.int(0.1,13)
sigma1[c(1,9,10,11)] <- c(1,1,1,1)
sigma3 <- rep.int(1,13)
sigmaL <- c(rep(list(sigma3),times = 14))

rankbL <- rep.int(42,14)

sel1 <- c(1:13)
selLp <- lapply(sapply(sel1,function(x) list(x)),function(x) sel1[-x])
selL <- c(list(sel1),selLp)

load("sdsAdultM.RData")

MKP <- multiMK(adultODS10K,"sdsAdultParM",sigmaL,c(1:100),rankbL,selL)
MKD <- multiMK(adultODS10K,"sdsAdultDecM",sigmaL,c(1:100),rankbL,selL)
MKPT <- multiMK(adultODS10K,"sdsAdultParMT",sigmaL,c(1:100),rankbL,selL)
MKDT <- multiMK(adultODS10K,"sdsAdultDecMT",sigmaL,c(1:100),rankbL,selL)

save(list = paste0("MK",c("P","PT","D","DT")),
     file = "MK/MKR.RData")

MEP <- findEst("sdsAdultParM","MKP",c(1:100),c(1:13))
MED <- findEst("sdsAdultDecM","MKD",c(1:100),c(1:13))
MEPT <- findEst("sdsAdultParMT","MKPT",c(1:100),c(1:13))
MEDT <- findEst("sdsAdultDecMT","MKDT",c(1:100),c(1:13))

rm(list = c("sdsAdultParM","sdsAdultDecM","sdsAdultParMT","sdsAdultDecMT"))
rm(list = paste0("MK",c("P","PT","D","DT")))

save(list = paste0("ME",c("P","PT","D","DT")),
     file = "MK/MER.RData")

MRP <- getAll("MEP",adultODS10K,c(1:13))
MRD <- getAll("MED",adultODS10K,c(1:13))
MRPT <- getAll("MEPT",adultODS10K,c(1:13))
MRDT <- getAll("MEDT",adultODS10K,c(1:13))

rm(list = paste0("ME",c("P","PT","D","DT")))


load("sdsAdultMH.RData")

MKPH <- multiMK(adultODS10K,"sdsAdultParMH",sigmaL,c(1:100),rankbL,selL)
MKDH <- multiMK(adultODS10K,"sdsAdultDecMH",sigmaL,c(1:100),rankbL,selL)
MKPHT <- multiMK(adultODS10K,"sdsAdultParMHT",sigmaL,c(1:100),rankbL,selL)
MKDHT <- multiMK(adultODS10K,"sdsAdultDecMHT",sigmaL,c(1:100),rankbL,selL)

save(list = paste0("MK",c("PH","PHT","DH","DHT")),
     file = "MK/MKHR.RData")

MEPH <- findEst("sdsAdultParMH","MKPH",c(1:100),c(1:13))
MEDH <- findEst("sdsAdultDecMH","MKDH",c(1:100),c(1:13))
MEPHT <- findEst("sdsAdultParMHT","MKPHT",c(1:100),c(1:13))
MEDHT <- findEst("sdsAdultDecMHT","MKDHT",c(1:100),c(1:13))

rm(list = c("sdsAdultParMH","sdsAdultDecMH","sdsAdultParMHT","sdsAdultDecMHT"))
rm(list = paste0("MK",c("PH","PHT","DH","DHT")))

save(list = paste0("ME",c("PH","PHT","DH","DHT")),
     file = "MK/MEHR.RData")

MRPH <- getAll("MEPH",adultODS10K,c(1:13))
MRDH <- getAll("MEDH",adultODS10K,c(1:13))
MRPHT <- getAll("MEPHT",adultODS10K,c(1:13))
MRDHT <- getAll("MEDHT",adultODS10K,c(1:13))

rm(list = paste0("ME",c("PH","PHT","DH","DHT")))


load("sdsAdultML.RData")

MKPL <- multiMK(adultODS10K,"sdsAdultParML",sigmaL,c(1:100),rankbL,selL)
MKDL <- multiMK(adultODS10K,"sdsAdultDecML",sigmaL,c(1:100),rankbL,selL)
MKPLT <- multiMK(adultODS10K,"sdsAdultParMLT",sigmaL,c(1:100),rankbL,selL)
MKDLT <- multiMK(adultODS10K,"sdsAdultDecMLT",sigmaL,c(1:100),rankbL,selL)

save(list = paste0("MK",c("PL","PLT","DL","DLT")),
     file = "MK/MKLR.RData")

MEPL <- findEst("sdsAdultParML","MKPL",c(1:100),c(1:13))
MEDL <- findEst("sdsAdultDecML","MKDL",c(1:100),c(1:13))
MEPLT <- findEst("sdsAdultParMLT","MKPLT",c(1:100),c(1:13))
MEDLT <- findEst("sdsAdultDecMLT","MKDLT",c(1:100),c(1:13))

rm(list = c("sdsAdultParML","sdsAdultDecML","sdsAdultParMLT","sdsAdultDecMLT"))
rm(list = paste0("MK",c("PL","PLT","DL","DLT")))

save(list = paste0("ME",c("PL","PLT","DL","DLT")),
     file = "MK/MELR.RData")

MRPL <- getAll("MEPL",adultODS10K,c(1:13))
MRDL <- getAll("MEDL",adultODS10K,c(1:13))
MRPLT <- getAll("MEPLT",adultODS10K,c(1:13))
MRDLT <- getAll("MEDLT",adultODS10K,c(1:13))

rm(list = paste0("ME",c("PL","PLT","DL","DLT")))


load("sdsAdultMS.RData")

MKCP <- multiMK(adultODS10K,"sdsAdultCatMP",sigmaL,c(1:100),rankbL,selL)
MKCPT <- multiMK(adultODS10K,"sdsAdultCatMPT",sigmaL,c(1:100),rankbL,selL)
MKCC <- multiMK(adultODS10K,"sdsAdultCatMC",sigmaL,c(1:100),rankbL,selL)
MKCCT <- multiMK(adultODS10K,"sdsAdultCatMCT",sigmaL,c(1:100),rankbL,selL)

save(list = paste0("MK",c("CP","CPT","CC","CCT")),
     file = "MK/MKSR.RData")

MECP <- findEst("sdsAdultCatMP","MKCP",c(1:100),c(1:13))
MECPT <- findEst("sdsAdultCatMP","MKCPT",c(1:100),c(1:13))
MECC <- findEst("sdsAdultCatMC","MKCC",c(1:100),c(1:13))
MECCT <- findEst("sdsAdultCatMCT","MKCCT",c(1:100),c(1:13))

rm(list = c("sdsAdultCatMP","sdsAdultCatMPT","sdsAdultCatMC","sdsAdultCatMCT"))
rm(list = paste0("MK",c("CP","CPT","CC","CCT")))

save(list = paste0("ME",c("CP","CPT","CC","CCT")),
     file = "MK/MESR.RData")

MRCP <- getAll("MECP",adultODS10K,c(1:13))
MRCPT <- getAll("MECPT",adultODS10K,c(1:13))
MRCC <- getAll("MECC",adultODS10K,c(1:13))
MRCCT <- getAll("MECCT",adultODS10K,c(1:13))

rm(list = paste0("ME",c("CP","CPT","CC","CCT")))

## Smoothing

load("sdsAdultMSm.RData")


MKDS <- multiMK(adultODS10K,"sdsAdultDecMS",sigmaL,c(1:100),rankbL,selL)
MKDTS <- multiMK(adultODS10K,"sdsAdultDecMTS",sigmaL,c(1:100),rankbL,selL)

save(list = paste0("MK",c("DS","DTS")),
     file = "MK/MKSmR.RData")

MEDS <- findEst("sdsAdultDecMS","MKDS",c(1:100),c(1:13))
MEDTS <- findEst("sdsAdultDecMTS","MKDTS",c(1:100),c(1:13))

rm(list = c("sdsAdultDecMS","sdsAdultDecMTS"))
rm(list = paste0("MK",c("DS","DTS")))

save(list = paste0("ME",c("DS","DTS")),
     file = "MK/MESmR.RData")

MRDS <- getAll("MEDS",adultODS10K,c(1:13))
MRDTS <- getAll("MEDTS",adultODS10K,c(1:13))

rm(list = paste0("ME",c("DS","DTS")))


load("sdsAdultMHSm.RData")

MKDHS <- multiMK(adultODS10K,"sdsAdultDecMHS",sigmaL,c(1:100),rankbL,selL)
MKDHTS <- multiMK(adultODS10K,"sdsAdultDecMHTS",sigmaL,c(1:100),rankbL,selL)

save(list = paste0("MK",c("DH","DHT"),"S"),
     file = "MK/MKHSmR.RData")

MEDHS <- findEst("sdsAdultDecMHS","MKDHS",c(1:100),c(1:13))
MEDHTS <- findEst("sdsAdultDecMHTS","MKDHTS",c(1:100),c(1:13))

rm(list = c("sdsAdultDecMHS","sdsAdultDecMHTS"))
rm(list = paste0("MK",c("DH","DHT"),"S"))

save(list = paste0("ME",c("DH","DHT"),"S"),
     file = "MK/MEHSmR.RData")

MRDHS <- getAll("MEDHS",adultODS10K,c(1:13))
MRDHTS <- getAll("MEDHTS",adultODS10K,c(1:13))

rm(list = paste0("ME",c("DH","DHT"),"S"))


load("sdsAdultMLSm.RData")

MKDLS <- multiMK(adultODS10K,"sdsAdultDecMLS",sigmaL,c(1:100),rankbL,selL)
MKDLTS <- multiMK(adultODS10K,"sdsAdultDecMLTS",sigmaL,c(1:100),rankbL,selL)

save(list = paste0("MK",c("DL","DLT"),"S"),
     file = "MK/MKLSmR.RData")

MEDLS <- findEst("sdsAdultDecMLS","MKDLS",c(1:100),c(1:13))
MEDLTS <- findEst("sdsAdultDecMLTS","MKDLTS",c(1:100),c(1:13))

rm(list = c("sdsAdultDecMLS","sdsAdultDecMLTS"))
rm(list = paste0("MK",c("DLS","DLTS")))

save(list = paste0("ME",c("DL","DLT"),"S"),
     file = "MK/MELSmR.RData")

MRDLS <- getAll("MEDLS",adultODS10K,c(1:13))
MRDLTS <- getAll("MEDLTS",adultODS10K,c(1:13))

rm(list = paste0("ME",c("DLS","DLTS")))


load("sdsAdultMSSm.RData")

MKCPS <- multiMK(adultODS10K,"sdsAdultCatMPS",sigmaL,c(1:100),rankbL,selL)
MKCPTS <- multiMK(adultODS10K,"sdsAdultCatMPTS",sigmaL,c(1:100),rankbL,selL)
MKCCS <- multiMK(adultODS10K,"sdsAdultCatMCS",sigmaL,c(1:100),rankbL,selL)
MKCCTS <- multiMK(adultODS10K,"sdsAdultCatMCTS",sigmaL,c(1:100),rankbL,selL)

save(list = paste0("MK",c("CP","CPT","CC","CCT"),"S"),
     file = "MK/MKSSmR.RData")

MECPS <- findEst("sdsAdultCatMPS","MKCPS",c(1:100),c(1:13))
MECPTS <- findEst("sdsAdultCatMPS","MKCPTS",c(1:100),c(1:13))
MECCS <- findEst("sdsAdultCatMCS","MKCCS",c(1:100),c(1:13))
MECCTS <- findEst("sdsAdultCatMCTS","MKCCTS",c(1:100),c(1:13))

rm(list = c("sdsAdultCatMPS","sdsAdultCatMPTS","sdsAdultCatMCS","sdsAdultCatMCTS"))
rm(list = paste0("MK",c("CP","CPT","CC","CCT"),"S"))

save(list = paste0("ME",c("CP","CPT","CC","CCT"),"S"),
     file = "MK/MESSmR.RData")

MRCPS <- getAll("MECPS",adultODS10K,c(1:13))
MRCPTS <- getAll("MECPTS",adultODS10K,c(1:13))
MRCCS <- getAll("MECCS",adultODS10K,c(1:13))
MRCCTS <- getAll("MECCTS",adultODS10K,c(1:13))

rm(list = paste0("ME",c("CP","CPT","CC","CCT"),"S"))

## Selective

load("sdsAdultMSe.RData")

MKPSe <- multiMK(adultODS10K,"sdsAdultParMSe",sigmaL,c(1:100),rankbL,selL)
MKDSe <- multiMK(adultODS10K,"sdsAdultDecMSe",sigmaL,c(1:100),rankbL,selL)
MKPTSe <- multiMK(adultODS10K,"sdsAdultParMTSe",sigmaL,c(1:100),rankbL,selL)
MKDTSe <- multiMK(adultODS10K,"sdsAdultDecMTSe",sigmaL,c(1:100),rankbL,selL)

save(list = paste0("MK",c("P","PT","D","DT"),"Se"),
     file = "MK/MKSeR.RData")

MEPSe <- findEst("sdsAdultParMSe","MKPSe",c(1:100),c(1:13))
MEDSe <- findEst("sdsAdultDecMSe","MKDSe",c(1:100),c(1:13))
MEPTSe <- findEst("sdsAdultParMTSe","MKPTSe",c(1:100),c(1:13))
MEDTSe <- findEst("sdsAdultDecMTSe","MKDTSe",c(1:100),c(1:13))

rm(list = c("sdsAdultParMSe","sdsAdultDecMSe","sdsAdultParMTSe","sdsAdultDecMTSe"))
rm(list = paste0("MK",c("P","PT","D","DT"),"Se"))

save(list = paste0("ME",c("P","PT","D","DT"),"Se"),
     file = "MK/MESeR.RData")

MRPSe <- getAll("MEPSe",adultODS10K,c(1:13))
MRDSe <- getAll("MEDSe",adultODS10K,c(1:13))
MRPTSe <- getAll("MEPTSe",adultODS10K,c(1:13))
MRDTSe <- getAll("MEDTSe",adultODS10K,c(1:13))

rm(list = paste0("ME",c("P","PT","D","DT"),"Se"))


load("sdsAdultMHSe.RData")

MKPHSe <- multiMK(adultODS10K,"sdsAdultParMHSe",sigmaL,c(1:100),rankbL,selL)
MKDHSe <- multiMK(adultODS10K,"sdsAdultDecMHSe",sigmaL,c(1:100),rankbL,selL)
MKPHTSe <- multiMK(adultODS10K,"sdsAdultParMHTSe",sigmaL,c(1:100),rankbL,selL)
MKDHTSe <- multiMK(adultODS10K,"sdsAdultDecMHTSe",sigmaL,c(1:100),rankbL,selL)

save(list = paste0("MK",c("PH","PHT","DH","DHT"),"Se"),
     file = "MK/MKHSeR.RData")

MEPHSe <- findEst("sdsAdultParMHSe","MKPHSe",c(1:100),c(1:13))
MEDHSe <- findEst("sdsAdultDecMHSe","MKDHSe",c(1:100),c(1:13))
MEPHTSe <- findEst("sdsAdultParMHTSe","MKPHTSe",c(1:100),c(1:13))
MEDHTSe <- findEst("sdsAdultDecMHTSe","MKDHTSe",c(1:100),c(1:13))

rm(list = c("sdsAdultParMHSe","sdsAdultDecMHSe","sdsAdultParMHTSe","sdsAdultDecMHTSe"))
rm(list = paste0("MK",c("PH","PHT","DH","DHT"),"Se"))

save(list = paste0("ME",c("PH","PHT","DH","DHT"),"Se"),
     file = "MK/MEHSeR.RData")

MRPHSe <- getAll("MEPHSe",adultODS10K,c(1:13))
MRDHSe <- getAll("MEDHSe",adultODS10K,c(1:13))
MRPHTSe <- getAll("MEPHTSe",adultODS10K,c(1:13))
MRDHTSe <- getAll("MEDHTSe",adultODS10K,c(1:13))

rm(list = paste0("ME",c("PH","PHT","DH","DHT"),"Se"))


load("sdsAdultMLSe.RData")

MKPLSe <- multiMK(adultODS10K,"sdsAdultParMLSe",sigmaL,c(1:100),rankbL,selL)
MKDLSe <- multiMK(adultODS10K,"sdsAdultDecMLSe",sigmaL,c(1:100),rankbL,selL)
MKPLTSe <- multiMK(adultODS10K,"sdsAdultParMLTSe",sigmaL,c(1:100),rankbL,selL)
MKDLTSe <- multiMK(adultODS10K,"sdsAdultDecMLTSe",sigmaL,c(1:100),rankbL,selL)

save(list = paste0("MK",c("PL","PLT","DL","DLT"),"Se"),
     file = "MK/MKLSeR.RData")

MEPLSe <- findEst("sdsAdultParMLSe","MKPLSe",c(1:100),c(1:13))
MEDLSe <- findEst("sdsAdultDecMLSe","MKDLSe",c(1:100),c(1:13))
MEPLTSe <- findEst("sdsAdultParMLTSe","MKPLTSe",c(1:100),c(1:13))
MEDLTSe <- findEst("sdsAdultDecMLTSe","MKDLTSe",c(1:100),c(1:13))

rm(list = c("sdsAdultParMLSe","sdsAdultDecMLSe","sdsAdultParMLTSe","sdsAdultDecMLTSe"))
rm(list = paste0("MK",c("PL","PLT","DL","DLT"),"Se"))

save(list = paste0("ME",c("PL","PLT","DL","DLT"),"Se"),
     file = "MK/MELSeR.RData")

MRPLSe <- getAll("MEPLSe",adultODS10K,c(1:13))
MRDLSe <- getAll("MEDLSe",adultODS10K,c(1:13))
MRPLTSe <- getAll("MEPLTSe",adultODS10K,c(1:13))
MRDLTSe <- getAll("MEDLTSe",adultODS10K,c(1:13))

rm(list = paste0("ME",c("PL","PLT","DL","DLT"),"Se"))


load("sdsAdultMSSe.RData")

MKCPSe <- multiMK(adultODS10K,"sdsAdultCatMPSe",sigmaL,c(1:100),rankbL,selL)
MKCPTSe <- multiMK(adultODS10K,"sdsAdultCatMPTSe",sigmaL,c(1:100),rankbL,selL)
MKCCSe <- multiMK(adultODS10K,"sdsAdultCatMCSe",sigmaL,c(1:100),rankbL,selL)
MKCCTSe <- multiMK(adultODS10K,"sdsAdultCatMCTSe",sigmaL,c(1:100),rankbL,selL)

save(list = paste0("MK",c("CP","CPT","CC","CCT"),"Se"),
     file = "MK/MKSSeR.RData")

MECPSe <- findEst("sdsAdultCatMPSe","MKCPSe",c(1:100),c(1:13))
MECPTSe <- findEst("sdsAdultCatMPSe","MKCPTSe",c(1:100),c(1:13))
MECCSe <- findEst("sdsAdultCatMCSe","MKCCSe",c(1:100),c(1:13))
MECCTSe <- findEst("sdsAdultCatMCTSe","MKCCTSe",c(1:100),c(1:13))

rm(list = c("sdsAdultCatMPSe","sdsAdultCatMPTSe","sdsAdultCatMCSe","sdsAdultCatMCTSe"))
rm(list = paste0("MK",c("CP","CPT","CC","CCT"),"Se"))

save(list = paste0("ME",c("CP","CPT","CC","CCT"),"Se"),
     file = "MK/MESSeR.RData")

MRCPSe <- getAll("MECPSe",adultODS10K,c(1:13))
MRCPTSe <- getAll("MECPTSe",adultODS10K,c(1:13))
MRCCSe <- getAll("MECCSe",adultODS10K,c(1:13))
MRCCTSe <- getAll("MECCTSe",adultODS10K,c(1:13))

rm(list = paste0("ME",c("CP","CPT","CC","CCT"),"Se"))

MRL <- c(pasteC(list("MR",c("P","D"),c("","H","L"),c("","T"),c("","Se"))),
         pasteC(list("MR","D",c("","H","L"),c("","T"),"S")),
         pasteC(list("MRC",c("P","C"),c("","T"),c("","S","Se"))))

## Polish and Avila

sigma1 <- rep.int(0.1,14)
sigma1[c(2,5,8,13,14)] <- c(1,1,1,1,1)
sigma3 <- rep.int(1,14)
sigmaL <- rep(list(sigma3),times = 15)

rankbL <- rep.int(42,15)

sel1 <- c(1:14)
selLp <- lapply(sapply(sel1,function(x) list(x)),function(x) sel1[-x])
selL <- c(sel1,selLp)


load("sdsPolishMH.RData")

MKPP <- multiMK(ods[c(1:2500),],"sdsPolishParMH",sigmaL,c(1:100),rankbL,selL)
MKPPT <- multiMK(ods[c(1:2500),],"sdsPolishParMTH",sigmaL,c(1:100),rankbL,selL)
MKPD <- multiMK(ods[c(1:2500),],"sdsPolishDecMH",sigmaL,c(1:100),rankbL,selL)
MKPDT <- multiMK(ods[c(1:2500),],"sdsPolishDecMTH",sigmaL,c(1:100),rankbL,selL)

save(list = paste0("MKP",c("P","PT","D","DT")),
     file = "MK/MKPR.RData")

MEPP <- findEst("sdsPolishParMH","MKPP",c(1:100),c(1:14))
MEPPT <- findEst("sdsPolishParMTH","MKPPT",c(1:100),c(1:14))
MEPD <- findEst("sdsPolishDecMH","MKPD",c(1:100),c(1:14))
MEPDT <- findEst("sdsPolishDecMTH","MKPDT",c(1:100),c(1:14))

rm(list = c("sdsPolishParMH","sdsPolishParMTH","sdsPolishDecMH","sdsPolishDecMTH"))
rm(list = paste0("MKP",c("P","PT","D","DT")))

save(list = paste0("MEP",c("P","PT","D","DT")),
     file = "MK/MEPR.RData")

MRPP <- getAll("MEPP",ods[c(1:2500),],c(1:14))
MRPPT <- getAll("MEPPT",ods[c(1:2500),],c(1:14))
MRPD <- getAll("MEPD",ods[c(1:2500),],c(1:14))
MRPDT <- getAll("MEPDT",ods[c(1:2500),],c(1:14))

rm(list = paste0("MEP",c("P","PT","D","DT")))


load("sdsPolishMSH.RData")

MKPCP <- multiMK(ods[c(1:2500),],"sdsPolishCatMPH",sigmaL,c(1:100),rankbL,selL)
MKPCPT <- multiMK(ods[c(1:2500),],"sdsPolishCatMPTH",sigmaL,c(1:100),rankbL,selL)
MKPCC <- multiMK(ods[c(1:2500),],"sdsPolishCatMCH",sigmaL,c(1:100),rankbL,selL)
MKPCCT <- multiMK(ods[c(1:2500),],"sdsPolishCatMCTH",sigmaL,c(1:100),rankbL,selL)

save(list = paste0("MKP",c("CP","CPT","CC","CCT")),
     file = "MK/MKSPR.RData")

MEPCP <- findEst("sdsPolishCatMPH","MKPCP",c(1:100),c(1:14))
MEPCPT <- findEst("sdsPolishCatMPTH","MKPCPT",c(1:100),c(1:14))
MEPCC <- findEst("sdsPolishCatMCH","MKPCC",c(1:100),c(1:14))
MEPCCT <- findEst("sdsPolishCatMCTH","MKPCCT",c(1:100),c(1:14))

rm(list = c("sdsPolishCatMPH","sdsPolishCatMPTH","sdsPolishCatMCH","sdsPolishCatMCTH"))
rm(list = paste0("MKP",c("CP","CPT","CC","CCT")))

save(list = paste0("MEP",c("CP","CPT","CC","CCT")),
     file = "MK/MESPR.RData")

MRPCP <- getAll("MEPCP",ods[c(1:2500),],c(1:14))
MRPCPT <- getAll("MEPCPT",ods[c(1:2500),],c(1:14))
MRPCC <- getAll("MEPCC",ods[c(1:2500),],c(1:14))
MRPCCT <- getAll("MEPCCT",ods[c(1:2500),],c(1:14))

rm(list = paste0("MEP",c("CP","CPT","CC","CCT")))



sigma1 <- rep.int(0.01,11)
sigma2 <- rep.int(0.001,11)
sigma3 <- rep.int(0.1,11)
sigmaS <- rep.int(1,11)
sigmaL <- rep(list(sigmaS),times = 12)

rankbL <- rep.int(4,12)

sel1 <- c(1:11)
selLp <- lapply(sapply(sel1,function(x) list(x)),function(x) sel1[-x])
selL <- c(sel1,selLp)

load("sdsAvilaMH.RData")

MKAP <- multiMK(avilaODS[c(1:5215),],"sdsAvilaParMH",sigmaL,c(1:100),rankbL,selL)
MKAPT <- multiMK(avilaODS[c(1:5215),],"sdsAvilaParMTH",sigmaL,c(1:100),rankbL,selL)
MKAD <- multiMK(avilaODS[c(1:5215),],"sdsAvilaDecMH",sigmaL,c(1:100),rankbL,selL)
MKADT <- multiMK(avilaODS[c(1:5215),],"sdsAvilaDecMTH",sigmaL,c(1:100),rankbL,selL)

save(list = paste0("MKA",c("P","PT","D","DT")),
     file = "MK/MKAR.RData")

MEAP <- findEst("sdsAvilaParMH","MKAP",c(1:100),c(1:11))
MEAPT <- findEst("sdsAvilaParMTH","MKAPT",c(1:100),c(1:11))
MEAD <- findEst("sdsAvilaDecMH","MKAD",c(1:100),c(1:11))
MEADT <- findEst("sdsAvilaDecMTH","MKADT",c(1:100),c(1:11))

rm(list = c("sdsAvilaParMH","sdsAvilaParMTH","sdsAvilaDecMH","sdsAvilaDecMTH"))
rm(list = paste0("MKA",c("P","PT","D","DT")))

save(list = paste0("MEA",c("P","PT","D","DT")),
     file = "MK/MEAR.RData")

MRAP <- getAll("MEAP",avilaODS[c(1:5215),],c(1:11))
MRAPT <- getAll("MEAPT",avilaODS[c(1:5215),],c(1:11))
MRAD <- getAll("MEAD",avilaODS[c(1:5215),],c(1:11))
MRADT <- getAll("MEADT",avilaODS[c(1:5215),],c(1:11))

rm(list = paste0("MEA",c("P","PT","D","DT")))



load("sdsAvilaMSH.RData")

MKACP <- multiMK(avilaODS[c(1:5215),],"sdsAvilaCatMPH",sigmaL,c(1:100),rankbL,selL)
MKACPT <- multiMK(avilaODS[c(1:5215),],"sdsAvilaCatMPTH",sigmaL,c(1:100),rankbL,selL)
MKACC <- multiMK(avilaODS[c(1:5215),],"sdsAvilaCatMCH",sigmaL,c(1:100),rankbL,selL)
MKACCT <- multiMK(avilaODS[c(1:5215),],"sdsAvilaCatMCTH",sigmaL,c(1:100),rankbL,selL)

save(list = paste0("MKA",c("CP","CPT","CC","CCT")),
     file = "MK/MKSAR.RData")

MEACP <- findEst("sdsAvilaCatMPH","MKACP",c(1:100),c(1:11))
MEACPT <- findEst("sdsAvilaCatMPTH","MKACPT",c(1:100),c(1:11))
MEACC <- findEst("sdsAvilaCatMCH","MKACC",c(1:100),c(1:11))
MEACCT <- findEst("sdsAvilaCatMCTH","MKACCT",c(1:100),c(1:11))

rm(list = c("sdsAvilaCatMPH","sdsAvilaCatMPTH","sdsAvilaCatMCH","sdsAvilaCatMCTH"))
rm(list = paste0("MKA",c("CP","CPT","CC","CCT")))

save(list = paste0("MEA",c("CP","CPT","CC","CCT")),
     file = "MK/MESAR.RData")

MRACP <- getAll("MEACP",avilaODS[c(1:5215),],c(1:11))
MRACPT <- getAll("MEACPT",avilaODS[c(1:5215),],c(1:11))
MRACC <- getAll("MEACC",avilaODS[c(1:5215),],c(1:11))
MRACCT <- getAll("MEACCT",avilaODS[c(1:5215),],c(1:11))

rm(list = paste0("MEA",c("CP","CPT","CC","CCT")))

MRL <- c(pasteC(list("MR",c("P","A"),c("P","D","CP","CC"),c("","T"))))

MRL <- c(pasteC(list("MR",c("P","D"),c("","H","L"),c("","T"),c("","Se"))),
         pasteC(list("MR","D",c("","H","L"),c("","T"),"S")),
         pasteC(list("MRC",c("P","C"),c("","T"),c("","S","Se"))),
         pasteC(list("MR",c("P","A"),c("P","D","CP","CC"),c("","T"))))

## Final

sigma3 <- rep.int(1,13)
sigmaL <- c(rep(list(sigma3),times = 13))

rankbL <- rep.int(4,13)

sel1 <- c(1:13)
selLp <- lapply(sapply(sel1,function(x) list(x)),function(x) sel1[-x])
selL <- selLp

load("sdsAdultM.RData")

MKP <- multiMK(adultODS10K,"sdsAdultParM",sigmaL,c(1:100),rankbL,selL)
MKD <- multiMK(adultODS10K,"sdsAdultDecM",sigmaL,c(1:1000),rankbL,selL)
MKPT <- multiMK(adultODS10K,"sdsAdultParMT",sigmaL,c(1:100),rankbL,selL)
MKDT <- multiMK(adultODS10K,"sdsAdultDecMT",sigmaL,c(1:1000),rankbL,selL)

save(list = paste0("MK",c("P","PT","D","DT")),file = "MK/MKF.RData")

rm(list = paste0("MK",c("P","PT","D","DT")))
rm(list = pasteC(list("sdsAdult",c("Par","Dec"),"M","",c("","T"))))


load("sdsAdultMH.RData")

MKPH <- multiMK(adultODS10K,"sdsAdultParMH",sigmaL,c(1:100),rankbL,selL)
MKDH <- multiMK(adultODS10K,"sdsAdultDecMH",sigmaL,c(1:1000),rankbL,selL)
MKPHT <- multiMK(adultODS10K,"sdsAdultParMHT",sigmaL,c(1:100),rankbL,selL)
MKDHT <- multiMK(adultODS10K,"sdsAdultDecMHT",sigmaL,c(1:1000),rankbL,selL)

save(list = paste0("MK",c("PH","PHT","DH","DHT")),file = "MK/MKHF.RData")

rm(list = paste0("MK",c("PH","PHT","DH","DHT")))
rm(list = pasteC(list("sdsAdult",c("Par","Dec"),"M","H",c("","T"))))


load("sdsAdultML.RData")

MKPL <- multiMK(adultODS10K,"sdsAdultParML",sigmaL,c(1:100),rankbL,selL)
MKDL <- multiMK(adultODS10K,"sdsAdultDecML",sigmaL,c(1:1000),rankbL,selL)
MKPLT <- multiMK(adultODS10K,"sdsAdultParMLT",sigmaL,c(1:100),rankbL,selL)
MKDLT <- multiMK(adultODS10K,"sdsAdultDecMLT",sigmaL,c(1:1000),rankbL,selL)

save(list = paste0("MK",c("PL","PLT","DL","DLT")),file = "MK/MKLF.RData")

rm(list = paste0("MK",c("PL","PLT","DL","DLT")))
rm(list = pasteC(list("sdsAdult",c("Par","Dec"),"M","L",c("","T"))))


load("sdsAdultMS.RData")

MKCP <- multiMK(adultODS10K,"sdsAdultCatMP",sigmaL,c(1:1000),rankbL,selL)
MKCPT <- multiMK(adultODS10K,"sdsAdultCatMPT",sigmaL,c(1:1000),rankbL,selL)
MKCC <- multiMK(adultODS10K,"sdsAdultCatMC",sigmaL,c(1:1000),rankbL,selL)
MKCCT <- multiMK(adultODS10K,"sdsAdultCatMCT",sigmaL,c(1:1000),rankbL,selL)

save(list = paste0("MK",c("CP","CPT","CC","CCT")),file = "MK/MKSF.RData")

rm(list = paste0("MK",c("CP","CPT","CC","CCT")))
rm(list = paste0("sdsAdultCatM",c("P","PT","C","CT")))

## Smoothing

load("sdsAdultMSm.RData")

MKDS <- multiMK(adultODS10K,"sdsAdultDecMS",sigmaL,c(1:1000),rankbL,selL)
MKDTS <- multiMK(adultODS10K,"sdsAdultDecMTS",sigmaL,c(1:1000),rankbL,selL)

save(list = paste0("MK",c("D","DT"),"S"),file = "MK/MKSmF.RData")

rm(list = paste0("MK",c("D","DT"),"S"))
rm(list = pasteC(list("sdsAdult",c("Par","Dec"),"M","",c("","T"),"S")))


load("sdsAdultMHSm.RData")

MKDHS <- multiMK(adultODS10K,"sdsAdultDecMHS",sigmaL,c(1:1000),rankbL,selL)
MKDHTS <- multiMK(adultODS10K,"sdsAdultDecMHTS",sigmaL,c(1:1000),rankbL,selL)

save(list = paste0("MK",c("DH","DHT"),"S"),file = "MK/MKHSmF.RData")

rm(list = paste0("MK",c("DH","DHT"),"S"))
rm(list = pasteC(list("sdsAdult",c("Par","Dec"),"M","H",c("","T"),"S")))


load("sdsAdultMLSm.RData")

MKDLS <- multiMK(adultODS10K,"sdsAdultDecMLS",sigmaL,c(1:1000),rankbL,selL)
MKDLTS <- multiMK(adultODS10K,"sdsAdultDecMLTS",sigmaL,c(1:1000),rankbL,selL)

save(list = paste0("MK",c("DL","DLT"),"S"),file = "MK/MKLSmF.RData")

rm(list = paste0("MK",c("DL","DLT"),"S"))
rm(list = pasteC(list("sdsAdult",c("Par","Dec"),"M","L",c("","T"),"S")))


load("sdsAdultMSSm.RData")

MKCPS <- multiMK(adultODS10K,"sdsAdultCatMPS",sigmaL,c(1:1000),rankbL,selL)
MKCPTS <- multiMK(adultODS10K,"sdsAdultCatMPTS",sigmaL,c(1:1000),rankbL,selL)
MKCCS <- multiMK(adultODS10K,"sdsAdultCatMCS",sigmaL,c(1:1000),rankbL,selL)
MKCCTS <- multiMK(adultODS10K,"sdsAdultCatMCTS",sigmaL,c(1:1000),rankbL,selL)

save(list = paste0("MK",c("CP","CPT","CC","CCT"),"S"),file = "MK/MKSSmF.RData")

rm(list = paste0("MK",c("CP","CPT","CC","CCT"),"S"))
rm(list = paste0("sdsAdultCatM",c("P","PT","C","CT"),"S"))

## Selective

load("sdsAdultMSe.RData")

MKPSe <- multiMK(adultODS10K,"sdsAdultParMSe",sigmaL,c(1:100),rankbL,selL)
MKDSe <- multiMK(adultODS10K,"sdsAdultDecMSe",sigmaL,c(1:1000),rankbL,selL)
MKPTSe <- multiMK(adultODS10K,"sdsAdultParMTSe",sigmaL,c(1:100),rankbL,selL)
MKDTSe <- multiMK(adultODS10K,"sdsAdultDecMTSe",sigmaL,c(1:1000),rankbL,selL)

save(list = paste0("MK",c("P","PT","D","DT"),"Se"),file = "MK/MKSeF.RData")

rm(list = paste0("MK",c("P","PT","D","DT"),"Se"))
rm(list = pasteC(list("sdsAdult",c("Par","Dec"),"M","",c("","T"),"Se")))


load("sdsAdultMHSe.RData")

MKPHSe <- multiMK(adultODS10K,"sdsAdultParMHSe",sigmaL,c(1:100),rankbL,selL)
MKDHSe <- multiMK(adultODS10K,"sdsAdultDecMHSe",sigmaL,c(1:1000),rankbL,selL)
MKPHTSe <- multiMK(adultODS10K,"sdsAdultParMHTSe",sigmaL,c(1:100),rankbL,selL)
MKDHTSe <- multiMK(adultODS10K,"sdsAdultDecMHTSe",sigmaL,c(1:1000),rankbL,selL)

save(list = paste0("MK",c("PH","PHT","DH","DHT"),"Se"),file = "MK/MKHSeF.RData")

rm(list = paste0("MK",c("PH","PHT","DH","DHT"),"Se"))
rm(list = pasteC(list("sdsAdult",c("Par","Dec"),"M","H",c("","T"),"Se")))


load("sdsAdultMLSe.RData")

MKPLSe <- multiMK(adultODS10K,"sdsAdultParMLSe",sigmaL,c(1:100),rankbL,selL)
MKDLSe <- multiMK(adultODS10K,"sdsAdultDecMLSe",sigmaL,c(1:1000),rankbL,selL)
MKPLTSe <- multiMK(adultODS10K,"sdsAdultParMLTSe",sigmaL,c(1:100),rankbL,selL)
MKDLTSe <- multiMK(adultODS10K,"sdsAdultDecMLTSe",sigmaL,c(1:1000),rankbL,selL)

save(list = paste0("MK",c("PL","PLT","DL","DLT"),"Se"),file = "MK/MKLSeF.RData")

rm(list = paste0("MK",c("PL","PLT","DL","DLT"),"Se"))
rm(list = pasteC(list("sdsAdult",c("Par","Dec"),"M","L",c("","T"),"Se")))


load("sdsAdultMSSe.RData")

MKCPSe <- multiMK(adultODS10K,"sdsAdultCatMPSe",sigmaL,c(1:1000),rankbL,selL)
MKCPTSe <- multiMK(adultODS10K,"sdsAdultCatMPTSe",sigmaL,c(1:1000),rankbL,selL)
MKCCSe <- multiMK(adultODS10K,"sdsAdultCatMCSe",sigmaL,c(1:1000),rankbL,selL)
MKCCTSe <- multiMK(adultODS10K,"sdsAdultCatMCTSe",sigmaL,c(1:1000),rankbL,selL)

save(list = paste0("MK",c("CP","CPT","CC","CCT"),"Se"),file = "MK/MKSSeF.RData")

rm(list = paste0("MK",c("CP","CPT","CC","CCT"),"Se"))
rm(list = paste0("sdsAdultCatM",c("P","PT","C","CT"),"Se"))

## Polish and Avila

sigma3 <- rep.int(1,14)
sigmaL <- c(rep(list(sigma3),times = 14))

rankbL <- rep.int(4,14)

sel1 <- c(1:14)
selLp <- lapply(sapply(sel1,function(x) list(x)),function(x) sel1[-x])
selL <- selLp

load("sdsPolishMH.RData")

MKPP <- multiMK(ods[c(1:2500),],"sdsPolishParMH",sigmaL,c(1:1000),rankbL,selL)
MKPD <- multiMK(ods[c(1:2500),],"sdsPolishDecMH",sigmaL,c(1:1000),rankbL,selL)
MKPPT <- multiMK(ods[c(1:2500),],"sdsPolishParMTH",sigmaL,c(1:1000),rankbL,selL)
MKPDT <- multiMK(ods[c(1:2500),],"sdsPolishDecMTH",sigmaL,c(1:1000),rankbL,selL)

save(list = paste0("MKP",c("P","PT","D","DT")),file = "MK/MKPF.RData")

rm(list = paste0("MKP",c("P","PT","D","DT")))
rm(list = pasteC(list("sdsPolish",c("Par","Dec"),"M","",c("","T"),"H")))

load("sdsPolishMSH.RData")

MKPCP <- multiMK(ods[c(1:2500),],"sdsPolishCatMPH",sigmaL,c(1:1000),rankbL,selL)
MKPCPT <- multiMK(ods[c(1:2500),],"sdsPolishCatMPTH",sigmaL,c(1:1000),rankbL,selL)
MKPCC <- multiMK(ods[c(1:2500),],"sdsPolishCatMCH",sigmaL,c(1:1000),rankbL,selL)
MKPCCT <- multiMK(ods[c(1:2500),],"sdsPolishCatMCTH",sigmaL,c(1:1000),rankbL,selL)

save(list = paste0("MKPC",c("P","PT","C","CT")),file = "MK/MKSPF.RData")

rm(list = paste0("MKPC",c("P","PT","C","CT")))
rm(list = pasteC(list("sdsPolishCatM",c("P","C"),"","",c("","T"),"H")))


sigma3 <- rep.int(1,11)
sigmaL <- c(rep(list(sigma3),times = 11))

rankbL <- rep.int(4,11)

sel1 <- c(1:11)
selLp <- lapply(sapply(sel1,function(x) list(x)),function(x) sel1[-x])
selL <- selLp

load("sdsAvilaMH.RData")

MKAP <- multiMK(avilaODS[c(1:5215),],"sdsAvilaParMH",sigmaL,c(1:1000),rankbL,selL)
MKAD <- multiMK(avilaODS[c(1:5215),],"sdsAvilaDecMH",sigmaL,c(1:1000),rankbL,selL)
MKAPT <- multiMK(avilaODS[c(1:5215),],"sdsAvilaParMTH",sigmaL,c(1:1000),rankbL,selL)
MKADT <- multiMK(avilaODS[c(1:5215),],"sdsAvilaDecMTH",sigmaL,c(1:1000),rankbL,selL)

save(list = paste0("MKA",c("P","PT","D","DT")),file = "MK/MKAF.RData")

rm(list = paste0("MKA",c("P","PT","D","DT")))
rm(list = pasteC(list("sdsAvila",c("Par","Dec"),"M","",c("","T"),"H")))

load("sdsAvilaMSH.RData")

MKACP <- multiMK(avilaODS[c(1:5215),],"sdsAvilaCatMPH",sigmaL,c(1:1000),rankbL,selL)
MKACPT <- multiMK(avilaODS[c(1:5215),],"sdsAvilaCatMPTH",sigmaL,c(1:1000),rankbL,selL)
MKACC <- multiMK(avilaODS[c(1:5215),],"sdsAvilaCatMCH",sigmaL,c(1:1000),rankbL,selL)
MKACCT <- multiMK(avilaODS[c(1:5215),],"sdsAvilaCatMCTH",sigmaL,c(1:1000),rankbL,selL)

save(list = paste0("MKAC",c("P","PT","C","CT")),file = "MK/MKSAF.RData")

rm(list = paste0("MKAC",c("P","PT","C","CT")))
rm(list = pasteC(list("sdsAvilaCatM",c("P","C"),"","",c("","T"),"H")))


## Final Results

smt <- sapply(adultODS10K,function(x) length(levels(x)))
tm <- rep.int(1,times = 13)
smtp <- sapply(c(1:13),function(x) tm)
smtp[c(1:13) + c(0:12)*13] <- smt
smts <- sapply(c(1:13),function(x) list(smtp[x,]))
smts2 <- smts
smts2[c(2:8,12,13)] <- lapply(smts2[c(2:8,12,13)],function(x) tm)
smtp3 <- smtp
smtp3[c(1:13) + c(0:12)*13] <- rep.int(-1,times = 13)
smts3 <- sapply(c(1:13),function(x) list(smtp3[x,]))
smts3[c(2:8,12,13)] <- lapply(smts3[c(2:8,12,13)],function(x) tm)

load("sdsAdultM.RData")
load("MK/MKF.RData")

MR <- findEstComp("sdsAdultParM","MKP",adultODS10K,c(1:100),c(1,2,3,5,10,20,50,100),100,5,smts)
MR2 <- findEstComp("sdsAdultParM","MKP",adultODS10K,c(1:100),c(1,2,3,5,10,20,50,100),100,9,smts2)
MR3 <- findEstComp("sdsAdultParM","MKP",adultODS10K,c(1:100),c(1,2,3,5,10,20,50,100),100,13,smts3)
MR4 <- do.call(cbind,c(MR,MR2,MR3))
MRP <- MR4[,c(c(1:13)+c(0:12)*13,c(1,9:11)+c(0,8:10)*13+13*13,c(1,9:11)+c(0,8:10)*13+13*13*2)]

MR <- findEstComp("sdsAdultDecM","MKD",adultODS10K,c(1:1000),c(1,2,3,5,10,20,50,100),100,5,smts)
MR2 <- findEstComp("sdsAdultDecM","MKD",adultODS10K,c(1:1000),c(1,2,3,5,10,20,50,100),100,9,smts2)
MR3 <- findEstComp("sdsAdultDecM","MKD",adultODS10K,c(1:1000),c(1,2,3,5,10,20,50,100),100,13,smts3)
MR4 <- do.call(cbind,c(MR,MR2,MR3))
MRD <- MR4[,c(c(1:13)+c(0:12)*13,c(1,9:11)+c(0,8:10)*13+13*13,c(1,9:11)+c(0,8:10)*13+13*13*2)]

MR <- findEstComp("sdsAdultParMT","MKPT",adultODS10K,c(1:100),c(1,2,3,5,10,20,50,100),100,5,smts)
MR2 <- findEstComp("sdsAdultParMT","MKPT",adultODS10K,c(1:100),c(1,2,3,5,10,20,50,100),100,9,smts2)
MR3 <- do.call(cbind,c(MR,MR2))
MRPT <- MR3[,c(c(1:13)+c(0:12)*13,c(1,9:11)+c(0,8:10)*13+13*13)]

MR <- findEstComp("sdsAdultDecMT","MKDT",adultODS10K,c(1:1000),c(1,2,3,5,10,20,50,100),100,5,smts)
MR2 <- findEstComp("sdsAdultDecMT","MKDT",adultODS10K,c(1:1000),c(1,2,3,5,10,20,50,100),100,9,smts2)
MR3 <- do.call(cbind,c(MR,MR2))
MRDT <- MR3[,c(c(1:13)+c(0:12)*13,c(1,9:11)+c(0,8:10)*13+13*13)]

save(list = pasteC(list("MR",c("P","D"),"",c("","T"))),file = "MK/MRF.RData")
rm(list = pasteC(list("sdsAdult",c("Par","Dec"),"M",c("","T"))))
rm(list = pasteC(list("MK",c("P","D"),"",c("","T"))))

load("sdsAdultMH.RData")
load("MK/MKHF.RData")

MR <- findEstComp("sdsAdultParMH","MKPH",adultODS10K,c(1:100),c(1,2,3,5,10,20,50,100),100,5,smts)
MR2 <- findEstComp("sdsAdultParMH","MKPH",adultODS10K,c(1:100),c(1,2,3,5,10,20,50,100),100,9,smts2)
MR3 <- do.call(cbind,c(MR,MR2))
MRPH <- MR3[,c(c(1:13)+c(0:12)*13,c(1,9:11)+c(0,8:10)*13+13*13)]

MR <- findEstComp("sdsAdultDecMH","MKDH",adultODS10K,c(1:1000),c(1,2,3,5,10,20,50,100),100,5,smts)
MR2 <- findEstComp("sdsAdultDecMH","MKDH",adultODS10K,c(1:1000),c(1,2,3,5,10,20,50,100),100,9,smts2)
MR3 <- do.call(cbind,c(MR,MR2))
MRDH <- MR3[,c(c(1:13)+c(0:12)*13,c(1,9:11)+c(0,8:10)*13+13*13)]

MR <- findEstComp("sdsAdultParMHT","MKPHT",adultODS10K,c(1:100),c(1,2,3,5,10,20,50,100),100,5,smts)
MR2 <- findEstComp("sdsAdultParMHT","MKPHT",adultODS10K,c(1:100),c(1,2,3,5,10,20,50,100),100,9,smts2)
MR3 <- do.call(cbind,c(MR,MR2))
MRPHT <- MR3[,c(c(1:13)+c(0:12)*13,c(1,9:11)+c(0,8:10)*13+13*13)]

MR <- findEstComp("sdsAdultDecMHT","MKDHT",adultODS10K,c(1:1000),c(1,2,3,5,10,20,50,100),100,5,smts)
MR2 <- findEstComp("sdsAdultDecMHT","MKDHT",adultODS10K,c(1:1000),c(1,2,3,5,10,20,50,100),100,9,smts2)
MR3 <- do.call(cbind,c(MR,MR2))
MRDHT <- MR3[,c(c(1:13)+c(0:12)*13,c(1,9:11)+c(0,8:10)*13+13*13)]

save(list = pasteC(list("MR",c("P","D"),"H",c("","T"))),file = "MK/MRHF.RData")
rm(list = pasteC(list("sdsAdult",c("Par","Dec"),"MH",c("","T"))))
rm(list = pasteC(list("MK",c("P","D"),"H",c("","T"))))

load("sdsAdultML.RData")
load("MK/MKLF.RData")

MR <- findEstComp("sdsAdultParML","MKPL",adultODS10K,c(1:100),c(1,2,3,5,10,20,50,100),100,5,smts)
MR2 <- findEstComp("sdsAdultParML","MKPL",adultODS10K,c(1:100),c(1,2,3,5,10,20,50,100),100,9,smts2)
MR3 <- do.call(cbind,c(MR,MR2))
MRPL <- MR3[,c(c(1:13)+c(0:12)*13,c(1,9:11)+c(0,8:10)*13+13*13)]

MR <- findEstComp("sdsAdultDecML","MKDL",adultODS10K,c(1:1000),c(1,2,3,5,10,20,50,100),100,5,smts)
MR2 <- findEstComp("sdsAdultDecML","MKDL",adultODS10K,c(1:1000),c(1,2,3,5,10,20,50,100),100,9,smts2)
MR3 <- do.call(cbind,c(MR,MR2))
MRDL <- MR3[,c(c(1:13)+c(0:12)*13,c(1,9:11)+c(0,8:10)*13+13*13)]

MR <- findEstComp("sdsAdultParMLT","MKPLT",adultODS10K,c(1:100),c(1,2,3,5,10,20,50,100),100,5,smts)
MR2 <- findEstComp("sdsAdultParMLT","MKPLT",adultODS10K,c(1:100),c(1,2,3,5,10,20,50,100),100,9,smts2)
MR3 <- do.call(cbind,c(MR,MR2))
MRPLT <- MR3[,c(c(1:13)+c(0:12)*13,c(1,9:11)+c(0,8:10)*13+13*13)]

MR <- findEstComp("sdsAdultDecMLT","MKDLT",adultODS10K,c(1:1000),c(1,2,3,5,10,20,50,100),100,5,smts)
MR2 <- findEstComp("sdsAdultDecMLT","MKDLT",adultODS10K,c(1:1000),c(1,2,3,5,10,20,50,100),100,9,smts2)
MR3 <- do.call(cbind,c(MR,MR2))
MRDLT <- MR3[,c(c(1:13)+c(0:12)*13,c(1,9:11)+c(0,8:10)*13+13*13)]

save(list = pasteC(list("MR",c("P","D"),"L",c("","T"))),file = "MK/MRLF.RData")
rm(list = pasteC(list("sdsAdult",c("Par","Dec"),"ML",c("","T"))))
rm(list = pasteC(list("MK",c("P","D"),"L",c("","T"))))

load("sdsAdultMS.RData")
load("MK/MKSF.RData")

MR <- findEstComp("sdsAdultCatMP","MKCP",adultODS10K,c(1:1000),c(1,2,3,5,10,20,50,100),100,5,smts)
MR2 <- findEstComp("sdsAdultCatMP","MKCP",adultODS10K,c(1:1000),c(1,2,3,5,10,20,50,100),100,9,smts2)
MR3 <- do.call(cbind,c(MR,MR2))
MRCP <- MR3[,c(c(1:13)+c(0:12)*13,c(1,9:11)+c(0,8:10)*13+13*13)]

MR <- findEstComp("sdsAdultCatMPT","MKCPT",adultODS10K,c(1:1000),c(1,2,3,5,10,20,50,100),100,5,smts)
MR2 <- findEstComp("sdsAdultCatMPT","MKCPT",adultODS10K,c(1:1000),c(1,2,3,5,10,20,50,100),100,9,smts2)
MR3 <- do.call(cbind,c(MR,MR2))
MRCPT <- MR3[,c(c(1:13)+c(0:12)*13,c(1,9:11)+c(0,8:10)*13+13*13)]

MR <- findEstComp("sdsAdultCatMC","MKCC",adultODS10K,c(1:1000),c(1,2,3,5,10,20,50,100),100,5,smts)
MR2 <- findEstComp("sdsAdultCatMC","MKCC",adultODS10K,c(1:1000),c(1,2,3,5,10,20,50,100),100,9,smts2)
MR3 <- do.call(cbind,c(MR,MR2))
MRCC <- MR3[,c(c(1:13)+c(0:12)*13,c(1,9:11)+c(0,8:10)*13+13*13)]

MR <- findEstComp("sdsAdultCatMCT","MKCCT",adultODS10K,c(1:1000),c(1,2,3,5,10,20,50,100),100,5,smts)
MR2 <- findEstComp("sdsAdultCatMCT","MKCCT",adultODS10K,c(1:1000),c(1,2,3,5,10,20,50,100),100,9,smts2)
MR3 <- do.call(cbind,c(MR,MR2))
MRCCT <- MR3[,c(c(1:13)+c(0:12)*13,c(1,9:11)+c(0,8:10)*13+13*13)]

save(list = pasteC(list("MRC",c("P","C"),"",c("","T"))),file = "MK/MRSF.RData")
rm(list = pasteC(list("sdsAdultCatM",c("P","C"),c("","T"))))
rm(list = pasteC(list("MKC",c("P","C"),c("","T"))))

## Smooth

load("sdsAdultMSm.RData")
load("MK/MKSmF.RData")

MR <- findEstComp("sdsAdultDecMS","MKDS",adultODS10K,c(1:1000),c(1,2,3,5,10,20,50,100),100,5,smts)
MR2 <- findEstComp("sdsAdultDecMS","MKDS",adultODS10K,c(1:1000),c(1,2,3,5,10,20,50,100),100,9,smts2)
MR3 <- do.call(cbind,c(MR,MR2))
MRDS <- MR3[,c(c(1:13)+c(0:12)*13,c(1,9:11)+c(0,8:10)*13+13*13)]

MR <- findEstComp("sdsAdultDecMTS","MKDTS",adultODS10K,c(1:1000),c(1,2,3,5,10,20,50,100),100,5,smts)
MR2 <- findEstComp("sdsAdultDecMTS","MKDTS",adultODS10K,c(1:1000),c(1,2,3,5,10,20,50,100),100,9,smts2)
MR3 <- do.call(cbind,c(MR,MR2))
MRDTS <- MR3[,c(c(1:13)+c(0:12)*13,c(1,9:11)+c(0,8:10)*13+13*13)]

save(list = pasteC(list("MR",c("D"),"",c("","T"),"S")),file = "MK/MRSmF.RData")
rm(list = pasteC(list("sdsAdult",c("Dec"),"M",c("","T"),"S")))
rm(list = pasteC(list("MK",c("D"),"",c("","T"),"S")))

load("sdsAdultMHSm.RData")
load("MK/MKHSmF.RData")

MR <- findEstComp("sdsAdultDecMHS","MKDHS",adultODS10K,c(1:1000),c(1,2,3,5,10,20,50,100),100,5,smts)
MR2 <- findEstComp("sdsAdultDecMHS","MKDHS",adultODS10K,c(1:1000),c(1,2,3,5,10,20,50,100),100,9,smts2)
MR3 <- do.call(cbind,c(MR,MR2))
MRDHS <- MR3[,c(c(1:13)+c(0:12)*13,c(1,9:11)+c(0,8:10)*13+13*13)]

MR <- findEstComp("sdsAdultDecMHTS","MKDHTS",adultODS10K,c(1:1000),c(1,2,3,5,10,20,50,100),100,5,smts)
MR2 <- findEstComp("sdsAdultDecMHTS","MKDHTS",adultODS10K,c(1:1000),c(1,2,3,5,10,20,50,100),100,9,smts2)
MR3 <- do.call(cbind,c(MR,MR2))
MRDHTS <- MR3[,c(c(1:13)+c(0:12)*13,c(1,9:11)+c(0,8:10)*13+13*13)]

save(list = pasteC(list("MR",c("D"),"H",c("","T"),"S")),file = "MK/MRHSmF.RData")
rm(list = pasteC(list("sdsAdult",c("Dec"),"MH",c("","T"),"S")))
rm(list = pasteC(list("MK",c("D"),"H",c("","T"),"S")))

load("sdsAdultMLSm.RData")
load("MK/MKLSmF.RData")

MR <- findEstComp("sdsAdultDecMLS","MKDLS",adultODS10K,c(1:1000),c(1,2,3,5,10,20,50,100),100,5,smts)
MR2 <- findEstComp("sdsAdultDecMLS","MKDLS",adultODS10K,c(1:1000),c(1,2,3,5,10,20,50,100),100,9,smts2)
MR3 <- do.call(cbind,c(MR,MR2))
MRDLS <- MR3[,c(c(1:13)+c(0:12)*13,c(1,9:11)+c(0,8:10)*13+13*13)]

MR <- findEstComp("sdsAdultDecMLTS","MKDLTS",adultODS10K,c(1:1000),c(1,2,3,5,10,20,50,100),100,5,smts)
MR2 <- findEstComp("sdsAdultDecMLTS","MKDLTS",adultODS10K,c(1:1000),c(1,2,3,5,10,20,50,100),100,9,smts2)
MR3 <- do.call(cbind,c(MR,MR2))
MRDLTS <- MR3[,c(c(1:13)+c(0:12)*13,c(1,9:11)+c(0,8:10)*13+13*13)]

save(list = pasteC(list("MR",c("D"),"L",c("","T"),"S")),file = "MK/MRLSmF.RData")
rm(list = pasteC(list("sdsAdult",c("Dec"),"ML",c("","T"),"S")))
rm(list = pasteC(list("MK",c("D"),"L",c("","T"),"S")))

load("sdsAdultMSSm.RData")
load("MK/MKSSmF.RData")

MR <- findEstComp("sdsAdultCatMPS","MKCPS",adultODS10K,c(1:1000),c(1,2,3,5,10,20,50,100),100,5,smts)
MR2 <- findEstComp("sdsAdultCatMPS","MKCPS",adultODS10K,c(1:1000),c(1,2,3,5,10,20,50,100),100,9,smts2)
MR3 <- do.call(cbind,c(MR,MR2))
MRCPS <- MR3[,c(c(1:13)+c(0:12)*13,c(1,9:11)+c(0,8:10)*13+13*13)]

MR <- findEstComp("sdsAdultCatMPTS","MKCPTS",adultODS10K,c(1:1000),c(1,2,3,5,10,20,50,100),100,5,smts)
MR2 <- findEstComp("sdsAdultCatMPTS","MKCPTS",adultODS10K,c(1:1000),c(1,2,3,5,10,20,50,100),100,9,smts2)
MR3 <- do.call(cbind,c(MR,MR2))
MRCPTS <- MR3[,c(c(1:13)+c(0:12)*13,c(1,9:11)+c(0,8:10)*13+13*13)]

MR <- findEstComp("sdsAdultCatMCS","MKCCS",adultODS10K,c(1:1000),c(1,2,3,5,10,20,50,100),100,5,smts)
MR2 <- findEstComp("sdsAdultCatMCS","MKCCS",adultODS10K,c(1:1000),c(1,2,3,5,10,20,50,100),100,9,smts2)
MR3 <- do.call(cbind,c(MR,MR2))
MRCCS <- MR3[,c(c(1:13)+c(0:12)*13,c(1,9:11)+c(0,8:10)*13+13*13)]

MR <- findEstComp("sdsAdultCatMCTS","MKCCTS",adultODS10K,c(1:1000),c(1,2,3,5,10,20,50,100),100,5,smts)
MR2 <- findEstComp("sdsAdultCatMCTS","MKCCTS",adultODS10K,c(1:1000),c(1,2,3,5,10,20,50,100),100,9,smts2)
MR3 <- do.call(cbind,c(MR,MR2))
MRCCTS <- MR3[,c(c(1:13)+c(0:12)*13,c(1,9:11)+c(0,8:10)*13+13*13)]

save(list = pasteC(list("MRC",c("P","C"),"",c("","T"),"S")),file = "MK/MRSSmF.RData")
rm(list = pasteC(list("sdsAdultCatM",c("P","C"),c("","T"),"S")))
rm(list = pasteC(list("MKC",c("P","C"),c("","T"),"S")))

## Selective

load("sdsAdultMSe.RData")
load("MK/MKSeF.RData")

MR <- findEstComp("sdsAdultParMSe","MKPSe",adultODS10K,c(1:100),c(1,2,3,5,10,20,50,100),100,5,smts)
MR2 <- findEstComp("sdsAdultParMSe","MKPSe",adultODS10K,c(1:100),c(1,2,3,5,10,20,50,100),100,9,smts2)
MR3 <- do.call(cbind,c(MR,MR2))
MRPSe <- MR3[,c(c(1:13)+c(0:12)*13,c(1,9:11)+c(0,8:10)*13+13*13)]

MR <- findEstComp("sdsAdultDecMSe","MKDSe",adultODS10K,c(1:1000),c(1,2,3,5,10,20,50,100),100,5,smts)
MR2 <- findEstComp("sdsAdultDecMSe","MKDSe",adultODS10K,c(1:1000),c(1,2,3,5,10,20,50,100),100,9,smts2)
MR3 <- do.call(cbind,c(MR,MR2))
MRDSe <- MR3[,c(c(1:13)+c(0:12)*13,c(1,9:11)+c(0,8:10)*13+13*13)]

MR <- findEstComp("sdsAdultParMTSe","MKPTSe",adultODS10K,c(1:100),c(1,2,3,5,10,20,50,100),100,5,smts)
MR2 <- findEstComp("sdsAdultParMTSe","MKPTSe",adultODS10K,c(1:100),c(1,2,3,5,10,20,50,100),100,9,smts2)
MR3 <- do.call(cbind,c(MR,MR2))
MRPTSe <- MR3[,c(c(1:13)+c(0:12)*13,c(1,9:11)+c(0,8:10)*13+13*13)]

MR <- findEstComp("sdsAdultDecMTSe","MKDTSe",adultODS10K,c(1:1000),c(1,2,3,5,10,20,50,100),100,5,smts)
MR2 <- findEstComp("sdsAdultDecMTSe","MKDTSe",adultODS10K,c(1:1000),c(1,2,3,5,10,20,50,100),100,9,smts2)
MR3 <- do.call(cbind,c(MR,MR2))
MRDTSe <- MR3[,c(c(1:13)+c(0:12)*13,c(1,9:11)+c(0,8:10)*13+13*13)]

save(list = pasteC(list("MR",c("P","D"),"",c("","T"),"Se")),file = "MK/MRSeF.RData")
rm(list = pasteC(list("sdsAdult",c("Par","Dec"),"M",c("","T"),"Se")))
rm(list = pasteC(list("MK",c("P","D"),"",c("","T"),"Se")))

load("sdsAdultMHSe.RData")
load("MK/MKHSeF.RData")

MR <- findEstComp("sdsAdultParMHSe","MKPHSe",adultODS10K,c(1:100),c(1,2,3,5,10,20,50,100),100,5,smts)
MR2 <- findEstComp("sdsAdultParMHSe","MKPHSe",adultODS10K,c(1:100),c(1,2,3,5,10,20,50,100),100,9,smts2)
MR3 <- do.call(cbind,c(MR,MR2))
MRPHSe <- MR3[,c(c(1:13)+c(0:12)*13,c(1,9:11)+c(0,8:10)*13+13*13)]

MR <- findEstComp("sdsAdultDecMHSe","MKDHSe",adultODS10K,c(1:1000),c(1,2,3,5,10,20,50,100),100,5,smts)
MR2 <- findEstComp("sdsAdultDecMHSe","MKDHSe",adultODS10K,c(1:1000),c(1,2,3,5,10,20,50,100),100,9,smts2)
MR3 <- do.call(cbind,c(MR,MR2))
MRDHSe <- MR3[,c(c(1:13)+c(0:12)*13,c(1,9:11)+c(0,8:10)*13+13*13)]

MR <- findEstComp("sdsAdultParMHTSe","MKPHTSe",adultODS10K,c(1:100),c(1,2,3,5,10,20,50,100),100,5,smts)
MR2 <- findEstComp("sdsAdultParMHTSe","MKPHTSe",adultODS10K,c(1:100),c(1,2,3,5,10,20,50,100),100,9,smts2)
MR3 <- do.call(cbind,c(MR,MR2))
MRPHTSe <- MR3[,c(c(1:13)+c(0:12)*13,c(1,9:11)+c(0,8:10)*13+13*13)]

MR <- findEstComp("sdsAdultDecMHTSe","MKDHTSe",adultODS10K,c(1:1000),c(1,2,3,5,10,20,50,100),100,5,smts)
MR2 <- findEstComp("sdsAdultDecMHTSe","MKDHTSe",adultODS10K,c(1:1000),c(1,2,3,5,10,20,50,100),100,9,smts2)
MR3 <- do.call(cbind,c(MR,MR2))
MRDHTSe <- MR3[,c(c(1:13)+c(0:12)*13,c(1,9:11)+c(0,8:10)*13+13*13)]

save(list = pasteC(list("MR",c("P","D"),"H",c("","T"),"Se")),file = "MK/MRHSeF.RData")
rm(list = pasteC(list("sdsAdult",c("Par","Dec"),"MH",c("","T"),"Se")))
rm(list = pasteC(list("MK",c("P","D"),"H",c("","T"),"Se")))

load("sdsAdultMLSe.RData")
load("MK/MKLSeF.RData")

MR <- findEstComp("sdsAdultParMLSe","MKPLSe",adultODS10K,c(1:100),c(1,2,3,5,10,20,50,100),100,5,smts)
MR2 <- findEstComp("sdsAdultParMLSe","MKPLSe",adultODS10K,c(1:100),c(1,2,3,5,10,20,50,100),100,9,smts2)
MR3 <- do.call(cbind,c(MR,MR2))
MRPLSe <- MR3[,c(c(1:13)+c(0:12)*13,c(1,9:11)+c(0,8:10)*13+13*13)]

MR <- findEstComp("sdsAdultDecMLSe","MKDLSe",adultODS10K,c(1:1000),c(1,2,3,5,10,20,50,100),100,5,smts)
MR2 <- findEstComp("sdsAdultDecMLSe","MKDLSe",adultODS10K,c(1:1000),c(1,2,3,5,10,20,50,100),100,9,smts2)
MR3 <- do.call(cbind,c(MR,MR2))
MRDLSe <- MR3[,c(c(1:13)+c(0:12)*13,c(1,9:11)+c(0,8:10)*13+13*13)]

MR <- findEstComp("sdsAdultParMLTSe","MKPLTSe",adultODS10K,c(1:100),c(1,2,3,5,10,20,50,100),100,5,smts)
MR2 <- findEstComp("sdsAdultParMLTSe","MKPLTSe",adultODS10K,c(1:100),c(1,2,3,5,10,20,50,100),100,9,smts2)
MR3 <- do.call(cbind,c(MR,MR2))
MRPLTSe <- MR3[,c(c(1:13)+c(0:12)*13,c(1,9:11)+c(0,8:10)*13+13*13)]

MR <- findEstComp("sdsAdultDecMLTSe","MKDLTSe",adultODS10K,c(1:1000),c(1,2,3,5,10,20,50,100),100,5,smts)
MR2 <- findEstComp("sdsAdultDecMLTSe","MKDLTSe",adultODS10K,c(1:1000),c(1,2,3,5,10,20,50,100),100,9,smts2)
MR3 <- do.call(cbind,c(MR,MR2))
MRDLTSe <- MR3[,c(c(1:13)+c(0:12)*13,c(1,9:11)+c(0,8:10)*13+13*13)]

save(list = pasteC(list("MR",c("P","D"),"L",c("","T"),"Se")),file = "MK/MRLSeF.RData")
rm(list = pasteC(list("sdsAdult",c("Par","Dec"),"ML",c("","T"),"Se")))
rm(list = pasteC(list("MK",c("P","D"),"L",c("","T"),"Se")))

load("sdsAdultMSSe.RData")
load("MK/MKSSeF.RData")

MR <- findEstComp("sdsAdultCatMPSe","MKCPSe",adultODS10K,c(1:1000),c(1,2,3,5,10,20,50,100),100,5,smts)
MR2 <- findEstComp("sdsAdultCatMPSe","MKCPSe",adultODS10K,c(1:1000),c(1,2,3,5,10,20,50,100),100,9,smts2)
MR3 <- do.call(cbind,c(MR,MR2))
MRCPSe <- MR3[,c(c(1:13)+c(0:12)*13,c(1,9:11)+c(0,8:10)*13+13*13)]

MR <- findEstComp("sdsAdultCatMPTSe","MKCPTSe",adultODS10K,c(1:1000),c(1,2,3,5,10,20,50,100),100,5,smts)
MR2 <- findEstComp("sdsAdultCatMPTSe","MKCPTSe",adultODS10K,c(1:1000),c(1,2,3,5,10,20,50,100),100,9,smts2)
MR3 <- do.call(cbind,c(MR,MR2))
MRCPTSe <- MR3[,c(c(1:13)+c(0:12)*13,c(1,9:11)+c(0,8:10)*13+13*13)]

MR <- findEstComp("sdsAdultCatMCSe","MKCCSe",adultODS10K,c(1:1000),c(1,2,3,5,10,20,50,100),100,5,smts)
MR2 <- findEstComp("sdsAdultCatMCSe","MKCCSe",adultODS10K,c(1:1000),c(1,2,3,5,10,20,50,100),100,9,smts2)
MR3 <- do.call(cbind,c(MR,MR2))
MRCCSe <- MR3[,c(c(1:13)+c(0:12)*13,c(1,9:11)+c(0,8:10)*13+13*13)]

MR <- findEstComp("sdsAdultCatMCTSe","MKCCTSe",adultODS10K,c(1:1000),c(1,2,3,5,10,20,50,100),100,5,smts)
MR2 <- findEstComp("sdsAdultCatMCTSe","MKCCTSe",adultODS10K,c(1:1000),c(1,2,3,5,10,20,50,100),100,9,smts2)
MR3 <- do.call(cbind,c(MR,MR2))
MRCCTSe <- MR3[,c(c(1:13)+c(0:12)*13,c(1,9:11)+c(0,8:10)*13+13*13)]

save(list = pasteC(list("MRC",c("P","C"),"",c("","T"),"Se")),file = "MK/MRSSeF.RData")
rm(list = pasteC(list("sdsAdultCatM",c("P","C"),c("","T"),"Se")))
rm(list = pasteC(list("MKC",c("P","C"),c("","T"),"Se")))

## Polish and Avila

smt <- sapply(ods[c(1:2500),],function(x) length(levels(x)))
tm <- rep.int(-1,times = 14)
smtp <- sapply(c(1:14),function(x) tm)
smtp[c(1:14) + c(0:13)*14] <- smt
smts <- sapply(c(1:14),function(x) list(smtp[x,]))
smts2 <- smts
smts2[c(1,3,4,6,7,9:12)] <- lapply(smts2[c(1,3,4,6,7,9:12)],function(x) tm)


load("sdsPolishMH.RData")
load("MK/MKPF.RData")

MR <- findEstComp("sdsPolishParMH","MKPP",ods[c(1:2500),],c(1:1000),c(1,2,3,5,10,20,50,100),100,5,smts)
MR2 <- findEstComp("sdsPolishParMH","MKPP",ods[c(1:2500),],c(1:1000),c(1,2,3,5,10,20,50,100),100,9,smts2)
MR3 <- do.call(cbind,c(MR,MR2))
MRPP <- MR3[,c(c(1:14)+c(0:13)*14,c(2,5,8,13,14)+c(1,4,7,12,13)*14+14*14)]

MR <- findEstComp("sdsPolishDecMH","MKPD",ods[c(1:2500),],c(1:1000),c(1,2,3,5,10,20,50,100),100,5,smts)
MR2 <- findEstComp("sdsPolishDecMH","MKPD",ods[c(1:2500),],c(1:1000),c(1,2,3,5,10,20,50,100),100,9,smts2)
MR3 <- do.call(cbind,c(MR,MR2))
MRPD <- MR3[,c(c(1:14)+c(0:13)*14,c(2,5,8,13,14)+c(1,4,7,12,13)*14+14*14)]

MR <- findEstComp("sdsPolishParMTH","MKPPT",ods[c(1:2500),],c(1:1000),c(1,2,3,5,10,20,50,100),100,5,smts)
MR2 <- findEstComp("sdsPolishParMTH","MKPPT",ods[c(1:2500),],c(1:1000),c(1,2,3,5,10,20,50,100),100,9,smts2)
MR3 <- do.call(cbind,c(MR,MR2))
MRPPT <- MR3[,c(c(1:14)+c(0:13)*14,c(2,5,8,13,14)+c(1,4,7,12,13)*14+14*14)]

MR <- findEstComp("sdsPolishDecMTH","MKPDT",ods[c(1:2500),],c(1:1000),c(1,2,3,5,10,20,50,100),100,5,smts)
MR2 <- findEstComp("sdsPolishDecMTH","MKPDT",ods[c(1:2500),],c(1:1000),c(1,2,3,5,10,20,50,100),100,9,smts2)
MR3 <- do.call(cbind,c(MR,MR2))
MRPDT <- MR3[,c(c(1:14)+c(0:13)*14,c(2,5,8,13,14)+c(1,4,7,12,13)*14+14*14)]

save(list = pasteC(list("MRP",c("P","D"),"",c("","T"))),file = "MK/MRPF.RData")
rm(list = pasteC(list("sdsPolish",c("Par","Dec"),"M",c("","T"),"H")))
rm(list = pasteC(list("MKP",c("P","D"),"",c("","T"))))


load("sdsPolishMSH.RData")
load("MK/MKSPF.RData")

MR <- findEstComp("sdsPolishCatMPH","MKPCP",ods[c(1:2500),],c(1:1000),c(1,2,3,5,10,20,50,100),100,5,smts)
MR2 <- findEstComp("sdsPolishCatMPH","MKPCP",ods[c(1:2500),],c(1:1000),c(1,2,3,5,10,20,50,100),100,9,smts2)
MR3 <- do.call(cbind,c(MR,MR2))
MRPCP <- MR3[,c(c(1:14)+c(0:13)*14,c(2,5,8,13,14)+c(1,4,7,12,13)*14+14*14)]

MR <- findEstComp("sdsPolishCatMPTH","MKPCPT",ods[c(1:2500),],c(1:1000),c(1,2,3,5,10,20,50,100),100,5,smts)
MR2 <- findEstComp("sdsPolishCatMPTH","MKPCPT",ods[c(1:2500),],c(1:1000),c(1,2,3,5,10,20,50,100),100,9,smts2)
MR3 <- do.call(cbind,c(MR,MR2))
MRPCPT <- MR3[,c(c(1:14)+c(0:13)*14,c(2,5,8,13,14)+c(1,4,7,12,13)*14+14*14)]

MR <- findEstComp("sdsPolishCatMCH","MKPCC",ods[c(1:2500),],c(1:1000),c(1,2,3,5,10,20,50,100),100,5,smts)
MR2 <- findEstComp("sdsPolishCatMCH","MKPCC",ods[c(1:2500),],c(1:1000),c(1,2,3,5,10,20,50,100),100,9,smts2)
MR3 <- do.call(cbind,c(MR,MR2))
MRPCC <- MR3[,c(c(1:14)+c(0:13)*14,c(2,5,8,13,14)+c(1,4,7,12,13)*14+14*14)]

MR <- findEstComp("sdsPolishCatMCTH","MKPCCT",ods[c(1:2500),],c(1:1000),c(1,2,3,5,10,20,50,100),100,5,smts)
MR2 <- findEstComp("sdsPolishCatMCTH","MKPCCT",ods[c(1:2500),],c(1:1000),c(1,2,3,5,10,20,50,100),100,9,smts2)
MR3 <- do.call(cbind,c(MR,MR2))
MRPCCT <- MR3[,c(c(1:14)+c(0:13)*14,c(2,5,8,13,14)+c(1,4,7,12,13)*14+14*14)]

save(list = pasteC(list("MRPC",c("P","C"),"",c("","T"))),file = "MK/MRSPF.RData")
rm(list = pasteC(list("sdsPolishCatM",c("P","C"),"",c("","T"),"H")))
rm(list = pasteC(list("MKPC",c("P","C"),"",c("","T"))))



smt <- sapply(avilaODS[c(1:5215),],function(x) length(levels(x)))
tm <- rep.int(-1,times = 11)
smtp <- sapply(c(1:11),function(x) tm)
smtp[c(1:11) + c(0:10)*11] <- smt
smts <- sapply(c(1:11),function(x) list(smtp[x,]))
smts2 <- smts
smts2[c(11)] <- lapply(smts2[c(11)],function(x) tm)

load("sdsAvilaMH.RData")
load("MK/MKAF.RData")

MR <- findEstComp("sdsAvilaParMH","MKAP",avilaODS[c(1:5215),],c(1:1000),c(1,2,3,5,10,20,50,100),100,5,smts)
MR2 <- findEstComp("sdsAvilaParMH","MKAP",avilaODS[c(1:5215),],c(1:1000),c(1,2,3,5,10,20,50,100),100,9,smts2)
MR3 <- do.call(cbind,c(MR,MR2))
MRAP <- MR3[,c(c(1:11)+c(0:10)*11,c(1:10)+c(0:9)*11+11*11)]

MR <- findEstComp("sdsAvilaDecMH","MKAD",avilaODS[c(1:5215),],c(1:1000),c(1,2,3,5,10,20,50,100),100,5,smts)
MR2 <- findEstComp("sdsAvilaDecMH","MKAD",avilaODS[c(1:5215),],c(1:1000),c(1,2,3,5,10,20,50,100),100,9,smts2)
MR3 <- do.call(cbind,c(MR,MR2))
MRAD <- MR3[,c(c(1:11)+c(0:10)*11,c(1:10)+c(0:9)*11+11*11)]

MR <- findEstComp("sdsAvilaParMTH","MKAPT",avilaODS[c(1:5215),],c(1:1000),c(1,2,3,5,10,20,50,100),100,5,smts)
MR2 <- findEstComp("sdsAvilaParMTH","MKAPT",avilaODS[c(1:5215),],c(1:1000),c(1,2,3,5,10,20,50,100),100,9,smts2)
MR3 <- do.call(cbind,c(MR,MR2))
MRAPT <- MR3[,c(c(1:11)+c(0:10)*11,c(1:10)+c(0:9)*11+11*11)]

MR <- findEstComp("sdsAvilaDecMTH","MKADT",avilaODS[c(1:5215),],c(1:1000),c(1,2,3,5,10,20,50,100),100,5,smts)
MR2 <- findEstComp("sdsAvilaDecMTH","MKADT",avilaODS[c(1:5215),],c(1:1000),c(1,2,3,5,10,20,50,100),100,9,smts2)
MR3 <- do.call(cbind,c(MR,MR2))
MRADT <- MR3[,c(c(1:11)+c(0:10)*11,c(1:10)+c(0:9)*11+11*11)]

save(list = pasteC(list("MRA",c("P","D"),"",c("","T"))),file = "MK/MRAF.RData")
rm(list = pasteC(list("sdsAvila",c("Par","Dec"),"M",c("","T"),"H")))
rm(list = pasteC(list("MKA",c("P","D"),"",c("","T"))))



load("sdsAvilaMSH.RData")
load("MK/MKSAF.RData")

MR <- findEstComp("sdsAvilaCatMPH","MKACP",avilaODS[c(1:5215),],c(1:1000),c(1,2,3,5,10,20,50,100),100,5,smts)
MR2 <- findEstComp("sdsAvilaCatMPH","MKACP",avilaODS[c(1:5215),],c(1:1000),c(1,2,3,5,10,20,50,100),100,9,smts2)
MR3 <- do.call(cbind,c(MR,MR2))
MRACP <- MR3[,c(c(1:11)+c(0:10)*11,c(1:10)+c(0:9)*11+11*11)]

MR <- findEstComp("sdsAvilaCatMPTH","MKACPT",avilaODS[c(1:5215),],c(1:1000),c(1,2,3,5,10,20,50,100),100,5,smts)
MR2 <- findEstComp("sdsAvilaCatMPTH","MKACPT",avilaODS[c(1:5215),],c(1:1000),c(1,2,3,5,10,20,50,100),100,9,smts2)
MR3 <- do.call(cbind,c(MR,MR2))
MRACPT <- MR3[,c(c(1:11)+c(0:10)*11,c(1:10)+c(0:9)*11+11*11)]

MR <- findEstComp("sdsAvilaCatMCH","MKACC",avilaODS[c(1:5215),],c(1:1000),c(1,2,3,5,10,20,50,100),100,5,smts)
MR2 <- findEstComp("sdsAvilaCatMCH","MKACC",avilaODS[c(1:5215),],c(1:1000),c(1,2,3,5,10,20,50,100),100,9,smts2)
MR3 <- do.call(cbind,c(MR,MR2))
MRACC <- MR3[,c(c(1:11)+c(0:10)*11,c(1:10)+c(0:9)*11+11*11)]

MR <- findEstComp("sdsAvilaCatMCTH","MKACCT",avilaODS[c(1:5215),],c(1:1000),c(1,2,3,5,10,20,50,100),100,5,smts)
MR2 <- findEstComp("sdsAvilaCatMCTH","MKACCT",avilaODS[c(1:5215),],c(1:1000),c(1,2,3,5,10,20,50,100),100,9,smts2)
MR3 <- do.call(cbind,c(MR,MR2))
MRACCT <- MR3[,c(c(1:11)+c(0:10)*11,c(1:10)+c(0:9)*11+11*11)]

save(list = pasteC(list("MRAC",c("P","C"),"",c("","T"))),file = "MK/MRSAF.RData")
rm(list = pasteC(list("sdsAvilaCatM",c("P","C"),"",c("","T"),"H")))
rm(list = pasteC(list("MKAC",c("P","C"),"",c("","T"))))


## Final Results Extra

smt <- sapply(adultODS10K,function(x) length(levels(x)))
tm <- rep.int(1,times = 13)
smtp <- sapply(c(1:13),function(x) tm)
smtp[c(1:13) + c(0:12)*13] <- smt
smts <- sapply(c(1:13),function(x) list(smtp[x,]))
smts2 <- smts
smts2[c(2:8,12,13)] <- lapply(smts2[c(2:8,12,13)],function(x) tm)
smtp3 <- smtp
smtp3[c(1:13) + c(0:12)*13] <- rep.int(-1,times = 13)
smts3 <- sapply(c(1:13),function(x) list(smtp3[x,]))
smts3[c(2:8,12,13)] <- lapply(smts3[c(2:8,12,13)],function(x) tm)

load("sdsAdultM.RData")
load("MK/MKF.RData")

MR <- findEstComp("sdsAdultParM","MKP",adultODS10K,c(1:100),c(1,2,3,5,10,20,50,100),100,13,smts3)
MR3 <- do.call(cbind,MR)
MRP3 <- MR3[,c(c(1,9:11)+c(0,8:10)*13)]

MR <- findEstComp("sdsAdultDecM","MKD",adultODS10K,c(1:1000),c(1,2,3,5,10,20,50,100),100,13,smts3)
MR3 <- do.call(cbind,MR)
MRD3 <- MR3[,c(c(1,9:11)+c(0,8:10)*13)]

MR <- findEstComp("sdsAdultParMT","MKPT",adultODS10K,c(1:100),c(1,2,3,5,10,20,50,100),100,13,smts3)
MR3 <- do.call(cbind,MR)
MRPT3 <- MR3[,c(c(1,9:11)+c(0,8:10)*13)]

MR <- findEstComp("sdsAdultDecMT","MKDT",adultODS10K,c(1:1000),c(1,2,3,5,10,20,50,100),100,13,smts3)
MR3 <- do.call(cbind,MR)
MRDT3 <- MR3[,c(c(1,9:11)+c(0,8:10)*13)]

save(list = pasteC(list("MR",c("P","D"),"",c("","T"),"3")),file = "MK/MRF3.RData")
rm(list = pasteC(list("sdsAdult",c("Par","Dec"),"M",c("","T"))))
rm(list = pasteC(list("MK",c("P","D"),"",c("","T"))))

load("sdsAdultMH.RData")
load("MK/MKHF.RData")

MR <- findEstComp("sdsAdultParMH","MKPH",adultODS10K,c(1:100),c(1,2,3,5,10,20,50,100),100,13,smts3)
MR3 <- do.call(cbind,MR)
MRPH3 <- MR3[,c(c(1,9:11)+c(0,8:10)*13)]

MR <- findEstComp("sdsAdultDecMH","MKDH",adultODS10K,c(1:1000),c(1,2,3,5,10,20,50,100),100,13,smts3)
MR3 <- do.call(cbind,MR)
MRDH3 <- MR3[,c(c(1,9:11)+c(0,8:10)*13)]

MR <- findEstComp("sdsAdultParMHT","MKPHT",adultODS10K,c(1:100),c(1,2,3,5,10,20,50,100),100,13,smts3)
MR3 <- do.call(cbind,MR)
MRPHT3 <- MR3[,c(c(1,9:11)+c(0,8:10)*13)]

MR <- findEstComp("sdsAdultDecMHT","MKDHT",adultODS10K,c(1:1000),c(1,2,3,5,10,20,50,100),100,13,smts3)
MR3 <- do.call(cbind,MR)
MRDHT3 <- MR3[,c(c(1,9:11)+c(0,8:10)*13)]

save(list = pasteC(list("MR",c("P","D"),"H",c("","T"),"3")),file = "MK/MRHF3.RData")
rm(list = pasteC(list("sdsAdult",c("Par","Dec"),"MH",c("","T"))))
rm(list = pasteC(list("MK",c("P","D"),"H",c("","T"))))

load("sdsAdultML.RData")
load("MK/MKLF.RData")

MR <- findEstComp("sdsAdultParML","MKPL",adultODS10K,c(1:100),c(1,2,3,5,10,20,50,100),100,13,smts3)
MR3 <- do.call(cbind,MR)
MRPL3 <- MR3[,c(c(1,9:11)+c(0,8:10)*13)]

MR <- findEstComp("sdsAdultDecML","MKDL",adultODS10K,c(1:1000),c(1,2,3,5,10,20,50,100),100,13,smts3)
MR3 <- do.call(cbind,MR)
MRDL3 <- MR3[,c(c(1,9:11)+c(0,8:10)*13)]

MR <- findEstComp("sdsAdultParMLT","MKPLT",adultODS10K,c(1:100),c(1,2,3,5,10,20,50,100),100,13,smts3)
MR3 <- do.call(cbind,MR)
MRPLT3 <- MR3[,c(c(1,9:11)+c(0,8:10)*13)]

MR <- findEstComp("sdsAdultDecMLT","MKDLT",adultODS10K,c(1:1000),c(1,2,3,5,10,20,50,100),100,13,smts3)
MR3 <- do.call(cbind,MR)
MRDLT3 <- MR3[,c(c(1,9:11)+c(0,8:10)*13)]

save(list = pasteC(list("MR",c("P","D"),"L",c("","T"),"3")),file = "MK/MRLF3.RData")
rm(list = pasteC(list("sdsAdult",c("Par","Dec"),"ML",c("","T"))))
rm(list = pasteC(list("MK",c("P","D"),"L",c("","T"))))

load("sdsAdultMS.RData")
load("MK/MKSF.RData")

MR <- findEstComp("sdsAdultCatMP","MKCP",adultODS10K,c(1:1000),c(1,2,3,5,10,20,50,100),100,13,smts3)
MR3 <- do.call(cbind,MR)
MRCP3 <- MR3[,c(c(1,9:11)+c(0,8:10)*13)]

MR <- findEstComp("sdsAdultCatMPT","MKCPT",adultODS10K,c(1:1000),c(1,2,3,5,10,20,50,100),100,13,smts3)
MR3 <- do.call(cbind,MR)
MRCPT3 <- MR3[,c(c(1,9:11)+c(0,8:10)*13)]

MR <- findEstComp("sdsAdultCatMC","MKCC",adultODS10K,c(1:1000),c(1,2,3,5,10,20,50,100),100,13,smts3)
MR3 <- do.call(cbind,MR)
MRCC3 <- MR3[,c(c(1,9:11)+c(0,8:10)*13)]

MR <- findEstComp("sdsAdultCatMCT","MKCCT",adultODS10K,c(1:1000),c(1,2,3,5,10,20,50,100),100,13,smts3)
MR3 <- do.call(cbind,MR)
MRCCT3 <- MR3[,c(c(1,9:11)+c(0,8:10)*13)]

save(list = pasteC(list("MRC",c("P","C"),"",c("","T"),"3")),file = "MK/MRSF3.RData")
rm(list = pasteC(list("sdsAdultCatM",c("P","C"),c("","T"))))
rm(list = pasteC(list("MKC",c("P","C"),c("","T"))))

## Smooth

load("sdsAdultMSm.RData")
load("MK/MKSmF.RData")

MR <- findEstComp("sdsAdultDecMS","MKDS",adultODS10K,c(1:1000),c(1,2,3,5,10,20,50,100),100,13,smts3)
MR3 <- do.call(cbind,MR)
MRDS3 <- MR3[,c(c(1,9:11)+c(0,8:10)*13)]

MR <- findEstComp("sdsAdultDecMTS","MKDTS",adultODS10K,c(1:1000),c(1,2,3,5,10,20,50,100),100,13,smts3)
MR3 <- do.call(cbind,MR)
MRDTS3 <- MR3[,c(c(1,9:11)+c(0,8:10)*13)]

save(list = pasteC(list("MR",c("D"),"",c("","T"),"S","3")),file = "MK/MRSmF3.RData")
rm(list = pasteC(list("sdsAdult",c("Dec"),"M",c("","T"),"S")))
rm(list = pasteC(list("MK",c("D"),"",c("","T"),"S")))

load("sdsAdultMHSm.RData")
load("MK/MKHSmF.RData")

MR <- findEstComp("sdsAdultDecMHS","MKDHS",adultODS10K,c(1:1000),c(1,2,3,5,10,20,50,100),100,13,smts3)
MR3 <- do.call(cbind,MR)
MRDHS3 <- MR3[,c(c(1,9:11)+c(0,8:10)*13)]

MR <- findEstComp("sdsAdultDecMHTS","MKDHTS",adultODS10K,c(1:1000),c(1,2,3,5,10,20,50,100),100,13,smts3)
MR3 <- do.call(cbind,MR)
MRDHTS3 <- MR3[,c(c(1,9:11)+c(0,8:10)*13)]

save(list = pasteC(list("MR",c("D"),"H",c("","T"),"S","3")),file = "MK/MRHSmF3.RData")
rm(list = pasteC(list("sdsAdult",c("Dec"),"MH",c("","T"),"S")))
rm(list = pasteC(list("MK",c("D"),"H",c("","T"),"S")))

load("sdsAdultMLSm.RData")
load("MK/MKLSmF.RData")

MR <- findEstComp("sdsAdultDecMLS","MKDLS",adultODS10K,c(1:1000),c(1,2,3,5,10,20,50,100),100,13,smts3)
MR3 <- do.call(cbind,MR)
MRDLS3 <- MR3[,c(c(1,9:11)+c(0,8:10)*13)]

MR <- findEstComp("sdsAdultDecMLTS","MKDLTS",adultODS10K,c(1:1000),c(1,2,3,5,10,20,50,100),100,13,smts3)
MR3 <- do.call(cbind,MR)
MRDLTS3 <- MR3[,c(c(1,9:11)+c(0,8:10)*13)]

save(list = pasteC(list("MR",c("D"),"L",c("","T"),"S","3")),file = "MK/MRLSmF3.RData")
rm(list = pasteC(list("sdsAdult",c("Dec"),"ML",c("","T"),"S")))
rm(list = pasteC(list("MK",c("D"),"L",c("","T"),"S")))

load("sdsAdultMSSm.RData")
load("MK/MKSSmF.RData")

MR <- findEstComp("sdsAdultCatMPS","MKCPS",adultODS10K,c(1:1000),c(1,2,3,5,10,20,50,100),100,13,smts3)
MR3 <- do.call(cbind,MR)
MRCPS3 <- MR3[,c(c(1,9:11)+c(0,8:10)*13)]

MR <- findEstComp("sdsAdultCatMPTS","MKCPTS",adultODS10K,c(1:1000),c(1,2,3,5,10,20,50,100),100,13,smts3)
MR3 <- do.call(cbind,MR)
MRCPTS3 <- MR3[,c(c(1,9:11)+c(0,8:10)*13)]

MR <- findEstComp("sdsAdultCatMCS","MKCCS",adultODS10K,c(1:1000),c(1,2,3,5,10,20,50,100),100,13,smts3)
MR3 <- do.call(cbind,MR)
MRCCS3 <- MR3[,c(c(1,9:11)+c(0,8:10)*13)]

MR <- findEstComp("sdsAdultCatMCTS","MKCCTS",adultODS10K,c(1:1000),c(1,2,3,5,10,20,50,100),100,13,smts3)
MR3 <- do.call(cbind,MR)
MRCCTS3 <- MR3[,c(c(1,9:11)+c(0,8:10)*13)]

save(list = pasteC(list("MRC",c("P","C"),"",c("","T"),"S","3")),file = "MK/MRSSmF3.RData")
rm(list = pasteC(list("sdsAdultCatM",c("P","C"),c("","T"),"S")))
rm(list = pasteC(list("MKC",c("P","C"),c("","T"),"S")))

## Selective

load("sdsAdultMSe.RData")
load("MK/MKSeF.RData")

MR <- findEstComp("sdsAdultParMSe","MKPSe",adultODS10K,c(1:100),c(1,2,3,5,10,20,50,100),100,13,smts3)
MR3 <- do.call(cbind,MR)
MRPSe3 <- MR3[,c(c(1,9:11)+c(0,8:10)*13)]

MR <- findEstComp("sdsAdultDecMSe","MKDSe",adultODS10K,c(1:1000),c(1,2,3,5,10,20,50,100),100,13,smts3)
MR3 <- do.call(cbind,MR)
MRDSe3 <- MR3[,c(c(1,9:11)+c(0,8:10)*13)]

MR <- findEstComp("sdsAdultParMTSe","MKPTSe",adultODS10K,c(1:100),c(1,2,3,5,10,20,50,100),100,13,smts3)
MR3 <- do.call(cbind,MR)
MRPTSe3 <- MR3[,c(c(1,9:11)+c(0,8:10)*13)]

MR <- findEstComp("sdsAdultDecMTSe","MKDTSe",adultODS10K,c(1:1000),c(1,2,3,5,10,20,50,100),100,13,smts3)
MR3 <- do.call(cbind,MR)
MRDTSe3 <- MR3[,c(c(1,9:11)+c(0,8:10)*13)]

save(list = pasteC(list("MR",c("P","D"),"",c("","T"),"Se","3")),file = "MK/MRSeF3.RData")
rm(list = pasteC(list("sdsAdult",c("Par","Dec"),"M",c("","T"),"Se")))
rm(list = pasteC(list("MK",c("P","D"),"",c("","T"),"Se")))

load("sdsAdultMHSe.RData")
load("MK/MKHSeF.RData")

MR <- findEstComp("sdsAdultParMHSe","MKPHSe",adultODS10K,c(1:100),c(1,2,3,5,10,20,50,100),100,13,smts3)
MR3 <- do.call(cbind,MR)
MRPHSe3 <- MR3[,c(c(1,9:11)+c(0,8:10)*13)]

MR <- findEstComp("sdsAdultDecMHSe","MKDHSe",adultODS10K,c(1:1000),c(1,2,3,5,10,20,50,100),100,13,smts3)
MR3 <- do.call(cbind,MR)
MRDHSe3 <- MR3[,c(c(1,9:11)+c(0,8:10)*13)]

MR <- findEstComp("sdsAdultParMHTSe","MKPHTSe",adultODS10K,c(1:100),c(1,2,3,5,10,20,50,100),100,13,smts3)
MR3 <- do.call(cbind,MR)
MRPHTSe3 <- MR3[,c(c(1,9:11)+c(0,8:10)*13)]

MR <- findEstComp("sdsAdultDecMHTSe","MKDHTSe",adultODS10K,c(1:1000),c(1,2,3,5,10,20,50,100),100,13,smts3)
MR3 <- do.call(cbind,MR)
MRDHTSe3 <- MR3[,c(c(1,9:11)+c(0,8:10)*13)]

save(list = pasteC(list("MR",c("P","D"),"H",c("","T"),"Se","3")),file = "MK/MRHSeF3.RData")
rm(list = pasteC(list("sdsAdult",c("Par","Dec"),"MH",c("","T"),"Se")))
rm(list = pasteC(list("MK",c("P","D"),"H",c("","T"),"Se")))

load("sdsAdultMLSe.RData")
load("MK/MKLSeF.RData")

MR <- findEstComp("sdsAdultParMLSe","MKPLSe",adultODS10K,c(1:100),c(1,2,3,5,10,20,50,100),100,13,smts3)
MR3 <- do.call(cbind,MR)
MRPLSe3 <- MR3[,c(c(1,9:11)+c(0,8:10)*13)]

MR <- findEstComp("sdsAdultDecMLSe","MKDLSe",adultODS10K,c(1:1000),c(1,2,3,5,10,20,50,100),100,13,smts3)
MR3 <- do.call(cbind,MR)
MRDLSe3 <- MR3[,c(c(1,9:11)+c(0,8:10)*13)]

MR <- findEstComp("sdsAdultParMLTSe","MKPLTSe",adultODS10K,c(1:100),c(1,2,3,5,10,20,50,100),100,13,smts3)
MR3 <- do.call(cbind,MR)
MRPLTSe3 <- MR3[,c(c(1,9:11)+c(0,8:10)*13)]

MR <- findEstComp("sdsAdultDecMLTSe","MKDLTSe",adultODS10K,c(1:1000),c(1,2,3,5,10,20,50,100),100,13,smts3)
MR3 <- do.call(cbind,MR)
MRDLTSe3 <- MR3[,c(c(1,9:11)+c(0,8:10)*13)]

save(list = pasteC(list("MR",c("P","D"),"L",c("","T"),"Se","3")),file = "MK/MRLSeF3.RData")
rm(list = pasteC(list("sdsAdult",c("Par","Dec"),"ML",c("","T"),"Se")))
rm(list = pasteC(list("MK",c("P","D"),"L",c("","T"),"Se")))

load("sdsAdultMSSe.RData")
load("MK/MKSSeF.RData")

MR <- findEstComp("sdsAdultCatMPSe","MKCPSe",adultODS10K,c(1:1000),c(1,2,3,5,10,20,50,100),100,13,smts3)
MR3 <- do.call(cbind,MR)
MRCPSe3 <- MR3[,c(c(1,9:11)+c(0,8:10)*13)]

MR <- findEstComp("sdsAdultCatMPTSe","MKCPTSe",adultODS10K,c(1:1000),c(1,2,3,5,10,20,50,100),100,13,smts3)
MR3 <- do.call(cbind,MR)
MRCPTSe3 <- MR3[,c(c(1,9:11)+c(0,8:10)*13)]

MR <- findEstComp("sdsAdultCatMCSe","MKCCSe",adultODS10K,c(1:1000),c(1,2,3,5,10,20,50,100),100,13,smts3)
MR3 <- do.call(cbind,MR)
MRCCSe3 <- MR3[,c(c(1,9:11)+c(0,8:10)*13)]

MR <- findEstComp("sdsAdultCatMCTSe","MKCCTSe",adultODS10K,c(1:1000),c(1,2,3,5,10,20,50,100),100,13,smts3)
MR3 <- do.call(cbind,MR)
MRCCTSe3 <- MR3[,c(c(1,9:11)+c(0,8:10)*13)]

save(list = pasteC(list("MRC",c("P","C"),"",c("","T"),"Se","3")),file = "MK/MRSSeF3.RData")
rm(list = pasteC(list("sdsAdultCatM",c("P","C"),c("","T"),"Se")))
rm(list = pasteC(list("MKC",c("P","C"),c("","T"),"Se")))

## Polish and Avila

smt <- sapply(ods[c(1:2500),],function(x) length(levels(x)))
tm <- rep.int(-1,times = 14)
smtp <- sapply(c(1:14),function(x) tm)
smtp[c(1:14) + c(0:13)*14] <- smt
smts <- sapply(c(1:14),function(x) list(smtp[x,]))
smts2 <- smts
smts2[c(1,3,4,6,7,9:12)] <- lapply(smts2[c(1,3,4,6,7,9:12)],function(x) tm)
smtp3 <- smtp
smtp3[c(1:14) + c(0:13)*14] <- rep.int(-1,times = 14)
smts3 <- sapply(c(1:14),function(x) list(smtp3[x,]))
smts3[c(1,3,4,6,7,9:12)] <- lapply(smts3[c(1,3,4,6,7,9:12)],function(x) tm)


load("sdsPolishMH.RData")
load("MK/MKPF.RData")

MR <- findEstComp("sdsPolishParMH","MKPP",ods[c(1:2500),],c(1:1000),c(1,2,3,5,10,20,50,100),100,13,smts3)
MR3 <- do.call(cbind,MR)
MRPP3 <- MR3[,c(c(2,5,8,13,14)+c(1,4,7,12,13)*14)]

MR <- findEstComp("sdsPolishDecMH","MKPD",ods[c(1:2500),],c(1:1000),c(1,2,3,5,10,20,50,100),100,13,smts3)
MR3 <- do.call(cbind,MR)
MRPD3 <- MR3[,c(c(2,5,8,13,14)+c(1,4,7,12,13)*14)]

MR <- findEstComp("sdsPolishParMTH","MKPPT",ods[c(1:2500),],c(1:1000),c(1,2,3,5,10,20,50,100),100,13,smts3)
MR3 <- do.call(cbind,MR)
MRPPT3 <- MR3[,c(c(2,5,8,13,14)+c(1,4,7,12,13)*14)]

MR <- findEstComp("sdsPolishDecMTH","MKPDT",ods[c(1:2500),],c(1:1000),c(1,2,3,5,10,20,50,100),100,13,smts3)
MR3 <- do.call(cbind,MR)
MRPDT3 <- MR3[,c(c(2,5,8,13,14)+c(1,4,7,12,13)*14)]

save(list = pasteC(list("MRP",c("P","D"),"",c("","T"),"3")),file = "MK/MRPF3.RData")
rm(list = pasteC(list("sdsPolish",c("Par","Dec"),"M",c("","T"),"H")))
rm(list = pasteC(list("MKP",c("P","D"),"",c("","T"))))


load("sdsPolishMSH.RData")
load("MK/MKSPF.RData")

MR <- findEstComp("sdsPolishCatMPH","MKPCP",ods[c(1:2500),],c(1:1000),c(1,2,3,5,10,20,50,100),100,13,smts3)
MR3 <- do.call(cbind,MR)
MRPCP3 <- MR3[,c(c(2,5,8,13,14)+c(1,4,7,12,13)*14)]

MR <- findEstComp("sdsPolishCatMPTH","MKPCPT",ods[c(1:2500),],c(1:1000),c(1,2,3,5,10,20,50,100),100,13,smts3)
MR3 <- do.call(cbind,MR)
MRPCPT3 <- MR3[,c(c(2,5,8,13,14)+c(1,4,7,12,13)*14)]

MR <- findEstComp("sdsPolishCatMCH","MKPCC",ods[c(1:2500),],c(1:1000),c(1,2,3,5,10,20,50,100),100,13,smts3)
MR3 <- do.call(cbind,MR)
MRPCC3 <- MR3[,c(c(2,5,8,13,14)+c(1,4,7,12,13)*14)]

MR <- findEstComp("sdsPolishCatMCTH","MKPCCT",ods[c(1:2500),],c(1:1000),c(1,2,3,5,10,20,50,100),100,13,smts3)
MR3 <- do.call(cbind,MR)
MRPCCT3 <- MR3[,c(c(2,5,8,13,14)+c(1,4,7,12,13)*14)]

save(list = pasteC(list("MRPC",c("P","C"),"",c("","T"),"3")),file = "MK/MRSPF3.RData")
rm(list = pasteC(list("sdsPolishCatM",c("P","C"),"",c("","T"),"H")))
rm(list = pasteC(list("MKPC",c("P","C"),"",c("","T"))))



smt <- sapply(avilaODS[c(1:5215),],function(x) length(levels(x)))
tm <- rep.int(1,times = 11)
smtp <- sapply(c(1:11),function(x) tm)
smtp[c(1:11) + c(0:10)*11] <- smt
smts <- sapply(c(1:11),function(x) list(smtp[x,]))
smts2 <- smts
smts2[c(11)] <- lapply(smts2[c(11)],function(x) tm)
smtp3 <- smtp
smtp3[c(1:11) + c(0:10)*11] <- c(rep.int(-0.001,times = 7),-0.00001,-0.001,-0.00001,-1)
smts3 <- sapply(c(1:11),function(x) list(smtp3[x,]))
smts3[c(11)] <- lapply(smts3[c(11)],function(x) tm)

load("sdsAvilaMH.RData")
load("MK/MKAF.RData")

MR <- findEstComp("sdsAvilaParMH","MKAP",avilaODS[c(1:5215),],c(1:1000),c(1,2,3,5,10,20,50,100),100,13,smts3)
MR3 <- do.call(cbind,MR)
MRAP3 <- MR3[,c(c(1:10)+c(0:9)*11)]

MR <- findEstComp("sdsAvilaDecMH","MKAD",avilaODS[c(1:5215),],c(1:1000),c(1,2,3,5,10,20,50,100),100,13,smts3)
MR3 <- do.call(cbind,MR)
MRAD3 <- MR3[,c(c(1:10)+c(0:9)*11)]

MR <- findEstComp("sdsAvilaParMTH","MKAPT",avilaODS[c(1:5215),],c(1:1000),c(1,2,3,5,10,20,50,100),100,13,smts3)
MR3 <- do.call(cbind,MR)
MRAPT3 <- MR3[,c(c(1:10)+c(0:9)*11)]

MR <- findEstComp("sdsAvilaDecMTH","MKADT",avilaODS[c(1:5215),],c(1:1000),c(1,2,3,5,10,20,50,100),100,13,smts3)
MR3 <- do.call(cbind,MR)
MRADT3 <- MR3[,c(c(1:10)+c(0:9)*11)]

save(list = pasteC(list("MRA",c("P","D"),"",c("","T"),"3")),file = "MK/MRAF3.RData")
rm(list = pasteC(list("sdsAvila",c("Par","Dec"),"M",c("","T"),"H")))
rm(list = pasteC(list("MKA",c("P","D"),"",c("","T"))))



load("sdsAvilaMSH.RData")
load("MK/MKSAF.RData")

MR <- findEstComp("sdsAvilaCatMPH","MKACP",avilaODS[c(1:5215),],c(1:1000),c(1,2,3,5,10,20,50,100),100,13,smts3)
MR3 <- do.call(cbind,MR)
MRACP3 <- MR3[,c(c(1:10)+c(0:9)*11)]

MR <- findEstComp("sdsAvilaCatMPTH","MKACPT",avilaODS[c(1:5215),],c(1:1000),c(1,2,3,5,10,20,50,100),100,13,smts3)
MR3 <- do.call(cbind,MR)
MRACPT3 <- MR3[,c(c(1:10)+c(0:9)*11)]

MR <- findEstComp("sdsAvilaCatMCH","MKACC",avilaODS[c(1:5215),],c(1:1000),c(1,2,3,5,10,20,50,100),100,13,smts3)
MR3 <- do.call(cbind,MR)
MRACC3 <- MR3[,c(c(1:10)+c(0:9)*11)]

MR <- findEstComp("sdsAvilaCatMCTH","MKACCT",avilaODS[c(1:5215),],c(1:1000),c(1,2,3,5,10,20,50,100),100,13,smts3)
MR3 <- do.call(cbind,MR)
MRACCT3 <- MR3[,c(c(1:10)+c(0:9)*11)]

save(list = pasteC(list("MRAC",c("P","C"),"",c("","T"),"3")),file = "MK/MRSAF3.RData")
rm(list = pasteC(list("sdsAvilaCatM",c("P","C"),"",c("","T"),"H")))
rm(list = pasteC(list("MKAC",c("P","C"),"",c("","T"))))

## Combine

load("MK/MRF.RData")
load("MK/MRF3.RData")

MRP2 <- cbind(MRP,MRP3)
MRD2 <- cbind(MRD,MRD3)
MRPT2 <- cbind(MRPT,MRPT3)
MRDT2 <- cbind(MRDT,MRDT3)

save(list = pasteC(list("MR",c("P","D"),"",c("","T"),"2")),file = "MK/MRF2.RData")

load("MK/MRHF.RData")
load("MK/MRHF3.RData")

MRPH2 <- cbind(MRPH,MRPH3)
MRDH2 <- cbind(MRDH,MRDH3)
MRPHT2 <- cbind(MRPHT,MRPHT3)
MRDHT2 <- cbind(MRDHT,MRDHT3)

save(list = pasteC(list("MR",c("P","D"),"H",c("","T"),"2")),file = "MK/MRHF2.RData")

load("MK/MRLF.RData")
load("MK/MRLF3.RData")

MRPL2 <- cbind(MRPL,MRPL3)
MRDL2 <- cbind(MRDL,MRDL3)
MRPLT2 <- cbind(MRPLT,MRPLT3)
MRDLT2 <- cbind(MRDLT,MRDLT3)

save(list = pasteC(list("MR",c("P","D"),"L",c("","T"),"2")),file = "MK/MRLF2.RData")

load("MK/MRSF.RData")
load("MK/MRSF3.RData")

MRCP2 <- cbind(MRCP,MRCP3)
MRCC2 <- cbind(MRCC,MRCC3)
MRCPT2 <- cbind(MRCPT,MRCPT3)
MRCCT2 <- cbind(MRCCT,MRCCT3)

save(list = pasteC(list("MRC",c("P","C"),"",c("","T"),"2")),file = "MK/MRSF2.RData")


load("MK/MRSmF.RData")
load("MK/MRSmF3.RData")

MRDS2 <- cbind(MRDS,MRDS3)
MRDTS2 <- cbind(MRDTS,MRDTS3)

save(list = pasteC(list("MR",c("D"),"",c("","T"),"S2")),file = "MK/MRSmF2.RData")

load("MK/MRHSmF.RData")
load("MK/MRHSmF3.RData")

MRDHS2 <- cbind(MRDHS,MRDHS3)
MRDHTS2 <- cbind(MRDHTS,MRDHTS3)

save(list = pasteC(list("MR",c("D"),"H",c("","T"),"S2")),file = "MK/MRHSmF2.RData")

load("MK/MRLSmF.RData")
load("MK/MRLSmF3.RData")

MRDLS2 <- cbind(MRDLS,MRDLS3)
MRDLTS2 <- cbind(MRDLTS,MRDLTS3)

save(list = pasteC(list("MR",c("D"),"L",c("","T"),"S2")),file = "MK/MRLSmF2.RData")

load("MK/MRSSmF.RData")
load("MK/MRSSmF3.RData")

MRCPS2 <- cbind(MRCPS,MRCPS3)
MRCCS2 <- cbind(MRCCS,MRCCS3)
MRCPTS2 <- cbind(MRCPTS,MRCPTS3)
MRCCTS2 <- cbind(MRCCTS,MRCCTS3)

save(list = pasteC(list("MRC",c("P","C"),"",c("","T"),"S2")),file = "MK/MRSSmF2.RData")


load("MK/MRSeF.RData")
load("MK/MRSeF3.RData")

MRPSe2 <- cbind(MRPSe,MRPSe3)
MRDSe2 <- cbind(MRDSe,MRDSe3)
MRPTSe2 <- cbind(MRPTSe,MRPTSe3)
MRDTSe2 <- cbind(MRDTSe,MRDTSe3)

save(list = pasteC(list("MR",c("P","D"),"",c("","T"),"Se2")),file = "MK/MRSeF2.RData")

load("MK/MRHSeF.RData")
load("MK/MRHSeF3.RData")

MRPHSe2 <- cbind(MRPHSe,MRPHSe3)
MRDHSe2 <- cbind(MRDHSe,MRDHSe3)
MRPHTSe2 <- cbind(MRPHTSe,MRPHTSe3)
MRDHTSe2 <- cbind(MRDHTSe,MRDHTSe3)

save(list = pasteC(list("MR",c("P","D"),"H",c("","T"),"Se2")),file = "MK/MRHSeF2.RData")

load("MK/MRLSeF.RData")
load("MK/MRLSeF3.RData")

MRPLSe2 <- cbind(MRPLSe,MRPLSe3)
MRDLSe2 <- cbind(MRDLSe,MRDLSe3)
MRPLTSe2 <- cbind(MRPLTSe,MRPLTSe3)
MRDLTSe2 <- cbind(MRDLTSe,MRDLTSe3)

save(list = pasteC(list("MR",c("P","D"),"L",c("","T"),"Se2")),file = "MK/MRLSeF2.RData")

load("MK/MRSSeF.RData")
load("MK/MRSSeF3.RData")

MRCPSe2 <- cbind(MRCPSe,MRCPSe3)
MRCCSe2 <- cbind(MRCCSe,MRCCSe3)
MRCPTSe2 <- cbind(MRCPTSe,MRCPTSe3)
MRCCTSe2 <- cbind(MRCCTSe,MRCCTSe3)

save(list = pasteC(list("MRC",c("P","C"),"",c("","T"),"Se2")),file = "MK/MRSSeF2.RData")



load("MK/MRPF.RData")
load("MK/MRPF3.RData")

MRPP2 <- cbind(MRPP,MRPP3)
MRPD2 <- cbind(MRPD,MRPD3)
MRPPT2 <- cbind(MRPPT,MRPPT3)
MRPDT2 <- cbind(MRPDT,MRPDT3)

save(list = pasteC(list("MRP",c("P","D"),"",c("","T"),"2")),file = "MK/MRPF2.RData")

load("MK/MRSPF.RData")
load("MK/MRSPF3.RData")

MRPCP2 <- cbind(MRPCP,MRPCP3)
MRPCC2 <- cbind(MRPCC,MRPCC3)
MRPCPT2 <- cbind(MRPCPT,MRPCPT3)
MRPCCT2 <- cbind(MRPCCT,MRPCCT3)

save(list = pasteC(list("MRPC",c("P","C"),"",c("","T"),"2")),file = "MK/MRPSF2.RData")


load("MK/MRAF.RData")
load("MK/MRAF3.RData")

MRAP2 <- cbind(MRAP,MRAP3)
MRAD2 <- cbind(MRAD,MRAD3)
MRAPT2 <- cbind(MRAPT,MRAPT3)
MRADT2 <- cbind(MRADT,MRADT3)

save(list = pasteC(list("MRA",c("P","D"),"",c("","T"),"2")),file = "MK/MRAF2.RData")

load("MK/MRSAF.RData")
load("MK/MRSAF3.RData")

MRACP2 <- cbind(MRACP,MRACP3)
MRACC2 <- cbind(MRACC,MRACC3)
MRACPT2 <- cbind(MRACPT,MRACPT3)
MRACCT2 <- cbind(MRACCT,MRACCT3)

save(list = pasteC(list("MRAC",c("P","C"),"",c("","T"),"2")),file = "MK/MRASF2.RData")

## Extra

sigma1 <- rep.int(0.1,13)
sigma1[c(1,9,10,11)] <- c(1,1,1,1)
sigma2 <- rep.int(0.1,13)
sigma3 <- rep.int(1,13)
sigmaL <- rep(list(sigma1,sigma2,sigma3,sigma1,sigma2,sigma3,sigma1,sigma2,sigma3,sigma1,sigma2,sigma3,sigma1,sigma2,sigma3),times = 13)

rankbL <- rep(c(0,0,0,1,1,1,2,2,2,3,3,3,4,4,4),times = 13)

sel1 <- c(1:13)
selLp <- lapply(sapply(sel1,function(x) list(x)),function(x) sel1[-x])
selL <- lapply(selLp,function(x) rep(list(x),times = 15))
selL <- do.call(c,selL)

load("sdsAdultM.RData")

time <- list()
time <- c(time,list(Sys.time()))
MKDTe <- multiMK(adultODS10K,"sdsAdultDecM",sigmaL,c(1:100),rankbL,selL)
time <- c(time,list(Sys.time()))

save(list = c("MKDTe"),
     file = "MK/MKTe.RData")

time <- c(time,list(Sys.time()))
MEDTe <- findEst("sdsAdultDecM","MKDTe",c(1:100),c(1:13))
time <- c(time,list(Sys.time()))

rm(list = c("sdsAdultParM","sdsAdultDecM","sdsAdultParMT","sdsAdultDecMT"))
rm(list = c("MKDTe"))

save(list = c("MEDTe"),
     file = "MK/METe.RData")

MRDTe <- getAll("MEDTe",adultODS10K,c(1:13))

rm(list = c("MEDTe"))



sigma1 <- rep.int(0.1,13)
sigma1[c(1,9,10,11)] <- c(1,1,1,1)
sigma2 <- rep.int(0.1,13)
sigma3 <- rep.int(1,13)
sigmaL <- rep(list(sigma1,sigma2,sigma3),times = 6)

rankbL <- c(0,0,0,1,1,1,2,2,2,3,3,3,4,4,4,42,42,42)


selL <- rep(list(c(1:13)),times = 18)

load("sdsAdultM.RData")

time <- list()
time <- c(time,list(Sys.time()))
MKDTe <- multiMK(adultODS10K,"sdsAdultDecM",sigmaL,c(1:100),rankbL,selL)
time <- c(time,list(Sys.time()))

save(list = c("MKDTe"),
     file = "MK/MKTe2.RData")

time <- c(time,list(Sys.time()))
MEDTe <- findEst("sdsAdultDecM","MKDTe",c(1:100),c(1:13))
time <- c(time,list(Sys.time()))

rm(list = c("sdsAdultParM","sdsAdultDecM","sdsAdultParMT","sdsAdultDecMT"))
rm(list = c("MKDTe"))

save(list = c("MEDTe"),
     file = "MK/METe2.RData")

MRDTe <- getAll("MEDTe",adultODS10K,c(1:13))

rm(list = c("MEDTe"))


time <- list()
time <- c(time,list(Sys.time()))
MIDr <- multiPar(adultODSM2[c(1:10000,10001:20000),],"sdsAdultDecM",sigmaL,c(1:1000))
time <- c(time,list(Sys.time()))

load("sdsAdultM.RData")

time <- list()
time <- c(time,list(Sys.time()))
tst <- testMK(adultODS10K,"sdsAdultDecM",c(1:100),100)
time <- c(time,list(Sys.time()))

load("MK/MKTe2.RData")

MKDTe2 <- c(MKDTe,list(tst))
MKDTe2 <- lapply(MKDTe2,function(x) x[c(1,100),])

tst2 <- findEst("sdsAdultDecM","MKDTe2",c(1:100),c(1:13))

tst3 <- getAll("tst2",adultODS10K,c(1:13))

smt <- sapply(adultODS10K,function(x) length(levels(x)))
tm <- rep.int(-1,times = 13)
smtp <- sapply(c(1:13),function(x) tm)
smtp[c(1:13) + c(0:12)*13] <- smt
smts <- sapply(c(1:13),function(x) list(smtp[x,]))
smts2 <- smts
smts2[c(2:8,12,13)] <- lapply(smts2[c(2:8,12,13)],function(x) tm)

time <- list()
time <- c(time,list(Sys.time()))
tst <- findEstComp("sdsAdultDecM","MKD",adultODS10K,c(1:1000),c(1,2,3,5,10,20,50,100),100,0,smts)
time <- c(time,list(Sys.time()))
tst2 <- findEstComp("sdsAdultDecM","MKD",adultODS10K,c(1:1000),c(1,2,3,5,10,20,50,100),100,2,smts2)
time <- c(time,list(Sys.time()))
tst3 <- do.call(cbind,c(tst,tst2))
tst3 <- tst3[,c(c(1:13)+c(0:12)*13,c(1,9:11)+c(0,8:10)*13+13*13)]


rnd1 <- sapply(c(1:1000),function(x) sample(c(1:10000),10000,replace = TRUE))
rnd2 <- rnd1[,c(rep.int(1,times = 1000))]
rnd3 <- rnd1[,c(rep.int(21,times = 1000))]
rnd4 <- rnd1[,c(rep.int(132,times = 1000))]
rnd5 <- rnd1[,c(rep.int(278,times = 1000))]
rnd6 <- rnd1[,c(rep.int(633,times = 1000))]
rnd <- list(rnd1,rnd2,rnd3,rnd4,rnd5,rnd6)
rndS <- list(rnd1)

smt <- sapply(adultODS10K,function(x) length(levels(x)))
smts <- sapply(c(1:6),function(x) list(smt))
smt2 <- smt
smt2[smt > 0] <- -1
smts2 <- sapply(c(1:6),function(x) list(smt2))


time <- list()
time <- c(time,list(Sys.time()))
tst <- findEstComp("sdsAdultDecM","rnd",adultODS10K,c(1:1000),c(1,2,3,5,10,20,50,100),100,0,smts)
time <- c(time,list(Sys.time()))
tst2 <- findEstComp("sdsAdultDecM","rnd",adultODS10K,c(1:1000),c(1,2,3,5,10,20,50,100),100,2,smts2)
time <- c(time,list(Sys.time()))
#tst3 <- do.call(cbind,c(tst,tst2))
#tst3 <- tst3[,c(c(1:13)+c(0:12)*13,c(1,9:11)+c(0,8:10)*13+13*13)]

save(list = c(tst,tst2),file = "MK/MRRND.RData")

tst <- testEstAlt("sdsAdultDecM",rnd[[1]],adultODS10K,c(1),c(1:100))

time <- list()
time <- c(time,list(Sys.time()))
tst <- findEstComp("sdsAdultParM","rndS",adultODS10K,c(1:100),c(1,2,3,5,10,20,50,100),100,0,list(smt))
time <- c(time,list(Sys.time()))
tst2 <- findEstComp("sdsAdultParM","rndS",adultODS10K,c(1:100),c(1,2,3,5,10,20,50,100),100,2,list(smt2))
time <- c(time,list(Sys.time()))
MRPRND <- c(tst,tst2)

time <- c(time,list(Sys.time()))
tst <- findEstComp("sdsAdultDecM","rndS",adultODS10K,c(1:1000),c(1,2,3,5,10,20,50,100),100,0,list(smt))
time <- c(time,list(Sys.time()))
tst2 <- findEstComp("sdsAdultDecM","rndS",adultODS10K,c(1:1000),c(1,2,3,5,10,20,50,100),100,2,list(smt2))
time <- c(time,list(Sys.time()))
MRDRND <- c(tst,tst2)

time <- c(time,list(Sys.time()))
tst <- findEstComp("sdsAdultCatMP","rndS",adultODS10K,c(1:1000),c(1,2,3,5,10,20,50,100),100,0,list(smt))
time <- c(time,list(Sys.time()))
tst2 <- findEstComp("sdsAdultCatMP","rndS",adultODS10K,c(1:1000),c(1,2,3,5,10,20,50,100),100,2,list(smt2))
time <- c(time,list(Sys.time()))
MRCPRND <- c(tst,tst2)

time <- c(time,list(Sys.time()))
tst <- findEstComp("sdsAdultCatMC","rndS",adultODS10K,c(1:1000),c(1,2,3,5,10,20,50,100),100,0,list(smt))
time <- c(time,list(Sys.time()))
tst2 <- findEstComp("sdsAdultCatMC","rndS",adultODS10K,c(1:1000),c(1,2,3,5,10,20,50,100),100,2,list(smt2))
time <- c(time,list(Sys.time()))
MRCCRND <- c(tst,tst2)

time <- c(time,list(Sys.time()))
tst <- findEstComp("sdsAdultCatMPT","rndS",adultODS10K,c(1:1000),c(1,2,3,5,10,20,50,100),100,0,list(smt))
time <- c(time,list(Sys.time()))
tst2 <- findEstComp("sdsAdultCatMPT","rndS",adultODS10K,c(1:1000),c(1,2,3,5,10,20,50,100),100,2,list(smt2))
time <- c(time,list(Sys.time()))
MRCPTRND <- c(tst,tst2)

time <- c(time,list(Sys.time()))
tst <- findEstComp("sdsAdultCatMCS","rndS",adultODS10K,c(1:1000),c(1,2,3,5,10,20,50,100),100,0,list(smt))
time <- c(time,list(Sys.time()))
tst2 <- findEstComp("sdsAdultCatMCS","rndS",adultODS10K,c(1:1000),c(1,2,3,5,10,20,50,100),100,2,list(smt2))
time <- c(time,list(Sys.time()))
MRCCSRND <- c(tst,tst2)

time <- c(time,list(Sys.time()))
tst <- findEstComp("sdsAdultCatMCSe","rndS",adultODS10K,c(1:1000),c(1,2,3,5,10,20,50,100),100,0,list(smt))
time <- c(time,list(Sys.time()))
tst2 <- findEstComp("sdsAdultCatMCSe","rndS",adultODS10K,c(1:1000),c(1,2,3,5,10,20,50,100),100,2,list(smt2))
time <- c(time,list(Sys.time()))
MRCCSeRND <- c(tst,tst2)


time <- list()
time <- c(time,list(Sys.time()))
tst1 <- findEstComp("sdsAdultDecM","MKD",adultODS10K,c(1:1000),c(1,2,3,5,10,20,50,100),100,13,smts3)
time <- c(time,list(Sys.time()))
tst2 <- findEstComp("sdsAdultDecM","MKD",adultODS10K,c(1:1000),c(1,2,3,5,10,20,50,100),100,14,smts3)
time <- c(time,list(Sys.time()))
tst3 <- findEstComp("sdsAdultDecM","MKD",adultODS10K,c(1:1000),c(1,2,3,5,10,20,50,100),100,15,smts3)
time <- c(time,list(Sys.time()))



pN <- 10^seq(-50,20,1)
pN <- c(rbind(pN,2*pN,3*pN,5*pN))[1:(4*length(pN)-3)]
ml <- c(1:3,5,10,20,50,100)

load("MIN.RData")
alm <- c(pasteC(list(c("D","P"),c("","H","L"),c("","T"))),pasteC(list("C",c("P","C"),c("","T"))))
sapply(alm,function(x) assign(paste0("abo",x),multiMIPar(eval(as.name(paste0("MI",x))),pN,10000,10000,100,ml),envir = globalenv()))
rm(list = paste0("MI",alm))
tst <- GBDH(paste0("abo",alm),8,3,3,0)
lst = data.frame(Syn = factor(pasteC(list(c("Medium","High")," ",c(1,2,3,5,10,20,50,100))),
                              levels = pasteC(list(c("Medium","High")," ",c(1,2,3,5,10,20,50,100)))) ,
                 Clr = c("darkorange","darkred","navy","red","blue","tomato","steelblue","forestgreen",
                         "darkorange","darkred","navy","red","blue","tomato","steelblue","forestgreen"),
                 Lntp = c(rep.int(1,8),rep.int(2,8)),
                 Shp = rep.int(19,16))

sapply(alm,function(x) assign(paste0("plt",x),createPlot(t(eval(as.name(paste0("abo",x)))[[3]][c(1:16),]),pN,lst) +
                                labs(title = paste0("Difference Curves for ",x), x = "Threshold", y = "Difference") + ylim(0,1),envir = globalenv()))

for(c in paste0("plt",alm)){
  t.plot <- eval(as.name(c))
  ggsave(paste0("plots2/",c,"_Diff.pdf"),t.plot,device = "pdf")
}

load("MIS.RData")
almS <- c(pasteC(list("D",c("","H","L"),c("","T"),"S")),pasteC(list("C",c("P","C"),c("","T"),"S")))
sapply(almS,function(x) assign(paste0("abo",x),multiMIPar(eval(as.name(paste0("MI",x))),pN,10000,10000,100,ml),envir = globalenv()))
rm(list = paste0("MI",almS))
tst <- GBDH(paste0("abo",almS),8,3,3,0)

sapply(almS,function(x) assign(paste0("plt",x),createPlot(t(eval(as.name(paste0("abo",x)))[[3]][c(9:24),]),pN,lst) +
                                 labs(title = paste0("Difference Curves for ",x), x = "Threshold", y = "Difference") + ylim(0,1),envir = globalenv()))

for(c in paste0("plt",almS)){
  t.plot <- eval(as.name(c))
  ggsave(paste0("plots2/",c,"_Diff.pdf"),t.plot,device = "pdf")
}

load("MISe.RData")
almSe <- c(pasteC(list(c("D","P"),c("","H","L"),c("","T"),"Se")),pasteC(list("C",c("P","C"),c("","T"),"Se")))
sapply(almSe,function(x) assign(paste0("abo",x),multiMIPar(eval(as.name(paste0("MI",x))),pN,10000,10000,100,ml),envir = globalenv()))
rm(list = paste0("MI",almSe))
tst <- GBDH(paste0("abo",almSe),8,3,3,0)

sapply(almSe,function(x) assign(paste0("plt",x),createPlot(t(eval(as.name(paste0("abo",x)))[[3]][c(9:24),]),pN,lst) +
                                 labs(title = paste0("Difference Curves for ",x), x = "Threshold", y = "Difference") + ylim(0,1),envir = globalenv()))

for(c in paste0("plt",almSe)){
  t.plot <- eval(as.name(c))
  ggsave(paste0("plots2/",c,"_Diff.pdf"),t.plot,device = "pdf")
}

load("MIP.RData")
almP <- pasteC(list("P",c("D","P","CP","CC"),c("","T")))
sapply(almP,function(x) assign(paste0("abo",x),multiMIPar(eval(as.name(paste0("MI",x))),pN,2500,2500,100,ml),envir = globalenv()))
rm(list = paste0("MI",almP))
tst <- GBDH(paste0("abo",almP),8,3,3,0)

lst = data.frame(Syn = factor(pasteC(list(c("Medium","High")," ",c(1,2,3,5,10,20,50,100))),
                              levels = pasteC(list(c("Medium","High")," ",c(1,2,3,5,10,20,50,100)))) ,
                 Clr = c("darkorange","darkred","navy","red","blue","tomato","steelblue","forestgreen",
                         "darkorange","darkred","navy","red","blue","tomato","steelblue","forestgreen"),
                 Lntp = c(rep.int(1,8),rep.int(2,8)),
                 Shp = rep.int(19,16))

sapply(almP,function(x) assign(paste0("plt",x),createPlot(t(eval(as.name(paste0("abo",x)))[[3]][c(1:16),]),pN,lst) +
                                labs(title = paste0("Difference Curves for ",x), x = "Threshold", y = "Difference") + ylim(0,1),envir = globalenv()))

for(c in paste0("plt",almP)){
  t.plot <- eval(as.name(c))
  ggsave(paste0("plots2/",c,"_Diff.pdf"),t.plot,device = "pdf")
}

load("MIA.RData")
almA <- pasteC(list("A",c("D","P","CP","CC"),c("","T")))
sapply(almA,function(x) assign(paste0("abo",x),multiMIPar(eval(as.name(paste0("MI",x))),pN,5215,5215,100,ml),envir = globalenv()))
rm(list = paste0("MI",almA))
tst <- GBDH(paste0("abo",almA),8,3,3,0)

lst = data.frame(Syn = factor(pasteC(list(c("Medium","High")," ",c(1,2,3,5,10,20,50,100))),
                              levels = pasteC(list(c("Medium","High")," ",c(1,2,3,5,10,20,50,100)))) ,
                 Clr = c("darkorange","darkred","navy","red","blue","tomato","steelblue","forestgreen",
                         "darkorange","darkred","navy","red","blue","tomato","steelblue","forestgreen"),
                 Lntp = c(rep.int(1,8),rep.int(2,8)),
                 Shp = rep.int(19,16))

sapply(almA,function(x) assign(paste0("plt",x),createPlot(t(eval(as.name(paste0("abo",x)))[[3]][c(1:16),]),pN,lst) +
                                 labs(title = paste0("Difference Curves for ",x), x = "Threshold", y = "Difference") + ylim(0,1),envir = globalenv()))

for(c in paste0("plt",almA)){
  t.plot <- eval(as.name(c))
  ggsave(paste0("plots2/",c,"_Diff.pdf"),t.plot,device = "pdf")
}

data <- expand.grid(X = c(1:3), Y = c(1:length(pN)))
data$Z <- c(aboD[[3]][seq(1,24,8),])
ggplot(data, aes(X, Y, fill= Z)) + geom_tile() + scale_fill_gradient(low="white", high="blue")

lst = data.frame(Syn = factor(c("Medium 1","Medium 2","Medium 3","Medium 5","Medium 10","Medium 20","Medium 50","Medium 100","High 1","High 2","High 3","High 5","High 10","High 20","High 50","High 100","baseline"),
                              levels = c("Medium 1","Medium 2","Medium 3","Medium 5","Medium 10","Medium 20","Medium 50","Medium 100","High 1","High 2","High 3","High 5","High 10","High 20","High 50","High 100","baseline")) ,
                 Clr = c("darkorange","darkred","navy","red","blue","tomato","steelblue","forestgreen","limegreen","grey","lightseagreen","purple","turquoise","goldenrod","tan","chocolate","black"),
                 Lntp = replicate(17,1),
                 Shp = replicate(17,19))

lst = data.frame(Syn = factor(c(pasteC(list(c("Medium","High")," ",c(1,2,3,5,10,20,50,100))),"Baseline"),
                              levels = c(pasteC(list(c("Medium","High")," ",c(1,2,3,5,10,20,50,100))),"Baseline")) ,
                 Clr = c("darkorange","darkred","navy","red","blue","tomato","steelblue","forestgreen",
                         "darkorange","darkred","navy","red","blue","tomato","steelblue","forestgreen","black"),
                 Lntp = c(rep.int(1,8),rep.int(2,8),2),
                 Shp = rep.int(19,17))

sapply(alm,function(x) assign(paste0("plt",x),createPlot2(t(rbind(eval(as.name(paste0("abo",x)))[[2]][c(1:16),],seq(0,1,1/280))),
                                                          t(rbind(eval(as.name(paste0("abo",x)))[[1]][c(1:16),],seq(0,1,1/280))),lst) +
                                 labs(title = paste0("ROC Curve for ",x), x = "False Positive", y = "True Positive"),envir = globalenv()))

for(c in paste0("plt",alm)){
  t.plot <- eval(as.name(c))
  ggsave(paste0("plots2/",c,"_ROC.pdf"),t.plot,device = "pdf")
}

sapply(almS,function(x) assign(paste0("plt",x),createPlot2(t(rbind(eval(as.name(paste0("abo",x)))[[2]][c(1:16),],seq(0,1,1/280))),
                                                          t(rbind(eval(as.name(paste0("abo",x)))[[1]][c(1:16),],seq(0,1,1/280))),lst) +
                                labs(title = paste0("ROC Curve for ",x), x = "False Positive", y = "True Positive"),envir = globalenv()))

for(c in paste0("plt",almS)){
  t.plot <- eval(as.name(c))
  ggsave(paste0("plots2/",c,"_ROC.pdf"),t.plot,device = "pdf")
}

sapply(almSe,function(x) assign(paste0("plt",x),createPlot2(t(rbind(eval(as.name(paste0("abo",x)))[[2]][c(1:16),],seq(0,1,1/280))),
                                                          t(rbind(eval(as.name(paste0("abo",x)))[[1]][c(1:16),],seq(0,1,1/280))),lst) +
                                labs(title = paste0("ROC Curve for ",x), x = "False Positive", y = "True Positive"),envir = globalenv()))

for(c in paste0("plt",almSe)){
  t.plot <- eval(as.name(c))
  ggsave(paste0("plots2/",c,"_ROC.pdf"),t.plot,device = "pdf")
}

lst = data.frame(Syn = factor(c(pasteC(list(c("Medium","High")," ",c(1,2,3,5,10,20,50,100))),"Baseline"),
                              levels = c(pasteC(list(c("Medium","High")," ",c(1,2,3,5,10,20,50,100))),"Baseline")) ,
                 Clr = c("darkorange","darkred","navy","red","blue","tomato","steelblue","forestgreen",
                         "darkorange","darkred","navy","red","blue","tomato","steelblue","forestgreen","black"),
                 Lntp = c(rep.int(1,8),rep.int(2,8),2),
                 Shp = rep.int(19,17))

sapply(almP,function(x) assign(paste0("plt",x),createPlot2(t(rbind(eval(as.name(paste0("abo",x)))[[2]][c(1:16),],seq(0,1,1/280))),
                                                          t(rbind(eval(as.name(paste0("abo",x)))[[1]][c(1:16),],seq(0,1,1/280))),lst) +
                                labs(title = paste0("ROC Curve for ",x), x = "False Positive", y = "True Positive"),envir = globalenv()))

for(c in paste0("plt",almP)){
  t.plot <- eval(as.name(c))
  ggsave(paste0("plots2/",c,"_ROC.pdf"),t.plot,device = "pdf")
}

lst = data.frame(Syn = factor(c(pasteC(list(c("Medium","High")," ",c(1,2,3,5,10,20,50,100))),"Baseline"),
                              levels = c(pasteC(list(c("Medium","High")," ",c(1,2,3,5,10,20,50,100))),"Baseline")) ,
                 Clr = c("darkorange","darkred","navy","red","blue","tomato","steelblue","forestgreen",
                         "darkorange","darkred","navy","red","blue","tomato","steelblue","forestgreen","black"),
                 Lntp = c(rep.int(1,8),rep.int(2,8),2),
                 Shp = rep.int(19,17))

sapply(almA,function(x) assign(paste0("plt",x),createPlot2(t(rbind(eval(as.name(paste0("abo",x)))[[2]][c(1:16),],seq(0,1,1/280))),
                                                          t(rbind(eval(as.name(paste0("abo",x)))[[1]][c(1:16),],seq(0,1,1/280))),lst) +
                                labs(title = paste0("ROC Curve for ",x), x = "False Positive", y = "True Positive"),envir = globalenv()))

for(c in paste0("plt",almA)){
  t.plot <- eval(as.name(c))
  ggsave(paste0("plots2/",c,"_ROC.pdf"),t.plot,device = "pdf")
}

lst = data.frame(Syn = factor(c(pasteC(list("Medium"," ",c(1,2,3,5,10,20,50,100))),"Baseline"),
                              levels = c(pasteC(list("Medium"," ",c(1,2,3,5,10,20,50,100))),"Baseline")) ,
                 Clr = c("darkorange","darkred","navy","red","blue","tomato","steelblue","forestgreen","black"),
                 Lntp = c(rep.int(1,8),2),
                 Shp = rep.int(19,9))

sapply(almP,function(x) assign(paste0("plt",x),createPlot2(t(rbind(eval(as.name(paste0("abo",x)))[[2]][c(1:8),],seq(0,1,1/280))),
                                                           t(rbind(eval(as.name(paste0("abo",x)))[[1]][c(1:8),],seq(0,1,1/280))),lst) +
                                 labs(title = paste0("ROC Curve for ",x), x = "False Positive", y = "True Positive"),envir = globalenv()))

for(c in paste0("plt",almP)){
  t.plot <- eval(as.name(c))
  ggsave(paste0("plots2/",c,"_ROC2.pdf"),t.plot,device = "pdf")
}

lst = data.frame(Syn = factor(c(pasteC(list("Medium"," ",c(1,2,3,5,10,20,50,100))),"Baseline"),
                              levels = c(pasteC(list("Medium"," ",c(1,2,3,5,10,20,50,100))),"Baseline")) ,
                 Clr = c("darkorange","darkred","navy","red","blue","tomato","steelblue","forestgreen","black"),
                 Lntp = c(rep.int(1,8),2),
                 Shp = rep.int(19,9))

sapply(almA,function(x) assign(paste0("plt",x),createPlot2(t(rbind(eval(as.name(paste0("abo",x)))[[2]][c(1:8),],seq(0,1,1/280))),
                                                           t(rbind(eval(as.name(paste0("abo",x)))[[1]][c(1:8),],seq(0,1,1/280))),lst) +
                                 labs(title = paste0("ROC Curve for ",x), x = "False Positive", y = "True Positive"),envir = globalenv()))

for(c in paste0("plt",almA)){
  t.plot <- eval(as.name(c))
  ggsave(paste0("plots2/",c,"_ROC2.pdf"),t.plot,device = "pdf")
}

adultODS10KD <- createDict(adultODS10K)
adultODS20KD <- createDict(adultODS10K,20000)
adultODS100KD <- createDict(adultODS10K,100000)

time <- list()
time <- c(time,list(Sys.time()))
tst10 <- findRLA(adultODS10K,adultODS10KD,"sdsAdultCatMC",rep.int(1.0,times = 13),c(1:100),0,c(1:13))
time <- c(time,list(Sys.time()))
tst20 <- findRLA(adultODS10K,adultODS20KD,"sdsAdultCatMC",rep.int(1.0,times = 13),c(1:100),0,c(1:13))
time <- c(time,list(Sys.time()))
tst100 <- findRLA(adultODS10K,adultODS100KD,"sdsAdultCatMC",rep.int(1.0,times = 13),c(1:100),0,c(1:13))
time <- c(time,list(Sys.time()))

time <- list()
time <- c(time,list(Sys.time()))
tst10P <- findRLA(adultODS10K,adultODS10KD,"sdsAdultCatMP",rep.int(1.0,times = 13),c(1:100),0,c(1:13))
time <- c(time,list(Sys.time()))
tst10PT <- findRLA(adultODS10K,adultODS10KD,"sdsAdultCatMPT",rep.int(1.0,times = 13),c(1:100),0,c(1:13))
time <- c(time,list(Sys.time()))


## MK Plots
c("darkorange","darkred","navy","red","blue","tomato","steelblue","forestgreen","limegreen",
  "grey","lightseagreen","purple","turquoise","goldenrod","tan","chocolate","black")

ml <- c(1:3,5,10,20,50,100)
#xp <- seq(0,1,0.125)

## Adult

cho <- c(18,2:8,19:21,12,13)

sapply(paste0("MK/MR",c("","H","L","S"),"F2.RData"),function(x) load(x,.GlobalEnv))

alm <- c(pasteC(list(c("D","P"),c("","H","L"),c("","T"))),pasteC(list("C",c("P","C"),c("","T"))))

lst = data.frame(Syn = factor(colnames(adultODS10K),
                              levels = colnames(adultODS10K)) ,
                 Clr = c("darkorange","darkred","blue","red","steelblue","forestgreen",
                         "grey","purple","turquoise","limegreen","tan","chocolate","black"),
                 Lntp = rep.int(1,13),
                 Shp = rep.int(19,13))

sapply(alm,function(x) assign(paste0("plt",x),createPlot(eval(as.name(paste0("MR",x,"2")))[,cho],ml,lst) +
                                labs(title = paste0("Maximum Knowledge (Individual Categories) for ",x), x = "m", y = "Accuracy") +
                                scale_x_log10(breaks=ml) + ylim(0,1),envir = globalenv()))

for(c in paste0("plt",alm)){
  t.plot <- eval(as.name(c))
  ggsave(paste0("plots2/",c,"_MKIC.pdf"),t.plot,device = "pdf")
}


sapply(paste0("MK/MR",c("","H","L","S"),"Sm","F2.RData"),function(x) load(x,.GlobalEnv))

alm <- c(pasteC(list("D",c("","H","L"),c("","T"),"S")),pasteC(list("C",c("P","C"),c("","T"),"S")))

sapply(alm,function(x) assign(paste0("plt",x),createPlot(eval(as.name(paste0("MR",x,"2")))[,cho],ml,lst) +
                                labs(title = paste0("Maximum Knowledge (Individual Categories) for ",x), x = "m", y = "Accuracy") +
                                scale_x_log10(breaks=ml) + ylim(0,1),envir = globalenv()))

for(c in paste0("plt",alm)){
  t.plot <- eval(as.name(c))
  ggsave(paste0("plots2/",c,"_MKIC.pdf"),t.plot,device = "pdf")
}


sapply(paste0("MK/MR",c("","H","L","S"),"Se","F2.RData"),function(x) load(x,.GlobalEnv))

alm <- c(pasteC(list(c("D","P"),c("","H","L"),c("","T"),"Se")),pasteC(list("C",c("P","C"),c("","T"),"Se")))

sapply(alm,function(x) assign(paste0("plt",x),createPlot(eval(as.name(paste0("MR",x,"2")))[,cho],ml,lst) +
                                labs(title = paste0("Maximum Knowledge (Individual Categories) for ",x), x = "m", y = "Accuracy") +
                                scale_x_log10(breaks=ml) + ylim(0,1),envir = globalenv()))

for(c in paste0("plt",alm)){
  t.plot <- eval(as.name(c))
  ggsave(paste0("plots2/",c,"_MKIC.pdf"),t.plot,device = "pdf")
}

## Polish

choP <- c(1,20,3,4,21,6,7,22,9:12,23,24)

sapply(paste0("MK/MRP",c("","S"),"F2.RData"),function(x) load(x,.GlobalEnv))

almP <- pasteC(list("P",c("D","P","CP","CC"),c("","T")))

lst = data.frame(Syn = factor(colnames(ods),
                              levels = colnames(ods)) ,
                 Clr = c("darkorange","darkred","blue","red","steelblue","forestgreen","lightseagreen",
                         "grey","purple","turquoise","limegreen","tan","chocolate","black"),
                 Lntp = rep.int(1,14),
                 Shp = rep.int(19,14))

sapply(almP,function(x) assign(paste0("plt",x),createPlot(eval(as.name(paste0("MR",x,"2")))[,choP],ml,lst) +
                                labs(title = paste0("Maximum Knowledge (Individual Categories) for ",x), x = "m", y = "Accuracy") +
                                scale_x_log10(breaks=ml) + ylim(0,1),envir = globalenv()))

for(c in paste0("plt",almP)){
  t.plot <- eval(as.name(c))
  ggsave(paste0("plots2/",c,"_MKIC.pdf"),t.plot,device = "pdf")
}

## Avila

choA <- c(11,22:31)

sapply(paste0("MK/MRA",c("","S"),"F2.RData"),function(x) load(x,.GlobalEnv))

almA <- pasteC(list("A",c("D","P","CP","CC"),c("","T")))

lst = data.frame(Syn = factor(colnames(avilaODS),
                              levels = colnames(avilaODS)) ,
                 Clr = c("darkorange","blue","red","steelblue","forestgreen",
                         "grey","purple","turquoise","limegreen","chocolate","black"),
                 Lntp = rep.int(1,11),
                 Shp = rep.int(19,11))

sapply(almA,function(x) assign(paste0("plt",x),createPlot(eval(as.name(paste0("MR",x,"2")))[,choA],ml,lst) +
                                 labs(title = paste0("Maximum Knowledge (Individual Categories) for ",x), x = "m", y = "Accuracy") +
                                 scale_x_log10(breaks=ml) + ylim(0,1),envir = globalenv()))

for(c in paste0("plt",almA)){
  t.plot <- eval(as.name(c))
  ggsave(paste0("plots2/",c,"_MKIC.pdf"),t.plot,device = "pdf")
}
