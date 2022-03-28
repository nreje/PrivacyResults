source("functions.R")
source("privacy.R")
#source("resultFunctions.R")

## Rest

load("glmAdultMSm.RData")
load("glmAdultMHSm.RData")
load("glmAdultMLSm.RData")
load("glmAdultMSSm.RData")

teP2 <- c("D","DT","DO","DOT","DV","DVT","CP","CPT","CC","CCT")
glmP2 <- c(paste0("glm","Adult","Dec","M",c("S","TS","HS","HTS","LS","LTS")),
           paste0("glm","Adult","Cat","M",c("PS","PTS","CS","CTS")))

CIavg2AsPS9 <- avgModelsCIPerSamp(2,glm = glmP2,te = teP2,CI = 0.9)

load("glmPolishMH.RData")
load("glmPolishMSH.RData")
load("glmAvilaMH.RData")
load("glmAvilaMSH.RData")

teP2 <- c("PP","PPT","PD","PDT","PCP","PCPT","PCC","PCCT","AP","APT","AD","ADT","ACP","ACPT","ACC","ACCT")
glmP2 <- c(paste0("glm","Polish","Par","M",c("H","TH")),paste0("glm","Polish","Dec","M",c("H","TH")),
           paste0("glm","Polish","Cat","M",c("PH","PTH","CH","CTH")),
           paste0("glm","Avila","Par","M",c("H","TH")),paste0("glm","Avila","Dec","M",c("H","TH")),
           paste0("glm","Avila","Cat","M",c("PH","PTH","CH","CTH")))

CIavg2PAsPS9 <- avgModelsCIPerSamp(2,glm = glmP2,te = teP2,CI = 0.9)


save(list = c("CIavg2AsPS9","CIavg2PAsPS9"),file = "privacyUt.RData")

load("glmAvilaKH.RData")

teP2 <- c("P","PT","D","DT","CP","CPT","CC","CCT")
glmP2 <- c(paste0("glm","Avila","Par","M",c("","T"),"KH"),paste0("glm","Avila","Dec","M",c("","T"),"KH"),
           paste0("glm","Avila","Cat","M",c("P","PT","C","CT"),"KH"))

CIavg2AKPS9 <- avgModelsCIPerSamp(2,glm = glmP2,te = teP2,CI = 0.9)

save(list = c("CIavg2AKPS9"),file = "privacyUt2.RData")

## Combine
paste0("abo","P",c("P","PT","D","DT"))


pN <- 10^seq(-50,20,1)
pN <- c(rbind(pN,2*pN,3*pN,5*pN))[1:(4*length(pN)-3)]
ml <- c(1:3,5,10,20,50,100)

##Polish
load("privacyUt.RData")
load("MIP.RData")

aboPP <- multiMIPar(MIPP,pN,2500,2500,100,ml)
aboPPT <- multiMIPar(MIPPT,pN,2500,2500,100,ml)
aboPD <- multiMIPar(MIPD,pN,2500,2500,100,ml)
aboPDT <- multiMIPar(MIPDT,pN,2500,2500,100,ml)

aboPCP <- multiMIPar(MIPCP,pN,2500,2500,100,ml)
aboPCPT <- multiMIPar(MIPCPT,pN,2500,2500,100,ml)
aboPCC <- multiMIPar(MIPCC,pN,2500,2500,100,ml)
aboPCCT <- multiMIPar(MIPCCT,pN,2500,2500,100,ml)

pri <- cbind(GBD(aboPP[[3]],3),GBD(aboPPT[[3]],3),GBD(aboPD[[3]],3),GBD(aboPDT[[3]],3),
             GBD(aboPCP[[3]],3),GBD(aboPCPT[[3]],3),GBD(aboPCC[[3]],3),GBD(aboPCCT[[3]],3))

lst = data.frame(Syn = factor(c("P U","PT U","D U","DT U","CP U","CPT U","CC U","CCT U","P I","PT I","D I","DT I","CP I","CPT I","CC I","CCT I"),
                              levels = c("P U","PT U","D U","DT U","CP U","CPT U","CC U","CCT U","P I","PT I","D I","DT I","CP I","CPT I","CC I","CCT I")) ,
                 Clr = c("darkred","red","navy","blue","forestgreen","limegreen","black","grey","darkred","red","navy","blue","forestgreen","limegreen","black","grey"),
                 Lntp = c(replicate(8,1),replicate(8,2)),
                 Shp = replicate(16,19))

dat <- cbind(CIavg2PAsPS9[[4]][[3]][,1:8],pri)

pltPH <- createPlot(dat,ml,lst) +
  labs(y = "Accuracy",title = "Polish Utility and Membership Inference")

rm(list = c(paste0("abo","P",c("P","PT","D","DT")),paste0("abo","P",c("CP","CPT","CC","CCT"))))
rm(list = c(paste0("MI","P",c("P","PT","D","DT")),paste0("abo","P",c("CP","CPT","CC","CCT"))))

##Avila
load("privacyUt.RData")
load("privacyUt2.RData")
load("MIA.RData")

aboAP <- multiMIPar(MIAP,pN,5215,5215,100,ml)
aboAPT <- multiMIPar(MIAPT,pN,5215,5215,100,ml)
aboAD <- multiMIPar(MIAD,pN,5215,5215,100,ml)
aboADT <- multiMIPar(MIADT,pN,5215,5215,100,ml)


aboACP <- multiMIPar(MIACP,pN,5215,5215,100,ml)
aboACPT <- multiMIPar(MIACPT,pN,5215,5215,100,ml)
aboACC <- multiMIPar(MIACC,pN,5215,5215,100,ml)
aboACCT <- multiMIPar(MIACCT,pN,5215,5215,100,ml)

pri <- cbind(GBD(aboAP[[3]],3),GBD(aboAPT[[3]],3),GBD(aboAD[[3]],3),GBD(aboADT[[3]],3),
             GBD(aboACP[[3]],3),GBD(aboACPT[[3]],3),GBD(aboACC[[3]],3),GBD(aboACCT[[3]],3))

lst = data.frame(Syn = factor(c("P U","PT U","D U","DT U","CP U","CPT U","CC U","CCT U","P I","PT I","D I","DT I","CP I","CPT I","CC I","CCT I"),
                              levels = c("P U","PT U","D U","DT U","CP U","CPT U","CC U","CCT U","P I","PT I","D I","DT I","CP I","CPT I","CC I","CCT I")) ,
                 Clr = c("darkred","red","navy","blue","forestgreen","limegreen","black","grey","darkred","red","navy","blue","forestgreen","limegreen","black","grey"),
                 Lntp = c(replicate(8,1),replicate(8,2)),
                 Shp = replicate(16,19))

dat <- cbind(CIavg2PAsPS9[[4]][[3]][,9:16],pri)

pltAH <- createPlot(dat,ml,lst)

dat <- cbind(CIavg2AKPS9[[4]][[3]],pri)

pltAHK <- createPlot(dat,ml,lst)

rm(list = c(paste0("abo","A",c("P","PT","D","DT")),paste0("abo","A",c("CP","CPT","CC","CCT"))))
rm(list = c(paste0("MI","A",c("P","PT","D","DT")),paste0("abo","A",c("CP","CPT","CC","CCT"))))

##Adult
load("adult3.RData")
load("MIN.RData")

aboP <- multiMIPar(MIP,pN,10000,10000,100,ml)
aboPT <- multiMIPar(MIPT,pN,10000,10000,100,ml)
aboD <- multiMIPar(MID,pN,10000,10000,100,ml)
aboDT <- multiMIPar(MIDT,pN,10000,10000,100,ml)


aboPH <- multiMIPar(MIPH,pN,10000,10000,100,ml)
aboPHT <- multiMIPar(MIPHT,pN,10000,10000,100,ml)
aboDH <- multiMIPar(MIDH,pN,10000,10000,100,ml)
aboDHT <- multiMIPar(MIDHT,pN,10000,10000,100,ml)

pri <- cbind(GBD(aboP[[3]],3),GBD(aboPT[[3]],3),GBD(aboD[[3]],3),GBD(aboDT[[3]],3),
             GBD(aboPH[[3]],3),GBD(aboPHT[[3]],3),GBD(aboDH[[3]],3),GBD(aboDHT[[3]],3))

lst = data.frame(Syn = factor(c("P U","PT U","D U","DT U","PH U","PHT U","DH U","DHT U","P I","PT I","D I","DT I","PH I","PHT I","DH I","DHT I"),
                              levels = c("P U","PT U","D U","DT U","PH U","PHT U","DH U","DHT U","P I","PT I","D I","DT I","PH I","PHT I","DH I","DHT I")) ,
                 Clr = c("darkred","red","navy","blue","forestgreen","limegreen","black","grey","darkred","red","navy","blue","forestgreen","limegreen","black","grey"),
                 Lntp = c(replicate(8,1),replicate(8,2)),
                 Shp = replicate(16,19))

dat <- cbind(CIavg2APS9[[4]][[3]][,c(2,4,3,5,6,8,7,9)],pri)

pltA1 <- createPlot(dat,ml,lst)


aboPL <- multiMIPar(MIPL,pN,10000,10000,100,ml)
aboPLT <- multiMIPar(MIPLT,pN,10000,10000,100,ml)
aboDL <- multiMIPar(MIDL,pN,10000,10000,100,ml)
aboDLT <- multiMIPar(MIDLT,pN,10000,10000,100,ml)


aboCP <- multiMIPar(MICP,pN,10000,10000,100,ml)
aboCPT <- multiMIPar(MICPT,pN,10000,10000,100,ml)
aboCC <- multiMIPar(MICC,pN,10000,10000,100,ml)
aboCCT <- multiMIPar(MICCT,pN,10000,10000,100,ml)

pri <- cbind(GBD(aboPL[[3]],3),GBD(aboPLT[[3]],3),GBD(aboDL[[3]],3),GBD(aboDLT[[3]],3),
             GBD(aboCP[[3]],3),GBD(aboCPT[[3]],3),GBD(aboCC[[3]],3),GBD(aboCCT[[3]],3))

lst = data.frame(Syn = factor(c("PL U","PLT U","DL U","DLT U","CP U","CPT U","CC U","CCT U","PL I","PLT I","DL I","DLT I","CP I","CPT I","CC I","CCT I"),
                              levels = c("PL U","PLT U","DL U","DLT U","CP U","CPT U","CC U","CCT U","PL I","PLT I","DL I","DLT I","CP I","CPT I","CC I","CCT I")) ,
                 Clr = c("darkred","red","navy","blue","forestgreen","limegreen","black","grey","darkred","red","navy","blue","forestgreen","limegreen","black","grey"),
                 Lntp = c(replicate(8,1),replicate(8,2)),
                 Shp = replicate(16,19))

dat <- cbind(CIavg2APS9[[4]][[3]][,c(10,12,11,13,14,15,16,17)],pri)

pltA2 <- createPlot(dat,ml,lst)

rm(list = c(paste0("abo","",c("P","PT","D","DT")),paste0("abo","",c("PH","PHT","DH","DHT")),
            paste0("abo","",c("PL","PLT","DL","DLT")),paste0("abo","",c("CP","CPT","CC","CCT"))))
rm(list = c(paste0("MI","",c("P","PT","D","DT")),paste0("MI","",c("PH","PHT","DH","DHT")),
            paste0("MI","",c("PL","PLT","DL","DLT")),paste0("MI","",c("CP","CPT","CC","CCT"))))

##Adult Smooth
load("privacyUt.RData")
load("MIS.RData")

aboDS <- multiMIPar(MIDS,pN,10000,10000,100,ml)
aboDTS <- multiMIPar(MIDTS,pN,10000,10000,100,ml)


aboDHS <- multiMIPar(MIDHS,pN,10000,10000,100,ml)
aboDHTS <- multiMIPar(MIDHTS,pN,10000,10000,100,ml)


aboDLS <- multiMIPar(MIDLS,pN,10000,10000,100,ml)
aboDLTS <- multiMIPar(MIDLTS,pN,10000,10000,100,ml)


aboCPS <- multiMIPar(MICPS,pN,10000,10000,100,ml)
aboCPTS <- multiMIPar(MICPTS,pN,10000,10000,100,ml)
aboCCS <- multiMIPar(MICCS,pN,10000,10000,100,ml)
aboCCTS <- multiMIPar(MICCTS,pN,10000,10000,100,ml)

pri <- cbind(GBD(aboDS[[3]],3),GBD(aboDTS[[3]],3),GBD(aboDHS[[3]],3),GBD(aboDHTS[[3]],3),GBD(aboDLS[[3]],3),GBD(aboDLTS[[3]],3),
             GBD(aboCPS[[3]],3),GBD(aboCPTS[[3]],3),GBD(aboCCS[[3]],3),GBD(aboCCTS[[3]],3))

lst = data.frame(Syn = factor(c("D U","DT U","DH U","DHT U","DL U","DLT U","CP U","CPT U","CC U","CCT U","D I","DT I","DH I","DHT I","DL I","DLT I","CP I","CPT I","CC I","CCT I"),
                              levels = c("D U","DT U","DH U","DHT U","DL U","DLT U","CP U","CPT U","CC U","CCT U","D I","DT I","DH I","DHT I","DL I","DLT I","CP I","CPT I","CC I","CCT I")) ,
                 Clr = c("darkred","red","navy","blue","purple","pink","forestgreen","limegreen","black","grey","darkred","red","navy","blue","purple","pink","forestgreen","limegreen","black","grey"),
                 Lntp = c(replicate(10,1),replicate(10,2)),
                 Shp = replicate(20,19))

dat <- cbind(CIavg2AsPS9[[4]][[3]],pri)

pltAS <- createPlot(dat,ml,lst)

rm(list = c(paste0("abo","",c("D","DT"),"S"),paste0("abo","",c("DH","DHT"),"S"),
            paste0("abo","",c("DL","DLT"),"S"),paste0("abo","",c("CP","CPT","CC","CCT"),"S")))
rm(list = c(paste0("MI","",c("D","DT"),"S"),paste0("MI","",c("DH","DHT"),"S"),
            paste0("MI","",c("DL","DLT"),"S"),paste0("MI","",c("CP","CPT","CC","CCT"),"S")))

##Adult Selective
load("adultSe.RData")
load("MISe.RData")

aboPSe <- multiMIPar(MIPSe,pN,10000,10000,100,ml)
aboPTSe <- multiMIPar(MIPTSe,pN,10000,10000,100,ml)
aboDSe <- multiMIPar(MIDSe,pN,10000,10000,100,ml)
aboDTSe <- multiMIPar(MIDTSe,pN,10000,10000,100,ml)


aboPHSe <- multiMIPar(MIPHSe,pN,10000,10000,100,ml)
aboPHTSe <- multiMIPar(MIPHTSe,pN,10000,10000,100,ml)
aboDHSe <- multiMIPar(MIDHSe,pN,10000,10000,100,ml)
aboDHTSe <- multiMIPar(MIDHTSe,pN,10000,10000,100,ml)

pri <- cbind(GBD(aboPSe[[3]],3),GBD(aboPTSe[[3]],3),GBD(aboDSe[[3]],3),GBD(aboDTSe[[3]],3),
             GBD(aboPHSe[[3]],3),GBD(aboPHTSe[[3]],3),GBD(aboDHSe[[3]],3),GBD(aboDHTSe[[3]],3))

lst = data.frame(Syn = factor(c("P U","PT U","D U","DT U","PH U","PHT U","DH U","DHT U","P I","PT I","D I","DT I","PH I","PHT I","DH I","DHT I"),
                              levels = c("P U","PT U","D U","DT U","PH U","PHT U","DH U","DHT U","P I","PT I","D I","DT I","PH I","PHT I","DH I","DHT I")) ,
                 Clr = c("darkred","red","navy","blue","forestgreen","limegreen","black","grey","darkred","red","navy","blue","forestgreen","limegreen","black","grey"),
                 Lntp = c(replicate(8,1),replicate(8,2)),
                 Shp = replicate(16,19))

dat <- cbind(CIavg2ANPS9[[4]][[3]][,c(2,6,4,8)],CIavg2AOPS9[[4]][[3]][,c(2,6,4,8)],pri)

pltASe1 <- createPlot(dat,ml,lst)


aboPLSe <- multiMIPar(MIPLSe,pN,10000,10000,100,ml)
aboPLTSe <- multiMIPar(MIPLTSe,pN,10000,10000,100,ml)
aboDLSe <- multiMIPar(MIDLSe,pN,10000,10000,100,ml)
aboDLTSe <- multiMIPar(MIDLTSe,pN,10000,10000,100,ml)


aboCPSe <- multiMIPar(MICPSe,pN,10000,10000,100,ml)
aboCPTSe <- multiMIPar(MICPTSe,pN,10000,10000,100,ml)
aboCCSe <- multiMIPar(MICCSe,pN,10000,10000,100,ml)
aboCCTSe <- multiMIPar(MICCTSe,pN,10000,10000,100,ml)

pri <- cbind(GBD(aboPLSe[[3]],3),GBD(aboPLTSe[[3]],3),GBD(aboDLSe[[3]],3),GBD(aboDLTSe[[3]],3),
             GBD(aboCPSe[[3]],3),GBD(aboCPTSe[[3]],3),GBD(aboCCSe[[3]],3),GBD(aboCCTSe[[3]],3))

lst = data.frame(Syn = factor(c("PL U","PLT U","DL U","DLT U","CP U","CPT U","CC U","CCT U","PL I","PLT I","DL I","DLT I","CP I","CPT I","CC I","CCT I"),
                              levels = c("PL U","PLT U","DL U","DLT U","CP U","CPT U","CC U","CCT U","PL I","PLT I","DL I","DLT I","CP I","CPT I","CC I","CCT I")) ,
                 Clr = c("darkred","red","navy","blue","forestgreen","limegreen","black","grey","darkred","red","navy","blue","forestgreen","limegreen","black","grey"),
                 Lntp = c(replicate(8,1),replicate(8,2)),
                 Shp = replicate(16,19))

dat <- cbind(CIavg2AVPS9[[4]][[3]][,c(2,6,4,8)],CIavg2ASPS9[[4]][[3]][,c(2,6,4,8)],pri)

pltASe2 <- createPlot(dat,ml,lst)

rm(list = c(paste0("abo","",c("P","PT","D","DT"),"Se"),paste0("abo","",c("PH","PHT","DH","DHT"),"Se"),
            paste0("abo","",c("PL","PLT","DL","DLT"),"Se"),paste0("abo","",c("CP","CPT","CC","CCT"),"Se")))
rm(list = c(paste0("MI","",c("P","PT","D","DT"),"Se"),paste0("MI","",c("PH","PHT","DH","DHT"),"Se"),
            paste0("MI","",c("PL","PLT","DL","DLT"),"Se"),paste0("MI","",c("CP","CPT","CC","CCT"),"Se")))

##Save plot

plots <- paste0("plt",c("PH","AH","AHK","AS","A1","A2","ASe1","ASe2"))
for(c in plots){
  t.plot <- eval(as.name(c))
  ggsave(paste0("plots/",c,".pdf"),t.plot,device = "pdf")
}


## Maximum Knowledge

ml <- c(1:3,5,10,20,50,100)

##Polish
load("privacyUt.RData")
sapply(paste0("MK/MRP",c("","S"),"F2.RData"),function(x) load(x,.GlobalEnv))


pri <- cbind(AMR(MRPP2,c(1,20,3,4,21,6,7,22,9:12,23,24)),AMR(MRPPT2,c(1,20,3,4,21,6,7,22,9:12,23,24)),AMR(MRPD2,c(1,20,3,4,21,6,7,22,9:12,23,24)),AMR(MRPDT2,c(1,20,3,4,21,6,7,22,9:12,23,24)),
             AMR(MRPCP2,c(1,20,3,4,21,6,7,22,9:12,23,24)),AMR(MRPCPT2,c(1,20,3,4,21,6,7,22,9:12,23,24)),AMR(MRPCC2,c(1,20,3,4,21,6,7,22,9:12,23,24)),AMR(MRPCCT2,c(1,20,3,4,21,6,7,22,9:12,23,24)))

lst = data.frame(Syn = factor(c("P U","PT U","D U","DT U","CP U","CPT U","CC U","CCT U","P I","PT I","D I","DT I","CP I","CPT I","CC I","CCT I"),
                              levels = c("P U","PT U","D U","DT U","CP U","CPT U","CC U","CCT U","P I","PT I","D I","DT I","CP I","CPT I","CC I","CCT I")) ,
                 Clr = c("darkred","red","navy","blue","forestgreen","limegreen","black","grey","darkred","red","navy","blue","forestgreen","limegreen","black","grey"),
                 Lntp = c(replicate(8,1),replicate(8,2)),
                 Shp = replicate(16,19))

dat <- cbind(CIavg2PAsPS9[[4]][[3]][,1:8],pri)

pltPMH <- createPlot(dat,ml,lst)

rm(list = c(paste0("abo","P",c("P","PT","D","DT"),"2"),paste0("abo","P",c("CP","CPT","CC","CCT"),"2")))
rm(list = c(paste0("MR","P",c("P","PT","D","DT"),"2"),paste0("MR","P",c("CP","CPT","CC","CCT"),"2")))

##Avila
load("privacyUt.RData")
load("privacyUt2.RData")
sapply(paste0("MK/MRA",c("","S"),"F2.RData"),function(x) load(x,.GlobalEnv))


pri <- cbind(AMR(MRAP,c(1:11)),AMR(MRAPT,c(1:11)),AMR(MRAD,c(1:11)),AMR(MRADT,c(1:11)),
             AMR(MRACP,c(1:11)),AMR(MRACPT,c(1:11)),AMR(MRACC,c(1:11)),AMR(MRACCT,c(1:11)))

lst = data.frame(Syn = factor(c("P U","PT U","D U","DT U","CP U","CPT U","CC U","CCT U","P I","PT I","D I","DT I","CP I","CPT I","CC I","CCT I"),
                              levels = c("P U","PT U","D U","DT U","CP U","CPT U","CC U","CCT U","P I","PT I","D I","DT I","CP I","CPT I","CC I","CCT I")) ,
                 Clr = c("darkred","red","navy","blue","forestgreen","limegreen","black","grey","darkred","red","navy","blue","forestgreen","limegreen","black","grey"),
                 Lntp = c(replicate(8,1),replicate(8,2)),
                 Shp = replicate(16,19))

dat <- cbind(CIavg2PAsPS9[[4]][[3]][,9:16],pri)

pltAMH <- createPlot(dat,ml,lst)

dat <- cbind(CIavg2AKPS9[[4]][[3]],pri)

pltAMHK <- createPlot(dat,ml,lst)

rm(list = c(paste0("abo","A",c("P","PT","D","DT"),"2"),paste0("abo","A",c("CP","CPT","CC","CCT"),"2")))
rm(list = c(paste0("MR","A",c("P","PT","D","DT"),"2"),paste0("MR","A",c("CP","CPT","CC","CCT"),"2")))

##Adult
load("adult3.RData")
sapply(paste0("MK/MR",c("","H","L","S"),"F2.RData"),function(x) load(x,.GlobalEnv))


pri <- cbind(AMR(MRP2,c(18,2:8,19:21,12,13)),AMR(MRPT2,c(18,2:8,19:21,12,13)),AMR(MRD2,c(18,2:8,19:21,12,13)),AMR(MRDT2,c(18,2:8,19:21,12,13)),
             AMR(MRPH2,c(18,2:8,19:21,12,13)),AMR(MRPHT2,c(18,2:8,19:21,12,13)),AMR(MRDH2,c(18,2:8,19:21,12,13)),AMR(MRDHT2,c(18,2:8,19:21,12,13)))

lst = data.frame(Syn = factor(c("P U","PT U","D U","DT U","PH U","PHT U","DH U","DHT U","P I","PT I","D I","DT I","PH I","PHT I","DH I","DHT I"),
                              levels = c("P U","PT U","D U","DT U","PH U","PHT U","DH U","DHT U","P I","PT I","D I","DT I","PH I","PHT I","DH I","DHT I")) ,
                 Clr = c("darkred","red","navy","blue","forestgreen","limegreen","black","grey","darkred","red","navy","blue","forestgreen","limegreen","black","grey"),
                 Lntp = c(replicate(8,1),replicate(8,2)),
                 Shp = replicate(16,19))

dat <- cbind(CIavg2APS9[[4]][[3]][,c(2,4,3,5,6,8,7,9)],pri)

pltAM1 <- createPlot(dat,ml,lst)


pri <- cbind(AMR(MRPL2,c(18,2:8,19:21,12,13)),AMR(MRPLT2,c(18,2:8,19:21,12,13)),AMR(MRDL2,c(18,2:8,19:21,12,13)),AMR(MRDLT2,c(18,2:8,19:21,12,13)),
             AMR(MRCP2,c(18,2:8,19:21,12,13)),AMR(MRCPT2,c(18,2:8,19:21,12,13)),AMR(MRCC2,c(18,2:8,19:21,12,13)),AMR(MRCCT2,c(18,2:8,19:21,12,13)))

lst = data.frame(Syn = factor(c("PL U","PLT U","DL U","DLT U","CP U","CPT U","CC U","CCT U","PL I","PLT I","DL I","DLT I","CP I","CPT I","CC I","CCT I"),
                              levels = c("PL U","PLT U","DL U","DLT U","CP U","CPT U","CC U","CCT U","PL I","PLT I","DL I","DLT I","CP I","CPT I","CC I","CCT I")) ,
                 Clr = c("darkred","red","navy","blue","forestgreen","limegreen","black","grey","darkred","red","navy","blue","forestgreen","limegreen","black","grey"),
                 Lntp = c(replicate(8,1),replicate(8,2)),
                 Shp = replicate(16,19))

dat <- cbind(CIavg2APS9[[4]][[3]][,c(10,12,11,13,14,15,16,17)],pri)

pltAM2 <- createPlot(dat,ml,lst)

rm(list = c(paste0("abo","",c("P","PT","D","DT"),"2"),paste0("abo","",c("PH","PHT","DH","DHT"),"2"),
            paste0("abo","",c("PL","PLT","DL","DLT"),"2"),paste0("abo","",c("CP","CPT","CC","CCT"),"2")))
rm(list = c(paste0("MR","",c("P","PT","D","DT"),"2"),paste0("MR","",c("PH","PHT","DH","DHT"),"2"),
            paste0("MR","",c("PL","PLT","DL","DLT"),"2"),paste0("MR","",c("CP","CPT","CC","CCT"),"2")))

##Adult Smooth
load("privacyUt.RData")
sapply(paste0("MK/MR",c("","H","L","S"),"SmF2.RData"),function(x) load(x,.GlobalEnv))

pri <- cbind(AMR(MRDS2,c(18,2:8,19:21,12,13)),AMR(MRDTS2,c(18,2:8,19:21,12,13)),AMR(MRDHS2,c(18,2:8,19:21,12,13)),AMR(MRDHTS2,c(18,2:8,19:21,12,13)),AMR(MRDLS2,c(18,2:8,19:21,12,13)),AMR(MRDLTS2,c(18,2:8,19:21,12,13)),
             AMR(MRCPS2,c(18,2:8,19:21,12,13)),AMR(MRCPTS2,c(18,2:8,19:21,12,13)),AMR(MRCCS2,c(18,2:8,19:21,12,13)),AMR(MRCCTS2,c(18,2:8,19:21,12,13)))

lst = data.frame(Syn = factor(c("D U","DT U","DH U","DHT U","DL U","DLT U","CP U","CPT U","CC U","CCT U","D I","DT I","DH I","DHT I","DL I","DLT I","CP I","CPT I","CC I","CCT I"),
                              levels = c("D U","DT U","DH U","DHT U","DL U","DLT U","CP U","CPT U","CC U","CCT U","D I","DT I","DH I","DHT I","DL I","DLT I","CP I","CPT I","CC I","CCT I")) ,
                 Clr = c("darkred","red","navy","blue","purple","pink","forestgreen","limegreen","black","grey","darkred","red","navy","blue","purple","pink","forestgreen","limegreen","black","grey"),
                 Lntp = c(replicate(10,1),replicate(10,2)),
                 Shp = replicate(20,19))

dat <- cbind(CIavg2AsPS9[[4]][[3]],pri)

pltAMS <- createPlot(dat,ml,lst)

rm(list = c(paste0("abo","",c("D","DT"),"S2"),paste0("abo","",c("DH","DHT"),"S2"),
            paste0("abo","",c("DL","DLT"),"S2"),paste0("abo","",c("CP","CPT","CC","CCT"),"S2")))
rm(list = c(paste0("MR","",c("D","DT"),"S2"),paste0("MR","",c("DH","DHT"),"S2"),
            paste0("MR","",c("DL","DLT"),"S2"),paste0("MR","",c("CP","CPT","CC","CCT"),"S2")))

##Adult Selective
load("adultSe.RData")
sapply(paste0("MK/MR",c("","H","L","S"),"SeF2.RData"),function(x) load(x,.GlobalEnv))

pri <- cbind(AMR(MRPSe2,c(18,2:8,19:21,12,13)),AMR(MRPTSe2,c(18,2:8,19:21,12,13)),AMR(MRDSe2,c(18,2:8,19:21,12,13)),AMR(MRDTSe2,c(18,2:8,19:21,12,13)),
             AMR(MRPHSe2,c(18,2:8,19:21,12,13)),AMR(MRPHTSe2,c(18,2:8,19:21,12,13)),AMR(MRDHSe2,c(18,2:8,19:21,12,13)),AMR(MRDHTSe2,c(18,2:8,19:21,12,13)))

lst = data.frame(Syn = factor(c("P U","PT U","D U","DT U","PH U","PHT U","DH U","DHT U","P I","PT I","D I","DT I","PH I","PHT I","DH I","DHT I"),
                              levels = c("P U","PT U","D U","DT U","PH U","PHT U","DH U","DHT U","P I","PT I","D I","DT I","PH I","PHT I","DH I","DHT I")) ,
                 Clr = c("darkred","red","navy","blue","forestgreen","limegreen","black","grey","darkred","red","navy","blue","forestgreen","limegreen","black","grey"),
                 Lntp = c(replicate(8,1),replicate(8,2)),
                 Shp = replicate(16,19))

dat <- cbind(CIavg2ANPS9[[4]][[3]][,c(2,6,4,8)],CIavg2AOPS9[[4]][[3]][,c(2,6,4,8)],pri)

pltAMSe1 <- createPlot(dat,ml,lst)


pri <- cbind(AMR(MRPLSe2,c(18,2:8,19:21,12,13)),AMR(MRPLTSe2,c(18,2:8,19:21,12,13)),AMR(MRDLSe2,c(18,2:8,19:21,12,13)),AMR(MRDLTSe2,c(18,2:8,19:21,12,13)),
             AMR(MRCPSe2,c(18,2:8,19:21,12,13)),AMR(MRCPTSe2,c(18,2:8,19:21,12,13)),AMR(MRCCSe2,c(18,2:8,19:21,12,13)),AMR(MRCCTSe2,c(18,2:8,19:21,12,13)))

lst = data.frame(Syn = factor(c("PL U","PLT U","DL U","DLT U","CP U","CPT U","CC U","CCT U","PL I","PLT I","DL I","DLT I","CP I","CPT I","CC I","CCT I"),
                              levels = c("PL U","PLT U","DL U","DLT U","CP U","CPT U","CC U","CCT U","PL I","PLT I","DL I","DLT I","CP I","CPT I","CC I","CCT I")) ,
                 Clr = c("darkred","red","navy","blue","forestgreen","limegreen","black","grey","darkred","red","navy","blue","forestgreen","limegreen","black","grey"),
                 Lntp = c(replicate(8,1),replicate(8,2)),
                 Shp = replicate(16,19))

dat <- cbind(CIavg2AVPS9[[4]][[3]][,c(2,6,4,8)],CIavg2ASPS9[[4]][[3]][,c(2,6,4,8)],pri)

pltAMSe2 <- createPlot(dat,ml,lst)

rm(list = c(paste0("abo","",c("P","PT","D","DT"),"Se2"),paste0("abo","",c("PH","PHT","DH","DHT"),"Se2"),
            paste0("abo","",c("PL","PLT","DL","DLT"),"Se2"),paste0("abo","",c("CP","CPT","CC","CCT"),"Se2")))
rm(list = c(paste0("MR","",c("P","PT","D","DT"),"Se2"),paste0("MR","",c("PH","PHT","DH","DHT"),"Se2"),
            paste0("MR","",c("PL","PLT","DL","DLT"),"Se2"),paste0("MR","",c("CP","CPT","CC","CCT"),"Se2")))

##Save plot

plots <- paste0("plt",c("PMH","AMH","AMHK","AMS","AM1","AM2","AMSe1","AMSe2"))
for(c in plots){
  t.plot <- eval(as.name(c))
  ggsave(paste0("plots/",c,".pdf"),t.plot,device = "pdf")
}


## Maximum Knowledge 2

sapply(paste0("MK/MR",c("","H","L","S"),"F2.RData"),function(x) load(x,.GlobalEnv))
sapply(paste0("MK/MR",c("","H","L","S"),"SmF2.RData"),function(x) load(x,.GlobalEnv))
sapply(paste0("MK/MR",c("","H","L","S"),"SeF2.RData"),function(x) load(x,.GlobalEnv))

c(paste0("MR","",c("P","PT","D","DT"),"2"),paste0("MR","",c("PH","PHT","DH","DHT"),"2"),
  paste0("MR","",c("PL","PLT","DL","DLT"),"2"),paste0("MR","",c("CP","CPT","CC","CCT"),"2"))
c(paste0("MR","",c("D","DT"),"S2"),paste0("MR","",c("DH","DHT"),"S2"),
  paste0("MR","",c("DL","DLT"),"S2"),paste0("MR","",c("CP","CPT","CC","CCT"),"S2"))
c(paste0("MR","",c("P","PT","D","DT"),"Se2"),paste0("MR","",c("PH","PHT","DH","DHT"),"Se2"),
  paste0("MR","",c("PL","PLT","DL","DLT"),"Se2"),paste0("MR","",c("CP","CPT","CC","CCT"),"Se2"))

pasteC(list("MR",c("P","D"),c("","H","L"),c("","T"),c("","Se"),"2"))
pasteC(list("MR","D",c("","H","L"),c("","T"),"S2"))
pasteC(list("MRC",c("P","C"),c("","T"),c("","S","Se"),"2"))

MRK2 <- MRT(c(pasteC(list("MR",c("P","D"),c("","H","L"),c("","T"),c("","Se"),"2")),
             pasteC(list("MR","D",c("","H","L"),c("","T"),"S2")),
             pasteC(list("MRC",c("P","C"),c("","T"),c("","S","Se"),"2"))),c(18,2:8,19:21,12,13),5)

rownames(MRK2) <- c(pasteC(list(c("P","D"),c("","H","L"),c("","T"),c("","Se"))),
                   pasteC(list("D",c("","H","L"),c("","T"),"S")),
                   pasteC(list("C",c("P","C"),c("","T"),c("","S","Se"))))

MRK2N <- MRK2[c(seq(1,24,2),seq(31,42,3)),]
MRK2S <- MRK2[c(seq(25,30,1),seq(32,42,3)),]
MRK2Se <- MRK2[c(seq(2,24,2),seq(33,42,3)),]


## Polish and Avila

sapply(paste0("MK/MRP",c("","S"),"F2.RData"),function(x) load(x,.GlobalEnv))
sapply(paste0("MK/MRA",c("","S"),"F2.RData"),function(x) load(x,.GlobalEnv))

c(paste0("MR","P",c("P","PT","D","DT"),"2"),paste0("MR","P",c("CP","CPT","CC","CCT"),"2"))
c(paste0("MR","A",c("P","PT","D","DT"),"2"),paste0("MR","A",c("CP","CPT","CC","CCT"),"2"))

pasteC(list("MR","P",c("P","D","CP","CC"),c("","T"),"2"))
pasteC(list("MR","A",c("P","D","CP","CC"),c("","T"),"2"))

MRPK2 <- MRT(pasteC(list("MR","P",c("P","D","CP","CC"),c("","T"),"2")),c(1,20,3,4,21,6,7,22,9:12,23,24),5)

MRAK2 <- MRT(pasteC(list("MR","A",c("P","D","CP","CC"),c("","T"),"2")),c(1:11),5)


## Extra

pN <- 10^seq(-50,20,1)
pN <- c(rbind(pN,2*pN,3*pN,5*pN))[1:(4*length(pN)-3)]
ml <- c(1:3,5,10,20,50,100)

##Adult
load("adult3.RData")
load("MIN.RData")

alm <- c(pasteC(list(c("D","P"),c("","H","L"),c("","T"))),pasteC(list("C",c("P","C"),c("","T"))))
sapply(alm,function(x) assign(paste0("abo",x),multiMIPar(eval(as.name(paste0("MI",x))),pN,10000,10000,100,ml),envir = globalenv()))

pri <- do.call(cbind,lapply(alm,function(x) GBD(eval(as.name(paste0("abo",x)))[[3]],3)))

lst = data.frame(Syn = factor(c("P U","PT U","D U","DT U","PH U","PHT U","DH U","DHT U","P I","PT I","D I","DT I","PH I","PHT I","DH I","DHT I"),
                              levels = c("P U","PT U","D U","DT U","PH U","PHT U","DH U","DHT U","P I","PT I","D I","DT I","PH I","PHT I","DH I","DHT I")) ,
                 Clr = c("darkred","red","navy","blue","forestgreen","limegreen","black","grey","darkred","red","navy","blue","forestgreen","limegreen","black","grey"),
                 Lntp = c(replicate(8,1),replicate(8,2)),
                 Shp = replicate(16,19))

dat <- cbind(CIavg2APS9[[4]][[3]][,c(2,4,3,5,6,8,7,9)],pri[,c(7,8,1,2,9,10,3,4)])

pltA1 <- createPlot(dat,ml,lst) + geom_point() + scale_x_log10(breaks=ml) + ylim(0,1) +
  labs(y = "Accuracy",title = "Adult Utility and Membership Inference - 1")


lst = data.frame(Syn = factor(c("PL U","PLT U","DL U","DLT U","CP U","CPT U","CC U","CCT U","PL I","PLT I","DL I","DLT I","CP I","CPT I","CC I","CCT I"),
                              levels = c("PL U","PLT U","DL U","DLT U","CP U","CPT U","CC U","CCT U","PL I","PLT I","DL I","DLT I","CP I","CPT I","CC I","CCT I")) ,
                 Clr = c("darkred","red","navy","blue","forestgreen","limegreen","black","grey","darkred","red","navy","blue","forestgreen","limegreen","black","grey"),
                 Lntp = c(replicate(8,1),replicate(8,2)),
                 Shp = replicate(16,19))

dat <- cbind(CIavg2APS9[[4]][[3]][,c(10,12,11,13,14,15,16,17)],pri[,c(11,12,5,6,13,14,15,16)])

pltA2 <- createPlot(dat,ml,lst) + geom_point() + scale_x_log10(breaks=ml) + ylim(0,1) +
  labs(y = "Accuracy",title = "Adult Utility and Membership Inference - 2")

rm(list = paste0("abo",alm))
rm(list = paste0("MI",alm))

##Adult Smooth
load("privacyUt.RData")
load("MIS.RData")

almS <- c(pasteC(list("D",c("","H","L"),c("","T"),"S")),pasteC(list("C",c("P","C"),c("","T"),"S")))
sapply(almS,function(x) assign(paste0("abo",x),multiMIPar(eval(as.name(paste0("MI",x))),pN,10000,10000,100,ml),envir = globalenv()))

pri <- do.call(cbind,lapply(almS,function(x) GBD(eval(as.name(paste0("abo",x)))[[3]],3)))

lst = data.frame(Syn = factor(c("D U","DT U","DH U","DHT U","DL U","DLT U","CP U","CPT U","CC U","CCT U","D I","DT I","DH I","DHT I","DL I","DLT I","CP I","CPT I","CC I","CCT I"),
                              levels = c("D U","DT U","DH U","DHT U","DL U","DLT U","CP U","CPT U","CC U","CCT U","D I","DT I","DH I","DHT I","DL I","DLT I","CP I","CPT I","CC I","CCT I")) ,
                 Clr = c("darkred","red","navy","blue","purple","pink","forestgreen","limegreen","black","grey","darkred","red","navy","blue","purple","pink","forestgreen","limegreen","black","grey"),
                 Lntp = c(replicate(10,1),replicate(10,2)),
                 Shp = replicate(20,19))

dat <- cbind(CIavg2AsPS9[[4]][[3]],pri)

pltAS <- createPlot(dat,ml,lst) + geom_point() + scale_x_log10(breaks=ml) + ylim(0,1) +
  labs(y = "Accuracy",title = "Adult Smooth Utility and Membership Inference")

rm(list = paste0("abo",almS))
rm(list = paste0("MI",almS))

##Adult Selective
load("adultSe.RData")
load("MISe.RData")

almSe <- c(pasteC(list(c("D","P"),c("","H","L"),c("","T"),"Se")),pasteC(list("C",c("P","C"),c("","T"),"Se")))
sapply(almSe,function(x) assign(paste0("abo",x),multiMIPar(eval(as.name(paste0("MI",x))),pN,10000,10000,100,ml),envir = globalenv()))

pri <- do.call(cbind,lapply(almSe,function(x) GBD(eval(as.name(paste0("abo",x)))[[3]],3)))

lst = data.frame(Syn = factor(c("P U","PT U","D U","DT U","PH U","PHT U","DH U","DHT U","P I","PT I","D I","DT I","PH I","PHT I","DH I","DHT I"),
                              levels = c("P U","PT U","D U","DT U","PH U","PHT U","DH U","DHT U","P I","PT I","D I","DT I","PH I","PHT I","DH I","DHT I")) ,
                 Clr = c("darkred","red","navy","blue","forestgreen","limegreen","black","grey","darkred","red","navy","blue","forestgreen","limegreen","black","grey"),
                 Lntp = c(replicate(8,1),replicate(8,2)),
                 Shp = replicate(16,19))

dat <- cbind(CIavg2ANPS9[[4]][[3]][,c(2,6,4,8)],CIavg2AOPS9[[4]][[3]][,c(2,6,4,8)],pri[,c(7,8,1,2,9,10,3,4)])

pltASe1 <- createPlot(dat,ml,lst) + geom_point() + scale_x_log10(breaks=ml) + ylim(0,1) +
  labs(y = "Accuracy",title = "Adult Selective Utility and Membership Inference - 1")


lst = data.frame(Syn = factor(c("PL U","PLT U","DL U","DLT U","CP U","CPT U","CC U","CCT U","PL I","PLT I","DL I","DLT I","CP I","CPT I","CC I","CCT I"),
                              levels = c("PL U","PLT U","DL U","DLT U","CP U","CPT U","CC U","CCT U","PL I","PLT I","DL I","DLT I","CP I","CPT I","CC I","CCT I")) ,
                 Clr = c("darkred","red","navy","blue","forestgreen","limegreen","black","grey","darkred","red","navy","blue","forestgreen","limegreen","black","grey"),
                 Lntp = c(replicate(8,1),replicate(8,2)),
                 Shp = replicate(16,19))

dat <- cbind(CIavg2AVPS9[[4]][[3]][,c(2,6,4,8)],CIavg2ASPS9[[4]][[3]][,c(2,6,4,8)],pri[,c(11,12,5,6,13,14,15,16)])

pltASe2 <- createPlot(dat,ml,lst) + geom_point() + scale_x_log10(breaks=ml) + ylim(0,1) +
  labs(y = "Accuracy",title = "Adult Selective Utility and Membership Inference - 2")

rm(list = paste0("abo",almSe))
rm(list = paste0("MI",almSe))

##Polish
load("privacyUt.RData")
load("MIP.RData")

almP <- pasteC(list("P",c("D","P","CP","CC"),c("","T")))
sapply(almP,function(x) assign(paste0("abo",x),multiMIPar(eval(as.name(paste0("MI",x))),pN,2500,2500,100,ml),envir = globalenv()))

pri <- do.call(cbind,lapply(almP,function(x) GBD(eval(as.name(paste0("abo",x)))[[3]],3)))

lst = data.frame(Syn = factor(c("P U","PT U","D U","DT U","CP U","CPT U","CC U","CCT U","P I","PT I","D I","DT I","CP I","CPT I","CC I","CCT I"),
                              levels = c("P U","PT U","D U","DT U","CP U","CPT U","CC U","CCT U","P I","PT I","D I","DT I","CP I","CPT I","CC I","CCT I")) ,
                 Clr = c("darkred","red","navy","blue","forestgreen","limegreen","black","grey","darkred","red","navy","blue","forestgreen","limegreen","black","grey"),
                 Lntp = c(replicate(8,1),replicate(8,2)),
                 Shp = replicate(16,19))

dat <- cbind(CIavg2PAsPS9[[4]][[3]][,1:8],pri[,c(3,4,1,2,5:8)])

pltPH <- createPlot(dat,ml,lst) + geom_point() + scale_x_log10(breaks=ml) + ylim(0,1) +
  labs(y = "Accuracy",title = "Polish Utility and Membership Inference")

rm(list = paste0("abo",almP))
rm(list = paste0("MI",almP))

##Avila
load("privacyUt.RData")
load("privacyUt2.RData")
load("MIA.RData")

almA <- pasteC(list("A",c("D","P","CP","CC"),c("","T")))
sapply(almA,function(x) assign(paste0("abo",x),multiMIPar(eval(as.name(paste0("MI",x))),pN,5215,5215,100,ml),envir = globalenv()))

pri <- do.call(cbind,lapply(almA,function(x) GBD(eval(as.name(paste0("abo",x)))[[3]],3)))

lst = data.frame(Syn = factor(c("P U","PT U","D U","DT U","CP U","CPT U","CC U","CCT U","P I","PT I","D I","DT I","CP I","CPT I","CC I","CCT I"),
                              levels = c("P U","PT U","D U","DT U","CP U","CPT U","CC U","CCT U","P I","PT I","D I","DT I","CP I","CPT I","CC I","CCT I")) ,
                 Clr = c("darkred","red","navy","blue","forestgreen","limegreen","black","grey","darkred","red","navy","blue","forestgreen","limegreen","black","grey"),
                 Lntp = c(replicate(8,1),replicate(8,2)),
                 Shp = replicate(16,19))

dat <- cbind(CIavg2PAsPS9[[4]][[3]][,9:16],pri[,c(3,4,1,2,5:8)])

pltAH <- createPlot(dat,ml,lst) + geom_point() + scale_x_log10(breaks=ml) + ylim(0,1) +
  labs(y = "Accuracy",title = "Avila Utility and Membership Inference")

dat <- cbind(CIavg2AKPS9[[4]][[3]],pri[,c(3,4,1,2,5:8)])

pltAHK <- createPlot(dat,ml,lst) + geom_point() + scale_x_log10(breaks=ml) + ylim(0,1) +
  labs(y = "Accuracy",title = "Avila Utility (K-anonymized variables) and Membership Inference")

rm(list = paste0("abo",almA))
rm(list = paste0("MI",almA))

## Save plots

plots <- paste0("plt",c("PH","AH","AHK","AS","A1","A2","ASe1","ASe2"))
for(c in plots){
  t.plot <- eval(as.name(c))
  ggsave(paste0("plots2/",c,"_UI.pdf"),t.plot,device = "pdf")
}



## Extra MK

ml <- c(1:3,5,10,20,50,100)

##Adult
load("adult3.RData")
sapply(paste0("MK/MR",c("","H","L","S"),"F2.RData"),function(x) load(x,.GlobalEnv))

cho <- c(18,2:8,19:21,12,13)

alm <- c(pasteC(list(c("D","P"),c("","H","L"),c("","T"))),pasteC(list("C",c("P","C"),c("","T"))))

pri <- do.call(cbind,lapply(alm,function(x) apply(eval(as.name(paste0("MR",x,"2")))[,cho],1,mean)))

paste0(c("PL","PLT","DL","DLT","CP","CPT","CC","CCT")," U")

lst = data.frame(Syn = factor(c(paste0(c("P","PT","D","DT","PH","PHT","DH","DHT")," U"),paste0(c("P","PT","D","DT","PH","PHT","DH","DHT")," MK")),
                              levels = c(paste0(c("P","PT","D","DT","PH","PHT","DH","DHT")," U"),paste0(c("P","PT","D","DT","PH","PHT","DH","DHT")," MK"))) ,
                 Clr = c("darkred","red","navy","blue","forestgreen","limegreen","black","grey","darkred","red","navy","blue","forestgreen","limegreen","black","grey"),
                 Lntp = c(replicate(8,1),replicate(8,2)),
                 Shp = replicate(16,19))

dat <- cbind(CIavg2APS9[[4]][[3]][,c(2,4,3,5,6,8,7,9)],pri[,c(7,8,1,2,9,10,3,4)])

pltA1 <- createPlot(dat,ml,lst) + geom_point() + scale_x_log10(breaks=ml) + ylim(0,1) +
  labs(y = "Accuracy",title = "Adult Utility and Maximum Knowledge - 1")


lst = data.frame(Syn = factor(c(paste0(c("PL","PLT","DL","DLT","CP","CPT","CC","CCT")," U"),paste0(c("PL","PLT","DL","DLT","CP","CPT","CC","CCT")," MK")),
                              levels = c(paste0(c("PL","PLT","DL","DLT","CP","CPT","CC","CCT")," U"),paste0(c("PL","PLT","DL","DLT","CP","CPT","CC","CCT")," MK"))) ,
                 Clr = c("darkred","red","navy","blue","forestgreen","limegreen","black","grey","darkred","red","navy","blue","forestgreen","limegreen","black","grey"),
                 Lntp = c(replicate(8,1),replicate(8,2)),
                 Shp = replicate(16,19))

dat <- cbind(CIavg2APS9[[4]][[3]][,c(10,12,11,13,14,15,16,17)],pri[,c(11,12,5,6,13,14,15,16)])

pltA2 <- createPlot(dat,ml,lst) + geom_point() + scale_x_log10(breaks=ml) + ylim(0,1) +
  labs(y = "Accuracy",title = "Adult Utility and Maximum Knowledge - 2")

rm(list = paste0("MR",alm,"2"))

##Adult Smooth
load("privacyUt.RData")
sapply(paste0("MK/MR",c("","H","L","S"),"SmF2.RData"),function(x) load(x,.GlobalEnv))

almS <- c(pasteC(list("D",c("","H","L"),c("","T"),"S")),pasteC(list("C",c("P","C"),c("","T"),"S")))

pri <- do.call(cbind,lapply(almS,function(x) apply(eval(as.name(paste0("MR",x,"2")))[,cho],1,mean)))

paste0(c("D","DT","DH","DHT","DL","DLT","CP","CPT","CC","CCT")," U")

lst = data.frame(Syn = factor(c(paste0(c("D","DT","DH","DHT","DL","DLT","CP","CPT","CC","CCT")," U"),paste0(c("D","DT","DH","DHT","DL","DLT","CP","CPT","CC","CCT")," MK")),
                              levels = c(paste0(c("D","DT","DH","DHT","DL","DLT","CP","CPT","CC","CCT")," U"),paste0(c("D","DT","DH","DHT","DL","DLT","CP","CPT","CC","CCT")," MK"))) ,
                 Clr = c("darkred","red","navy","blue","purple","pink","forestgreen","limegreen","black","grey","darkred","red","navy","blue","purple","pink","forestgreen","limegreen","black","grey"),
                 Lntp = c(replicate(10,1),replicate(10,2)),
                 Shp = replicate(20,19))

dat <- cbind(CIavg2AsPS9[[4]][[3]],pri)

pltAS <- createPlot(dat,ml,lst) + geom_point() + scale_x_log10(breaks=ml) + ylim(0,1) +
  labs(y = "Accuracy",title = "Adult Smooth Utility and Maximum Knowledge")

rm(list = paste0("MR",almS,"2"))

##Adult Selective
load("adultSe.RData")
sapply(paste0("MK/MR",c("","H","L","S"),"SeF2.RData"),function(x) load(x,.GlobalEnv))

almSe <- c(pasteC(list(c("D","P"),c("","H","L"),c("","T"),"Se")),pasteC(list("C",c("P","C"),c("","T"),"Se")))

pri <- do.call(cbind,lapply(almSe,function(x) apply(eval(as.name(paste0("MR",x,"2")))[,cho],1,mean)))

paste0(c("PL","PLT","DL","DLT","CP","CPT","CC","CCT")," U")

lst = data.frame(Syn = factor(c(paste0(c("P","PT","D","DT","PH","PHT","DH","DHT")," U"),paste0(c("P","PT","D","DT","PH","PHT","DH","DHT")," MK")),
                              levels = c(paste0(c("P","PT","D","DT","PH","PHT","DH","DHT")," U"),paste0(c("P","PT","D","DT","PH","PHT","DH","DHT")," MK"))) ,
                 Clr = c("darkred","red","navy","blue","forestgreen","limegreen","black","grey","darkred","red","navy","blue","forestgreen","limegreen","black","grey"),
                 Lntp = c(replicate(8,1),replicate(8,2)),
                 Shp = replicate(16,19))

dat <- cbind(CIavg2ANPS9[[4]][[3]][,c(2,6,4,8)],CIavg2AOPS9[[4]][[3]][,c(2,6,4,8)],pri[,c(7,8,1,2,9,10,3,4)])

pltASe1 <- createPlot(dat,ml,lst) + geom_point() + scale_x_log10(breaks=ml) + ylim(0,1) +
  labs(y = "Accuracy",title = "Adult Selective Utility and Maximum Knowledge - 1")


lst = data.frame(Syn = factor(c(paste0(c("PL","PLT","DL","DLT","CP","CPT","CC","CCT")," U"),paste0(c("PL","PLT","DL","DLT","CP","CPT","CC","CCT")," MK")),
                              levels = c(paste0(c("PL","PLT","DL","DLT","CP","CPT","CC","CCT")," U"),paste0(c("PL","PLT","DL","DLT","CP","CPT","CC","CCT")," MK"))) ,
                 Clr = c("darkred","red","navy","blue","forestgreen","limegreen","black","grey","darkred","red","navy","blue","forestgreen","limegreen","black","grey"),
                 Lntp = c(replicate(8,1),replicate(8,2)),
                 Shp = replicate(16,19))

dat <- cbind(CIavg2AVPS9[[4]][[3]][,c(2,6,4,8)],CIavg2ASPS9[[4]][[3]][,c(2,6,4,8)],pri[,c(11,12,5,6,13,14,15,16)])

pltASe2 <- createPlot(dat,ml,lst) + geom_point() + scale_x_log10(breaks=ml) + ylim(0,1) +
  labs(y = "Accuracy",title = "Adult Selective Utility and Maximum Knowledge - 2")

rm(list = paste0("MR",almSe,"2"))

##Polish
choP <- c(1,20,3,4,21,6,7,22,9:12,23,24)
load("privacyUt.RData")
sapply(paste0("MK/MRP",c("","S"),"F2.RData"),function(x) load(x,.GlobalEnv))

almP <- pasteC(list("P",c("D","P","CP","CC"),c("","T")))

pri <- do.call(cbind,lapply(almP,function(x) apply(eval(as.name(paste0("MR",x,"2")))[,choP],1,mean)))

paste0(c("P","PT","D","DT","CP","CPT","CC","CCT")," U")

lst = data.frame(Syn = factor(c(paste0(c("P","PT","D","DT","CP","CPT","CC","CCT")," U"),paste0(c("P","PT","D","DT","CP","CPT","CC","CCT")," MK")),
                              levels = c(paste0(c("P","PT","D","DT","CP","CPT","CC","CCT")," U"),paste0(c("P","PT","D","DT","CP","CPT","CC","CCT")," MK"))) ,
                 Clr = c("darkred","red","navy","blue","forestgreen","limegreen","black","grey","darkred","red","navy","blue","forestgreen","limegreen","black","grey"),
                 Lntp = c(replicate(8,1),replicate(8,2)),
                 Shp = replicate(16,19))

dat <- cbind(CIavg2PAsPS9[[4]][[3]][,1:8],pri[,c(3,4,1,2,5:8)])

pltPH <- createPlot(dat,ml,lst) + geom_point() + scale_x_log10(breaks=ml) + ylim(0,1) +
  labs(y = "Accuracy",title = "Polish Utility and Maximum Knowledge")

rm(list = paste0("MR",almP,"2"))

##Avila
choA <- c(11,22:31)
load("privacyUt.RData")
load("privacyUt2.RData")
sapply(paste0("MK/MRA",c("","S"),"F2.RData"),function(x) load(x,.GlobalEnv))

almA <- pasteC(list("A",c("D","P","CP","CC"),c("","T")))

pri <- do.call(cbind,lapply(almA,function(x) apply(eval(as.name(paste0("MR",x,"2")))[,choA],1,mean)))

lst = data.frame(Syn = factor(c(paste0(c("P","PT","D","DT","CP","CPT","CC","CCT")," U"),paste0(c("P","PT","D","DT","CP","CPT","CC","CCT")," MK")),
                              levels = c(paste0(c("P","PT","D","DT","CP","CPT","CC","CCT")," U"),paste0(c("P","PT","D","DT","CP","CPT","CC","CCT")," MK"))) ,
                 Clr = c("darkred","red","navy","blue","forestgreen","limegreen","black","grey","darkred","red","navy","blue","forestgreen","limegreen","black","grey"),
                 Lntp = c(replicate(8,1),replicate(8,2)),
                 Shp = replicate(16,19))

dat <- cbind(CIavg2PAsPS9[[4]][[3]][,9:16],pri[,c(3,4,1,2,5:8)])

pltAH <- createPlot(dat,ml,lst) + geom_point() + scale_x_log10(breaks=ml) + ylim(0,1) +
  labs(y = "Accuracy",title = "Avila Utility and Maximum Knowledge")

dat <- cbind(CIavg2AKPS9[[4]][[3]],pri[,c(3,4,1,2,5:8)])

pltAHK <- createPlot(dat,ml,lst) + geom_point() + scale_x_log10(breaks=ml) + ylim(0,1) +
  labs(y = "Accuracy",title = "Avila Utility (K-anonymized variables) and Maximum Knowledge")

rm(list = paste0("MR",almA,"2"))

## Save plots

plots <- paste0("plt",c("PH","AH","AHK","AS","A1","A2","ASe1","ASe2"))
for(c in plots){
  t.plot <- eval(as.name(c))
  ggsave(paste0("plots2/",c,"_UMK.pdf"),t.plot,device = "pdf")
}