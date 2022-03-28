library("float")

gausSmooth <- function(omega_d,sigma){
  gss <- 1/sqrt(2*pi*sigma)*exp(-omega_d^2/(2*sigma^2))
  #for(i in c(1:length(sigma))){
  #  gss[[i]] <- 1/sqrt(2*pi*sigma[[i]])*exp(-omega_d[[i]]^2/(2*sigma[[i]]^2))
  #}
  return(gss)
}

#Used to check the correctness of GPU vector sum function together with parTestLoop.
parTest <- function(vect,width,n_v){
  #n_v <- length(vect)/m
  i = 0
  while(i*width + (width / 2) < n_v){
    vect[[i*width + 1]] <- vect[[i*width + 1]] + vect[[i*width + (width / 2) + 1]]
    i <- i + 1
  }
  return(vect)
}

#Used to check the correctness of GPU vector sum function.
#m is number of GPU SM
parTestLoop <- function(vect,m){
  width = 2
  n_v <- length(vect)
  n_vm <- round(n_v/m)
  while(width < 2 * n_vm){
    for(i in c(1:m)){
      vect[c(((i-1)*n_vm +1):(i*n_vm))] <- parTest(vect[c(((i-1)*n_vm +1):(i*n_vm))],width,n_vm)
    }
    width <- 2*width
  }
  return(vect)
}

parIns <- function(vect,width){
  n_v <- length(vect)
  i = 0
  while(i*width < n_v){
    vect[[i + 1]] <- vect[[i*width + 1]]
    i <- i + 1
  }
  return(vect)
}

#CPU function to calculate membership inference. TOO SLOW FOR PRACTICAL USE!
findIncl <- function(ods,sdsN,sigma,m = 100){
  sds <- eval(as.name(sdsN))
  odsT <- do.call(cbind,ods)
  baseL <- prod(gausSmooth(odsT[1,] - odsT[1,],sigma))
  n <- nrow(odsT)
  k <- sds$k
  #m <- sds$m
  #c_list <- list()
  message("PRE; ",appendLF = FALSE)
  sdsT <- NULL
  omega_d <- NULL
  pre <- NULL
  ans <- NULL
  sdsM <- matrix(NA,k*m,ncol(odsT))
  cc_ans <- matrix(0,2,n)
  for(j in c(1:m)){
    sdsT <- do.call(cbind,sds$syn[[j]])
    sdsM[c(((j-1)*k + 1):(j*k)),] <- sdsT
  }
  cc_ans <- matrix(0,2,n)
  message("START: ",appendLF = FALSE)
  for(i in c(1:n)){
    message(paste0(i,", "),appendLF = FALSE)
    ods_c <- odsT[i,]
    c_ans <- matrix(0,2,m)
    for(j in c(1:m)){
      sdsT <- sdsM[c(((j-1)*k + 1):(j*k)),]
      omega_d <- abs(sdsT - rep(ods_c, each = nrow(sdsT)))
      pre <- t(apply(omega_d,1,function(x) gausSmooth(x,sigma)))
      ans <- apply(pre,1,prod)
      c_ans[1,j] <- sum(ans)
      c_ans[2,j] <- sum(ans < baseL)
    }
    cc_ans[,i] <- apply(c_ans,1,sum)
    #c_list <- c(c_list,list(c_ans))
  }
  message("DONE",appendLF = TRUE)
  return(cc_ans)
}

#GPU function to calculate membership inference.
#ods is the dataset object.
#sdsN is the object name of the synthetic dataset.
#sigma is the weight used for the gaussian function.
#mc is the selected synthetic datasets.
#smooth is whether to smooth the numerical values on the fly.
#ori is the records used from ods to generate sds.
#returns matrix with "membership"-values for each record in ods.
findPar <- function(ods,sdsN,sigma,mc = c(1:100),smooth = FALSE,ori = NULL){
  if(!is.loaded("memberInf")){
    dyn.load("member_inference.dll")
  }
  sds <- eval(as.name(sdsN))
  odsT <- do.call(cbind,ods)
  if(smooth == TRUE) smt <- sapply(ods,is.numeric)
  n <- nrow(odsT)
  k <- sds$k
  m <- length(mc)#200#sds$m
  sdsM <- matrix(NA,ncol(odsT),k*m)
  cc_ans <- rep.int(0,n*m)
  perc <- sapply(seq(0.1,0.9,0.1)*m,round)
  message("SDS: ",appendLF = FALSE)
  for(j in c(1:m)){
    sdsT <- do.call(cbind,sds$syn[[ mc[[j]] ]])
    if(smooth == TRUE){
      for(i in c(1:ncol(ods))[smt]){
        sdsT[,i] <- syn.smooth(sdsT[,i],ods[ori,i])
      }
    }
    sdsM[,c(((j-1)*k + 1):(j*k))] <- t(sdsT)
    if(j %in% perc){
      message(paste0(seq(0.1,0.9,0.1)[perc %in% j],", "),appendLF = FALSE)
    }
  }
  message("SDS END ",appendLF = TRUE)
  ans <- .C("memberInf",
            cc = as.integer(cc_ans),
            as.float(sdsM)@Data,
            as.float(t(odsT))@Data,
            as.float(sigma)@Data,
            as.integer(ncol(odsT)*k*m),
            as.integer(ncol(odsT)*n),
            as.integer(length(sigma)),
            as.integer(m))
  ans <- float32(t(matrix(ans[["cc"]],m,n)))
  return(ans)
}

#Create binary file used to debug GPU code.
createFile <- function(ds,fileName,int = FALSE,tra = TRUE){
  if(class(ds) == "synds"){
    c <- ncol(ds$syn[[1]])
    k <- ds$k
    m <- ds$m
    dsM <- matrix(NA,c,k*m)
    for(j in c(1:m)){
      dsT <- do.call(cbind,ds$syn[[j]])
      dsM[,c(((j-1)*k + 1):(j*k))] <- t(dsT)
    }
  }
  else{
    if(tra == TRUE){
      dsM <- t(do.call(cbind,ds))
    }
    else{
      dsM <- do.call(cbind,ds)
    }
  }
  if(int){
    writeBin(as.integer(dsM),fileName)
  }
  else{
    writeBin(as.double(dsM),fileName)
  }
}

printTime <- function(time){
  for(i in c(2:length(time))){
    print(time[[i]] - time[[i-1]])
  }
}

createPlot <- function(data,mSet,setting){
  ms <- data.frame(mSet)
  colnames(ms) <- "m"
  df <- NULL
  for(i in c(1:ncol(data))){
    df.tmp <- data.frame(m = mSet,y = data[,i],Syn = setting[[i,1]] # m is the x-axis. data[,i] is the data for one synthesizer while syn is the name of that syntheiszer.
                         ,lntp = as.factor(setting[[i,3]]),shp = as.factor(setting[[i,4]])) #lntp is the line shape while shp is the point shape.
    df <- rbind(df,df.tmp)
  }
  plot <- ggplot(data = df,aes(x = m,y = y, colour = Syn,linetype = Syn,shape = Syn)) + #Set color, linetype and shape to syn so that the legend match.
    geom_line(size = 0.8)# + geom_point()
  plot <- plot + scale_x_log10() # Set x-axis to log. Can be changed!
  plot <- plot + scale_color_manual("Syn",values = as.character(setting[,2])) 
  plot <- plot + scale_linetype_manual("Syn",values = as.numeric(setting[,3]))
  plot <- plot + scale_shape_manual("Syn",values = as.numeric(setting[,4]))
  return(plot)
}

###-----syn.smooth--------------------------------------------------------- 
syn.smooth <- function(ysyn, yobs)
{
  ys <- 1:length(ysyn)
  # exclude from smoothing if freq for a single value higher than 70% 
  maxfreq <- which.max(table(ysyn))
  maxcat  <- as.numeric(names(table(ysyn))[maxfreq])
  if (table(ysyn)[maxfreq]/sum(table(ysyn)) > .7) ys <- which(ysyn != maxcat)
  # exclude from smoothing if data are top-coded - approximate check
  if (10*table(ysyn)[length(table(ysyn)) - 1] <
      tail(table(ysyn), n = 1) - table(ysyn)[length(table(ysyn)) - 1]) {
    rmys <- which(ysyn == max(yobs))
    if(length(rmys) > 0){
      ys   <- ys[-which(ysyn == max(yobs))]
    }
    maxy <- max(yobs)
  }
  densbw  <- density(ysyn[ys], width = "SJ")$bw
  ysyn[ys] <- rnorm(n = length(ysyn[ys]), mean = ysyn[ys], sd = densbw)
  if (!exists("maxy")) maxy <- max(yobs) + densbw
  ysyn[ys] <- pmax(pmin(ysyn[ys], maxy), min(yobs))
  ysyn[ys] <- round(ysyn[ys], max(sapply(yobs, decimalplaces)))      
  return(ysyn)
}

decimalplaces <- function(x) 
{
  x <- x - floor(x) # -> more digit numbers 
  if ((x %% 1) != 0 & (round(x, 15) %% 1 != 0)) {
    nchar(strsplit(sub("0+$", "", as.character(x)), ".", fixed = TRUE)[[1]][[2]])
  } else {
    return(0)
  }
}

#Help function to call findPar with multiple sigmas .
#ods is the dataset object.
#sdsN is the object name of the synthetic dataset.
#sigmaL is a list of different weights used for the gaussian function.
#mc is the selected synthetic datasets.
#smooth is whether to smooth the numerical values on the fly.
#ori is the records used from ods to generate sds.
#returns list of matrices with "membership"-values for each record in ods.
multiPar <- function(ods,sdsN,sigmaL,mc,smooth = FALSE,ori = NULL){
  resList <- list()
  for(sigma in sigmaL){
    res <- findPar(ods,sdsN,sigma,mc,smooth,ori)
    resList <- c(resList,list(res))
  }
  return(resList)
}

#CPU version of multiMIPar. TOO SLOW FOR PRACTICAL USE!
multiMI <- function(MIf,pN,IN,NN){
  s <- length(MIf)
  mMax <- ncol(MIf[[1]])
  if(mMax == 100) m <- c(1,10,100)
  else m <- c(1,10,100,1000)
  p <- length(pN)
  mn <- length(m)
  aboM1 <- matrix(NA,s*mn,p)
  aboM2 <- matrix(NA,s*mn,p)
  aboM3 <- matrix(NA,s*mn,p)
  aboM4 <- matrix(NA,s*mn,p)
  for(i in c(1:s)){
    MI <- dbl(MIf[[i]])
    for(j in c(1:mn)){
      for(q in c(1:length(pN))){
        # if(m[[j]] == 1){
        #   ors <- MI[[i]][c(1:IN),] / m[[j]]
        #   sys <- MI[[i]][c((IN+1):(IN+NN)),] / m[[j]]
        #   sum1 <- sum(apply(ors < pN[[q]],1,mean))
        #   sum2 <- sum(apply(sys < pN[[q]],1,mean))
        # }
        # else if(m[[j]] == mMax){
        #   ors <- MI[[i]][c(1:IN),] / m[[j]]
        #   sys <- MI[[i]][c((IN+1):(IN+NN)),] / m[[j]]
        #   sum1 <- sum(sum(ors) < pN[[q]])
        #   sum2 <- sum(sum(sys) < pN[[q]])
        # }
        # else{
        #   tmp1 <- matrix(NA,IN,100)
        #   tmp2 <- matrix(NA,NN,100)
        #   for(t in c(1:100)){
        #     sam <- sample.int(mMax,m[[j]],replace = TRUE)
        #     ors <- MI[[i]][c(1:IN),sam] / m[[j]]
        #     sys <- MI[[i]][c((IN+1):(IN+NN)),sam] / m[[j]]
        #     tmp1[,t] <- apply(ors,1,sum) < pN[[q]]
        #     tmp2[,t] <- apply(sys,1,sum) < pN[[q]]
        #   }
        #   sum1 <- sum(apply(tmp1,1,mean))
        #   sum2 <- sum(apply(tmp2,1,mean))
        # }
        if(m[[j]] == 1){
          ors <- MI[c(1:IN),]
          sys <- MI[c((IN+1):(IN+NN)),]
          sum1 <- sum(apply(ors < pN[[q]],1,mean))
          sum2 <- sum(apply(sys < pN[[q]],1,mean))
        }
        else{
          ors <- apply(MI[c(1:IN),c(1:m[[j]])],1,sum) / m[[j]]
          sys <- apply(MI[c((IN+1):(IN+NN)),c(1:m[[j]])],1,sum) / m[[j]]
          sum1 <- sum(ors < pN[[q]])
          sum2 <- sum(sys < pN[[q]])
        }
        
        aboM1[[(i-1)*mn + j,q]] <- 1-sum1/IN
        aboM2[[(i-1)*mn + j,q]] <- 1-sum2/NN
        aboM3[[(i-1)*mn + j,q]] <- sum2/NN-sum1/IN
        aboM4[[(i-1)*mn + j,q]] <- if((NN-sum2) == 0 & (IN-sum1) == 0) 0.5 else (NN-sum2)/((NN-sum2)+(IN-sum1))
        #aboM3[c(4*i-3,4*i-2,4*i-1,4*i),j] <- c(1-sum1/IN,1-sum2/NN,sum2/NN-sum1/IN,sum2/(sum2+sum1))
      }
    }
  }
  return(list(aboM1,aboM2,aboM3,aboM4))
}

#Used to generate synthetic datasets with smooth numeric values.
createSmooth <- function(sds,ods){
  m <- sds$m
  smt <- sapply(ods,is.numeric)
  perc <- sapply(seq(0.1,0.9,0.1)*m,round)
  message("SDS: ",appendLF = FALSE)
  for(i in c(1:m)){
    for(j in c(1:ncol(ods))[smt]){
      sds$syn[[i]][,j] <- syn.smooth(sds$syn[[i]][,j],ods[,j])
    }
    if(i %in% perc){
      message(paste0(seq(0.1,0.9,0.1)[perc %in% i],", "),appendLF = FALSE)
    }
  }
  message("SDS END ",appendLF = TRUE)
  return(sds)
}

multiMIParTest <- function(MIf,pN,IN,NN){
  MIlist <- list()
  MIf <- (MIf < pN) / 1000
  MIlist <- c(MIlist,list(MIf))
  MIf <- t(matrix(parTestLoop(as.double(t(dbl(MIf))),20000),1000,20000))
  MIlist <- c(MIlist,list(MIf))
  MIf <- t(matrix(parIns(as.double(t(dbl(MIf))),1000),20000,1))
  MIlist <- c(MIlist,list(MIf))
  MIf <- t(matrix(parTestLoop(as.double(t(dbl(MIf))),2),20000,1))
  MIlist <- c(MIlist,list(MIf))
  return(MIlist)
}

multiMIParTest1 <- function(MIf,pN,IN,NN){
  MIlist <- list()
  MIf <- t(matrix(parTestLoop(as.double(t(dbl(MIf))),1000),1,20000))
  MIlist <- c(MIlist,list(MIf))
  MIf <- (MIf < pN) / 1
  MIlist <- c(MIlist,list(MIf))
  MIf <- t(matrix(parTestLoop(as.double(t(dbl(MIf))),20000),1,20000))
  MIlist <- c(MIlist,list(MIf))
  MIf <- t(matrix(parIns(as.double(t(dbl(MIf))),1),20000,1))
  MIlist <- c(MIlist,list(MIf))
  MIf <- t(matrix(parTestLoop(as.double(t(dbl(MIf))),2),20000,1))
  MIlist <- c(MIlist,list(MIf))
  return(MIlist)
}

#Used to calculate how many records have values below a certain threshold p.
#MIf is a list of matrices with membership-values per record and synthetic dataset
#pN is vector of thresholds.
#IN is number of used records in generation.
#NN is number of unused records in generation.
multiMIPar <- function(MIf,pN,IN,NN,Sa,m){
  if(!is.loaded("MIDr")){
    dyn.load("member_inference.dll")
  }
  s <- length(MIf)
  mMax <- ncol(MIf[[1]])
  if(missing(m) || is.null(m)){
    if(mMax == 100) m <- c(1,10,100)
    else m <- c(1,10,100,1000)
  }
  p <- length(pN)
  mn <- length(m)
  aboM1 <- matrix(NA,s*mn,p)
  aboM2 <- matrix(NA,s*mn,p)
  aboM3 <- matrix(NA,s*mn,p)
  aboM4 <- matrix(NA,s*mn,p)
  cc_ans <- rep.int(0,2*mn*p)
  message(paste0("START: "),appendLF = FALSE)
  for(i in c(1:s)){
    message(paste0(i,", "),appendLF = FALSE)
    MI <- MIf[[i]]
    
    ans <- .C("MIDr",
              cc = as.integer(cc_ans),
              as.float(t(MI))@Data,
              as.float(pN)@Data,
              as.integer(m),
              as.integer(IN),
              as.integer(NN),
              as.integer(mMax),
              as.integer(Sa),
              as.integer((IN+NN)*mMax),
              as.integer(p),
              as.integer(mn))
    ans <- float32(t(matrix(ans[["cc"]],2,p*mn)))
    for(j in c(1:mn)){
      for(q in c(1:p)){
        
        sum1 <- ans[(j-1)*p + q,1]
        sum2 <- ans[(j-1)*p + q,2]
        
        aboM1[[(i-1)*mn + j,q]] <- 1-sum1/IN
        aboM2[[(i-1)*mn + j,q]] <- 1-sum2/NN
        aboM3[[(i-1)*mn + j,q]] <- sum2/NN-sum1/IN
        aboM4[[(i-1)*mn + j,q]] <- if((sum1+sum2)/(IN+NN) > 0.5) (1-((sum1/IN)/((sum2/NN)+(sum1/IN)))) else (1-sum1/IN)/((1-sum1/IN)+(1-sum2/NN))
        #aboM4[[(i-1)*mn + j,q]] <- if((NN-sum2) == 0 & (IN-sum1) == 0) 0.5 else (1-sum1/IN)/((1-sum1/IN)+(1-sum2/NN))
        #aboM4[[(i-1)*mn + j,q]] <- if((sum1 + sum2) == 0) 0 else (1-((sum1/IN)/((sum2/NN)+(sum1/IN))))
        #aboM3[c(4*i-3,4*i-2,4*i-1,4*i),j] <- c(1-sum1/IN,1-sum2/NN,sum2/NN-sum1/IN,sum2/(sum2+sum1))
      }
    }
  }
  message(paste0("END"),appendLF = TRUE)
  return(list(aboM1,aboM2,aboM3,aboM4))
}

GBD <- function(abo,di){
  nr <- di
  nc <- nrow(abo)/nr
  tm <- apply(abo,1,max)
  gbd <- apply(matrix(tm,nc,nr),1,max)
  return(gbd)
}

testReord <- function(memp,pre,post){
  memn <- matrix(NA,1,pre*post)
  for(i in c(0:(pre*post-1))){
    pos <- (i %% pre)*post + floor(i/pre)
    memn[pos+1] <- memp[i+1]
  }
  return(memn)
}

testReordAlt <- function(memp,pre,post){
  #memn <- matrix(NA,1,pre*post)
  for(i in c(0:(pre*post-1))){
    pos <- (i %% pre)*post + floor(i/pre)
    if(pos < i){
      tmp = memp[pos+1]
      memp[pos+1] <- memp[i+1]
      memp[i+1] <- tmp
    }
  }
  return(memp)
}

testBS <- function(memp){
  done <- 1
  odd <- 0
  memn <- c(1:length(memp))
  while(done == 1){
    done <- 0
    i <- 0
    while(2*i + 1 + odd < length(memp)){
      low <- memp[2*i + odd + 1]
      hig <- memp[2*i + 1 + odd + 1]
      if(hig < low){
        memp[2*i + odd + 1] <- hig
        memp[2*i + 1 + odd + 1] <- low
        tmp <- memn[2*i + odd + 1]
        memn[2*i + odd + 1] <- memn[2*i + 1 + odd + 1]
        memn[2*i + 1 + odd + 1] <- tmp
        done <- 1
      }
      i <- i + 1
    }
    if(odd == 1) odd = 0 else odd = 1
  }
  return(memn)
}

#rankb is to select different "relationship" functions 0-4. 42 is a special one to test something.
findMK <- function(ods,sdsN,sigma,mc = c(1:100),rankb = 0,sel){
  if(!is.loaded("MKr")){
    dyn.load("member_inference.dll")
  }
  sds <- eval(as.name(sdsN))
  odsT <- do.call(cbind,ods)
  n <- nrow(odsT)
  k <- sds$k
  c <- ncol(odsT)
  m <- length(mc)#200#sds$m
  if(missing(sel)) sel <- c(1:c)
  ca <- length(sel)
  sdsM <- matrix(NA,k,ca*m)
  cc_ans <- rep.int(0,n*m)
  perc <- sapply(seq(0.1,0.9,0.1)*m,round)
  message("SDS: ",appendLF = FALSE)
  for(j in c(1:m)){
    sdsT <- do.call(cbind,sds$syn[[ mc[[j]] ]])
    sdsM[,c(((j-1)*ca + 1):(j*ca))] <- sdsT[,sel]
    if(j %in% perc){
      message(paste0(seq(0.1,0.9,0.1)[perc %in% j],", "),appendLF = FALSE)
    }
  }
  message("SDS END ",appendLF = TRUE)
  ans <- .C("MKr",
            cc = as.integer(cc_ans),
            as.float(sdsM)@Data,
            as.float(odsT[,sel])@Data,
            as.float(sigma[sel])@Data,
            as.integer(rankb),
            as.integer(ca*k*m),
            as.integer(ca*n),
            as.integer(ca),
            as.integer(m))
  ans <- t(matrix(ans[["cc"]],m,n)) + 1
  return(ans)
}

multiMK <- function(ods,sdsN,sigmaL,mc,rankbL,selL){
  resList <- list()
  srn <- length(sigmaL)
  time <- list()
  time <- c(time,list(Sys.time()))
  for(i in c(1:srn)){
    sigma <- sigmaL[[i]]
    rankb <- rankbL[[i]]
    sel <- selL[[i]]
    res <- findMK(ods,sdsN,sigma,mc,rankb,sel)
    resList <- c(resList,list(res))
    time <- c(time,list(Sys.time()))
  }
  assign("timeMK", time, envir = .GlobalEnv)
  return(resList)
}

findEst <- function(sdsN,MKDN,mc,sel){
  sds <- eval(as.name(sdsN))
  MKD <- eval(as.name(MKDN))
  smt <- sapply(sds$syn[[1]],is.numeric)
  nr <- nrow(MKD[[1]])
  nc <- length(sel)
  m <- length(mc)
  k <- sds$k
  cn <- colnames(sds$syn[[1]])[sel]
  estL <- list()
  sdsM <- as.data.frame(matrix(NA,k*m,ncol(sds$syn[[1]])))
  colnames(sdsM) <- colnames(sds$syn[[1]])
  #sdsM <- NULL
  time <- list()
  time <- c(time,list(Sys.time()))
  for(j in c(1:m)){
    sdsT <- sds$syn[[ mc[[j]] ]]
    sdsM[c(((j-1)*k + 1):(j*k)),] <- sdsT[]
  }
  time <- c(time,list(Sys.time()))
  for(i in c(1:ncol(sds$syn[[1]]))[!smt]){
    sdsM[,i] <- factor(sdsM[,i],c(1:length(levels(sds$syn[[1]][,i]))),labels = levels(sds$syn[[1]][,i]))
  }
  time <- c(time,list(Sys.time()))
  
  altCol <- function(art, cl){
    res <- matrix(NA,length(art),1)
    for(i in c(1:length(art))){
      res[i] <- sds$syn[[i]][art[[i]],cl]
    }
    return(res)
  }
  for(MK in MKD){
    estM <- as.data.frame(matrix(NA,nr,nc))
    colnames(estM) <- cn
    for(i in c(1:nr)){
      ar <- MK[i,]
      are <- ar + c(0:(m-1))*k
      for(j in c(1:length(sel))){
        pre <- sdsM[are,sel[[j]]]
        #pre <- altCol(ar,sel[[j]])
        if(smt[[sel[[j]]]] == TRUE){
          mn <- mean(pre)
          estM[i,j] <- mn
        }
        else{
          tmp <- as.data.frame(table(pre))
          fr <- as.factor(tmp$pre[[which.max(tmp$Freq)]])
          estM[i,j] <- fr
        }
      }
      #chg <- c(1:ncol(sds$syn[[1]]))[!smt][c(1:ncol(sds$syn[[1]]))[!smt] %in% sel]
    }
    estL <- c(estL,list(estM))
  }
  
  time <- c(time,list(Sys.time()))
  for(j in c(1:length(estL))){
    for(i in c(1:nc)){
      if(sel[i] %in% c(1:ncol(sds$syn[[1]]))[!smt]){
        estL[[j]][,i] <- factor(estL[[j]][,i],c(1:length(levels(sds$syn[[1]][,sel[i]]))),labels = levels(sds$syn[[1]][,sel[i]]))
      }
    }
  }
  time <- c(time,list(Sys.time()))
  return(estL)
}

compME <- function(MEDN,ods,sel){
  MED <- eval(as.name(MEDN))
  smt <- sapply(ods,is.numeric)
  nr <- nrow(ods)
  resL <- list()
  for(ME in MED){
    tes <- matrix(NA,3,length(sel))
    colnames(tes) <- colnames(ods)[sel]
    for(i in c(1:length(sel))){
      if(smt[[sel[[i]]]] == TRUE){
        tmp <- abs(ME[,i] - ods[,sel[[i]]])
        tes[,i] <- c(mean(tmp),sqrt(var(tmp)),NA)
      }
      else{
        tmp <- ME[,i] == ods[,sel[[i]]]
        tmp2 <- summary(ME[,i])
        tmp3 <- summary(ods[,sel[[i]]])
        tes[,i] <- c(mean(abs(tmp2-tmp3)),sqrt(var(abs(tmp2-tmp3))),sum(tmp)/nr)
      }
    }
    resL <- c(resL,list(tes))
  }
  return(resL)
}

getBest <- function(res){
  nr <- nrow(res)
  seq1 <- seq(1,nr,3)
  seq2 <- seq(2,nr,3)
  seq3 <- seq(3,nr,3)
  res[seq1,] <- apply(res[seq1,],2,rank,na.last = "keep")
  res[seq2,] <- apply(res[seq2,],2,rank,na.last = "keep")
  res[seq3,] <- apply(1-res[seq3,],2,rank,na.last = "keep")
  res <- cbind(res,apply(res,1,mean,na.rm = TRUE))
  return(res)
}

getAll <- function(MEDN,ods,sel){
  MRD <- compME(MEDN,ods,sel)
  MRD2 <- getBest(do.call(rbind,MRD))
  nr <- nrow(MRD2)
  nce <- ncol(MRD2)
  tp <- nr/3 - 1
  MRD3 <- unlist(lapply(split(seq(1:nr),c(sapply(c(0:tp),rep.int,times = 3))),function(x) mean(MRD2[x,nce])))
  return(list(MRD,MRD2,MRD3))
}

pasteC <- function(argL){
  argC <- prod(unlist(lapply(argL,length)))
  comb <- ""
  post <- argC
  pasExp <- function(arg,pre){
    argS <- length(arg)
    post <<- pre/argS
    res <- argC/(post*argS)
    return(rep(sapply(arg,rep,times = post),times = res))
  }
  for(arg in argL){
    tmp <- pasExp(arg,post)
    comb <- paste0(comb,tmp)
  }
  return(comb)
}

getBest2 <- function(MRND,ch = 3){
  res <- matrix(NA,1,length(MRND),dimnames = list(c("BEST"),c(MRND)))
  for(i in c(1:length(MRND))){
    MR <- eval(as.name(MRND[[i]]))
    res[1,i] <- which.min(MR[[3]])
  }
  return(res)
}

combMR <- function(MR,sel){
  cna <- colnames(MR[[1]])[sel]
  res <- matrix(NA,3,length(sel),dimnames = list(c(1,2,3),cna))
  for(i in c(1:length(sel))){
    res[,i] <- MR[[i]][,sel[[i]]]
  }
  return(res)
}

getBest3 <- function(MR,spl,sel,type = 1){
  cna <- colnames(MR[[1]])[sel]
  res <- matrix(NA,(3*spl),length(sel),dimnames = list(c(1:(3*spl)),cna))
  for(i in c(1:spl)){
    if(type == 1){
      selc <- seq(i,(length(sel)*spl),spl)
    }
    else{
      selc <- seq(((i-1)*length(sel)+1),(i*length(sel)),1)
    }
    res[c(((i-1)*3+1):(i*3)),] <- combMR(MR[selc],sel)
  }
  return(res)
}

getBest4 <- function(MRN,sel,spl){
  par <- NULL
  for(j in c(1:length(MRN))){
    MR <- eval(as.name(MRN[[j]]))[[1]]
    cna <- colnames(MR[[1]])[sel]
    res <- matrix(NA,(3*spl),length(sel),dimnames = list(c(1:(3*spl)),cna))
    for(i in c(1:spl)){
      selc <- c(((i-1)*length(sel)+1):(i*length(sel)))
      res[c(((i-1)*3+1):(i*3)),] <- combMR(MR[selc],sel)
    }
    par <- c(par,list(res))
  }
  par2 <- NULL
  for(j in c(1:length(sel))){
    res <- matrix(NA,(3*spl),length(MRN),dimnames = list(c(1:(3*spl)),MRN))
    for(i in c(1:length(MRN))){
      res[,i] <- par[[i]][,j]
    }
    par2 <- c(par2,list(res))
  }
  return(list(par,par2))
}

testMK <- function(ods,sdsN,mc,ns,alt = FALSE){
  sds <- eval(as.name(sdsN))
  odsT <- do.call(cbind,ods)
  n <- nrow(odsT)
  if(missing(ns)) ns <- n
  k <- sds$k
  c <- ncol(odsT)
  m <- length(mc)#200#sds$m
  sel <- c(1:c)
  ca <- length(sel)
  sdsM <- matrix(NA,k,ca*m)
  perc <- sapply(seq(0.1,0.9,0.1)*m,round)
  message("SDS: ",appendLF = FALSE)
  for(j in c(1:m)){
    sdsT <- do.call(cbind,sds$syn[[ mc[[j]] ]])
    sdsM[,c(((j-1)*ca + 1):(j*ca))] <- sdsT[,sel]
    if(j %in% perc){
      message(paste0(seq(0.1,0.9,0.1)[perc %in% j],", "),appendLF = FALSE)
    }
  }
  message("SDS END ",appendLF = TRUE)
  
  odsR <- apply(odsT,2,rank)
  sdsR <- apply(sdsM,2,rank)
  
  ans <- matrix(NA,n,m)
  
  message("START: ",appendLF = FALSE)
  for(i in c(1:ns)){
    od <- odsR[i,]
    
    for(j in c(1:m)){
      sd <- apply(sdsR[,c(((j-1)*ca + 1):(j*ca))],1,function(x) abs(x-od))
      if(alt == FALSE){
        sd <- apply(sd,2,max)
      }
      else{
        sd <- apply(sd,2,sum)
      }
      ans[i,j] <- which.min(sd)
    }
    message(paste0(i,","),appendLF = FALSE)
  }
  message("END",appendLF = TRUE)
  
  return(ans)
}

findEstComp <- function(sdsN,MKDN,ods,mc,ml,sa,mea,smts){
  if(!is.loaded("MEDr")){
    dyn.load("member_inference.dll")
  }
  sds <- eval(as.name(sdsN))
  MKD <- eval(as.name(MKDN))
  odsT <- do.call(cbind,ods)
  #smtp <- sapply(sds$syn[[1]],is.numeric)
  #smt <- rep.int(0,length(smtp))
  #smt[smtp] <- 1
  #smt <- sapply(ods,function(x) length(levels(x)))
  nr <- nrow(MKD)
  nc <- ncol(ods)
  m <- length(mc)
  k <- sds$k
  cn <- colnames(ods)
  estL <- list()
  sdsM <- matrix(NA,k,nc*m)
  
  cc_ans <- rep.int(0,nc*length(ml))
  perc <- sapply(seq(0.1,0.9,0.1)*m,round)
  message("SDS: ",appendLF = FALSE)
  for(j in c(1:m)){
    sdsT <- do.call(cbind,sds$syn[[ mc[[j]] ]])
    sdsM[,c(((j-1)*nc + 1):(j*nc))] <- sdsT[]
    if(j %in% perc){
      message(paste0(seq(0.1,0.9,0.1)[perc %in% j],", "),appendLF = FALSE)
    }
  }
  message("SDS END",appendLF = TRUE)
  
  message("START: ",appendLF = FALSE)
  for(i in c(1:length(smts))){
    message(paste0(i,","),appendLF = FALSE)
    smt <- smts[[i]]
    MK <- MKD[[i]]
    cc_ans <- rep.int(0,nc*length(ml))
    ans <- .C("MEDr",
              cc = as.integer(cc_ans),
              as.float(sdsM)@Data,
              as.float(odsT)@Data,
              as.integer(MK),
              as.integer(ml),
              as.float(smt)@Data,
              as.integer(nc*length(ml)),
              as.integer(nc*k*m),
              as.integer(nc*k),
              as.integer(k*m),
              as.integer(length(ml)),
              as.integer(nc),
              as.integer(m),
              as.integer(sa),
              as.integer(mea))
    ans <- as.double(float32(t(matrix(ans[["cc"]],nc,length(ml)))))
    colnames(ans) <- cn
    rownames(ans) <- as.character(ml)
    estL <- c(estL,list(ans))
  }
  message("END",appendLF = TRUE)
  
  
  return(estL)
}

testEstAlt <- function(sdsN,MK,ods,sel,mc){
  sds <- eval(as.name(sdsN))
  smt <- sapply(ods,is.numeric)
  nr <- nrow(MK)
  nc <- length(sel)
  m <- length(mc)
  k <- sds$k
  cn <- colnames(ods)[sel]
  sdsM <- as.data.frame(matrix(NA,k*m,ncol(ods)))
  colnames(sdsM) <- colnames(ods)
  #sdsM <- NULL
  time <- list()
  time <- c(time,list(Sys.time()))
  for(j in c(1:m)){
    sdsT <- sds$syn[[ mc[[j]] ]]
    sdsM[c(((j-1)*k + 1):(j*k)),] <- sdsT[]
  }
  time <- c(time,list(Sys.time()))
  
  altCol <- function(art, cl){
    res <- matrix(NA,length(art),1)
    for(i in c(1:length(art))){
      res[i] <- sds$syn[[i]][art[[i]],cl]
    }
    return(res)
  }
  
  medianAlt <- function(pre){
    low <- floor((length(pre) + 1) / 2)
    pre <- sort(pre)
    return(pre[low])
  }
  
  estM1 <- matrix(NA,nr,m)
  estM100 <- matrix(NA,nr,1)
  estM100M <- matrix(NA,nr,1)
  for(i in c(1:nr)){
    ar <- MK[i,mc]
    are <- ar + c(0:(m-1))*k
    pre <- sdsM[are,sel]
    mn <- mean(pre)
    estM100[i,1] <- mn
    estM100M[i,1] <- medianAlt(pre)
    estM1[i,] <- pre
    #chg <- c(1:ncol(sds$syn[[1]]))[!smt][c(1:ncol(sds$syn[[1]]))[!smt] %in% sel]
  }
  
  aveNew <- function(est, ods){
    getEq <- est == ods
    ans <- matrix(NA,length(est),1)
    ans[getEq,1] <- 1
    ans[!getEq,1] <- 1 - abs(est[!getEq] - ods[!getEq]) / (abs(est[!getEq]) + abs(ods[!getEq]))
    return(ans)
  }
  
  aveOrg <- function(est, ods){
    ans <- matrix(NA,length(est),1)
    ans <- abs(est - ods)
    return(ans)
  }
  
  aveNew2 <- function(est, ods, lim){
    ans <- matrix(NA,length(est),1)
    ans <- sapply(abs(est - ods) < lim,function(x) if(x == TRUE) 1 else 0)
    return(ans)
  }
  
  es1O <- apply(estM1,2,function(x) aveOrg(x,ods[,sel]))
  es100O <- aveOrg(estM100[,1],ods[,sel])
  es100MO <- aveOrg(estM100M[,1],ods[,sel])
  
  es1N <- apply(estM1,2,function(x) aveNew(x,ods[,sel]))
  es100N <- aveNew(estM100[,1],ods[,sel])
  es100MN <- aveNew(estM100M[,1],ods[,sel])
  
  es1N2 <- apply(estM1,2,function(x) aveNew2(x,ods[,sel],1))
  es100N2 <- aveNew2(estM100[,1],ods[,sel],1)
  es100MN2 <- aveNew2(estM100M[,1],ods[,sel],1)
  
  return(list(es1O,es100O,es100MO,es1N,es100N,es100MN,es1N2,es100N2,es100MN2))
}

AMR <- function(abo,di){
  amr <- apply(abo[,di],1,mean)
  return(amr)
}

MRT <- function(MR,cho,n){
  MRp <- eval(as.name(MR[[1]]))
  MRT <- matrix(NA,length(MR),length(cho),dimnames = list(MR,colnames(MRp[,cho])))
  for(i in c(1:length(MR))){
    MRp <- eval(as.name(MR[[i]]))
    MRT[i,] <- MRp[n,cho]
  }
  return(MRT)
}

GBD2 <- function(abo,di){
  nr <- length(di)
  nc <- nrow(abo)/nr
  gbd <- matrix(0,nc,ncol(abo))
  for(i in c(1:nc)){
    tm <- abo[seq(i,nrow(abo),nc),]
    tm <- tm[di,]
    gbd[i,] <- apply(tm,2,which.max)
  }
  return(gbd)
}

GBD3 <- function(abo,nr,di){
  nc <- nrow(abo)/nr
  gbd <- matrix(0,nc*(nr-1),ncol(abo))
  for(i in c(1:nc)){
    tm <- abo[seq(i,nrow(abo),nc),]
    tm <- apply(tm[-di,],1,function(x) tm[di,] - x)
    gbd[c((i-1)*(nr-1)+1,i*(nr-1)),] <- tm
  }
  return(gbd)
}

GBDH <- function(aboN,nc,nr,di,li){
  gbdh <- matrix(NA,length(aboN),nc*(nr-1),
                 dimnames = list(aboN,pasteC(list(c(1,2,3,5,10,20,50,100),",",c(1:nr)[-di]))))
  for(i in c(1:length(aboN))){
    abo <- eval(as.name(aboN[[i]]))
    gbd <- apply(GBD3(abo[[3]],nr,di),1,function(x) sum(x < li))
    gbdh[i,] <- gbd
  }
  return(gbdh)
}

createPlot2 <- function(dataX,dataY,setting){
  df <- NULL
  for(i in c(1:ncol(dataY))){
    df.tmp <- data.frame(x = dataX[,i],y = dataY[,i],Syn = setting[[i,1]] # m is the x-axis. data[,i] is the data for one synthesizer while syn is the name of that syntheiszer.
                         ,lntp = as.factor(setting[[i,3]]),shp = as.factor(setting[[i,4]])) #lntp is the line shape while shp is the point shape.
    df <- rbind(df,df.tmp)
  }
  plot <- ggplot(data = df,aes(x = x, y = y, colour = Syn,linetype = Syn,shape = Syn)) + #Set color, linetype and shape to syn so that the legend match.
    geom_line(size = 0.8)# + geom_point()
  #plot <- plot + scale_x_log10() # Set x-axis to log. Can be changed!
  plot <- plot + scale_color_manual("Syn",values = as.character(setting[,2])) 
  plot <- plot + scale_linetype_manual("Syn",values = as.numeric(setting[,3]))
  plot <- plot + scale_shape_manual("Syn",values = as.numeric(setting[,4]))
  return(plot)
}

createDict <- function(ods,n){
  if(missing(n) || is.null(n)) n <- nrow(ods)
  #dict <- ods[1,]
  #if(n > 1){
  #  dict <- rbind(dict,matrix(NA,(n-1),ncol(ods),dimnames = list(c(2:n),colnames(ods))))
  #}
  dict <- ods[rep.int(1,times = n),]
  message("Dict: ",appendLF = FALSE)
  perc <- sapply(seq(0.1,0.9,0.1)*n,round)
  for(i in c(1:n)){
    for(j in c(1:ncol(ods))){
      sam <- sample.int(nrow(ods),1)
      dict[[i,j]] <- ods[[sam,j]]
    }
    if(i %in% perc){
      message(paste0(seq(0.1,0.9,0.1)[perc %in% i],", "),appendLF = FALSE)
    }
  }
  message("Dict END ",appendLF = TRUE)
  return(dict)
}

findRLA <- function(ods,odsD,sdsN,sigma,mc = c(1:100),rankb = 0,sel){
  if(!is.loaded("RLAr")){
    dyn.load("member_inference.dll")
  }
  sds <- eval(as.name(sdsN))
  odsT <- do.call(cbind,ods)
  odsDT <- do.call(cbind,odsD)
  n <- nrow(odsT)
  nd <- nrow(odsDT)
  k <- sds$k
  c <- ncol(odsT)
  m <- length(mc)#200#sds$m
  if(missing(sel)) sel <- c(1:c)
  ca <- length(sel)
  sdsM <- matrix(NA,k,ca*m)
  cc_ans <- rep.int(0,n*m + nd*m)
  perc <- sapply(seq(0.1,0.9,0.1)*m,round)
  message("SDS: ",appendLF = FALSE)
  for(j in c(1:m)){
    sdsT <- do.call(cbind,sds$syn[[ mc[[j]] ]])
    sdsM[,c(((j-1)*ca + 1):(j*ca))] <- sdsT[,sel]
    if(j %in% perc){
      message(paste0(seq(0.1,0.9,0.1)[perc %in% j],", "),appendLF = FALSE)
    }
  }
  message("SDS END ",appendLF = TRUE)
  ans <- .C("RLAr",
            cc = as.integer(cc_ans),
            as.float(sdsM)@Data,
            as.float(odsT[,sel])@Data,
            as.float(odsDT[,sel])@Data,
            as.integer(0),
            as.float(sigma[sel])@Data,
            as.integer((n+nd)*m),
            as.integer(ca*k*m),
            as.integer(ca*n),
            as.integer(ca*nd),
            as.integer(0),
            as.integer(ca),
            as.integer(m),
            as.integer(0),
            as.integer(rankb))
  ans <- as.double(float32(t(matrix(ans[["cc"]],m,n+nd))))
  return(ans)
}

KSD <- function(dist,splt){
  dd <- DD(dist,splt)
  dda <- 0
  smo <- 0
  smd <- 0
  for(i in c(1:((2*(dd$mx[[1]] - dd$mn[[1]]) + 1)))){
    smo <- smo + dd$do[i]
    smd <- smd + dd$dd[i]
    ddt <- abs(smo - smd)
    if(ddt > dda) dda <- ddt
  }
  return(dda)
}

DD <- function(dist,splt){
  mn <- min(dist)
  mx <- max(dist)
  d1 <- rep.int(0,times = (2*(mx - mn) + 1))
  d2 <- rep.int(0,times = (2*(mx - mn) + 1))
  for(i in c(1:(splt))){
    pos <- 2*(dist[i] - mn) + 1
    d1[pos] <- d1[pos] + 1
  }
  d1 <- d1 / sum(d1)
  for(i in c((splt+1):length(dist))){
    pos <- 2*(dist[i] - mn) + 1
    d2[pos] <- d2[pos] + 1
  }
  d2 <- d2 / sum(d2)
  dd <- data.frame(do = d1, dd = d2, mn = mn, mx = mx)
  return(dd)
}
