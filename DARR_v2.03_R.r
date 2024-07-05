Darr_R <- function(csvDataFileName = 'example_csvMRDataFile.csv',
                   NTraps = 1,
                   pool.apriori = NULL,
                   makePlot = TRUE) {

  
  # DARR_v2.03_R  -- Eric P. Bjorkstedt, NOAA Fisheries, SWFSC  5 July 2024
  # 
  # This version of DARR updates the mode of use to facilitate batch processing or integration in replicable workflows by 
  # substuting functional arguments for 'point-and-click' operations to designate data files, number of traps in an experiment,
  # and any strata to be pooled a priori. 
  # 
  # Use will still proceed as sourcing this file (DARR_v2.03.R) to bring in the various functions (some of which revised) 
  # for subsequent invocation as "Darr_R(...)"
  # 
  # Note that the operational code has not been altered or updated to the tidyverse, but default plot has been updated to
  # use ggplot and to indicate pooled groups of strata by color
  # 
  # Arguments are:
  #    csvDataFileName (default = 'exampleMR_csvDataFile.csv' [included in repo])
  #    NTraps (default = 1)
  #    pool.apriori (default = NULL)
  #    makePlot (default = TRUE)

  require(tidyverse)
  
  
  MRData <- ReadMRData(csvDataFileName)
  MRData.Orig <- MRData
  MRData <- as.matrix(MRData)

  Darr.Results <- Run_DARR_R(MRData, NTraps, pool.apriori)
  
  distributed.pCap <- rep(Darr.Results$pCap.hat, Darr.Results$Pooled.Strata)
  distributed.U.hat <- MRData.Orig[,2] / distributed.pCap
  distributed.pCap <- rep(Darr.Results$pCap.hat, Darr.Results$Pooled.Strata)

  distributed.N.hat <- distributed.U.hat
  if (NTraps == 2) distributed.N.hat <- distributed.U.hat + MRData.Orig[,3]

  S <- length(Darr.Results$Pooled.Strata)

  v.space <- c("","");
  h.space <- t(c("","",""))
  h.col.space <- matrix(rep("",S), nrow = S)
  hx.col.space <- matrix(rep("",S+1), nrow = S+1)
  h3.col.space <- matrix(rep("",3*S), nrow = S)
  h3x.col.space <- matrix(rep("",3*(S+1)), nrow = S+1)
  h2x.col.space <- matrix(rep("",2*(S+1)), nrow = S+1)

  write.table(MRData.Orig[, -1], file = csvDataFileName, append = FALSE, sep = ",", eol = "\n", na = "NA", dec = ".", row.names = FALSE,
              col.names = FALSE, qmethod = c("escape", "double"))

  write.table(v.space, file = csvDataFileName, append = TRUE, sep = ",", eol = "\n", na = "NA", dec = ".", row.names = FALSE,
              col.names = FALSE, qmethod = c("escape", "double"))
  write.table(c("Estimated.N"), file = csvDataFileName, append = TRUE, sep = ",", eol = "\n", na = "NA", dec = ".", row.names = FALSE,
              col.names = FALSE, qmethod = c("escape", "double"))
  write.table(Darr.Results$N.hat, file = csvDataFileName, append = TRUE, sep = ",", eol = "\n", na = "NA", dec = ".", row.names = FALSE,
              col.names = FALSE, qmethod = c("escape", "double"))

  write.table(v.space, file = csvDataFileName, append = TRUE, sep = ",", eol = "\n", na = "NA", dec = ".", row.names = FALSE,
              col.names = FALSE, qmethod = c("escape", "double"))
  write.table(c("Estimated.SE"), file = csvDataFileName, append = TRUE, sep = ",", eol = "\n", na = "NA", dec = ".", row.names = FALSE,
              col.names = FALSE, qmethod = c("escape", "double"))
  write.table(Darr.Results$SE.hat, file = csvDataFileName, append = TRUE, sep = ",", eol = "\n", na = "NA", dec = ".", row.names = FALSE,
              col.names = FALSE, qmethod = c("escape", "double"))


  PD <-  Darr.Results$PooledData[,-1]
  if (is.vector(PD)) PD <- t(as.matrix(PD))
  PD <- as.matrix(PD)


  a <- cbind(h.space,t(Darr.Results$Pooled.Strata))
  b <- rbind(c(" "), t(t(Darr.Results$Pooled.Strata)))

  write.table(v.space, file = csvDataFileName, append = TRUE, sep = ",", eol = "\n", na = "NA", dec = ".", row.names = FALSE,
              col.names = FALSE, qmethod = c("escape", "double"))
  write.table(c("Pooled.Data.and.Strata"), file = csvDataFileName, append = TRUE, sep = ",", eol = "\n", na = "NA", dec = ".", row.names = FALSE,
              col.names = FALSE, qmethod = c("escape", "double"))
  write.table(cbind(rbind(a, cbind(h.col.space, PD)), b),
              file = csvDataFileName, append = TRUE, sep = ",", eol = "\n", na = "NA", dec = ".", row.names = FALSE,
              col.names = FALSE, qmethod = c("escape", "double"))

  write.table(v.space, file = csvDataFileName, append = TRUE, sep = ",", eol = "\n", na = "NA", dec = ".", row.names = FALSE,
              col.names = FALSE, qmethod = c("escape", "double"))
  write.table(c("Estimated.U.hat"), file = csvDataFileName, append = TRUE, sep = ",", eol = "\n", na = "NA", dec = ".", row.names = FALSE,
              col.names = FALSE, qmethod = c("escape", "double"))
  write.table(cbind(h.space,t(Darr.Results$U.hat)), file = csvDataFileName, append = TRUE, sep = ",", eol = "\n", na = "NA", dec = ".", row.names = FALSE,
              col.names = FALSE, qmethod = c("escape", "double"))

  write.table(v.space, file = csvDataFileName, append = TRUE, sep = ",", eol = "\n", na = "NA", dec = ".", row.names = FALSE,
              col.names = FALSE, qmethod = c("escape", "double"))
  write.table(c("Estimated.pCap"), file = csvDataFileName, append = TRUE, sep = ",", eol = "\n", na = "NA", dec = ".", row.names = FALSE,
              col.names = FALSE, qmethod = c("escape", "double"))
  write.table(cbind(h.space,t(Darr.Results$pCap.hat)), file = csvDataFileName, append = TRUE, sep = ",", eol = "\n", na = "NA", dec = ".", row.names = FALSE,
              col.names = FALSE, qmethod = c("escape", "double"))

  write.table(v.space, file = csvDataFileName, append = TRUE, sep = ",", eol = "\n", na = "NA", dec = ".", row.names = FALSE,
              col.names = FALSE, qmethod = c("escape", "double"))
  write.table(c("Estimated.pMig"), file = csvDataFileName, append = TRUE, sep = ",", eol = "\n", na = "NA", dec = ".", row.names = FALSE,
              col.names = FALSE, qmethod = c("escape", "double"))
  write.table(cbind(h3.col.space, Darr.Results$theta), file = csvDataFileName, append = TRUE, sep = ",", eol = "\n", na = "NA", dec = ".", row.names = FALSE,
              col.names = FALSE, qmethod = c("escape", "double"))

  write.table(v.space, file = csvDataFileName, append = TRUE, sep = ",", eol = "\n", na = "NA", dec = ".", row.names = FALSE,
              col.names = FALSE, qmethod = c("escape", "double"))
  write.table(c("Estimated.Cov.N"), file = csvDataFileName, append = TRUE, sep = ",", eol = "\n", na = "NA", dec = ".", row.names = FALSE,
              col.names = FALSE, qmethod = c("escape", "double"))
  write.table(cbind(h3.col.space, Darr.Results$Cov.N), file = csvDataFileName, append = TRUE, sep = ",", eol = "\n", na = "NA", dec = ".", row.names = FALSE,
              col.names = FALSE, qmethod = c("escape", "double"))

  write.table(v.space, file = csvDataFileName, append = TRUE, sep = ",", eol = "\n", na = "NA", dec = ".", row.names = FALSE,
              col.names = FALSE, qmethod = c("escape", "double"))
  write.table(c("Distributed"), file = csvDataFileName, append = TRUE, sep = ",", eol = "\n", na = "NA", dec = ".", row.names = FALSE,
              col.names = FALSE, qmethod = c("escape", "double"))
  write.table(t(c("N.hat","pCap.hat","Strata")), file = csvDataFileName, append = TRUE, sep = ",", eol = "\n", na = "NA", dec = ".", row.names = FALSE,
              col.names = FALSE, qmethod = c("escape", "double"))
  write.table(t(rbind(t(distributed.U.hat), t(distributed.pCap), t(rep(1:length(Darr.Results$Pooled.Strata),Darr.Results$Pooled.Strata)))), file = csvDataFileName, append = TRUE, sep = ",", eol = "\n", na = "NA", dec = ".", row.names = FALSE,
              col.names = FALSE, qmethod = c("escape", "double"))
  
  
   
  stratData <- data.frame(originalStratum=seq(length(distributed.U.hat)),
                          uHat = distributed.U.hat,
                          pCap = as.factor(distributed.pCap))
  
  stratID <- data.frame(pCap = unique(stratData$pCap))
  stratID$stratum = seq(length(stratID$pCap)) %>% as.factor()
  stratData <- stratData %>% 
    left_join(stratID)
  
  pooledStrataString <- ''
  for (i in stratID$stratum) {
    pooledStrata <- paste0(min(which(stratData$stratum == i)),'-',max(which(stratData$stratum == i)), '; ')
    pooledStrataString <- paste0(pooledStrataString, pooledStrata)
  }

  print(c("N_hat = ", Darr.Results$N.hat), quote=FALSE)
  print(c("SE_hat = ", Darr.Results$SE.hat), quote=FALSE)
  print(c("Pooled.Strata = ", pooledStrataString), quote=FALSE)
  print(c("pCap.hat = ", Darr.Results$pCap.hat), quote=FALSE)
  
  if (makePlot)    PlotDARRResults(Darr.Results,distributed.U.hat,distributed.pCap)

}

# *******************

ReadMRData <- function(MRDataFile) {

  TestMRData <- readLines(MRDataFile, n=1)
  TestMRData <- as.vector(as.numeric(unlist(strsplit(TestMRData, ","))))
  S <- length(TestMRData) - 2;

  MRData <- readLines(MRDataFile, n=S)
  MRData <- t(matrix(as.vector(as.numeric(unlist(strsplit(MRData, ",")))),ncol=S))

  MRData <- cbind(t(t(rep(1,S))), MRData)

}

# *******************

Run_DARR_R <- function(MRData, NTraps, pool.apriori) {

  if (length(pool.apriori) > 0) {
    Stratum2Pool <- rep(0,min(dim(as.matrix(MRData))))
    Stratum2Pool[pool.apriori] <- 1
    while (max(Stratum2Pool) == 1) {
      S2P <- max(which(Stratum2Pool == 1))
      if (S2P > 2) {
        if (Stratum2Pool[S2P-1] == 1) {
          MRData <- PoolMRData(MRData, c(S2P-1, S2P))
          Stratum2Pool <- Stratum2Pool[-max(which(Stratum2Pool == 1))]
          if (Stratum2Pool[S2P-2] == 0) {
            Stratum2Pool <- Stratum2Pool[-max(which(Stratum2Pool == 1))]
          }
        }
        else {
          MRData <- PoolMRData(MRData, S2P)
          Stratum2Pool <- Stratum2Pool[-max(which(Stratum2Pool == 1))]
          if (Stratum2Pool[1] == 1) {
            Stratum2Pool <- Stratum2Pool[-max(which(Stratum2Pool == 1))]
          }
        }
      }
      else {
        if (S2P == 2) {
          if (Stratum2Pool[S2P-1] == 1) {
            MRData <- PoolMRData(MRData, c(S2P-1, S2P))
            Stratum2Pool <- Stratum2Pool[-max(which(Stratum2Pool == 1))]
            if (Stratum2Pool[S2P-1] == 1) {
              Stratum2Pool <- Stratum2Pool[-max(which(Stratum2Pool == 1))]
            }
          }
        }
        else {
          MRData <- PoolMRData(MRData, c(1,2))
          Stratum2Pool <- Stratum2Pool[-max(which(Stratum2Pool == 1))]
        }
      }
    }
  }

  # print('1') # retained for future diagnostics
  MRData <- AlgorithmStepTwo(MRData)
  # print('2')
  MRData <- AlgorithmStepThree(MRData)
  # print('3')
  MRData <- AlgorithmStepFour(MRData)
  # print('4')
  Darr.Results <- Darroch_1961(MRData, NTraps)

  Darr.Results = Darr.Results

}

# *******************

PoolMRData <- function(MRData, Stratum2Pool) {


  S <- min(dim(MRData))
  SumRecovered <- rowSums(MRData[, 4:(S+3)])
  ImmediateRecovery <- diag(MRData[, 4:(S+3)])


    if (length(Stratum2Pool) == 2) {

      MRData[min(Stratum2Pool), ] <- MRData[min(Stratum2Pool), ] + MRData[max(Stratum2Pool), ]
      MRData <- MRData[-max(Stratum2Pool), ]
      if(S > 2) {  # thus pooling is yielding S' > 1
        MRData[ ,min(Stratum2Pool) + 3] <- MRData[ ,min(Stratum2Pool) + 3] + MRData[ ,max(Stratum2Pool) + 3]
        MRData <- MRData[, -(max(Stratum2Pool)+3)]
      }
      else { #pooling is yielding S' = 1
        MRData[min(Stratum2Pool) + 3] <- MRData[min(Stratum2Pool) + 3] + MRData[max(Stratum2Pool) + 3]
        MRData <- MRData[-(max(Stratum2Pool)+3)]
      }

    }
    else {
      if(Stratum2Pool == 1) {
        poolIDX <- 1
        poolSEQ <- c(2:S)
      }
      if(Stratum2Pool == S) {
        poolIDX <- -1
        poolSEQ <- c(1:(S-1))
      }
      if(Stratum2Pool > 1 & Stratum2Pool < S) {
        poolIDX <- c(-1,1)
        poolIDX <- poolIDX[ which(abs(SumRecovered[Stratum2Pool + poolIDX] * poolIDX) == 
                                    min(abs(SumRecovered[Stratum2Pool + poolIDX] * poolIDX))) ]
        if(Stratum2Pool == 1) {
          poolIDX <- 1
          poolSEQ <- c(2:S)
        }
        if(Stratum2Pool == S) {
          poolIDX <- -1
          poolSEQ <- c(1:(S-1))
        }
        if(Stratum2Pool > 1 & Stratum2Pool < S) {
          poolIDX <- c(-1,1)
          poolIDX <- poolIDX[ which(abs(SumRecovered[Stratum2Pool + poolIDX] * poolIDX) == 
                                      min(abs(SumRecovered[Stratum2Pool + poolIDX] * poolIDX))) ]
          if (length(poolIDX > 1)) {
            poolIDX <- c(-1,1)
            poolIDX <- poolIDX[ which(abs(ImmediateRecovery[Stratum2Pool + poolIDX] * poolIDX) == 
                                        min(abs(ImmediateRecovery[Stratum2Pool + poolIDX] * poolIDX))) ]
          }
          if (length(poolIDX > 1)) {
            poolIDX <- c(-1,1)
            poolIDX <- poolIDX[ which(abs(((Stratum2Pool + poolIDX) * poolIDX) - S/2) == 
                                        max(abs(((Stratum2Pool + poolIDX) * poolIDX) - S/2))) ]
          }
          if (length(poolIDX > 1)) {
            poolIDX <- c(-1,1)
            poolIDX <- poolIDX[2]
          }
          poolSEQ <- c(sequence(min(Stratum2Pool + poolIDX, Stratum2Pool - poolIDX)), 
                       (sequence(S - Stratum2Pool) + Stratum2Pool))
        }
      }

      MRData[Stratum2Pool + poolIDX, ] <- MRData[Stratum2Pool, ] + MRData[Stratum2Pool + poolIDX, ]
      MRData <- MRData[poolSEQ, ]
      if(S > 2) {  # thus pooling is yielding S' > 1
        MRData[ ,Stratum2Pool + poolIDX + 3] <- MRData[ ,Stratum2Pool + 3] + MRData[ ,Stratum2Pool + poolIDX + 3]
        MRData <- MRData[, c(1,2,3,(poolSEQ + 3))]
      }
      else { #pooling is yielding S' = 1
        MRData[Stratum2Pool + poolIDX + 3] <- MRData[Stratum2Pool + 3] + MRData[Stratum2Pool + poolIDX + 3]
        MRData <- MRData[c(1,2,3,(poolSEQ + 3))]
        }

    }

  MRData

}

# *******************

AlgorithmStepTwo <- function(MRData) {

  if (is.vector(MRData))  MRData <- t(as.matrix(MRData))
  else MRData <- as.matrix(MRData)

  S <- min(dim(MRData))
  Stratum2Pool <- diag(MRData[, 4:(S+3)]) == 0
  Stratum2Pool[Stratum2Pool == TRUE] = 1

  while (sum(Stratum2Pool) > 0 & S > 1)
  {
    Stratum2Pool <- min(which(Stratum2Pool == 1))
    MRData <- PoolMRData(MRData, Stratum2Pool)
    S <- min(dim(MRData))
    Stratum2Pool <- diag(MRData[, 4:(S+3)]) == 0
    Stratum2Pool[Stratum2Pool == TRUE] = 1
  }

  MRData

}

# *******************

AlgorithmStepThree <- function(MRData) {

  if (is.vector(MRData))  MRData <- t(as.matrix(MRData))
  else MRData <- as.matrix(MRData)

  S <- min(dim(as.matrix(MRData)))
  # S <- min(dim(MRData))
  C.crit <- mean(diag(MRData[, 4:(S+3)]))
  svd.R <- svd(MRData[, 4:(S+3)])
  Cond <- max(svd.R$d)/min(svd.R$d)

  while (max(svd.R$d)/min(svd.R$d) > C.crit & S > 1)
  {
    if(S < 3) { MRData <- PoolMRData(MRData, 1) }
    else {

      min.svd.temp <- rep(0,S);
      max.svd.temp <- rep(0,S);
      for(i in 1:S) {
        mrd.temp <- PoolMRData(MRData,i)
        S <- min(dim(mrd.temp))

        svd.temp <- svd(mrd.temp[, 4:(S + 3)]);
        svd.temp <- as.vector(svd.temp$d)
        min.svd.temp[i] <- min(svd.temp)
        max.svd.temp[i] <- max(svd.temp)
      }
      cond.temp <- max.svd.temp/min.svd.temp
      Stratum2Pool <- which(cond.temp == min(cond.temp))

      if(length(Stratum2Pool) > 1) Stratum2Pool <- Stratum2Pool[which(min.svd.temp[Stratum2Pool] == 
                                                                        min(min.svd.temp[Stratum2Pool]))]
      if(length(Stratum2Pool) > 1) Stratum2Pool <- Stratum2Pool[1]


      MRData <- PoolMRData(MRData, Stratum2Pool)
      S <- min(dim(MRData))
      svd.R <- svd(MRData[, 4:((S-1)+3)])
    }
  }


  MRData
}

# *******************

AlgorithmStepFour <- function(MRData) {


  if (is.vector(MRData))  MRData <- t(as.matrix(MRData))
  else MRData <- as.matrix(MRData)

  S <- min(dim(MRData))
  # print(dim(MRData))
  pCap.hat <- 1/(solve(MRData[ ,4:(S+3)]) %*% MRData[, 3])
  # print(pCap.hat)

  while ((max(pCap.hat) > 1 | min(pCap.hat) < 0) & S > 1)
  {
    min.crit <- pCap.hat < 0; min.crit[min.crit == TRUE] = 1;
    max.crit <- pCap.hat > 1; max.crit[max.crit == TRUE] = 1;
    Stratum2Pool <- as.matrix(min.crit * abs(pCap.hat) + max.crit * (pCap.hat - 1))
    Stratum2Pool <- which(Stratum2Pool == max(Stratum2Pool))
    MRData <- PoolMRData(MRData, Stratum2Pool)
    S <- min(dim(as.matrix(MRData)))
    if (S > 1) {
      pCap.hat <- 1/(solve(MRData[ ,4:(S+3)]) %*% MRData[, 3])
      # print(pCap.hat)
    }
  }

  MRData
}

# *******************

Darroch_1961 <- function(MRData, NTraps) {

  if (is.vector(MRData))  MRData <- t(as.matrix(MRData))
  else MRData <- as.matrix(MRData)

  S <- min(dim(MRData))

  if (S == 1){
    pCap.hat <- MRData[4] / MRData[3]
    U.hat <- (MRData[2] / (MRData[4] + 1) * (MRData[3] + 1))  #Bailey 1951
    theta <- 1
    Cov.N <- ( MRData[3]^2 * (MRData[2] + 1) * (MRData[2] - MRData[4]) ) /
      ( (MRData[4] + 1)^2 * (MRData[4] + 2) )
    N.hat <- U.hat + (NTraps-1) * MRData[3]
    V.hat <- Cov.N
    SE.hat <- sqrt(V.hat)
  } else {
    rho <- solve(MRData[ ,4:(S+3)]) %*% MRData[, 3]
    rho <- rho[,1]
    pCap.hat <- 1/rho
    U.hat <- MRData[, 2] * rho
    theta <- solve(diag(MRData[, 3])) %*% MRData[ ,4:(S+3)] %*% solve(diag(pCap.hat))
    mu <- rep(-1,S)
    P <- t(matrix(rep(pCap.hat,S), nrow = S))
    mu <- mu + rowSums(theta/P)

    Cov.N <- diag(U.hat) %*% solve(theta) %*% diag(mu) %*% solve(diag(MRData[, 3])) %*% solve(t(theta)) %*% diag(U.hat)
    Cov.N <- Cov.N + diag(U.hat) %*% (diag(pCap.hat) - diag(rep(1,S)))

    N.hat <- sum(U.hat) + (NTraps-1) * sum(MRData[, 3])
    N.hat = sum(N.hat)
    V.hat = sum(rowSums(Cov.N))
    SE.hat = sqrt(sum(rowSums(Cov.N)))
  }

  output <- list( PooledData = MRData,
                  pCap.hat = pCap.hat,
                  U.hat = U.hat,
                  theta = theta,
                  Cov.N = Cov.N,
                  Pooled.Strata = MRData[ ,1],
                  N.hat = N.hat,
                  V.hat = V.hat,
                  SE.hat = SE.hat )

}

# *******************

PlotDARRResults <- function(Darr.Results,distributed.U.hat,distributed.pCap) {
  
  require(tidyverse)
  
  plotData <- data.frame(originalStratum=seq(length(distributed.U.hat)),
                         uHat = distributed.U.hat,
                         pCap = as.factor(distributed.pCap))
  
  stratID <- data.frame(pCap = unique(plotData$pCap))
  stratID$stratum = seq(length(stratID$pCap)) %>% as.factor()
  plotData <- plotData %>% 
    left_join(stratID)
  
  darrPlot <- ggplot(plotData) +
    geom_col(aes(x=originalStratum, y=uHat,color=stratum,fill=stratum)) + 
    theme(legend.position = "none")

  print(darrPlot)  

}
