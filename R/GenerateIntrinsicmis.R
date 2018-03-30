#' Title
#'
#' @param ER
#' @param PR
#' @param HER2
#' @param Grade
#'
#' @return
#' @export
#'
#' @examples
GenerateIntrinsicmis <- function(ER,PR,HER2,Grade){
  n <- length(ER)
  idx.LA <- which(HER2==0&(ER==1|PR==1)&Grade!=3)
  idx.LB <- which(HER2==1&(ER==1|PR==1))
  idx.LUBHER2 <- which(HER2==0&(ER==1|PR==1)&Grade==3)
  idx.HER2 <- which(HER2==1&ER==0&PR==0)
  idx.Tp <- which(HER2==0&ER==0&PR==0)
  idx.mis <- which(HER2==888|ER==888|PR==888|Grade==888)
  subtypes <- rep("control",n)
  subtypes[idx.mis] <- "mis"
  subtypes[idx.LA] <- "Luminal_A"
  subtypes[idx.LB] <- "Luminal_B"
  subtypes[idx.LUBHER2] <- "Luminal_B_HER2Neg"
  subtypes[idx.HER2] <- "HER2Enriched"
  subtypes[idx.Tp] <- "TripleNeg"
  subtypes <- factor(subtypes,levels=c("control",
                                       "Luminal_A",
                                       "Luminal_B",
                                       "Luminal_B_HER2Neg",
                                       "HER2Enriched",
                                       "TripleNeg",
                                       "mis"))
  return(subtypes)
}

#' Title
#'
#' @param subtypes
#'
#' @return
#' @export
#'
#' @examples
Generatetestid <- function(subtypes){
  set.seed(1)
  M <- length(table(subtypes))
  testcasesize <- rep(0,M-1)
  completecasesize <- rep(0,M-2)
  for(i in 2:(M-1)){
    testcasesize[i-1] <- floor(table(subtypes)[i]/100)*10
    completecasesize[i-1] <- table(subtypes)[i]
  }
  names(testcasesize) <- names(table(subtypes))[2:(M-1)]
  names(completecasesize) <- names(table(subtypes))[2:(M-1)]
  testcaseid <- list()
  testcontrolid <- list()
  n.control <- as.numeric(table(subtypes)[1])
  id.control <- which(subtypes=="control")
  n.control <- length(id.control)
  for(i in 1:(M-2)){
    id.case <- which(subtypes==names(testcasesize)[i])
    testcaseid[[i]] <- id.case[sample(completecasesize[i],testcasesize[i])]
    testcontrolid[[i]] <- id.control[sample(n.control,testcasesize[i])]
  }
  i <-i +1
  temp <- NULL
  for(j in 1:(M-2)){
    temp <- c(temp,testcaseid[[j]])
  }
  testcaseid[[i]] <- temp
  testcontrolid[[i]] <- id.control[sample(n.control,length(testcaseid[[i]]))]
  testcasesize[[i]] <- length(testcaseid[[i]])
  return(list(testcaseid=testcaseid,testcontrolid=testcontrolid,subtypenames=names(testcasesize),testcasesize=testcasesize))
}
