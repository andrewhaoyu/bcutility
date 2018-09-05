#' Title
#'
#' @param log.odds.standard
#' @param log.odds.intrinsic
#' @param log.odds.dic
#' @param log.odds.intrinsic.eb
#' @param log.odds.intrinsic.la
#' @param x.test
#' @param y.test
#'
#' @return
#' @export
#'
#' @examples
GenerateAuc_Cal <- function(
  log.odds.standard,
  log.odds.intrinsic,
  log.odds.dic,
  log.odds.intrinsic.eb,
  log.odds.intrinsic.la,
  log.odds.intrinsic.tree,
  x.test,
  y.test
){
  auc.result <- rep(0,6)
  auc.95 <- rep("c",6)
  cal.result <- matrix(0,6,10)
  prs.standard <- x.test%*%log.odds.standard

  cal.standard <- calibration(y.test,prs.standard)
  roc.standard <- roc(y.test,as.vector(prs.standard),ci=T,plot=F)
  pla <- 4
  auc.result[1] <- round(as.numeric(roc.standard$auc),pla)*100

  auc.95[1] <- paste0(round(as.numeric(roc.standard$ci)[1],pla)*100,
                      "-",
                      round(as.numeric(roc.standard$ci)[3],pla)*100)
  cal.standard <- calibration(y.test,prs.standard)
  cal.result[1,] <- cal.standard
  n <- length(roc.standard$sensitivities)
  sensitivities <- matrix(0,n,6)
  specificities <- matrix(0,n,6)
  sensitivities[,1] <- roc.standard$sensitivities
  specificities[,1] <- roc.standard$specificities


  prs.intrinsic <- x.test%*%log.odds.intrinsic

  cal.intrinsic <- calibration(y.test,prs.intrinsic)
  roc.intrinsic <- roc(y.test,as.vector(prs.intrinsic),ci=T,plot=F)
  pla <- 4
  auc.result[2] <- round(as.numeric(roc.intrinsic$auc),pla)*100

  auc.95[2] <- paste0(round(as.numeric(roc.intrinsic$ci)[1],pla)*100,
                      "-",
                      round(as.numeric(roc.intrinsic$ci)[3],pla)*100)
  cal.intrinsic <- calibration(y.test,prs.intrinsic)
  cal.result[2,] <- cal.intrinsic
  sensitivities[,2] <- roc.intrinsic$sensitivities
  specificities[,2] <- roc.intrinsic$specificities


  prs.intrinsic.dic <- x.test%*%log.odds.dic

  cal.intrinsic.dic <- calibration(y.test,prs.intrinsic.dic)
  roc.intrinsic.dic <- roc(y.test,as.vector(prs.intrinsic.dic),ci=T,plot=F)
  pla <- 4
  auc.result[3] <- round(as.numeric(roc.intrinsic.dic$auc),pla)*100

  auc.95[3] <- paste0(round(as.numeric(roc.intrinsic.dic$ci)[1],pla)*100,
                      "-",
                      round(as.numeric(roc.intrinsic.dic$ci)[3],pla)*100)
  cal.intrinsic.dic <- calibration(y.test,prs.intrinsic.dic)
  cal.result[3,] <- cal.intrinsic.dic
  sensitivities[,3] <- roc.intrinsic.dic$sensitivities
  specificities[,3] <- roc.intrinsic.dic$specificities


  prs.intrinsic.eb <- x.test%*%log.odds.intrinsic.eb

  cal.intrinsic.eb <- calibration(y.test,prs.intrinsic.eb)
  roc.intrinsic.eb <- roc(y.test,as.vector(prs.intrinsic.eb),ci=T,plot=F)
  pla <- 4
  auc.result[4] <- round(as.numeric(roc.intrinsic.eb$auc),pla)*100

  auc.95[4] <- paste0(round(as.numeric(roc.intrinsic.eb$ci)[1],pla)*100,
                      "-",
                      round(as.numeric(roc.intrinsic.eb$ci)[3],pla)*100)
  cal.intrinsic.eb <- calibration(y.test,prs.intrinsic.eb)
  cal.result[4,] <- cal.intrinsic.eb
  sensitivities[,4] <- roc.intrinsic.eb$sensitivities
  specificities[,4] <- roc.intrinsic.eb$specificities



  prs.intrinsic.la <- x.test%*%log.odds.intrinsic.la

  cal.intrinsic.la <- calibration(y.test,prs.intrinsic.la)
  roc.intrinsic.la <- roc(y.test,as.vector(prs.intrinsic.la),ci=T,plot=F)
  pla <- 4
  auc.result[5] <- round(as.numeric(roc.intrinsic.la$auc),pla)*100

  auc.95[5] <- paste0(round(as.numeric(roc.intrinsic.la$ci)[1],pla)*100,
                      "-",
                      round(as.numeric(roc.intrinsic.la$ci)[3],pla)*100)
  cal.intrinsic.la <- calibration(y.test,prs.intrinsic.la)
  cal.result[5,] <- cal.intrinsic.la
  sensitivities[,5] <- roc.intrinsic.la$sensitivities
  specificities[,5] <- roc.intrinsic.la$specificities



  prs.intrinsic.tree <- x.test%*%log.odds.intrinsic.tree

  cal.intrinsic.tree <- calibration(y.test,prs.intrinsic.tree)
  roc.intrinsic.tree <- roc(y.test,as.vector(prs.intrinsic.tree),ci=T,plot=F)
  ptree <- 4
  auc.result[6] <- round(as.numeric(roc.intrinsic.tree$auc),ptree)*100

  auc.95[6] <- paste0(round(as.numeric(roc.intrinsic.tree$ci)[1],ptree)*100,
                      "-",
                      round(as.numeric(roc.intrinsic.tree$ci)[3],ptree)*100)
  cal.intrinsic.tree <- calibration(y.test,prs.intrinsic.tree)
  cal.result[6,] <- cal.intrinsic.tree
  sensitivities[,6] <- roc.intrinsic.tree$sensitivities
  specificities[,6] <- roc.intrinsic.tree$specificities







  return(list(auc.result=auc.result,
              auc.95=auc.95,
              cal.result=cal.result,
              sensitivities=sensitivities,
              specificities=specificities))


}



#' Title
#'
#' @param y
#' @param prs
#'
#' @return
#' @export
#'
#' @examples
calibration <- function(y,prs){
  n <- length(y)
  n.qun <- 12
  idx.control <- which(y==0)
  idx.case <- which(y==1)
  prs.control <- prs[idx.control]
  prs.case <- prs[idx.case]
  control.qun <- quantile(prs.control,probs=seq(0,1,1/(n.qun-1)))
  odds <- rep(0,n.qun-2)
  for(i in 2:(n.qun-1)){
    if(i==2){
      idx <- which(prs.control<=control.qun[i])
      n.control <- length(idx)
      idx <- which(prs.case<= control.qun[i])
      n.case <- length(idx)
      odds[i-1] <- n.case/n.control
    }else if(i==(n.qun-1)){

      idx <- which(prs.control>=control.qun[i])
      n.control <- length(idx)
      idx <- which(prs.case>= control.qun[i])
      n.case <- length(idx)
      odds[n.qun-2] <- n.case/n.control
    }else{
      idx <- which(prs.control>=control.qun[i-1]&
                     prs.control<=control.qun[i])
      n.control <- length(idx)
      idx <- which(prs.case>= control.qun[i-1]&
                     prs.case<= control.qun[i])
      n.case <- length(idx)
      odds[i-1] <- n.case/n.control
    }

  }
  odds.ratio <- rep(0,length(odds))
  for(i in 1:length(odds)){
    odds.ratio[i] <- odds[i]/odds[5]
  }
  return(odds.ratio)
}


