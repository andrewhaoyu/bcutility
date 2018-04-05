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
GenerateCoeff <- function(
  log.odds.standard,
  log.odds.intrinsic,
  log.odds.dic,
  log.odds.intrinsic.eb,
  log.odds.intrinsic.la,
  log.odds.intrinsic.tree,
  x.test,
  y.test
){
  pla <- 2
  coeff <- rep(0,6)
  coeff_95 <- rep("c",6)
  prs.standard <- x.test%*%log.odds.standard
  idx.control <- which(y.test==0)
  sd.prs.control <- sd(prs.standard[idx.control])
  prs.standard.scale <- prs.standard/sd.prs.control
  model <- glm(y.test~  prs.standard.scale,family = binomial(link='logit'))
  temp <- exp(coef(model)[2]+summary(model)$coefficient[2,2]*qnorm(c(0.025,0.5,0.975)))
  coeff[1] <- temp[2]
  coeff_95[1] <- paste0(round(temp[2],pla)," (",round(temp[1],pla)," - ",round(temp[3],pla),")")

  prs.intrinsic <- x.test%*%log.odds.intrinsic
  sd.prs.control <- sd(prs.intrinsic[idx.control])
  prs.intrinsic.scale <- prs.intrinsic/sd.prs.control
  model <- glm(y.test~  prs.intrinsic.scale,family = binomial(link='logit'))
  temp <- exp(coef(model)[2]+summary(model)$coefficient[2,2]*qnorm(c(0.025,0.5,0.975)))
  coeff[2] <- temp[2]
  coeff_95[2] <- paste0(round(temp[2],pla)," (",round(temp[1],pla)," - ",round(temp[3],pla),")")
  prs.intrinsic.dic <- x.test%*%log.odds.intrinsic.dic
  sd.prs.control <- sd(prs.intrinsic.dic[idx.control])
  prs.intrinsic.dic.scale <- prs.intrinsic.dic/sd.prs.control
  model <- glm(y.test~  prs.intrinsic.dic.scale,family = binomial(link='logit'))
  temp <- exp(coef(model)[2]+summary(model)$coefficient[2,2]*qnorm(c(0.025,0.5,0.975)))
  coeff[3] <- temp[2]
  coeff_95[3] <- paste0(round(temp[2],pla)," (",round(temp[1],pla)," - ",round(temp[3],pla),")")


  prs.intrinsic.eb <- x.test%*%log.odds.intrinsic.eb
  sd.prs.control <- sd(prs.intrinsic.eb[idx.control])
  prs.intrinsic.eb.scale <- prs.intrinsic.eb/sd.prs.control
  model <- glm(y.test~  prs.intrinsic.eb.scale,family = binomial(link='logit'))
  temp <- exp(coef(model)[2]+summary(model)$coefficient[2,2]*qnorm(c(0.025,0.5,0.975)))
  coeff[4] <- temp[2]
  coeff_95[4] <- paste0(round(temp[2],pla)," (",round(temp[1],pla)," - ",round(temp[3],pla),")")

  prs.intrinsic.la <- x.test%*%log.odds.intrinsic.la
  sd.prs.control <- sd(prs.intrinsic.la[idx.control])
  prs.intrinsic.la.scale <- prs.intrinsic.la/sd.prs.control
  model <- glm(y.test~  prs.intrinsic.la.scale,family = binomial(link='logit'))
  temp <- exp(coef(model)[2]+summary(model)$coefficient[2,2]*qnorm(c(0.025,0.5,0.975)))
  coeff[5] <- temp[2]
  coeff_95[5] <- paste0(round(temp[2],pla)," (",round(temp[1],pla)," - ",round(temp[3],pla),")")
  prs.intrinsic.tree <- x.test%*%log.odds.intrinsic.tree
  sd.prs.control <- sd(  prs.intrinsic.tree[idx.control])
  prs.intrinsic.tree.scale <-  prs.intrinsic.tree/sd.prs.control
  model <- glm(y.test~  prs.intrinsic.tree.scale,family = binomial(link='logit'))
  temp <- exp(coef(model)[2]+summary(model)$coefficient[2,2]*qnorm(c(0.025,0.5,0.975)))
  coeff[6] <- temp[2]
  coeff_95[6] <- paste0(round(temp[2],pla)," (",round(temp[1],pla)," - ",round(temp[3],pla),")")

  return(list(coeff=coeff,
              coeff_95=coeff_95))


}


