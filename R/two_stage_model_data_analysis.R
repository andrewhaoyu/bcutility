#' Title
#'
#' @param y.pheno.mis1
#' @param gene1
#' @param x.covar1
#' @param y.pheno.mis2
#' @param gene2
#' @param x.covar2
#'
#' @return
#' @export
#'
#' @examples
two_data_two_stage_fixed <- function(
  y.pheno.mis1,
  gene1,
  x.covar1,
  y.pheno.mis2,
  gene2,
  x.covar2
){

  Heter.result1 = EMmvpoly(y.pheno.mis1,baselineonly = NULL,additive = cbind(gene1,x.covar1),pairwise.interaction = NULL,saturated = NULL,missingTumorIndicator = 888)
  z.standard <- Heter.result1[[12]]
  M <- nrow(z.standard)
  number.of.tumor <- ncol(z.standard)
  log.odds1 <- Heter.result1[[1]][(M+1):(M+1+number.of.tumor)]
  sigma.log.odds1 <- Heter.result1[[2]][(M+1):(M+1+number.of.tumor),(M+1):(M+1+number.of.tumor)]
  Heter.result2 = EMmvpoly(y.pheno.mis2,baselineonly = NULL,additive = cbind(gene2,x.covar2),pairwise.interaction = NULL,saturated = NULL,missingTumorIndicator = 888)

  M <- nrow(z.standard)
  number.of.tumor <- ncol(z.standard)
  log.odds2 <- Heter.result2[[1]][(M+1):(M+1+number.of.tumor)]
  sigma.log.odds2 <- Heter.result2[[2]][(M+1):(M+1+number.of.tumor),(M+1):(M+1+number.of.tumor)]
  meta.result <- LogoddsMetaAnalysis(log.odds1,
                                     sigma.log.odds1,
                                     log.odds2,
                                     sigma.log.odds2)
  second.stage.logodds.meta <- meta.result[[1]]
  second.stage.sigma.meta <- meta.result[[2]]
  test.result.second.wald <- DisplaySecondStageTestResult(second.stage.logodds.meta,second.stage.sigma.meta)
  p.value <- test.result.second.wald[length(test.result.second.wald)-1]
  return(list(meta.result,globalp = p.value))
}
#' Title
#'
#' @param y.pheno.mis1
#' @param gene1
#' @param x.covar1
#' @param y.pheno.mis2
#' @param gene2
#' @param x.covar2
#'
#' @return
#' @export
#'
#' @examples
two_data_two_stage_random <- function(y.pheno.mis1,
                                      gene1,
                                      x.covar1,
                                      y.pheno.mis2,
                                      gene2,
                                      x.covar2){

  result1 <- two_stage_random(y.pheno.mis1,gene1,x.covar1)
  result2 <- two_stage_random(y.pheno.mis2,gene2,x.covar2)
  meta.result.fixed  <- ScoreMetaAnalysis(result1[[1]],
                                          result1[[2]],
                                          result2[[1]],
                                          result2[[2]])
  meta.result.random  <- ScoreMetaAnalysis(result1[[3]],
                                           result1[[4]],
                                           result2[[3]],
                                           result2[[4]])
  test.result <- DisplayMixedScoreTestResult(meta.result.fixed[[1]],
                                             meta.result.fixed[[2]],
                                             meta.result.random[[1]],
                                             meta.result.random[[2]])

  p.value <- test.result[1]
  return(p.value)

}


#' Title
#'
#' @param y.pheno.mis1
#' @param gene1
#' @param x.covar1
#'
#' @return
#' @export
#'
#' @examples
two_stage_random <-function(
  y.pheno.mis1,
  gene1,
  x.covar1
){
  score.test.support.fixed1 <- ScoreTestSupport(
    y.pheno.mis1,
    baselineonly = NULL,
    additive = x.covar1,
    pairwise.interaction = NULL,
    saturated = NULL,
    missingTumorIndicator = 888
  )
  score.test.fixed1<- ScoreTest(y=y.pheno.mis1,
                                x=as.matrix(gene1),
                                second.stage.structure="additive",
                                score.test.support=  score.test.support.fixed1,
                                missingTumorIndicator=888)
  score.fixed1 <- score.test.fixed1[[1]]
  infor.fixed1 <- score.test.fixed1[[2]]

  score.test.support.random1 <- ScoreTestSupportSelfDesign(
    y.pheno.mis1,
    x.self.design  = as.matrix(gene1),
    z.design = z.random.support,
    additive = x.covar1,
    pairwise.interaction = NULL,
    saturated = NULL,
    missingTumorIndicator = 888
  )
  score.test.random1<- ScoreTestSelfDesign(y=y.pheno.mis1,
                                           x= as.matrix(gene1),
                                           z.design=z.random.test,
                                           score.test.support=score.test.support.random1,
                                           missingTumorIndicator=888)
  score.random1 <- score.test.random1[[1]]
  infor.random1 <- score.test.random1[[2]]

  result <- list(  score.fixed= score.fixed1,
                   infor.fixed= infor.fixed1,
                   score.random = score.random1,
                   infor.random = infor.random1)
}

