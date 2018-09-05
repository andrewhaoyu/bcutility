#' Title
#'
#' @param log.odds
#' @param sigma
#'
#' @return
#' @export
#'
#' @examples
HeterVarianceEstimate <- function(log.odds,sigma){
  M <- length(log.odds)
  result <- (sum((log.odds-mean(log.odds))^2)-sum(diag(sigma))+sum(sigma)/M)/(M-1)
  if(result <= 0){
    result <- 0
  }
  return(result)
}


#' Title
#'
#' @param log.odds
#' @param sigma
#' @param R
#'
#' @return
#' @export
#'
#' @examples
HeterVarianceEstimateNew <- function(log.odds,sigma,R){
  M <- length(log.odds)
  result <- (sum((log.odds-mean(log.odds))^2)-sum(diag(sigma))+sum(sigma)/M)/(M-sum(R)/M)
  #result <- (sum((log.odds-mean(log.odds))^2)-(M+1)*sum(diag(sigma))/M)/(M-1)
  #if(result <= 0){
  #  result <- 0
  # }
  if(result <= 0){
  result <- 0
}
return(result)
  return(result)
}
