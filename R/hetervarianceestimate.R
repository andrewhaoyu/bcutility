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
