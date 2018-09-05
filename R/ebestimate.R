
#' Title
#'
#' @param logodds.subtype
#' @param sigma.subtype
#' @param logodds.standard
#' @param prior.sigma
#'
#' @return
#' @export
#'
#' @examples
EbestimateNew <- function(logodds.subtype,
                          sigma.subtype,
                          logodds.standard,
                          prior.sigma
){
  M <- length(logodds.subtype)
  if(det(prior.sigma)==0){
    result <- as.vector(rep(logodds.standard,M))
  }else{
    result <- solve(solve(sigma.subtype)+solve(prior.sigma))%*%(solve(sigma.subtype)%*%logodds.subtype+ solve(prior.sigma)%*%as.vector(rep(logodds.standard,M)))
  }

  return(result)

}



















#' Title
#'
#' @param logodds.subtype
#' @param sigma.subtype
#' @param logodds.standard
#' @param prior.sigma
#'
#' @return
#' @export
#'
#' @examples
ebestimate <- function(logodds.subtype,
                       sigma.subtype,
                       logodds.standard,
                       prior.sigma
){
  M <- length(logodds.subtype)
  if(prior.sigma==0){
    return(rep(logodds.standard,M))
  }else{
    result <- solve(solve(sigma.subtype)+(1/prior.sigma)*diag(M))%*%(solve(sigma.subtype)%*%logodds.subtype+
                                                                       (1/prior.sigma)*rep(logodds.standard,M))
    return(result)
  }
}



#' Title
#'
#' @param logodds.subtype
#' @param sigma.subtype
#' @param logodds.standard
#' @param p.heter
#' @param pcutoff
#'
#' @return
#' @export
#'
#' @examples
dicestiamte <- function(logodds.subtype,
                        sigma.subtype,
                        logodds.standard,
                        p.heter,
                        pcutoff){
M <- length(logodds.subtype)
  if(p.heter<pcutoff){
  return(logodds.subtype)
}else{
  return(rep(logodds.standard,M))
}
}
