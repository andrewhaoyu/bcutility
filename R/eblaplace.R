#' Title
#'
#' @param betahat
#' @param Sigma
#' @param beta0
#' @param heter.sigma
#'
#' @return
#' @export
#'
#' @examples
eblaplace <- function(betahat,Sigma,beta0,heter.sigma){
  M <- length(betahat)
  if(heter.sigma==0){
    log.odds.meta.la <- rep(beta0,M)
  }else{
    b <- sqrt(heter.sigma/2)
    data <- list(M=M,beta0=beta0,betahat=betahat,Sigma=Sigma,
                 b=b)

    stan.model <- '
  data{
  //define data
  int<lower=1> M;
  real beta0;
  vector[M] betahat;
  matrix[M,M] Sigma;
  real<lower=0> b;
  }
  parameters{
  vector[M] beta;
  }
  model{
  //prior
  for(i in 1:M){
  beta[i] ~ double_exponential(beta0,b);
  }
  //data
  betahat ~ multi_normal(beta,Sigma);
  }
  '
    smodel <- stan_model(model_code = stan.model)
    fit1 <- sampling(smodel,
                     data=data,
                     warmup=5000,
                     iter=10000,
                     control = list(adapt_delta=0.95),
                     chains=4)
    #traceplot(fit1,pars=c('beta'))

    log.odds.meta.la <- colMeans(rstan::extract(fit1)[[1]])
  }
return(log.odds.meta.la)
}
