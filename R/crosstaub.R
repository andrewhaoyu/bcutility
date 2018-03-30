

#' Title
#'
#' @param prs1
#' @param prs2
#'
#' @return
#' @export
#'
#' @examples
crosstaub <- function(prs1,prs2){
  n <- length(prs1)

  qun1 <- quantile(prs1,probs=
                     c(0,0.01,0.05,0.10,0.20,
                       0.40,0.60,0.80,
                       0.9,
                       0.95,0.99,1))
  qun2 <- quantile(prs2,probs=
                     c(0,0.01,0.05,0.10,0.20,
                       0.40,0.60,0.80,
                       0.9,
                       0.95,0.99,1))
  group1 <- cut(prs1,breaks = qun1)

  group2 <- cut(prs2,breaks=qun2)
  result <- table(group1,group2)/n
  colnames(result) <- c("<1%", "1-5%", "5-10%", "10-20%", "20-40%", "40-60%", "60-80%", "80-90%", "90-95%", "95-99%", ">99%")
  rownames(result) <- c("<1%", "1-5%", "5-10%", "10-20%", "20-40%", "40-60%", "60-80%", "80-90%", "90-95%", "95-99%", ">99%")
  return(result)
}
