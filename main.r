library(psych)
factor<-read.table("factor.csv",sep=";",header=T,dec=",")
fa.parallel(factor, n.obs=147, fa='fa', n.iter=100)
library(GPArotation)
fa.promax<-fa(factor, nfactors=8, n.obs=147, rotate='promax', fm='pa')
fa.diagram(fa.promax, simple=TRUE)
fsm <- function(oblique) {
  if (class(oblique)[2]=='fa' & is.null(oblique$Phi)) {
    warning('Объект не похож на результат наклонного вращения факторов')
  } else {
    P <- unclass(oblique$loading)
    F <- P %*% oblique$Phi
    colnames(F) <- c('PA1', 'PA2','PA3','PA4','PA5','PA6')
    return(F)
  }
}
fsm(fa.promax)