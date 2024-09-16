### R code from vignette source 'other-Bessels.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: preliminaries
###################################################
options(width=75)
library(Bessel)


###################################################
### code chunk number 2: Rmpfr-1
###################################################
suppressPackageStartupMessages(require("Rmpfr"))


###################################################
### code chunk number 3: gsl-do
###################################################
library(gsl)


###################################################
### code chunk number 4: gsl-help (eval = FALSE)
###################################################
## ?bessel_Knu
## ?Airy


###################################################
### code chunk number 5: gsl-bessel-ls
###################################################
igsl <- match("package:gsl", search())
aB <- apropos("Bessel", where=TRUE); unname(aB)[names(aB) == igsl]
aA <- apropos("Airy",   where=TRUE); unname(aA)[names(aA) == igsl]


###################################################
### code chunk number 6: bessel-real-nu
###################################################
lst <- ls(patt="bessel_.*nu", pos="package:gsl")
l2 <- sapply(lst, function(.) args(get(.)), simplify=FALSE)
lnms <- setNames(format(lst), lst)
arglst <- lapply(lst, ## a bit ugly, using deparse(.)
    function(nm) sub(" *$","", sub("^function", lnms[[nm]], deparse(l2[[nm]])[[1]])))
.tmp <- lapply(arglst, function(.) cat(format(.),"\n"))


###################################################
### code chunk number 7: bessel_Inu_scaled
###################################################
   x <- (1:500)*50000; b2 <- BesselI(x, pi, expo=TRUE)
   b1 <- bessel_Inu_scaled(pi, x)
   all.equal(b1,b2,tol=0) ## "Mean relative difference: 1.544395e-12"

   ## the accuracy is *as* limited (probably):
   b1 <- bessel_Inu_scaled(pi, x, give=TRUE)
   summary(b1$err)


###################################################
### code chunk number 8: bessel_Inu-relErr
###################################################
    range(b1$err/ b1$val)


###################################################
### code chunk number 9: Jnu-100
###################################################
bessel_Jnu(100,  2^seq(-5,1, by=1/4))
bessel_Jnu( 20,  2^seq(-50,-40, by=1/2))
bessel_Jnu(  5,  2^seq(-210,-200, by=.5))


###################################################
### code chunk number 10: Jnu-underflow-status-ex
###################################################
as.data.frame(bessel_Jnu( 20,  2^seq(-50,-40, by=1/2), give=TRUE, strict=FALSE))


###################################################
### code chunk number 11: J-gsl
###################################################
gslJ <- function(nu, f1 = .90, f2 = 1.10, nout = 512, give=FALSE, strict=FALSE) {
    stopifnot(is.numeric(nu), length(nu) == 1, nout >= 1, f1 <= 1, f2 >= 1)
    x <- seq(f1*nu, f2*nu, length.out = nout)
    list(x=x, Jnu.x = bessel_Jnu(nu, x, give=give, strict=strict))
}
plJ <- function(nu, f1 =.90, f2=1.10, nout=512,
                col=2, lwd=2, main = bquote(nu == .(nu)), ...) {
    dJ <- gslJ(nu, f1=f1, f2=f2, nout=nout)
    plot(Jnu.x ~ x, data=dJ, type="l", col=col, lwd=lwd, main=main, ...)
    abline(h=0, lty=3, col=adjustcolor(1, 0.5))
    invisible(dJ)
}
sfsmisc::mult.fig(4)
plJ(500, f1=0)
r1k <- plJ(1000, f1=0)
head(as.data.frame(r1k)) # all 0 now (NaN's for  'strict=TRUE' !!)
r10k <- plJ(10000, f1=0.5, f2=2)
str( with(r10k, x[!is.finite(Jnu.x)]) ) # empty; had all NaN upto x = 8317
r1M <- plJ(1e6, f1=0.8)


###################################################
### code chunk number 12: require-again
###################################################



###################################################
### code chunk number 13: sessionInfo
###################################################
toLatex(sessionInfo(), locale=FALSE)


###################################################
### code chunk number 14: show-date
###################################################
cat(sprintf("Date (run in R): %s\n", format(Sys.Date())))


