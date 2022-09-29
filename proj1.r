## Ziwen Zhong S2326022
## Wenzheng Zhang
## Tianqi Dai

## Contribution


####################################################################
a <- scan("pg10.txt", what="character", skip=104) ## skip contents
n <- length(a)
a <- a[-((n-2886):n)] ## strip license
a <- a[-grep("[0123456789]:[0123456789]", a)] ## strip out verse numbers

## pre-processing: definition of the function split_punct
split_punct <- function(a){
  ii <- grep('[,.;!:?]', a)
  xs <- rep("", length(a)+length(ii))
  iis <- ii + 1:length(ii)
  xs[iis] <- substr(a[ii], nchar(a[ii]), nchar(a[ii]))
  xs[-iis] <- gsub('[,.;!:?]', '', a)
  return(xs)
}

## use split_punct to process a
a <- split_punct(a)
a <- tolower(a)

b <- unique(a)
e <- match(a, b)
fre <- tabulate(e)
b <- b[which(fre>=500)]



