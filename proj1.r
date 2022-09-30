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

## create b
b <- unique(a)
e <- match(a, b)
fre <- tabulate(e)
b <- b[which(fre>=500)]

## generate T
e <- match(a, b)
t <- cbind(c(NA,NA,e),c(NA,e,NA),c(e,NA,NA))
# t <- t[complete.cases(t),]

MT <- array(0,c(length(b),length(b),length(b)))
count.na <- rowSums(is.na(t))
for (index in 1:nrow(t)){
  if (count.na[index] == 0){
    i <- t[index,1]
    j <- t[index,2]
    k <- t[index,3]
    MT[i,j,k] <- MT[i,j,k] + 1
  }
}

for (index1 in 1:length(b)){
  for (index2 in 1:length(b)) {
    if (sum(MT[index1,index2,])!=0){
      MT[index1,index2,] <- MT[index1,index2,]/sum(MT[index1,index2,])
    }
  }
}

## Generate A
e <- match(a, b)
t <- cbind(c(NA,e),c(e,NA))
# t <- t[complete.cases(t),]

MA <- array(0,c(length(b),length(b)))
count.na <- rowSums(is.na(t))
for (index in 1:nrow(t)){
  if (count.na[index] == 0){
    i <- t[index,1]
    j <- t[index,2]
    MA[i,j] <- MA[i,j] + 1
  }
}

for (index1 in 1:length(b)){
  if (sum(MA[index1,])!=0){
    MA[index1,] <- MA[index1,]/sum(MA[index1,])
  }
}

## Generate S
e <- match(a, b)
S <- tabulate(e)

## Word Generation

## simulation
result <- c()
firstWord <- sample(1:length(b), size=1, prob=S)
if (sum(MA[firstWord,])!=0){
  secondWord <- sample(1:length(b), size=1, prob=MA[firstWord,])
} else{
  secondWord <- sample(1:length(b), size=1, prob=S)
}
wl <- c(firstWord, secondWord)
for (i in 3:50){
  first <- wl[i-2]
  second <- wl[i-1]
  if (sum(MT[first,second,])!=0){
    nextWord <- sample(1:length(b), size=1, prob=MT[first,second,])
  } else if (sum(MA[second,])!=0){
    nextWord <- sample(1:length(b), size=1, prob=MA[second,])
  } else{
    nextWord <- sample(1:length(b), size=1, prob=S)
  }
  wl <- c(wl, nextWord)
}
wl
wordlist <- b[wl]
