## Ziwen Zhong S2326022
## Wenzheng Zhang S2310185
## Tianqi Dai S2302524

## Contributions: This file is group project 1 from Statistical Programming Group 1. 
## When project 1 was released,we discussed our opinions about the project and generalized 
## a guideline of our main idea and instructions for coding we would use. Ziwen, as our group leader, 
## allocated the schedule of the group discussion. We gathered after working from home and spoke about our own opinions.
## After making sure the questions are perfectly solved and everyone fully understands the command, 
## we put our work together. We use Tianqi’s step3 and step4, Wenzheng’s step5 and step6, 
## and Ziwen’s step7 and step8. For steps 9 and 10, we asked our questions about the ambiguous part to the lecturer. 
## We proposed our suggestions for the coding procedure and Ziwen decided on the most efficient and effective version. 
## After finishing the code, Tianqi tested the code and spotted some misunderstandings in the instruction, and we fixed them together. 
## Wenzheng wrote a rough note to explain every command, and we wrote the final comments together.
## Ziwen always has so many outstanding and creative ideas, he provided the team member with support and always inspire the team to attempt further and deeper. 
## This group project 1 was finished with good communication and collaborations and uploaded to Github and Learn on 06/10/2022


####################################################################

## steep 3
## Read the file into R
# setwd("")
a <- scan("pg10.txt", what="character", skip=104) ## skip contents
n <- length(a)
a <- a[-((n-2886):n)] ## strip license
a <- a[-grep("[0123456789]:[0123456789]", a)] ## strip out verse numbers
a

## step4
## pre-processing: definition of the function split_punct
split_punct <- function(a){
  ii <- grep('[,.;!:?]', a) ## locate the word with punctuation
  xs <- rep("", length(a)+length(ii)) ## create a space to store the final words and punctuation
  iis <- ii + 1:length(ii) ## calculate the location of the punctuation
  xs[iis] <- substr(a[ii], nchar(a[ii]), nchar(a[ii]))  ## fill the space with punctuation
  xs[-iis] <- gsub('[,.;!:?]', '', a) ## fill the rest with word
  return(xs) ## return the final vector which is word-punctuation apart
}

## step 5
## use split_punct function to process a
a <- split_punct(a) 

## step 6
## create b
a <- tolower(a) ## lowercase the vector a
b <- unique(a) ## find the vector b of unique words in the bible text
e <- match(a, b) ## find the index of which element in b each element of a corresponds to
fre <- tabulate(e) ## calculate the frequency of every unique word in a
thre_num <- sort(fre, decreasing=T)[500] ## arrange the frequency vector to a decreasing order to find the threshold number
b <- b[which(fre>=thre_num)] ## output about 500 most common words, store in vector b
## step 7
## generate T array
e <- match(a, b) ## find the index of which element in b each element of a corresponds to
t <- cbind(c(NA,NA,e),c(NA,e,NA),c(e,NA,NA)) ## combine 3 vectors, each vector followed by the same vector shifted by one place

MT <- array(0,c(length(b),length(b),length(b))) ## initialize an empty MT array to count
t <- t[rowSums(is.na(t))==0,] ## drop the word triplets that contain NA
for (index in 1:nrow(t)){
  ## loop through each row of word triples, count its frequency by add 1 to MT[i, k, j] every time [i, k, j] shows up.
    i <- t[index,1]
    k <- t[index,2]
    j <- t[index,3]
    MT[i,k,j] <- MT[i,k,j] + 1
}

## loop through each vector [i,k,], divide the sum of the vector to obtain the probability
for (index1 in 1:length(b)){
  for (index2 in 1:length(b)) {
    if (sum(MT[index1,index2,])!=0){
      MT[index1,index2,] <- MT[index1,index2,]/sum(MT[index1,index2,])
    }
  }
}

## Generate A array
e <- match(a, b) ## translate vector of words to vector of index corresponds to b.
t <- cbind(c(NA,e),c(e,NA))  ## combine 2 vectors, the first vector followed by the same vector shifted by one place

MA <- array(0,c(length(b),length(b)))  ## initialize an empty MA array to count
t <- t[rowSums(is.na(t))==0,]  ## drop the word doubles that contain NA
## loop through each row of word doubles, count its frequency by add 1 to MA[i, k] every time [i, k] shows up.
for (index in 1:nrow(t)){
    i <- t[index,1]
    k <- t[index,2]
    MA[i,k] <- MA[i,k] + 1
}

## loop through each vector [i,], divide the sum of the vector to obtain the probability
for (index1 in 1:length(b)){
  if (sum(MA[index1,])!=0){
    MA[index1,] <- MA[index1,]/sum(MA[index1,])
  }
}

## Generate vector S
e <- match(a, b)
S <- tabulate(e)
S <- S/sum(S) ## translate the frequency to probabilities


## step 8
## Word Generation
firstWord <- sample(1:length(b), size=1, prob=S) ## generate the first word where the word probabilities are simply taken from S
## generate the second word where the word probabilities are taken from A[word1,] as the first word were given
## if fail to do so, falling back to S
if (sum(MA[firstWord,])!=0){
  secondWord <- sample(1:length(b), size=1, prob=MA[firstWord,])
} else{
  secondWord <- sample(1:length(b), size=1, prob=S)
}
wl <- c(firstWord, secondWord) ## store the two words in vector wl

## as two words were given, use the word probabilities in T[word1, word2,], generate the 3rd word, 
## if fail to do so, try to generate the 3rd word using the word probabilities in A[word2,],
## if all failed, falling back to S,
## add the 3rd word to vector wl, and move on, use 2nd and 3rd word to generate the 4th one,
## keep going, until there are 50 word in vector wl.
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
wordlist1 <- b[wl] ## translate indexes from b to words.


## step 9
## simulate 50-word sections of text where the word probabilities are simply taken from S
wl2 <- c()
for (i in 1:50){
  word <- sample(1:length(b), size=1, prob=S)
  wl2 <- c(wl2,word)
}
wordlist2 <- b[wl2]

## show both result
wordlist1
wordlist2


## step 10

