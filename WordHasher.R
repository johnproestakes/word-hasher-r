createHash<- function(sline){
  arr <- as.list(strsplit(gsub("[^[:alnum:][:space:]]", "", sline), " ")[[1]])
  len <- length(arr)
  n1 <-1
  a <- c()
  for(x in arr){
    n2 <-1
    l <- c()
    for(y in 0:(len-n1)){
      l<-c(l,arr[n1+y])
      p<- paste(l, collapse=" ")
      if(p %in% a) { } else { a <- c(a, p) }
      n2 = n2+1
    }
    n1=n1+1
  }
  sapply(a, tolower)
  a <- a[order(sapply(a,nchar),decreasing=T)]
  return(list(a))
}
percentSimilarWords <- function(dx,dy){
  a <- dx[dy %in% dx]
  return(length(a)/length(dx))
}

csv <- read.csv("/Users/johnproestakes/Downloads/SubjectLines.csv", header=TRUE)
theL <- list()

percentSimilarWords(createHash("35 subject line."),createHash("more stuff learn laskjdf werd subject about"))
createHash("35 subject line.")
createHash("more stuff learn about")

names(csv)
q <- createHash("Please attend webinar today!") #my Query
theL <- c(theL, q)
fr<-as.data.frame(c(theL,list("")))
colnames(fr)<-c("word","wow")
fr
write.csv(fr, "/Users/johnproestakes/Downloads/SubjectLines_exp.csv")
csv$Similarity = percentSimilarWords(q, createHash(csv$Email.Subject.Line))
