
setwd("C:/Users/Amanda/Desktop/WWO/WWO")


getwd()

input.dir <- "Wroth and Sidney Herbert/R corpus"
files.v <- dir(path=input.dir, pattern=".*xml")

file.path(input.dir, files.v)


library(XML)  
#create a function to load xml nodes
getTEIWordTableList <- function(doc.object){
    paras <- getNodeSet(doc.object,
                        "/d:TEI/d:text/d:body/d:div1/d:div2/d:ab//d:w",
                        c(d = "http://www.tei-c.org/ns/1.0"))
    words <- paste(sapply(paras,xmlValue), collapse=" ")
    words.lower <- tolower(words)
    words.l <- strsplit(words.lower, "\\W")
    word.v <- unlist(words.l)
    text.freqs.t <- table(word.v[which(word.v!="")])
    text.freqs.rel.t <- 100*(text.freqs.t/sum(text.freqs.t))
    return(text.freqs.rel.t)
}

#test code
i<-1
file.path(input.dir, files.v[i])
doc.object <- xmlTreeParse(file.path(input.dir, files.v[i]),
                           useInternalNodes=TRUE)
doc.object

#call above function and embed in loop
source("WWO/WWO")
text.freqs.l <- list() # a list object to hold the results
for(i in 1:length(files.v)){
  doc.object <- xmlTreeParse(file.path(input.dir, files.v[i]),
                             useInternalNodes=TRUE)
  worddata <- getTEIWordTableList(doc.object)
  text.freqs.l[[files.v[i]]] <- worddata
}
str(worddata)
??source

text.freqs.l
worddata