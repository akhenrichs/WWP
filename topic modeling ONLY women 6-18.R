library(tm)
setwd("C:/Users/Amanda/Dropbox/Sidney women/WWO/WWO/Wroth and Sidney Herbert/txt files")
getwd()
filenames <- list.files(getwd(),pattern="*.txt")
filenames
#create corpus
files<-lapply(filenames,readLines)

docs<-Corpus(VectorSource(files))


#clean corpus, including regularizing special characters
toSpace <- content_transformer(function(x, pattern) {return (gsub(pattern, " ", x))})
docs<-tm_map(docs,content_transformer(tolower))


toAE<-content_transformer(function(x, pattern){return(gsub(pattern,"ae",x))})
docs<-tm_map(docs, toAE, "ã¦")


options(max.print = 1000000)
docs<-tm_map(docs, removePunctuation)
docs<-tm_map(docs, removeNumbers)
docs<-tm_map(docs, toSpace, "page")
docs<-tm_map(docs, stripWhitespace)
docs<-tm_map(docs, toSpace, "filename")

#left in some of the headings (ie tr, author, etc) because stripping them affected the rest of the text
docs<-tm_map(docs, toSpace, "fakesidneyantoniexml")
docs<-tm_map(docs, toSpace, "msidneyenw")
docs<-tm_map(docs, toSpace, "rgarnierpbo")
docs<-tm_map(docs, toSpace, "fakesidneydiscoursexml")
docs<-tm_map(docs, toSpace, "pmornayjvp")
docs<-tm_map(docs, toSpace, "fakwrothuraniaxml")
docs<-tm_map(docs, toSpace, "mwrothzoq")
#remove stopwords using the standard list in tm
docs <- tm_map(docs, removeWords, stopwords("english"))
#define and eliminate all custom stopwords
myStopwords <- c("hee", "shee", "one", "yet", "said", "thee", "thy","thou","anthony","antonie","cleopatra")
docs <- tm_map(docs, removeWords, myStopwords)



#write cleaned corpus to file
#writeCorpus(docs, "C:/Users/Amanda/Desktop/WWO/WWO/Wroth and Sidney Herbert/cleaned txt files", filenames = NULL)


#convert to dtm
dtm <- DocumentTermMatrix(docs)

#convert rownames to filenames
rownames(dtm) <- filenames
#collapse matrix by summing over columns
freq <- colSums(as.matrix(dtm))
#length should be total number of terms
length(freq)
#create sort order (descending)
ord <- order(freq,decreasing=TRUE)
#List all terms in decreasing order of freq and write to disk
freq[ord]
#inspect most frequently occurring terms
freq[head(ord)]
#write.csv(freq[ord],"word_freq.csv")

#load topic models library
library(topicmodels)


#Set parameters for Gibbs sampling
burnin <- 4000
iter <- 2000
thin <- 500
seed <-list(2003,5,63,100001,765)
nstart <- 5
best <- TRUE

#Number of topics
k <- 25
#Run LDA using Gibbs sampling
ldaOut.nostopwords <-LDA(dtm,k, method="Gibbs", control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))
#write out results
#docs to topics
ldaOut.topics.nostopwords <- as.matrix(topics(ldaOut.nostopwords))
#write.csv(ldaOut.topics.nostopwords,file=paste("LDAGibbs",k,"DocsToTopicsWWOnostopwords.csv"))

#top 20 terms in each topic
ldaOut.terms.nostopwords.20 <- as.matrix(terms(ldaOut.nostopwords,20))
#write.csv(ldaOut.terms.nostopwords,file=paste("LDAGibbs",k,"TopicsToTermsWWOnostopwords.csv"))

#probabilities associated with each topic assignment
topicProbabilities.nostopwords <- as.data.frame(ldaOut.nostopwords@gamma)

#write.csv(topicProbabilities.nostopwords,file=paste("LDAGibbs",k,"TopicProbabilitiesWWOnostopwords.csv"))

#Find relative importance of top 2 topics
topic1ToTopic2.nostopwords <- lapply(1:nrow(dtm),function(x)
  sort(topicProbabilities.nostopwords[x,])[k]/sort(topicProbabilities.nostopwords[x,])[k-1])

#Find relative importance of second and third most important topics
topic2ToTopic3.nostopwords <- lapply(1:nrow(dtm),function(x)
  sort(topicProbabilities.nostopwords[x,])[k-1]/sort(topicProbabilities.nostopwords[x,])[k-2])

#write to file
#write.csv(topic1ToTopic2.nostopwords,file=paste("LDAGibbs",k,"Topic1ToTopic2WWOnostopwords.csv"))
#write.csv(topic2ToTopic3.nostopwords,file=paste("LDAGibbs",k,"Topic2ToTopic3WWOnostopwords.csv"))

#create wordclouds based on term frequency 
#wordcloud
library(wordcloud)
#setting the same seed each time ensures consistent look across clouds
set.seed(50)
#limit words by specifying min frequency
wordcloud(names(freq),freq, min.freq=70)
#this produces a massive word cloud, since the top 10 terms are used over 500 times each
#try again
wordcloud(names(freq), freq, min.freq=100)
wordcloud(names(freq), freq, min.freq=250)

#to fix, can create topic models with more terms
#top 25 terms in each topic
ldaOut.terms25.nostopwords <- as.matrix(terms(ldaOut.nostopwords,25))
#top 50 terms in each topic
ldaOut.terms50<-as.matrix(terms(ldaOut.nostopwords, 50))

#try to make wordclouds with the larger topics; not going to work b/c not proportional

#try to modify Jockers code for wordclouds
#from now on, only works in windows, because Java can go fuck itself
library(XML)
library(mallet)

library(rJava)


chunk.size <- 1000 # number of words per chunk

makeFlexTextChunks <- function(docs, chunk.size=500, percentage=TRUE){
  x <- seq_along(docs)
  if(percentage){
    max.length <- length(docs)/chunk.size
    chunks.l <- split(docs, ceiling(x/max.length))
  } else {
    chunks.l <- split(docs, ceiling(x/chunk.size))
    #deal with small chunks at the end
    if(length(chunks.l[[length(chunks.l)]]) <=
       length(chunks.l[[length(chunks.l)]])/2){
      chunks.l[[length(chunks.l)-1]] <-
        c(chunks.l[[length(chunks.l)-1]],
          chunks.l[[length(chunks.l)]])
      chunks.l[[length(chunks.l)]] <- NULL
    }
  }
  chunks.l <- lapply(chunks.l, paste, collapse=" ")
  chunks.df <- do.call(rbind, chunks.l)
  return(chunks.df)
}
topic.m <- NULL
for(i in 1:length(files)){
  chunk.m <- makeFlexTextChunks(files, chunk.size,
                                percentage=FALSE)
  textname <- gsub("\\..*","", files[i])
  segments.m <- cbind(paste(textname,
                            segment=1:nrow(chunk.m), sep="_"), chunk.m)
  topic.m <- rbind(topic.m, segments.m)
}

documents <- as.data.frame(topic.m, stringsAsFactors=F)
colnames(documents) <- c("id", "text")

mallet.instances <- mallet.import(documents$id,
                                  documents$text,
                                  "em stopwordlist.csv",
                                  FALSE,
                                  token.regexp="[\\p{L}']+")

## Create a topic trainer object.
topic.model <- MalletLDA(num.topics=25)
topic.model$loadDocuments(mallet.instances)

vocabulary <- topic.model$getVocabulary()

word.freqs <- mallet.word.freqs(topic.model)

topic.model$setAlphaOptimization(40, 80)
topic.model$train(400)



word.freqs
head(vocabulary)
order(word.freqs[1:50], decreasing = TRUE)
vocabulary[1:50]
topic.words.m <- mallet.topic.words(topic.model,
                                    smoothed=TRUE,
                                    normalized=TRUE)
topic.top.words <- mallet.top.words(topic.model,
                                    topic.words.m, 100)
wordcloud(topic.top.words$words,
          topic.top.words$weights,
          c(4,.8), rot.per=0, random.order=F)

for(i in 1:25){
  topic.top.words <- mallet.top.words(topic.model,
                                      topic.words.m[i,], 100)
  print(wordcloud(topic.top.words$words,
                  topic.top.words$weights,
                  c(4,.8), rot.per=0,
                  random.order=F))
}

#add color to 20
for(i in 1:25){ 
  topic.top.words <- mallet.top.words(topic.model,
                                      topic.words.m[i,], 100)
  print(wordcloud(topic.top.words$words,
                  topic.top.words$weights,
                  c(4,.8), rot.per=0,
                  random.order=F,
                  colors=brewer.pal(6,"Dark2")))
}
