setwd("/Users/ahenrichs/Desktop/WWO/WWO/Wroth and Sidney Herbert/FINAL complete cleaned files WITH stopwords")
getwd()
filenames <- list.files(getwd(),pattern="*.txt")
filenames
#create corpus
files.l<-lapply(filenames,readLines)
#warnings()

library(tm)
#install.packages("tidyverse")
library(tidyverse)  # data manipulation
#install.packages("cluster")
library(cluster)    # clustering algorithms
#install.packages("factoextra")
library(factoextra) # clustering visualization
#install.packages("dendextend")
library(dendextend)
#corpus with robert and stopwords
docs.c<-Corpus(VectorSource(files.l))
#could skip this bit and make the corpus directly into a df? apr2019

#options(max.print = 100000000)
#now chunk and run dendrograms
full.words.l<-gsub("[^[:alnum:][:space:]']", " ", files.l)
full.words.l<-strsplit(full.words.l, "\\s+")
#now make a vector, in order to split into chunks
full.words.v<-unlist(full.words.l)
#full.words.v

#make chunks!!!
chunks.l.3000 <- split(full.words.v, ceiling(seq_along(full.words.v)/3000))
#chunks.l.3000

#make vector in order to be able to human-read where the chunk divisions are
chunks.v.3000<-unlist(chunks.l.3000)
length(chunks.v.3000)
#set up for loop; i=the length of the total vector
#i<-length(chunks.v.3000)
#capture.output writes the results of the for loop to a txt file (doesn't work for csv)
#capture.output(summary(for(i in 1:length(chunks.l.3000)){
  #print(chunks.l.3000[i])
#}), file = "chunk divisions with robert full 3000.txt")

#now back to corpus, then to dtm
corpus.chunks.c.3000<-Corpus(VectorSource(chunks.l.3000))

#now try to dendrogram
#convert to dtm, then to matrix

corpus.chunks.dtm.3000<-DocumentTermMatrix(corpus.chunks.c.3000)
corpus.chunks.m.3000<-as.matrix(corpus.chunks.dtm.3000)
#this produces a chunk-term matrix (or ctm)
write.csv(corpus.chunks.m.3000, file = "chunked corpus WITH robert WITH stopwords.csv")

#try dataframe, to scale and then enhance dendrograms Apr2019
corpus.chunks.df.3000<-as.data.frame(corpus.chunks.m.3000)
#??data.frame
corpus.chunks.df.na.omit<-na.omit(corpus.chunks.df.3000)
corpus.chunks.df.clean<-scale(corpus.chunks.df.na.omit)

#the below chunk was to test if skipping the dtm would work; doesn't seem to.
corpus.docs.c<-Corpus(VectorSource(docs.c))
corpus.docs.dtm<-DocumentTermMatrix(corpus.docs.c)
corpus.docs.m<-as.matrix(corpus.docs.dtm)
corpus.docs.df<-as.data.frame(corpus.docs.m)
#now remove empty items, and scale
corpus.docs.df.na.omit<-na.omit(corpus.docs.df)
corpus.docs.df.clean<-scale(corpus.docs.df.na.omit)

#write to file to try to view; doesn't work, outputs only numerical values
#i.2<-length(corpus.chunks.df.clean)
#capture.output(summary(for(i.2 in 1:length(corpus.chunks.df.clean)){
#print(corpus.chunks.df.clean[i.2])
#}), file = "corpus.df.clean.csv")
#head(corpus.chunks.df.clean)

# Compute with agnes, which gives the agglomerative coefficient score; closer to 1 means stronger clustering structure.
corpus.chunks.df.2<- agnes(corpus.chunks.df.clean, method = "complete")
corpus.docs.df.2.noscale<-agnes(corpus.docs.df, method = "complete")
# Agglomerative coefficient
corpus.docs.df.clean.2$ac
corpus.chunks.df.2$ac

# Dissimilarity matrix
d <- dist(corpus.chunks.df.clean, method = "euclidean")
d2<-dist(corpus.docs.df, method = "euclidean")

# Hierarchical clustering using Complete Linkage; complete means that the distance between pairs is calculated
#based on the members of each pair that are MOST distant
hc1 <- hclust(d, method = "complete" )

# Plot the obtained dendrogram
plot(hc1, cex = 0.6, hang = -1)
# Ward's method
hc5 <- hclust(d, method = "ward.D2" )

# Cut tree into 4 groups
sub_grp <- cutree(hc5, k = 4)

# Number of members in each cluster; this is how many chunks are in each cluster, ie has been grouped according to similarity
table(sub_grp)

#add clusters to the original dataset, in this case the sub_grp has to match the original variable
corpus.chunks.df.na.omit%>%
  mutate(cluster = sub_grp) %>%
  head
??mutate
#plot this
plot(hc5, cex = 0.6)
rect.hclust(hc5, k = 4, border = 2:5)
#reformat dendro to compare with earlier dendros
colors.4<-c("red", "dark blue", "dark green", "blue")
clusters.4<-cutree(hc5, 4)
library(ape)
plot(as.phylo(hc5), type = "phylo", tip.color = colors.4[clusters.4],
     label.offset = 1, cex = 0.7)

#different visual of agnes
fviz_cluster(list(data = corpus.chunks.df.clean, cluster = sub_grp))
#shows 123-126 in a cluster: that is Robert Sidney's poetry!!!!!

#make tanglegram, comparing two dendrograms that use diff measures 
# Compute distance matrix
res.dist <- dist(corpus.chunks.df.clean, method = "euclidean")

# Compute 2 hierarchical clusterings
hc1 <- hclust(res.dist, method = "complete")
hc2 <- hclust(res.dist, method = "ward.D2")

# Create two dendrograms
dend1 <- as.dendrogram (hc1)
dend2 <- as.dendrogram (hc2)

tanglegram(dend1, dend2)

#calculate entanglement measure, lower number is less entanglement, 
#meaning measurements are better
dend_list <- dendlist(dend1, dend2)

tanglegram(dend1, dend2,
           highlight_distinct_edges = FALSE, # Turn-off dashed lines
           common_subtrees_color_lines = FALSE, # Turn-off line colors
           common_subtrees_color_branches = TRUE, # Color common branches 
           main = paste("entanglement =", round(entanglement(dend_list), 2))
)

#compute distance between document vectors
corpus.chunks.d.3000 <- dist(corpus.chunks.m.3000)
#run hierarchical clustering using Ward’s method
corpus.chunks.full.groups.3000 <- hclust(corpus.chunks.d.3000,method="ward.D")
#plot dendogram, use hang to ensure that labels fall below tree
plot(corpus.chunks.full.groups.3000)

#group dendrogram
rect.hclust(corpus.chunks.full.groups.3000,8)


library(ape)
plot(as.phylo(corpus.chunks.full.groups.3000), type="fan")
plot(as.phylo(corpus.chunks.full.groups.3000), type="cladogram")
plot(as.phylo(corpus.chunks.full.groups.3000), type="unrooted")

#create new variable to zoom in and otherwise manipulate the visual
corpus.dendro.full.3000<-as.dendrogram(corpus.chunks.full.groups.3000)
plot(corpus.dendro.full.3000, xlim = c(50,100), ylim = c(1,500))

colors.10<-c("red", "dark blue", "dark green", "black", "orange", "purple","blue","green","yellow","pink")
clusters.10<-cutree(corpus.chunks.full.groups.3000, 10)
plot(as.phylo(corpus.chunks.full.groups.3000), type = "clado", tip.color = colors.10[clusters.10],
     label.offset = 1, cex = 0.7)


