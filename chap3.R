# Text mining associations are similar to the concept of coerelation
# as the frequency of single word occurs, how correlated or associated
# is it with another word. 
# Word Network: the relationship between words are captured in a 
# special matrix called adjacency matrix. 

## TERM FREQUENCY 
library(tm)
library(ggplot2)
library(ggthemes)
text.df<- read.csv('oct_delta.csv')
tweets<- data.frame(ID=seq(1:nrow(text.df)),text=text.df$text)
tryTolower <- function(x){
  y = NA
  try_error = tryCatch(tolower(x), error = function(e) e)
  if (!inherits(try_error, 'error'))
    y = tolower(x)
  return(y)
} # we need to create this function caues sometime try lower will fail
# with special characters & throw errors, this fuction make it NA. 

custom.stopwords<- c(stopwords('english'), 'lol', 'smh', 'delta', 'amp')
# have added some custom stopword based on my understanding of problem domain.

# Cleaning to covert to lower, removing punctuation, stopwords
# numbers and whitespaces. 
# following function was defined to automate the clean up. 
clean.corpus<- function(corpus){
corpus<- tm_map(corpus, content_transformer(tryTolower))
corpus<- tm_map(corpus, removeWords,custom.stopwords)
corpus<- tm_map(corpus, removePunctuation)
corpus<- tm_map(corpus, stripWhitespace)
corpus<- tm_map(corpus, removeNumbers)
return(corpus)
}

#  tweets was not a data frame, coverting into a data frame
tweets<- data.frame(doc_id = tweets$ID,text=tweets$text)
corpus<- VCorpus(DataframeSource(tweets))

# running the clean up function
corpus<- clean.corpus(corpus)

# Converting into a term document matrix
tdm<- TermDocumentMatrix(corpus,control=list(weighting=weightTf))

# Converting to tdm to matrix
tdm.tweets.m<- as.matrix(tdm)
# calculating term frequency
term.freq<- rowSums(tdm.tweets.m)
# creating a term frequcy data frame & ordering it
freq.df<-data.frame(word=names(term.freq),frequency=term.freq)
freq.df<- freq.df[order(freq.df[,2], decreasing=T),]

## Creating bar plot of most frequent words.
freq.df$word<- factor(freq.df$word, levels=unique(as.character(freq.df$word)))
ggplot(freq.df[1:20,], aes(x=word, y=frequency))+geom_bar(stat="identity", fill='darkred')+coord_flip()+theme_gdocs()+ geom_text(aes(label=frequency), colour="white",hjust=1.25, size=5.0)


# WORD ASSOCIATONS :similar to the concept of correlation
# but scale is 0 to 1, no negative correlation. 
associations<-findAssocs(tdm, 'apologies', 0.11) # 0.11 is just a random number on scale.
# need to convert to data frame
associations<- as.data.frame(associations)
associations$terms<- row.names(associations)
associations$terms<- factor(associations$terms, levels=associations$terms)
# Plotting the word associaton of word appologies
ggplot(associations,aes(y=associations$terms)) + geom_point(aes(x = associations$apologies),data = associations, size = 5)+
  theme_gdocs() + geom_text(aes(x = associations$apologies, label = associations$apologies), colour = "darkred", hjust = -0.25, size = 8)+
  theme(text = element_text(size = 20), axis.title.y = element_blank())

## WORD NETWORK 
install.packages('igraph')
library(igraph)
refund<- tweets[grep("refund", tweets$text, ignore.case=T), ]
refund.corpus<- VCorpus(DataframeSource(refund[1:3,]))

refund.corpus<- clean.corpus(refund.corpus)               
refund.tdm<-TermDocumentMatrix(refund.corpus,control=list(weighting=weightTf))

refund.m<- as.matrix(refund.tdm)
refund.adj<- refund.m %*% t(refund.m)
refund.adj<- graph.adjacency(refund.adj, weighted=TRUE, mode="undirected", diag=T)
refund.adj<- simplify(refund.adj)

plot.igraph(refund.adj, vertex.shape="none",
            vertex.label.font=2, vertex.label.color="darkred",
            vertex.label.cex=.7, edge.color="gray85")
title(main='@DeltaAssist Refund Word Network')

library(qdap)
word_network_plot(refund$text[1:3])
title(main='@DeltaAssist Refund Word Network')

word_associate(tweets$text, match.string = c('refund'),
               stopwords = Top200Words, network.plot = T, cloud.colors = c('gray85','darkred'))
title(main='@DeltaAssist Refund Word Network')
