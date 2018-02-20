# Text Mining in Practice in R
# Mithun Radhakrishnan 
# mithunradhakrishnan@hotmail.com
# Chapter 2 Text mining basics

# Set Options 
options(stringsAsFactors = FALSE)
Sys.setlocale('LC_ALL','C')

# Calling libraries for data manipulation
library(stringi)
library(stringr)
library(qdap)

# Reading delta airways data 
text.df<-read.csv("oct_delta.csv")
nchar(head(text.df$text)) # The base function nchar will return the number of characters
# in a string. 
# Looking at specfic text, just have to convery the row & column
nchar(text.df[4,5])# Counts space as a character. 

mean(nchar(text.df$text))# mean of texts. 

#Remove blank texts 
subset.doc<-subset(text.df, nchar(text.df$text)>0)
dim(text.df)
dim(subset.doc)

#There are functions that replace defined patterns in the strings. These functions are
# help you replace or substitute parts of strings. 
# The sub function first looks for pattern match in the string and replaces it. 
sub('thanks','thank you',text.df[1,5],ignore.case = T)
# gsub is better than sub function
fake.text<-'R text mining is good but text mining is python is also'
sub('text mining','tm',fake.text,ignore.case = T)# only the first text mining is changed to TM
gsub('text mining','tm',fake.text,ignore.case = T)# gsub function changes everywhere.
gsub('&amp','',text.df[5,5])# if something needs to dropped all together.
# sub/gsub can be used to drop punctuations or specific words.
# specfically for punctuation we have a specfic code.
gsub('[[:punct:]]','',text.df[1:5,5])


library(qdap)
# making multiple substitutions at one go.
patterns<- c('good','also','text mining')
replacement<-c('great','just as suitable','tm')
mgsub(patterns,replacement,fake.text)# mgsub is more efficient. 

# Things to know about String Manipulation:
# Paste
patterns<- c('Jan','Feb','Mar','April','May','June','Jully','Aug','Sep','Oct','Nov','Dec')
replacements<-seq(1:12)
text.df$month<-mgsub(patterns,replacements,text.df$month)
text.df$combined<-paste(text.df$month,text.df$date,text.df$year,sep = '-')
# Paste creates a new vector called combined. Paste0 cam be used
# if there is no seperating characters needed. 

## Now have to covert text.df$combined into dates. as its not 
# currently read as dates and date manipulation not possible.
library(lubridate)
text.df$combined<- mdy(text.df$combined)

text.df$text[1:2]
# String split function can split on the asterisk to identify the agent of each tweet. 
agents<-strsplit(text.df$text,'[*]') # need to put astrix in square brackets. 
agents[1:2]
# Substring functions
substring('R text mining is great', 18,22) # count spaces, i need the word from 18th to 22nd space.

last.chars<- function(text,num){ 
last<-substr(text,nchar(text)-num+1, nchar(text))
return(last)
}
last.chars('R text mining is great',5)
last.chars('mithun has a great sense of purpose',7)

weekdays<-subset(text.df,text.df$combined>= mdy('10-05-2015')
                 & text.df$combined<= mdy('10-09-2015'))
table(as.factor(last.chars(weekdays$text,2)))

## Keyword Scanning 
# Functions used grep and grep1 
grep('sorry',text.df$text,ignore.case = T)# grep notifies the tweets with sorry
sorry<-grepl('sorry',text.df$text,ignore.case = T)# grep1 treats sorry tweets as true
sum(sorry)/nrow(text.df)# expressed as % of sorries in al the tweets.Can use any other such tweets. 
#use the pipe to add more keywords
grep(c('sorry|apologize'),text.df$text,ignore.case = T)

#Lets find out whether tweet directed to website or phone numbers were shared
sum(grepl('http',text.df$text,ignore.case = T)/nrow(text.df))
sum(grepl('[0-9]{3})|[0-9]{4}',text.df$text))/nrow(text.df)

library(stringi) # if we are looking for to count the number of times
# with in a document a string was found use stri
stri_count(text.df$text, fixed = 'http')

library(stringr)
str_detect(text.df$text,'http')

patterns<-with(text.df,str_detect(text.df$text,'http') & str_detect(text.df$text,'DM'))
text.df[patterns,5]

## Pre-processing steps for bag of words Text Mining
options(stingsAsFactors=FALSE)
Sys.setlocale('LC_ALL','C')
library(tm)
library(stringi)

# Will continue with delta tweets, since they dont have unique identifier lets get it.
tweets<-data.frame(ID=seq(1:nrow(text.df)), text=text.df$text)

## Text cleaning tasks involve the following
# lowering text, removing punctuation, striping extra white spac, removing numbers and
# removing stopwords- stopwords are common words that do not provide any additional insights. 

# tolower functions fails when r encounters special characters that it is not able to
# recognise. way out is trycatch function
# return NA instead of tolower error
tryTolower<-function(x){# return NA when there is an error 
  y = NA #try catch error
  try_error = tryCatch(tolower(x), error= function(e)e)
  if(!inherits(try_error, 'error'))
    y=tolower(x)
  return(y)
}
  
custom.stopwords<- c(stopwords('english'),'lol','smh','delta')
## Creating the clean corpus
clean.corpus<- function(corpus){
  corpus<- tm_map(corpus,content_transformer(tryTolower))
  corpus<-tm_map(corpus,removeWords,custom.stopwords)
  corpus<- tm_map(corpus,  removePunctuation)
  corpus<- tm_map(corpus, stripWhitespace)
  corpus<- tm_map(corpus,removeNumbers)
  return(corpus)
}   
meta.data.reader<-


## Spell check
tm.definition<- 'txt mining is the process of distilling actionable insyghts from text'
which_misspelled(tm.definition)
check_spelling_interactive(tm.definition)
2
3
3

fix.text<-function(mystr){
  check<- check_spelling(mystr)
  splitted<-strsplit(mystr,split ='')
for(i in 1:length(check$row)){
splitted[[check$row[i]]][as.numeric(check$word.no[i])] = check$suggestion[i]}
df<- unlist(lapply(splitted,function(x) paste(x, collapse='')))
return(df)
}

fix.text(tm.definition)

newtweets<- data.frame(doc_id = tweets$ID,text=tweets$text)

corpus<- VCorpus(DataframeSource(newtweets))
corpus<- clean.corpus(corpus)
as.list(corpus)[1]

tdm<- TermDocumentMatrix(corpus,control = list(weighting = weightTf))
tdm.tweets.m<- as.matrix(tdm)
dim(tdm.tweets.m)
tdm.tweets.m[2250:2255,1340:1342]

term.freq<- rowSums(tdm.tweets.m)
freq.df<- data.frame(word = names(term.freq), frequency = term.freq)

freq.df<- freq.df[order(freq.df[,2],decreasing = TRUE),]
freq.df[1:10,]
