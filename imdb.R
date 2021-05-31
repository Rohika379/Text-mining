


######### IMDB ###########
library(rvest)
IMDB <- "https://www.imdb.com/title/tt1477834/reviews?ref_=tt_ov_rt"
TZP <- NULL

for (i in 1){
  murl <- read_html(as.character(paste(IMDB,i,sep='=')))
  rev <- murl %>% html_nodes(".show-more__control") %>% html_text()
  TZP <- c(TZP,rev)
}

write.table(TZP,"Aquaman.txt")
getwd()

#### Sentiment Analysis ####
txt <- TZP

str(txt)
length(txt)
View(txt)

# install.packages("tm")
library(tm)

# Convert the character data to corpus type
x <- Corpus(VectorSource(txt))

x <- tm_map(x, function(x) iconv(enc2utf8(x), sub='byte'))


# Data Cleansing
x1 <- tm_map(x, tolower)

x1 <- tm_map(x1, removePunctuation)

x1 <- tm_map(x1, removeNumbers)

x1 <- tm_map(x1, removeWords, stopwords('english'))
inspect(x1[1])

# striping white spaces 
x1 <- tm_map(x1, stripWhitespace)
inspect(x1[1])

# Term document matrix 
# converting unstructured data to structured format using TDM

tdm <- TermDocumentMatrix(x1)
tdm
dtm <- t(tdm) # transpose
dtm <- DocumentTermMatrix(x1)

# To remove sparse entries upon a specific value
corpus.dtm.frequent <- removeSparseTerms(tdm, 0.99) 

tdm <- as.matrix(tdm)
dim(tdm)

tdm[1:20, 1:20]

inspect(x[1])

# Bar plot
w <- rowSums(tdm)
w

w_sub <- subset(w, w >= 10)
w_sub

barplot(w_sub, las=2, col = rainbow(10))

# Term laptop repeats maximum number of times
x1 <- tm_map(x1, removeWords, c('aquaman','film'))
x1 <- tm_map(x1, stripWhitespace)

tdm <- TermDocumentMatrix(x1)
tdm

tdm <- as.matrix(tdm)
tdm[100:109, 1:20]

# Bar plot after removal of the term 'phone'
w <- rowSums(tdm)
w
w_sub <- subset(w, w >=5)
w_sub

barplot(w_sub, las=2, col = rainbow(10))
