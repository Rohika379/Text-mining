
######### Amazon URL ###########
library(rvest)
Speaker <- "https://www.flipkart.com/hp-14-core-i5-10th-gen-8-gb-512-gb-ssd-windows-10-home-14-ck2018tu-thin-light-laptop/product-reviews/itm0b2aab6f42997?pid=COMFVD2WVZWBNKZA&lid=LSTCOMFVD2WVZWBNKZAP4TG2S&marketplace=FLIPKART"
flipcart_reviews <- NULL

for (i in 1:5){
  murl <- read_html(as.character(paste(Speaker,i,sep='')))
  rev <- murl %>% html_nodes(".t-ZTKy") %>% html_text()
  flipcart_reviews <- c(flipcart_reviews,rev)
}

write.table(flipcart_reviews,"hp1034tu.txt")
getwd()

#### Sentiment Analysis ####
txt <- flipcart_reviews

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
dtm <- t(tdm) # transpose
dtm <- DocumentTermMatrix(x1)

# To remove sparse entries upon a specific value
corpus.dtm.frequent <- removeSparseTerms(tdm, 0.99) 

tdm <- as.matrix(tdm)
dim(tdm)

tdm[1:20, 1:20]

inspect(x[1])

tdm <- TermDocumentMatrix(x1)

tdm <- as.matrix(tdm)
tdm[100:109, 1:20]

# Bar plot after removal of the term 'phone'
w <- rowSums(tdm)
w
w_sub <- subset(w, w >= 20)
w_sub

barplot(w_sub, las=2, col = rainbow(30))

##### Word cloud #####
install.packages("wordcloud")
library(wordcloud)

wordcloud(words = names(w_sub), freq = w_sub)

w_sub1 <- sort(rowSums(tdm), decreasing = TRUE)
head(w_sub1)

wordcloud(words = names(w_sub1), freq = w_sub1) # all words are considered


############# Wordcloud ###############

installed.packages("wordcloud2")
library(wordcloud2)

w1 <- data.frame(names(w_sub), w_sub)
colnames(w1) <- c('word', 'freq')

wordcloud2(w1, size=0.3, shape='circle')
?wordcloud2

wordcloud2(w1, size=0.3, shape = 'triangle')
wordcloud2(w1, size=0.3, shape = 'star')


#### Bigram ####
library(RWeka)
library(wordcloud)

minfreq_bigram <- 2
bitoken <- NGramTokenizer(x1, Weka_control(min = 2, max = 2))
two_word <- data.frame(table(bitoken))
sort_two <- two_word[order(two_word$Freq, decreasing = TRUE), ]

wordcloud(sort_two$bitoken, sort_two$Freq, random.order = F, scale = c(2, 0.35), min.freq = minfreq_bigram, colors = brewer.pal(8, "Dark2"), max.words = 150)

