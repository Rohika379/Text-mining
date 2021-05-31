

######### SNAPDEAL URL ###########
library(rvest)
smart <- "https://www.snapdeal.com/product/activa-act32-smart-80-cm/662384079103/reviews?&sortBy=HELPFUL#defRevPDP"
snapdeal_reviews <- NULL

for (i in 1:20){
  murl <- read_html(as.character(paste(smart,i,sep='=')))
  rev <- murl %>% html_nodes(".user-review") %>% html_text()
  snapdeal_reviews <- c(snapdeal_reviews,rev)
}
  
write.table(snapdeal_reviews,"sup_smart.txt")
getwd()

#### Sentiment Analysis ####
txt <- snapdeal_reviews

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

w_sub <- subset(w, w >= 65)
w_sub

barplot(w_sub, las=2, col = rainbow(30))


##### Word cloud #####
install.packages("wordcloud")
library(wordcloud)

wordcloud(words = names(w_sub), freq = w_sub)

w_sub1 <- sort(rowSums(tdm), decreasing = TRUE)
head(w_sub1)

wordcloud(words = names(w_sub1), freq = w_sub1) # all words are considered

# better visualization
wordcloud(words = names(w_sub1), freq = w_sub1, random.order=F, colors=rainbow(30), scale = c(2,0.5), rot.per = 0.4)
windows()
wordcloud(words = names(w_sub1), freq = w_sub1, random.order=F, colors= rainbow(30),scale=c(3,0.5),rot.per=0.3)
?wordcloud

windowsFonts(JP1 = windowsFont("MS Gothic"))
par(family = "JP1")
wordcloud(x1, scale= c(2,0.5))
?windowsFonts
warning()

#### Bigram ####
library(RWeka)
library(wordcloud)

minfreq_bigram <- 2
bitoken <- NGramTokenizer(x1, Weka_control(min = 2, max = 2))
two_word <- data.frame(table(bitoken))
sort_two <- two_word[order(two_word$Freq, decreasing = TRUE), ]

wordcloud(sort_two$bitoken, sort_two$Freq, random.order = F, scale = c(2, 0.35), min.freq = minfreq_bigram, colors = brewer.pal(8, "Dark2"), max.words = 50)

