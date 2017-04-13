##############Text Mining + Naive Bayes##################
##############Detect Spam Emails########################
require(tm) || install.packages("tm")
library(tm)
require(SnowballC) || install.packages("SnowballC")
library(SnowballC)
require(wordcloud) || install.packages("wordcloud")
library(wordcloud)
require(e1071) || install.packages("e1071")
library(e1071)
require(RWeka) || install.packages("RWeka")
library(RWeka)
sms_raw <- read.csv("C:/Work/Knowledge/Rprogram/Datasets/sms_spam.csv", stringsAsFactors = FALSE)
str(sms_raw)
sms_raw$type <- factor(sms_raw$type)
table(sms_raw$type)
sms_corpus <- Corpus(VectorSource(sms_raw$text))
print(sms_corpus)    
inspect(sms_corpus[1:2])
str(sms_corpus)
###### Clean text#####
sms_corpus_clean <- tm_map(sms_corpus,
                           content_transformer(tolower))
as.character(sms_corpus_clean[[1]])
sms_corpus_clean <- tm_map(sms_corpus_clean, removeNumbers)
as.character(sms_corpus_clean[[1]])
sms_corpus_clean <- tm_map(sms_corpus_clean,
                           removeWords, stopwords())
as.character(sms_corpus_clean[[1]])
sms_corpus_clean <- tm_map(sms_corpus_clean, removePunctuation)
as.character(sms_corpus_clean[[1]])
sms_corpus_clean <- tm_map(sms_corpus_clean, stripWhitespace)
as.character(sms_corpus_clean[[1]])
sms_dtm <- DocumentTermMatrix(sms_corpus_clean)

sms_dtm
sms_dtm_train <- sms_dtm[1:4169, ]
sms_dtm_test <- sms_dtm[4170:5559, ]
sms_train_labels <- sms_raw[1:4169, ]$type
sms_test_labels <- sms_raw[4170:5559, ]$type

prop.table(table(sms_train_labels))
wordcloud(sms_corpus_clean, min.freq = 50, random.order = FALSE)
spam <- subset(sms_raw, type == "spam")
ham <- subset(sms_raw, type == "ham")
wordcloud(spam$text, max.words = 40, scale = c(3, 0.5))
wordcloud(ham$text, max.words = 40, scale = c(3, 0.5))
sms_freq_words <- findFreqTerms(sms_dtm_train, 5)
str(sms_freq_words)
sms_dtm_freq_train<- sms_dtm_train[ , sms_freq_words]
sms_dtm_freq_test <- sms_dtm_test[ , sms_freq_words]

convert_counts <- function(x) {
  x <- ifelse(x > 0, "Yes", "No")
}
sms_train <- apply(sms_dtm_freq_train, MARGIN = 2,
                   convert_counts)
sms_test <- apply(sms_dtm_freq_test, MARGIN = 2,
                  convert_counts)

sms_classifier <- naiveBayes(sms_train, sms_train_labels)
sms_test_pred <- predict(sms_classifier, sms_test)
library(gmodels)
CrossTable(sms_test_pred, sms_test_labels,
           prop.chisq = FALSE, prop.t = FALSE,
           dnn = c('predicted', 'actual'))
######################Improve the Model#################################
sms_classifier2 <- naiveBayes(sms_train, sms_train_labels,
                              laplace = 1)
sms_test_pred2 <- predict(sms_classifier2, sms_test)
CrossTable(sms_test_pred2, sms_test_labels,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))