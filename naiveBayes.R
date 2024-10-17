install.packages("SnowballC")
library(e1071)
stopwords()
sms_raw <-read.csv(file.choose(),stringsAsFactor =FALSE)
str(sms_raw)
sms_raw$type <-factor(sms_raw$type)
str(sms_raw$type)
table(sms_raw$type)
library(tm)
sms_corpus <-VCorpus(VectorSource(sms_raw$text))
#View(sms_corpus)
print(sms_corpus)
inspect(sms_corpus[1:3])
sms_corpus
as.character(sms_corpus[[1]])
lapply(sms_corpus[1:2],as.character)
#View(sms_corpus)
sms_corpus_clean <-tm_map(sms_corpus,content_transformer(tolower))
#View(sms_corpus_clean)

as.character(sms_corpus_clean[[1]])
as.character(sms_corpus_clean[[3]])

sms_corpus_clean<-tm_map(sms_corpus_clean,removeNumbers)
as.character(sms_corpus_clean[[3]])
sms_corpus_clean<-tm_map(sms_corpus_clean,removeWords, stopwords())

as.character(sms_corpus_clean[[3]])
sms_corpus_clean<- tm_map(sms_corpus_clean,removePunctuation)
as.character((sms_corpus_clean[[3]]))

library(SnowballC)

wordStem(c("learns","learned","learning","learns"))
sms_corpus_clean<- tm_map(sms_corpus_clean,stemDocument)

sms_corpus_clean<-tm_map(sms_corpus_clean,stripWhitespace)

sms_dtm<-DocumentTermMatrix(sms_corpus_clean)

#View(sms_dtm)
inspect(sms_dtm)
sms_dtm_train <- sms_dtm[1:4169, ]
inspect(sms_dtm_train)
sms_dtm_test <-sms_dtm[4170:5559, ]
inspect(sms_dtm_test)

sms_train_labels<-sms_raw[1:4169, ]$type
sms_test_labels<-sms_raw[4170:5559, ]$type
install.packages("wordcloud")
library(wordcloud)
wordcloud(sms_corpus_clean,min.freq = 50,random.order = FALSE)
findFreqTerms(sms_dtm_train,5)
sms_freq_words <-findFreqTerms(sms_dtm_train, 5)

str(sms_freq_words)
sms_dtm_freq_train<- sms_dtm_train[ ,sms_freq_words]
sms_dtm_freq_test <-sms_dtm_test[ , sms_freq_words]
inspect(sms_dtm_freq_train)
convert_counts <- function(x){
  x <- ifelse(x > 0,"Yes","No")
}
sms_train <-apply(sms_dtm_freq_train,MARGIN=2, convert_counts)
sms_test <- apply(sms_dtm_freq_test,MARGIN = 2, convert_counts)


#View(sms_train)

library(e1071)
sms_classiffier <-naiveBayes(sms_train,sms_train_labels)

sms_test_pred <- predict(sms_classiffier,sms_test)
head(sms_test_pred)
a=table(sms_test_pred,sms_test_labels)
a
#install.packages("gmodels")
library(gmodels)
CrossTable(sms_test_pred, sms_test_labels,prop.chisq = FALSE,prop.t = FALSE,dnn = c('predicted','actual'))
install.packages("stringi")
library(stringr)
library(ggplot2)
library(lattice)
library(caret)
confussionMatrix(a)

