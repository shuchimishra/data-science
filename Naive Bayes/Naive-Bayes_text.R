getwd()
sms_raw = read.csv("sms_spam.csv", stringsAsFactors = FALSE)

str(sms_raw)
sms_raw$type = factor(sms_raw$type)
str(sms_raw)

table(sms_raw$type)
prop.table(table(sms_raw$type))


install.packages("tm")
library(tm)

?VCorpus

getSources()
getReaders()
sms_corpus = VCorpus(VectorSource(sms_raw$text))

print(sms_corpus)
inspect(sms_corpus[1:3]) #reads the first 3 lines
lapply(sms_corpus[1:3],as.character)
as.character((sms_corpus[[1]]))

corpus_clean=tm_map(sms_corpus,content_transformer(tolower))
as.character((corpus_clean[[1]]))

corpus_clean=tm_map(corpus_clean,removeNumbers)
corpus_clean=tm_map(corpus_clean,removeWords, stopwords())
corpus_clean=tm_map(corpus_clean,removePunctuation)
as.character((corpus_clean[[1]]))

install.packages("SnowballC")
library(SnowballC)
wordStem(c("learn", "learned", "learning", "learns"))

corpus_clean <- tm_map(corpus_clean, stemDocument)
corpus_clean=tm_map(corpus_clean,stripWhitespace)
as.character((corpus_clean[[1]]))

sms_dtm <- DocumentTermMatrix(corpus_clean)
sms_dtm

sms_raw_train = sms_raw[1:4169,]
sms_raw_test = sms_raw[4170:5559,]

sms_dtm_train = sms_dtm[1:4169,]
sms_dtm_test = sms_dtm[4170:5559,]

sms_corpus_train = corpus_clean[1:4169]
sms_corpus_test = corpus_clean[4170:5559]

findFreqTerms(sms_dtm_train, 25)
sms_freq_words <- findFreqTerms(sms_dtm_train, 5)
str(sms_freq_words)

# create DTMs with only the frequent terms
sms_dtm_freq_train <- sms_dtm_train[ , sms_freq_words]
sms_dtm_freq_test <- sms_dtm_test[ , sms_freq_words]

# convert counts to a factor
convert_counts <- function(x) {
  x <- ifelse(x > 0, "Yes", "No")
}

sms_train <- apply(sms_dtm_freq_train, MARGIN = 2, convert_counts)
sms_test  <- apply(sms_dtm_freq_test, MARGIN = 2, convert_counts)

library(e1071)
sms_classifier <- naiveBayes(sms_train, sms_train_labels)

## Step 4: Evaluating model performance ----
sms_test_pred <- predict(sms_classifier, sms_test)

library(gmodels)
CrossTable(sms_test_pred, sms_test_labels,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))

## Step 5: Improving model performance ----
sms_classifier2 <- naiveBayes(sms_train, sms_train_labels, laplace = 1)
sms_test_pred2 <- predict(sms_classifier2, sms_test)
CrossTable(sms_test_pred2, sms_test_labels,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))

