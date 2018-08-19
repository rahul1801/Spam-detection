data = sms_spam
data$type = factor(data$type)
table(data$type)
install.packages("tm")
library(tm)
# We create a corpus so that the text is formulated into documents 
sms_corpus = VCorpus(VectorSource(data$text))
sms_corpus

# For inspecting the first few corpus documents 
inspect(sms_corpus[1:3])
as.character(sms_corpus[[1]])
lapply(sms_corpus[1:2],as.character)

# We transform all text to lower case using tm_map tranformation
sms_corpus_clean = tm_map(sms_corpus,content_transformer(tolower))
sms_corpus_clean = tm_map(sms_corpus_clean,removeNumbers)
sms_corpus_clean = tm_map(sms_corpus_clean,removeWords,stopwords())
sms_corpus_clean = tm_map(sms_corpus_clean,removePunctuation)
install.packages("SnowballC")
library(SnowballC)
sms_corpus_clean = tm_map(sms_corpus_clean,stemDocument)
sms_corpus_clean = tm_map(sms_corpus_clean,stripWhitespace)  
lapply(sms_corpus[1:3], as.character)
lapply(sms_corpus_clean[1:3], as.character)
sms_dtm = DocumentTermMatrix(sms_corpus_clean)

#Data Split
sms_dtm_train = sms_dtm[1:4169,]
sms_dtm_test = sms_dtm[4170:5559,]
sms_train_labels = data[1:4169,]$type
sms_test_labels = data[4170:5559,]$type

# We check if equal proportion of spam/ham is present in both train and test data
prop.table(table(sms_train_labels))
prop.table(table(sms_test_labels))

install.packages("wordcloud")
library(wordcloud)
wordcloud(sms_corpus_clean,min.freq = 50,random.order = F)

# Creating wordcloud seperately for spam and ham
spam = subset(data,type == "spam")
ham = subset(data,type == "ham")
wordcloud(spam$text,max.words = 40,scale = c(3,0.5))
wordcloud(ham$text,max.words = 40,scale = c(3,0.5))

sms_freq_words = findFreqTerms(sms_dtm_train,5)
sms_dtm_freq_train = sms_dtm_train[,sms_freq_words]
sms_dtm_freq_test = sms_dtm_test[,sms_freq_words]
sms_freq_words_test = findFreqTerms(sms_dtm_test,5)
sms_dtm_freq_test1 = sms_dtm_test[,sms_freq_words_test]

convert_counts = function(x) {
  ifelse(x>0,"Yes","No")
}
sms_train = apply(sms_dtm_freq_train, MARGIN = 2,convert_counts)
sms_test = apply(sms_dtm_freq_test1, MARGIN = 2,convert_counts)
library(e1071)
sms_classifier = naiveBayes(sms_train,sms_train_labels)
sms_test_pred = predict(sms_classifier,sms_test)
library(gmodels)
CrossTable(sms_test_pred,sms_test_labels,prop.chisq = F,prop.t = F,dnn = c('predicted','actual'))

# Improvinig model perfomance

sms_classifier2 = naiveBayes(sms_train,sms_train_labels,laplace = 1)
sms_test_pred2 = predict(sms_classifier2,sms_test)
CrossTable(sms_test_pred2,sms_test_labels,prop.chisq = F,prop.t = F,dnn = c('predicted','actual'))
