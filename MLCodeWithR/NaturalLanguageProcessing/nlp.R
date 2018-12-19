# NLP
# problem : identifying the review is positive or negative


# Importing a dataset with tab as delimiter
# quote = '' : Ignoring any kind of text
# to prevent idenitfying reviews as factors, stringsAsFactors = F
dataset_original = read.delim('Restaurant_Reviews.tsv', quote = '', stringsAsFactors = F)

# Clearing the texts
install.packages('tm')
install.packages('SnowballC') # for stopwords
library(tm)
library(SnowballC)
corpus = VCorpus(VectorSource(dataset$Review))
# putting all review into Lower case
corpus = tm_map(corpus, content_transformer(tolower))
#as.character(corpus[[1]])  to get first review
# removing all the numbers, for eg : check for index 841
corpus = tm_map(corpus, removeNumbers)
# removing punctuation
corpus = tm_map(corpus, removePunctuation)
# removing non relevant words, for eg : this is removed in "wow i loved this place"
corpus = tm_map(corpus, removeWords, stopwords())
# Stemming : Getting the root of each word, eg : for Loved the root is Love 
# jsut for simplifying the tenses which will be better for correlation for algorithm
corpus = tm_map(corpus, stemDocument)
# Removing the extra or multi spaces and convert it into single space
corpus = tm_map(corpus, stripWhitespace)

# Create the bag of words model by using Documentar Matrix to create sparse matrix
dtm = DocumentTermMatrix(corpus)

# filter all the words that are not frequent 
# contains 99 % of columns that have 1
#dtm = removeSparseTerms(dtm, 0.99)
dtm = removeSparseTerms(dtm, 0.999)
dataset = as.data.frame(as.matrix(dtm)) # created dataframe with independent variable
dataset$Liked = dataset_original$Liked # we added the dependent variable

# Here we use Random Forest Classification Model

# Encoding the target feature as factor bcoz we will get argument length erro in computing confusion matrix.
dataset$Liked = factor(dataset$Liked , levels = c(0,1))

# Splitting data into Training and Test set
install.packages('caTools') # if this is present then comment
library(caTools)
set.seed(123) # like putting random_state in python
split = sample.split(dataset$Liked, SplitRatio = 0.75)
training_set = subset(dataset,split == T)
test_set = subset(dataset, split == F)

#Feature Scaling  is not neede here
#training_set[,1:2] = scale(training_set[, 1:2]) 
#test_set[,1:2] = scale(test_set[, 1:2])


# Fitting  Random Forest Classifier to the Training Set
install.packages('randomForest')
library(randomForest)
classifier = randomForest(x = training_set[-692],
                          y = training_set$Liked,
                          ntree = 10)

# Predicting the Test Set Results

Y_pred = predict(classifier, newdata = test_set[-692]) 
#Y_pred = ifelse(prob_pred > 0.5, 1, 0)

# Making confusion Matrix
cm = table(test_set[, 692], Y_pred)
accuracy = (90+90)/200
  






