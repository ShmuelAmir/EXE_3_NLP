library(dplyr)
library(stringr)
library(quanteda)
library(Matrix)
library(caret)
library(rpart)

# library(ggplot2)
# library(mosaic)
# library(xtable)
# library(gridExtra)
# library(stopwords)

# pdf()
options(digits = 2)

# ---------- Load texts ----------#
profiles <- read.csv('data/okcupid_profiles.csv.gz', header=TRUE, stringsAsFactors=FALSE)


# ---------- Clean texts, tokenize, remove stop words ----------#
profiles <- profiles %>% select(starts_with("essay"), sex)

essays <- select(profiles, starts_with("essay"))
essays <- apply(essays, MARGIN = 1, FUN = paste, collapse=" ")

profiles_sex <- profiles$sex
N <- length(profiles_sex)
rm(profiles)
  
html <- c( "<a[^>]+>", "class=[\"'][^\"']+[\"']", "&[a-z]+;", "\n", "\\n", "<br ?/>", "</[a-z]+ ?>" )
stop.words <- c( "a", "am", "an", "and", "as", "at", "are", "be", "but", "can", "do", "for", "have", "i'm", "if", "in", "is", "it", "like", "love", "my", "of", "on", "or", "so", "that", "the", "to", "with", "you", "i" )

html.pat <- paste0( "(", paste(html, collapse = "|"), ")" )
stop.words.pat <- paste0( "\\b(", paste(stop.words, collapse = "|"), ")\\b" )

essays <- str_replace_all(essays, html.pat, " ")
essays <- str_replace_all(essays, stop.words.pat, " ")

rm(html, stop.words, html.pat, stop.words.pat)
gc()

all.tokens <- tokens(essays, what = "word",
                     remove_numbers = TRUE, remove_punct = TRUE,
                     remove_symbols = TRUE, remove_hyphens = TRUE)
all.tokens <- tokens_tolower(all.tokens)
all.tokens <- tokens_select(all.tokens, stopwords(), selection = "remove")


# ---------- Stemming ----------#
all.tokens <- tokens_wordstem(all.tokens, language = "english")
all.tokens <- tokens_select(all.tokens, "^[a-z]$", selection = "remove", valuetype = "regex")


# ---------- DFM ----------#
all.tokens.dfm <- dfm(all.tokens, tolower = FALSE)
rm(essays, all.tokens)

dfm.trimmed <- dfm_trim(all.tokens.dfm, min_docfreq = 0.025 * N, min_termfreq = 0.025 * N, verbose = TRUE)
rm(all.tokens.dfm)
gc()

# ---------- TF - IDF ----------#

tokens.matrix <- convert(dfm.trimmed, to = "matrix")

tf <- prop.table(tokens.matrix, margin = 1)

idf <- apply(tokens.matrix, MARGIN = 2, function(x) log10(dim(tokens.matrix)[1] / sum(x != 0)))

tf.idf <- tf * idf
tf.idf <- na.omit(tf.idf)


# quanteda.tf <- dfm_weight(dfm.trimmed[1:n_max, 1:n_max], "prop")
# quanteda.idf <- docfreq(dfm.trimmed[1:n_max, 1:n_max], "inverse")
# 
# quanteda.df.idf <- quanteda.tf * quanteda.idf
# all.equal(tf.idf, convert(quanteda.df.idf, to = "matrix"))
# 
# 
# # quanteda implementation of tf idf (because mine not run on all data)
# all.tokens.tfidf <- dfm_weight(dfm.trimmed, "prop") * docfreq(dfm.trimmed, "inverse")

rm(tokens.matrix, tf, idf)
gc()

# ---------- male vs female by text ----------#

# df, names
all.tokens.df <- as.data.frame(dfm.trimmed, row.names = NULL, optional = FALSE, make.names = TRUE)
names(all.tokens.df) <- make.names(names(all.tokens.df), unique = TRUE) 

rm(dfm.trimmed)
gc()

# cross validation
cross_validation <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

# male, female labels
all.tokens.df <- cbind(sex = profiles_sex, all.tokens.df)
rm(profiles_sex)

# decision tree
trainmodel <- train(all.tokens.df[,-1], all.tokens.df[,1], trControl = cross_validation, method = "rpart")

# confusion matrix
confusionMatrix(trainmodel) #rpart

# ---------- Remove batch effect ----------#
words_identified_by_gender <- list()
for (word in names(trainmodel$finalModel$variable.importance)) {
  words_identified_by_gender <- c(words_identified_by_gender, substring(word, 6)) # remove 'Data.'
}

all.tokens.dfm <- select(all.tokens.df, -words_identified_by_gender)

# ---------- Clustering ----------#
#kmeans(tf.idf, k)

# plot T-SNE / PCA
# Calculate PCA
pca <- prcomp(tf.idf)
pca.data <- data.frame(Sample = rownames(pca$x), X = pca$x[,1], Y = pca$x[,2])
pca.var <- pca$sdev ^ 2
pca.var.per <- round(pca.var / sum(pca.var) * 100, 1)

colors <- paste0("cluster #", 1:10)

for (k in c(2, 3, 4, 10)) {
  
  clusters <- kmeans(tf.idf, k)
  
  print(ggplot(data = pca.data, aes(x = X, y = Y, label = Sample ,colour = colors[clusters$cluster])) +
          geom_point() + theme_bw() + ggtitle(paste0("Kmeans: K = ", k)) +
          theme(legend.position = "bottom", panel.background = element_rect(fill = "grey")) +
          xlab(paste0("PC1 (", pca.var.per[1], "%)")) + ylab(paste0("PC2 (", pca.var.per[2], "%)")))
}









