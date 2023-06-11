library(dplyr)
library(stringr)
library(quanteda)
library(Matrix)
library(caret)
library(rpart)
library(ggplot2)

# library(mosaic)
# library(xtable)
# library(gridExtra)
# library(stopwords)

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

train.dfm <- tf * idf

rm(tokens.matrix, tf, idf)
gc()

# ---------- male vs female by text ----------#

# df, names
all.tokens.df <- as.data.frame(train.dfm, row.names = NULL, optional = FALSE, make.names = TRUE)

names(all.tokens.df) <- make.names(names(all.tokens.df), unique = TRUE) 

rm(dfm.trimmed)
gc()

# cross validation
cross_validation <- trainControl(method = "repeatedcv", number = 10, repeats = 3, verboseIter = TRUE)

# male, female labels
all.tokens.df <- cbind(sex = profiles_sex, data = all.tokens.df)
all.tokens.df <- na.omit(all.tokens.df)
rm(profiles_sex)

# decision tree
before <- Sys.time() # how to do for each repeat??????????
train.model <- train(sex ~ ., data = all.tokens.df, trControl = cross_validation, method = "rpart")
Sys.time() - before

# confusion matrix
confusionMatrix(train.model) #rpart


# ---------- Remove batch effect ----------#
male.df <- subset(all.tokens.df, sex == "m")
female.df <- subset(all.tokens.df, sex == "f")

male_term_freq <- colSums(male.df[, -1])
female_term_freq <- colSums(female.df[, -1])

num_most <- 25
male_most_freq <- sort(male_term_freq, decreasing = TRUE)[1:num_most]
female_most_freq <- sort(female_term_freq, decreasing = TRUE)[1:num_most]

male_most_freq
female_most_freq

train.dfm <- train.dfm[, !colnames(train.dfm) %in% male_most_freq]
train.dfm <- train.dfm[, !colnames(train.dfm) %in% female_most_freq]

train.dfm <- na.omit(train.dfm)

# ---------- Clustering ----------#

# Calculate PCA
cluster.pca <- prcomp(train.dfm)
pca.data_frame <- data.frame(Text = rownames(cluster.pca$x), x = cluster.pca$x[,1], y = cluster.pca$x[,2])
pca.var <- cluster.pca$sdev ^ 2
pca.var.per <- round(pca.var / sum(pca.var) * 100, 1)

colors <- paste0("cluster #", 1:10)

# plot PCA for each cluster result

pdf("Week5_datingNLP.pdf")

kmeans_plots <- function(k) {
  clusters <- kmeans(train.dfm, k)
  
  ggplot(
    data = pca.data_frame,
    aes(x = x, y = y, label = Text, color = clusters$cluster)) +
    geom_point() +
    ggtitle(paste0("Kmeans: K = ", k, " clusters")) +
    xlab(paste0("PC1 (", pca.var.per[1], "%)")) + 
    ylab(paste0("PC2 (", pca.var.per[2], "%)")
    )
}

kmeans_plots(2)
kmeans_plots(3)
kmeans_plots(4)
kmeans_plots(10)

# plot T-SNE
sign.pca <- pca.var.per[pca.var.per > 0.7]
tsne.df <- data.frame(pc = seq(1:length(sign.pca)), val = sign.pca)
ggplot(data = tsne.df, aes(pc, val)) + 
  geom_col()

save(file="Week5_datingNLP.rdata", train.model, train.dfm, cluster.pca)

dev.off()
