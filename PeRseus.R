## setwd, modify according to your needs

setwd("~/OneDrive/GithubProjects/PeRseus")

## libraries needed

library(tm)
library(XML)
library(RCurl)
library(plyr)
library(lda)
library(LDAvis)
library(compiler)

## User settings:
K <- 12
G <- 5000
alpha <- 0.02
eta <- 0.02
seed <- 37
terms_shown <- 40
swLatin <- TRUE
swEnglish <- FALSE
swGreek <- FALSE
swAdditional <- TRUE
language <- "Latin" # (Greek, Persian, Arabic, Latin)
requestURN <- "urn:cts:latinLit:phi1056.phi001" 
capabilities_URL <- "http://cts.perseids.org/api/cts?request=GetCapabilities"
baseURL <- "http://cts.perseids.org/api/cts?request=GetPassage&urn="
reffURL <- "http://cts.perseids.org/api/cts?request=GetValidReff&urn="
morpheusURL <- "https://services.perseids.org/bsp/morphologyservice/analysis/word?word="
searchterms <- ""

## read in some stopwords:

stopwords_english <- stopwords("SMART")
stopwords_latin <- c("ab", "ac", "ad", "adhic", "aliqui", "aliquis", "an", "ante", "apud", "at", "atque", "aut", "autem", "cum", "cur", "de", "deinde", "dum", "ego", "enim", "ergo", "es", "est", "et", "etiam", "etsi", "ex", "fio", "haud", "hic", "iam", "idem", "igitur", "ille", "in", "infra", "inter", "interim", "ipse", "is", "ita", "magis", "modo", "mox", "nam", "ne", "nec", "necque", "neque", "nisi", "non", "nos", "o", "ob", "per", "possum", "post", "pro", "quae", "quam", "quare", "qui", "quia", "quicumque", "quidem", "quilibet", "quis", "quisnam", "quisquam", "quisque", "quisquis", "quo", "quoniam", "sed", "si", "sic", "sive", "sub", "sui", "sum", "super", "suus", "tam", "tamen", "trans", "tu", "tum", "ubi", "uel", "uero", "ut", "t", "cos2", "coepio", "sum", "edo")
stopwords_greek <- c("μή", "ἑαυτοῦ", "ἄν", "ἀλλ’", "ἀλλά", "ἄλλοσ", "ἀπό", "ἄρα", "αὐτόσ", "δ’", "δέ", "δή", "διά", "δαί", "δαίσ", "ἔτι", "ἐγώ", "ἐκ", "ἐμόσ", "ἐν", "ἐπί", "εἰ", "εἰμί", "εἴμι", "εἰσ", "γάρ", "γε", "γα^", "ἡ", "ἤ", "καί", "κατά", "μέν", "μετά", "μή", "ὁ", "ὅδε", "ὅσ", "ὅστισ", "ὅτι", "οὕτωσ", "οὗτοσ", "οὔτε", "οὖν", "οὐδείσ", "οἱ", "οὐ", "οὐδέ", "οὐκ", "περί", "πρόσ", "σύ", "σύν", "τά", "τε", "τήν", "τῆσ", "τῇ", "τι", "τί", "τισ", "τίσ", "τό", "τοί", "τοιοῦτοσ", "τόν", "τούσ", "τοῦ", "τῶν", "τῷ", "ὑμόσ", "ὑπέρ", "ὑπό", "ὡσ", "ὦ", "ὥστε", "ἐάν", "παρά", "σόσ")
# stopwords_arabic and stopwords_persian are currently based on frequency only. I welcome pointers to stopword lists for Classical Arabic and Persian

## Decide which set of stopwords

stop_words <- stopwords_latin

# Enable JIT-compiling

enableJIT(3)

### Functions:

## Take the terms from a word list and put them into a format needed by the LDA package

get.terms <- function(x) {
  index <- match(x, vocab)
  index <- index[!is.na(index)]
  rbind(as.integer(index - 1), as.integer(rep(1, length(index))))}

## Replace word-token with lemmata-vector

lemmatiser <- function(x){
  lemmatised <- stem_dictionary[[x]]
  return(lemmatised)}

## Choose lemma from each lemmata-vector based on frequency of that lemma in the research corpus

choose_lemma <- function(x){
  lemma <- names(which(NumberOccurrences[x]==max(NumberOccurrences[x])))
  if (length(lemma)==1) {return(lemma)
  }
  else {return (x[1])}
}

### parsing the XML in R is a bit of a pain. I am happy for suggestions to make this more efficient!

XMLminer <- function(x){
  xname <- xmlName(x)
  xattrs <- xmlAttrs(x)
  c(sapply(xmlChildren(x), xmlValue), name = xname, xattrs)}

XMLpassage1 <-function(xdata){
  result <- xmlParse(xdata)
  as.data.frame(t(xpathSApply(result, "//*/tei:body", XMLminer)), stringsAsFactors = FALSE)}

XMLpassage2 <-function(xdata){
  result <- xmlParse(xdata)
  temp.df <- as.data.frame(t(xpathSApply(result, "//*/hdwd", XMLminer)), stringsAsFactors = FALSE)
  as.vector(temp.df[['text']])}

### parsing function: Uses Perseids morphology API to retrive vector of lemmata
### two drawbacks: 1. internet problems would not break code (not anymore), but lead to no lemma returned for requested.
### Uses US server in Boston. Quick in Boston very slow from Europe
### Possible solutions: Requesting already parsed data for edition, thus reducing the API requests from n=number of forms in a corpus to n=1.

parsing <- function(x){
  word_form <- x
  URL <- paste(morpheusURL, word_form, "&lang=lat&engine=morpheuslat", sep = "")
  message(round((match(word_form, corpus_words)-1)/length(corpus_words)*100, digits=2), "% processed. Checking ", x," now.")
  
  URLcontent <- tryCatch({
    getURLContent(URL)}, 
    error = function(err)
    {tryCatch({
      Sys.sleep(0.1)
      message("Try once more")
      getURLContent(URL)},
      error = function(err)
      {message("Return original value: ", word_form)
       return(word_form)
      })
    })
  if (URLcontent == "ServerError") {
    lemma <- x
    message(x, " is ", lemma)
    return(lemma)}
  else {
    lemma <- if (is.null(XMLpassage2(URLcontent)) == TRUE) {
      lemma <- x
      message(x, " is ", lemma)
      return(lemma)}
    else {lemma <- tryCatch({XMLpassage2(URLcontent)},
                            error = function(err) {
                              message(x, " not found. Return original value.")
                              lemma <- "NotFound1"
                              message(x, " is ", lemma)
                              return(lemma)})
          
          lemma <- gsub("[0-9]", "", lemma)
          lemma <- tolower(lemma)
          lemma <- unique(lemma)
          if (nchar(lemma) == 0) {
            lemma <- x
            message(x, " is ", lemma)
            return(lemma)}
          else {
            message(x, " is ", lemma)
            return(lemma)
          }
    }
  }
}

### quick helper functions for vector splitting

first_element <- function(x){
  first_element <- head(x, n=1)
  return(first_element)}

last_element <- function(x){
  last_element <- tail(x, n=1)
  return(last_element)}

### find out how topic similarity of citable units
### comparing the mean deviation of theta-values for each topic

is_similar <- function(x) {
  check <- all.equal(theta.frame[which(theta.frame[,1] == first_element(unlist(x))),], theta.frame[which(theta.frame[,1] == last_element(unlist(x))),]) # comparing with all.equal
  result <- mean(as.numeric(sub(".*?difference: (.*?)", "\\1", check)[3:length(check)])) 
  return(result)
} # produces NA if compared with itself

### building test matrix to compare a sentence with all other sentences in the corpus

build_test <- function(x){
  test_cases <- as.character(AblAbsCaesar[,1]) [! as.character(AblAbsCaesar[,1]) %in% x]
  first_column <- rep(x, length(test_cases))
  test_matrix <- matrix(nrow=length(test_cases), ncol = 2)
  test_matrix[,1] <- first_column
  test_matrix[,2] <- test_cases
  return(test_matrix)
}

### Get Capabilities of API

URLcontent <- getURLContent(capabilities_URL, .opts=curlOptions(followlocation=TRUE))

### Import corpus from CTS repository

## Fetch Reffs for CTS Repository

t1 <- Sys.time()
message("Retrieve Reffs for ", requestURN)
URL <- paste(reffURL, requestURN, sep = "")
URLcontent <- getURLContent(URL)
reffs <- unlist(strsplit(URLcontent, split="<urn>|</urn>"))
reffs <- reffs[2:length(reffs)]
reffs <- reffs[seq(1, length(reffs), 2)]
t2 <- Sys.time()
time_ref <- t2 - t1
urls <- paste(baseURL, reffs, sep = "")

## Fetch Text from CTS Repository

### All of it
t1 <- Sys.time()
XMLcorpus <- getURIAsynchronous(urls)
while(length(which(XMLcorpus == "")) > 0) {
  print(paste("Fetch rest: ", as.character(length(which(XMLcorpus == ""))), " CTS Units", sep ="")); 
  XMLcorpus[which(XMLcorpus == "")] <- getURIAsynchronous(urls[which(XMLcorpus == "")])}
t2 <- Sys.time()
Time_fetchingAll <- t2 - t1
Time_fetchingAll

### Batches
t1 <- Sys.time()
batch_urls <- split(urls, ceiling(seq_along(urls)/100))
output_list <- list()
for (i in 1:length(batch_urls)) {
  temp_vector <- getURIAsynchronous(batch_urls[[i]])
  while(length(which(temp_vector == "")) > 0) {
    print(paste("Fetch rest of batch-request ", as.character(i), "/", as.character(length(batch_urls)), sep="")); 
    temp_vector[which(temp_vector == "")] <- getURIAsynchronous(batch_urls[[i]][which(temp_vector == "")])}
  output_list[[i]] <- getURIAsynchronous(batch_urls[[i]])
  print(paste("Fetched ", as.character(i), "/", as.character(length(batch_urls)), sep=""))
  }
XMLcorpus2 <- unlist(output_list)
t2 <- Sys.time()
Time_fetchingBatches <- t2 - t1
Time_fetchingBatches

#########################################################

output_list <- list()
error_log <- list()
for (i in reffs) {
  message("Retrieve section ", i)
  URL <- paste(baseURL, i, sep = "")
  message("Fetching ", URL)
  URLcontent <- tryCatch({getURLContent(URL)},
                         error = function(err)
                         {result <- getURLContent(URL)
                          return(result)}
  )
  # Parse the XML and extract needed information. 
  output_list[[i]] <- tryCatch({
    XMLpassage1(URLcontent)},
    error = function(err)
    {message(i, " -retrieval failed. Put in log.")
     error_log[[i]] <- i
     return("fehler")}
  )
  message("---------------------------------------")}
t2 <- Sys.time()
Time_fetchingBatches <- t2 - t1

## Build corpus

corpus <- do.call("rbind",output_list) #combine all vectors into a matrix
corpus <- unique(corpus) # returns the unique rows of catalogue.
output_names <- rownames(corpus)

temp.corpus <- matrix(nrow=length(corpus[,1]), ncol = length(corpus[1,]))
temp.corpus[, 1] <- output_names
temp.corpus[, 2] <- unname(corpus[,1])
colnames(temp.corpus) <- c("identifier", "text")
corpus <- temp.corpus

### Perseus-text sometimes have inaccuracies in their punction. The next few lines address this. 

corpus[,2] <- gsub(".", ". ", corpus[,2], fixed=TRUE)
corpus[,2] <- gsub(",", ", ", corpus[,2], fixed=TRUE)
corpus[,2] <- gsub(":", ": ", corpus[,2], fixed=TRUE)
corpus[,2] <- gsub(";", "; ", corpus[,2], fixed=TRUE)
corpus[,2] <- gsub("’", "’ ", corpus[,2], fixed=TRUE)
corpus[,2] <- gsub("†", "† ", corpus[,2], fixed=TRUE)
corpus[,2] <- gsub("]", "] ", corpus[,2], fixed=TRUE)
corpus[,2] <- gsub("[", " [", corpus[,2], fixed=TRUE)
corpus[,2] <- gsub(")", ") ", corpus[,2], fixed=TRUE)
corpus[,2] <- gsub("(", " (", corpus[,2], fixed=TRUE)
while(length(grep("   ", corpus[,2], fixed=TRUE))>0) {
  corpus[,2] <- gsub("  ", " ", corpus[,2], fixed=TRUE)
} ## repeat until none in corpus
while(length(grep("  ", corpus[,2], fixed=TRUE))>0) {
  corpus[,2] <- gsub("  ", " ", corpus[,2], fixed=TRUE)
} ## repeat until none in corpus
corpus[,2] <- gsub(" ,", ",", corpus[,2], fixed=TRUE) 
corpus[,2] <- gsub(" .", ".", corpus[,2], fixed=TRUE) 
corpus[,2] <- gsub(" :", ":", corpus[,2], fixed=TRUE)
corpus[,2] <- gsub(" ;", ";", corpus[,2], fixed=TRUE)
corpus[,2] <- gsub(" ;", ";", corpus[,2], fixed=TRUE)
corpus[,2] <- gsub(" ’", "’", corpus[,2], fixed=TRUE)
corpus[,2] <- gsub(" ]", "]", corpus[,2], fixed=TRUE)
corpus[,2] <- gsub(" )", ")", corpus[,2], fixed=TRUE)
corpus[,2] <- gsub("( ", "(", corpus[,2], fixed=TRUE)
corpus[,2] <- trimws(corpus[,2])

authorwork <- tail(unlist(strsplit(requestURN, ":", fixed=TRUE)), n=1)
author <- unlist(strsplit(authorwork, ".", fixed=TRUE))[1]
work <- unlist(strsplit(authorwork, ".", fixed=TRUE))[2]
filename <- paste(work, ".rds", sep="")
filename_csv <- paste(work, ".csv", sep="")
filename_parsed <- paste(work, "_parsed.rds", sep="")
filename_stem  <- paste(work, "_stems.rds", sep="")
filename_parsed_csv <- paste(work, "_parsed.csv", sep="")
filename_stem_csv  <- paste(work, "_stems.csv", sep="")
foldername <- author
dir.create(foldername)
pathname_rds <- paste(foldername, "/", filename, sep="")
saveRDS(corpus, file = pathname_rds)
pathname_csv <- paste(foldername, "/", filename_csv, sep="")
write.csv(corpus, file = pathname_csv)

## Build base for topic modelling

research_corpus <- corpus[,2]
research_corpus <- factor(research_corpus)

### pre-processing:

research_corpus <- tolower(research_corpus)  # force to lowercase
research_corpus <- gsub("'", " ", research_corpus)  # remove apostrophes
research_corpus <- gsub("-", "", research_corpus)  # remove hyphens, create composita
# research_corpus <- gsub("v", "u", research_corpus) # normalise to 'u'
# research_corpus <- gsub("j", "i", research_corpus) # normalise to 'i'

research_corpus <- gsub("[[:punct:]]", " ", research_corpus)  # replace punctuation with space
research_corpus <- gsub("[[:cntrl:]]", " ", research_corpus)  # replace control characters with space
research_corpus <- gsub("^[[:space:]]+", "", research_corpus) # remove whitespace at beginning of documents
research_corpus <- gsub("[[:space:]]+$", "", research_corpus) # remove whitespace at end of documents
research_corpus <- gsub("[0-9]", "", research_corpus) #remove numbers

## produce dictionary for stemming:

t1 <- Sys.time()

## tokenize on space and output as a list:
doc.list <- strsplit(research_corpus, "[[:space:]]+")
corpus_words <- unique(unlist(doc.list))
corpus_words <- sort(corpus_words)

## stemming

stem_dictionary <- sapply(corpus_words, parsing)

NumberOfForms <- max(unique(sapply(stem_dictionary, length)))
number_lemmata <- sapply(stem_dictionary, length)

t2 <- Sys.time()
time_stemming <- t2 - t1

## correcting
t1 <- Sys.time()

temp <- strsplit(research_corpus, " ")
temp_correct <- list()
for (i in 1:length(temp)) {
  temp_correct[[i]] <- sapply(temp[[i]], lemmatiser) 
}
NumberOccurrences <- table(unlist(temp_correct))

corrected_corpus <- list()
for (n in 1:length(temp_correct)) {
  temp_corrected <- list()
  counter <- n
  for (i in 1:length(temp_correct[[counter]])) {
    temp_corrected[[i]] <- choose_lemma(temp_correct[[counter]][[i]])  
  }  
  corrected_corpus[[n]] <- temp_corrected
}

for (i in 1:length(corrected_corpus)) {
  corrected_corpus[[i]] <- paste(unlist(corrected_corpus[[i]]), collapse=" ")
}

research_corpus <- unlist(corrected_corpus)

# Save corrected corpus to disk

temp.corpus <- matrix(nrow=length(research_corpus), ncol = 2)
temp.corpus[, 1] <- output_names
temp.corpus[, 2] <- research_corpus
colnames(temp.corpus) <- c("identifier", "text")
corpus_parsed <- temp.corpus

pathname_rds <- paste(foldername, "/", filename_parsed, sep="")
saveRDS(corpus_parsed, file = pathname_rds)
pathname_csv <- paste(foldername, "/", filename_parsed_csv, sep="")
write.csv(corpus_parsed, file = pathname_csv)

pathname_rds <- paste(foldername, "/", filename_stem, sep="")
saveRDS(stem_dictionary, file = pathname_rds)

## Produce CSV Stem-Dictionary

stem_dictionary_CSV <- vapply(stem_dictionary, 
                              function(x){result <- paste(x, collapse = ";")
                              return(result)
                              },
                              character(1))
pathname_csv <- paste(foldername, "/", filename_stem_csv, sep="")
write.csv(stem_dictionary_CSV, file = pathname_csv)

### Compare length of corpus and corpus_parsed

length_corpus <- length(output_names)
test_corpus_length <- vector()
for (i in 1:length_corpus){
  test_corpus_length[i] <- length(unlist(strsplit(as.character(unlist(corpus_parsed[i,2])), "[[:space:]]+")[1])) == length(unlist(strsplit(as.character(unlist(corpus[i,2])), "[[:space:]]+")[1]))
}
table_corpus_length <- table(test_corpus_length)
bug_report <- which(test_corpus_length == FALSE)
failure <- corpus[bug_report,]
control <- corpus_parsed[bug_report,]

filename_bug  <- paste(work, "_iobugs_", as.character(round(100*length(failure[,1])/length(corpus[,1]))),sep="")
pathname_csv <- paste(foldername, "/", filename_bug, ".csv", sep="")
pathname_rds <- paste(foldername, "/", filename_bug, ".rds", sep="")
saveRDS(failure, file = pathname_rds)
write.csv(failure, file = pathname_csv)

# Split to word level

doc.list <- strsplit(research_corpus, "[[:space:]]+")
t2 <- Sys.time()
time_correcting <- t2 - t1

### Prepare Topic-modelling 

## compute the table of terms:
term.table <- table(unlist(doc.list))
term.table <- sort(term.table, decreasing = TRUE)

## compute additional stop_words:
add_stop_words <- as.data.frame(term.table)
add_stop_words <- row.names(as.data.frame(add_stop_words[1:10,]))

stop_words <- c(stop_words, add_stop_words)
stop_words <- unique(stop_words)

## remove terms that are stop words or occur fewer than "occurenses" times:
occurences <- 3
del <- names(term.table) %in% stop_words | term.table < occurences
term.table <- term.table[!del]
vocab <- names(term.table)

## now put the documents into the format required by the lda package:

documents <- lapply(doc.list, get.terms)

## Compute some statistics related to the data set:
D <- length(documents)  # number of documents
W <- length(vocab)  # number of terms in the vocab
doc.length <- sapply(documents, function(x) sum(x[2, ]))  # number of tokens per document
N <- sum(doc.length)  # total number of tokens in the data
term.frequency <- as.integer(term.table)  # frequencies of terms in the corpus

## Fit the model:
set.seed(seed)
t1 <- Sys.time()
fit <- lda.collapsed.gibbs.sampler(documents = documents, K = K, vocab = vocab, 
                                   num.iterations = G, alpha = alpha, 
                                   eta = eta, initial = NULL, burnin = 0,
                                   compute.log.likelihood = TRUE)
t2 <- Sys.time()
modelling_time <- t2 - t1

## Visualize
tm_dir <- paste(foldername, "/", work, "_tm_",  as.character(K), "_", as.character(G), "_", as.character(seed), sep="")
tmtables_dir <-paste(tm_dir, "/tables", sep="")
tmviz_dir<-paste(tm_dir, "/viz", sep="")
dir.create(tm_dir)
dir.create(tmtables_dir)
dir.create(tmviz_dir)

theta <- t(apply(fit$document_sums + alpha, 2, function(x) x/sum(x)))
phi <- t(apply(t(fit$topics) + eta, 2, function(x) x/sum(x)))

research_corpusAbstracts <- list(phi = phi,
                                 theta = theta,
                                 doc.length = doc.length,
                                 vocab = vocab,
                                 term.frequency = term.frequency)

## create the JSON object to feed the visualization:
json <- createJSON(phi = research_corpusAbstracts$phi, 
                   theta = research_corpusAbstracts$theta, 
                   doc.length = research_corpusAbstracts$doc.length, 
                   vocab = research_corpusAbstracts$vocab, 
                   term.frequency = research_corpusAbstracts$term.frequency,
                   R = terms_shown)

## Visualise and start browser, sava output

serVis(json, out.dir = tmviz_dir, open.browser = FALSE)

## reorder phi and theta according to JSON file

new.order <- RJSONIO::fromJSON(json)$topic.order 
phi <- phi[new.order,]
theta <- theta[,new.order]

## generate topicnames

phi.t <- t(phi)
topicnames <- vector(mode="character", length=K)
for (i in 1:K){
  topicnames[i] <- paste(rownames(head(phi.t[order(phi.t[,i],decreasing=TRUE),], n=7)), sep="", collapse="_")

}

## get term-topic matrix

pathname_csv <- paste(tmtables_dir, "/", "/phi.rds", sep="")
pathname_rds <- paste(tmtables_dir, "/", "/phi.csv", sep="")
rownames(phi) <- topicnames
saveRDS(phi, file = pathname_rds)
write.csv(phi, file = pathname_csv)

## get document-topic matrix

pathname_csv <- paste(tmtables_dir, "/", "/theta.rds", sep="")
pathname_rds <- paste(tmtables_dir, "/", "/theta.csv", sep="")
rownames(theta) <- output_names
colnames(theta) <- topicnames
saveRDS(theta, file = pathname_rds)
write.csv(theta, file = pathname_csv)

## calculate distance between topics and cluster based on it, save viz as PDF

distance <- dist(theta, method = "euclidean")
fit.distance <- hclust(distance, method = "ward.D")
pathname_pdf <- paste(tmtables_dir, "/", "/euclideancluster_theta.pdf", sep="")
pdf(pathname_pdf, width=200, height=15)
plot(fit.distance)
dev.off()

View(corpus[which(corpus[,1] %in% names(which(cutree(fit.distance, k = 500) == unname(cutree(fit.distance, k = 500)["urn:cts:latinLit:phi1056.phi001.perseus-lat1:7.5.7"])))),])
