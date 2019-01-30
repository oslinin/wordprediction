require(tm)
require(quanteda)
require(ngram)
require(data.table)
require(mvbutils)


# corp <- VCorpus(VectorSource("I eat pie"))
# corp[[1]][[1]]
# inspect(corp)
# meta(corp[[1]])
# dtm <- DocumentTermMatrix(corp)
# inspect(dtm)
# findFreqTerms(dtm)


#classes: data (e.g. dummy, corpus); model (ngram, etc.)
text.data <- function(type="dummy",...){
  me <- list()
  class(me) <- type
  class(me) <- append(class(me),"text.model")
  me <- read.data(me,...)
  me 
}

read.data <- function(x,...) UseMethod("read.data", x)
read.data.dummy <- function(obj){
  dumb.text=c("I eat pie daily.", "I eat dog.")
  #obj$data <- quanteda::corpus(tm::VCorpus(VectorSource("I eat pie daily.")))
  #obj$data <- tm::VCorpus(VectorSource(dumb.text))
  obj$data <- dumb.text
  obj
}
read.data.coursera <- function(data.obj2, nrow=-1L, train=c(train=.6, test=.4)){
  dir1 <- "/home/oleg/Downloads/final/en_US"
  #dir(dir1)
  # x <- PCorpus(DirSource(dir1, encoding = "UTF-8"),
  #                readerControl = list(language = "eng"), 
  #                dbControl=list(dbName="eng.db"))
  # files1 <-  "en_US.blogs.txt" 
  # x2 <- readLines(x[[files1]])
  dir(dir1)
  x <- readLines(file.path(dir1, "en_US.blogs.txt"), n=nrow)
  data.obj2$data <- x
  length(x)
  data.obj2$train <- sample(1:length(x), train['train']*length(x))
  data.obj2$train <- data.obj2$train[order(data.obj2$train)]
  data.obj2$test <- 1:length(x) %except% data.obj2$train
  data.obj2
}



ngram.model <- function(n, backoff=F){
  lang.model=list(n=n, backoff=backoff)
  class(lang.model)="ngram"
  lang.model
}

train <- function(model.bigram,data.obj) UseMethod("train", model.bigram)

train.ngram <- function(model.bigram, data.obj){
  if (model.bigram$backoff){
    m3 <- model.bigram; m3$backoff <- F;      # m3$n <- m3$n - 1; 
    for (i in 0 : (model.bigram$n - 1)){
      n = model.bigram$n-i; m3$n=n 
      m3 <- train(m3, data.obj)
    }
    model.bigram$ngram.tables <- m3$ngram.tables
    return(model.bigram)
  }
  if (!is.null(  model.bigram$ngram.tables[[as.character(model.bigram$n)]]  ))  return(model.bigram)
  #lang.model$dfm <- quanteda::dfm(dat, tolower=T)
  #quanteda::tokens_ngrams(tokens(dummy.data$data,remove_punct=T), n=2)
  x <- data.obj$data
  if (!is.null(data.obj$train)) {
    stopifnot(length(data.obj$data)==length(data.obj$train)+length(data.obj$test))
    print("using "%&% length(data.obj$train)%&%" lines to train")
    x = data.obj$data[data.obj$train]
  }
  if (is.list(x)) x <-concatenate(lapply(x,"[[", 1))
  x <- concatenate(x, collapse="\n")
  x  <- preprocess(x, remove.punct = T, case="lower")
  x = gsub("\n", " . ", x)
  x = ". " %&% x %&% " ."
  x2 <- ngram(x, n=model.bigram$n)
  x4 <- as.data.table(get.phrasetable(x2))
  x3 <- do.call(rbind, strsplit(x4$ngrams, " "))
  colnames(x3) <- "W" %&% c(1:ncol(x3))
  x3 <- as.data.table(data.frame(x3, x4[,.(freq, prop)]))
  if (is.null(model.bigram$ngram.tables)) model.bigram$ngram.tables=list()
  model.bigram$ngram.tables[[as.character(model.bigram$n)]] <- x3
  model.bigram 
}

predict.word <- function(trained, word) UseMethod("predict.word", trained)
predict.word.ngram <- function(model.bigram, word = "I eat"){
  word1 = strsplit(tolower(word), " ")[[1]]
  if (length(word1)==0) return(NULL)
  w = tail(word1,model.bigram$n-1)
  if (model.bigram$backoff) return(predict.backoff(model.bigram, word))
  if (model.bigram$n==1)  res=model.bigram$ngram.tables[["1"]]
  if (model.bigram$n==2)  res=model.bigram$ngram.tables[["2"]][W1==w]
  if (model.bigram$n==3)  res=model.bigram$ngram.tables[["3"]][W1==w[1] & W2==w[2]]
  head(get.prob(res),10)
}
get.prob <- function(prediction){
  prediction<-prediction[order(-freq)]
  prediction[,P:=freq/.N]
  prediction
}
predict.backoff <- function(model.bigram, word){
  m3 <- model.bigram; m3$backoff = F
  p1 <- predict.word.ngram(m3, word)
  while (nrow(p1)==0 & m3$n>0){
    m3$n = m3$n -1
    p1 <- predict.word.ngram(m3, word)
  }
  return(p1)
}


save.models <- function(models){
  models$model.backoff$ngram.tables[["3"]] <- NULL
  models$model.backoff$ngram.tables[["2"]] <- NULL
  saveRDS(models, file= "/home/oleg/Downloads/final/gui/shiny.R/models.RData")
}

load.models <- function(){
  models <- readRDS("models.RData")
  models$model.backoff$ngram.tables[["3"]] <- models$model.trigram$ngram.tables[["3"]]
  models$model.backoff$ngram.tables[["2"]] <- models$model.bigram$ngram.tables[["2"]]
  models
}