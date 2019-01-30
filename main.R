setwd("/home/oleg/Downloads/final"); source("functions.R")

data.obj <- text.data("dummy")
data.obj2 <- text.data("coursera", n=100)

models <- list()
models$model.bigram <-  ngram.model(n=2)
models$model.trigram <- ngram.model(n=3)
models$model.backoff <- ngram.model(n=3, backoff=T)

models$model.bigram0 <- train(models$model.bigram, data.obj)
models$model.trigram0 <- train(models$model.trigram, data.obj)
models$model.backoff0 <- train(models$model.backoff, data.obj)


models$model.bigram <- train(models$model.bigram, data.obj2)
models$model.trigram <- train(models$model.trigram, data.obj2)
models$model.backoff$ngram.tables[["3"]] <- models$model.trigram$ngram.tables[["3"]]
models$model.backoff$ngram.tables[["2"]] <- models$model.bigram$ngram.tables[["2"]]
models$model.backoff <- train(models$model.backoff, data.obj2)

save.models(models)
models <- load.models()


#debugonce(predict.word)
#model.backoff$ngram.tables[[2]]
names(models)
predict.word(models$model.bigram, "I eat")
predict.word(models$model.trigram, "we eat")
predict.word(models$model.backoff, "we efat")
predict.word(models$model.backoff, "")

#setwd("/home/oleg/Downloads/final/gui/shiny.R")
#source("/home/oleg/Downloads/final/gui/shiny.R/app.R")