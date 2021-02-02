# questo codice è preso dal tutorial: https://github.com/h2oai/h2o-3/blob/master/h2o-docs/src/product/tutorials/gbm/gbmTuning.Rmd
# spiega come usare h2o e come creare un modello tramite metodo Gradient Boosted Machine


########### CREA AMBIENTE DI CALCOLO ##########

# richiama la libreria h2o
# è una libreria scritt ain Java, quindi serve avere installata JRE e JDK (download for free dal sito Oracle)
library(h2o)

# questo avvia il cluster; ha tanti parametri. Le risorse di calcolo del cluster possono essere settate a piacimento
h2o.init(nthreads=-1)




########### PREPARA I DATI ###########

# sclego di usare il famoso dataset iris
head(iris)

# qui si trasfroma il semplice data frame iris in un oggetto h2o
df<-as.h2o(iris)
str(df)

## pick a response for the supervised problem
# gli si dice qual'è la colonna della risposta che si vuole classificare
response <- "Species"

## use all other columns (except for the name) as predictors
# questo banalmente gli dice di usare tutte le altre colonne tranne la risposta come predittori. 
# Si può tranquillamente fare diversamente in R
predictors <- setdiff(names(df), c(response, "name")) 

# qui crea training set, test set e volendo anche validation set. Si coccupa lui automaticamente di fare un sampling randomico
# questo comando fa 80% in train e 20% in test (che poi uso anche come validation) 
splits <- h2o.splitFrame(
  data = df, 
  ratios = c(0.8),   
  destination_frames = c("train.hex", "test.hex")
)

str(splits)

# associo dei nomi comodi da usare agli elementi che ho creato in splits (che è una lista) 
train <- splits[[1]]
test  <- splits[[2]]



########### CREA E USA IL MODELLO GBM ###########

gbm <- h2o.gbm(x = predictors, y = response, training_frame = train, validation_frame = test)
gbm
plot(gbm)

preds<-h2o.predict(gbm,test)
as.data.frame(preds)




########### CREA E USA IL MODELLO DEEPLEARNING ###########

dl<-h2o.deeplearning(x=predictors, y=response,training_frame = train, validation_frame = test)
dl
plot(dl)

preds<-h2o.predict(dl,test)
as.data.frame(preds)



########## AUTO MACHINE LEARNING #########

aml <- h2o.automl(x = predictors, y = response, training_frame = train, leaderboard_frame = test, max_runtime_secs = 30)

# vedi i risultati dei vari algoritmi
as.data.frame(aml@leaderboard)

# vedi il modello migliore
aml@leader

# usa il modello migliore
pred <- h2o.predict(aml@leader, test)




########### SALVA IL MODELLO E LA PREDIZIONE GBM (UGUALE PER DL) ###########

# linee di codice esempio qualora si volesse salvare il modello
h2o.saveModel(gbm, "/tmp/bestModel.csv", force=TRUE)
h2o.exportFile(preds, "/tmp/bestPreds.csv", force=TRUE)




