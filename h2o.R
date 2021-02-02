#Intro h2o and example of deep learning on iris



# The following two commands remove any previously installed H2O packages for R.
if ("package:h2o" %in% search()) { detach("package:h2o", unload=TRUE) }
if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }

# Next, we download packages that H2O depends on.
pkgs <- c("RCurl","jsonlite")
for (pkg in pkgs) {
  if (! (pkg %in% rownames(installed.packages()))) { install.packages(pkg) }
}

# Now we download, install and initialize the H2O package for R.
install.packages("h2o", type="source", repos="http://h2o-release.s3.amazonaws.com/h2o/rel-wolpert/4/R")

# Finally, let's load H2O and start up an H2O cluster
library(h2o)
h2o.init()

#open h2o flow in the browser
h2o.flow()

#remove an H2O frame directly
#h2o.rm()

#import file from directory
#data <- h2o.import_file("/path/to/my.csv")
#import a list of files senza dover fare cbind
#df <-  h2o.import_file(["/path/to/my1.csv", "/path/to/my2.csv"])

###############################DATASET###############################################

#NB all the data is on the cluster (the server), not on our client
#datasets <-  "https://raw.githubusercontent.com/DarrenCook/h2o/bk/datasets/"
#data <- h2o.importFile(paste0(datasets,"iris_wheader.csv"))

data <- h2o.importFile("https://raw.githubusercontent.com/DarrenCook/h2o/bk/datasets/iris_wheader.csv",
                       destination_frame = "iris")

#it takes an R data.frame, turns it into an H2O frame
data <- as.h2o(data)
#viceversa rarely need
as.data.frame(data)
as.matrix(data)





#in caso di modifiche al dataset fatte in R su un oggetto di tipo H2oframe,
#si riassegna l'ogg modificato allo stesso frame su cui si sta lavorando in flow
data <- h2o.assign(data, "iris")

#joining two frames together, based on columns they have in common
#no real numbers! only integers and enums
h2o.merge()

#example
prices <- as.h2o( data.frame(list(
  petal_len = 2:5, price = c(4, 5.5, 8, 10)
) ) )
data$petal_len <- round(data$petal_len) #to integer
irisPrices <- h2o.merge(data, prices)

#################################EXPORT####################################
#export files from the cluster
h2o.exportFile(frame_name, "/path/to/d.csv")
#in flow there are buttons for Download or Export

#export model
#Model ID is the filename and it cannot be changed
fname <- h2o.saveModel(model, "/tmp/h2o_models/tes", T) #true for overwrite

#re-load saved model
h2o.loadModel(fname)


##################################BASIC##########################################
#descriptive
h2o.describe(data)

#basic stat
h2o.sd(data[,2])
h2o.cor(data)
h2o.mean( data[,c("sepal_wid", "petal_wid")] )

#aggregating rows by group and having functions
h2o.group_by(data, by = "class",
             nrow("class"),
             mean("petal_len"))

#graph
h2o.hist(data$petal_len)


#assign short variables names
y <- "class"
x <- setdiff(names(data), y)





##################################SPLIT############################################
#2 parts
parts <- h2o.splitFrame(data, 0.8)
train <- parts[[1]]
test <- parts[[2]]

#3 parts
parts <- h2o.splitFrame(data, c(0.6, 0.2))
train <- parts[[1]]
valid <- parts[[2]] 
test <- parts[[3]]

#altrimenti
ratios <- c(0.6, 0.2, 0.2)
sz <- nrow(data)
indices <- split(1:sz, sample( rep(1:3, sz * ratios) ) ) #tolgo sample se non voglio split casuale (time series)
train <- data[ indices[[1]], ]
valid <- data[ indices[[2]], ]
test <- data[ indices[[3]], ]


h2o.describe(train)
h2o.describe(valid)

##########################TRAINING MODEL#############################################

m <- h2o.deeplearning(x, y, train)
#see stats of the model
h2o.mse(m)
h2o.confusionMatrix(m)



#########################TESTING MODEL##############################################
#test model
p <- h2o.predict(m, test)
#see prediction and confidence
#ad.data.frame esegue il downld in R del dataset dal cluster h2o
as.data.frame(p)

#explore the difference between real class and predicted class
#this command loads in the cluster 2 combined columns
as.data.frame( h2o.cbind(p$predict, test$class) )
#cbind( as.data.frame(p$predict), as.data.frame(test$class) )

#a model performance summary 
h2o.performance(m, test)


