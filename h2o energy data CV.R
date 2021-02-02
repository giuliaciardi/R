#example of 10-fold cross validation and random forest in H2O

library(h2o)
h2o.init()
h2o.flow()


#This dataset is a regression
#we try to predict which house designs will be more energy efficient


data <- h2o.importFile("https://raw.githubusercontent.com/DarrenCook/h2o/bk/datasets/ENB2012_data.csv",
                       destination_frame = "energy")
#   X1: Relative Compactness
# . X2: Surface Area
# . X3: Wall Area
# . X4: Roof Area
# . X5: Overall Height
# . X6: Orientation
# . X7: Glazing area
# . X8: Glazing area distribution
# . Y1: Heating Load
# . Y2: Cooling Load





h2o.describe(data)

factorsList <- c("X6", "X8")
data[,factorsList] <- as.factor(data[,factorsList])

data <- h2o.assign(data, "energy")
#split
parts <- h2o.splitFrame(data, 0.8)
train <- parts[[1]]
test <- parts[[2]]
x <- c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8")
y <- "Y2" #Or "Y1"

###################CORRELATION and NORMALITY####################################

numericColumns <- setdiff(colnames(train),c("X6","X8")) #tolgo factors
d <- round(h2o.cor(train[,numericColumns]) ,2)
rownames(d) <- colnames(d)
d

par(mfrow = c(2 ,5))
dummy <- lapply(colnames(train), function(col){
  h <- h2o.hist(train[,col], breaks = 30, plot = FALSE)
  plot(h, main = col, xlab = "", ylim = NULL)
})


###########################Random Forest######################


m <- h2o.randomForest(x, y, train, nfolds = 10, model_id = "RF_defaults")


summary(m)