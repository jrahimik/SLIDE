## SCVI cross validation
set.seed(123)
k <- 5
replicates <- 50
cor_list <- vector("numeric", replicates)
Z <- read.table("/ix/djishnu/Javad/CDexpansion/scvi_factors.csv", sep = " ")
y <- read.table("/ix/djishnu/Javad/CDexpansion/data/expansion_y.csv", row.names = 1, sep = ",", header = T)
colnames(y) <- "y"
data <- data.frame(y=y,Z)


for (r in 1:replicates) {
  # shuffle Z and Y
  shuffled_indices <- sample(nrow(Z))
  data_shuffled <- data[shuffled_indices,]
  
  # perform k-fold cross-validation
  indices <- sample(1:k, nrow(Z_shuffled), replace = TRUE)
  predictions <- rep(0, nrow(Z_shuffled))
  
  
  for (i in 1:k) {
    # get train and test indices for this fold
    train_indices <- which(indices != i)
    val_indices   <- which(indices == i)
    
    
    
    # train model on k folds
    dataTr <- data_shuffled[train_indices,]
    dataVal <- data_shuffled[val_indices,]
    model <- lm(y~.,data=dataTr)
    
    # make predictions on validation fold

    
    predictions[val_indices] <- predict(model, newdata = dataVal[,-1])
  }
  
  
  
  
  # compute correlation of predictions
  cor_list[r] <- cor(predictions,data_shuffled$y)
}


SaveRDS(cor_list,file="SCVIVAE.rds")
