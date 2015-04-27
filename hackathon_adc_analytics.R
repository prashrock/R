install.packages("rfPermute")
install.packages("pamr")
install.packages("Matrix")
install.packages("party")
install.packages("caret")
install.packages("PerformanceAnalytics")
install.packages("e1071")
library("randomForest")
library("pamr")
library("MASS")
library("rfPermute")
library("party")
library("caret")
library("PerformanceAnalytics")
library("e1071")
library("ROCR") #tuneRF

#--------------------------------------------------------------------------------------------------
## Init
#--------------------------------------------------------------------------------------------------
ixia_data <- result
ixia_data$timestamp <- NULL                           #Timestamp has no meaning for analysis
ixia_data$rs_l7_total_time_avg_per_sec <- NULL        #Total time is NULL
ixia_data$test_name <- NULL                           #test_name is NULL
ixia_data$output <- factor(ixia_data$output)          #Good/Bad -- convert to factor
ixia_data$real_server <- as.factor(ixia_data$real_server)  #rs_name -- convert to factor
ixia_data2 <- ixia_data[sample(nrow(ixia_data)), ]    #Sort the values -- mix good and bad 
x <- 3:11
y <- 12

## Sanity check user X vectors
if(sum(is.na(ixia_data2)) > 0) { 
  stop("Error: NA's found in Input Data.")
}
ixia_data2[is.na(ixia_data2)] <- 0                    #When NA's found, set them to zero


## Standardize all independent variables in Dataset (Not needed for RF)
for (i in x) { ixia_data2[, i] = scale(ixia_data2[, i]) }

#--------------------------------------------------------------------------------------------------
## Supervised Learning
#--------------------------------------------------------------------------------------------------
set.seed(71)
#RF defaults, mtry = sq_root(#variables), ntree = 500
#Created a training set with ~2/3 dataset with replacement (not shown here)

#Tune mtry and ntree parameters of RF
#Use tuneRF to find the optimal mtry value (wrt Out-of-Bag error estimate).
#Below code comes up with mtry = 3  (Note: this may cause bias.)
mtry_values <- tuneRF(ixia_data2[, x], ixia_data2$output, ntreeTry = 500, stepFactor = 1.5,
                   improve = 0.01, trace=TRUE, plot=TRUE, dobest=TRUE)
bestmtry <- mtry_values[mtry_values[, 2] == min(mtry_values[, 2]), 1];
#Vary the ntree parameter in a loop to compute optimal #ntree
#Below code comes up with ntree = 200
ntree_values <- NULL
for (ntree in seq(50, 500, 50)) {
  ixia_data2.mtry <- randomForest(output ~ rs_state + rs_rx_throughput + rs_tx_throughput +
                                  rs_rx_pps + rs_tx_pps + rs_new_conn_cnt + rs_curr_conn_cnt + 
                                  rs_l7_rtt_avg_per_sec + rs_http_errors_per_sec, data=ixia_data2, 
                                  importance=TRUE, proximity=TRUE, mtry=bestmtry, ntree=ntree) 
  ntree_values <- rbind(ntree_values, c(ntree, ixia_data2.mtry$err.rate[500]))
  print(paste("OOB estimate for mtry=", bestmtry, 
              " ntree=", ntree, "is = ", ixia_data2.mtry$err.rate[500]))
}
ntree_values <- na.omit(ntree_values)
bestntree <- ntree_values[ntree_values[, 2] == min(ntree_values[, 2]), 1];

#Invoke RF. Note, class weights are used to penalizes misclassifying minority class (errors).
ixia_data2.rf <- randomForest(output ~ rs_state + rs_rx_throughput + rs_tx_throughput +
                              rs_rx_pps + rs_tx_pps + rs_new_conn_cnt + rs_curr_conn_cnt + 
                              rs_l7_rtt_avg_per_sec + rs_http_errors_per_sec, 
                              data = ixia_data2, importance = TRUE, proximity = TRUE, 
                              mtry = bestmtry, ntree = bestntree, classwt=c(1, 50))
print(ixia_data2.rf)
plot(ixia_data2.rf)


#Calculate proportion of correct predictions
labels <- as.factor(ixia_data2[[y]])
predictions <- levels(labels)[ixia_data2.rf$predicted]
predictionIsCorrect = labels == predictions
cat(sprintf("Proportion of correct predictions: %f\n", mean(predictionIsCorrect)))

#Look at variable importance:
round(importance(ixia_data2.rf), 2)
varImpPlot(ixia_data2.rf, type = 1,              #Mean Decrease in Accuracy plot
           main = paste("Reliability Metric - Variable Importance Chart"))

## Do MDS on 1 - proximity:
ixia_data2.mds <- cmdscale(1 - ixia_data2.rf$proximity, eig=TRUE)
op <- par(pty="s")
pairs(cbind(ixia_data2[, c(4, 6, 8, 10, 12)], ixia_data2.mds$points), cex=0.6, gap=0,
      col=c("red", "green", "blue")[as.numeric(ixia_data2$output)],
      main="Ixia Data: Predictors and MDS of Proximity Based on RandomForest")
par(op)
print(ixia_data2.mds$GOF)

#Predict class probabilities (i.e. 'certainty' scores)
pred <- predict(ixia_data2.rf, ixia_data2, "prob")
head(pred)

#--------------------------------------------------------------------------------------------------
## Unsupervised Learning
#--------------------------------------------------------------------------------------------------
set.seed(17)
ixia_data2.urf <- randomForest(ixia_data2[, x], importance=TRUE, proximity=TRUE, classwt=c(1,50))
MDSplot(ixia_data2.urf, ixia_data2$output, k=2, main = paste("Reliability Plot"),
        pch=as.numeric(ixia_data2$output), xlab = "X-axis", ylab = "Y-axis",
        xaxt='n', yaxt='n')

proximity.plot(ixia_data2.urf, dim.x = 1, dim.y = 2, legend.loc = NULL,
               grp.cols = NULL, circle.size = 4)
plot(outlier(ixia_data2.urf), type="h", 
     col=c("red", "green", "blue")[as.numeric(ixia_data2$output)])
print(ixia_data2.urf)

## Distance calculation - PAM (go with isoMDS for now)
#clust <- pam(ixia_data2.urf$proximity, k=2) 
#summary(clust)
 
## Distance Calculation - isoMDS
## Kruskal's Non-metric Multidimensional Scaling plot
ixia_data2.urf.cmd <- cmdscale(1-ixia_data2.urf$proximity)
distmat <- as.dist(1-ixia_data2.urf$proximity)
distmat[distmat==0] <- 0.5/dim(ixia_data2)[1]
ixia_data2.urf.mds <- isoMDS(distmat, y=ixia_data2.urf.cmd)
#--------------------------------------------------------------------------------------------------
