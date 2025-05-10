
#Set the folder for data analyses
setwd("/Users/____/Desktop/Data")

#For ELA (Fig.3) of ML (Fig.S10)
#Store the raw data for ELA within the folder "Data" 
#ELA_raw.csv (stored in the "2.Binarized data" of the sheet name "Figs.3 and S11" of DataFCPCfinal241115.xlsx)
#ELA_grouplist.csv (stored in the "3.Group list for ELA" of the sheet name "Figs.3 and S11" of DataFCPCfinal241115.xlsx)

#Make th folder for ELA
dir.create("./Result_ELA", showWarnings = TRUE, recursive = FALSE, mode = "0777")

#Analyze based on the following protocol
#(https://github.com/kecosz/ela; https://github.com/kecosz/rELA) and the DOIs (10.5281/zenodo.5492161 and 10.5281/zendo.7979838). 

#Store the calculated data within the folder "Result_ELA" 

#For AA (Fig.4a) 
#memory clear
rm(list=ls(all=TRUE))
invisible(replicate(20, gc()))
library(dplyr)

#Set the folder for data analyses
setwd("/Users/____/Desktop/Data")

#Set the R library
if (!require("arules")) install.packages("arules")
if (!require("arulesViz")) install.packages("arulesViz")
require(arules) 
require(arulesViz)

dir.create("./Result_AA_RF_XG", showWarnings = TRUE, recursive = FALSE, mode = "0777")

#Read the binarized file for AA (stored "Bibarized data for association analysis" in the sheet name "Fig.4a" of DataFCPCfinal241115.xlsx)
readfile="AA_binarized_raw_data.csv"

x=read.csv(readfile, header=T, colClasses="factor")
rulesAp1 <- apriori(x, parameter=list(support=0.25,confidence=0.5,maxlen=2))
rule_Ap1<-rulesAp1[quality(rulesAp1)$lift>1.2]

write(rule_Ap1,"./Result_AA_RF_XG/AA_calculated_data.csv",sep=",")

#For RF and XGBoost (Fig.4a) 
#Read the raw data file for AA-selected components
#For XGBoost
options(na.action='na.pass') # for missing value
library(xgboost)
FCPC.train <- read.csv("RF_XG_raw_data.csv") # tibble::glimpse() 
FCPC.test <- read.csv("RF_XG_raw_data.csv") ## please use the filename "RF_XG_raw_data.csv", which the test group of pig dataset is summarized as Comp_ThB, to classify into 8 groups
dim(FCPC.train)
train.x <- FCPC.train[, 2:43] #dim(FCPC.train)[1] 36 24
x <- rbind(train.x,FCPC.test[,-1])

y_0 <- c(FCPC.train$Species)
y_1 <- as.factor(y_0)
y <- as.integer(y_1)-1 #caution the order of list as 2,3,0,1,6,7,4,5 
#2,CW_control(Chicken_Con);3,CW_test(Chicken_Comp);0,FE_control(Fish_Con);1,FE_test(Fish_Comp);6,PK_control(Pig_Con);7,PK_test(Pig_Comp_ThB);4,KC_control(Cattel_Con);5,KC_test(Cattle_ThB)

x <- as.matrix(x)
trind <- 1:length(y) 
teind <- (nrow(train.x)+1):nrow(x) 
set.seed(131) #fixed seed

param <- list("objective" = "multi:softprob", 
              "eval_metric" = "mlogloss", 
              "num_class" = 6 # class (Fish_Con; Fish_Comp; Chicken_Con; Chicken_Comp; Pig_Con; Pig_Comp_ThB; Cattle_Before; Cattle_After)
)

k<-round(1+log2(nrow(train.x)))
cv.nround <- 100 #search
bst.cv <- xgb.cv(param=param, data = x[trind,], label = y,  nfold = k, nrounds=cv.nround)

set.seed(131)
nround <- 12 # or 12 based on the above bst.cv

bst <- xgboost(param=param, data = x[trind,], label = y, nrounds=nround)
pred <- predict(bst,x[teind,]) 
pred <- matrix(pred,6,length(pred)/6)　# class (Fish_Con; Fish_Comp; Chicken_Con; Chicken_Comp; Pig_Con; Pig_Comp_ThB; Cattle_Con; Cattle_ThB)
pred <- t(pred)
colnames(pred)<-c("FE_Control","FE_Test","CW_Control","CW_Test","PK_Control","PK_Test")
#2,CW_control(Chicken_Con);3,CW_test(Chicken_Comp);0,FE_control(Fish_Con);1,FE_test(Fish_Comp);6,PK_control(Pig_Con);7,PK_test(Pig_Comp_ThB);4,KC_control(Cattel_Con);5,KC_test(Cattle_ThB)

head(pred,6) #6 group/class

# preparation of test dataset
test.x <- FCPC.test[, 2:43]
x_t <- rbind(test.x,FCPC.test[,-1])

y_0_t <- c(FCPC.test$Species)
y_1_t <- as.factor(y_0_t)
y_t <- as.integer(y_1_t)-1

x_t <- as.matrix(x_t)
trind_t <- 1:length(y_t) 
teind_t <- (nrow(test.x)+1):nrow(x_t) 

param <- list("objective" = "multi:softmax", # modify the function "multi:softmax"
              "eval_metric" = "mlogloss", 
              "num_class" = 6 #class_No.
)

set.seed(131)
nround <- 28 # or 27 based on the above bst.cv
bst_t <- xgboost(param=param, data = x_t[trind_t,], label = y, nrounds=nround)
pred_t <- predict(bst_t,x_t[teind_t,])

#8 groups (Group_8 in Fig.S11)
x_1_f <- FCPC.test[,1]
for(i in 1:length(pred_t)){
  if(pred_t[i]==0) {pred_t[i]="Control"}
  else if(pred_t[i]==1) {pred_t[i]="Test"}
  else if(pred_t[i]==2) {pred_t[i]="Control"}
  else if(pred_t[i]==3) {pred_t[i]="Test"}
  else if(pred_t[i]==4) {pred_t[i]="Control"}
  else if(pred_t[i]==5) {pred_t[i]="Test"} #Pig_Comp
}

table(x_1_f,pred_t) # Please check 100% accuracy rate

sink('./Result_AA_RF_XG/XGboost_pre_x_1_f.txt', append = TRUE)
print (table(x_1_f,pred_t))
sink()

write.csv(table(x_1_f,pred_t),"./Result_AA_RF_XG/XGboost_pre_x_1_f.csv")

imp<-xgb.importance(names(y_1),model=bst_t)
print(imp)
xgb.plot.importance(imp) 

pdf ("./Result_AA_RF_XG/XGBoostgraph.pdf") 
xgb.plot.importance(imp) 
dev.off()

write.csv(print(imp),"./Result_AA_RF_XG/XGBoostgraph_raw.csv")


#For RF
library(randomForest)
set.seed(131)
train.x<- FCPC.train[,2:43] #dim(FCPC.train) [1]  36 24
train.y<-as.factor(FCPC.train[,1])
model.rf<-tuneRF(train.x,train.y,doBest=T)

sink('./Result_AA_RF_XG/randomforest_model_rf.txt', append = TRUE)
print (model.rf)
sink()

#preparation of test dataset
dim(FCPC.test)
test.x<-FCPC.test[,2:43] #dim(FCPC.test) [1]  36 24
pred_t<-predict(model.rf,test.x)
table(FCPC.train[,1],pred_t) # Please check 100% accuracy rate
rf_pred_t <- table(FCPC.train[,1],pred_t) 
write.csv(rf_pred_t,"./Result_AA_RF_XG/randomForest_pred1.csv")

pred_t_1<-predict(model.rf,FCPC.test[,2:24]) #dim(FCPC.test) [1]  36 24
table(FCPC.test[,1],pred_t_1) # Please check 100% accuracy rate
rf_pred_t_1 <- table(FCPC.test[,1],pred_t_1) 
write.csv(rf_pred_t_1,"./Result_AA_RF_XG/randomForest_pred2.csv") #to check

print(model.rf$importance /sum(model.rf$importance))

write.csv(print(model.rf$importance /sum(model.rf$importance)),"randomForest_raw.csv")

set.seed(22)
model = randomForest(y_1 ~ ., data = test.x, mtry=4, importance = TRUE, proximity = TRUE) # the number of mtry with minimum error based on the function tunRF (doBest=TRUE as parameter)
print(model)
print(varImpPlot(model))

FCPC.test_n <- cbind(y_1, test.x) #to check
  
set.seed(22)
model_t = randomForest(y_1 ~ ., data = test.x, importance = TRUE, proximity = TRUE) #to check
print(model_t)
print(varImpPlot(model_t))

write.csv(print(varImpPlot(model_t)),"./Result_AA_RF_XG/randomForest_pred_importance_Gini_model_t.csv") #to check

par(mar=c(100, 20, 30, 40)) 
rpp2 <- varImpPlot(model)
varFileName <- paste("./Result_AA_RF_XG/randomforest_tree_var.png",sep="") 
par(mar=c(100, 20, 30, 600)) 
png(file=varFileName, res=125, w=750, h=750)
rpp2 <- varImpPlot(model)
dev.off()

#Make the file (ML_mix.xlsx) containing the values of components selected by AA,RF, and XGBoost
#Use python to illustrate the Bubble chart
#"Bubblechart.py" (load the sheet name 'MLmix' in 'ML_mix.xlsx') #please use the file for classification of 8 groups
