#----------------------------------DECOMPOSING EXPECTED INFLATION PREDICTION
library(dplyr)
library(mice)
library(rsample)
#1.EDA
#Import the imput
inputs.fred <- read.csv(file.choose())
str(inputs.fred)
summary(inputs.fred)
#import the target
target <- read.csv(file.choose())
summary(target)
head(target)
str(target)
target %>% 
  rename(
    Target = Target
  )

dim(inputs.fred)
dim(target)
anyNA(inputs.fred)
str(target)
str(inputs.fred)

#Make a copy of the original data frame
inputs.fred.prep <- inputs.fred
#Insert `Target` back to original data frame
inputs.fred.prep$Target <- target$Target
dim(inputs.fred.prep)
colnames(inputs.fred.prep)
str(inputs.fred.prep)
#Exclude the target
#varToExc <- names(inputs.fred.prep) %in% c("Target")
#
#x <- inputs.fred.prep[!varToExc]
#dim(x)
#y <- inputs.fred.prep$Target
#dim(x)
#Split the data train and test sets
library(rsample)
library(partykit)
library(caretEnsemble)
library(caret)
library(mice)

set.seed(88)
data_split <- initial_split(inputs.fred.prep, prop = .80)
train <- training(data_split)
test  <- testing(data_split)
dim(train)
dim(test)
head(test)
#Box plot: target
boxplot(test$Target)
boxplot.stats(test$Target)$out
#To call all models names
model.names <- paste(names(getModelInfo()), collapse =', ')
model.names
#Turning parameters
modelLookup('ridge')
#2.-----------------------------MODELLING
#
#train$Target <- unlist(train$Target)
#test$Target <- unlist(test$Target)
#str(train$Target)
#3.------------------------------------FEATURES SELECTION
base.model <- lm(Target~1, data=train)#only intercept model
all.model <- lm(Target~., data=train)#all features model
summary(base.model)
summary(all.model)
#1.Forward Stepmwise selection
#step:The max number of models the algo will search 
forwardStep <- step(base.model, scope=list(lower=base.model, upper=all.model), 
                     direction="forward", trace=1, steps= 1000)

summary(forwardStep)
#Best AIC is the lowest possible(best model minimises the AIC)
#Step:  AIC=37135.99
#Target ~ Inflation + College + Loan12m.MuchHarder + Male + UnemRate.60.80 + 
  #FinanceSomewhatWorseOff + UnemRate..80 + IntRate..20 + UnemRate.40.60 + 
  #FinanceMuchWorseOff + Taxe.Increase + Age.Under.40 + Inflation_2y + 
  #Loan12m.SomewhatHarder + FinProspMuchWorseOff + FinProspSomewhatWorseOff + 
  #StockPrice..20 + y.2014 + Children5.Younger.1 + X100k.Over + 
  #FullTime + UnableToWork + HighSchool + West + Loan12m.SomewhatEasier + 
  #WorkD.Between6mAnd1y + Homemaker + y.2015 + y.2013 + HomePrice2y.Increase + 
  #Age.40.60 + IntRate.20.40 + Children18.24.0 + StockPrice.60.80 + 
  #SickOrLeave + NativeHawaiianOrPacificIslander + South + Children25.Older.1 + 
  #Loan.MuchHarder + Spouse.Partner.3More


#2.Backward Stepise selection 
backwardStep <- step(all.model, direction="backward", trace=1, steps= 1000)
summary(backwardStep)
# Target ~ FinanceMuchWorseOff + FinanceSomewhatWorseOff + 
#   FinProspMuchWorseOff + FinProspSomewhatWorseOff + UnemRate..20 + 
#   UnemRate.20.40 + UnemRate.40.60 + UnemRate..80 + IntRate..20 + 
#   IntRate.40.60 + IntRate.60.80 + StockPrice..20 + StockPrice.20.40 + 
#   Inflation + Inflation_2y + FullTime + PartTime + NotWorking + 
#   SickOrLeave + UnableToWork + Retiree + Other + Other.1 + 
#   NberJob.Missing + Job.WorkForSomeoneElse + Job.SelfEmployed + 
#   JobSearching.No + Taxe.Increase + Loan.EquallyEasy.Hard + 
#   Loan12m.MuchHarder + Loan12m.SomewhatEasier + Female + White + 
#   NativeHawaiianOrPacificIslander + WorkD.LessThan1m + WorkD.Between1yAnd5y + 
#   WorkD.MoreThan5y + TimeSpentRes..2 + TimeSpentRes.2.5 + TimeSpentRes.5.10 + 
#   TimeSpentRes.10.20 + TimeSpentRes..20 + Spouse.Partner.0 + 
#   Spouse.Partner.1 + Spouse.Partner.2 + Children25.Older.1 + 
#   Children18.24.0 + Children5.Younger.1 + AlmostAlwaysAll + 
#   Age.40.60 + Age.60.Over + Age.Under.40 + South + West + College + 
#   SomeCollege + Under50k + X50k.100k + y.2013 + y.2014 + y.2015
#AIC:
#Multiple R-squared:  0.1663,	Adjusted R-squared:  0.162 

#3.Forward/Backward selection
backForStep <- step(base.model, scope = list(lower=base.model, upper=all.model), 
                direction = "both", trace=1, steps= 1000)
Step:  AIC=37135.99
# Target ~ Inflation + College + Loan12m.MuchHarder + Male + UnemRate.60.80 + 
#   FinanceSomewhatWorseOff + UnemRate..80 + IntRate..20 + UnemRate.40.60 + 
#   FinanceMuchWorseOff + Taxe.Increase + Age.Under.40 + Inflation_2y + 
#   Loan12m.SomewhatHarder + FinProspMuchWorseOff + FinProspSomewhatWorseOff + 
#   StockPrice..20 + y.2014 + Children5.Younger.1 + X100k.Over + 
#   FullTime + UnableToWork + HighSchool + West + Loan12m.SomewhatEasier + 
#   WorkD.Between6mAnd1y + Homemaker + y.2015 + y.2013 + HomePrice2y.Increase + 
#   Age.40.60 + IntRate.20.40 + Children18.24.0 + StockPrice.60.80 + 
#   SickOrLeave + NativeHawaiianOrPacificIslander + South + Children25.Older.1 + 
#   Loan.MuchHarder + Spouse.Partner.3More
#AIC=37135.99
#Multiple R-squared:  0.1638,	Adjusted R-squared:  0.1609 
summary(backForStep)
#4.BEST SUBSET MODEL
#Best subset regression
lm <- lm(Target~., data = train)#Base Model
outpout <- olsrr::ols_step_all_possible(lm) 
outpout
plot(outpout)
dtTrain <- data.table(train)
head(dtTrain)
str(dtTrain)
#-------------------SELECT ONLY SIGNIFICANT VARIABLES
trainSig <- train[, (names(train) %in% c("FinanceMuchWorseOff","FinanceSomewhatWorseOff","FinanceAboutTheSame","FinanceSomewhatBetterOff", 
                             "FinanceMuchBetterOff","FinProspMuchWorseOff","FinProspSomewhatWorseOff","FinProspAboutTheSame",
                             "FinProspSomewhatBetterOff","FinProspMuchBetterOff","UnemRate:<20","UnemRate:20-40","UnemRate:40-60", 
                             "UnemRate:60-80","UnemRate:>80","IntRate:<20","IntRate:20-40","IntRate:40-60","IntRate:60-80","IntRate:>80", 
                             "StockPrice:<20","StockPrice:20-40','StockPrice:40-60','StockPrice:60-80','StockPrice:>80','Inflation", 
                             "Deflation","Inflation_2y","Deflation_2y","FullTime","PartTime","NotWorking","LaidOff","SickOrLeave",
                             "UnableToWork","Retiree","Student","Homemaker","Other","Taxe:Increase","Taxe:Decrease","Loan:MuchHarder",
                             "Loan:SomewhatHarder","Loan:EquallyEasy/Hard","Loan:SomewhatEasier","Loan:MuchEasier","Loan12m:MuchHarder", 
                             "Loan12m:SomewhatHarder","Loan12m:EquallyEasy/Hard","Loan12m:SomewhatEasier","Loan12m:MuchEasier","HomePrice2y:Increase",
                             "HomePrice2y:Decrease","HomePrice2y:Missing","Female","Male","Hispanic","NonHispanic","White","BlackOrAfrican",
                             "AmericanIndianOrAlaskaNative","Asian","NativeHawaiianOrPacificIslander","Other","WorkD:LessThan1m", 
                             "WorkD:Between1And6m",'WorkD:Between6mAnd1y","WorkD:Between1yAnd5y","WorkD:MoreThan5y","WorkD:Missing','Spouse/Partner:0',
                             "Spouse/Partner:1","Spouse/Partner:2","Spouse/Partner:3More","Children25-Older:0","Children25-Older:1",
                             "Children25-Older:2More","Children18-24:0","Children18-24:1","Children18-24:2More","Children5-Younger:0",
                             "Children5-Younger:1","Children5-Younger:2More","Age:40-60","Age:60-Over","Age:Under-40","South", 
                             "Midwest","West","Northeast","College","SomeCollege","HighSchool","Under50k","50k-100k","100k-Over",
                             "y:2013","y:2014","y:2015","y:2016","y:2017","y:2018","y:2019", "Target"))]

testSig <- test[, (names(test) %in% c("FinanceMuchWorseOff","FinanceSomewhatWorseOff","FinanceAboutTheSame","FinanceSomewhatBetterOff", 
                             "FinanceMuchBetterOff","FinProspMuchWorseOff","FinProspSomewhatWorseOff","FinProspAboutTheSame",
                             "FinProspSomewhatBetterOff","FinProspMuchBetterOff","UnemRate:<20","UnemRate:20-40","UnemRate:40-60", 
                             "UnemRate:60-80","UnemRate:>80","IntRate:<20","IntRate:20-40","IntRate:40-60","IntRate:60-80","IntRate:>80", 
                             "StockPrice:<20","StockPrice:20-40','StockPrice:40-60','StockPrice:60-80','StockPrice:>80','Inflation", 
                             "Deflation","Inflation_2y","Deflation_2y","FullTime","PartTime","NotWorking","LaidOff","SickOrLeave",
                             "UnableToWork","Retiree","Student","Homemaker","Other","Taxe:Increase","Taxe:Decrease","Loan:MuchHarder",
                             "Loan:SomewhatHarder","Loan:EquallyEasy/Hard","Loan:SomewhatEasier","Loan:MuchEasier","Loan12m:MuchHarder", 
                             "Loan12m:SomewhatHarder","Loan12m:EquallyEasy/Hard","Loan12m:SomewhatEasier","Loan12m:MuchEasier","HomePrice2y:Increase",
                             "HomePrice2y:Decrease","HomePrice2y:Missing","Female","Male","Hispanic","NonHispanic","White","BlackOrAfrican",
                             "AmericanIndianOrAlaskaNative","Asian","NativeHawaiianOrPacificIslander","Other","WorkD:LessThan1m", 
                             "WorkD:Between1And6m",'WorkD:Between6mAnd1y","WorkD:Between1yAnd5y","WorkD:MoreThan5y","WorkD:Missing','Spouse/Partner:0',
                             "Spouse/Partner:1","Spouse/Partner:2","Spouse/Partner:3More","Children25-Older:0","Children25-Older:1",
                             "Children25-Older:2More","Children18-24:0","Children18-24:1","Children18-24:2More","Children5-Younger:0",
                             "Children5-Younger:1","Children5-Younger:2More","Age:40-60","Age:60-Over","Age:Under-40","South", 
                             "Midwest","West","Northeast","College","SomeCollege","HighSchool","Under50k","50k-100k","100k-Over",
                             "y:2013","y:2014","y:2015","y:2016","y:2017","y:2018","y:2019", "Target"))]

dim(trainSig)
dim(testSig)
#Exporting into csv file
getwd()
write.csv(trainSig,'D:\\DataBases\\DataSets\\FED NY Expected Inlation\\TrainSig.csv', row.names=FALSE)
write.csv(testSig,'D:\\DataBases\\DataSets\\FED NY Expected Inlation\\TestSig.csv', row.names=FALSE)
#4.------------------------------HYPER TUNING PARAMETER
#1.GRID CROSS VALIDATION
#1.1Define the grid
marsGrid <- expand.grid(lambda= seq(0, 1, 0.01))
#1.2Define the train control function
#Tune hyper parameters by setting tunegrid
fitControl <- trainControl(
  method = 'cv',
  number = 5,
  search = 'grid',
  savePredictions = 'final',
  classProbs = F,
  summaryFunction =defaultSummary
)
startTime <- Sys.time()
set.seed(88)
TunedmarsGridModel <- train(Target ~., data=trainSig, method="ridge", metric="RMSE",
                            tuneGrid=marsGrid, trControl=fitControl)
endTime <- Sys.time()
timeTaken <- endTime - startTime
timeTaken

TunedmarsGridModel
#1.3.Make Prediction
predsGrid <- predict(TunedmarsGridModel, testSig)
predsGrid 
actuals <- testSig$Target
actuals
#Evaluation
DMwR::regr.eval(actuals, predsGrid)

#2.CRoss Validation
cv.Results <- suppressWarnings(CVlm(data=train, 
                                    form.lm=formula,
                                    m=5,
                                    dots=FALSE,
                                    seed= 88,
                                    legend.pos="topleft",
                                    printit =FALSE,
                                    main="small symbols are predicted velues while bigger ones are actuals"));

#3.MSE
attr(cvResults, 'ms')
#CV with caret package 
#library(caret)
train.control <- trainControl(method = "LOOCV")#LeaveOutOneCV
#Train the model
model1 <- train(formula, data= train, method="ridge", trControl= train.control)
#Summarize
print(model1)
summary(model1)

#Reapeted K-Fold CV
train.control <- trainControl(method = "repeatedcv", repeats = 5)
#Train
model2 <- train(formula, data=train, method="ridge", trControl= train.control)
#Summarize
print(model2)
summary(model2)
#LeaveGroupOut (CVLGOCV)

#Multiple model tuning
#Stacking Algorithms - Run multiple algos in one call.
trainControl <- trainControl(method="repeatedcv", 
                             number=10, 
                             repeats=3,
                             savePredictions=TRUE, 
                             classProbs=F)

algorithmList <- c('ridge', 'lasso', 'lm')
set.seed(88)
models <- caretList(Target ~ ., data=train, trControl=trainControl, methodList=algorithmList) 
results <- resamples(models)
results

#Box plots to compare models
scales <- list(x=list(relation="free"), y=list(relation="free"))
bwplot(results, scales=scales)
# Create the trainControl
set.seed(88)
stackControl <- trainControl(method="repeatedcv", 
                             number=10, 
                             repeats=5,
                             savePredictions=TRUE, 
                             classProbs=TRUE)

# Ensemble the predictions of `models` to form a new combined prediction based on glm
stack.glm <- caretStack(models, method="ridge", metric="mse", trControl=stackControl)
print(stack.glm)
?ridge
# Predict on df_test
stack_preds <- predict(stack.glm, newdata=test)
head(stack_preds)
##
#Model with best adj R-sq
outpout[which.max(outpout$adjr), ]

model.final <- lm(Target~ + inflation)
summary(model.final)
par(mfrow=c(2,2));plot(model.final)

#Breusch Pagan Test
#Ho: Homoscedastic variance
#H1: Heteroscedastic variance
#pvalue > alpha(0.1, 0.05, 0.01)
#Ho can not be rejected at alpha level: var are Homoscedastic
car::ncvTest(model.final)
#Make prediction with the best model
preds <- predict(model.final, test)
regr.eval(test$Target, preds)