#-------------EXPECTED INFLATION PREDICTION VARIABLES SELECTION R script----------------
#Loads

library(rsample)
library(caret)
library(parallel)
library(parallel)
detectCores()             # utile pour info sur nb de cores dispo
cls <- makeCluster(6)     # demarrer un cluster à n clusters
stopCluster(cls); rm(cls)
#foreach(i = 1:2) %do% f()#séquentiel
#foreach(i = 1:6) %dopar% f()# parallèle

#Data preprocessing
inputs_fred <- read.csv(file.choose())
target <- read.csv(file.choose())
summary(inputs_fred)
str(inputs_fred)
summary(target)
str(target)
#To rename 
#target %>% 
#  rename(
#    Target = target)

#To check dimension
dim(inputs_fred)
dim(target)
#check null values
anyNA(inputs_fred)
anyNA(target)

#Make a copy of the original data frame
inputs_fred_prep <- inputs_fred

#join inputs variables and target
inputs_fred_prep$Target <- target
dim(inputs_fred_prep)
colnames(inputs_fred_prep)
str(inputs_fred_prep)

#Split the data train and test sets
set.seed(007)
data_split <- initial_split(inputs_fred_prep, prop = .80)
train <- training(data_split)
test  <- testing(data_split)
dim(train)
dim(test)
class(train$Target)

#Exclude the target
varToExc <- names(inputs_fred_prep) %in% c("Target")
#inputs variables
X <- inputs_fred_prep[!varToExc]
dim(X)
y <- inputs_fred_prep$Target
dim(y)

#Box plot: target
boxplot(test$Target)

#print model name in caret
model.names <- paste(names(getModelInfo()), collapse =', ')
model.names
#Turning parameters
modelLookup('ridge')

#-----------MODELING----------------------
#1.PCA
#fit
pca_model <- preProcess(train[, !names(train) %in% "Target"], method='pca')  
#Predict
train_pca <- predict(pca_model, newdata = train[, !names(train) %in% "Target"])
test_pca <- predict(pca_model, newdata = test[, !names(test) %in% "Target"])
str(train_pca)#from 160 to 96 var with pca
str(test_pca)

#change target from list to vector
train$Target <- unlist(train$Target)
test$Target <- unlist(test$Target)
str(train$Target)

#2.---------FEATURES SELECTION-----------

base_model <- lm(Target~1, data=train)#only intercept model
all_model <- lm(Target~., data=train)
summary(base_model)
summary(all_model)
#1.Forward Stepmwise selection
#step:The max number of models the algo will search
#Forward stepswise selection
forwardStep <- step(base_model, scope=list(lower=base_model, upper=all_model), 
                                             direction="forward", trace=1, steps= 1000)

#Best AIC is the lowest possible(best model minimises the AIC)
#Step:AIC=39352.07
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
summary(forwardStep)

#2.Backward Stepise selection 
backwardStep <- step(all_model, direction="backward", trace=1, steps= 1000)
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
#stepAIC=39351.02


#3.Forward/Backward selection
back_ForwStep <- step(base_model, scope = list(lower=base_model, upper=all_model), 
                    direction = "both", trace=1, steps= 1000)

summary(back_ForwStep)
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
#step AIC=39350.36

#4.BEST SUBSET MODEL
#Best subset regression
lm <- lm(Target~., data = train)#Base Model
outpout <- olsrr::ols_step_all_possible(lm) 
outpout
plot(outpout)
dtTrain <- data.table(train)

#-------------------SELECT ONLY SIG VARIABLES-----------------------------------
test_Sig <- test[, (names(test) %in% c("FinanceMuchWorseOff","FinanceSomewhatWorseOff","FinanceAboutTheSame","FinanceSomewhatBetterOff", 
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


train_Sig <- train[, (names(train) %in% c("FinanceMuchWorseOff","FinanceSomewhatWorseOff","FinanceAboutTheSame","FinanceSomewhatBetterOff", 
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
dim(train_Sig)
dim(test_Sig)

#Export train and test as csv
write.csv(train_Sig,"D:/Venvs/CV/Finance/Inflation-Expectation-Prediction/data/train_Sig.csv", row.names = FALSE)
write.csv(test_Sig,"D:/Venvs/CV/Finance/Inflation-Expectation-Prediction/data/test_Sig.csv", row.names = FALSE)






