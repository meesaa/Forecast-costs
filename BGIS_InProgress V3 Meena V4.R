### Project ###

#Libraries and load data: "Data_Cleaned_Topics.csv" ----
#install.packages("glmnet")
library(glmnet)
library(quantreg)
library(rpart.plot)
pacman::p_load("tidyverse","gpairs","readxl", "caret","e1071","caTools","corrplot",
               "ggplot2","GGally", "MASS","arules","sqldf","car","rpart","Metrics","xgboost")
df_raw <- read.csv(file.choose())
  
  df <- df_raw

#EDA Summary stats----
  glimpse(df)
  summary(df)
  names(df)
  str(df)
  View(df)
  #DataType conversions
  df$LeaseInd <- as.factor(df$LeaseInd)
  df$Vendor_Key <- as.factor(ifelse(df$Vendor_Key > 0, 1,0))
  
#Recoding levels which have fewer records or provides meaning ----
  #Property_Usage: Recoding levels which have fewer records
  df$Property_Usage <- recode(df$Property_Usage, '"CORP_OPS" = "CORP"')
  df$Property_Usage <- recode(df$Property_Usage, 'c("BUILDING", "CONDO", "MIXED USE", "WAREHOUSES", "N/A") = "OTHER"')
  df$Property_Usage <- recode(df$Property_Usage, 'c("RET_LIMITED SVC", "RET_SEC_SERVICES", "RET_TRAINING") = "RET_OTHER"')
  table(df$Property_Usage)
  #Parent_workorder: recode nums to 1s and NA's to 0's - this made sense than having work ord num to missing parnet work ord num
  df$Parent_WorkOrderNbr  <-  ifelse(is.na(df$Parent_WorkOrderNbr) == TRUE , 0, 1)
  # ifelse(is.na(df$Parent_WorkOrderNbr) == TRUE , 
  #        df$Parent_WorkOrderNbr <- df$WorkOrder_Nbr, 
  #        df$Parent_WorkOrderNbr <- df$Parent_WorkOrderNbr) 
  df$Parent_WorkOrderNbr <- as.factor(df$Parent_WorkOrderNbr);
  str(df$Parent_WorkOrderNbr)
  
  
#find correlations ggcorr function disregards non-numeric variables automatically
  ggcorr(df,label = TRUE, label_alpha = TRUE, label_size = 4, hjust = 0.90, size = 4, layout.exp = 2) 
  #Work_Duration_Days vs Estimated_Time_Days have correlation of 0.8 - to explore further

  # Finding missing values - na's & N/A's ----
  if(any(is.na(df)) == TRUE) {
    names(which(sapply(df, anyNA)))
    sum(is.na(df))
  } else {
    "No Missing values found"
  }
  
#Response variable distribution
  plot(density(df$Func_Burdened_Cost), main="Density Plot: Function_Burdened_Cost", ylab="Cost", 
       sub=paste("Skewness:", round(e1071::skewness(df$Func_Burdened_Cost), 2)))  # density plot for 'cost'
  polygon(density(df$Func_Burdened_Cost), col="red")
  
#To check outliers and skewness for numeric variables - 6 variables are skewed
  #Rentable_SQFT Parent_WorkOrderNbr  Func_Burdened_Cost  Work_Duration_Days Estimated_Time_Days off_by_days
  #5.569586            1.960673           23.251340            7.357774            3.789893       6.994248
  skewValues <- apply(select_if(df, is.numeric), 2, skewness)
  head((skewValues)); rm(skewValues)

#Visualizing skewed data dsitributions - removing outliers, to apply transformations if required
  # boxplot((df$Func_Burdened_Cost))
  df %>% ggplot(aes(Func_Burdened_Cost)) + geom_histogram()
  df %>% count(Func_Burdened_Cost>20000)    #Removed outliers where Cost>$20,000, removed 7 obs
  df <- df %>% filter(Func_Burdened_Cost<20000)
  
  df %>% ggplot(aes(Work_Duration_Days)) + geom_histogram()   
  df %>% count(Work_Duration_Days>365)    #removed outliers which are >365 days, 38 obs 
  df <- df %>% filter(Work_Duration_Days<365)
  
  df %>% ggplot(aes(off_by_days)) + geom_histogram()   
  df %>% count(off_by_days< (-200))      #removed outliers which are < -200, 12 obs
  df <- df %>% filter(off_by_days> (-200))
  
  df %>% ggplot(aes(Rentable_SQFT)) + geom_histogram()   #we have many >50000
  df %>% count(Rentable_SQFT > 50000)
  
  df %>% ggplot(aes(Estimated_Time_Days)) + geom_histogram()
  df %>% count(Estimated_Time_Days < 0)    #estimated days < 0 ? - to remove these data (45 recs)?


#Working on date and time features
  # Creation time in proper date format
  df$Creation_time <- as.POSIXct(df$Creation_Date, format("%Y-%m-%d %H:%M:%S"))
  df$Completion_time <- as.POSIXct(df$Completion_Date, format("%Y-%m-%d %H:%M:%S"))
  df$TargetCompletion_time <- as.POSIXct(df$TargetCompletion_Date, format("%Y-%m-%d %H:%M:%S"))

  #without the hours
  df$Creation_timedate <- as.Date(df$Creation_Date, format("%Y-%m-%d"))
  df$Completion_timedate <- as.Date(df$Completion_Date, format("%Y-%m-%d"))
  df$TargetCompletion_timedate <- as.Date(df$TargetCompletion_Date, format("%Y-%m-%d"))

  #day of week
  df$Creation_day <- weekdays(as.Date(df$Creation_Date))
  df$Completion_day <- weekdays(as.Date(df$Completion_Date))
  df$TargetCompletion_day <- weekdays(as.Date(df$TargetCompletion_Date))
  
  #number of days between 2 dates
  df$Actual_time_needed = (df$Completion_timedate - df$Creation_timedate)
  df$Estimated_time_needed = df$TargetCompletion_timedate - df$Creation_timedate
  df$EstimedVSactual_time_needed = df$TargetCompletion_timedate - df$Completion_timedate
  
  #is weekend binary
  df$WeekendCompletion = as.factor(ifelse(df$TargetCompletion_day == 'Saturday' , 1, 
                                ifelse(df$TargetCompletion_day == 'Sunday', 1, 0)))
  
  #what season binary
  df$Month <- as.integer(format(as.Date(df$Creation_time), "%m"))
  df$ColdWeather <- as.factor(ifelse(df$Month > 10, 1, ifelse(df$Month <= 04, 1, 0)))
  df$Month <- as.factor(df$Month)
  #high standard dev in costs because has highest costs occured
  # df$HighCosts <-  ifelse(df$ServiceType_Name == 'HVAC REPAIRS', 1, 
  #                         ifelse(df$ServiceType_Name == 'ELECTRICAL REPAIRS', 1,  
  #                                ifelse(df$ServiceType_Name == 'HVAC MAINTENANCE', 1, 0)))
  # df$HighCosts
  
  #morning or afternoon service request
  df$Hour <- as.POSIXlt(df$Creation_time)$hour
  df$MorningServiceRequest <- as.factor(ifelse(df$Hour < 13, 1, 0))
  
  #overnight requests, possibly urgent
  df$OvernightServiceRequest <- as.factor(ifelse(df$Hour < 7, 1, ifelse(df$Hour > 20, 1, 0)))


  #Some more cleanup ----  
  str(df)
  # Switch month to factor
  df$Hour <- factor(df$Hour)
  df$Month <- factor(df$Month)
  df$Creation_day <- factor(df$Creation_day)
  df$Completion_day <- factor(df$Completion_day)
  df$TargetCompletion_day <- factor(df$TargetCompletion_day)
  # switch the time variables to integers 
  #df$Actual_time_needed <- as.integer(df$Actual_time_needed)
  df$Estimated_time_needed <- as.integer(df$Estimated_time_needed)
 # df$EstimedVSactual_time_needed <- as.integer(df$EstimedVSactual_time_needed)
  
  
  #Feature Engineering ----
  #Combine Building_ID and Building_Key & convert to factors
  df$BuildingIDKey <- paste(df$Building_ID,df$Building_Key); df$BuildingIDKey <- as.factor(df$BuildingIDKey)
  
  #Combining ServiceProvider_Class and ServiceProvider_Type - & recoding the ones which have few records
  # df$SerProv.ClassType <- paste(df$ServiceProvider_Class,df$ServiceProvider_Type, sep = ".")
  # df$SerProv.ClassType <- recode(df$SerProv.ClassType, 
  #                                'c("LANDLORD.CLIENT", "LANDLORD.OPS MGR", "LANDLORD.PROPMGR", "LANDLORD.RD") 
  #                                = "LANDLORD.OTHER"')
  # df$SerProv.ClassType <- recode(df$SerProv.ClassType, 
  #                                'c("VENDOR.CLIENT SERVICES/TENANT", "VENDOR.EMPLOYEE", "VENDOR.FOOD SERVICES/CATERING",
  #                                "VENDOR.FURNISHING/INSTALL/REPAIR", "VENDOR.FURNISHINGS/SUPPLY ONLY","VENDOR.GOVERNMENT AGENCY",
  #                                "VENDOR.INTERCOMPANY","VENDOR.MARKETING/ADVERTISING") 
  #                                = "VENDOR.OTHER"')
  # table(df$SerProv.ClassType)  
  
  #Recoding WorkOrderType_Cd<1000 to OTHER
  df$WorkOrderType_Cd <- recode(df$WorkOrderType_Cd, 'c("CDI", "DC", "CW", "QI", "DR", "PR", 
                                "RR", "RCC", "ENG", "CP", "SC") = "OTHER"'); levels(df$WorkOrderType_Cd)
  
  #Drop variables - Duplicates or non-sgnificant ones - 12 variables
  dropVar <- c("Building_ID",   #combined
               "Building_Key",  #combined
               "City",          #Duplicate
               "WorkOrder_Nbr", #Just an unique identifier
               "Description_Document",    #Text Analytics
               "Resolution_Document",     #Text Analytics
               "ServiceType_Cd",         #Duplicate
               "BOMA_Sub_Category",      #Only one level of data
               "WorkOrderType_Desc1",    #Duplicate
               "LeaseInd2",                   #Duplicate
               "Work_Duration_Days_Rounded",  #Duplicate
               "off_by_days_Rounded")         #Duplicate
  df <- df[, !(colnames(df) %in% dropVar)]; rm(dropVar)
  # going to have to remove the above two columns for now 
  # df$Vendor_Key <- NULL
  df$Creation_Date <- NULL;
  df$Completion_Date <- NULL
  df$TargetCompletion_Date <- NULL;
  df$Creation_timedate  <- NULL;    
  df$Completion_timedate <- NULL
  df$TargetCompletion_timedate <- NULL
  df$TargetCompletion_time <- NULL
  df$Creation_time <- NULL;         
  df$Completion_time <- NULL 
  df$Creation_time <- NULL;
  df <- df[df$Estimated_Time_Days > 0,]
  df <- df[df$Work_Duration_Days > 0,]
  
  df#cities buckets
  combinerarecategories<-function(data_frame,mincount){ 
    a<-data_frame
    replace <- names(which(table(a) < mincount))
    levels(a)[levels(a) %in% replace] <-paste("Other",colnames(data_frame),sep=".")
    data_frame<-a
    return(data_frame) }
  
  df$City_up2<-combinerarecategories(df$City_up2,25)
  
  combinerarecategoriesbetween<-function(data_frame,mincount,maxcount){ 
      a<-data_frame
      replace <- names(which(table(a) >= mincount & table(a) < maxcount))
      levels(a)[levels(a) %in% replace] <-paste("Other1",colnames(data_frame),sep=".")
      data_frame<-a
    return(data_frame) }
  
  df$City_up2<-combinerarecategoriesbetween(df$City_up2,25,50)
  
  combinerarecategoriesbetween<-function(data_frame,mincount,maxcount){ 
      a<-data_frame
      replace <- names(which(table(a) >= mincount & table(a) < maxcount))
      levels(a)[levels(a) %in% replace] <-paste("Other2",colnames(data_frame),sep=".")
      data_frame<-a 
    return(data_frame) }
  
  df$City_up2<-combinerarecategoriesbetween(df$City_up2,51,75) 
  
  
  combinerarecategoriesbetween<-function(data_frame,mincount,maxcount){ 
      a<-data_frame
      replace <- names(which(table(a) >= mincount & table(a) < maxcount))
      levels(a)[levels(a) %in% replace] <-paste("Other3",colnames(data_frame),sep=".")
      data_frame<-a 
    return(data_frame) }
  
  df$City_up2<-combinerarecategoriesbetween(df$City_up2,76,100)
  
  #normalize
  p <- preProcess(subset(df, select = -Func_Burdened_Cost), method = c('YeoJohnson'))
  dfYeoJohnson <- predict(p, df) 
  Func_Burdened_CostLog <-  log(df$Func_Burdened_Cost)
 dfYJ <- cbind(Func_Burdened_CostLog,subset(dfYeoJohnson, select = -Func_Burdened_Cost)) 

dim(dfYJ)
dfYJ <- (filter(dfYJ, exp(Func_Burdened_CostLog) >=100 & exp(Func_Burdened_CostLog)<=1000))

dfYJWD <- (filter(dfYJ,ServiceType_Name =="DOORS & WINDOWS")); str(dfYJWD)
dfYJR <- (filter(dfYJ,ServiceType_Name =="ELECTRICAL REPAIRS" | ServiceType_Name =="INTERIOR LIGHTING REPAIRS")); str(dfYJER)
dfYJHMR <- (filter(dfYJ,ServiceType_Name =="HVAC MAINTENANCE" | ServiceType_Name =="HVAC REPAIRS"));summary(dfYJHMR)
dfYJPLUMB <- (filter(dfYJ,ServiceType_Name =="PLUMBING MAINTENANCE"));str(dfYJPLUMB)

#cluster
set.seed(123)
fviz_nbclust(training_set[,c("Func_Burdened_CostLog" )], kmeans, method = "wss", k.max=100)
ggsave(file="out/kmeans_sil.png", width=6, height=4)
clusters = kmeans(training_set[,c("Func_Burdened_CostLog" )], 
                  centers=8, 
                  nstart=10)
training_set$kmeans8 = as.factor(clusters$cluster)

#Split data using Random Sampling - train(90%) & test data (10%) - test_data(work order cost !=0) ----
  #library(caTools)
  set.seed(123)
  split <- sample.split(dfYJR$Func_Burdened_CostLog, SplitRatio = 0.49)
  test_set = subset(dfYJR, split == TRUE)
  training_set = subset(dfYJR, split == FALSE)
  any(test_set$Func_Burdened_CostLog==0)  #to check if there are any zero workorder costs
  rm(split)
  # count(df) == count(training_set)+count(test_set)


  
  
#Model building
  #----
  # Residual standard error: 446.8 on 127149 degrees of freedom
  # Multiple R-squared:  0.2569,	Adjusted R-squared:  0.2441 
  # F-statistic: 20.02 on 2196 and 127149 DF,  p-value: < 2.2e-16
  # > linear_rmse [1] 443.0015
  linear_model <- glm(Func_Burdened_CostLog ~ LeaseInd + Property_Usage + Province +  
                        Rentable_SQFT + Parent_WorkOrderNbr + ServiceType_Name + 
                        Estimated_Time_Days + off_by_days + 
                        Vendor_Key_pred + ServiceType_Name+ 
                        WorkOrderSource_Cd + WorkOrderType_Cd + 
                         City_up2 + doc_lengths + t_1 + 
                        t_2 + t_3 + t_4 + t_5 + t_6 + t_7 + t_8 + t_9 + t_10 + t_11 + 
                        t_12 + t_13 + t_14 + t_15 + t_16 + t_17 + t_18 + t_19 + t_20 + 
                        Creation_day + TargetCompletion_day +  
                        Estimated_time_needed +  WeekendCompletion + 
                        Month + ColdWeather + Hour + MorningServiceRequest + OvernightServiceRequest, data = training_set)
  summary(linear_model)
  coef(linear_model)
  
  ## Stepwise regressions. 
  linear_model_stepwiseAIC<-stepAIC(linear_model,direction = c("both"),trace = 1) #AIC stepwise
  summary(linear_model_stepwiseAIC) 
  
  
  varImp(linear_model_stepwiseAIC)
  
  
  
  
  # prediction
  linear_pred <- (predict(linear_model_stepwiseAIC, data = test_set))
  linear_rmse <- rmse((test_set$Func_Burdened_CostLog), linear_pred); linear_rmse
  Step_percent.errors <- mean(abs((test_set$Func_Burdened_CostLog-linear_pred)/test_set$Func_Burdened_CostLog)*100); Step_percent.errors
  
  comparing <- cbind(linear_pred, exp(test_set$Func_Burdened_CostLog))
   # plot predicted vs actual 
  plot(linear_pred, training_set$Func_Burdened_Cost, ylab = "Actual", xlab = "Predicted")
  abline(a=0,b=1)
  # compare with summary of costs in train
  summary(training_set$Func_Burdened_Cost)
  # check out the regression plots 
  plot(linear_model)
  
  Vendor_Key_model <- rpart( Vendor_Key ~ LeaseInd + Property_Usage + Province +  
                         Rentable_SQFT + Parent_WorkOrderNbr +  
                         Estimated_Time_Days + off_by_days + 
                         WorkOrderSource_Cd +  WorkOrderType_Cd + 
                         City_up2 + doc_lengths + t_1 + ServiceType_Name+
                         t_2 + t_3 + t_4 + t_5 + t_6 + t_7 + t_8 + t_9 + t_10 + t_11 + 
                         t_12 + t_13 + t_14 + t_15 + t_16 + t_17 + t_18 + t_19 + t_20 + 
                         Creation_day + TargetCompletion_day +  
                         Estimated_time_needed +  WeekendCompletion + 
                         Month + ColdWeather + Hour + MorningServiceRequest + OvernightServiceRequest,method = "class",data = training_set)
  rpart.plot(Vendor_Key_model)

  # prediction
#install.packages("rpart.plot")
#library(rpart.plot)
  Vendor_Key_pred <- predict(Vendor_Key_model, data = test_set, type ="class")
confusionMatrix(data=Vendor_Key_pred, reference = training_set$Vendor_Key)  

  

# ----
grid <- expand.grid( interaction.depth=c(1,3), shrinkage = c(.05,.1), n.minobsinnode=c(10,30))
train.control <- trainControl(
  method = "cv",
  number = 10, ## 10-fold CV
  classProbs = FALSE
)
mod_glm1 <-  caret::train (Func_Burdened_CostLog ~ LeaseInd + Property_Usage + Province + 
                             Rentable_SQFT + Parent_WorkOrderNbr + ServiceType_Name + 
                             Estimated_Time_Days + off_by_days + Vendor_Key + WorkOrderSource_Cd + 
                             WorkOrderType_Cd + WorkOrder_Priority_Desc + doc_lengths + 
                             t_1 + t_3 + t_5 + t_7 + t_8 + t_9 + t_10 + t_13 + t_14 + 
                             t_15 + t_16 + t_18 + t_19 + TargetCompletion_day + Estimated_time_needed + 
                             WeekendCompletion, data = dfYJ,  method = "glm", metric="RMSE", trControl = train.control, tuneGrid = grid)




  
  #create the y variable and matrix (capital X) of x variables (will make the code below easier to read + will ensure that all interactoins exist)
  y<-dfYeoJohnson$Func_Burdened_Cost
  X<-model.matrix(Func_Burdened_Cost ~ LeaseInd + Property_Usage + Province +  
                    Rentable_SQFT + Parent_WorkOrderNbr + ServiceType_Name + 
                    Estimated_Time_Days + off_by_days + 
                    Vendor_Key +  
                    WorkOrderSource_Cd + WorkOrderStatus_Desc + WorkOrderType_Cd + 
                    WorkOrder_Priority_Desc + City_up2 + doc_lengths + t_1 + 
                    t_2 + t_3 + t_4 + t_5 + t_6 + t_7 + t_8 + t_9 + t_10 + t_11 + 
                    t_12 + t_13 + t_14 + t_15 + t_16 + t_17 + t_18 + t_19 + t_20 + 
                    Creation_day + TargetCompletion_day +  
                    Estimated_time_needed +  WeekendCompletion + 
                    Month + ColdWeather + Hour + MorningServiceRequest + OvernightServiceRequest, data = dfYeoJohnson )[,-1]

  # split X into testing, trainig/holdout and prediction as before
  train.rows<- createDataPartition(y= dfYeoJohnson$Func_Burdened_Cost, p=0.1, list = FALSE)
  X.training<- X[train.rows,]
  X.testing<- X[-train.rows,]
  y.training<- y[train.rows,]
  y.testing<- y[-train.rows,]
 # X.training<-subset(X,X[,1]<=12000)
 # X.testing<-subset(X,X[,1]>=12001)
  y.training<-head(y,14373)
  y.testing<-subset(y,y[12001:.,])
  
  
  #LASSO (alpha=1)
  lasso.fit<-glmnet(x = X.training,  y.training, alpha = 1)
  plot(lasso.fit, xvar = "lambda")
  
  #selecting the best penalty lambda
  crossval <-  cv.glmnet(x = X.training, y = y.training, alpha = 1) #create cross-validation data
  plot(crossval)
  penalty.lasso <- crossval$lambda.min #determine optimal penalty parameter, lambda
  log(penalty.lasso) #see where it was on the graph
  lasso.opt.fit <-glmnet(x = X.training, y = y.training, alpha = 1, lambda = penalty.lasso) #estimate the model with the optimal penalty
  summary(lasso.opt.fit)
  coef(lasso.opt.fit) #resultant model coefficients
  predict(lasso.opt.fit,s = penalty.lasso,type = "coefficients")
  # predicting the performance on the testing set
  lasso.testing <- (predict(lasso.opt.fit, s = penalty.lasso, newx =X.training))
  mean(abs(lasso.testing-y.training)/y.training) #calculate and display MAPE
  
  #ridge (alpha=0)
  ridge.fit<-glmnet(x = X.training, y = y.training, alpha = 0)
  plot(ridge.fit, xvar = "lambda")
  
  
  
  
  #selecting the best penalty lambda
  crossval <-  cv.glmnet(x = X.training, y = y.training, alpha = 0)
  plot(crossval)
  penalty.ridge <- crossval$lambda.min 
  log(penalty.ridge) 
  ridge.opt.fit <-glmnet(x = X.training, y = y.training, alpha = 0, lambda = penalty.ridge) #estimate the model with that
  coef(ridge.opt.fit)
  
  ridge.testing <- exp(predict(ridge.opt.fit, s = penalty.ridge, newx =X.training))
  mean(abs(ridge.testing-y.training)/y.training*100) 
  
  
  
  
  # prediction
  linear_pred <- predict(linear_model_stepwiseAIC, data = test_set)
  linear_rmse <- rmse(training_set$Func_Burdened_Cost, linear_pred); linear_rmse
  # plot predicted vs actual 
  plot(linear_pred, training_set$Func_Burdened_Cost, ylab = "Actual", xlab = "Predicted")
  abline(a=0,b=1)
  # compare with summary of costs in train
  summary(training_set$Func_Burdened_Cost)
  # check out the regression plots 
  plot(linear_model)
  
  
  #Classification - test ----
  # classifier <- rpart(formula = WorkOrder_Priority_Desc ~ .,
  #                     data = training_set)
  # y_pred <- predict(classifier, test_set[8])
  # plot(classifier); text(classifier)
  
  # df4reg <- training_set[1:5000,]     #SUBSET TO BE REMOVED AFTER MODEL FIT
  #LM Step regression:   ----
  reg_StepModel <- step(lm(Func_Burdened_Cost ~ ., data=training_set, na.action=na.exclude), 
                  direction = "forward")
    summary(reg_StepModel)
    summary(reg)$adj.r.squared
    #RMSE: R2: 0.2425572, RMSE:443.0015
    reg_StepModel_pred <- predict(reg_StepModel, data = test_set)
    reg_StepModel_rmse <- rmse(training_set$Func_Burdened_Cost, reg_StepModel_pred); reg_StepModel_rmse
    #MAPE: 133.7504 - since data is skewed, MAPE is not the best evaluator
    Step_percent.errors <- abs((test_set$Func_Burdened_Cost-reg_StepModel_pred)/test_set$Func_Burdened_Cost)*100
    mean(Step_percent.errors) #display Mean Absolute Percentage Error (MAPE)
    

#Manual feature selection  ----
  #Not included:(creates large size vector) Creation_Date+Completion_Date+TargetCompletion_Date
  #Not included:(getting NA in reg result) off_by_days, TargetCompletion_time, 
              #Estimated_time_needed, EstimedVSactual_time_needed, WeekendCompletion, ColdWeather
              #MorningServiceRequest +OvernightServiceRequest
  #Non significants: Completion_timedate, Actual_time_needed, Month, MorningServiceRequest, 
                    #OvernightServiceRequest
                    #Completion_time+Creation_timedate+TargetCompletion_timedate
                    #WorkOrderStatus_Desc+  Creation_day +Completion_day +Target +Month +ColdWeather
  #if ServiceProvider_Class = Vendor, we have vendor key, if ServiceProvider_Class = Landlord, V_key=0
  #R2:0.2425572, RMSE: 443.5
  reg <- lm(formula = (Func_Burdened_Cost) ~ 
            LeaseInd +Property_Usage +Province +Region_Name +Rentable_SQFT
            +Parent_WorkOrderNbr +ServiceType_Name +WorkOrderSource_Cd
            +WorkOrderType_Cd +WorkOrder_Priority_Desc
            +doc_lengths+Hour     #very margninal improvement with this
            +t_1+t_2+t_3+t_4++t_5+t_6+t_7+t_8+t_9+t_10
            +t_11+t_12+t_13+t_14++t_15+t_16+t_17+t_18+t_19+t_20
            +Work_Duration_Days +Estimated_Time_Days
            +ServiceProvider_Class +ServiceProvider_Type      
            +Vendor_Key+City_up2 +BuildingIDKey
            ,data=training_set, na.action=na.exclude)
  summary(reg)
  
  summary(reg)$adj.r.squared
  reg_pred <- predict(reg, data = test_set)
  reg_rmse <- rmse(training_set$Func_Burdened_Cost, reg_pred); reg_rmse
  
  # summary(reg_log)$adj.r.squared
  # reg_pred_log <- exp(predict(reg_log, data = test_set))
  # reg_rmse_log <- rmse(training_set$Func_Burdened_Cost, reg_pred_log); reg_rmse_log

  
  par(mfrow=c(2,2))
  plot(density(resid(reg)))
  par(mfrow=c(1,1))
  str(df$ServiceProvider_Class)
  +++Region_Name+
    ServiceProvider_Class+ServiceProvider_Type+
    WorkOrderSource_Cd+WorkOrderStatus_Desc+WorkOrderType_Cd+WorkOrder_Priority_Desc+
    City_up2+doc_lengths+
    BuildingIDKey+
    Creation_day+Completion_day+TargetCompletion_day+Hour+doc_lengths
  
  
  
  
  #Backward elimination ----  
  #The funcion declaration - x=data, sl=significance level
  backwardElimination <- function(x, sl) {
    numVars = length(x)
    for (i in c(1:numVars) ){
      regressor = lm(formula = Func_Burdened_Cost ~ ., data = x)
      maxVar = max(coef(summary(regressor))[c(2:numVars), "Pr(>|t|)"])
      if (maxVar > sl){
        j = which(coef(summary(regressor))[c(2:numVars),"Pr(>|t|)"]==maxVar)
        x = x[,-j]
      }
      numVars = numVars - 1
    }
    return(summary(regressor))
  }
  
  #Input Variables Selection
  selcol <- c("Func_Burdened_Cost",
              "LeaseInd",
              "Region_Name",
              "Rentable_SQFT",
              "Parent_WorkOrderNbr",
              "ServiceType_Name",
              "Work_Duration_Days",
              "Estimated_Time_Days",
              "ServiceProvider_Class",
              "ServiceProvider_Type",
              "WorkOrderSource_Cd",
              "WorkOrderStatus_Desc",
              "WorkOrderType_Cd",
              "WorkOrder_Priority_Desc",
              "TargetCompletion_day",
              "Hour",
              "City_up2",
              "doc_lengths")
  dataset <- training_set[, (colnames(training_set) %in% selcol)]
  rm(selcol)
  
  #Parameters set and backward selection funciton call
  SL = 0.05
  backwardElimination(dataset, SL)
  summary(df$Func_Burdened_Cost)
  histogram(log(df$Func_Burdened_Cost))
  range(df$Func_Burdened_Cost)
  
  
  
  # ----
  grid <- expand.grid(n.trees=c(35,100,300), interaction.depth=c(1,3), shrinkage = c(.05,.1), n.minobsinnode=c(10,30))
  train.control <- trainControl(
    method = "cv",
    number = 10, ## 10-fold CV
    summaryFunction = twoClassSummary,
    classProbs = TRUE
  )
  mod_glm1 <-  caret::train ("Func_Burdened_Cost" ~., data = df,  method = "glm", metric="RMSE", trControl = train.control, tuneGrid = grid)
  
