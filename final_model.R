#### Importing required libraries ###########
library(readr)
library(caTools)
library(ROCR)


#Utility function to get ROC value
getROC <- function(data,model,classification.columnn){
  p <- getPredictedProb(data,model)
  pr <- prediction(p, classification.columnn)
  prf <- performance(pr, measure = "tpr", x.measure = "fpr")
  auc <- performance(pr, measure = "auc")
  auc@y.values[[1]]
}

#Function to get the predicted probability
getPredictedProb <- function(data,model){
  predict(model, newdata=data, type="response")
}

#Function to Create Categorical Variables by replacing the NA's with the modes of that column
createFactor <- function(column){
  column[is.na(column)] <- names(table(column)[which.max(table(column))])
  factor(column,levels = unique(column))
}

#Function to read and clean the data
read_data <-function(file.Location){
  ip = read_csv(file.Location,col_types = cols(ni.age = col_double()))
  
  # Treating Outliers using Mahalanobis Distance 
  ip=if(checkColumn(ip,"cancel")){
      removeOutliers(ip)
   }else{
      replaceOutliers(ip)
   }
   
  # Treating Missing Continuous Values
  ip$tenure[is.na(ip$tenure)] <- mean(ip$tenure,na.rm=T)
  ip$ni.age[is.na(ip$ni.age)] <- mean(ip$ni.age,na.rm=T)
  ip$len.at.res[is.na(ip$len.at.res)] <- mean(ip$len.at.res,na.rm=T)
  ip$premium[is.na(ip$premium)] <- mean(ip$premium,na.rm=T)
  
  # Create factors and handle extreme cases
  ip$claim.ind=createFactor(ip$claim.ind)
  ip$n.adults[ip$n.adults==9] <- names(table(ip$n.adults)[which.max(table(ip$n.adults))])
  ip$n.adults=createFactor(ip$n.adults)
  ip$n.children=createFactor(ip$n.children)
  ip$ni.gender=createFactor(ip$ni.gender)
  ip$ni.marital.status=createFactor(ip$ni.marital.status)
  ip$sales.channel=createFactor(ip$sales.channel)
  ip$coverage.type=createFactor(ip$coverage.type)
  ip$dwelling.type[ip$dwelling.type=="Landlord"] <- names(table(ip$dwelling.type)[which.max(table(ip$dwelling.type))])
  ip$dwelling.type=createFactor(ip$dwelling.type)
  ip$credit=createFactor(ip$credit)
  ip$house.color=createFactor(ip$house.color)
  ip$year=createFactor(ip$year)
  ip$zip.code=createFactor(ip$zip.code)
  if(checkColumn(ip,"cancel")){
    ip = ip[(ip$cancel == 0 | ip$cancel == 1 ),]
    ip$cancel=createFactor(ip$cancel)
  }
  ip
}


#Funtcion to remove the outliers in the continuous columns
removeOutliers <- function(ip){
  na_rows=ip[(is.na(ip$tenure) | is.na(ip$ni.age) | is.na(ip$len.at.res) | is.na(ip$premium)),]
  ip=ip[!(is.na(ip$tenure) | is.na(ip$ni.age) | is.na(ip$len.at.res) | is.na(ip$premium)),]
  continuous_cols=ip[,c(2,8,12,15)]
  m_dist <- mahalanobis(continuous_cols, colMeans(continuous_cols,na.rm = T),cov(continuous_cols,use = "complete.obs"))
  ip$MD <- round(m_dist, 1)
  # Binary Outlier Variable
  ip=ip[ip$MD < 14,]
  ip=ip[,-c(length(colnames(ip)))]
  rbind(ip,na_rows)
}


#Funtcion to replace the outliers in the continuous columns
replaceOutliers <- function(ip){
  na_rows=ip[(is.na(ip$tenure) | is.na(ip$ni.age) | is.na(ip$len.at.res) | is.na(ip$premium)),]
  ip=ip[!(is.na(ip$tenure) | is.na(ip$ni.age) | is.na(ip$len.at.res) | is.na(ip$premium)),]
  continuous_cols=ip[,c(2,8,12,15)]
  m_dist <- mahalanobis(continuous_cols, colMeans(continuous_cols,na.rm = T),cov(continuous_cols,use = "complete.obs"))
  ip$MD <- round(m_dist, 1)
  # Binary Outlier Variable
  outliers=ip[ip$MD >= 14,]
  ip=ip[ip$MD < 14,]
  # Treating Outlier Values 
  outliers[outliers$tenure > max(ip$tenure),"tenure"]=mean(ip$tenure,na.rm=T)
  outliers[outliers$ni.age > max(ip$ni.age),"ni.age"]=mean(ip$ni.age,na.rm=T)
  outliers[outliers$len.at.res > max(ip$len.at.res),"len.at.res"]=mean(ip$len.at.res,na.rm=T)
  outliers[outliers$premium > max(ip$premium),"premium"]=mean(ip$premium,na.rm=T)
  ip=rbind(ip,outliers)
  ip=ip[,-c(length(colnames(ip)))]
  rbind(ip,na_rows)
}

#Function to extract city and state form zipcode
transform.Zipcode.to.Location <- function(data){
  library(zipcode)
  data("zipcode")
  getState <- function(x){
    zipcode[zipcode$zip==x,]$state
  }
  getCity <- function(x){
    zipcode[zipcode$zip==x,]$city
  }
  code.column=data.frame("zip"=data$zip.code,"seq"=data$id)
  unique.code=data.frame("zip"=unique(code.column$zip))
  unique.locations=data.frame("zip"=unique.code,"city"=apply(unique.code,1,getCity),"state"=apply(unique.code,1,getState))
  locations=merge(unique.locations[, c("zip", "city","state")], code.column[, c("zip","seq")], by="zip")
  locations = data.frame("seq"=locations$seq,locations$zip,"Location"=paste(locations$city,locations$state, sep=","))
  locations=locations[order(locations$seq),]
  data$location = locations$Location
  data
}


# Function to remove id, zipcode and year columns which would not be used in the model
removeExtraCols <- function(data){subset(data, select = -c(zip.code,id,year) )}

# Function to check if a column exists in a dataframe
checkColumn <- function(df,column.name){ ifelse(column.name %in% colnames(df),TRUE,FALSE)}

#Read and Clean Training Data
clean.data=removeExtraCols(transform.Zipcode.to.Location(read_data("E:/Travelers/Train.csv")))

# Read and Treat test data
test.data.with.all.columns=transform.Zipcode.to.Location(read_data("E:/Travelers/Test.csv"))
# Remove unused columns from test data
test.data=removeExtraCols(test.data.with.all.columns)

#write.csv(clean.data,"Cleaned_data.csv")
set.seed(59) # To ensure that the train and validation splits are created in the same way always 
split <- sample.split(clean.data, SplitRatio = 0.75)

#get training and validation data
train_set <- subset(clean.data, split == TRUE) 
validation_set <- subset(clean.data, split == FALSE) 

# varaible selection was done using forward selection based on minimum AIC value
logit.model=glm(formula = train_set$cancel ~ location + credit + sales.channel + 
          n.children + ni.age + claim.ind + tenure + len.at.res + ni.marital.status + 
          n.adults, family = binomial("logit"), data = train_set)

# Null and Full Models used for variable selection
# creating null model
#null_model=glm(train_set$cancel~location,data = train_set,family = binomial("logit"))
#summary(null_model)
# creating full model
#full_model=glm(train_set$cancel~.,data = train_set,family = binomial("logit"))
#summary(full_model)
#summary(logit.model)
# Command used for forward variable selection based on minimum AIC value
#step(null_model,data=train_set,scope = list(lower=null_model,upper=full_model),direction = "forward")


validation.ROC=getROC(validation_set,logit.model,validation_set$cancel)
sub.train.ROC=getROC(train_set,logit.model,train_set$cancel)
full.train.ROC=getROC(clean.data,logit.model,clean.data$cancel)
sub.train.ROC
validation.ROC 
full.train.ROC

test.output = data.frame("id"=test.data.with.all.columns$id,"cancel"=getPredictedProb(test.data,logit.model))
#write.csv(test.output,"final_test_op.csv")

