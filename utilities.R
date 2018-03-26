
fwd=step(null_model,data=train_set,scope = list(lower=null_model,upper=full_model),direction = "forward")
summary(fwd)

back=step(full_model,data=train_set,direction="backward")
summary(back)

both=step(null_model, scope = list(upper=full_model), data=train_set, direction="both")
summary(both)

test_model = glm(train_set$cancel~train_set$credit+train_set$location+train_set$sales.channel+train_set$n.children+
                   train_set$ni.age+train_set$claim.ind+train_set$ni.marital.status+train_set$tenure+train_set$coverage.type+
                   train_set$n.adults+train_set$len.at.res,family = binomial)

test_model = glm(cancel~credit+location+sales.channel+n.children+ni.age+claim.ind+ni.marital.status+tenure+coverage.type+n.adults+len.at.res,
                 data=train_set,family = binomial("logit"))

attach(train_set)
locations = data.frame("seq"=locations$seq,locations$zip,"Location"=paste(locations$city,locations$state, sep=","))
library(gdata)
library(rms)
lrm.full <- rms::lrm(full_model, data = train_set)
test=fastbw(lrm.full, rule = "p", sls = 0.1)
summary(test)
fastbw(lrm.full)

test_data=data.frame("credit"=validation_set$credit,"location"=validation_set$location,"sales.channel"=validation_set$sales.channel,"n.children"=validation_set$n.children,
                     "ni.age"=validation_set$ni.age,"claim.ind"=validation_set$claim.ind,"ni.marital.status"=validation_set$ni.marital.status,"tenure"=validation_set$tenure,
                     "coverage.type"=validation_set$coverage.type,"n.adults"=validation_set$n.adults,"len.at.res"=validation_set$len.at.res,"cancel"=validation_set$cancel)

library(ROCR)
p <- predict(full_model, newdata=validation_set, type="response")
pr <- prediction(p, validation_set$cancel)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

summary(train_set$n.adults)
summary(validation_set$n.adults)

ip = read_csv("E:/Travelers/Train.csv",col_types = cols(ni.age = col_double()))
summary(train_set)
summary(validation_set)

clean.data=ip
names(table(clean.data$claim.ind)[which.max(table(clean.data$claim.ind))])
summary(clean.data)
clean.data[clean.data$id==1436,]
clean.data[clean.data$id==361,]$cancel
clean.data[is.na(clean.data$id),]
sapply(clean.data,function(x) sum(is.na(x)))

# 
# #mahalnobis scratchpad
# df2=ip[,c(2,8,12,15)]
# plot(ip$ni.age)
# plot(df2)
# m_dist <- mahalanobis(df2, colMeans(df2,na.rm = T),cov(df2,use = "complete.obs"))
# df2$MD <- round(m_dist, 1)
# # Binary Outlier Variable
# df2=df2[df2$MD < 12.5,]
# plot(df2$ni.age)
# boxplot(ip[,c(2,12,15)])
# boxplot(df2[-2])
# max(df2$ni.age,na.rm = T)
# 
# plot(df2$tenure)
# max(df2$tenure,na.rm = T)
# 
# plot(df2$premium)
# max(df2$premium,na.rm = T)
# 
# plot(df2$len.at.res)
# max(df2$len.at.res,na.rm = T)



#Backward
full_model=glm(train_set$cancel~.,data = train_set,family = binomial)

stats=data.frame(summary(full_model)$coefficients)
stats=data.frame("Columns"=row.names(stats),stats)
row.names(stats)=seq(1,nrow(stats),1)
stats=stats[seq(2,nrow(stats),1),]
max.p.column=stats[stats$Pr...z.. ==max(stats$Pr...z..),c(1,5)]
new_cols=as.character(stats[stats$Columns != as.character(max.p.column$Columns),]$Columns)



stats$coefficients

full_model$coefficients
coef(full_model)
coef(summary(full_model))[,4]

lapply(full_model, summary)




library(readr)
final_test_prob <- read_csv("E:/Travelers/final_test_prob.csv")
test=data.frame(test.output$id, test.output$cancel,final_test_prob$cancel)
colnames(test)=c("x","y","z")
test=data.frame(test,"a"=round(test$y-test$z,3))
test=data.frame(test,"new"=ifelse(test$y>=0.5,1,0),"old"=ifelse(test$z>=0.5,1,0))
test=data.frame(test,"bool"=xor(test$old,test$new))
nrow(test[test$bool,])*100/nrow(test)