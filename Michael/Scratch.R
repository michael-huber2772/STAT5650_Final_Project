library(ggplot2)
library(gridExtra)
library(MASS)
library(vioplot)

library(verification)
library(caret)
library(rpart)
library(e1071) # SVM
library(gbm) # GBM


library(klaR)
library(dplyr)

hotel = read.csv("../Data/hotel_bookings.csv")

hotel1 = subset(na.omit(hotel), select = -c(country, agent, company, reservation_status, reservation_status_date))

kappa=function(x){
  n=sum(x)
  pobs=(x[1,1]+x[2,2])/n
  pexp=(sum(x[1,])*sum(x[,1])+sum(x[2,])*sum(x[,2]))/n^2
  kappa=(pobs-pexp)/(1-pexp)
  t1=0
  t2=0
  t3=0
  pii=x/n
  pidot=apply(pii,1,sum)
  pdotj=apply(pii,2,sum)
  for(i in 1:2){
    t1 = t1 + pii[i,i]*((1-pexp) - (1-pobs)*(pidot[i]+pdotj[i]))^2
  }
  t2 = pii[1,2]*(pdotj[1]+pidot[2])^2 + pii[2,1]*(pdotj[2] + pidot[1])^2
  t3 = (pobs*pexp-2*pexp+pobs)^2
  vhat = (t1 + t2*(1-pobs)^2 -t3)/(n*(1-pexp)^4)
  se=sqrt(vhat)
  return(c(kappa,se))
}


class.sum=function(truth,predicted){
  xt=table(truth,round(predicted+0.000001))
  pcc=round(100*sum(diag(xt))/sum(xt),2)
  spec=round(100*xt[1,1]/sum(xt[1,]),2)
  sens=round(100*xt[2,2]/sum(xt[2,]),2)
  kap=round(kappa(xt)[1],4)
  au=round(roc.area(truth,predicted)$A,4)
  return(cbind(c("Percent Correctly Classified = ","Specificity = ","Sensitivity = ","Kappa =","AUC= "),c(pcc,spec,sens,kap,au)))
}

set.seed(5341)
hotel1.rpart=rpart(is_canceled~.,data=hotel1,method="class",control=rpart.control(cp=0.0,minsplit=2))


plotcp(hotel1.rpart)


#### 5 splits ####
hotel1.rpart5=rpart(is_canceled~.,data=hotel1,method="class",control=rpart.control(cp=1.2981e-02,minsplit=2))
plot(hotel1.rpart5, margin=0.1)
text(hotel1.rpart5,use.n=TRUE)



#### CROSS VALIDATION ACCURACY FOR 5 SPLITS####
set.seed(5341)

xvs=rep(c(1:10),length=nrow(hotel1))
xvs=sample(xvs)
hotel1.rpart5.xval=rep(0,length(nrow(hotel1)))
for(i in 1:10){
  train=hotel1[xvs!=i,]
  test=hotel1[xvs==i,]
  rp=rpart(is_canceled~ . 
           ,method="class",data=train,control=rpart.control(cp=0.0,minsplit=2))
  hotel1.rpart5.xval[xvs==i]=predict(rp,test,type="prob")[,2]
}

table(hotel1$is_canceled,round(hotel1.rpart5.xval))
class.sum(hotel1$is_canceled,hotel1.rpart5.xval)


##################################################################################################
                     # Support Vector Machines
##################################################################################################

Hotel = names(hotel) %in% c("hotel")
HotelDataSansHotel= hotel[!Hotel]

par(mar=c(.5,.5,.5,.5))
par(mfrow=c(13,2))
qqnorm(HotelDataSansHotel$lead_time, main="lead_time")
qqnorm(HotelDataSansHotel$arrival_date_year, main="arrival_date_year")
#qqnorm(HotelDataSansHotel$arrival_date_month, main="lead_time")
qqnorm(HotelDataSansHotel$arrival_date_week_number, main="arrival_date_week_number")
qqnorm(HotelDataSansHotel$arrival_date_day_of_month, main="arrival_date_day_of_month")

qqnorm(HotelDataSansHotel$stays_in_weekend_nights, main="stays_in_weekend_nights")
qqnorm(HotelDataSansHotel$stays_in_week_nights, main="stays_in_week_nights")
qqnorm(HotelDataSansHotel$adults, main="adults")
qqnorm(HotelDataSansHotel$children, main="children")
qqnorm(HotelDataSansHotel$babies, main="babies")

#qqnorm(HotelDataSansHotel$meal)
#qqnorm(HotelDataSansHotel$market_segment)
#qqnorm(HotelDataSansHotel$distribution_channel)
qqnorm(HotelDataSansHotel$is_repeated_guest, main="is_repeated_guest")
qqnorm(HotelDataSansHotel$previous_cancellations, main="previous_cancellations")

qqnorm(HotelDataSansHotel$previous_bookings_not_canceled, main="previous_bookings_not_canceled")
#qqnorm(HotelDataSansHotel$reserved_room_type)
#qqnorm(HotelDataSansHotel$assigned_room_type)
qqnorm(HotelDataSansHotel$booking_changes, main="booking_changes")
#qqnorm(HotelDataSansHotel$deposit_type)

qqnorm(HotelDataSansHotel$days_in_waiting_list, main="days_in_waiting_list")
#qqnorm(HotelDataSansHotel$customer_type)
qqnorm(HotelDataSansHotel$adr, main="adr")
qqnorm(HotelDataSansHotel$required_car_parking_spaces, main="required_car_parking_spacese")
qqnorm(HotelDataSansHotel$total_of_special_requests, main="total_of_special_requests")



#subset to include only binomial and numeric variables
nonNumericVariables = names(hotel) %in% c("hotel", "arrival_date_month", "meal", "country", "market_segment", "distribution_channel", "reserved_room_type", "assigned_room_type", "deposit_type", "agent", "company", "customer_type", "reservation_status", "reservation_status_date")
HotelDataSubset=hotel[!nonNumericVariables]
colnames(HotelDataSubset)


library(tidyr)
HotelDataNumericSansNAN = drop_na(HotelDataSubset)


HotelDataNumeric.lr = glm(is_canceled~ . ,family=binomial,data=HotelDataNumericSansNAN)

#confusion matrix and accuracy (Dr. Cutler's way)
table(HotelDataNumericSansNAN$is_canceled,round(predict(HotelDataNumeric.lr,type="response")))
class.sum(HotelDataNumericSansNAN$is_canceled,predict(HotelDataNumeric.lr,type="response"))


HotelDataNumeric.lr = glm(is_canceled~ . ,family=binomial,data=HotelDataNumericSansNAN)
step.model=stepAIC(HotelDataNumeric.lr, direction = "both", trace = FALSE)
summary(step.model)
step.model
class.sum(HotelDataNumericSansNAN$is_canceled,predict(step.model,type="response"))




HotelDataNumeric.lr.xval=rep(0,nrow(HotelDataNumericSansNAN))
xvs=rep(1:10,length=nrow(HotelDataNumericSansNAN))
xvs=sample(xvs)
for(i in 1:10){
  train=HotelDataNumericSansNAN[xvs!=i,]
  test=HotelDataNumericSansNAN[xvs==i,]
  glub=glm(is_canceled~ lead_time + arrival_date_year + arrival_date_week_number + 
             stays_in_weekend_nights + adults + children + babies + is_repeated_guest + 
             previous_cancellations + previous_bookings_not_canceled + 
             booking_changes + days_in_waiting_list + adr + required_car_parking_spaces + 
             total_of_special_requests ,family=binomial,data=train)
  HotelDataNumeric.lr.xval[xvs==i]=predict(glub,test,type="response")
}
table(HotelDataNumericSansNAN$is_canceled,round(HotelDataNumericSansNAN.lr.xval))
class.sum(HotelDataNumericSansNAN$is_canceled,HotelDataNumericSansNAN.lr.xval)
