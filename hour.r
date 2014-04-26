setwd('D:/我的文档/文档/复旦大学/大四/专题讲座下/Project1');
Hour = read.table("hour.csv", header=T, sep=",", na.strings="?");
Hour = na.omit(Hour);
col_num = ncol(Hour);
data_num = length(Hour[,1]);
colname = colnames(Hour);
names(Hour);

corelation = cor(Hour[,3:14]);

##############################################################
# fix the dataset
HourFix = Hour[,c(11:14)];

HourFix["season"] = as.factor(Hour$season);
HourFix["year"] = as.factor(Hour$yr);
HourFix["month"] = as.factor(Hour$mnth);
HourFix["hour"] = as.factor(Hour$hr);
HourFix["holiday"] = as.factor(Hour$holiday);
HourFix["weekday"] = as.factor(Hour$weekday);
HourFix["workingday"] = as.factor(Hour$workingday);
HourFix["weather"] = as.factor(Hour$weathersit);

HourFix["casual"] = Hour[,15];
HourFix["registered"] = Hour[,16];
HourFix["cnt"] = Hour[,17];

lm.fit3=lm(cnt~(temp+atemp+hum+windspeed
                       +season+year+month+hour
                       +holiday+weekday+workingday+weather)
           ,data=HourFix);
anova(lm.fit3);
confint(lm.fit3);
summary(lm.fit3);
predict(lm.fit3,data.frame(temp=(c(0.2,0.25,0.3)),
                           atemp=(0.25), 
                            hum=(0.5), 
                            windspeed=(0.3),
                           season=as.factor(4),
                           year=as.factor(1),
                           month=as.factor(12),
                           holiday=as.factor(1),
                           hour=as.factor(23),
                           weekday=as.factor(1),
                           workingday=as.factor(1),
                           weather=as.factor(1)), 
        interval="prediction");

##############################################################
ansMatrix = matrix(nrow = data_num, ncol = 2);
#registered
lm.fit1=lm(registered~(temp+atemp+hum+windspeed
                  +season+year+month+hour
                  +holiday+weekday+workingday+weather)
          *
            (temp+atemp+hum+windspeed
             +season+year+month+hour
             +holiday+weekday+workingday+weather)
          -atemp:windspeed-atemp:year-hum:holiday
          +I(temp^2)+I(atemp^2)+I(hum^2)+I(windspeed^2)
          +I(temp^3)+I(atemp^3)+I(hum^3)
          +I(temp^4)+I(hum^4)
          ,data=HourFix);

summary(lm.fit1);
parameters = coef(lm.fit1);

tempAns = predict(lm.fit1);
tempAns = as.matrix(tempAns);
ansMatrix[,1] = tempAns;
##############################################################
#casual
lm.fit2=lm(casual~(temp+atemp+hum+windspeed
               +season+year+month+hour
               +holiday+weekday+workingday+weather)
          *
            (temp+atemp+hum+windspeed
             +season+year+month+hour
             +holiday+weekday+workingday+weather)
          -atemp:windspeed-atemp:year-hum:holiday
          +I(temp^2)+I(atemp^2)+I(hum^2)+I(windspeed^2)
          +I(temp^3)+I(atemp^3)+I(hum^3)
          +I(temp^4)+I(hum^4)
          ,data=HourFix);

summary(lm.fit2);
parameters = coef(lm.fit2);

tempAns = predict(lm.fit2);
tempAns = as.matrix(tempAns);
ansMatrix[,2] = tempAns;
##############################################################
lm.fit=lm(cnt~(temp+atemp+hum+windspeed
                  +season+year+month+hour
                  +holiday+weekday+workingday+weather)
              *
              (temp+atemp+hum+windspeed
                 +season+year+month+hour
                 +holiday+weekday+workingday+weather)
              -atemp:windspeed-atemp:year-hum:holiday
              +I(temp^2)+I(atemp^2)+I(hum^2)+I(windspeed^2)
              +I(temp^3)+I(atemp^3)+I(hum^3)
              +I(temp^4)+I(hum^4)
          ,data=HourFix);
#sink("record.lis");
anova(lm.fit);
#sink();
summary(lm.fit);
##############################################################