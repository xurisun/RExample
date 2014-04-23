setwd('D:/我的文档/文档/复旦大学/大四/专题讲座下/Project1');
Hour = read.table("hour.csv", header=T, sep=",", na.strings="?");
Hour = na.omit(Hour);
col_num = ncol(Hour);
data_num = length(Hour[,1]);
colname = colnames(Hour);
names(Hour);

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
##############################################################
ansMatrix = matrix(nrow = data_num, ncol = 2);
#registered
lm.fit=lm(registered~(temp+atemp+hum+windspeed
                      +season+year+hour
                      +holiday+weekday+weather
                  )
          ,data=HourFix);

summary(lm.fit);
parameters = coef(lm.fit);

tempAns = predict(lm.fit);
tempAns = as.matrix(tempAns);
ansMatrix[,1] = tempAns;
##############################################################
#casual
lm.fit=lm(casual~(temp+atemp+hum+windspeed
                      +season+year+month+hour
                      +holiday+weekday+weather
)
          ,data=HourFix);

summary(lm.fit);
parameters = coef(lm.fit);

tempAns = predict(lm.fit);
tempAns = as.matrix(tempAns);
ansMatrix[,2] = tempAns;
##############################################################
rmse = 0;
error = 0;
for (i in 1: data_num) {
  diff = HourFix[i,]$cnt - (ansMatrix[i,1] + ansMatrix[i,2]);
  rmse = rmse + diff ^ 2;
  error = error + abs(diff);
}
rmse = as.numeric(sqrt(rmse / data_num));
error = as.numeric(error / data_num);

rm(Hour, lm.fit, diff, i, tempAns);