setwd('D:/我的文档/文档/复旦大学/大四/专题讲座下/Project1');
Hour = read.table("hour.csv", header=T, sep=",", na.strings="?");
Hour = na.omit(Hour);
col_num = ncol(Hour);
data_num = length(Hour[,1]);
colname = colnames(Hour);
names(Hour);

##############################################################
# fix the dataset
HourFix = Hour[,c(11:14, 7, 9)];

HourFix["weather1"] = (Hour[,10] == 1) * 1;
HourFix["weather2"] = (Hour[,10] == 2) * 1;
HourFix["weather3"] = (Hour[,10] == 3) * 1;
HourFix["weather4"] = (Hour[,10] == 4) * 1;

HourFix["weakday0"] = (Hour[,8] == 0) * 1;
HourFix["weakday1"] = (Hour[,8] == 1) * 1;
HourFix["weakday2"] = (Hour[,8] == 2) * 1;
HourFix["weakday3"] = (Hour[,8] == 3) * 1;
HourFix["weakday4"] = (Hour[,8] == 4) * 1;
HourFix["weakday5"] = (Hour[,8] == 5) * 1;
HourFix["weakday6"] = (Hour[,8] == 6) * 1;

HourFix["casual"] = Hour[,15];
HourFix["registered"] = Hour[,16];
HourFix["cnt"] = Hour[,17];
##############################################################

#registered
lm.fit=lm(registered~(atemp+hum+windspeed+workingday
                  #+weather1+weather2+weather3+weather4
                  #+weakday0+weakday1+weakday2+weakday3+weakday4
                  #+weakday5+weakday6
                  )
          #+temp:atemp
          #+atemp:hum
          #+I(temp^2)
          #+I(atemp^2)
          #+I(windspeed^2)
          ,data=HourFix);

summary(lm.fit);
parameters = coef(lm.fit);
rmse = 0;
error = 0;

HourData = matrix(nrow = data_num, ncol = 5);
HourData[,1] = 1;
HourData[,2] = HourFix[,2];
HourData[,3] = HourFix[,3];
HourData[,4] = HourFix[,4];
HourData[,5] = HourFix[,6];

ans = matrix(nrow = data_num, ncol = 2);

for (i in 1: data_num) {
  ans[i,1] = HourData[i,] %*% parameters;
  diff = HourFix[i,]$casual - HourData[i,] %*% parameters;
  rmse = rmse + diff ^ 2;
  error = error + abs(diff);
}
rmse = as.numeric(sqrt(rmse / data_num));
error = as.numeric(error / data_num);
##############################################################
#casual
lm.fit=lm(casual~(temp+atemp+hum+windspeed+holiday+workingday
          #+weather1+weather2+weather3
          +weakday0+weakday1+weakday2+weakday3+weakday4)
          +temp:atemp
          +atemp:hum
          +I(temp^2)
          +I(atemp^2)
          +I(windspeed^2)
          ,data=HourFix);

summary(lm.fit);
parameters = coef(lm.fit);
rmse = 0;
error = 0;

HourData = matrix(nrow = data_num, ncol = 17);
HourData[,1] = 1;
HourData[,2] = HourFix[,1];
HourData[,3] = HourFix[,2];
HourData[,4] = HourFix[,3];
HourData[,5] = HourFix[,4];
HourData[,6] = HourFix[,5];
HourData[,7] = HourFix[,6];
#HourData[,8] = HourFix[,7];
#HourData[,9] = HourFix[,8];
#HourData[,10] = HourFix[,9];
HourData[,8] = HourFix[,11];
HourData[,9] = HourFix[,12];
HourData[,10] = HourFix[,13];
HourData[,11] = HourFix[,14];
HourData[,12] = HourFix[,15];
HourData[,13] = HourFix[,1] * HourFix[,1];
HourData[,14] = HourFix[,2] * HourFix[,2];
HourData[,15] = HourFix[,4] * HourFix[,4];
HourData[,16] = HourFix[,1] * HourFix[,2];
HourData[,17] = HourFix[,2] * HourFix[,3];

for (i in 1: data_num) {
  ans[i,2] = HourData[i,] %*% parameters;
  diff = HourFix[i,]$casual - HourData[i,] %*% parameters;
  rmse = rmse + diff ^ 2;
  error = error + abs(diff);
}
rmse = as.numeric(sqrt(rmse / data_num));
error = as.numeric(error / data_num);

##############################################################
rmse = 0;
error = 0;
for (i in 1: data_num) {
  diff = HourFix[i,]$cnt - (ans[i,1] + ans[i,2]);
  rmse = rmse + diff ^ 2;
  error = error + abs(diff);
}
rmse = as.numeric(sqrt(rmse / data_num));
error = as.numeric(error / data_num);
rm(Hour, lm.fit, diff, i);