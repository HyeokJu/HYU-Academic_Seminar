#데이터 불러오기
library(readr)
data <- read_csv("data_unified_scaled_minus1.csv")
summary(data)

#Train/Test data split
data_train = head(data, 107)
data_test = tail(data, 17)

#다중회귀모델 생성
lm_1 = lm(ASPI_t0 ~ ASPI_t1 + ALPI_t1 + Liquidity_t1 + IR_t1 + LEI_t1,data=data_train)
summary(lm_1)

#다중공선성 확인(상태지수 검정)
install.packages('car')
library(car)
vif(lm_1)

#유동성 제외 후 다중회귀모델 생성
lm_2 = lm(ASPI_t0 ~ ASPI_t1 + ALPI_t1 + IR_t1 + LEI_t1, data=data_train)
summary(lm_2)
vif(lm_2)

#경기선행지수 제외 후 다중회귀모델 생성
lm_3 = lm(ASPI_t0 ~ ASPI_t1 + ALPI_t1 + IR_t1, data=data_train)
vif(lm_3)
summary(lm_3)

#잔차분석
install.packages('lmtest')
library(lmtest)
dwtest(lm_3) #잔차의 독립성 확인

shapiro.test(lm_3$residuals) #잔차의 정규성 확인

bptest(lm_3) #잔차의 등분산성 확인

mae = function (x,y){
  return(mean(abs(x-y)))
}
rmse = function (x,y){
  return(sqrt(mean((x-y)^2)))
}

#test 값 예측
predict(lm_3, data_test)
mae(predict(lm_3, data_test), data_test$ASPI_t0)
rmse(predict(lm_3, data_test), data_test$ASPI_t0)
conf_interval=predict(lm_3, data_test, interval = 'confidence') #예측값 신뢰구간
write.csv(conf_interval, 'conf_interval_rm.csv')

#ARIMA 모델
install.packages('forecast')
library(forecast)
res=lm_3$residuals
res[108:124]=data_test$ASPI_t0-predict(lm_3, data_test)
conf_interval_ARIMA = matrix(ncol=3, nrow=17)

for(i in 1:17){
  res_data = res[0+i : 106+i]
  model = arima(res_data, order=c(2, 0, 1))
  result = forecast(model, h=1) 
  conf_interval_ARIMA[0+i,1] = conf_interval[0+i,1]+result$mean[1] 
  conf_interval_ARIMA[0+i,2] = conf_interval[0+i,2]+result$lower[2] 
  conf_interval_ARIMA[0+i,3] = conf_interval[0+i,3]+result$upper[2]
}
mae(conf_interval_ARIMA[,1], data_test$ASPI_t0)
rmse(conf_interval_ARIMA[,1], data_test$ASPI_t0)

write.csv(predict(lm_3, data), 'predict.csv')
write.csv(conf_interval_ARIMA, 'conf_interval_arima.csv')
write.csv(res, 'res.csv')

