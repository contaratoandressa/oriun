####### SVM #######

# atualizar
# portfolio = c("BTC-USD","ETH-USD","LTC-USD")
# getSymbols(portfolio, src="yahoo", from="2017-01-01")
# prices.data <- merge.zoo(`BTC-USD`[,6], `ETH-USD`[,6], `LTC-USD`[,6])

# in case of package problem

# suppressMessages(library(forecast))
# suppressMessages(library(tseries))
# suppressMessages(library(quantmod))
# suppressMessages(library(caret))
# suppressMessages(library(nnet))

import_data <- function(data_init = '2000-01-01', data_end = '2020-03-31'){
  
  #' import_data = input: data, return: times series for analysis
  #' @param data_init: data of the dataset from start the time series
  
  # Obtendo os dados de ações de empresas listadas na bolsa de valores americana
  #getSymbols(c("MSFT", "SBUX", "IBM", "AAPL", "^GSPC", "AMZN"), from=data_init, to=data_end, auto.assign = T)
  getSymbols(c("BTC-USD", "ETH-USD", "LTC-USD"), from=data_init, to=data_end, auto.assign = T)
  
  # Criando um dataframe e ajustado o preço das ações
  #prices.data <- merge.zoo(MSFT[,6], SBUX[,6], IBM[,6], AAPL[,6], GSPC[,6], AMZN[,6])
  prices.data <- merge.zoo(`BTC-USD`[,6], `ETH-USD`[,6], `LTC-USD`[,6])
  
  # print(names(prices.data))
  
  return(prices.data)
}
  

adjust_data <- function(data, var){

  #' adjust_data = input ts with several times series output: clean data and structure for analysis
  #' @param data: dataset with class ts for analysis with the several columns
  #' @param: variable for the study
  
  # criando o dataset
  data = as.data.frame(prices.data)
  time_data = row.names(data)
  data = na.omit(data.frame(time_data, data[var]))
  names(data)[2] <- var
  
  print('Tamanho do dataset: ')
  print(dim(data))
  # print(paste0('Descritivas do dataset:', summary(data)))
  return(data)
}


arima_data <- function(data, percent_train, var){

  #' arima_data = input: dataset output: best model arima
  #' @param data: dataset for aplication
  #' @param percent_train: percentage of dataset for train
  
  # Daily Seasonality (frequency set to 1 for daily data)
  msft <- ts(na.omit(data[,var]),frequency=1)
  
  # Delimit training range
  msft.train <- window(msft,end=c(round(length(msft)*percent_train),1)) # pegando 70% dos dados para treino
  
  # Delimit testing range
  msft.test <- window(msft,start=c((round(length(msft)*percent_train)+1),1))
  
  # Training and testing ranges chart
  plot(msft,main=paste0("Série Temporal - ", var),ylab="Price",xlab="Days")
  lines(msft.train,col="blue"); lines(msft.test,col="green")
  legend("bottomright",col=c("blue","green"),lty=1,legend=c("Training","Testing"))
  
  # We shall perform the PP Unit Root and ADFC Test to check for stationarity in the above time series
  
  # Unit Root Tests
  # Augmented Dickey-Fuller Test ADF
  # h0: nao estacionaria
  # print(paste0('Teste Dickey-Fuller de raiz unitaria =  de H0: nao estacionaria'))
  adf.test(msft.train,alternative="stationary") # nao estacionaria
  print(paste0("Teste da raíz unitária: ", adf.test(msft.train,alternative="stationary")$p.value))
  # Phillips-Perron Test 
  # print(paste0('Teste Phillips-Perron de raiz unitaria =  de H0: nao estacionaria'))
  # pp.test(msft.train,alternative="stationary") # nao estacionaria

  # Time Series Differencing
  # plot(diff(msft), type="l",main="Time Series One Differencing",ylab="Price Differences",xlab="Days")
  # plot(diff(diff(msft)), type="l",main="Time Series One Differencing",ylab="Price Differences",xlab="Days")
  
  # Now applying the PP and the ADF test on the differenced time series shows below that the time series is stationary:
  # Apply Unit Root Tests on Differenced data
  # Augmented Dickey-Fuller Test ADF
  # print(paste0('Teste Dickey-Fuller de raiz unitaria após primeira diferença=  de H0: nao estacionaria'))
  # adf.test(diff(msft.train),alternative="stationary") # estacionaria
  print(paste0("Teste da raíz unitária para a primeira diferença: ", adf.test(diff(msft.train),alternative="stationary")$p.value))
  # Apply Unit Root Tests on Differenced data
  # Phillips-Perron Test PP 
  # print(paste0('Teste Phillips-Perron de raiz unitaria apos primeira diferenca =  de H0: nao estacionaria'))
  # pp.test(diff(msft.train),alternative="stationary") # estacionaria
  
  # Find Optimal Parameters
  # The parameters p, and q can be found using ACF and PACF plots.
  
  # ARIMA Models Specification
  # Normal and Partial Autocorrelation Functions ACF & PACF
  #acf(diff(msft.train)) # parece ter uma sazonalidade
  #pacf(diff(msft.train))
  
  # Build ARIMA Model
  
  # aic = bic = NULL
  # for(i in 0:11){
  #   
  #   tryCatch({
  #     model1 <- Arima(msft.train,order=c(0,i,0),include.constant=T)
  #     bic[i] = model1$bic
  #     aic[i] = model1$aic
  #   }, error=function(e){})
  # }
  # print(paste0("Best AIC: ",round(min(aic, na.rm = T),2), " Posicao: ",match(min(bic, na.rm = T),bic)))
  # print(paste0("Best BIC: ",round(min(bic, na.rm = T),2), " Posicao: ", match(min(aic, na.rm = T),aic)))
  #best_param <- match(min(aic, na.rm = T),aic)
  
  #Implement ARIMA (0,1,0) model
  #model1 <- Arima(msft.train,order=c(0,best_param,0),include.constant=T)
  #print(paste0('Best Model: ', model1))
  
  # tsdisplay(residuals(model1), lag.max=45, main='(0,1,0) Model Residuals')
  
  #print(paste0("P-valor do teste de normalidade dos resíduos para o primeiro modelo: ", shapiro.test(residuals(model1))$p.value ))  
  
  #Check Residuals forARIMA (0,1,0) model
  # checkresiduals(model1)
  # Ljung-Box test com pvalor menor que o nivel de confianca alpha de 0.05, os residuos sao ruidos brancos
  
  # Segunda hipotese
  # Thus we can confirm that the residuals are not distinguishable from a white noise series as the results are not significant.
  # We can also reconfirm whether the ARIMA (0,1,0) was indeed a good model by using the auto.arima test in R, as follows:
  #Checking using auto.arima
  modelauto <- auto.arima((msft.train), seasonal=FALSE)
  print(paste0('Segundo modelo declarado como melhor: ', modelauto))

  # We can make predictions using two methods: 
  # a) Multi-Step Forecast and b) One-Step Forecast without Re-Estimation
  
  # Multi-Steps Forecast
  # plot(forecast(model1,h=252),main="ARIMA(0,1,0) using Multi-Steps Forecast",ylab="Price",xlab="Date")
  # lines(msft.test,lty=3)
  
  # Multi-Steps Forecast Accuracy
  #teste <- accuracy(forecast(model1,h=252),msft.test)[2,1:6]
  #print(paste0('Acurácia do primeiro modelo por Multi-Step Forecast: ', round(teste['RMSE'],1)))
  teste <- accuracy(forecast(modelauto,h=252),msft.test)[2,1:6]
  print(paste0('Acurácia do segundo modelo por Multi-Step Forecast: ', round(teste['RMSE'],2)))
  # Thus this method of forecasting gives an RMSE of 16.08373
  
  # b) One-Step Forecast without Re-Estimation The One-Step Forecast does forecasting considering test data set.
  
  # One-Step Forecast without Re-Estimation
  #model2 <- Arima(msft.test,model=model1)$fitted
  # plot(msft.train,main="ARIMA(0,1,0) using One-Step Forecast without Re-Estimation",ylab="Price",xlab="Date",ylim=c(min(msft),max(msft)))
  # lines(model2,col="green")
  # lines(msft.test,lty=3)
  # legend("bottomright",col="green",lty=1,legend="Forecasted Price")
  
  # One-Step Forecast without Re-Estimation Accuracy
  #teste <- accuracy(model2,msft.test)[1,1:5]
  # print(paste0('Acurácia do primeiro modelo por One-Step Forecast without re-estimation: ', teste['RMSE']))
  
  # One-Step Forecast without Re-Estimation
  model2 <- Arima(msft.test,model=modelauto)
  res <- residuals(model2)
  print(paste0("P-valor do teste de normalidade dos resíduos para o segundo modelo: ", shapiro.test(residuals(model2))$p.value ))  
  model2 <- Arima(msft.test,model=modelauto)$fitted
  # plot(msft.train,main="ARIMA(0,1,0) using One-Step Forecast without Re-Estimation",ylab="Price",xlab="Date",ylim=c(min(msft),max(msft)))
  # lines(model2,col="green")
  # lines(msft.test,lty=3)
  # legend("bottomright",col="green",lty=1,legend="Forecasted Price")
  
  # One-Step Forecast without Re-Estimation Accuracy
  teste <- accuracy(model2,msft.test)[1,1:5]
  # print(paste0('Acurácia do segundo modelo por One-Step Forecast without re-estimation: ', teste['RMSE']))
  
  # quanto menor o rmse melhor, logo, o melhor modelo é o b
  
  preditor <- as.data.frame(forecast(modelauto,length(msft.test)))
  result <- data.frame(original = msft.test, preditor = preditor$`Point Forecast`)
  
  #options(xlsx.date.format="yyyy-MM-dd")
  write_xlsx(result, paste0('/home/andressa/Desktop/result_arima_',var,'.xlsx'))
  
  return(res)
  
}

ann_data <- function(data, percent_train, var){
  
  #' ann_data = input: dataset output: model with Artificial Neural Network - ANN -  Method for predicting MSFT Close Price
  #' @param data: dataset for aplication
  #' @param percent_train: percentage of dataset for train
   
  # This study employed a three-layer (one hidden layer) multilayer perceptron model trained with back-propagation algorithm.Sermpinis et al., (2012); Dai et al, (2012) and Atsalakis-Valavanis (2009) pointed out, that feed-forward neural networks (FFNN) can be the most effective to forecast financial time series.
  # Preparing input data for ANN
  #data <- as.data.frame(data[,var])
  #View(data)
  #data$Date <- rownames(data)
  #data <- data[,c(2,1)]
  colnames(data) <- c("Date","AdjClose")
  data$AdjCloseL1 <- Lag(data$AdjClose,1)
  data <- na.omit(data)
 
  
  # Delimit training range
  data.train <- data[1:(round(dim(data)[1]*percent_train)),]
  
  # Delimit testing range
  data.test <- data[(round(dim(data)[1]*percent_train)+1):dim(data)[1],]
  
  # The training parameters were set as follows: decay rate = 0.00001, number of units in the hidden layer = 10, and epoch size = 10000. Finally, the network was tested with the data set to estimate its generalization ability.
  
  #N Net Function for Close Price
  set.seed(1)
  neural.price.model <- nnet(AdjClose ~ AdjCloseL1, data = data.train,
                             maxit=10000,  size = 10, decay = 0.00001, linout = 1, skip=TRUE, MaxNWts=10000, trace=FALSE)
  #Predict Close Price on test data
  data.test$NN_Prediction <- predict(neural.price.model, newdata = data.test)
  
  #calculate RMS error
  rmse <- sqrt(mean((data.test$NN_Prediction-data.test$AdjClose)^2))
  
  # save the dataset prediction
  #options(xlsx.date.format="yyyy-MM-dd")
  
  write_xlsx(data.test, paste0('/home/andressa/Desktop/result_ann_',var,'.xlsx'))
  
  return(print(paste0('A acurácia - RMSE - do modelo ANN foi de: ', round(rmse,2))))

}

svr_data <- function(data, percent_train, var){
  
  #' svr_data = input: dataset output: model Suport Vector Machine
  #' @param data: dataset for aplication
  #' @param percent_train: percentage of dataset for train
  
  # Support Vector Regression(SVR) Method for predicting MSFT Close Price
  # SVR uses the same basic idea as Support Vector Machine (SVM), a classification algorithm, but applies it to predict real values rather than a class. SVR acknowledges the presence of non-linearity in the data and provides a proficient prediction model. A major benefit of using SVR is that it is a non-parametric technique. The SVR does not depend on distributions of the underlying dependent and independent variables. Instead the SVR technique depends on kernel functions. The close prices of Microsoft were predicted as shown below:

  #data <- as.data.frame(data[,var])
  #data$Date <- rownames(data)
  #data <- data[,c(2,1)]
  colnames(data) <- c("Date","AdjClose")
  data$AdjCloseL1 <- Lag(data$AdjClose,1)
  data <- na.omit(data)
  
  # Delimit training range
  data.train <- data[1:(round(dim(data)[1]*percent_train)),]
  
  # Delimit testing range
  data.test <- data[(round(dim(data)[1]*percent_train)+1):dim(data)[1],]
  
  # SVR Model
  trainControl = trainControl(method="repeatedcv", number=10, repeats=10)
  svm.price.model = train(AdjClose ~ AdjCloseL1, data=data.train, method="svmLinear", 
                          preProc=c("range"),trControl=trainControl)
  
  #Predict Close Price on test data
  data.test$SVM_Prediction <- predict(svm.price.model, newdata = data.test)
  
  #calculate RMS error
  rmse <- sqrt(mean((data.test$SVM_Prediction-data.test$AdjClose)^2))
  
  # save the prediction dataset
  #options(xlsx.date.format="yyyy-MM-dd")
  write_xlsx(data.test, paste0('/home/andressa/Desktop/result_svr_',var,'.xlsx'))
  
  print(paste0('A acurácia - RMSE - do modelo SVR foi de: ', rmse))
  
  
  return()
}

# GHST

garch_data <- function(data, percent_train, var){
  
  # Daily Seasonality (frequency set to 1 for daily data)
  msft <- ts(na.omit(data[,var]),frequency=1)
  # Delimit training range
  msft.train <- window(msft,end=c(round(length(msft)*percent_train),1)) # pegando 70% dos dados para treino
  # Delimit testing range
  msft.test <- window(msft,start=c((round(length(msft)*percent_train)+1),1))
  
  modelauto <- auto.arima((msft.train), seasonal=FALSE)
  m = Arima(msft.train,model=modelauto) # modelo escolhido do arima
  print(paste0("Teste de normalidade para os resíduos do modelo ARIMA: ", shapiro.test(residuals(m))$p.value ) ) # nao é normal (h0 sao normais)
  z = msft.train # serie que estamos trabalhando
  dlz = diff(msft.train) # primeira diferenca
  n = length(msft.train) # tamanho do vetor de treino
  res = residuals(m) # residuos do modelo
  previsao = msft.test
  
  spec <- ugarchspec( # construindo modelo ghskew
    mean.model = list(armaOrder =c(1,0), include.mean = F, arfima = FALSE),
    variance.model = list(model = "sGARCH", garchOrder = c(0,0)),
    distribution.model="ghst" # ghyp
  )
  
  spec <- ugarchspec(
    mean.model = list(armaOrder =c(1,0), include.mean = F, arfima = FALSE),
    variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
    fixed.pars = list(ar1=0),
    distribution.model="ghst"
  )
  
  fit <- ugarchfit(spec, dlz) # modelando com garch com a distribuição ghskew
  #fit_N <- ugarchfit(spec, res, solver="lbfgs")
  
  #pred = ugarchforecast(fit, n.ahead = 1, n.roll = 999)
  
  # pred = ugarchforecast(fit, n.ahead=100)
  # mu.predict = fitted(pred) 
  # sig.predict = sigma(pred)
  # VaR.predict = as.numeric(quantile(pred, probs=0.05))
  
  res <- sres1(fit)
  
  mode <- ugarchroll(spec, diff(log(c(z,previsao))), # vetor de var
                     forecast.length = 316, refit.every = 315)
  
  media <- mode@forecast$density$Mu
  desviopadrao <- mode@forecast$density$Sigma
  
  m <- length(previsao)
  k <- length(media) # poderia ser de sigma tb
  
  #parame <- GHfit(res, plot=F)
  
  parame <- GHfitm(res)
  forc = ugarchforecast(fit, n.ahead=100)
  
  mu <- parame$param["mu"]
  delta <- parame$param["delta"]
  beta <- parame$param["beta"]
  nu <- parame$param["nu"]
  
  a<- desviopadrao;b<- media
  (mu2 <- a*mu + b) 
  (delta2 <- delta*a)
  (nu2 <- nu )
  (beta2 <- beta/a)
  
  
  tam <- length(previsao)+1
  
  z_atz<-c(last(z),previsao[c(-(tam-1),-tam)])
  f3 <- function(mu2,delta2,beta2,nu2){
    
    qskewhyp(0.05, mu = mu2, delta= delta2, beta= beta2, nu = nu2)
    
  }
  
  f4 <- function(mu2,delta2,beta2,nu2){
    
    qskewhyp(0.95, mu = mu2, delta= delta2, beta= beta2, nu = nu2)
    
  }
  
  A3 <-mapply(f3,mu2,delta2,beta2,nu2)
  A4 <-mapply(f4,mu2,delta2,beta2,nu2)
  
  log_p <- log(z_atz)
  C3 <- exp(A4 + log_p) 
  C4 <- exp(A3 + log_p) 
  
  # previsao sem o ultimo
  
  tam<-length(previsao)
  
  # nao precisa retirar o ultimo dado
  #z_test <- previsao[-tam]
  
  z_test <- previsao
  length(z_test)
  
  a_second <- sum(z_test < C3)/length(C3)
  vector <- c(T,F,F,T,T)
  sum(vector)
  
  # teste da Binomeial para comparar se o modelo foi bem adequado
  # Artigo 
  
  # VaRloss
  x<-as.numeric(z_test < C3)
  length(z_test)
  sucessos <- sum(x)
  fracassos <- length(C3) - sucessos
  par <- c(sucessos , fracassos)
  
  # hipoteses a serem testadas
  # H0: modelo adequado
  # H1: modelo n?o adequado
  
  # para a GHST
  
  result = binom.test(par,length(C3) , p = 0.05,
                      alternative = "two.sided",
                      conf.level = 0.95)
  print(paste0("P-valor do teste de adequação do modelo: ", result$p.value))
  
  #calculate RMS error
  rmse <- sqrt(mean((C4-previsao)^2))
  print(paste0("RMSE do modelo com garch: ", rmse))
  
  data.test = data.frame(C4, msft.test)
  write_xlsx(data.test, paste0('/home/andressa/Desktop/result_garch_',var,'.xlsx'))
  
  return(C4)
}


total_data <- function(data, var, percent_train){
  
  for(i in 1:length(var)){
    
    print(paste0('DATASET QUE ESTOU ANALISANDO AGORA: ', var[i]))
    # print(paste0('Inicio: ', min(as.Date(as.character(data$time_data)))))
    # print(paste0('Inicio do Teste: ', data[round(length(as.Date(as.character(data$time_data)))*0.80)+1,1]))
    data = adjust_data(data, var = var[i])
    arima_data(data, percent_train, var = var[i])
    garch_data(data, percent_train, var[i])
    ann_data(data, percent_train = percent_train, var[i])
    svr_data(data, percent_train = percent_train, var[i])  
    
  }
  return()
}









# Links:
# https://rpubs.com/prateekdaniels/msft
# https://www.r-bloggers.com/machine-learning-using-support-vector-machines/
# https://www.svms.org/regularization/MSRS97a.pdf
# https://www.r-bloggers.com/fitting-a-neural-network-in-r-neuralnet-package/
# https://analisemacro.com.br/data-science/dicas-de-rstats/avaliando-a-acuracia-de-uma-previsao-com-o-r/