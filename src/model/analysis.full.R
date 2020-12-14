##
# first run full.run to get files
#
#
##
graphics.off()
store_data_folder <- file.path("data","tmp","model2","v13")
data_files <- list.files(store_data_folder,pattern='Npost*')

data <- c()
for(file in data_files){
    data <- rbind(data, readRDS(paste(store_data_folder,'/',file,sep="")))
}
data$df <- as.Date(data$State)-as.Date(data$dates)

days = 1:14
data <- data[order(data$State),]
for(day in days){
    index <- data$df==day
    data_temp <- data[index,]
    CI_cov <- (data_temp$Truth >= data_temp$lCI) &(data_temp$Truth <= data_temp$uCI)
    CI_under <- (data_temp$Truth < data_temp$lCI)
    CI_over <- (data_temp$Truth > data_temp$uCI)
    cat(' day =  ',day,': ')
    cat(' mabs = ', round(mean(abs(data_temp$Truth-data_temp$median)),2))
    cat(' rmse = ',round(sqrt(mean((data_temp$Truth-data_temp$median)^2)),2))
    cat(' CI =  ',round(mean(CI_cov),2))
    cat(' CI_width =  ',round(mean( data_temp$uCI-data_temp$lCI ),2))
    cat(' (',round(mean(CI_under),2),',',round(mean(CI_over),2),")" ,sep="")
    cat('\n')
}
day_ <- 1
index <- data$df==day_
data_temp <- data[index,]
#data_temp <- data_temp[80:sum(index),]
CI_cov <- (data_temp$Truth >= data_temp$lCI) &(data_temp$Truth <= data_temp$uCI)

x11()
par(mfrow=c(2,2))
plot(1:length(CI_cov),cumsum(CI_cov)/(1:length(CI_cov)),xlab='days',ylab='coverage',main=paste(day_,'days'))
plot(data_temp$Truth,CI_cov)
plot(as.Date(data_temp$dates),data_temp$Truth, ylim=c(min(data_temp$lCI,data_temp$Truth),
                                                      max(data_temp$uCI,data_temp$Truth)),
     main = paste('cov = ',round(mean(CI_cov),2)))
lines(as.Date(data_temp$dates),data_temp$lCI,col='red')
lines(as.Date(data_temp$dates),data_temp$uCI,col='red')
plot(1:length(CI_cov),cumsum(abs(data_temp$Truth-data_temp$median))/(1:length(CI_cov)),xlab='days',ylab='mabs',main=paste(day_,'days'))


