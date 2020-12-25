##
# analysis of slutenvard
#
##
library(gamm4)
library(drake)
graphics.off()
lag <- 7
delay <- 20
drake::loadd(model_death_dt)
drake::loadd(socstyr_dt)

n = dim(socstyr_dt)[1]
slutenvard <- as.numeric(socstyr_dt$hospital_n)
intensvard <- as.numeric(socstyr_dt$icu_n)
elder     <- as.numeric(socstyr_dt$infected_eldercare_n)
days       <- 1:n

##
# the three covariates
data.sluten <- data.frame(days= days[!is.na(slutenvard)],
                          date = as.Date(socstyr_dt$date[!is.na(slutenvard)]),
                          y   = slutenvard[!is.na(slutenvard)])
data.intensvard <- data.frame(days  = days[!is.na(intensvard)],
                               date = as.Date(socstyr_dt$date[!is.na(intensvard)]),
                               y    = intensvard[!is.na(intensvard)])
data.elder <- data.frame(days  = days[!is.na(elder)],
                              date = as.Date(socstyr_dt$date[!is.na(elder)]),
                              y    = elder[!is.na(elder)])

model.sluten   <- gam(y~  s(days), data=data.sluten,     family = nb(),method="REML" )
model.iva      <- gam(y~  s(days), data=data.intensvard, family = nb(),method="REML" )
model.elder    <- gam(y~  s(days), data=data.elder,      family = nb(),method="REML" )

model.covs <- list(sluten = model.sluten,
                   iva    = model.iva,
                   elder  = model.elder )
cov.data <- list()
for(i in 1:length(model.covs)){
    cov.data[[i]] <- data.frame( date  =as.Date(socstyr_dt$date),
                                 theta = predict(model.covs[[i]],
                                                 newdata = data.frame(days = days)))
}


#number of dead
Y <- apply(model_death_dt$detected,1,max,na.rm=T)
death.list = data.frame( date = as.Date(model_death_dt$dates), death = Y)


# analys lag fit
lags <- 1:20
lags.model <- list(sluten = rep(NA,length(lags)),
                   iva    = rep(NA,length(lags)),
                   elder  = rep(NA,length(lags)))
lags.sluten <- rep(NA,length(lags))
models.selected <- list()
for(i in 1:length(lags)){
    lag <- lags[i]
    for(j in 1:length(lags.model)){
        cov.data_j <- cov.data[[j]]
        cov.data.lag = data.frame( date   = c(cov.data_j$date, cov.data_j$date[n]+1:lag),
                                   theta  = c(rep(0,lag),cov.data_j$theta))

        data.total <- merge(cov.data.lag,death.list, by ="date")
        # remove fist 30 days and last 20
        data.total.rem <- data.total[30:(dim(data.total)[1]-delay),]
        model.fit <- gam(death~  theta, data=data.total.rem, family = nb(),method="REML" )
        lags.model[[j]][i] <- -model.fit$aic
        if(-model.fit$aic == max(lags.model[[j]], na.rm=T)){
            model.fit$date <- data.total.rem$date
            models.selected[[j]] <- model.fit
        }
    }


}
x11()
plot(lags, lags.model[[1]], ylim= c(min(unlist(lags.model)), max(unlist(lags.model)) ), col='blue')
points(lags, lags.model[[2]],col='red')
points(lags, lags.model[[3]],col='green')
x11()
plot(data.total.rem$date,data.total.rem$death, xlab='date',ylab='Y',
     main = paste("final date = ",max(data.total.rem$date),sep="") ,pch=3)
lines(models.selected[[1]]$date,exp(predict(models.selected[[1]])),col='blue')
lines(models.selected[[2]]$date,exp(predict(models.selected[[2]])),col='red')
lines(models.selected[[3]]$date,exp(predict(models.selected[[3]])),col='green')
legend(data.total.rem$date[110], 80, legend=c(paste("sluten lag = ",which.max(lags.model$sluten),sep=""),
                                              paste("iva    lag = ",which.max(lags.model$iva),sep=""),
                                              paste("alder  lag = ",which.max(lags.model$elder),sep="")),
       col=c("blue", "red", "green"), lty=1:1, cex=0.5)
