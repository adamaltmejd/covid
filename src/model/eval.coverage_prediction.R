##
# output analysis
#
##

#'
#'
#'@param days.ago - how long ago since the predicition was made
#'@param lag      - only report for result lag days ago
#'@param max.days - only report the max.days
coverage_data <- function(model_death_dt,
                          death_prediction_model,
                          days.ago=0,
                          lag = 20){

    N <- length(model_death_dt$dates)
    death_reported_so_far <- data.frame(
                                        T_deaths = apply(model_death_dt$detected,1,max, na.rm=T)[1:(N-lag)],
                                        dates  =  model_death_dt$dates[1:(N-lag)])
    df <- as.Date(death_prediction_model$prediction_date)-as.Date(death_prediction_model$date)
    death_prediction_model_mod <- death_prediction_model[df==days.ago,]
    death_prediction_model_mod <- death_prediction_model_mod[death_prediction_model_mod$prediction_date%in%death_reported_so_far$dates,]
    death_prediction_model_mod <- merge(death_prediction_model_mod,
                                        death_reported_so_far,
                                        by.x='date',
                                        by.y='dates')
    coverage <- data.frame(
                           date            = death_prediction_model_mod$date,
                           reported        = death_prediction_model_mod$T_deaths,
                           prediciton      = death_prediction_model_mod$total,
                           uCI             = death_prediction_model_mod$predicted_deaths_uCI,
                           lCI             = death_prediction_model_mod$predicted_deaths_lCI,
                           lmiss           = death_prediction_model_mod$T_deaths < death_prediction_model_mod$predicted_deaths_lCI,
                           umiss           = death_prediction_model_mod$T_deaths > death_prediction_model_mod$predicted_deaths_uCI
                           )
    return(coverage)
}
#'
#'
#'@param coverage - data from coverage_data
coverage.plot <- function(coverage, days.ago, theme){
    main = paste('Prediciton days ago ',days.ago,'. coverage = ',
                             round(1-mean(coverage$lmiss+coverage$umiss),2),
                                    ", (", round(mean(coverage$lmiss),2),",",
                                     round(mean(coverage$umiss),2),")", sep="")
    ggfig <- ggplot(data = coverage, aes(y = reported, x = date)) + geom_point() +
             geom_ribbon(aes(ymin=uCI,ymax=lCI),alpha=0.3,fill='blue') + geom_line(aes(y=prediciton),color='blue')+
          ggtitle(main)
    return(ggfig)
}
#coverage <- coverage_data(model_death_dt,
 #                     death_prediction_model,
 #                     days.ago=0)
#main = paste('Prediciton days ago',days.ago,' coverage = ',
#             round(1-mean(coverage$lmiss+coverage$umiss),2),
#                   ", (", round(mean(coverage$lmiss),2),",",
#                    round(mean(coverage$umiss),2),")", sep="")
#plot(coverage$date,coverage$reported, main=main)
#lines(coverage$date, coverage$uCI,col='red')
#lines(coverage$date, coverage$lCI,col='red')
