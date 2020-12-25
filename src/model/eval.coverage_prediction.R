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
                          death_prediction,
                          days.ago=0,
                          lag = 20){

    N <- length(model_death_dt$dates)
    death_reported_so_far <- data.frame(
                                        T_deaths = apply(model_death_dt$detected,1,max, na.rm=T)[1:(N-lag)],
                                        dates  =  model_death_dt$dates[1:(N-lag)])
    df <- as.Date(death_prediction$prediction_date)-as.Date(death_prediction$date)
    death_prediction_mod <- death_prediction[df==days.ago,]
    death_prediction_mod <- death_prediction_mod[death_prediction_mod$prediction_date%in%death_reported_so_far$dates,]
    death_prediction_mod <- merge(death_prediction_mod,
                                        death_reported_so_far,
                                        by.x='date',
                                        by.y='dates')
    coverage <- data.frame(
                           date            = death_prediction_mod$date,
                           reported        = death_prediction_mod$T_deaths,
                           prediciton      = death_prediction_mod$total,
                           uCI             = death_prediction_mod$total_uCI,
                           lCI             = death_prediction_mod$total_lCI,
                           lmiss           = death_prediction_mod$T_deaths < death_prediction_mod$total_lCI,
                           umiss           = death_prediction_mod$T_deaths > death_prediction_mod$total_uCI
                           )

    coverage <- coverage[coverage$date >= "2020-05-01",]
    return(coverage)
}
#'
#'
#'@param coverage - data from coverage_data
coverage.plot <- function(coverage, days.ago, theme, type){
    title <- paste0("Evaluation of forecast (", type, " model)")
    subtitle <- paste0("Plot shows nowcast of total deaths as predicted for t=-", days.ago, " days back, with 95% CI.\n Black points are actual outcomes. ",
                       "Coverage = ", 100*round(1-mean(coverage$lmiss+coverage$umiss),2), "% (",
                       round(mean(coverage$lmiss),2),",", round(mean(coverage$umiss),2),")")
    ggfig <- ggplot(data = coverage, aes(y = reported, x = date)) +
        geom_point() +
        geom_ribbon(aes(ymin = uCI, ymax = lCI), alpha = 0.3, fill = 'blue') +
        geom_line(aes(y = prediciton), color = 'blue') +
        scale_x_date(date_breaks = "1 month", date_labels = "%B", expand = expansion(add = 0)) +
        scale_y_continuous(limits = c(-10, 130), minor_breaks = seq(0,200,10), breaks = seq(0,200,20), expand = expansion(add = c(10, 10))) +
        theme +
        labs(title = title, subtitle = subtitle,
             caption = paste0("Last day included = ", max(coverage$date), "."),
             x = "Date",
             y = "Number of deaths")
    return(ggfig)
}


#'
#' Plots all n/N when N > N_min
#'
#' @param model_death_dt
#' @param lag      - only report for result lag days ago
probability_analysis <- function(model_death_dt ,theme,days.reported=5, lag = 20, N_min=5){

    report_cleaned <- report_clean(model_death_dt$detected,model_death_dt$dates)
    new_cases <- newCases(report_cleaned)
    rownames(new_cases) <- as.character(model_death_dt$dates)
    colnames(new_cases) <- as.character(model_death_dt$dates)
    diag(new_cases) <- NA
    N <- apply(new_cases,1,sum, na.rm=T)
    Prob <- matrix(0,nrow=dim(new_cases)[1]-lag,ncol=40)
    for(i in 1:(dim(new_cases)[1]-lag)){
        cases <- new_cases[i,]
        m <- min(length(cases)-i,40)
        cases <- cases[i+ (0:m)]
        #cases <- cases[is.na(cases)==F ]

        Prob[i,1:m] = cases[1:m]/sum(new_cases[i,], na.rm=T)
    }
    N <- N[1:(dim(new_cases)[1]-lag)]
    dates <- as.Date(model_death_dt$dates[1:(dim(new_cases)[1]-lag)])
    data  <- data.frame(date = dates[N>N_min],
                        prob = apply(Prob[N>N_min,1:days.reported,drop=F],1,sum, na.rm=T))
    ggfig <- ggplot(data = data, aes(y = prob, x = date)) +
             geom_point()+# theme +
             labs(title = paste0("reprorted up to day ",days.reported," given total deaths greater then >",N_min,sep=""),
             x = "Date",
             y = "reported/Total")
    return(ggfig)
}
