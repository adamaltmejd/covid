plan <- drake_plan(
    latest_fhm = target(download_latest_fhm(folder = file_out(!!file.path("data", "FHM"))),
                        trigger = trigger(condition = trigger_new_download(!!file.path("data", "FHM", "FHM_latest.xlsx")))),
    ecdc = target(get_ecdc(url = "https://opendata.ecdc.europa.eu/covid19/casedistribution/csv",
                                  f = file_out(!!file.path("data", "ECDC.csv"))),
                  trigger = trigger(condition = RCurl::url.exists("opendata.ecdc.europa.eu/covid19/casedistribution/csv"),
                                    change = Sys.Date())),
    fhm_paths = list.files(file_in(!!file.path("data", "FHM")), pattern = "^Folkhalso", full.names = TRUE),
    fhm_files = target(fhm_paths, format = "file", dynamic = map(fhm_paths)),
    death_dts = target(load_fhm_data(fhm_files, type = "deaths"), dynamic = map(fhm_files)),
    death_dt = join_data(death_dts),
    time_to_finished = calculate_lag(death_dt),
    days = day_of_week(death_dt),

    ##
    # Other countries

    # Finland
    url_finland = "https://sampo.thl.fi/pivot/prod/en/epirapo/covid19case/fact_epirapo_covid19case.csv?row=dateweek20200101-508804L&column=measure-444833.445356.492118.",
    deaths_dt_finland = target(
        unique(rbind(
            fread(file.path("data", "other_countries", "finland.csv")),
            data.table(publication_date = as.IDate(Sys.Date()),
                       fread(url_finland, sep = ";",
                             select = c("Time" = "IDate", "Measure" = "character", "val" = "integer"),
                             col.names = c("date", "variable", "value")),
                       key = c("publication_date", "date"))[!is.na(value)],
            use.names = TRUE
        ), by = c("publication_date", "date")),
        trigger = trigger(condition = RCurl::url.exists(url_finland) &
                          max(fread(file.path("data", "other_countries", "finland.csv"))$publication_date) < Sys.Date(),
                          change = Sys.Date())
    ),
    fwrite(deaths_dt_finland, file.path("data", "other_countries", "finland.csv")),

    # UK
    url_uk = "https://coronavirus.data.gov.uk/api/v1/data?filters=areaType=overview&structure=%7B%22areaType%22:%22areaType%22,%22areaName%22:%22areaName%22,%22areaCode%22:%22areaCode%22,%22date%22:%22date%22,%22newDeaths28DaysByDeathDate%22:%22newDeaths28DaysByDeathDate%22,%22cumDeaths28DaysByDeathDate%22:%22cumDeaths28DaysByDeathDate%22%7D&format=csv",
    deaths_dt_uk = target(
        unique(rbind(
            fread(file.path("data", "other_countries", "uk.csv")),
            data.table(publication_date = as.IDate(Sys.Date()),
                       fread(url_uk,
                             select = c("date" = "IDate", "newDeaths28DaysByDeathDate" = "integer"),
                             col.names = c("date", "N")),
                       key = c("publication_date", "date"))[!is.na(N)],
            use.names = TRUE
        ), by = c("publication_date", "date")),
        trigger = trigger(condition = RCurl::url.exists(url_uk) &
                          max(fread(file.path("data", "other_countries", "uk.csv"))$publication_date) < Sys.Date(),
                          change = Sys.Date())
    ),
    fwrite(deaths_dt_uk, file.path("data", "other_countries", "uk.csv")),

    ##
    # Hospital/ICU Stats
    # From FHM
    icu_dts = target(load_fhm_data(fhm_files, type = "icu"), dynamic = map(fhm_files)),
    icu_dt = join_data(icu_dts),
    # From Socialstyrelsen
    socstyr_dt = target(update_socstyr(f = file_out(!!file.path("data", "Socialstyrelsen_latest.csv"))),
                        trigger = trigger(condition = trigger_new_download(!!file.path("data", "Socialstyrelsen_latest.csv"), type = "SocStyr"))),

    # Model predictions
    # Constant model
    death_prediction_constant = predict_lag(death_dt),

    # Statistical model
    model_death_dt = model_build_death_dt(death_dt),
    model_icu_dt = model_build_icu_dt(icu_dt),
    death_prediction_model_raw = run.model2.all(model_death_dt, model_icu_dt, days.to.pred = 25,prior=F),
    death_prediction_model = gp_smooth(death_prediction_model_raw, model_death_dt),

    # Save data
    fwrite(icu_dt, file_out(!!file.path("data", "covid_icu_latest.csv"))),
    fwrite(death_dt, file_out(!!file.path("data", "covid_deaths_latest.csv"))),
    fwrite(merge(death_prediction_constant[, -"predicted_deaths"],
                 death_prediction_model[, -c("sure_deaths", "predicted_deaths")],
                 suffixes = c(".constant", ".model"), all = TRUE),
           file_out(!!file.path("data", "predictions.csv"))),

    # Main Plots
    default_theme = set_default_theme(),
    death_plot = plot_lagged_deaths(death_dt, death_prediction_model, default_theme),
    lag_plot1 = plot_lag_trends1(time_to_finished, default_theme),
    lag_plot2 = plot_lag_trends2(death_dt, default_theme),
    lag_plot = plot_lag_trends_grid(lag_plot1, lag_plot2, default_theme),

    # Save main plots
    target(archive_plots(file_out(!!file.path("docs", "archive"))), trigger = trigger(change = Sys.Date())),
    save_plot(death_plot, file_out(!!file.path("docs", paste0("deaths_lag_sweden_", Sys.Date() , ".png")))),
    save_plot(death_plot, file_out(!!file.path("docs", paste0("deaths_lag_sweden_latest.png"))), bgcolor = "white"),
    save_plot(lag_plot, file_out(!!file.path("docs", paste0("lag_trend_sweden_", Sys.Date() , ".png")))),
    save_plot(lag_plot, file_out(!!file.path("docs", paste0("lag_trend_sweden_latest.png"))), bgcolor = "white"),
    update_web(death_plot = file_in(!!file.path("docs", paste0("deaths_lag_sweden_", Sys.Date() , ".png"))),
               lag_plot = file_in(!!file.path("docs", paste0("lag_trend_sweden_", Sys.Date() , ".png"))),
               index = file_out(!!file.path("docs", "index.md"))),

    # UK Plot
    death_plot_uk = plot_lagged_deaths(
        join_data(deaths_dt_uk), NULL, default_theme,
        custom_labs = labs(title = paste0("Covid-19 deaths in the UK"),
                           subtitle = paste0("Each death is attributed to its actual day of death. Colored bars show reporting delay."),
                           caption = paste0("Source: GOV.UK. Updated: ", Sys.Date(), "."),
                           fill = "Reporting delay", x = "Date of death", y = "Number of deaths")),
    save_plot(death_plot_uk, file_out(!!file.path("docs", paste0("deaths_lag_uk.png"))), bgcolor = "white"),

    # Evaluation plots
    coverage_plot_t0 = plot_coverage_eval(death_dt, death_prediction_constant, death_prediction_model, 0, default_theme),
    save_plot(coverage_plot_t0, file_out(!!file.path("docs", "eval", paste0("coverage_eval_t0.png"))), bgcolor = "white"),
    coverage_plot_t1 = plot_coverage_eval(death_dt, death_prediction_constant, death_prediction_model, 1, default_theme),
    save_plot(coverage_plot_t1, file_out(!!file.path("docs", "eval", paste0("coverage_eval_t1.png"))), bgcolor = "white"),
    coverage_plot_t2 = plot_coverage_eval(death_dt, death_prediction_constant, death_prediction_model, 2, default_theme),
    save_plot(coverage_plot_t2, file_out(!!file.path("docs", "eval", paste0("coverage_eval_t2.png"))), bgcolor = "white"),
    coverage_plot_t3 = plot_coverage_eval(death_dt, death_prediction_constant, death_prediction_model, 3, default_theme),
    save_plot(coverage_plot_t3, file_out(!!file.path("docs", "eval", paste0("coverage_eval_t3.png"))), bgcolor = "white"),
    coverage_plot_t4 = plot_coverage_eval(death_dt, death_prediction_constant, death_prediction_model, 4, default_theme),
    save_plot(coverage_plot_t4, file_out(!!file.path("docs", "eval", paste0("coverage_eval_t4.png"))), bgcolor = "white"),
    coverage_plot_t5 = plot_coverage_eval(death_dt, death_prediction_constant, death_prediction_model, 5, default_theme),
    save_plot(coverage_plot_t5, file_out(!!file.path("docs", "eval", paste0("coverage_eval_t5.png"))), bgcolor = "white"),

    # Prior evaluation plot
    prior_eval_plot = deaths_icu_hospital_corr_plot(model_death_dt, socstyr_dt, default_theme),
    save_plot(prior_eval_plot, file_out(!!file.path("docs", "eval", paste0("prior_eval.png"))), bgcolor = "white"),
)

