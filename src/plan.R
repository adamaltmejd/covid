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
    death_prediction = predict_lag(death_dt),
    time_to_finished = calculate_lag(death_dt),
    days = day_of_week(death_dt),

    # ICU Stats
    icu_dts = target(load_fhm_data(fhm_files, type = "icu"), dynamic = map(fhm_files)),
    icu_dt = join_data(icu_dts),

    # Model predictions
    model_death_dt = model_build_death_dt(death_dt),
    model_icu_dt = model_build_icu_dt(icu_dt),

    #model_results <- run.model.all(readd(model_death_dt), readd(model_icu_dt))
    model_results = run.model.all(model_death_dt, model_icu_dt),

    # Save data
    fwrite(icu_dt, file_out(!!file.path("data", "covid_icu_latest.csv"))),
    fwrite(death_dt, file_out(!!file.path("data", "covid_deaths_latest.csv"))),
    fwrite(death_prediction, file_out(!!file.path("data", "predictions.csv"))),

    # Plots
    default_theme = set_default_theme(),
    death_plot = plot_lagged_deaths(death_dt, death_prediction, ecdc, days, default_theme),
    lag_plot1 = plot_lag_trends1(time_to_finished, days, default_theme),
    lag_plot2 = plot_lag_trends2(death_dt, days, default_theme),
    lag_plot = plot_lag_trends_grid(lag_plot1, lag_plot2, default_theme),

    # Save plots
    target(archive_plots(file_out(!!file.path("docs", "archive"))), trigger = trigger(change = Sys.Date())),

    save_plot(death_plot, file_out(!!file.path("docs", paste0("deaths_lag_sweden_", Sys.Date() , ".png")))),
    save_plot(death_plot, file_out(!!file.path("docs", paste0("deaths_lag_sweden_latest.png"))), bgcolor = "white"),

    save_plot(lag_plot, file_out(!!file.path("docs", paste0("lag_trend_sweden_", Sys.Date() , ".png")))),
    save_plot(lag_plot, file_out(!!file.path("docs", paste0("lag_trend_sweden_latest.png"))), bgcolor = "white"),

    update_web(death_plot = file_in(!!file.path("docs", paste0("deaths_lag_sweden_", Sys.Date() , ".png"))),
               lag_plot = file_in(!!file.path("docs", paste0("lag_trend_sweden_", Sys.Date() , ".png"))),
               index = file_out(!!file.path("docs", "index.md")))
)

