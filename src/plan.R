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

    # ICU Stats
    icu_dts = target(load_fhm_data(fhm_files, type = "icu"), dynamic = map(fhm_files)),
    icu_dt = join_data(icu_dts),

    # Model predictions
    model_death_dt = model_build_death_dt(death_dt),
    model_icu_dt = model_build_icu_dt(icu_dt),

    death_prediction_constant = predict_lag(death_dt),
    death_prediction_model = run.model.all(model_death_dt, model_icu_dt),

    #model prediction seperate lag
    death_prediction_model2 = run.model2.all(model_death_dt,model_icu_dt, days.to.pred = 25,prior=F),
    death_prediction_model2_smooth  = gp_smooth(death_prediction_model2, model_death_dt),
    days.ago = 7,
    coverage_constant = coverage_data(model_death_dt, death_prediction_constant, days.ago),
    coverage_model = coverage_data(model_death_dt, death_prediction_model, days.ago),
    coverage_model2 = coverage_data(model_death_dt, death_prediction_model2_smooth, days.ago),

    # Save data
    fwrite(icu_dt, file_out(!!file.path("data", "covid_icu_latest.csv"))),
    fwrite(death_dt, file_out(!!file.path("data", "covid_deaths_latest.csv"))),
    fwrite(merge(death_prediction_constant[, -"predicted_deaths"],
                 death_prediction_model[, -c("sure_deaths", "predicted_deaths")],
                 suffixes = c(".constant", ".model"), all = TRUE),
           file_out(!!file.path("data", "predictions.csv"))),

    # Plots
    default_theme = set_default_theme(),
    death_plot = plot_lagged_deaths(death_dt, death_prediction_constant, death_prediction_model2_smooth, ecdc, days, default_theme),
    lag_plot1 = plot_lag_trends1(time_to_finished, days, default_theme),
    lag_plot2 = plot_lag_trends2(death_dt, days, default_theme),
    lag_plot = plot_lag_trends_grid(lag_plot1, lag_plot2, default_theme),

    coverage_plot_constant = coverage.plot(coverage_constant, days.ago, default_theme, type = "constant"),
    coverage_plot_model = coverage.plot(coverage_model, days.ago, default_theme, type = "statistical"),
    coverage_plot_model2 = coverage.plot(coverage_model2, days.ago, default_theme, type = "stat sep lag"),

    analysis.prob.fig = probability_analysis(model_death_dt),

    # Save plots
    target(archive_plots(file_out(!!file.path("docs", "archive"))), trigger = trigger(change = Sys.Date())),

    save_plot(death_plot, file_out(!!file.path("docs", paste0("deaths_lag_sweden_", Sys.Date() , ".png")))),
    save_plot(death_plot, file_out(!!file.path("docs", paste0("deaths_lag_sweden_latest.png"))), bgcolor = "white"),

    save_plot(lag_plot, file_out(!!file.path("docs", paste0("lag_trend_sweden_", Sys.Date() , ".png")))),
    save_plot(lag_plot, file_out(!!file.path("docs", paste0("lag_trend_sweden_latest.png"))), bgcolor = "white"),

    save_plot(coverage_plot_constant, file_out(!!file.path("docs", "eval", paste0("coverage_eval_constant.png"))), bgcolor = "white"),
    save_plot(coverage_plot_model, file_out(!!file.path("docs", "eval", paste0("coverage_eval_model.png"))), bgcolor = "white"),
    save_plot(coverage_plot_model2, file_out(!!file.path("docs", "eval", paste0("coverage_eval_model2.png"))), bgcolor = "white"),
    save_plot(analysis.prob.fig, file_out(!!file.path("docs", "eval", paste0("probability.png"))), bgcolor = "white"),
    update_web(death_plot = file_in(!!file.path("docs", paste0("deaths_lag_sweden_", Sys.Date() , ".png"))),
               lag_plot = file_in(!!file.path("docs", paste0("lag_trend_sweden_", Sys.Date() , ".png"))),
               index = file_out(!!file.path("docs", "index.md")))
)

