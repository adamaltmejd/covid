plan <- drake_plan(
    latest_fhm = target(download_latest_fhm(folder = file_out(!!file.path("data", "FHM"))),
                        trigger = trigger(condition = trigger_new_download(!!file.path("data", "FHM", "FHM_latest.xlsx")))),
    ecdc = target(get_remote_data(url = file_in("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv"),
                                  f = file_out(!!file.path("data", "ECDC.csv"))),
                  trigger = trigger(change = Sys.Date())),
    fhm_files = target(list_fhm_files(folder = file_in(!!file.path("data", "FHM"))), format = "file"),
    death_dts = target(load_fhm(fhm_files), dynamic = map(fhm_files)),
    death_dt = join_data(death_dts),
    death_prediction = predict_lag(death_dt),
    time_to_finished = calculate_lag(death_dt),

    # Save data
    fwrite(death_dt, file_out(!!file.path("data", "covid_deaths_latest.csv"))),

    # Plots
    default_theme = set_default_theme(),
    death_plot = target(plot_lagged_deaths(death_dt, death_prediction, ecdc, default_theme)),
    lag_plot = plot_lag_trends(death_dt, default_theme),

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

