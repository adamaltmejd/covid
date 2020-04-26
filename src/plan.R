plan <- drake_plan(
    latest_fhm = target(download_latest_fhm(folder = file_out(!!file.path("data", "FHM"))),
                        trigger = trigger(condition = trigger_new_download(!!file.path("data", "FHM", "FHM_latest.xlsx")))),
    fhm_files = target(list_fhm_files(folder = file_in(!!file.path("data", "FHM"))), format = "file"),
    death_dts = target(load_fhm(fhm_files), dynamic = map(fhm_files)),
    death_dt = join_data(death_dts),
    write.csv(death_dt, file_out(!!file.path("data", "FHM", "covid_deaths_latest.csv"))),
    death_prediction = predict_lag(death_dt),

    # Plots
    default_theme = set_default_theme(),

    lag_plot = target(plot_lagged_deaths(death_dt, death_prediction, my_theme = default_theme)),
    target(archive_plots(file_out(!!file.path("docs", "archive"))), trigger = trigger(change = Sys.Date())),
    save_plot(lag_plot, file_out(!!file.path("docs", paste0("deaths_lag_sweden_", Sys.Date() , ".png")))),
    save_plot(lag_plot, file_out(!!file.path("docs", paste0("deaths_lag_sweden_latest.png"))), bgcolor = "white"),
    update_web(plots = file_in(!!file.path("docs", paste0("deaths_lag_sweden_", Sys.Date() , ".png"))),
               index = file_out(!!file.path("docs", "index.md")))
)

