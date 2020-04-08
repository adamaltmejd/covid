plan <- drake_plan(
    latest_fhm = target(download_latest_fhm(file_out(!!file.path("data", "FHM", "FHM_latest.xlsx"))),
                        trigger = trigger(condition = trigger_new_download(!!file.path("data", "FHM", "FHM_latest.xlsx")))),
    fhm_files = target(file_in(!!list.files(file.path("data", "FHM"), pattern = "^Folkhalso", full.names = TRUE)), format = "file"),
    deaths = target(load_fhm(fhm_files), dynamic = map(fhm_files)),
    DT = join_data(deaths),
    prediction = predict_lag(DT),
    lag_plot = target(plot_lagged_deaths(DT, prediction)),
    save_plot(lag_plot, file_out(!!file.path("out", "lag_plot.pdf")))
)
