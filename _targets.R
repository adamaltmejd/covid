library(targets)
library(tarchetypes)

source('src/functions.R')
source('src/model/prepare_data.R')
source('src/model/run.model.all.R')
source('src/model/seperate_lag_model.R')
source('src/model/eval.coverage_prediction.R')
source('src/model/GPsmoothing_model.R')

tar_option_set(
    library = .libPaths(),
    packages = c(
        "data.table", "fst", "readxl", "stringr", "qs",
        "ggplot2", "cowplot", "hrbrthemes", "wesanderson",
        "forcats", "lubridate", "tools", "tidyr",
        "invgamma", "numDeriv", "mvtnorm", "gamm4"
    ),
    workspace_on_error = TRUE, error = "abridge", format = "qs",
    garbage_collection = TRUE, memory = "transient",
    deployment = "worker", storage = "worker", retrieval = "worker"
)


list(
    # FHM (run new download daily)
    tarchetypes::tar_change(get_fhm_latest, download_latest_fhm(folder = file.path("data", "FHM")), Sys.Date()),
    tarchetypes::tar_change(f_fhm_paths,
        list.files(file.path("data", "FHM"), pattern = "^Folkhalso", full.names = TRUE),
        get_fhm_latest),
    tar_target(f_fhm_files, f_fhm_paths, format = "file", pattern = map(f_fhm_paths)),
    # ECDC
    tarchetypes::tar_download(ecdc,
        urls = "https://opendata.ecdc.europa.eu/covid19/casedistribution/csv",
        paths = file.path("data", "ECDC.csv")),

    tar_target(death_dts, load_fhm_data(f_fhm_files, type = "deaths"), pattern = map(f_fhm_files), format = "fst_dt"),
    tar_target(death_dt, join_data(death_dts), format = "fst_dt"),
    tar_target(icu_dts, load_fhm_data(f_fhm_files, type = "deaths"), pattern = map(f_fhm_files), format = "fst_dt"),
    tar_target(icu_dt, join_data(icu_dts), format = "fst_dt"),
    tar_target(time_to_finished, calculate_lag(death_dt)),
    tar_target(days, day_of_week(death_dt)),


    # Model predictions
    # Constant model
    tar_target(death_prediction_constant, predict_lag(death_dt)),

    # Statistical model
    tar_target(model_death_dt, model_build_death_dt(death_dt)),
    tar_target(model_icu_dt, model_build_icu_dt(icu_dt)),
    tar_target(death_prediction_model_raw, run.model2.all(model_death_dt, model_icu_dt, days.to.pred = 25,prior=F)),
    tar_target(death_prediction_model, gp_smooth(death_prediction_model_raw, model_death_dt)),

    # Save data
    tar_target(f_out_icu_dt, exec_ret_path(icu_dt, fwrite, file.path("data", "covid_icu_latest.csv")), format = "file"),
    tar_target(f_out_death_dt, exec_ret_path(death_dt, fwrite, file.path("data", "covid_deaths_latest.csv")), format = "file"),
    tar_target(f_out_constant_predictions,
        exec_ret_path(
            merge(death_prediction_constant[, -"predicted_deaths"],
                  death_prediction_model[, -c("sure_deaths", "predicted_deaths")],
                  suffixes = c(".constant", ".model"), all = TRUE),
           fwrite, file.path("data", "predictions.csv")), format = "file"),

    # Main Plots
    tar_target(default_theme, set_default_theme()),
    tar_target(death_plot, plot_lagged_deaths(death_dt, death_prediction_model, default_theme, y_max = 150, br_major = 20)),
    tar_target(lag_plot1, plot_lag_trends1(time_to_finished, default_theme)),
    tar_target(lag_plot2, plot_lag_trends2(death_dt, default_theme)),
    tar_target(lag_plot, plot_lag_trends_grid(lag_plot1, lag_plot2, default_theme)),

    # Save main plots
    tarchetypes::tar_change(plot_archive, archive_plots(file.path("docs", "archive"), plots = list(death_plot, lag_plot)), Sys.Date()),
    tar_target(p_out_death_plot, save_plot(death_plot, file.path("docs", paste0("deaths_lag_sweden_", Sys.Date() , ".png"))), format = "file"),
    tar_target(p_out_death_plot_latest, save_plot(death_plot, file.path("docs", paste0("deaths_lag_sweden_latest.png")), bgcolor = "white"), format = "file"),
    tar_target(p_out_lag_plot, save_plot(lag_plot, file.path("docs", paste0("lag_trend_sweden_", Sys.Date() , ".png"))), format = "file"),
    tar_target(p_out_lag_plot_latest, save_plot(lag_plot, file.path("docs", paste0("lag_trend_sweden_latest.png")), bgcolor = "white"), format = "file"),
    tar_target(web_update,
        update_web(plots = list(death_plot, lag_plot),
                   death_plot = file.path("docs", paste0("deaths_lag_sweden_", Sys.Date() , ".png")),
                   lag_plot = file.path("docs", paste0("lag_trend_sweden_", Sys.Date() , ".png")),
                   index = file.path("docs", "index.md")), format = "file"),

    # Evaluation plots
    tar_target(coverage_plot_t0, plot_coverage_eval(death_dt, death_prediction_constant, death_prediction_model, 0, default_theme)),
    tar_target(coverage_plot_t1, plot_coverage_eval(death_dt, death_prediction_constant, death_prediction_model, 1, default_theme)),
    tar_target(coverage_plot_t2, plot_coverage_eval(death_dt, death_prediction_constant, death_prediction_model, 2, default_theme)),
    tar_target(coverage_plot_t3, plot_coverage_eval(death_dt, death_prediction_constant, death_prediction_model, 3, default_theme)),
    tar_target(coverage_plot_t4, plot_coverage_eval(death_dt, death_prediction_constant, death_prediction_model, 4, default_theme)),
    tar_target(coverage_plot_t5, plot_coverage_eval(death_dt, death_prediction_constant, death_prediction_model, 5, default_theme)),

    tar_target(p_out_coverage_plot_t0, save_plot(coverage_plot_t0, file.path("docs", "eval", paste0("coverage_eval_t0.png")), bgcolor = "white"), format = "file"),
    tar_target(p_out_coverage_plot_t1, save_plot(coverage_plot_t1, file.path("docs", "eval", paste0("coverage_eval_t1.png")), bgcolor = "white"), format = "file"),
    tar_target(p_out_coverage_plot_t2, save_plot(coverage_plot_t2, file.path("docs", "eval", paste0("coverage_eval_t2.png")), bgcolor = "white"), format = "file"),
    tar_target(p_out_coverage_plot_t3, save_plot(coverage_plot_t3, file.path("docs", "eval", paste0("coverage_eval_t3.png")), bgcolor = "white"), format = "file"),
    tar_target(p_out_coverage_plot_t4, save_plot(coverage_plot_t4, file.path("docs", "eval", paste0("coverage_eval_t4.png")), bgcolor = "white"), format = "file"),
    tar_target(p_out_coverage_plot_t5, save_plot(coverage_plot_t5, file.path("docs", "eval", paste0("coverage_eval_t5.png")), bgcolor = "white"), format = "file")
)
