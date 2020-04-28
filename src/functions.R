download_latest_fhm <- function(folder = file.path("data", "FHM")) {
    require(readxl)

    DL <- download.file("https://www.arcgis.com/sharing/rest/content/items/b5e7488e117749c19881cce45db13f7e/data",
                        destfile = file.path(folder, "FHM_latest.xlsx"), method = "curl", extra = c("-L"), quiet = TRUE)
    if (DL != 0) { stop("File download error.") }

    # Check archived files for latest record
    latest_record <- max(as.Date(gsub("^.*(2020-[0-9]{2}-[0-9]{2}).xlsx", "\\1", list.files(folder, pattern = "^Folkhalso"))))

    # Check if new download is newer than latest record, in that case, archive it.
    new_record <- get_record_date(file.path(folder, "FHM_latest.xlsx"))

    if (latest_record < new_record) {
        file.copy(file.path(folder, "FHM_latest.xlsx"),
                  file.path("data", "FHM", paste0("Folkhalsomyndigheten_Covid19_", new_record, ".xlsx")))
    }
}

get_record_date <- function(f) {
    sheets <- excel_sheets(f)
    return(as.Date(sub("^FOHM ", "", sheets[length(sheets)]), format="%d %b %Y"))
}

trigger_new_download <- function(f) {
    require(data.table)

    if (!file.exists(f)) {
        return(TRUE)
    }

    latest_record <- get_record_date(f)

    if (latest_record < Sys.Date()) {
        if (as.ITime(Sys.time(), tz = "Europe/Stockholm") > as.ITime("14:00")) {
            return(TRUE)
        }
    }

    return(FALSE)
}

list_fhm_files <- function(folder = file.path("data", "FHM")) {
    list.files(folder, pattern = "^Folkhalso", full.names = TRUE)
}

load_fhm <- function(f) {
    require(data.table)
    require(readxl)

    DT <- data.table((
        read_excel(path = f, sheet = 2, col_types = c("text", "numeric"))
    ))

    setnames(DT, c("date", "N"))

    DT[date == "Uppgift saknas" | date == "uppgift saknas", date := NA]

    if (can_be_numeric(DT[, date])) {
        DT[, date := as.Date(as.numeric(date), origin = "1899-12-30")]
    } else {
        DT[, date := as.Date(date)]
    }

    DT[is.na(N), N := 0]

    DT[, publication_date := get_record_date(f)]

    return(as.data.frame(DT))
}

can_be_numeric <- function(x) {
    # Check if vector can be converted to numeric
    stopifnot(is.atomic(x) || is.list(x)) # check if x is a vector
    numNAs <- sum(is.na(x))
    numNAs_new <- suppressWarnings(sum(is.na(as.numeric(x))))
    return(numNAs_new == numNAs)
}

join_data <- function(death_dts) {
    death_dt <- data.table(death_dts)
    setkey(death_dt, publication_date, date)

    death_dt[!is.na(date) & publication_date > "2020-04-02", days_since_publication := publication_date - date]
    death_dt[date == "2020-04-02" & publication_date == "2020-04-02", days_since_publication := 0]

    death_dt[!is.na(date), paste0("n_m", 1) := shift(N, n = 1, type = "lag", fill = 0L), by = date]
    death_dt[!is.na(date), n_diff := N - n_m1]
    death_dt[!is.na(date) & n_m1 > 0 & !is.na(n_m1), n_diff_pct := N/n_m1 - 1]
    death_dt[!is.na(date) & n_m1 == 0 & N == 0, n_diff_pct := 0]
    death_dt[, n_m1 := NULL]

    # If no death reported on publication date
    for (i in seq_along(unique(death_dt$publication_date))) {
        pub <- unique(death_dt$publication_date)[i]
        if (death_dt[date == publication_date & publication_date == pub, .N] == 0) {
            death_dt <- rbind(death_dt,
                              data.table(date = pub, N = 0,
                                         publication_date = pub,
                                         days_since_publication = as.difftime(0, units = "days"),
                                         n_diff = 0, n_diff_pct = 0))
        }
    }

    # Categorize by grouped days since publication.
    # 1, 2, ... > 1 week, 2 weeks
    death_dt[, delay := as.numeric(days_since_publication)]
    death_dt[delay >= 14, delay := 14]
    death_dt[delay >= 7 & delay < 14, delay := 7]
    death_dt[is.na(delay), delay := -1]
    death_dt[, delay := factor(delay,
                               levels = c(-1, 0, 1, 2, 3, 4, 5, 6, 7, 14),
                               labels = c("No Data", "Same day", "1 Day", "2 Days",
                                          "3-4 Days", "3-4 Days", "5-6 Days",
                                          "5-6 Days", "7-13 Days", "≥14 Days"))]

    return(death_dt)
}

predict_lag <- function(death_dt) {
    # Exclude last 7 days
    DT <- copy(death_dt)

    avg_delay <- DT[date %between% c(Sys.Date() - 21, Sys.Date())  &
                    !is.na(days_since_publication) & days_since_publication != 0,
                    .(avg_diff = mean(n_diff, na.rm = TRUE)), by = days_since_publication]
    setorder(avg_delay, -days_since_publication)
    avg_delay[, sum_cum := cumsum(avg_diff)]
    avg_delay[, match := days_since_publication - 1]

    prediction <- DT[!is.na(date)]

    prediction <- prediction[, .(days_since_publication = max(days_since_publication, na.rm = TRUE), deaths = max(N, na.rm = TRUE)), by = date][order(date)]
    prediction <- merge(prediction, avg_delay[, .(match, sum_cum)], by.x = "days_since_publication", by.y = "match")

    prediction <- prediction[, .(date, sure_deaths = deaths, predicted_deaths = sum_cum, total = deaths + sum_cum)]

    return(prediction)
}

##
# Plots
set_default_theme <- function() {
    require(hrbrthemes)

    theme_ipsum(base_family = "EB Garamond") %+replace%
        theme(
            text = element_text(size = 12, color = "#333333", family = "EB Garamond"),
            plot.title = element_text(size = rel(2), face = "bold", hjust = 0, margin = margin(0,0,5,0)),
            plot.subtitle = element_text(size = rel(1), face = "plain", hjust = 0, margin = margin(0,0,5,0)),
            plot.caption = element_text(size = rel(0.7), family = "EB Garamond", face = "italic", hjust = 1, vjust = 1, margin = margin(12,0,0,0)),
            legend.background = element_rect(fill = "#F5F5F5", color = "#333333"),
            legend.margin = margin(5,5,5,5),
            legend.direction = "vertical",
            legend.position = "right",

            axis.title.y = element_text(size = rel(1.2), face = "bold", angle = 90, hjust = 1, vjust = 1, margin = margin(0,2.88,0,0)),
            axis.title.x = element_text(size = rel(1.2), face = "bold", hjust = 1, vjust = 1, margin = margin(2.88,0,0,0)),
            axis.text.x = element_text(angle = 35, hjust = 1, vjust = 1.1),

            # Panels
            plot.background = element_rect(fill = "transparent", color = NA),
            # plot.background = element_rect(fill = "#f5f5f5", color = NA), # bg of the plot
            panel.border = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.y = element_line(linetype = "dotted", color = "#CCCCCC", size = 0.3),
            panel.grid.minor.y = element_line(linetype = "dotted", color = "#CECECE", size = 0.2)
        )
}

plot_lagged_deaths <- function(death_dt, death_prediction, my_theme) {
    require(ggplot2)
    require(forcats)

    total_deaths <- death_dt[publication_date == max(publication_date, na.rm = TRUE), sum(N, na.rm = TRUE)]
    predicted_deaths <- round(death_prediction[, sum(predicted_deaths)], 0)
    latest_date <- death_dt[, max(publication_date)]

    # Create day of week markers
    days <- unique(death_dt[!is.na(date), .(date, wd = substr(weekdays(date),1, 2), weekend = FALSE)])
    days[wd %in% c("Sa", "Su"), weekend := TRUE]
    days[date %between% c("2020-04-10", "2020-04-13"), weekend := TRUE]

    death_dt[publication_date == "2020-04-02" & is.na(days_since_publication), publication_date := NA]
    date_diff <- death_dt[!is.na(publication_date), sum(n_diff, na.rm = TRUE), by = publication_date]
    death_dt <- death_dt[n_diff != 0 & !is.na(n_diff)]

    # Only one observation per group
    death_dt <- death_dt[, .(n_diff = sum(n_diff, na.rm = TRUE)), by = .(date, delay)]
    levels(death_dt$delay) <- death_dt[, sum(n_diff), delay][order(c(1,2,8,7,6,5,4,3))][, paste0(delay, " (N=", V1, ")")]

    fill_colors <- c("gray40", "#FF0000", "#507159", "#55AC62", "#F2AD00", "#F69100", "#5BBCD6", "#478BAF", "#E1E1E1")
    fill_colors <- setNames(fill_colors, c(levels(death_dt$delay), "Forecast (avg. lag)"))
    death_dt[, delay := forcats::fct_rev(delay)]
    label_order <- c("Forecast (avg. lag)", levels(death_dt$delay))

    # Drop earliest data
    death_dt <- death_dt[date >= "2020-03-12"]
    death_prediction <- death_prediction[date >= "2020-03-12"]
    days <- days[date >= "2020-03-12"]

    ggplot(data = death_dt, aes(y = n_diff, x = date)) +
        geom_hline(yintercept = 0, linetype = "solid", color = "#999999", size = 0.4) +
        geom_bar(data = death_prediction, aes(y = total, fill = "Forecast (avg. lag)"), stat="identity") +
        geom_bar(position = "stack", stat = "identity", aes(fill = delay)) +
        geom_text(data = days, aes(y = -4, label = wd, color = weekend), size = 2.5, family = "EB Garamond", show.legend = FALSE) +
        annotate(geom = "label", fill = "#F5F5F5", color = "#333333",
                 hjust = 0, family = "EB Garamond",
                 label.r = unit(0, "lines"), label.size = 0.5,
                 x = as.Date("2020-03-12"), y = 100,
                 label = paste0("\nReported:                \nPredicted: \nTotal: ")) +
        annotate(geom = "text", color = "#333333", hjust = 1, family = "EB Garamond",
                 x = as.Date("2020-03-18"), y = 100,
                 label = paste0(latest_date, "\n",
                                format(total_deaths, big.mark = ","), "\n",
                                format(predicted_deaths, big.mark = ","), "\n",
                                format(total_deaths + predicted_deaths, big.mark = ","))) +
        scale_color_manual(values = c("black", "red")) +
        scale_fill_manual(values = fill_colors, limits = label_order, drop = FALSE) +
        scale_x_date(date_breaks = "3 day", date_labels = "%b %d", expand = expansion(add = 0.8)) +
        scale_y_continuous(minor_breaks = seq(0,200,10), breaks = seq(0,200,20), expand = expansion(add = c(5, 10))) +
        my_theme +
        labs(title = paste0("Swedish Covid-19 mortality: actual death dates and reporting delay"),
             subtitle = paste0("Each death is attributed to its actual day of death. Colored bars show reporting delay. Negative values indicate data corrections.\n",
                               "Light grey bars show total predicted deaths based on the average lags during the last 3 weeks."),
             caption = paste0("Source: Folkhälsomyndigheten. Updated: ", Sys.Date(), ". Latest version available at https://adamaltmejd.se/covid."),
             fill = "Reporting delay",
             x = "Date of death",
             y = "Number of deaths")
}

plot_lag_trends <- function(death_dt, default_theme) {
    # loadd(death_dt)
    # loadd(default_theme)
    # print(plot_lag_trends(readd(death_dt), readd(default_theme)))
    DT <- copy(death_dt)

    DT <- DT[n_diff > 0 & publication_date > "2020-04-02"]
    DT[, lag := as.numeric(days_since_publication)]

    DT[, perc90_days := quantile(rep(lag, times = n_diff), probs = c(0.90)), by = publication_date]

    # Create day of week markers
    days <- unique(death_dt[!is.na(publication_date), .(publication_date, wd = substr(weekdays(publication_date),1, 2), weekend = FALSE)])
    days[wd %in% c("Sa", "Su"), weekend := TRUE]
    days[publication_date %between% c("2020-04-10", "2020-04-13"), weekend := TRUE]

    colors <- c("#FF0000", "#507159", "#55AC62", "#F2AD00", "#F69100", "#5BBCD6", "#478BAF", "#FF0000", "#000000")
    names <- c(levels(DT$delay)[!grepl("No Data", levels(DT$delay))], "Weekend", "Weekday")
    colors <- setNames(colors, names)
    DT[, delay := forcats::fct_rev(delay)]
    label_order <- c(levels(DT$delay)[!grepl("No Data", levels(DT$delay))], "Weekend", "Weekday")

    g <- ggplot(data = DT,
                aes(x = publication_date, y = lag)) +
        geom_point(aes(size = n_diff, color = delay)) +
        geom_line(aes(y = perc90_days, linetype = "90th Percentile"), color = "#555555", alpha = 0.8) +
        geom_text(data = days[weekend == TRUE], aes(y = -1.5, label = wd), color = "red", size = 2.5, family = "EB Garamond") +
        geom_text(data = days[weekend == FALSE], aes(y = -1.5, label = wd), color = "black", size = 2.5, family = "EB Garamond") +
        scale_x_date(date_breaks = "2 day", date_labels = "%B %d", expand = c(0.05,0.05)) +
        scale_y_continuous(expand = expansion(add = c(1, 0)), breaks = c(7, 14, 21, 28), minor_breaks = c(1, 2, 3, 5)) +
        scale_size(range = c(0.5, 5)) +
        scale_color_manual(values = colors) + #limits = label_order
        scale_linetype_manual(values = c("90th Percentile" = "dashed"), name = "Statistics") +
        default_theme +
        labs(title = paste0("Swedish Covid-19 mortality: delay by report date"),
             subtitle = paste0("Deaths are sorted by report date along horizontal axis. Vertical axis shows delay in number of deaths.\n",
                               "Size of points indicate the number of deaths reported for each day."),
             caption = paste0("Source: Folkhälsomyndigheten. Updated: ", Sys.Date(), ". Latest version available at https://adamaltmejd.se/covid."),
             size = "Number of deaths",
             color = "Reporting delay",
             x = "Report date",
             y = "Reporting delay (days)")
}

calculate_lag <- function(death_dt) {
    # Count a day as "finished" when for the first time 3-day increase is less
    # than 5%.
    finished <- copy(death_dt)
    setorder(finished, date, publication_date)
    finished[!is.na(date), n_3_days_ago := shift(N, n = 3), by = date]
    finished[, chng_3_day := N / n_3_days_ago]
    finished[!is.na(chng_3_day), cummin_increase := cummin(chng_3_day), by = date]
    finished[, finished := FALSE]
    finished[cummin_increase <= 1.05, finished := TRUE] # 5%
    finished[finished == TRUE, finished_date := min(publication_date), by = date]
    finished[finished == TRUE, days_until_finished := min(days_since_publication), by = date]

    finished <- finished[publication_date == max(publication_date),
                         .(date, publication_date, finished, days_until_finished, finished_date)]

    return(finished)

}

archive_plots <- function(out_dir) {
    files <- list.files("docs", pattern = ".png")
    files <- files[!grepl(Sys.Date(), files)]
    files <- files[!grepl("latest", files)]
    file.copy(file.path("docs", files), file.path(out_dir, files), overwrite = TRUE)
    unlink(file.path("docs", files))
}

save_plot <- function(p, f, bgcolor = "transparent") {
    require(ggplot2)
    require(tools)

    h <- 6 # inches
    w <- 11.46 # inches (twitter ratio 1.91:1)

    if (tools::file_ext(f) == "pdf") {
        ggplot2::ggsave(filename = f, plot = p,
           height = h, width = w,
           device = cairo_pdf)
    }
    if (tools::file_ext(f) == "png") {
        ggplot2::ggsave(filename = f, plot = p,
               height = h, width = w, dpi = 400,
               device = png(), type = "cairo",
               bg = bgcolor, canvas = "#f5f5f5")
    }
}

update_web <- function(death_plot, lag_plot, index) {
    lines <- c(
        "---",
        "layout: page",
        "title: Reported Covid-19 deaths in Sweden",
        "author: Adam Altmejd",
        paste0("date: ", Sys.Date()),
        "---\n",
        paste0('![Graph of Swedish Covid-19 deaths with reporting delay.](', basename(death_plot), ' "Swedish Covid-19 deaths.")'),
        paste0('![Graph of Swedish Covid-19 reporting delay in daily deaths.](', basename(lag_plot), ' "Trend in Swedish Covid-19 mortality reporting delay.")'),
        "For code and data, visit <https://github.com/adamaltmejd/covid>."
    )
    con <- file(index, "w")
    writeLines(lines, con = con)
    close(con)
}


