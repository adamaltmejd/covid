download_latest_fhm <- function(folder = file.path("data", "FHM")) {
    require(readxl)

    DL <- download.file("https://www.arcgis.com/sharing/rest/content/items/b5e7488e117749c19881cce45db13f7e/data",
                        destfile = file.path(folder, "FHM_latest.xlsx"), method = "curl", extra = c("-L"), quiet = TRUE)
    if (DL != 0) { stop("File download error.") }

    # Check archived files for latest record
    latest_record <- max(as.Date(gsub("^.*(202[0-9]-[0-9]{2}-[0-9]{2}).xlsx", "\\1", list.files(folder, pattern = "^Folkhalso"))))

    # Check if new download is newer than latest record, in that case, archive it.
    new_record <- get_record_date(file.path(folder, "FHM_latest.xlsx"))

    if (latest_record < new_record) {
        file.copy(file.path(folder, "FHM_latest.xlsx"),
                  file.path("data", "FHM", paste0("Folkhalsomyndigheten_Covid19_", new_record, ".xlsx")))
    }
}

get_remote_data <- function(url, f) {
    require(curl)
    require(data.table)

    DT <- fread(url)
    fwrite(DT, f)
    return(DT)
}

get_ecdc <- function(url, f) {
    require(curl)
    require(data.table)

    DT <- tryCatch({
        DT <- fread(url)
        fwrite(DT, f)
        return(DT)
    }, error = function(cond) {
        warning("ECDC get_remote data error")
        return(fread(f))
    })

    return(DT)
}

get_record_date <- function(f) {
    sheets <- excel_sheets(f)
    ret <- as.Date(sub("^FOHM ", "", sheets[length(sheets)]), format="%d %b %Y")
    if (is.na(ret)) ret <- as.Date(sub("^FOHM ", "", sheets[grep("FOHM", sheets)]), format="%d %b %Y")
    if (length(ret) > 0) return(ret)
    return(as.Date(NA))
}

trigger_new_download <- function(f, type = "FHM") {
    require(data.table)

    if (!file.exists(f)) {
        return(TRUE)
    }

    if (type == "FHM") latest_record <- get_record_date(f)
    if (type == "SocStyr") latest_record <- max(fread(f)$date, na.rm = TRUE)

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

load_fhm_data <- function(f, type) { # type %in% c("deaths", "icu")
    require(data.table)
    require(readxl)
    require(stringr)
    require(lubridate)

    publication_date <- get_record_date(f)
    file_date <- as.Date(str_extract(f, "[0-9]{4}-[0-9]{2}-[0-9]{2}"))

    if (is.na(publication_date)) {
        publication_date <- file_date
    } else {
        if (publication_date != file_date) { warning("Pub date not file date: [", f, "].") }
    }

    # Skip early reports that do not contain death data
    if (type == "deaths") start_date <- as.Date("2020-04-01")
    if (type == "icu") start_date <- as.Date("2020-04-24")
    if (publication_date <= start_date) return(NULL)

    if (type == "deaths") sheet_n <- 2
    if (type == "icu") {
        sheets <- excel_sheets(f)
        sheet_n <- grep("intensivvårdade", sheets)
    }

    DT <- data.table((
        read_excel(path = f, sheet = sheet_n, col_types = c("text", "numeric"))
    ))

    setnames(DT, c("date", "N"))
    DT[(tolower(date) %in% c("uppgift saknas", "uppgift saknaa", "uppgift saknas+a1")), date := NA]

    if (can_be_numeric(DT[, date])) {
        DT[, date := as.Date(as.numeric(date), origin = "1899-12-30")]
    } else {
        DT[, date := as.Date(date)]
    }

    # Ensure starting point is March 1st, and that all dates have a value
    date_seq <- seq.Date(as.Date("2020-03-01"), publication_date, by = 1)

    DT <- merge(DT, data.table(date = date_seq), all = TRUE)
    DT[is.na(N), N := 0]
    DT[, publication_date := publication_date]
    setkey(DT, publication_date, date)

    return(as.data.frame(DT))
}

update_socstyr <- function(f = file.path("data", "Socialstyrelsen_latest.csv")) {
    require(data.table)
    require(rvest)
    require(jsonlite)
    require(magrittr)
    require(stringr)

    page <- tryCatch(
        read_html("https://www.socialstyrelsen.se/statistik-och-data/statistik/statistik-om-covid-19/sammanfattande-statistik-over-tid/"),
        error = function(e) {
            warning("Error downloading from Socialstyrelsen: ", e)
            return(NULL)
        }
    )

    if (!is.null(page)) {
        data <- page %>%
            html_nodes("iframe") %>%
            extract(2) %>%
            html_attr("src") %>%
            read_html()

        # Follow wrappers until at real page
        while (TRUE) {
            data <- data %>% html_elements(xpath = '//meta[@http-equiv="REFRESH"]') %>%
                html_attr("content") %>%
                sub("0; url=", "", .) %>%
                read_html()

            if (length(data %>% html_elements(xpath = '//meta[@http-equiv="REFRESH"]')) == 0) {
                break
            }
        }

        data_json <- data %>%
            html_nodes("script") %>%
            extract(2) %>%
            html_text() %>%
            substr(., str_locate(., "\\{")[1], str_locate_all(., "\\}")[[1]][length(str_locate_all(., "\\}")[[1]])]) %>%
            gsub("\\\\\\\"", "\\\"", .) %>%
            gsub("\\\\\"", "\"", .) %>%
            parse_json

        data_delim <- data_json$data
        if ("chartData" %in% names(data_delim)) data_delim <- data_json$chartData
        data_delim <- gsub("\\\\n", "\n", data_delim)
        data_delim <- gsub("\\\\t", "\t", data_delim)
        data_delim <- gsub("\\\\r", "\t", data_delim)

        if (str_count(data_delim, "\t") > str_count(data_delim, ",")) {
            sep <- "\t"
        } else {
            sep <- ","
        }

        DT <- fread(text = data_delim, sep = sep,
                    na.strings = c("-", "\"-\""), fill = TRUE)

        setnames(DT,
                 c("datum",
                   "Inskrivna i slutenvård - rullande medelvärde",
                   "Inskrivna i slutenvård - antal",
                   "Inskrivna i intensivvård - rullande medelvärde",
                   "Inskrivna i intensivvård - antal",
                   "Avlidna - rullande medelvärde",
                   "Avlidna - antal",
                   "Smittade 70+ särskilt boende - rullande medelvärde",
                   "Smittade 70+ särskilt boende - antal"),
                 c("date",
                   "hospital_7day_avg", "hospital_n",
                   "icu_7day_avg", "icu_n",
                   "dead_7day_avg", "dead_n",
                   "infected_eldercare_7day_avg", "infected_eldercare_n"))

        DT[, date := as.Date(date)]
        DT <- DT[!is.na(date)]

        setkey(DT, date)
        fwrite(DT, f)
        return(DT)
    } else {
        return(fread(f))
    }
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

    first_pub_date <- death_dt[, min(publication_date, na.rm = TRUE)]
    death_dt[!is.na(date) & publication_date > first_pub_date, days_since_publication := publication_date - date]
    death_dt[date == first_pub_date & publication_date == first_pub_date, days_since_publication := 0]

    death_dt[!is.na(date), paste0("n_m", 1) := shift(N, n = 1, type = "lag", fill = 0L), by = date]
    death_dt[!is.na(date), n_diff := N - n_m1]
    death_dt[!is.na(date) & n_m1 > 0 & !is.na(n_m1), n_diff_pct := N/n_m1 - 1]
    death_dt[!is.na(date) & n_m1 == 0 & N == 0, n_diff_pct := 0]
    death_dt[, n_m1 := NULL]

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
                                          "5-6 Days", "7-13 Days", "14 Days +"))]

    return(death_dt)
}

predict_lag <- function(death_dt) {
    # Calculate the average lag from the last 14 days for each report
    # So when calculating the average number of deaths added on day 3,
    # include day-3 reports from two weeks back from that date.
    DT <- death_dt[days_since_publication != 0 &
                   !is.na(days_since_publication) &
                   !is.na(date) &
                   date >= "2020-04-02",
                   .(date, publication_date, days_since_publication, N, n_diff)]

    # Add missing report dates (FHM does not report every day)
    all_dates <- CJ(date = seq.Date(DT[, min(date)], DT[, max(date)], by = 1),
                    publication_date = seq.Date(DT[, min(publication_date)], DT[, max(publication_date)], by = 1))
    all_dates <- all_dates[date <= publication_date]
    DT <- merge(DT, all_dates, all = TRUE)
    DT[, days_since_publication := publication_date - date]

    # Set n_diff to zero when there was no report
    DT[is.na(n_diff), n_diff := 0]
    DT[, N := nafill(N, type = "locf"), by = .(date)]
    DT[is.na(N), N := 0]

    # Create predictions for each publication date so we can track and evaluate
    # historical predictions
    report_dates <- seq(as.Date("2020-04-14"), death_dt[, max(publication_date)], 1)
    dts <- vector(mode = "list", length = length(report_dates))
    for (i in seq_along(report_dates)) {
        avg_delay <- DT[publication_date <= report_dates[i]]

        # For each delay day, calculate the mean additional deaths added
        # for the two weeks of reports preceding that date.
        # --> If we are calculating deaths added 7 days after, we look at
        #     deaths added 1 week ago back to deaths added 3 weeks ago.
        #     This way, we always take the mean of 2 weeks of reports (when available).
        avg_delay[, ref_date := max(date), by = days_since_publication]
        dts[[i]] <- avg_delay[date %between% list(ref_date - 14, ref_date),
                              .(avg_diff = mean(n_diff, na.rm = TRUE),
                                sd_diff = sd(n_diff, na.rm = TRUE)),
                              by = days_since_publication]
    }

    names(dts) <- report_dates
    avg_delay <- rbindlist(dts, idcol = "publication_date")
    avg_delay[, publication_date := as.Date(publication_date)]

    setkey(avg_delay, publication_date, days_since_publication)

    # To create actual predictions of totals per day
    # we need to add the averages to the reported data
    predictions <- avg_delay[DT[date >= "2020-04-02" & publication_date >= "2020-04-14"],
              on = .(publication_date,
                     days_since_publication > days_since_publication),
              by = .EACHI,
              .(date,
                sure_deaths = N,
                predicted_deaths = sum(avg_diff, na.rm = TRUE),
                predicted_deaths_SD = sqrt(sum(sd_diff^2, na.rm = TRUE)))] # assuming independently normal

    setnames(predictions, "publication_date", "prediction_date")
    predictions[, total := sure_deaths + predicted_deaths]

    # CIs
    predictions[, total_lCI := total - 1.96 * predicted_deaths_SD]
    predictions[, total_uCI := total + 1.96 * predicted_deaths_SD]
    predictions[, predicted_deaths_SD := NULL]

    # Assume no more deaths after 28 days just to have a cleaner data set
    predictions <- predictions[days_since_publication <= 30]
    predictions[, days_since_publication := NULL]

    setkey(predictions, prediction_date, date)

    return(predictions)
}

calculate_lag <- function(death_dt, thresholds = c(0, 0.01, 0.02, 0.05, 0.10)) {
    # Count as finished when for 3 consecutive days, the daily increase is below threshold
    DT <- copy(death_dt)[!is.na(date) & date >= "2020-04-02"]
    setorder(DT, date, publication_date)

    # Calculate threshold values
    # As max of daily increase over last 3 days
    # DT[!is.na(date), paste0("n_diff_pct_m", 1:3) := shift(n_diff_pct, n = 1:3), by = date]
    # DT[, max_diff := pmax(n_diff_pct, n_diff_pct_m1, n_diff_pct_m2, n_diff_pct_m3, na.rm = TRUE)]
    # DT[!is.na(max_diff), forcing_var := cummin(max_diff), by = date]

    # Or as total 3-day increase
    DT[!is.na(date), paste0("N_m", 3) := shift(N, n = 3), by = date]
    DT[, forcing_var := (N / N_m3) - 1]

    # Often nothing is added in the first days, ensure its not counted as finished
    DT[days_since_publication %in% c(0,1,2,3) & is.na(forcing_var), forcing_var := Inf]

    DT <- DT[, .(publication_date, date, n_diff, forcing_var, days_since_publication)]
    setkey(DT, publication_date, date)

    for (t in thresholds) {
        DT[, finished := FALSE]
        DT[forcing_var <= t, finished := TRUE]

        # Days until finished
        DT[DT[finished == TRUE, min(as.numeric(days_since_publication), na.rm = TRUE), by = date],
           paste0("days_to_finished_", 100 * t) := i.V1, on = .(date)]

        # Rolling average
        DT[, paste0("days_to_finished_", 100 * t, "_avg") :=
            frollmean(get(paste0("days_to_finished_", 100 * t)), 4, algo = "exact", align = "center")]

        # Error (number of deaths added after tagged as finished)
        DT[DT[finished == TRUE, sum(n_diff, na.rm = TRUE), by = date],
           paste0("N_added_after_finished_", 100 * t) := i.V1, on = .(date)]

    }

    DT <- DT[publication_date == max(publication_date, na.rm = TRUE)]
    DT[, c("finished", "n_diff", "forcing_var", "publication_date", "days_since_publication") := NULL]

    setkey(DT, date)
    return(DT)
}

day_of_week <- function(death_dt) {
    # Create day of week markers
    days <- unique(death_dt[!is.na(date), .(date, wd = substr(weekdays(date),1, 1), weekend = FALSE)])
    days[wd %in% c("S", "S"), weekend := TRUE]
    days[date %between% c("2020-04-10", "2020-04-13"), weekend := TRUE]
    days[date == "2020-05-01", weekend := TRUE]
    days[date == "2020-05-21", weekend := TRUE]

    return(days)
}

##
# Plots
set_default_theme <- function() {
    require(ggplot2)
    require(hrbrthemes)

    theme_ipsum(base_family = "EB Garamond") %+replace%
        theme(
            text = element_text(size = 12, color = "#333333", family = "EB Garamond"),
            plot.title = element_text(size = rel(2), face = "plain", hjust = 0, margin = margin(0,0,5,0)),
            plot.subtitle = element_text(size = rel(1), face = "plain", hjust = 0, margin = margin(0,0,5,0)),
            plot.caption = element_text(size = rel(0.7), family = "EB Garamond", face = "italic", hjust = 1, vjust = 1, margin = margin(12,0,0,0)),

            legend.text = element_text(size = rel(0.9), family = "EB Garamond", hjust = 0, margin = margin(0, 0, 0, 0)),
            legend.background = element_rect(fill = "#F5F5F5", color = "#333333"),
            legend.margin = margin(5,5,5,5),
            legend.direction = "vertical",
            legend.position = "right",
            legend.justification = "left",
            legend.box.margin = margin(0,0,0,0),
            legend.box.just = "left",

            axis.title.y = element_text(size = rel(1.2), face = "plain", angle = 90, hjust = 1, vjust = 1, margin = margin(0, 4, 0, 0)),
            axis.title.x = element_text(size = rel(1.2), face = "plain", hjust = 1, vjust = 1, margin = margin(4, 0, 0, 0)),
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

plot_lagged_deaths <- function(death_dt, death_prediction_model = NULL,
                               default_theme, custom_labs = NULL, y_max = NULL, br_major = NULL) {
    require(ggplot2)
    require(forcats)

    first_pub_date <- death_dt[, min(publication_date, na.rm = TRUE)]
    latest_date <- death_dt[, max(publication_date, na.rm = TRUE)]
    total_deaths <- death_dt[publication_date == latest_date, sum(N, na.rm = TRUE)]

    death_dt <- death_dt[date >= "2020-03-12"]

    if (is.null(y_max)) {
        y_max <- ceiling((death_dt[, max(N)]/100)) * 100
    }
    if (is.null(br_major)) {
        br_major <- 10^nchar(y_max) / 100
    }

    # Only one observation per group
    death_dt[publication_date == first_pub_date & is.na(days_since_publication), publication_date := NA]
    death_dt <- death_dt[n_diff != 0 & !is.na(n_diff)]
    death_dt <- death_dt[, .(n_diff = sum(n_diff, na.rm = TRUE)), by = .(date, delay)]

    # Add numbers to labels
    n_labs <- merge(
        data.table(delay = levels(death_dt$delay)),
        death_dt[, sum(n_diff), delay],
        all = TRUE, sort = FALSE
    )
    n_labs[is.na(V1), V1 := 0]
    levels(death_dt$delay) <- n_labs[, paste0(delay, " (N=", V1, ")")]

    fill_colors <- c("gray40", "#FF0000", "#507159", "#55AC62", "#F2AD00", "#F69100", "#5BBCD6", "#478BAF", "#E1E1E1")
    fill_colors <- setNames(fill_colors, c(levels(death_dt$delay), "Model nowcast"))
    death_dt[, delay := forcats::fct_rev(delay)]
    label_order <- levels(death_dt$delay)

    # Prediction
    if (!is.null(death_prediction_model)) {
        death_prediction_model <- death_prediction_model[prediction_date == latest_date]
        predicted_deaths <- round(death_prediction_model[, sum(predicted_deaths)], 0)
        predicted_deaths_lCI <- round(death_prediction_model[, sum(total_lCI - sure_deaths)], 0)
        predicted_deaths_uCI <- round(death_prediction_model[, sum(total_uCI - sure_deaths)], 0)

        label_order <- c("Model nowcast", label_order)
    }

    ######
    # Plot

    # Main part
    p <- ggplot(data = death_dt, aes(y = n_diff, x = date)) +
        geom_hline(yintercept = 0, linetype = "solid", color = "#999999", size = 0.4) +
        geom_bar(position = "stack", stat = "identity", aes(fill = delay), width = 1)

    # Label
    lab <- paste0(latest_date, "\n",
                  "Reported:  ", format(total_deaths, big.mark = ","))

    # Add prediction
    if (!is.null(death_prediction_model)) {
        p <- p +
            geom_linerange(data = death_prediction_model, aes(y = total, ymin = total_lCI, ymax = total_uCI),
                        color = "#999999", size = 0.5) +
            geom_point(data = death_prediction_model, aes(y = total), color = "#888888", size = 0.2)

        lab <- paste0(lab, "\n",
                      "Predicted:  ", format(predicted_deaths_lCI, big.mark = ","), " - ", format(predicted_deaths_uCI, big.mark = ","), "\n",
                      "Total:         ", format(total_deaths + predicted_deaths_lCI, big.mark = ","), " - ", format(total_deaths + predicted_deaths_uCI, big.mark = ","))
    }

    # Add Annotations and styling
    p <- p +
        annotate(geom = "label", fill = "#F5F5F5", color = "#333333",
                 hjust = 0, family = "Eb Garamond",
                 label.r = unit(0, "lines"), label.size = 0.5,
                 x = as.Date("2020-07-01"), y = 2 * y_max / 3,
                 label = lab) +
        scale_fill_manual(values = fill_colors, limits = label_order, drop = FALSE) +
        scale_x_date(date_breaks = "1 month", date_labels = "%B", expand = expansion(add = 0)) +
        scale_y_continuous(minor_breaks = seq(0, y_max, br_major / 2),
                           breaks = seq(0, y_max, br_major),
                           expand = expansion(add = c(0, 10)),
                           sec.axis = dup_axis(name=NULL)) +
        default_theme
    if (!is.null(custom_labs)) {
        p <- p + custom_labs
    } else {
        p <- p +
            labs(title = paste0("Confirmed daily Covid-19 deaths in Sweden"),
                subtitle = paste0("Each death is attributed to its actual day of death. Colored bars show reporting delay. Negative values indicate data corrections by FHM.\n",
                                "Gray bars show 95% credible intervals for predicted actual deaths, with points at the median."),
                caption = paste0("Source: Folkhälsomyndigheten and ECDC. Updated: ", Sys.Date(), ". Latest version available at https://adamaltmejd.se/covid."),
                fill = "Reporting delay",
                x = "Date of death",
                y = "Number of deaths")
    }

    return(p)
}

plot_lag_trends1 <- function(time_to_finished, default_theme) {
    DT <- time_to_finished[, c(1, grep("days_to_finished_[0-9]+_avg$", names(time_to_finished))), with = FALSE]
    DT <- melt(DT, id.vars = "date", variable.factor = FALSE)
    DT <- DT[!is.na(value)]

    DT[, variable := factor(as.numeric(gsub("[a-z_]*", "", variable)))]
    levels(DT$variable) <- paste0(levels(DT$variable), "%")

    ggplot(data = DT, aes(x = date, y = value)) +
        geom_line(aes(group = variable, color = variable), linetype = "twodash", size = 0.9, alpha = 0.8) +
        scale_x_date(date_breaks = "1 month", date_labels = "%B", expand = c(0.02,0.02)) +
        scale_y_continuous(limits = c(-1, 32), expand = expansion(add = c(0, 0)), breaks = c(7, 14, 21, 28), minor_breaks = NULL) +
        scale_color_manual(values = wes_palette("Darjeeling2"), guide = guide_legend(title.position = "top")) +
        default_theme +
        theme(legend.direction = "horizontal",
              legend.position = c(0.4, 0.8), legend.justification = "center",
              panel.grid.major.x = element_line(linetype = "dotted", color = "#CCCCCC", size = 0.3),
              panel.grid.minor.x = element_line(linetype = "dotted", color = "#CECECE", size = 0.2)) +
        labs(color = "Completed = days until 3-day change is below:",
             x = "Death date",
             y = 'Days until date is "completed"')
}

plot_lag_trends2 <- function(death_dt, default_theme) {
    DT <- copy(death_dt)

    DT <- DT[n_diff > 0 & publication_date > "2020-04-02"]
    DT[, lag := as.numeric(days_since_publication)]

    DT[, perc90_days := quantile(rep(lag, times = n_diff), probs = c(0.90)), by = publication_date]

    colors <- c("#FF0000", "#507159", "#55AC62", "#F2AD00", "#F69100", "#5BBCD6", "#478BAF", "#FF0000", "#000000")
    names <- c(levels(DT$delay)[!grepl("No Data", levels(DT$delay))], "Weekend", "Weekday")
    colors <- setNames(colors, names)
    DT[, delay := forcats::fct_rev(delay)]
    label_order <- c(levels(DT$delay)[!grepl("No Data", levels(DT$delay))], "Weekend", "Weekday")

    g <- ggplot(data = DT[lag <= 30],
                aes(x = publication_date, y = lag)) +
        geom_point(aes(size = n_diff / 2, color = delay)) +
        geom_line(aes(y = perc90_days, linetype = "90th Percentile"), color = "#555555", alpha = 0.8) +
        scale_x_date(date_breaks = "1 month", date_labels = "%B", expand = c(0.02,0.02)) +
        scale_y_continuous(limits = c(-1, 32), expand = expansion(add = c(0, 0)), breaks = c(7, 14, 21, 28), minor_breaks = c(1, 2, 3, 5)) +
        scale_size(range = c(0.5, 5)) +
        scale_color_manual(values = colors) + #limits = label_order
        scale_linetype_manual(values = c("90th Percentile" = "dashed"), name = "Statistics") +
        default_theme +
        labs(size = "Number of deaths",
             color = "Reporting delay",
             x = "Report date",
             y = "Reporting delay (days)")
}

plot_lag_trends_grid <- function(lag_plot1, lag_plot2, default_theme) {
    lag_plot1 <- lag_plot1 + theme(plot.margin = margin(0,-5,0,30))
    lag_plot2 <- lag_plot2 + theme(plot.margin = margin(0,30,0,-5))
    pgrid <- plot_grid(lag_plot1, lag_plot2,
                       rel_widths = c(1, 1.5),
                       align = "hv", axis = "bt")

    title_theme <- calc_element("plot.title", default_theme)
    title <- ggdraw() +
        draw_label("Swedish Covid-19 reporting delay",
                   fontface = title_theme$face, fontfamily = title_theme$family,
                   size = title_theme$size, lineheight = title_theme$lineheight,
                   x = 0, hjust = 0, y = 0) +
        theme(plot.margin = margin(30, 30, 0, 65))

    subtitle_theme <- calc_element("plot.subtitle", default_theme)
    subtitle <- ggdraw() +
        draw_label(paste0("Left: Length of delay per death date, measured as the number of days until date is completed.\n",
                          "Right: Shows how far back in time each daily report adds deaths. Point size is number of deaths added."),
                   fontface = subtitle_theme$face, fontfamily = subtitle_theme$family,
                   size = subtitle_theme$size, lineheight = subtitle_theme$lineheight,
                   x = 0, hjust = 0, y = 0) +
        theme(plot.margin = margin(0, 30, 15, 65))

    caption_theme <- calc_element("plot.caption", default_theme)
    caption <- ggdraw() +
        draw_label(
            paste0("Source: Folkhälsomyndigheten. Updated: ", Sys.Date(), ". Latest version available at https://adamaltmejd.se/covid."),
            fontface = caption_theme$face, fontfamily = caption_theme$family,
            size = caption_theme$size, lineheight = caption_theme$lineheight,
            color = caption_theme$colour,
            hjust = 1, x = 1
        ) +
        theme(plot.margin = margin(10, 134.5, 30, 30))

    pgrid_labels <- plot_grid(title, subtitle, pgrid, caption, ncol = 1, rel_heights = c(0.1, 0.105, 0.74, 0.09))

    return(pgrid_labels)
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
        cowplot::ggsave2(filename = f, plot = p,
           height = h, width = w,
           device = cairo_pdf)
    }
    if (tools::file_ext(f) == "png") {
        cowplot::ggsave2(filename = f, plot = p,
               height = h, width = w, dpi = 300,
               device = grDevices::png(), type = "cairo",
               bg = bgcolor, canvas = "#f5f5f5")
    }
}

plot_coverage_eval <- function(death_dt, death_prediction_constant, death_prediction_model, days.ago = 0, default_theme) {
    if (death_prediction_constant[, max(prediction_date)] !=
        death_prediction_model[, max(prediction_date)]) stop("Prediction dates off.")

    DT <- rbind(
        data.table(type = "constant", death_prediction_constant[date >= "2020-04-20"]),
        data.table(type = "model", death_prediction_model[date >= "2020-04-20"][, -"obs"]),
        use.names = TRUE
    )
    # days.off sets which prediction day to use
    DT <- DT[prediction_date == date + days.ago]
    DT <- DT[, -c("prediction_date", "sure_deaths", "predicted_deaths")]

    death_dt <- death_dt[publication_date == death_prediction_model[, max(prediction_date)], .(date, N)]
    death_dt[, avg_N := frollmean(N, 7, algo = "exact", align = "center")]
    death_dt[, state := "Finished"]
    death_dt[date >= death_prediction_model[, max(prediction_date)] - 21, state := "Still reporting"]

    DT <- merge(DT, death_dt[date >= "2020-05-01"], by = "date")

    DT[month(date) == 12 & type == "constant"]
    DT[month(date) == 12 & type == "model"]

    coverage <- DT[state == "Finished", mean(N %between% list(total_lCI, total_uCI)), by = type]
    lmiss <- DT[state == "Finished", mean(N < total_lCI), by = type]
    umiss <- DT[state == "Finished", mean(N > total_uCI), by = type]
    DT[type == "constant", type := paste0("Constant [coverage=", round(coverage[type == "constant", V1], 2)*100, "% (lmiss=", round(lmiss[type == "constant", V1], 2), ", umiss=", round(umiss[type == "constant", V1], 2), ")]")]
    DT[type == "model", type := paste0("Model [coverage=", round(coverage[type == "model", V1], 2)*100, "% (lmiss=", round(lmiss[type == "model", V1], 2), ", umiss=", round(umiss[type == "model", V1], 2), ")]")]
    DT[, type := factor(type)]

    ggplot(data = DT, aes(x = date, y = total)) +
        geom_line(aes(y = avg_N, linetype = state)) +
        geom_line(aes(color = type)) +
        geom_ribbon(aes(ymin = total_uCI, ymax = total_lCI, fill = type),
                    alpha = 0.3) +
        scale_color_manual(values = wes_palette("Darjeeling2"), guide = guide_legend(legend.position = "top")) +
        scale_fill_manual(values = wes_palette("Darjeeling2"), guide = "none") +
        scale_x_date(date_breaks = "1 month", date_labels = "%B", expand = expansion(add = 0)) +
        scale_y_continuous(limits = c(-10, 160), minor_breaks = seq(0,160,10), breaks = seq(0,160,20), expand = expansion(add = c(10, 10))) +
        default_theme +
        theme(legend.direction = "horizontal", legend.position = "bottom",
              legend.justification = "center", legend.title = element_blank()) +
        labs(
            title = paste0("Nowcast evaluation: prediction t=", days.ago, " days back."),
            subtitle = paste0("Black line shows actual number of deaths, dashed for the last 21 days where numbers are still being updated. Shaded areas show 95% CI."),
            caption = paste0("Last day included = ", max(DT$date), "."),
            linetype = "",
            color = "",
            x = "Date",
            y = "Number of deaths"
        )
}

deaths_icu_hospital_corr_plot <- function(model_death_dt, socstyr_dt, default_theme) {
    require(data.table)
    require(gamm4)
    require(ggplot2)
    require(wesanderson)
    require(hrbrthemes)
    lag <- 7
    delay <- 20

    n = dim(socstyr_dt)[1]
    slutenvard <- as.numeric(socstyr_dt$hospital_n)
    intensvard <- as.numeric(socstyr_dt$icu_n)
    elder     <- as.numeric(socstyr_dt$infected_eldercare_n)
    days       <- 1:n

    ##
    # the three covariates
    data.sluten <- data.frame(days = days[!is.na(slutenvard)],
                              date = as.Date(socstyr_dt$date[!is.na(slutenvard)]),
                              y    = slutenvard[!is.na(slutenvard)])
    data.intensvard <- data.frame(days = days[!is.na(intensvard)],
                                  date = as.Date(socstyr_dt$date[!is.na(intensvard)]),
                                  y    = intensvard[!is.na(intensvard)])
    data.elder <- data.frame(days = days[!is.na(elder)],
                             date = as.Date(socstyr_dt$date[!is.na(elder)]),
                             y    = elder[!is.na(elder)])

    model.sluten   <- gam(y ~ s(days), data=data.sluten, family = nb(),method="REML")
    model.iva      <- gam(y ~ s(days), data=data.intensvard, family = nb(),method="REML")
    model.elder    <- gam(y ~ s(days), data=data.elder, family = nb(),method="REML")

    model.covs <- list(sluten = model.sluten,
                       iva    = model.iva,
                       elder  = model.elder)
    cov.data <- list()
    for(i in 1:length(model.covs)) {
        cov.data[[i]] <- data.frame(date = as.Date(socstyr_dt$date),
                                    theta = predict(model.covs[[i]],
                                    newdata = data.frame(days = days)))
    }

    #number of dead
    Y <- apply(model_death_dt$detected, 1, max, na.rm=T)
    death.list = data.frame(date = as.Date(model_death_dt$dates), death = Y)

    # analys lag fit
    lags <- 1:20
    lags.model <- list(sluten = rep(NA,length(lags)),
                    iva    = rep(NA,length(lags)),
                    elder  = rep(NA,length(lags)))
    lags.sluten <- rep(NA, length(lags))
    models.selected <- list()
    for(i in 1:length(lags)) {
        lag <- lags[i]
        for(j in 1:length(lags.model)) {
            cov.data_j <- cov.data[[j]]
            cov.data.lag = data.frame( date   = c(cov.data_j$date, cov.data_j$date[n]+1:lag),
                                    theta  = c(rep(0,lag),cov.data_j$theta))

            data.total <- merge(cov.data.lag,death.list, by ="date")
            # remove fist 30 days and last 20
            data.total.rem <- data.total[30:(dim(data.total)[1]-delay),]
            model.fit <- gam(death~  theta, data=data.total.rem, family = nb(),method="REML" )
            lags.model[[j]][i] <- -model.fit$aic
            if(-model.fit$aic == max(lags.model[[j]], na.rm=T)) {
                model.fit$date <- data.total.rem$date
                models.selected[[j]] <- model.fit
            }
        }
    }

    DT <- data.table(data.total.rem)[, -"theta"]
    sluten <- data.table(date = models.selected[[1]]$date, y = exp(predict(models.selected[[1]])), model = paste0("N hospitalized [lag=", which.max(lags.model$sluten), "]"))
    iva <- data.table(date = models.selected[[2]]$date, y = exp(predict(models.selected[[2]])), model = paste0("N in ICU [lag=", which.max(lags.model$iva), "]"))
    elder <- data.table(date = models.selected[[3]]$date, y = exp(predict(models.selected[[3]])), model = paste0("N infected eldercare [lag=", which.max(lags.model$elder), "]"))

    DT <- merge(DT, rbindlist(list(sluten, iva, elder)), by = "date", all = TRUE)

    p <- ggplot(data = DT, aes(x = date, y = y, group = model)) +
        geom_point(aes(y = death), color = "grey70", alpha = 0.5, size = 0.4) +
        geom_line(aes(color = model)) +
        default_theme +
        scale_color_manual(values = wes_palette("Darjeeling2"), guide = guide_legend(legend.position = "top")) +
        scale_x_date(date_breaks = "1 month", date_labels = "%B", expand = expansion(add = 0)) +
        labs(
            title = paste0("Model prior evaluation vs actual deaths."),
            color = "Models",
            x = "Date",
            y = "Number of deaths"
        )

    return(p)
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
        "For code and data, visit <https://github.com/adamaltmejd/covid>.",
        "Evaluations of the statistical model and the old constant average forecast are available here: <https://github.com/adamaltmejd/covid/tree/master/docs/eval>.",
        "For an indepth explanation and evaluation of the nowcasting model, see <https://arxiv.org/abs/2006.06840>."
    )
    con <- file(index, "w")
    writeLines(lines, con = con)
    close(con)
}
