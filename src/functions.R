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

poisson_model <- function(death_dt) {
    loadd(death_dt)
    start <- Sys.Date() - 21

    DT <- death_dt[date >= start & publication_date == max(publication_date),
                   .(date, day_n = as.numeric(date - start), N)]

    DT[, y := N]
    DT[, x1 := day_n]
    DT[, x2 := (x1 ^ 2) / 2]

    fit <- glm(y ~ x1 + x2, family = "poisson", data = DT[day_n %between% c(0, 14)])

    preds <- predict(fit, newdata = DT, type = "response", se.fit = TRUE)

    ret <- data.table(pred = preds$fit, se = preds$se.fit)
    ret[, lwr := pred - qnorm(0.975) * se]
    ret[, upr := pred + qnorm(0.975) * se]

    return(cbind(DT[, .(date, N)], ret[, .(pred, lwr, upr)]))
}

##
# Plots

set_default_theme <- function() {
    require(hrbrthemes)

    fam <- "sans"
    if (font_family_exists(font_family = "Arial")) fam <- "Arial"
    if (font_family_exists(font_family = "EB Garamond")) fam <- "EB Garamond"

    theme_ipsum(base_family = fam) %+replace%
        theme(
            text = element_text(color = "#333333", family = fam),
            plot.title = element_text(size = rel(2), face = "plain", hjust = 0, margin = margin(0,0,5,0)),
            plot.subtitle = element_text(size = rel(1), face = "plain", hjust = 0, margin = margin(0,0,5,0)),
            legend.background = element_rect(fill = "#F5F5F5", color = "#333333"),
            legend.margin = margin(5,5,5,5),
            legend.direction = "vertical",
            legend.position = "right",
            axis.text.x = element_text(angle = 40, hjust = 1, vjust = 1.2),

            # Panels
            plot.background = element_rect(fill = "transparent", color = NA),
            # plot.background = element_rect(fill = "#f5f5f5", color = NA), # bg of the plot
            panel.border = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.y = element_line(linetype = "dotted", color = "#CCCCCC", size = 0.2),
            panel.grid.minor. = element_blank(),
            # panel.grid.minor.y = element_line(linetype = "dotted", color = "grey90", size = 0.2)
        )
}

plot_lagged_deaths <- function(death_dt, death_prediction, my_theme) {
    require(ggplot2)
    require(forcats)

    total_deaths <- death_dt[publication_date == max(date, na.rm = TRUE), sum(N, na.rm = TRUE)]
    predicted_deaths <- round(death_prediction[, sum(predicted_deaths)], 0)

    # Create day of week markers
    days <- unique(death_dt[!is.na(date), .(date, wd = substr(weekdays(date),1, 2), weekend = FALSE)])
    days[wd %in% c("Sa", "Su"), weekend := TRUE]
    days[date %between% c("2020-04-10", "2020-04-13"), weekend := TRUE]

    death_dt[publication_date == "2020-04-02" & is.na(days_since_publication), publication_date := NA]
    date_diff <- death_dt[!is.na(publication_date), sum(n_diff, na.rm = TRUE), by = publication_date]
    death_dt <- death_dt[n_diff != 0 & !is.na(n_diff)]

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
    death_dt[, delay := forcats::fct_rev(delay)]

    # Only one observation per group
    death_dt <- death_dt[, .(n_diff = sum(n_diff, na.rm = TRUE)), by = .(date, delay)]

    # Drop earliest data
    death_dt <- death_dt[date >= "2020-03-12"]
    death_prediction <- death_prediction[date >= "2020-03-12"]
    days <- days[date >= "2020-03-12"]

    fill_colors <- c("No Data" = "gray40",
                     "Avg. historical lag" = "#E1E1E1",
                     "Same day" = "#FF0000",
                     "1 Day" = "#507159",
                     "2 Days" = "#55AC62",
                     "3-4 Days" = "#F2AD00",
                     "5-6 Days" = "#F69100",
                     "7-13 Days" = "#5BBCD6",
                     "≥14 Days" = "#478BAF")
    label_order <- c("Avg. historical lag", "≥14 Days", "7-13 Days", "5-6 Days", "3-4 Days", "2 Days", "1 Day", "Same day", "No Data")

    ggplot(data = death_dt, aes(y = n_diff, x = date)) +
        geom_hline(yintercept = 0, linetype = "solid", color = "#999999", size = 0.4) +
        geom_bar(data = death_prediction, aes(y = total, fill = "Avg. historical lag"), stat="identity") +
        geom_bar(position="stack", stat="identity", aes(fill = delay)) +
        geom_text(data = days, aes(y = -6, label = wd, color = weekend), size = 2.5, family = "EB Garamond", show.legend = FALSE) +
        annotate(geom = "label", fill = "#F5F5F5", color = "#333333",
                 hjust = "left", family = "EB Garamond",
                 label.r = unit(0, "lines"), label.size = 0.5,
                 x = as.Date("2020-03-14"), y = 130,
                 label = paste0("Reported: ", format(total_deaths, big.mark = ","), "\n",
                                "Predicted:    ", format(predicted_deaths, big.mark = ","), "\n",
                                "Total:        ", format(total_deaths + predicted_deaths, big.mark = ","))) +
        scale_color_manual(values = c("black", "red")) +
        scale_fill_manual(values = fill_colors, limits = label_order, drop = FALSE) +
        scale_x_date(date_breaks = "3 day", expand = c(0, 0)) +
        scale_y_continuous(minor_breaks = seq(0,200,10), breaks = seq(0,200,20), expand = expansion(add = c(7,20))) +
        my_theme +
        labs(title = paste0("Swedish Covid-19 mortality: actual death dates and reporting delay"),
             subtitle = paste0("Each death is attributed to its actual day of death. Colored bars show reporting delay. Negative values indicate data corrections.\n",
                               "Light grey bars show total predicted deaths based on the average lags of the last 3 weeks."),
             caption = paste0("Source: Folkhälsomyndigheten. Updated: ", Sys.Date(), ". Latest version available at https://adamaltmejd.se/covid."),
             fill = "Reporting delay",
             x = "Date of death",
             y = "Number of deaths")
}

save_plot <- function(p, f, bgcolor = "transparent") {
    require(ggplot2)
    require(tools)

    h <- 9 # inches
    w <- 16 # inches

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

update_web <- function(plots, index, head) {
    lines <- c(
        "---",
        "layout: page",
        "title: Reported Covid-19 deaths in Sweden",
        "author: Adam Altmejd",
        paste0("date: ", Sys.Date()),
        "---\n",
        paste0('![Graph of Swedish Covid-19 deaths with reporting delay.](', basename(plots), ' "Reporting delay in Swedish covid-19 deaths.")'),
        "For code and data, visit <https://github.com/adamaltmejd/covid>."
    )
    con <- file(index, "w")
    writeLines(lines, con = con)
    close(con)
}


