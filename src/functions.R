download_latest_fhm <- function(f) {
    require(readxl)

    DL <- download.file("https://www.arcgis.com/sharing/rest/content/items/b5e7488e117749c19881cce45db13f7e/data",
                        destfile = file.path(f), method = "curl", extra = c("-L"), quiet = TRUE)
    if (DL != 0) { stop("File download error.") }

    # Check archived files for latest record
    latest_record <- max(as.Date(gsub("^.*(2020-[0-9]{2}-[0-9]{2}).xlsx", "\\1", list.files(file.path("data", "FHM"), pattern = "^Folkhalso"))))

    # Check if new download is newer than latest record, in that case, archive it.
    new_record <- max(load_fhm(f)$publication_date)

    if (latest_record < new_record) { file.copy(f, file.path("data", "FHM", paste0("Folkhalsomyndigheten_Covid19_", new_record, ".xlsx"))) }
}

trigger_new_download <- function(f) {
    require(data.table)

    if (!file.exists(f)) {
        return(TRUE)
    }

    latest_record <- max(load_fhm(f)$publication_date)

    if (latest_record < Sys.Date()) {
        if (as.ITime(Sys.time(), tz = "Europe/Stockholm") > as.ITime("14:00")) {
            return(TRUE)
        }
    }

    return(FALSE)
}

load_fhm <- function(f) {
    require(data.table)
    require(readxl)

    DT <- data.table((
        read_excel(path = f, sheet = 2, col_types = c("text", "numeric"))
    ))

    setnames(DT, c("date", "N"))

    DT <- DT[!is.na(date) & date != "Uppgift saknas"] # drop deaths with no date assigned

    if (can_be_numeric(DT[, date])) {
        DT[, date := as.Date(as.numeric(date), origin = "1899-12-30")]
    } else {
        DT[, date := as.Date(date)]
    }

    DT[is.na(N), N := 0]

    DT[, publication_date := max(date)]

    return(as.data.frame(DT))
}

can_be_numeric <- function(x) {
    # Check if vector can be converted to numeric
    stopifnot(is.atomic(x) || is.list(x)) # check if x is a vector
    numNAs <- sum(is.na(x))
    numNAs_new <- suppressWarnings(sum(is.na(as.numeric(x))))
    return(numNAs_new == numNAs)
}

join_data <- function(DT) {
    DT <- data.table(DT)
    setkey(DT, publication_date, date)

    DT[, days_since_publication := publication_date - date]
    DT[publication_date == "2020-04-02", days_since_publication := NA]
    DT[date == "2020-04-02" & publication_date == "2020-04-02", days_since_publication := 0]

    DT[, paste0("n_m", 1) := shift(N, n = 1, type = "lag", fill = 0L), by = date]
    DT[, n_diff := N - n_m1]

    return(DT)
}

predict_lag <- function(DT) {
    avg_delay <- DT[!is.na(days_since_publication) & days_since_publication != 0,
        .(avg_diff = mean(n_diff, na.rm = TRUE)), by = days_since_publication]
    avg_delay[, sum_cum := cumsum(avg_diff)]
    avg_delay[, match := days_since_publication - 1]

    DT <- DT[, .(days_since_publication = max(days_since_publication, na.rm = TRUE), deaths = max(N, na.rm = TRUE)), by = date][order(date)]
    DT <- merge(DT, avg_delay[, .(match, sum_cum)], by.x = "days_since_publication", by.y = "match")

    DT <- DT[, .(date, sure_deaths = deaths, predicted_deaths = sum_cum, total = deaths + sum_cum)]

    return(DT)
}

##
# Plots

plot_lagged_deaths <- function(DT, prediction) {
    require(ggplot2)
    # require(forcats)

    DT <- DT[n_diff != 0]
    DT[, days_since_publication := forcats::fct_rev(factor(days_since_publication))]
    DT[publication_date == "2020-04-02" & is.na(days_since_publication), publication_date := NA]
    DT[, publication_date := forcats::fct_rev(factor(publication_date))]
    # DT[, days_since_publication := as.integer(days_since_publication)]

    pal <- wesanderson::wes_palette("Darjeeling1", length(levels(DT$publication_date)), type = "continuous")

    p <- ggplot(data = DT, aes(y = n_diff, x = date)) +
        geom_bar(position="stack", stat="identity", aes(fill = publication_date)) +
        geom_line(data = prediction, aes(y = total)) +
        scale_x_date(date_breaks = "2 day", expand = c(0, 0)) +
        scale_fill_manual(values = pal, na.value = "grey50") +
        theme(axis.text.x = element_text(angle = 60, hjust = 1),
              legend.position = "right") +
        labs(title = "Covid deaths (Sweden)",
             subtitle = "Number of deaths by report date, black line shows prediction.",
             caption = "Predicted deaths based on average historical reporting delay.",
             x = "Date",
             y = "N")

    p
}

save_plot <- function(p, f) {
    ggsave(filename = f, plot = p,
           height = fig_height,
           width = fig_height * fig_aspect_ratio_pres,
           device = cairo_pdf)
}
