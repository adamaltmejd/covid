download_latest_fhm <- function(folder) {
    require(readxl)

    DL <- download.file("https://www.arcgis.com/sharing/rest/content/items/b5e7488e117749c19881cce45db13f7e/data",
                        destfile = file.path(folder, "FHM_latest.xlsx"), method = "curl", extra = c("-L"), quiet = TRUE)
    if (DL != 0) { stop("File download error.") }

    # Check archived files for latest record
    latest_record <- max(as.Date(gsub("^.*(2020-[0-9]{2}-[0-9]{2}).xlsx", "\\1", list.files(folder, pattern = "^Folkhalso"))))

    # Check if new download is newer than latest record, in that case, archive it.
    new_record <- max(load_fhm(file.path(folder, "FHM_latest.xlsx"))$publication_date)

    if (latest_record < new_record) {
        file.copy(file.path(folder, "FHM_latest.xlsx"),
                  file.path("data", "FHM", paste0("Folkhalsomyndigheten_Covid19_", new_record, ".xlsx")))
    }
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

list_fhm_files <- function(folder) {
    list.files(folder, pattern = "^Folkhalso", full.names = TRUE)
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

join_data <- function(death_dt) {
    death_dt <- data.table(death_dt)
    setkey(death_dt, publication_date, date)

    death_dt[, days_since_publication := publication_date - date]
    death_dt[publication_date == "2020-04-02", days_since_publication := NA]
    death_dt[date == "2020-04-02" & publication_date == "2020-04-02", days_since_publication := 0]

    death_dt[, paste0("n_m", 1) := shift(N, n = 1, type = "lag", fill = 0L), by = date]
    death_dt[, n_diff := N - n_m1]

    return(death_dt)
}

predict_lag <- function(death_dt) {
    avg_delay <- death_dt[!is.na(days_since_publication) & days_since_publication != 0,
        .(avg_diff = mean(n_diff, na.rm = TRUE)), by = days_since_publication]
    avg_delay[, sum_cum := cumsum(avg_diff)]
    avg_delay[, match := days_since_publication - 1]

    death_dt <- death_dt[, .(days_since_publication = max(days_since_publication, na.rm = TRUE), deaths = max(N, na.rm = TRUE)), by = date][order(date)]
    death_dt <- merge(death_dt, avg_delay[, .(match, sum_cum)], by.x = "days_since_publication", by.y = "match")

    death_dt <- death_dt[, .(date, sure_deaths = deaths, predicted_deaths = sum_cum, total = deaths + sum_cum)]

    return(death_dt)
}

##
# Plots

set_default_theme <- function() {
    require(hrbrthemes)
    require(gdtools)

    if (font_family_exists(font_family = "Arial")) fam <- "Arial"
    if (font_family_exists(font_family = "EB Garamond")) fam <- "EB Garamond"
    if (font_family_exists(font_family = "Garamond Premier Pro")) fam <- "Garamond Premier Pro"

    theme_ipsum(base_family = fam) %+replace%
        theme(
            plot.title = element_text(size = rel(2), face = "plain", hjust = 0, margin = margin(0,0,5,0)),
            plot.subtitle = element_text(size = rel(1), face = "plain", hjust = 0, margin = margin(0,0,5,0)),
            legend.title = element_blank(),
            legend.position = c(0.15,0.9),
            legend.background = element_rect(fill = "white", color = "grey80"),
            legend.margin = margin(1,5,5,5),
            legend.direction = "vertical",
            # legend.box.margin = margin(0,0,0,0),
            # legend.justification = c(0,0),
            # legend.box.just = "left",
            # legend.text.align = 0,
            # legend.spacing = unit(0, "pt")
            # axis.title = element_text(size = rel(1.5)),
            # axis.text = element_text(size = rel(1)),

            # Panels
            panel.grid.major = element_line(linetype = "dotted", color = "grey60", size = 0.2),
            panel.grid.minor = element_line(linetype = "dotted", color = "grey80", size = 0.2)
        )
}

plot_lagged_deaths <- function(death_dt, death_prediction, th) {
    require(ggplot2)
    require(forcats)

    death_dt <- death_dt[n_diff != 0]
    death_dt[publication_date == "2020-04-02" & is.na(days_since_publication), publication_date := NA]
    death_dt[, publication_date := forcats::fct_rev(factor(publication_date))]

    pal <- wesanderson::wes_palette("Darjeeling1", length(levels(death_dt$publication_date)), type = "continuous")

    p <- ggplot(data = death_dt, aes(y = n_diff, x = date)) +
        geom_bar(position="stack", stat="identity", aes(fill = publication_date)) +
        geom_line(data = death_prediction, aes(y = total)) +
        scale_x_date(date_breaks = "2 day", expand = c(0, 0)) +
        scale_fill_manual(values = pal, na.value = "grey50") +
        th +
        theme(axis.text.x = element_text(angle = 60, hjust = 1),
              legend.position = "right") +
        labs(title = "Covid deaths (Sweden)",
             subtitle = "Number of deaths by report date, black line shows prediction.",
             caption = "Predicted deaths based on average historical reporting delay.",
             x = "Date",
             y = "Number of deaths")

    p
}

save_plot <- function(p, f) {
    require(ggplot2)
    require(Cairo)
    require(tools)
    require(cowplot)

    h <- 6 # inches
    w <- 10 # inches

    if (tools::file_ext(f) == "pdf") {
        ggsave(filename = f, plot = p,
           height = h, width = w,
           device = cairo_pdf)
    }
    if (tools::file_ext(f) == "png") {
        ggsave(filename = f, plot = p,
           height = h, width = w, dpi = 300,
           bg = "transparent",
           type = "cairo-png")
    }
}
