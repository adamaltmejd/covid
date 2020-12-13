load_fhm_icu_age <- function(f) {
    require(data.table)
    require(readxl)
    require(stringr)

    # Skip early reports that do not contain ICU data
    date <- as.Date(str_extract(f, "[0-9]{4}-[0-9]{2}-[0-9]{2}"))
    if (date <= as.Date("2020-04-24")) return(NULL)

    sheets <- excel_sheets(f)

    DT <- data.table((
        read_excel(path = f, sheet = grep("Totalt antal per åldersgrupp", sheets))#, col_types = c("text", "numeric"))
    ))[, .(Åldersgrupp, Totalt_antal_intensivvårdade)]

    setnames(DT, c("age_group", "N_icu_accumulated"))
    DT[, publication_date := date]
    DT <- dcast(DT, publication_date ~ age_group, value.var = "N_icu_accumulated")

    return(DT)
}

library(drake)
require(data.table)
require(readxl)
require(stringr)
library(ggplot2)
fhm_files <- list.files(file.path("data", "FHM"), pattern = "^Folkhalso", full.names = TRUE)
dts <- rbindlist(lapply(fhm_files, load_fhm_icu_age))
DT <- copy(dts)

# Add full sequence of dates
DT[, "Uppgift saknas" := NULL]

date_seq <- seq(DT[, min(publication_date)], DT[, max(publication_date)], 1)
DT <- merge(DT, data.table(publication_date = date_seq), by = "publication_date", all = TRUE)
setkey(DT, publication_date)

# Skip negatives by setting all to cummax
DT[is.na(Ålder_0_9), Ålder_0_9 := 0]
DT[is.na(Ålder_10_19), Ålder_10_19 := 0]
DT[is.na(Ålder_20_29), Ålder_20_29 := 0]
DT[is.na(Ålder_30_39), Ålder_30_39 := 0]
DT[is.na(Ålder_40_49), Ålder_40_49 := 0]
DT[is.na(Ålder_50_59), Ålder_50_59 := 0]
DT[is.na(Ålder_60_69), Ålder_60_69 := 0]
DT[is.na(Ålder_70_79), Ålder_70_79 := 0]
DT[is.na(Ålder_80_89), Ålder_80_89 := 0]
DT[is.na(Ålder_90_plus), Ålder_90_plus := 0]
DT[, Ålder_0_9 := cummax(Ålder_0_9)]
DT[, Ålder_10_19 := cummax(Ålder_10_19)]
DT[, Ålder_20_29 := cummax(Ålder_20_29)]
DT[, Ålder_30_39 := cummax(Ålder_30_39)]
DT[, Ålder_40_49 := cummax(Ålder_40_49)]
DT[, Ålder_50_59 := cummax(Ålder_50_59)]
DT[, Ålder_60_69 := cummax(Ålder_60_69)]
DT[, Ålder_70_79 := cummax(Ålder_70_79)]
DT[, Ålder_80_89 := cummax(Ålder_80_89)]
DT[, Ålder_90_plus := cummax(Ålder_90_plus)]

# DT[, week := strftime(publication_date, format = "%V")]
# setorder(DT, -publication_date)
# DT <- unique(DT, by = "week")
# setorder(DT, publication_date)

DT[, paste0(names(DT)[grep("Ålder", names(DT))], "_lag") := shift(.SD, 1, fill = 0), .SDcols = names(DT)[grep("Ålder", names(DT))]]
#setcolorder(DT, sort(names(DT)))

DT[, age_0_9_change := Ålder_0_9 - Ålder_0_9_lag]
DT[, age_10_19_change := Ålder_10_19 - Ålder_10_19_lag]
DT[, age_20_29_change := Ålder_20_29 - Ålder_20_29_lag]
DT[, age_30_39_change := Ålder_30_39 - Ålder_30_39_lag]
DT[, age_40_49_change := Ålder_40_49 - Ålder_40_49_lag]
DT[, age_50_59_change := Ålder_50_59 - Ålder_50_59_lag]
DT[, age_60_69_change := Ålder_60_69 - Ålder_60_69_lag]
DT[, age_70_79_change := Ålder_70_79 - Ålder_70_79_lag]
DT[, age_80_89_change := Ålder_80_89 - Ålder_80_89_lag]
DT[, age_90_plus_change := Ålder_90_plus - Ålder_90_plus_lag]



# By group size
# DT[, age_0_9_change := age_0_9_change / (1225802 / 100000)]
# DT[, age_10_19_change := age_10_19_change / (1177928 / 100000)]
# DT[, age_20_29_change := age_20_29_change / (1327395 / 100000)]
# DT[, age_30_39_change := age_30_39_change / (1366489 / 100000)]
# DT[, age_40_49_change := age_40_49_change / (1298355 / 100000)]
# DT[, age_50_59_change := age_50_59_change / (1297863 / 100000)]
# DT[, age_60_69_change := age_60_69_change / (1108438 / 100000)]
# DT[, age_70_79_change := age_70_79_change / (989013 / 100000)]
# DT[, age_80_89_change := age_80_89_change / (436679 / 100000)]
# DT[, age_90_plus_change := age_90_plus_change / (99627 / 100000)]



fwrite(DT[week != 17, mget(c("week", names(DT)[grep("change", names(DT))]))], "till_magnus.csv")

# plotdata <- DT[week > 17 & week < 50, mget(c("week", names(DT)[grep("change", names(DT))]))]
# plotdata <- melt(plotdata, id.vars = "week")
# library(ggplot2)
# p <- ggplot(data = plotdata, aes(x = week, y = value, group = variable, color = variable)) +
#     geom_line() + geom_point()

# p

plotdata <- melt(DT[publication_date > "2020-04-26", mget(c("publication_date", names(DT)[grep("change", names(DT))]))], id.vars = "publication_date")
plotdata[, avg := frollmean(value, 7, algo = "exact", align = "center"), by = variable]
plotdata <- plotdata[!is.na(avg)]
plotdata <- plotdata[!(variable %in% c("age_0_9_change", "age_10_19_change", "age_20_29_change", "age_90_plus_change"))]
p <- ggplot(data = plotdata, aes(x = publication_date, y = avg, group = variable, color = variable)) +
    geom_line() +
    set_default_theme()

p
