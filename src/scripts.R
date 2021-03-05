
# download_old_UK_data
library(data.table)
library(drake)
start_date <- as.Date("2020-08-13")
DT <- readd(deaths_dt_uk)
dates_to_fetch <- seq(start_date, DT[, min(publication_date) - 1], 1)

for (i in seq_along(dates_to_fetch)) {
    download.file(
        url = paste0("https://api.coronavirus.data.gov.uk/v2/data?areaType=overview&metric=newDeaths28DaysByDeathDate&format=csv&release=", dates_to_fetch[i]),
        destfile = paste0("data/tmp/uk_", dates_to_fetch[i], ".csv")
    )
    Sys.sleep(40)
}

# Add old UK data to UK dataset
DT <- readd(deaths_dt_uk)
new_data_files <- list.files("data/tmp", full.names = TRUE)
new_DT_list <- lapply(new_data_files, fread)
names(new_DT_list) <- gsub("[a-z/_]*([0-9-]*).csv$", "\\1", new_data_files)
new_DT <- rbindlist(new_DT_list, use.names = TRUE, idcol = "publication_date")

DT <- rbind(new_DT[, .(publication_date = as.IDate(publication_date), date = as.IDate(date), N = as.integer(newDeaths28DaysByDeathDate))],
            DT[, .(publication_date = as.IDate(publication_date), date = as.IDate(date), N = as.integer(N))],
            use.names = TRUE)
setkey(DT, publication_date, date)
DT <- unique(DT, by = key(DT))
fwrite(DT, file.path("data", "other_countries", "uk.csv"))
