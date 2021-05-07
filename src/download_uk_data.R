
# download_old_UK_data
library(data.table)
library(drake)
start_date <- as.Date("2020-08-24")
end_date <- as.Date("2021-05-06")

#DT <- readd(deaths_dt_uk)
dates_to_fetch <- seq(start_date, end_date, 1)
for (i in seq_along(dates_to_fetch)) {
    print(dates_to_fetch[i])
    download.file(
        url = paste0("https://api.coronavirus.data.gov.uk/v2/data?areaType=overview&metric=newDeaths28DaysByDeathDate&format=csv&release=", dates_to_fetch[i]),
        destfile = paste0("data/tmp/uk_", dates_to_fetch[i], ".csv")
    )
    Sys.sleep(40)
}

# Add old UK data to UK dataset
#DT <- readd(deaths_dt_uk)
new_data_files <- list.files("data/tmp", full.names = TRUE)
new_DT_list <- lapply(new_data_files, fread)
names(new_DT_list) <- gsub("[a-z/_]*([0-9-]*).csv$", "\\1", new_data_files)
new_DT <- rbindlist(new_DT_list, use.names = TRUE, idcol = "publication_date")
new_DT <- new_DT[, .(publication_date = as.IDate(publication_date),
                     date = as.IDate(date),
                     N = as.integer(newDeaths28DaysByDeathDate))]
setkey(new_DT, publication_date, date)
nrow(new_DT)
new_DT <- unique(new_DT, by = key(new_DT))
nrow(new_DT)
fwrite(new_DT, file.path("data", "other_countries", "uk_new.csv"))
