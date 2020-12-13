###
#
#
# funktioner tagna eller inspirerade av:
# <https://github.com/adamaltmejd/covid>
##
require(readxl)
require(ggplot2)
require(hrbrthemes)
require(gdtools)

download_latest_fhm <- function(folder = file.path("data", "FHM")) {


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



get_record_date <- function(f) {
  sheets <- excel_sheets(f)
  return(as.Date(sub("^FOHM ", "", sheets[length(sheets)]), format="%d %b %Y"))
}


set_default_theme <- function() {
    require(hrbrthemes)

    theme_ipsum(base_family = "EB Garamond") %+replace%
        theme(
            text = element_text(size = 12, color = "#333333", family = "EB Garamond"),
            #plot.title = element_text(size = rel(2), face = "plain", hjust = 0, margin = margin(0,0,5,0)),
            #plot.subtitle = element_text(size = rel(1), face = "plain", hjust = 0, margin = margin(0,0,0,0)),
            #plot.caption = element_text(size = rel(0.7), family = "EB Garamond", face = "italic", hjust = 1, vjust = 1, margin = margin(12,0,0,0)),

            legend.text = element_text(size = rel(0.9), family = "EB Garamond", hjust = 0, margin = margin(0, 0, 0, 0)),
            legend.background = element_rect(fill = "grey95"),
            legend.margin = margin(3, 3, 3, 3),
            legend.box.margin = margin(0,0,0,0),
            legend.direction = "horizontal",
            legend.position = "bottom",

            axis.title.y = element_text(size = rel(1.2), face = "bold", angle = 90, hjust = 1, vjust = 1, margin = margin(0, 5, 0, 0)),
            axis.title.x = element_text(size = rel(1.2), face = "bold", hjust = 1, vjust = 1, margin = margin(5, 0, 0, 0)),
            axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0),

            # Panels
            plot.background = element_rect(fill = "#FFFFFF", color = NA), # bg of the plot
            # plot.background = element_rect(fill = "#f5f5f5", color = NA), # bg of the plot
            plot.margin = unit(c(5, 5, 5, 5), "pt"),
            panel.border = element_blank(),
            panel.grid.major = element_line(linetype = "dotted", color = "#CCCCCC", size = 0.3),
            panel.grid.minor = element_line(linetype = "dotted", color = "#CECECE", size = 0.2),
            panel.spacing = unit(0.5, "lines")
        )
}

plot.predReport <- function(result, CI,  true.day=0, ymax = NULL){

  Reported <- result$detected
  N <- dim(Reported)[2]
  reported_so_far <- c()
  for(i in 1:N){
    reported_so_far <- c(reported_so_far,max(Reported[i,i:N]))
  }
  default_theme = set_default_theme()
  pred.data <- data.frame(date = c(result$dates),
                          deaths =reported_so_far,
                          CIl      = CI[1,],
                          CIu      = CI[2,])
  if(true.day>0){
    pred.data$type     = as.factor(c(rep("låst", true.day),
                                      rep("ej låst", N-true.day)))
    ggfig <- ggplot(data = pred.data, aes(y = deaths, x = date, fill = type)) +
      geom_bar( stat="identity") +
      default_theme+
      scale_x_date(date_breaks = "4 day", expand = c(0, 0)) +
      scale_fill_manual(values=c(  alpha("gray",0.8),"red")) +
      geom_errorbar(aes(ymin = CIl, ymax = CIu), width = 1) +
      guides(fill=guide_legend(title="låsta dagar"))
  }else{
    ggfig <- ggplot(data = pred.data, aes(y = deaths, x = date)) +
      geom_bar( stat="identity") +
      default_theme+
      scale_x_date(date_breaks = "4 day", expand = c(0, 0)) +
      scale_fill_manual(values=c(  alpha("gray",0.8))) +
      geom_errorbar(aes(ymin = CIl, ymax = CIu), width = 1)
  }
  if(is.null(ymax)==F)
  {
    ggfig <- ggfig + coord_cartesian(ylim = c(0,ymax))
  }
  return(ggfig)
}
