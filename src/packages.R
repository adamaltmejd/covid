suppressPackageStartupMessages(library(drake))
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(fst))
suppressPackageStartupMessages(library(readxl))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(Matrix))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(cowplot))
suppressPackageStartupMessages(library(hrbrthemes))
suppressPackageStartupMessages(library(wesanderson))
suppressPackageStartupMessages(library(forcats))
suppressPackageStartupMessages(library(gdtools))
suppressPackageStartupMessages(library(tools))
suppressPackageStartupMessages(library(curl))
suppressPackageStartupMessages(library(RCurl))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(callr))

# Setup drawing
my_null_device <- function(width, height) {
    grDevices::png(
        filename = tempfile(pattern = "cowplot_null_plot", fileext = ".png"),
        width = width, height = height, type = "cairo",
        units = "in", res = 300
    )
    grDevices::dev.control("enable")
}
cowplot::set_null_device(my_null_device)
