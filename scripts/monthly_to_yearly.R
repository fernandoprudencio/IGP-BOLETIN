#' @title
#' Monthly data to yearly data of GVMI
#'
#' @description
#' This script calculates from monthly data to yearly data
#'
#' @author Fernando Prudencio
#'

rm(list = ls())

#' INSTALL PACKAGES
pkg <- c(
  "tidyverse", "raster", "stringr"
)

sapply(
  pkg,
  function(x) {
    is.there <- x %in% rownames(installed.packages())
    if (is.there == FALSE) {
      install.packages(x, dependencies = T)
    }
  }
)

#' LOAD PACKAGES
library(tidyverse)
library(raster)
library(stringr)

#' LIST OF VEGETATION INDEX
lst.index <- list.files(
  "data/raster/monthly", pattern = "^.*\\.tif$", full.names = T
)

#' CREATE TIME SERIES WITHIN A DATAFRAME
df <- tibble(
  date = seq(as.Date("2000-02-01"), as.Date("2020-07-01"), by = "1 month")
) %>%
  mutate(id = 1:n())

#' BUIILD A FUNCTION TO CALCULATE CLIMATOLOGY
mnthy_to_yrly <- function(year, months, data, output) {
  bands <- df %>%
    filter(
      str_sub(date, 1, 4) == year &
        str_sub(date, 6, 7) %in% months
    )

  stack(data[bands$id]) %>%
    "*"(1) %>%
    mean(na.rm = T) %>%
    writeRaster(
      sprintf("%1$sMOD09A1.006_sur_GVMI_doy%2$s.tif", output, year),
      overwrite = T, datatype = "FLT4S"
    )
}

#' APPLY FUNCTION TO CALCULATE MONTHLY DATA
output <- "data/raster/yearly/"
sapply(
  2000:2019, FUN = mnthy_to_yrly, sprintf("%02d", 7:11), lst.index, output
)