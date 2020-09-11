#' @title
#' Daily data to monthly data of GVMI
#' 
#' @description
#' This script calculates from daily data to monthly data
#' 
#' @author Fernando Prudencio
#'

rm(list = ls())

#' INSTALL PACKAGES
pkg <- c(
  "tidyverse", "raster", "ncdf4", "stringr"
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
library(ncdf4)
library(stringr)

#' LIST OF VEGETATION INDEX
lst.img <- list.files(
  "data/raster/historic", pattern = "^.*\\.tif$", full.names = T
)

#' CREATE DATE VECTOR
date <- Sys.Date()
year <- str_sub(date, 1, 4) %>% as.numeric()
month <- str_sub(date, 6, 7) %>% as.numeric()
day <- str_sub(date, 9, 10) %>% as.numeric()

for (i in 2000:year) {
  if (i == 2000) {
    ts <- c(
      as.Date("2000-02-26"),
      seq(as.Date("2000-03-06"), as.Date("2000-12-31"), by = "8 day")
    )
  }
  if (i >= 2001 & i <= year - 1) {
    ts <- c(
      ts,
      seq(as.Date(sprintf("%s-01-01", i)),
          as.Date(sprintf("%s-02-26", i)),
          by = "8 day"
      ),
      seq(as.Date(sprintf("%s-03-06", i)),
          as.Date(sprintf("%s-12-31", i)),
          by = "8 day"
      )
    )
  }
  if (i == year & as.numeric(month) <= 2) {
    ts <- c(
      ts,
      seq(as.Date(sprintf("%s-01-01", i)),
          as.Date(sprintf("%s-%s-%s", i, month, day)),
          by = "8 day"
      )
    )
  }
  if (i == year & as.numeric(month) > 2) {
    ts <- c(
      ts,
      seq(as.Date(sprintf("%s-01-01", i)),
          as.Date(sprintf("%s-02-26", i)),
          by = "8 day"
      ),
      seq(as.Date(sprintf("%s-03-06", i)),
          as.Date(sprintf("%s-%s-%s", i, month, day)),
          by = "8 day"
      )
    )
  }
}

ts <- ts[1:length(lst.img)]

#' BUILD A DATAFRAME OF DATE
df <- tibble(date = ts) %>% mutate(id = 1:n())

#' BUILD FUNCTION TO CALCULATE MONTHLY DATA
mnth.date <- str_sub(ts, 1, 7) %>% unique()

dly_to_mnthly <- function(doy, data, output) {
  bands <- df %>% filter(str_sub(date, 1, 7) == doy)
  stack(data[bands$id]) %>%
    "*"(1) %>%
    max(na.rm = T) %>%
    writeRaster(
      sprintf(
        "%1$sMOD09A1.006_sur_GVMI_doy%2$s%3$s.tif",
        output, str_sub(doy, 1, 4), str_sub(doy, 6, 7)
      ),
      overwrite = T, datatype = "FLT4S"
    )
}

#' APPLY FUNCTION TO CALCULATE MONTHLY DATA
output <- "data/raster/monthly/"
sapply(mnth.date, FUN = dly_to_mnthly, lst.img, output)