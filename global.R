library(checkpoint)

options("checkpoint.mranUrl" = "file:///") # fast checkpoint load
checkpoint(snapshotDate    = "2019-04-15", # default for MRO 3.5.3
           scanForPackages = FALSE,
           verbose         = FALSE)

suppressPackageStartupMessages({
library(shiny)
library(shinycssloaders)
library(shinydashboard)
library(shinyjs)
library(shiny.i18n)
library(shinyWidgets)
library(plotly)
library(dplyr)
library(DT)
library(ggplot2)
library(RColorBrewer)
library(RSclient)
library(waiter)
})

lang_default  = "pl"
lang_fallback = "en"

# loading table/plot spinner config

options(spinner.type  = 1,
        spinner.color = "#D73925")

# load all modules

sapply(list.files(".", "mod-(.*)\\.R"), source)

# read data from Rserve

rs.con = if (.Platform$OS.type == "unix")
{
    RS.connect("wena-rserve", port = 0)
} else { # windows
    RS.connect(port = 8383)
}

rdat   = RS.eval(rs.con, quote(rdat))

# close connection to Rserve when all clients disconnects

onStop(function() {
    RS.close(rs.con)
})
