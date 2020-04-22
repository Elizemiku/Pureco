library(dplyr)
library(tibble)
library(purrr)
library(sf)
library(mapview)
library(lubridate)
library(ggplot2)
library(forecast)
library(tseries)
library(curl)
library(stringr)
library(plotly)
library(leaflet)
library(tidyr)
library(geosphere)
library(shiny)
library(shinythemes)
library(readr)
library(DT)
library(tsibble)
library(zoo)
library(xts)
library(rsconnect)
library(dygraphs)

source("Tabs.R")

ui <- navbarPage("Pureco - Serviços de Limpeza", 
                 #tema cerulean para ficar azul que nem o app do android 
                 theme = "https://stackpath.bootstrapcdn.com/bootswatch/3.4.1/cerulean/bootstrap.min.css",
                 Tab1,
                 Tab2) 
                 #Tab3)
                         