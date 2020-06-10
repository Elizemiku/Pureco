library(tidyverse)
library(sf)
library(mapview)
library(lubridate)
library(forecast)
library(tseries)
library(curl)
library(plotly)
library(leaflet)
library(geosphere)
library(shiny)
library(shinythemes)
library(DT)
library(tsibble)
library(zoo)
library(xts)
library(rsconnect)
library(dygraphs)

source("Tabs.R")
### source("Dados.R")

ui <- navbarPage("Pureco - Serviços de Limpeza", 
                 # tema escolhido para o site
                 theme = shinytheme("flatly"),
                 Tab1,
                 Tab2,
                 Tab3,
                 Tab4,
                 Tab5)
                         