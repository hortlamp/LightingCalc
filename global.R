#NO DB Lighting Calc Version: global.r -- this file is run once before app starts

#Libaries Used in App
library(shiny) #build interactive web apps
library(shinythemes) # layouts for shiny
library(dplyr) # data manipulation
library(ggplot2) #data visualization
library (DT) # for data tables #-# provides an R interface to the JavaScript library DataTables. R data objects (matrices or data frames) can be displayed as tables on HTML pages, and DataTables provides filtering, pagination, sorting, and many other features in the tables.
library(shinyWidgets) # for extra widgets
library(shinyjs)#-# lets you perform common useful JavaScript operations in Shiny applications without having to know any JavaScript
library(shinydashboard) #other helpful shiny functions
library(shinyBS) #Question mark widget
library(lubridate) #for dates
library(shinycssloaders) #for loading icons, see line below
library(rmarkdown) #Dynamic Documents for R
library(rintrojs) # for help intros #-#  makes it easy to include step-by-step introductions, and clickable hints in a 'Shiny' application
library(httr) #for Working with URLs and HTTP.
library(tidyverse) #collection of R packages designed for data science.
library(jsonlite) #fast JSON parser and generator
library(readr) #read rectangular data (like 'csv', 'tsv', and 'fwf')
library(stringr) #consistent, simple and easy to use set of wrappers for common string operations.
library(FinCal) #Package for time value of money calculation, time series analysis and computational finance
library(epitools) #needed forlogin credentials code
library(scales)
library(tidygeocoder) #tidy interface for geocoding
library(shinyMatrix) #implements a custom matrix input field.
library(shinyalert) #easily create pretty popup messages
library(writexl)

#Reads the file where NSRDB and other API keys are located
keys <-  'VirtualGrowerAppKeys.csv'
k <-  read_csv(keys) 

#Simple function to close window (not needed in shinyapps version) 
jscode <- "shinyjs.closeWindow = function() { window.close(); }"

#Geocodio API Key
Sys.setenv(GEOCODIO_API_KEY = k$GEOCODIO_API_KEY) #HortLamp Geocodio Key

# NSRDB API and URL to pull TMY data
URLbase <- paste0('https://developer.nrel.gov/api/nsrdb/v2/solar/psm3-tmy-download.csv?&names=tmy&attributes=ghi&email=', k$email, '&api_key=', k$NSRDB)

#Create a header to be used repeatedly in the code
Header_Details <- list(img(src = "LampLogo.png", height="30%", width="30%", align="left"),
                       h2(em("Welcome to our ‘Unlimited lighting' calculator!"), align="center"),
                       br(),
                       br(),
                       h2(em("This calculator determines how much electricity is required to reach the target DLI every single day of the year. To do so, a lighting system would need to be designed to provide enough light on even the darkest day of the year. Because of that unrealistic assumption, this calculator does not calculate demand charges, only the cost of the electricity used.")),
                       br(),
                       br(),
                       br(),
                       h2("This tool was developed for greenhouse growers as part of Project LAMP,",
                          em("lighting approaches to maximize profits."),"Our goal is to maximize controlled
               environment agriculture (CEA) grower profits through improved lighting systems and support tools.",align="center"),
                       h2("User information entered in this calculator is not stored.", align = "center"),
                       br(),br(),br())

#Create a header to be used repeatedly in the code
Footer_Details <- list(hr(),
                       p("Questions? Please contact us at", tags$a(href="info@hortlamp.org","info@hortlamp.org"), align="center", style = "font-size:20px;"), 
                       p("Funded by USDA-NIFA-SCRI Award 2018-51181-28365", align="center", style = "font-size:20px;"),
                       p(tags$a(href="www.hortlamp.org", "www.hortlamp.org"), align="center", style = "font-size:20px;"),
                       p("Copyright 2021 LAMP", align="center", style = "font-size:20px;"))

FAQ_Details <- list(
  h1("Frequently Asked Questions"),
  br(),
  h2(tags$b("What is light transmission?")),
  h3("The fraction of sunlight transmitted into the greenhouse. You need to know how much light is transmitted to calculate the daily light integral (DLI) inside the greenhouse. Greenhouse light transmission typically ranges from 40%-90%. If you have a light meter, you can check this for your own greenhouse, preferably on a sunny day, around noon, when light levels are steady: measure light inside and outside of the greenhouse and calculate transmission as light in the (greenhouse/light outside the greenhouse) x 100%. The transmission of your greenhouse as a whole is lower than the transmission of the glazing material. For accurate results, you need to use the greenhouse transmission, not the glazing material transmission. For more information and a discussion of factors that affect greenhouse transmission please read",
     tags$a(href="https://www.greenhousemag.com/article/does-light-transmission-through-greenhouse-glazing-matter/", tags$u("Does light transmission through greenhouse glazing matter?"))),
  h2(tags$b("What is target DLI? ")),
  h3("DLI is daily light integral, representing the total amount of photosynthetic light plants get in a day. Factors affecting DLI include light intensity, geographic location, and seasonal changes. DLIs typically range between 5-30 moles of light per square meter per day (mol/m2/d1).  For DLI recommendations for a range of crops, please see this",
     tags$a(href="https://www.extension.purdue.edu/extmedia/ho/ho-238-w.pdf", tags$u("publication on 'Measuring Daily Light Integral in a Greenhouse' from Purdue University."))),
  h2(tags$b("What is lighting efficacy?")),
  h3("An indicator of the efficiency of a light fixture, calculated as the ratio of light emitted to energy used. Typical lighting efficacy ranges from 1.2-3.5 micromoles per Joule (umol/J). Sometimes efficacy is provided as mol/kWh. To convert efficacy values from mol/kWk to umol/J, divide by 3.6. For more information on lighting efficacy, please read",
     tags$a(href="https://gpnmag.com/article/plant-lighting-efficiency-and-efficacy-%CE%BCmol%C2%B7j-%C2%B9/", tags$u("Plant Lighting Efficiency and Efficacy: umols per joule."))),
  h2(tags$b("How do I determine my electricity rate?")),
  h3("Your electricity rate is reported in cost per kilowatt hour ($/kWh) and can be found on your power bill or by contacting your electricity provider. Cost per kilowatt hour may be written clearly on your bill or you can determine it by taking you total power bill (minus taxes) and divide that by your power consumption. Electricity costs typically range from $0.05 - $0.30 / kWh."),
  h2(tags$em("For more information on greenhouse lighting, visit our Outreach Page at", tags$u(tags$a(href="https://hortlamp.org/outreach", "www.hortlamp.com/outreach"))))
)

#area matrix
m_area <- matrix(
  100, 
  nrow = 1, #number of rows
  ncol = 12, #number of columns
  dimnames = list(
    "%", #name of rows
    c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")#name of columns
  )
)

#append that year to the first of jan for any given year
FirstMonday <- as_date(paste0(year(Sys.Date()), "/1/1"))

#Retrieve the day # of the first of the year
FirstDay <- wday(
  FirstMonday,
  label = FALSE,
  abbr = TRUE,
  week_start = getOption("lubridate.week.start", 1) #Start the week on a monday
)

#Calculate the number of days to get to the first monday of the month and assign that
#to FirstMonday, which contains the date of the first Monday of any given year
if(FirstDay != 1) {
  while (FirstDay < 8) {
    FirstMonday = FirstMonday + days(1)
    FirstDay = FirstDay + 1
  }
} else {}
