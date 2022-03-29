# Click the small lights next to the publish button (top right of this code window) to navigate by headers

# Define a server for the Shiny app
function(input, output, session) {
  options(shiny.port = 1221) #Used for local testing, unused in shinyapps.
  useShinyjs()
  extendShinyjs(text = jscode, functions = c("closeWindow"))
  
  ###############################################.
  ## Server - UI Body  ----
  ###############################################.
  output$uibody <- renderUI({
    
    ###############################################.
    ## UI - Header NavBar Page ---- 
    ###############################################.
    navbarPage(title = "LightingCalc",
               theme = shinytheme("cerulean"),
               
               ###############################################.
               ## UI - Home ----
               ###############################################.           
               tabPanel("Home",
                        isolate({Header_Details}),
                        p(strong("How to use this calulator"), align="left", style="font-size:30px"),
                        p("1. Start by selecting the 'Add Location' tab where you provide a zip code and electricity rate in $/kWh.", align="left", style="font-size:30px"),
                        p("2. Then you can add a Greenhouse Design. Note: this information is discarded once you exit the calculator.", align="left", style="font-size:30px"),
                        p("3. Next, calculate your Electricity Costs by selecting a design for your location. ", align="left", style="font-size:30px"),
                        p("4. Voila! You've calculated your lighting costs!", align="left", style="font-size:30px"),
                        isolate({Footer_Details})
               ),#bracket Home tab panel 
               
               ###############################################.
               # UI - Locations - Add New ----
               ###############################################. 
               tabPanel("Add Location",
                        Header_Details,
                        sidebarPanel(titlePanel("Enter Greenhouse Location"), 
                                     div(title="Enter your Location Name Here",
                                         textInput(inputId ="New_Loc_Name",
                                                   label = shiny::HTML("<p>Location Name <br/> <span style='font-weight: 400'>(Enter a unique identifier for your Location)</span></p>"), width = '400px')),
                                     div(title="Enter the Zipcode Here",
                                         textInput(inputId ="New_Loc_Zip",
                                                   label = shiny::HTML("<p>Zip Code <br/> <span style='font-weight: 400'>(5 digits)</span></p>"), width = '125px')),
                                     div(title="Enter an Electricity Rate in $/kWh",
                                         textInput(inputId ="New_Loc_Elec_Rate",
                                                   label = p("Electricity Rate ($/kWh)",
                                                             bsButton("b2", label = "", icon = icon("question"), style = "info", size = "extra-small"), style = "width:200px")),
                                         bsPopover(id = "b2", title="", content = "Typically, between $0.05 and $0.30. Only use numbers with a decimal. Do NOT include a dollar sign ($)", placement="right", trigger="hover",options = list(container = "body"))),
                                     actionButton("Location_save", "Save")
                        ), #bracket sidebar Panel
                        p(strong("Enter Greenhouse Location"), align="left", style="font-size:30px"),
                        p("1. Add your location and electricity price ($/kWh).", align="left", style="font-size:30px"),
                        p("2. Do not add the dollar sign ($) in the electricity rate box.", align="left", style="font-size:30px"),
                        p("3. ", em("Not sure what your electricity rate is? Reference your most recent power bill for more information."), align="left", style="font-size:30px"),
                        Footer_Details
               ),#bracket Add New tab panel
               
               ###############################################.
               # UI - Greenhouse Designs - Add New ----
               ###############################################.
               tabPanel("Add Greenhouse Design",
                        Header_Details,
                        sidebarPanel(
                          titlePanel("Enter Greenhouse Design"),
                          p(strong("Update Gear Icon First"), align="center", style="font-size:20px"),
                          fluidRow(column(7,
                                          div(title="Enter your Design Name Here",
                                              textInput(inputId ="New_GHD_Name",
                                                        label = shiny::HTML("<p>Design Name <span style='font-weight: 400'></span></p>"), width = '300px')),
                                          div(title="Enter Greenhouse Length Here",
                                              numericInput(inputId ="New_GHD_Length",
                                                           label = shiny::HTML("<p>Length <span style='font-weight: 400'>(ft)</span></p>"),value=1,min=1, width = '100px')),
                                          div(title="Enter Greenhouse Width Here",
                                              numericInput(inputId ="New_GHD_Width",
                                                           label = shiny::HTML("<p>Width <span style='font-weight: 400'>(ft)</span></p>"),value=1,min=1, width = '100px')),
                                          div(title="Enter a % Transmission Here ",
                                              textInput(inputId ="New_GHD_Trans",
                                                        label = p("Greenhouse Transmission (%) ",
                                                                  bsButton("b3", label = "", icon = icon("question"), style = "info", size = "extra-small"), style = "width:175px")),
                                              bsPopover(id = "b3", title="", content = "Typically, between 40 and 90 percent. Use numbers only.", placement="right", trigger="hover",options = list(container = "body"))),
                                          div(title="Enter a Target DLI Here (mol/m2/day)",
                                              textInput(inputId ="New_GHD_TargetDLI",
                                                        label = p("Target DLI (mol/m2/day) ",
                                                                  bsButton("b4", label = "", icon = icon("question"), style = "info", size = "extra-small"), style = "width:200px")),
                                              bsPopover(id = "b4", title="", content = "Typically, between 5 and 30 mol/m2/day. Use numbers only.", placement="right", trigger="hover",options = list(container = "body"))),
                                          div(title="Enter Lighting Efficacy Here (umol/J)",
                                              textInput(inputId ="New_GHD_Efficacy",
                                                        label = p("Lighting Efficacy (umol/J) ",
                                                                  bsButton("b5", label = "", icon = icon("question"), style = "info", size = "extra-small"), style = "width:200px")),
                                              bsPopover(id = "b5", title = "", content = "Typically, between 1.2 and 3.5 umol/J. Use numbers only.", placement="right", trigger="hover",options = list(container = "body")))
                          ),#bracket column 1
                          #Gear icon for customize % of lighting area by month
                          column(5, 
                                 h6("Click gear to customize lighting % of area by month"),
                                 #Percent of the area to light by month
                                 shinyWidgets::dropdownButton(
                                   tags$h3("Percent of Greenhouse Area By Month"),
                                   uiOutput('percent_mat'), 
                                   shiny::actionButton(
                                     inputId = "update_permat", 
                                     label = "Update Percentages"
                                   ),
                                   circle = TRUE, status = "primary", icon = icon("gear"), width = "450",
                                   tooltip = tooltipOptions(title = "Click here to customize lighting area by month!")
                                 ),#bracket dropdownButton
                          ),#bracket column 2
                          ),#bracket fluidrow
                          actionButton("GHD_save", "Save")
                        ), #bracket sidebarPanel
                        p(strong("Enter Greenhouse Design"), align="left", style="font-size:30px"),
                        p("1. Add a unique greenhouse design to calculate electricity costs for your location.", align="left", style="font-size:30px"),
                        p("2. If desired, select the gear icon to customize lighting % of area by month.", align="left", style="font-size:30px"),
                        p("3. Indicate the square footage by providing the greenhouse length and width in feet. Provide your greenhouse transmission percentage,
                          target daily light integral (DLI) and lighting efficacy.", align="left", style="font-size:30px"),
                        p("4. Once saved, select the 'Electricity Costs' tab to see your calculated costs for the provided greenhouse design.", align="left", style="font-size:30px"),
                        p("* Note: If running multiple tests, press 'Save' after updating gear icon for accurate results.",  align="left", style="font-size:30px"),
                        Footer_Details
               ),#bracket Add New tab panel
               
               ###############################################.
               ## UI - Electricity Costs ----
               ###############################################.                   
               tabPanel("Electricity Cost",
                        h2("Electricity Cost"),
                        sidebarLayout(
                          sidebarPanel(
                            uiOutput('ui_EC_location'),
                            uiOutput('ui_EC_design'),
                            actionButton(inputId = 'EC_Choices', label = 'Update'),
                            downloadButton("EC_downloadData", "Download Results"),
                            width = "2",
                            h4("Press the Update Button above to view results")
                          ),#bracket sidebar panel
                          mainPanel(fluidRow(
                            column(8,
                                   h4("Monthly Area Lighting %"), 
                                   tableOutput("Percent_table"),
                                   h4("Grower Input"),
                                   tableOutput('EC_table')
                            ),
                            column(4,
                                   h4("Annual Lighting Cost"),
                                   tableOutput("Annual_EC_Display_table")
                            )
                          ),#bracket fluid row 1
                          fluidRow(
                            column(3,
                                   h4("Weekly Lighting Cost"),
                                   tableOutput({"EC_df_W_table"}),
                                   DT::dataTableOutput("trace_table"),
                                   style = "height:500px; overflow-y: scroll;"
                            ),
                            column(8,
                                   h4("Monthy Lighting Cost ($/acre)"),
                                   plotOutput({"EC_df_M_histo"})
                            )
                          )#bracket fluid row 2
                          )#bracket main panel          
                        )#bracket sidebar layout 
               ),
               tabPanel(title="FAQ",
                        FAQ_Details
               ),#Bracket FAQ Page
               tabPanel(title = "Quit",
                        actionButton("close", "Click Here to End Session"))
    )#Bracket UI navbarPage
  }) #bracket renderUI
  
  ###############################################.
  ## Server - Quit Application ---- 
  ###############################################.
  observeEvent(input$close, {
    js$closeWindow()
    stopApp()
  })
  
  ###############################################.
  # Server - Locations - Add New ----
  ###############################################. 
  Loc_values <- reactiveValues() # empty list to store reactive values for new location
  XTRA_values <- reactiveValues() # empty list to store radiation so it can be used downstream
  ## Add New Locations ####
  # Note that we use observeEvent() here, which depends on input$Location_save (the action button), so that the output is only updated when the grower clicks the button
  observeEvent(input$Location_save, { # When action button "Location_save" is clicked run code in this section
    elec_test <- FALSE #Electricity rate must be valid: no $. 
    geo_test <-FALSE #Location must be found
    field_test <- FALSE #All fields must be entered
    # get lat and long using street and zipcode
    if(nchar(input$New_Loc_Zip)!=5 & input$New_Loc_Zip!=""){
      shinyalert("Please enter a 5 digit zipcode", type="error")
      updateTextInput(session,"New_Loc_Zip", value="")
    } else{
      t <- try(geo(postalcode=as.character(input$New_Loc_Zip), lat = "lat", long = "long", verbose = FALSE, method="geocodio"))
      if ("try-error" %in% class(t)) {
        shinyalert("Location not found please try a different address or zip code", type="error")
      } else{
        geoloc <- geo(postalcode=as.character(input$New_Loc_Zip), lat = "lat", long = "long", verbose = FALSE, method="geocodio")
        if(is.logical(geoloc$lat)){ #is.logical() is used if geo cascade doesn't find address, it returns N/A
          shinyalert("Location not found please try a different address or zip code", type="error")
        } 
        else{
          geo_test <- TRUE #Set geo_test to true if location is found
          Loc_values$lat <- geoloc$lat[1] #Set lat to value from geo() function
          Loc_values$long <- geoloc$long[1] #Set long to value from geo() function
        }
      }
    }
    
    rate_test <- as.numeric(input$New_Loc_Elec_Rate)
    if(is.na(rate_test)){
      shinyalert("Please check Electricity Rate input: No '$' needed", type="error")
      updateTextInput(session,"New_Loc_Elec_Rate", value="")
    } else{
      elec_test <- TRUE #Set to true if electricty rate is a valid number 
    }
    
    if(input$New_Loc_Name=="" | input$New_Loc_Zip==""| input$New_Loc_Elec_Rate==""){
      shinyalert("Please make sure all fields are filled.", type="error")
    } else{
      field_test <- TRUE #Set to true if all inputs have been entered, no blanks
    }
    
    if(elec_test==TRUE & geo_test==TRUE & field_test==TRUE){ #If all tests are true, store the values. 
      Loc_values$Name <- input$New_Loc_Name
      Loc_values$Zip <- input$New_Loc_Zip
      Loc_values$Elec_Rate <- input$New_Loc_Elec_Rate  
      shinyalert("Success! Now add your Greenhouse Design.")
    }
    
    lon <- as.numeric(Loc_values$long)
    lat <- as.numeric(Loc_values$lat)
    
    # Pull TMY data from NSRDB
    URL <- paste0(URLbase,'&wkt=POINT(', lon, '+', lat,')')#,'&utc=', 0) #<- will sometimes crash the app with the tmy data. Tradeoff
    req <- GET(url = URL)
    NSRDB <- content(req)
    radiation <- NSRDB[-(1:2),(1:6)]
    names(radiation)<- c("Y","M","D","H","Min","ghi")
    radiation$Y <- rep(year(Sys.Date()),nrow(radiation))
    radiation$M <- sprintf("%02d", as.numeric(radiation$M))
    radiation$D <- sprintf("%02d", as.numeric(radiation$D))
    radiation$oldTimeStamp <- paste0(radiation$M, "/", radiation$D, "/", radiation$Y, " ", radiation$H,":",radiation$Min)
    radiation$TimeStamp <- as.POSIXct(strptime(radiation$oldTimeStamp,"%m/%d/%Y %H:%M"))
    radiation$Month <- as.character(paste0(radiation$M))
    radiation <- radiation %>% 
      filter(TimeStamp >= FirstMonday)
    radiation <- radiation[c(9,8,6)]
    
    
    XTRA_values$radiation <- radiation
  })
  
  ###############################################.
  # Server - Greenhouse Designs - Add New ----
  ###############################################.
  ## Add New Greenhouse Design ####
  GHD_values <- reactiveValues() # empty list to store reactive values for new design qry
  AP_values <- reactiveValues() # empty list to store reactive values for percent of area to light by month qry
  
  #Ui output of matrix values default to 100%
  output$percent_mat <- renderUI({
    matrixInput(
      "per_area_input",#name of matrix input
      value = m_area,#value which is the matrix
      rows = list(names = TRUE,extend = FALSE),
      cols = list(names = TRUE),
      class = "numeric"
    )
  })
  
  
  # AP_values <- list()
  #list of 100% area if the area percent matrix is never updated (did not click button)
  AP_values$percent <- m_area
  # AP_values$PerOfOne <- data.frame(AP_values$percent)/100
  
  observeEvent(input$update_permat, {# When action button "update_permat" is clicked run code in this section
    #make a dataframe of the input matrix
    per_m <- data.frame(input$per_area_input)
    
    #condaitonal outcomes of update_permat button
    #all months must have a % entered and the % must be between 0 and 100
    if(all(!is.na(per_m))){#if all month have a % entered
      if(all(per_m >= 0,per_m <=  100)){ #if all % are between 0 and 100
        #replace defult reactive list with the percent area input
        for (i in 1:length(per_m)){
          AP_values$percent[[i]] <- per_m[i][[1]]
        }
        
        #Ui output of matrix - change values to default on input
        output$percent_mat <- renderUI({
          matrixInput(
            "per_area_input",#name of matrix input
            value = input$per_area_input,#vaule which is the matrix
            rows = list(names = TRUE,extend = FALSE),
            cols = list(names = TRUE),
            class = "numeric"
          )
        })
        shinyalert("Success!")
      } else {
        #pop up warning message
        shinyalert("Oops!", "Precent must be between 0 and 100.", type = "error")
        
        #Ui output of matrix reset to values default to 100%
        output$percent_mat <- renderUI({
          matrixInput(
            "per_area_input",#name of matrix input
            value = m_area,#vaule which is the matrix
            rows = list(names = TRUE,extend = FALSE),
            cols = list(names = TRUE),
            class = "numeric"
          )
        })
        #return list to 100% area for all months
        AP_values$percent <- m_area
      }
    } else {
      #pop up warning message
      shinyalert("Oops!", "Each month must have a percent area.", type = "error")
      
      #Ui output of matrix reset to values default to 100%
      output$percent_mat <- renderUI({
        matrixInput(
          "per_area_input",#name of matrix input
          value = m_area,#vaule which is the matrix
          rows = list(names = TRUE,extend = FALSE),
          cols = list(names = TRUE),
          class = "numeric"
        )
      })
      #return list to 100% area for all months
      AP_values$percent <- m_area
    }
  })
  
  
  observeEvent(input$GHD_save, { # When action button "Design_save" is clicked run code in this section
    field_test <- FALSE
    
    #Test to make sure all fields are filled
    if(input$New_GHD_Name=="" | input$New_GHD_Length=="" | input$New_GHD_Width==""| input$New_GHD_Trans=="" | input$New_GHD_TargetDLI=="" | input$New_GHD_Efficacy==""){
      shinyalert("Please make sure all fields are filled.", type="error")
    } 
    else{
      field_test <- TRUE
    }
    #Test to make sure only numbers are used for inputs
    trans_test <- as.numeric(input$New_GHD_Trans)
    DLI_test <- as.numeric(input$New_GHD_TargetDLI)
    eff_test <- as.numeric(input$New_GHD_Efficacy)
    length_test <- as.numeric(input$New_GHD_Length)
    width_test <- as.numeric(input$New_GHD_Width)
    f_trans_test <- FALSE
    f_DLI_test <- FALSE
    f_Eff_test <- FALSE
    lw_test <- FALSE
    
    if(is.na(trans_test) | trans_test <= 1 | trans_test > 100){
      shinyalert("Please check Transmission percentage: no % needed, enter as 90 if 90%", type="error")
      updateTextInput(session,"New_GHD_Trans", value="")
    } else{
      f_trans_test <- TRUE
    }
    
    if(is.na(DLI_test) | DLI_test <= 0){
      shinyalert("Please check Target DLI: only positive numerical values are accepted", type="error")
      updateTextInput(session,"New_GHD_TargetDLI", value="")
    } else{
      f_DLI_test <- TRUE
    }
    
    if(is.na(eff_test) | eff_test <= 0 | eff_test > 100){
      shinyalert("Please check Lighting Efficacy: only positive numerical values between 0 and 100 are accepted", type="error")
      updateTextInput(session,"New_GHD_Efficacy", value="")
    } else{
      f_Eff_test <- TRUE
    }
    
    if(is.na(length_test) | is.na(width_test) | length_test <= 0 | width_test <= 0 | input$New_GHD_Length=="0" | input$New_GHD_Width=="0"){
      shinyalert("Please enter non zero values for length and width", type="error")
      updateTextInput(session,"New_GHD_Length", value="1")
      updateTextInput(session,"New_GHD_Width", value="1")
    } else{
      lw_test <- TRUE
    }
    #store input values in reactive list
    if(field_test==TRUE & f_trans_test==TRUE & f_DLI_test==TRUE & f_Eff_test==TRUE & lw_test==TRUE){
      GHD_values$Name <- input$New_GHD_Name
      GHD_values$Area <- isolate({input$New_GHD_Length}) * isolate({input$New_GHD_Width})
      GHD_values$Trans <- input$New_GHD_Trans
      GHD_values$TargetDLI <- input$New_GHD_TargetDLI
      GHD_values$Efficacy <- input$New_GHD_Efficacy
      shinyalert("Success! Now go to the Electricity Cost tab to view your results.")
    }
    
    #convert percent to a decimal 
    AP_values$PerOfOne <- data.frame(AP_values$percent)/100
    
    #loop to add percent area by month records to percent_area table 
    percent <- list()
    month <- list()
    for (i in 1:12){ #for each month of the year
      PerOfOne_i <- AP_values$PerOfOne[1,i] #percent of area
      MonthNum_i <- i #month number
      percent[[i]] <- PerOfOne_i
      month[[i]] <- MonthNum_i
    }
    percent1 <- unlist(percent)
    month1 <- unlist(month)
    
    
    Area <- data.frame(percent1,month1)
    names(Area) <- c("Percent", "Month")
    
    #Make copy of area table to display to user
    Area2 <- Area
    Area2$Percent2 <- label_percent()(Area$Percent) 
    
    #Transpose the area table
    t_area <- t(Area2)
    colnames(t_area) = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
    
    #Remove extra columns to display only month and area %
    output$Percent_table <- renderTable(t_area[3,, drop=F])
    output$Percent_table_SC <- renderTable(t_area[3,, drop=F])
    output$Percent_table_CC <- renderTable(t_area[3,, drop=F])
    
    val <- list()
    weeksum <- list()
    weekavg <- list()
    weekStart <- c(FirstMonday)
    
    for(i in 1:53){
      weekStart[[1 + (i)]]<- as_date(FirstMonday + weeks(i))
      for (j in seq(0,7,1)){
        val[j] <- AP_values$PerOfOne[[(month(weekStart[i] + days(j)))]]
      }
      weeksum[[i]] <- val
      weekavg[i] <- mean(sapply(weeksum[[i]],sum))
    }
    
    #loop to drop the 53rd week if it starts in the next year
    if (year(weekStart[53]) == year(Sys.Date() + years(1))){
      weeksum <- weeksum[-53]
      weekavg <- weekavg[-53]
    }
    length(weekStart) = length(weekavg) #remove the excess weeks from the defining loop
    
    #unlist the values
    weekavg1 <- unlist(weekavg)
    week1 <- unlist(weekStart)
    
    #Manually adjust overlapping months (easy). This and next year are done. Takes 3-5 mins.
    if (lubridate::year(as.Date(Sys.Date(), format = "%Y-%m-%d")) == 2022){
      weekavg1[5] <- (1*AP_values$PerOfOne[[1]] + 6*AP_values$PerOfOne[[2]])/7
      weekavg1[9] <- (1*AP_values$PerOfOne[[2]] + 6*AP_values$PerOfOne[[3]])/7
      weekavg1[13] <- (4*AP_values$PerOfOne[[3]] + 3*AP_values$PerOfOne[[4]])/7
      weekavg1[17] <- (6*AP_values$PerOfOne[[4]] + 1*AP_values$PerOfOne[[5]])/7
      weekavg1[22] <- (2*AP_values$PerOfOne[[5]] + 5*AP_values$PerOfOne[[6]])/7
      weekavg1[26] <- (4*AP_values$PerOfOne[[6]] + 3*AP_values$PerOfOne[[7]])/7
      weekavg1[35] <- (3*AP_values$PerOfOne[[8]] + 4*AP_values$PerOfOne[[9]])/7
      weekavg1[39] <- (5*AP_values$PerOfOne[[9]] + 2*AP_values$PerOfOne[[10]])/7
      weekavg1[44] <- (1*AP_values$PerOfOne[[10]] + 6*AP_values$PerOfOne[[11]])/7
      weekavg1[48] <- (3*AP_values$PerOfOne[[11]] + 4*AP_values$PerOfOne[[12]])/7
      weekavg1[52] <- AP_values$PerOfOne[[12]] 
    } else if (lubridate::year(as.Date(Sys.Date(), format = "%Y-%m-%d")) == 2023) {
      weekavg1[5] <- (2*AP_values$PerOfOne[[1]] + 5*AP_values$PerOfOne[[2]])/7
      weekavg1[9] <- (2*AP_values$PerOfOne[[2]] + 5*AP_values$PerOfOne[[3]])/7
      weekavg1[13] <- (5*AP_values$PerOfOne[[3]] + 2*AP_values$PerOfOne[[4]])/7
      weekavg1[22] <- (3*AP_values$PerOfOne[[5]] + 4*AP_values$PerOfOne[[6]])/7
      weekavg1[26] <- (5*AP_values$PerOfOne[[6]] + 2*AP_values$PerOfOne[[7]])/7
      weekavg1[31] <- (1*AP_values$PerOfOne[[7]] + 6*AP_values$PerOfOne[[8]])/7
      weekavg1[35] <- (4*AP_values$PerOfOne[[8]] + 3*AP_values$PerOfOne[[9]])/7
      weekavg1[39] <- (6*AP_values$PerOfOne[[9]] + 1*AP_values$PerOfOne[[10]])/7
      weekavg1[44] <- (2*AP_values$PerOfOne[[10]] + 5*AP_values$PerOfOne[[11]])/7
      weekavg1[48] <- (3*AP_values$PerOfOne[[11]] + 4*AP_values$PerOfOne[[12]])/7
      weekavg1[52] <- AP_values$PerOfOne[[12]]
    } else if (lubridate::year(as.Date(Sys.Date(), format = "%Y-%m-%d")) == 2024){ #Leap year
      weekavg1[5] <- (3*AP_values$PerOfOne[[1]] + 4*AP_values$PerOfOne[[2]])/7
      weekavg1[9] <- (4*AP_values$PerOfOne[[2]] + 3*AP_values$PerOfOne[[3]])/7
      weekavg1[18] <- (2*AP_values$PerOfOne[[4]] + 5*AP_values$PerOfOne[[5]])/7
      weekavg1[22] <- (5*AP_values$PerOfOne[[5]] + 2*AP_values$PerOfOne[[6]])/7
      weekavg1[31] <- (3*AP_values$PerOfOne[[7]] + 4*AP_values$PerOfOne[[8]])/7
      weekavg1[35] <- (6*AP_values$PerOfOne[[8]] + 1*AP_values$PerOfOne[[9]])/7
      weekavg1[40] <- (1*AP_values$PerOfOne[[9]] + 6*AP_values$PerOfOne[[10]])/7
      weekavg1[44] <- (4*AP_values$PerOfOne[[10]] + 3*AP_values$PerOfOne[[11]])/7
      weekavg1[48] <- (6*AP_values$PerOfOne[[11]] + 1*AP_values$PerOfOne[[12]])/7
      weekavg1[52] <- AP_values$PerOfOne[[12]]
    } else { # for 2025 and beyond, this needs to be recalculated. Currently is the same value from 2022
      weekavg1[5] <- (1*AP_values$PerOfOne[[1]] + 6*AP_values$PerOfOne[[2]])/7
      weekavg1[9] <- (1*AP_values$PerOfOne[[2]] + 6*AP_values$PerOfOne[[3]])/7
      weekavg1[13] <- (4*AP_values$PerOfOne[[3]] + 3*AP_values$PerOfOne[[4]])/7
      weekavg1[17] <- (6*AP_values$PerOfOne[[4]] + 1*AP_values$PerOfOne[[5]])/7
      weekavg1[22] <- (2*AP_values$PerOfOne[[5]] + 5*AP_values$PerOfOne[[6]])/7
      weekavg1[26] <- (4*AP_values$PerOfOne[[6]] + 3*AP_values$PerOfOne[[7]])/7
      weekavg1[35] <- (3*AP_values$PerOfOne[[8]] + 4*AP_values$PerOfOne[[9]])/7
      weekavg1[39] <- (5*AP_values$PerOfOne[[9]] + 2*AP_values$PerOfOne[[10]])/7
      weekavg1[44] <- (1*AP_values$PerOfOne[[10]] + 6*AP_values$PerOfOne[[11]])/7
      weekavg1[48] <- (3*AP_values$PerOfOne[[11]] + 4*AP_values$PerOfOne[[12]])/7
    }
    
    Area_week <- data.frame(weekavg1,week1)
    names(Area_week) <- c("Percent", "Week")
    
    #Make copy of area table to display to user
    Area_week_2 <- Area_week
    Area_week_2$Percent2 <- label_percent()(Area_week$Percent) 
    
    #Transpose the area table
    t_area_week <- t(Area_week_2)
    colnames(t_area_week) = week1
    
    #Remove extra columns to display only month and area %
    output$table_week <- renderTable(t_area_week[3,, drop=F])
    
  })
  
  ###############################################.
  ## Server - Electricity Costs ----
  ###############################################.
  
  #loction value
  output$ui_EC_location <- renderUI({
    textInput(inputId = 'EC_Select_location',
              label ='Location',
              width = "auto",
              value=Loc_values$Name)
  })
  
  # design value
  output$ui_EC_design <- renderUI({
    textInput(inputId = 'EC_Select_design',
              label ='Design',
              width = "auto",
              value=GHD_values$Name)
  })
  
  ####################################.
  ## Calculations and Graphs  ####
  ####################################.
  observeEvent(input$EC_Choices, { # When action button "EC_Choices" is clicked run code in this section
    ####################################.
    ## Variables and Dataframes ####
    ####################################.
    shinyalert("Success! Updating your results.")
    {
      # Assign variables from user inputs
      Location <- as.character(Loc_values$Name)
      ele_rate <- as.numeric(Loc_values$Elec_Rate)
      Design <- as.character(GHD_values$Name)
      trans <- as.numeric(GHD_values$Trans)
      target <- as.numeric(GHD_values$TargetDLI)
      area <- as.numeric(GHD_values$Area)
      efficacy <- as.numeric(GHD_values$Efficacy)
      
      #Calculate the area for each month based on percent of coverage
      # PA_Months <- as.numeric(c(EC.Area$jan,EC.Area$feb,EC.Area$mar,EC.Area$apr,EC.Area$may,EC.Area$jun,EC.Area$jul,EC.Area$aug,EC.Area$sept,EC.Area$oct,EC.Area$nov,EC.Area$decem))
      PA_Months <- as.numeric(AP_values$percent)/100
      monthly_areaft2 <- vector(mode="numeric", length=12)
      monthly_aream2 <- vector(mode="numeric", length=12)
      for(i in 1:12) {
        # monthly_areaft2[i] <- area*(PA_Months[i]/100)
        monthly_areaft2[i] <- area*(PA_Months[i])
        monthly_aream2[i] <- monthly_areaft2[i]/10.764
      }
      
      val <- list()
      weeksum <- list()
      weekavg <- list()
      weekStart <- c(FirstMonday)
      
      for(i in 1:53){
        weekStart[[1 + (i)]]<- as_date(FirstMonday + weeks(i))
        for (j in seq(0,7,1)){
          val[j] <- AP_values$PerOfOne[[(month(weekStart[i] + days(j)))]]
        }
        weeksum[[i]] <- val
        weekavg[i] <- mean(sapply(weeksum[[i]],sum))
      }
      
      #loop to drop the 53rd week if it starts in the next year
      if (year(weekStart[53]) == year(Sys.Date() + years(1))){
        weeksum <- weeksum[-53]
        weekavg <- weekavg[-53]
      }
      length(weekStart) = length(weekavg) #remove the excess weeks from the defining loop
      
      #unlist the values
      weekavg1 <- unlist(weekavg)
      week1 <- unlist(weekStart)
      
      #Manually adjust overlapping months (easy). This and next year are done. Takes 3-5 mins.
      if (lubridate::year(as.Date(Sys.Date(), format = "%Y-%m-%d")) == 2022){
        weekavg1[5] <- (1*AP_values$PerOfOne[[1]] + 6*AP_values$PerOfOne[[2]])/7
        weekavg1[9] <- (1*AP_values$PerOfOne[[2]] + 6*AP_values$PerOfOne[[3]])/7
        weekavg1[13] <- (4*AP_values$PerOfOne[[3]] + 3*AP_values$PerOfOne[[4]])/7
        weekavg1[17] <- (6*AP_values$PerOfOne[[4]] + 1*AP_values$PerOfOne[[5]])/7
        weekavg1[22] <- (2*AP_values$PerOfOne[[5]] + 5*AP_values$PerOfOne[[6]])/7
        weekavg1[26] <- (4*AP_values$PerOfOne[[6]] + 3*AP_values$PerOfOne[[7]])/7
        weekavg1[35] <- (3*AP_values$PerOfOne[[8]] + 4*AP_values$PerOfOne[[9]])/7
        weekavg1[39] <- (5*AP_values$PerOfOne[[9]] + 2*AP_values$PerOfOne[[10]])/7
        weekavg1[44] <- (1*AP_values$PerOfOne[[10]] + 6*AP_values$PerOfOne[[11]])/7
        weekavg1[48] <- (3*AP_values$PerOfOne[[11]] + 4*AP_values$PerOfOne[[12]])/7
        weekavg1[52] <- AP_values$PerOfOne[[12]] 
      } else if (lubridate::year(as.Date(Sys.Date(), format = "%Y-%m-%d")) == 2023) {
        weekavg1[5] <- (2*AP_values$PerOfOne[[1]] + 5*AP_values$PerOfOne[[2]])/7
        weekavg1[9] <- (2*AP_values$PerOfOne[[2]] + 5*AP_values$PerOfOne[[3]])/7
        weekavg1[13] <- (5*AP_values$PerOfOne[[3]] + 2*AP_values$PerOfOne[[4]])/7
        weekavg1[22] <- (3*AP_values$PerOfOne[[5]] + 4*AP_values$PerOfOne[[6]])/7
        weekavg1[26] <- (5*AP_values$PerOfOne[[6]] + 2*AP_values$PerOfOne[[7]])/7
        weekavg1[31] <- (1*AP_values$PerOfOne[[7]] + 6*AP_values$PerOfOne[[8]])/7
        weekavg1[35] <- (4*AP_values$PerOfOne[[8]] + 3*AP_values$PerOfOne[[9]])/7
        weekavg1[39] <- (6*AP_values$PerOfOne[[9]] + 1*AP_values$PerOfOne[[10]])/7
        weekavg1[44] <- (2*AP_values$PerOfOne[[10]] + 5*AP_values$PerOfOne[[11]])/7
        weekavg1[48] <- (3*AP_values$PerOfOne[[11]] + 4*AP_values$PerOfOne[[12]])/7
        weekavg1[52] <- AP_values$PerOfOne[[12]]
      } else if (lubridate::year(as.Date(Sys.Date(), format = "%Y-%m-%d")) == 2024){ #Leap year
        weekavg1[5] <- (3*AP_values$PerOfOne[[1]] + 4*AP_values$PerOfOne[[2]])/7
        weekavg1[9] <- (4*AP_values$PerOfOne[[2]] + 3*AP_values$PerOfOne[[3]])/7
        weekavg1[18] <- (2*AP_values$PerOfOne[[4]] + 5*AP_values$PerOfOne[[5]])/7
        weekavg1[22] <- (5*AP_values$PerOfOne[[5]] + 2*AP_values$PerOfOne[[6]])/7
        weekavg1[31] <- (3*AP_values$PerOfOne[[7]] + 4*AP_values$PerOfOne[[8]])/7
        weekavg1[35] <- (6*AP_values$PerOfOne[[8]] + 1*AP_values$PerOfOne[[9]])/7
        weekavg1[40] <- (1*AP_values$PerOfOne[[9]] + 6*AP_values$PerOfOne[[10]])/7
        weekavg1[44] <- (4*AP_values$PerOfOne[[10]] + 3*AP_values$PerOfOne[[11]])/7
        weekavg1[48] <- (6*AP_values$PerOfOne[[11]] + 1*AP_values$PerOfOne[[12]])/7
        weekavg1[52] <- AP_values$PerOfOne[[12]]
      } else { # for 2025 and beyond, this needs to be recalculated. Currently is the same value from 2022
        weekavg1[5] <- (1*AP_values$PerOfOne[[1]] + 6*AP_values$PerOfOne[[2]])/7
        weekavg1[9] <- (1*AP_values$PerOfOne[[2]] + 6*AP_values$PerOfOne[[3]])/7
        weekavg1[13] <- (4*AP_values$PerOfOne[[3]] + 3*AP_values$PerOfOne[[4]])/7
        weekavg1[17] <- (6*AP_values$PerOfOne[[4]] + 1*AP_values$PerOfOne[[5]])/7
        weekavg1[22] <- (2*AP_values$PerOfOne[[5]] + 5*AP_values$PerOfOne[[6]])/7
        weekavg1[26] <- (4*AP_values$PerOfOne[[6]] + 3*AP_values$PerOfOne[[7]])/7
        weekavg1[35] <- (3*AP_values$PerOfOne[[8]] + 4*AP_values$PerOfOne[[9]])/7
        weekavg1[39] <- (5*AP_values$PerOfOne[[9]] + 2*AP_values$PerOfOne[[10]])/7
        weekavg1[44] <- (1*AP_values$PerOfOne[[10]] + 6*AP_values$PerOfOne[[11]])/7
        weekavg1[48] <- (3*AP_values$PerOfOne[[11]] + 4*AP_values$PerOfOne[[12]])/7
      }
      
      
      PA_Weeks <- weekavg1
      
      radiation <- XTRA_values$radiation
      
      radiationWeek <- radiation
      radiationWeek$Week <- week(date(radiation$TimeStamp) - day(FirstMonday - days(1)))
      radiationWeek$Month <- month(date(radiation$TimeStamp))
      
      
      ####################################.
      ## Calculations for Ele Cost ####
      ####################################.
      # Calculations for Daily Light Integral (DLI):
      # 1. Convert GHI value for one hour from Wh * m^-2 to micromoles * m^-2*h-1
      radiation$ghi <- (as.numeric(radiation$ghi)*7272)
      # 2. Calculate Daily Light Integral for each hour of the day (loop from hour 0 to hour 23) sum.
      EC_df <- radiation %>%
        group_by(date(TimeStamp),Month) %>%                   
        summarize(dli_umol = sum(ghi)) %>% #"dli_umol"
        na.omit
      colnames(EC_df) <- c("TimeStamp","Month","dli_umol")
      # 3. Convert micromoles to moles
      EC_df$dli_mol = EC_df$dli_umol/(1*10^6)
      # Grower Iput: Greenhouse Transmission -> Retrieve: Daily Light Integral
      EC_df$trans <- trans
      # Calculate Sunlight Present in Greenhouse (SPG):
      EC_df$spg_mol <- EC_df$dli_mol * (trans/100) #transmission as %
      # Grower Input: Target Daily Light Integral (TDLI or plant light requirement) -> Retrieve: Daily Light
      EC_df$dli_target <- target
      # Calculate Supplemental Light Needed (SL):
      EC_df$SL_mol <- ifelse(EC_df$spg_mol > target, 0, target - EC_df$spg_mol)
      #EC_df$SL_mol_dark <- ifelse(EC_df$spg_mol > target, target, target)
      # Total light given
      EC_df$totalDLImols <- EC_df$spg_mol + EC_df$SL_mol
      # 1. Calculate electricity cost in S/m^2d
      # Lighting System Efficacy input by grower in micromol/J
      # Lighting System Efficacy micromol/J x 3.6 = Lighting System Efficacy mol/(m^2d)
      
      # Electricity Cost $/(m^2d) #HEYCJ
      EC_df$Elec_rate <- ele_rate
      EC_df$EC_m2d <- (EC_df$SL_mol/(efficacy*3.6))*ele_rate
      #EC_df$EC_m2d_dark <- (EC_df$SL_mol_dark/(efficacy*3.6))*ele_rate
      
      # 2. Convert the grower input of greenhouse size in square feet to square meters
      # Greenhouse Size m^2 = Greenhouse Size ft^2/10.764
      EC_df$aream2 <- (area/10.764)
      # 3. Multiply by the size of the greenhouse
      # Total Electricity Cost $/d = Electricity COst $/(m^2d)*(Greenhouse size m^2)
      # EC_df$ECtotald <- (EC_df$EC_m2d * EC_df$aream2)
      
      # Monthly ele cost 
      EC_df_M <- EC_df %>%
        group_by(Month) %>%
        summarize(EC_m2m  = sum(EC_m2d)) %>% #,EC_m2m_dark = sum(EC_m2d_dark)
        na.omit
      
      EC_df_M$MonthName <- month.abb[as.numeric(EC_df_M$Month)]
      EC_df_M$EC_acre <- EC_df_M$EC_m2m*4046.86*PA_Months #4046.86m2 per acre
      EC_df_M$EC_ftsq <- EC_df_M$EC_m2m/10.764*PA_Months
      
      EC_df_M_Graph <- data.frame(EC_df_M$MonthName,EC_df_M$EC_acre)
      colnames(EC_df_M_Graph) <- c("Month","per_acre")
      EC_df_M$EC_design <- EC_df_M$EC_m2m*(monthly_aream2[as.numeric(EC_df_M$Month)])
      #EC_df_M$EC_dark <- rep(EC_df_M$EC_m2m_dark*(monthly_aream2[as.numeric(EC_df_M$Month)]))
      
      EC_df_M_Display <- EC_df_M[c("MonthName","EC_ftsq","EC_acre","EC_design")]
      colnames(EC_df_M_Display) <- c("Month","$ Per ft2","$ Per acre","$ Total Design") #,"$ Total Design (No Sunlight)"
      
      # Calculations for Daily Light Integral (DLI):
      # 1. Convert GHI value for one hour from Wh * m^-2 to micromoles * m^-2*h-1
      radiationWeek$ghi <- (as.numeric(radiationWeek$ghi)*7272)
      # 2. Calculate Daily Light Integral for each hour of the day (loop from hour 0 to hour 23) sum.
      EC_df_Week <- radiationWeek %>%
        group_by(date(TimeStamp),Week) %>%                    
        summarize(dli_umol = sum(ghi)) %>% #"dli_umol"
        na.omit
      colnames(EC_df_Week) <- c("TimeStamp","Week","dli_umol")
      # 3. Convert micromoles to moles
      EC_df_Week$dli_mol = EC_df_Week$dli_umol/(1*10^6)
      # Grower Iput: Greenhouse Transmission -> Retrieve: Daily Light Integral
      EC_df_Week$trans <- trans
      # Calculate Sunlight Present in Greenhouse (SPG):
      EC_df_Week$spg_mol <- EC_df_Week$dli_mol * (trans/100) #transmission as %
      # Grower Input: Target Daily Light Integral (TDLI or plant light requirement) -> Retrieve: Daily Light
      EC_df_Week$dli_target <- target
      # Calculate Supplemental Light Needed (SL):
      EC_df_Week$SL_mol <- ifelse(EC_df_Week$spg_mol > target, 0, target - EC_df_Week$spg_mol)
      #EC_df_Week$SL_mol_dark <- ifelse(EC_df_Week$spg_mol > target, target, target)
      # Total light given
      EC_df_Week$totalDLImols <- EC_df_Week$spg_mol + EC_df_Week$SL_mol
      # 1. Calculate electricity cost in S/m^2d
      # Lighting System Efficacy input by grower in micromol/J
      # Lighting System Efficacy micromol/J x 3.6 = Lighting System Efficacy mol/(m^2d)
      
      # Electricity Cost $/(m^2d) #HEYCJ
      EC_df_Week$Elec_rate <- ele_rate
      EC_df_Week$EC_m2d <- (EC_df_Week$SL_mol/(efficacy*3.6))*ele_rate
      #EC_df_Week$EC_m2d_dark <- (EC_df_Week$SL_mol_dark/(efficacy*3.6))*ele_rate
      # 2. Convert the grower input of greenhouse size in square feet to square meters
      # Greenhouse Size m^2 = Greenhouse Size ft^2/10.764
      EC_df_Week$aream2 <- (area/10.764)
      # 3. Multiply by the size of the greenhouse
      # Total Electricity Cost $/d = Electricity COst $/(m^2d)*(Greenhouse size m^2)
      # EC_df$ECtotald <- (EC_df$EC_m2d * EC_df$aream2)
      #      # AP_values$percent
      
      weekly_areaft2 <- vector(mode="numeric", length = length(weekStart))
      weekly_aream2 <- vector(mode="numeric", length = length(weekStart))
      
      for(i in 1:length(weekStart)) {
        # monthly_areaft2[i] <- area*(PA_Months[i]/100)
        weekly_areaft2[i] <- area*(PA_Weeks[i])
        weekly_aream2[i] <- weekly_areaft2[i]/10.764
      }
      
      # Weekly ele cost 
      EC_df_W <- EC_df_Week %>%
        group_by(Week) %>%
        summarize(EC_m2m  = sum(EC_m2d)) %>% #,EC_m2m_dark = sum(EC_m2d_dark)
        na.omit
      
      EC_df_W$WeekNum <- paste(EC_df_W$Week)  
      EC_df_W$EC_acre <- EC_df_W$EC_m2m*4046.86*PA_Weeks
      EC_df_W$EC_ftsq <- EC_df_W$EC_m2m/10.764*PA_Weeks
      EC_df_W$EC_design <- rep(EC_df_W$EC_m2m*(weekly_aream2[as.numeric(EC_df_W$Week)]))
      #EC_df_W$EC_dark <- rep(EC_df_W$EC_m2m_dark*(weekly_aream2[as.numeric(EC_df_W$Week)]))
      
      a <- list()
      for (i in 1:52) {
        a[i] = toString(x = weekStart[i])
      }
      for (i in 1:52) {
        a[i] = substr(weekStart[i],nchar(weekStart[i])+1, 10)
      }
      EC_df_W$EC_Floor <- a
      #format(as.Date(floor_date(as.Date(paste(2020, EC_df_W$Week - 1, 1, sep="-"), "%Y-%U-%u"), unit = "week") + days(1)))
      EC_df_W_Display <- EC_df_W[c("WeekNum","EC_ftsq","EC_acre","EC_design")] #,9
      colnames(EC_df_W_Display) <- c("Week","$ Per ft2","$ Per acre","$ Total Design") #, "Start Date 2021"
      
      
      # Annual ele cost #HEYCJ
      Annual_EC <- data.frame(sum(EC_df_W$EC_ftsq),sum(EC_df_W$EC_acre),sum(EC_df_W$EC_design)) #,sum(EC_df_M$EC_dark)
      colnames(Annual_EC) <- c("AEC_ft2m","AEC_acre","AEC_design") #, "AEC_dark"
      Annual_EC_Display <- Annual_EC
      colnames(Annual_EC_Display) <- c("$ Per ft2","$ Per acre","$ Total Design") #, "$ Total Design (No Sunlight)"
      Annual_EC_Display$`$ Per ft2`<- prettyNum(round(Annual_EC_Display$`$ Per ft2`, digits = 3), big.mark = ',')
      Annual_EC_Display$`$ Per acre`<- prettyNum(round(Annual_EC_Display$`$ Per acre`, digits = 0), big.mark = ',')
      Annual_EC_Display$`$ Total Design`<- prettyNum(round(Annual_EC_Display$`$ Total Design`, digits = 0), big.mark = ',')
      #Annual_EC_Display$`$ Total Design (No Sunlight)`<- prettyNum(round(Annual_EC_Display$`$ Total Design (No Sunlight)`, digits = 2), big.mark = ',')
      
      
      # Weekly ele cost #HEYCJ
      # Weekly_EC <- data.frame(sum(EC_df_M$EC_ftsq)/52,sum(EC_df_M$EC_acre)/52,sum(EC_df_M$EC_design)/52) #,sum(EC_df_M$EC_dark)/52
      # colnames(Weekly_EC) <- c("WEC_ft2m","WEC_acre","WEC_design") #,"WEC_dark"
      # Weekly_EC_Display <- Weekly_EC
      # colnames(Weekly_EC_Display) <- c("$ Per ft2","$ Per acre","$ Total Design") #,"$ Total Design (No Sunlight)"
      # Weekly_EC_Display$`$ Per ft2`<- prettyNum(round(Weekly_EC_Display$`$ Per ft2`, digits = 3), big.mark = ',')
      # Weekly_EC_Display$`$ Per acre`<- prettyNum(round(Weekly_EC_Display$`$ Per acre`, digits = 0), big.mark = ',')
      # Weekly_EC_Display$`$ Total Design`<- prettyNum(round(Weekly_EC_Display$`$ Total Design`, digits = 0), big.mark = ',')
      #Weekly_EC_Display$`$ Total Design (No Sunlight)`<- prettyNum(round(Weekly_EC_Display$`$ Total Design (No Sunlight)`, digits = 2), big.mark = ',')
      
      #Electricity Cost Table #HEYCJ
      output$EC_table <- renderTable({
        EC_table_output <- data.frame(trans,target,efficacy,ele_rate,area)
        names(EC_table_output) <- (c("Greenhouse transmission (%)","Target DLI (mol/m2/day)","Lighting efficacy (umol/J)", "Electricity Cost ($/kWh)", "Area (ft2)"))
        EC_table_output
        
      }) #bracket EC_table
      
      #####################################.
      #### Download Results #####
      #####################################.
      Grower_input <- data.frame(trans,target,efficacy,ele_rate,area)
      names(Grower_input) <- (c("Greenhouse transmission (%)","Target DLI (mol/m2/day)","Lighting efficacy (umol/J)", "Electricity Cost ($/kWh)", "Area (ft2)"))
      
      EC_df_M_Display$`$ Per ft2` <- round(EC_df_M_Display$`$ Per ft2`, digits = 3)
      EC_df_M_Display$`$ Per acre` <- round(EC_df_M_Display$`$ Per acre`, digits = 0)
      EC_df_M_Display$`$ Total Design` <- round(EC_df_M_Display$`$ Total Design`, digits = 0)
      
      EC_df_W_Display$`$ Per ft2` <- prettyNum(round(EC_df_W_Display$`$ Per ft2`, digits = 3), big.mark = ',')
      EC_df_W_Display$`$ Per acre` <- prettyNum(round(EC_df_W_Display$`$ Per acre`, digits = 0), big.mark = ',')
      EC_df_W_Display$`$ Total Design` <- prettyNum(round(EC_df_W_Display$`$ Total Design`, digits = 0), big.mark = ',')
      
      EC_df2 <- EC_df
      names(EC_df2) <- c("TimeStamp", "Month", "DLI_umol", "DLI_mol", "Transmission (%)", "Sunlight Present in Greenhouse (mol)", "DLI Target", 
                         "Supplemental Light Needed (mol)", "Total DLI Mols", "Electricity Rate", "Cost per m2 per day", "Area (m2)")
      EC_df2 <- EC_df2[-c(3)]
      EC_df2 <- EC_df2[-c(11)]
      EC_df2$`Cost per m2 per day` <- round( EC_df2$`Cost per m2 per day`, digits = 2)
      
      months <-  c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
      Monthly_Area_Percent <- data.frame(months, label_percent()(PA_Months))
      names(Monthly_Area_Percent) <- c("Month", "Percent Area")
      
      #Store above tables in a list to write to Excel File
      EC_data_list <- reactive({
        list(
          Grower_Input = Grower_input,
          Monthly_Area = Monthly_Area_Percent,
          Monthly_Costs = EC_df_M_Display, 
          Weekly_Costs = EC_df_W_Display,
          Detailed_Electricity_Data = EC_df2
        )
      })
      #Button to Download Results 
      output$EC_downloadData <- downloadHandler(
        filename = paste0("LightCalc_", as.character(Sys.Date()), ".xlsx"), #File with current date
        content = function(file) {
          write_xlsx(EC_data_list(), path=file, col_names = TRUE)
        } 
      )
      
      
      
    } #bracket Variables and Dataframes code chunk
    
    #################################.
    ### Render Tables and Graph ###
    #################################.
    # Annual lighting cost  ($/acre)
    output$Annual_EC_Display_table<- renderTable({
      Annual_EC_Display
      
    })#bracket Annual_EC_Display_table
    # output$Weekly_EC_Display_table<- renderTable({
    #   Weekly_EC_Display
    # })
    
    #Weekly lighting costs ($/acre) Table
    output$EC_df_W_table <- renderTable({
      EC_df_W_Display
      
    })#bracket EC_df_M_table
    
    #Monthy lighting costs ($/acre) Graph
    output$EC_df_M_histo <- renderPlot({
      
      ggplot(data=EC_df_M_Graph, aes(x=Month, y=per_acre)) +
        geom_bar(stat="identity", fill="steelblue") + 
        geom_text(aes(label = prettyNum(round(per_acre,0),big.mark = ",")), vjust = 1, colour = "grey") + 
        labs(title = NULL, x = NULL, y = NULL)+
        scale_y_continuous(labels = dollar) + 
        scale_x_discrete(limits=EC_df_M_Graph$Month) + 
        theme(text = element_text(size=20))
      
    })#bracket EC_df_M_histo
    
  })
} #bracket server