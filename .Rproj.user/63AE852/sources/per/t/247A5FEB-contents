library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(dplyr)
library(lubridate) #to convert date into day
library(DataExplorer)
library(gmailr)
library(purrr)
library(DT)
library(plotly)
library(shinycssloaders)
library(rgdal)
library(shinythemes)



library("shinycssloaders", lib.loc="~/R/win-library/3.4")
library("gmailr", lib.loc="~/R/win-library/3.4")
library("plotly", lib.loc="~/R/win-library/3.4")
library("httr", lib.loc="~/R/win-library/3.4")
library("readr", lib.loc="~/R/win-library/3.4")
library("dplyr", lib.loc="~/R/win-library/3.4")
library("lubridate", lib.loc="~/R/win-library/3.4")
library("DataExplorer", lib.loc="~/R/win-library/3.4")
library("purrr", lib.loc="~/R/win-library/3.4")
library("DT", lib.loc="~/R/win-library/3.4")
library("leaflet", lib.loc="~/R/win-library/3.4")

options(warn=-1)


# # 1. Reading file
 dataframe <- read_csv("data1 - with numbers and email11.1city.csv")
 custData1 <- na.omit(dataframe)
 
 custData <- custData1[1:20000,]
 
 cities <- read.csv("ccit.csv")
 
 
 
 
 


use_secret_file("blutechtesting.json")
email_sender <- '<blutech.testing@gmail.com>' # your Gmail address
optional_bcc <- 'Anonymous <blutech.testing@gmail.com>'
#  
 #######SMS AUTH#######
 AUTH_ID = "MAYWU1OWRMMDHJYTZJNW"
 AUTH_TOKEN = "ZTc4ZTAzZWI5YTk4ZGRlZjY2MzQ3MTMwMjBhZjE3"

 url = "https://api.plivo.com/v1/Account/MAYWU1OWRMMDHJYTZJNW/Message/"
 senderNumber <- paste("923335417533")
 
 
 ########Global Variables#######################
 plotWidth = 800
 plotHeight = 400
 
#
# #separate date and time components of InvoiceDate
 custData$date <- sapply(custData$InvoiceDate, FUN = function(x) {strsplit(x, split = '[ ]')[[1]][1]})
 custData$time <- sapply(custData$InvoiceDate, FUN = function(x) {strsplit(x, split = '[ ]')[[1]][2]})


# # create month, year and hour of day variables
 custData$month <- sapply(custData$date, FUN = function(x) {strsplit(x, split = '[/]')[[1]][1]})
 custData$year <- sapply(custData$date, FUN = function(x) {strsplit(x, split = '[/]')[[1]][3]})
 custData$hourOfDay <- sapply(custData$time, FUN = function(x) {strsplit(x, split = '[:]')[[1]][1]})

#
# # convert date column datatype to date class
 custData$date <- as.Date(custData$date, "%m/%d/%Y")
#
# # now create a new column dayOfWeek from date column
 custData$dayOfWeek <- wday(custData$date, label=TRUE, abbr = FALSE)
#
# # adding a new column lineTotal, it is a prod of UnitPrice and Quantity
 custData <- custData %>% mutate(lineTotal = Quantity * UnitPrice)
#
# # now in order to start segmentation, we need to convert some of the columns as factors
custData$Country <- as.factor(custData$Country)
custData$month <- as.factor(custData$month)
custData$year <- as.factor(custData$year)
levels(custData$year) <- c(2010,2011)
custData$hourOfDay <- as.factor(custData$hourOfDay)
custData$dayOfWeek <- as.factor(custData$dayOfWeek)
#
#
# # creating a new dataframe to get more detail about per day revenue
weekdayDF <- custData %>%
  group_by(date, dayOfWeek) %>%
  summarise(revenue = sum(lineTotal), transactions = n_distinct(InvoiceNo)) %>%
  mutate(aveOrdVal = (round((revenue / transactions),2))) %>%
  ungroup()
#
#
#
# #creating a dataframe for countries
countrySummaryDF <- custData %>%
  group_by(Country) %>%
  summarise(revenue = sum(lineTotal), transactions = n_distinct(InvoiceNo)) %>%
  mutate(aveOrdVal = (round((revenue / transactions),2))) %>%
  ungroup() %>%
  arrange(desc(revenue))
#

for (row in 1:nrow(countrySummaryDF))
{
  countrySummaryDF[row, "City"] = as.character(cities[row, "city"])
  countrySummaryDF[row, "Longitude"] = as.double(cities[row, "long"])
  countrySummaryDF[row, "Latitude"] = as.double(cities[row, "lat"])
}









# head(countrySummaryDF, n = 10)
# unique(countrySummaryDF$Country)
#
# # Filtering dataframe to only five countries, removing UK
topFiveCountriesDF <- custData %>%
  filter(Country == 'Karachi' | Country == 'Lahore' | Country == 'Islamabad' | Country == 'Peshawar' | Country == 'Sialkot' | Country == 'Faisalabad')
#
# # Creating a new DF from filtered countries
topFiveCountrySummaryDF <- topFiveCountriesDF %>%
  group_by(Country, date) %>%
  summarise(revenue = sum(lineTotal), transactions = n_distinct(InvoiceNo), customers = n_distinct(CustomerID)) %>%
  mutate(aveOrdVal = (round((revenue / transactions),2))) %>%
  ungroup() %>%
  arrange(desc(revenue))

#head(topFiveCountrySummaryDF)

# CUSTOMER SEGMENTATION
custSummaryDF <- custData %>%
  group_by(CustomerID) %>%
  summarise(revenue = sum(lineTotal), transactions = n_distinct(InvoiceNo)) %>%
  mutate(aveOrdVal = (round((revenue / transactions),2))) %>%
  ungroup() %>%
  arrange(desc(revenue))

#head(custSummary, n = 10)

custSummaryBasic <- custData %>%
  group_by(CustomerID, InvoiceNo) %>%
  summarise(revenue = sum(lineTotal), transactions = n_distinct(InvoiceNo)) %>%
  mutate(aveOrdVal = (round((revenue / transactions),2))) %>%
  ungroup() %>%
  arrange(revenue) %>%
  mutate(cumsum=cumsum(revenue))



custSummaryBasic <- custData %>%
  group_by(InvoiceNo, CustomerID, Country, date, month, year, hourOfDay, dayOfWeek, Description, Email, Number) %>%
  summarise(orderVal = sum(lineTotal)) %>%
  mutate(recent = Sys.Date() - date) %>%
  ungroup()

custSummaryBasic$recent <- as.character(custSummaryBasic$recent)
custSummaryBasic$recentDays <- sapply(custSummaryBasic$recent, FUN = function(x) {strsplit(x, split = '[ ]')[[1]][1]})
custSummaryBasic$recentDays <- as.integer(custSummaryBasic$recentDays)

#head(custSummaryBasic, n = 5)

customerBreakdown <- custSummaryBasic %>%
  group_by(CustomerID, Country, Email, Number) %>%
  summarise(orders = n_distinct(InvoiceNo), revenue = sum(orderVal), 
            mostDay = names(which.max(table(dayOfWeek))), mostHour = names(which.max(table(hourOfDay))),
            mostProd = names(which.max(table(Description))),
            recency = min(recentDays))%>%
  ungroup() 

custBreakSum <- customerBreakdown %>%
  filter(orders > 1, revenue > 50)


# head(customerBreakdown)
customerBreakdown11 <- custBreakSum[order(-custBreakSum$revenue),]

customerBreakdown12<-head(customerBreakdown11, n = 5)

custSummary <- custData %>%
  group_by(CustomerID) %>%
  summarise(revenue = sum(lineTotal), transactions = n_distinct(InvoiceNo)) %>%
  mutate(aveOrdVal = (round((revenue / transactions),2))) %>%
  ungroup() %>%
  arrange(desc(revenue))


totalTransRevenueDF <- weekdayDF %>%
  group_by(dayOfWeek) %>%
  summarise(totalTransactions = sum(transactions),totalRevenue = sum(revenue))

#######################Product Summary Calculations##########################################
productSummary <- custData %>%
  group_by(StockCode, Description) %>%
  summarise(orders = n_distinct(InvoiceNo), revenue = sum(lineTotal), quantity = sum(Quantity), price = max(UnitPrice),
            mostDay = names(which.max(table(dayOfWeek))), mostHour = names(which.max(table(hourOfDay))), mostCountry = names(which.max(table(Country)))) %>%
  mutate(aveOrdVal = (round((revenue / orders),2))) %>%
  ungroup() %>%
  arrange(desc(revenue))

#top5 products
topprod <- head(productSummary,n = 10)


prodCountrySummary1 <- custData %>%
  group_by(Country, StockCode, Description) %>%
  summarise(orders = n_distinct(InvoiceNo), revenue = sum(lineTotal), quantity = sum(Quantity), price = max(UnitPrice),
            mostDay = names(which.max(table(dayOfWeek))), mostHour = names(which.max(table(hourOfDay)))) %>%
  mutate(aveOrdVal = (round((revenue / orders),2))) %>%
  ungroup() %>%
  arrange(desc(revenue))



productDaySummary <- custData %>% 
  group_by(dayOfWeek, StockCode, Description) %>%
  summarise(orders = n_distinct(InvoiceNo), revenue = sum(lineTotal), quantity = sum(Quantity), price = max(UnitPrice))%>%
  ungroup() %>%
  arrange(desc(revenue))

#filtering out 0 and - values
prodCountrySummary <- subset(prodCountrySummary1,revenue>0)


shinyServer(function(input, output, session) {
  
  #showing or hiding slider for date range
  # observe({
  #   if (is.null(output$sidePlot) || isTruthy(output$sidePlot)) {
  #     shinyjs::disable("sliderDaterange")
  #   } else {
  #     shinyjs::enable("sliderDaterange")
  #   }
  # })
  # 
  # 
  output$customerSidePaneltext <- renderPrint({
    print("This page shows our top five revenue generating customers and their relevant details including their most bought product.")
    
  })
  
  # output$value <- renderPrint({ input$camDay })
  
  output$topcust <- DT::renderDataTable(
    customerBreakdown12,
    options = list(scrollX = TRUE)
  )
  
  
  
  
  
  
  
  
  
  ########## Weekday Email Campaign ############################
  ####Displaying ui for multiple days
  
  output$dayFilterControlForEmail <- renderUI({
    
    selectInput("daysMarketingFilterEmail", "Please Select Day(s) for Email Marketing", multiple = FALSE ,
                choices = c('Mon' = 'Monday','Tue'= 'Tuesday', 'Wed' = 'Wednesday','Thu'='Thursday','Fri'='Friday','Sun'='Sunday'),1)
    
  })
  
  
  
  # this is how we recieve input from daysMarketingFilter
  dayMarketingFilterInput <- reactive({
  
    r<-input$daysMarketingFilterEmail
    
    return(r)
    
    
    
  })
  
  #manual Email Campaign
  observeEvent(input$manualEmailCampaign, {
    
    day <- dayMarketingFilterInput()
    
    if(isTruthy(day))
    {
      selectedDays <- custData[custData$dayOfWeek %in% day,]
      # showNotification("Returning filtered", type = "message")
      
      # return(obj1)
      
    }
    else
    {
      selectedDays <- custData[custData$dayOfWeek %in% "Monday",]
      # return(custData)
      # showNotification("Returning Custdata", type = "warning")
      
    }
   

    
    removeDuplicated <- subset(selectedDays, !duplicated(Email))
    
    top20cust <- head(removeDuplicated,5)
    
    # sub <- "Promotion offer for"
    # subject1 <- paste(sub," ",selectedDays$dayOfWeek)
    body1 <- input$textEmailMarketingWeekday
    
   
    # output$daysDT1<-DT::renderDataTable(
    #   selectedDays,
    #   options = list(scrollX = TRUE, scrolly = TRUE))
    # 
    # 
    
    # showNotification(paste("Day: ", day), type = "message")
    
    
    edataDays <- top20cust %>%
      mutate(
        To = top20cust$Email,
        Bcc = optional_bcc,
        From = email_sender,
        Subject = paste("Promotion for ", day),
        body = body1) %>%
      select(To, Bcc, From, Subject, body)
    
    # converting into mime
    emailsDaily <- edataDays %>%
      pmap(mime)
# 
#     output$daysDT<-DT::renderDataTable(
#       edataDays,
#       options = list(scrollX = TRUE, scrolly = TRUE))
#     
    # return(tryCatch( safe_send_message <- safely(send_message), error = function(e) stop(safeError(e))))
    # 
    #  sent_mail <- emails %>%
    #    map(safe_send_message)
    
    # sending mail
    safe_send_message <- safely(send_message)
    sent_mail <- emailsDaily %>%
       map(safe_send_message)

  #   saveRDS(sent_mail,
  #       paste(gsub("\\s+", "_", this_hw), "sent-emails.rds", sep = "_"))
  # 
  # errors <- sent_mail %>%
  #   transpose() %>%
  #   .$error %>%
  #   map_lgl(Negate(is.null))
  #   sent_mail[errors]
# 
#     if(isTruthy(safe_send_message))
#     {
#       showNotification("Emails Sent Successfully", type = "message")
#       
#     }
#     else
#     {
#        showNotification("Emails Not Sent", type = "warning")
#       
#     }
#     
   
    
    # output$daysDT<-DT::renderDataTable(
    #   top20cust,
    #   options = list(scrollX = TRUE, scrolly = TRUE))
    # 
    
    
    
  })# END of Observe Event for manualCampaign
  
  
  observeEvent(input$maxEmailCampaign, {
  
    #SELECT DAY of MAXimum Revenue
    maxTotalTrans <- totalTransRevenueDF %>% filter(totalTransactions == max(totalTransRevenueDF$totalTransactions))
    maxTotalRev <- totalTransRevenueDF %>% filter(totalRevenue == max(totalTransRevenueDF$totalRevenue))
    
    any5cust<-head(custData, n = 5)
    
    output$top5<-DT::renderDataTable(
      any5cust,
      options = list(scrollX = TRUE))
    
    # # creating vars for email
    day <- maxTotalRev$dayOfWeek
    body <- "Dear Customer,
    Special discount offers for %s
    Thankyou! "
    
    abc<-data.frame(email=c("atifusman.isb@gmail.com"))
    
    #creating a new dataframe containing mails
    edat <- any5cust %>%
      mutate(
        To = sprintf('<%s>', any5cust$Email),
        Bcc = optional_bcc,
        From = email_sender,
        Subject = paste("Promotion Offer", input$textEmailMarketingWeekday),
        body = sprintf(body, day, input$textEmailMarketingWeekday)) %>%
      select(To, Bcc, From, Subject, body)
    
    # write_csv(edat, "my-composed-emails.csv")
    
    emails <- edat %>%
      pmap(mime)
    # #map_n(mime)
    #
    # ## optional: use if you've created your own client id
    # #use_secret_file("gmailr-tutorial.json")
    #
    # sending mail
    safe_send_message <- safely(send_message)
    sent_mail <- emails %>%
      map(safe_send_message)
    
    
    
    
    output$daytext <- renderPrint({
      print("Emails Sent Succsesfuly")
      
    })
    
    # if you want to send a message to the client side, you can try 
    print(maxTotalRev)
    
    
    })
  
  observeEvent(input$minEmailCampaign,{
    minTotalTrans <- totalTransRevenueDF %>% filter(totalTransactions == min(totalTransRevenueDF$totalTransactions))
    minTotalRev <- totalTransRevenueDF %>% filter(totalRevenue == min(totalTransRevenueDF$totalRevenue))
    
    # x<-"max day: "
    
    
    any5cust<-head(custData, n = 5)
    
    # output$top5<-DT::renderDataTable(
    #   any5cust,
    #   options = list(scrollX = TRUE))
    
    
    
    #use_secret_file("gmailr-198710.json")
    #
    # # creating vars for email
    day <- minTotalRev$dayOfWeek
    body <- "Dear Customer,
    Special discount offers for %s
    Thankyou!"
    
    
    
    
    
    abc<-data.frame(email=c("atifusman.isb@gmail.com"))
    
    #creating a new dataframe containing mails
    edat <- any5cust %>%
      mutate(
        To = sprintf('<%s>', any5cust$Email),
        Bcc = optional_bcc,
        From = email_sender,
        Subject = "Promotion Offer",
        body = sprintf(body, day)) %>%
      select(To, Bcc, From, Subject, body)
    
    # write_csv(edat, "my-composed-emails.csv")
    
    emails <- edat %>%
      pmap(mime)
    # #map_n(mime)
    #
    # ## optional: use if you've created your own client id
    # #use_secret_file("gmailr-tutorial.json")
    #
    # sending mail
    safe_send_message <- safely(send_message)
    sent_mail <- emails %>%
      map(safe_send_message)
    
    
    
    
    # output$daytext <- renderPrint({
    #   print("Emails Sent Succsesfuly")})
    # 
    # 
  
                 
 })
  
  
  
  
  
  ######Weekday SMS Campaign########
  
  output$dayFilterControlForSMS <- renderUI({
    
    selectInput("daysMarketingFilterSMS", "Please Select Day(s)  for SMS Marketing", multiple = FALSE ,
                choices = c('Mon' = 'Monday','Tue'= 'Tuesday', 'Wed' = 'Wednesday','Thu'='Thursday','Fri'='Friday','Sun'='Sunday'))
    
  })
  
  # this is how we recieve input from daysMarketingFilter
  dayMarketingFilterInputSMS <- reactive({
    #directly filtering
    
    if(is.null(custData))
    {
      
      return(custData)
      # showNotification("Still Reading Data from CSV", type = "warning")
    }
    else
    {
      
      obj1 <- custData[custData$dayOfWeek %in% input$daysMarketingFilterSMS,]
      # showNotification("Day(s) Selected", type = "message")
      
      return(obj1)
      
    }
    
    
    
  })
  
  
  observeEvent(input$manualSmsCampaign, {
    
    # If user has entered any days for filtering
    if(isTruthy(input$daysMarketingFilterSMS))
    {
      selectedDays  <- dayMarketingFilterInputSMS()
      # showNotification("Day(s) observed", type = "message")
      
      # selectedCountries <- unique(rselectedCountries)
      
    }
    else
    {
      selectedDays <- dayMarketingFilterInputSMS()
    }
    
    removeDuplicated <- subset(selectedDays, !duplicated(Email))
    
    # totalDays <- unique(selectedDays$dayOfWeek)
    
    top20cust <- head(removeDuplicated,5)
    
    inputFromuser <- input$textSmsMarketingWeekday
     
    # for(row in 1:nrow(totalDays))
    # {

      day=paste(removeDuplicated[row,"dayOfWeek"])
      
      message <- paste("Promotion offer for ", day , "<br> ", inputFromuser)
      
      for (row in 1:nrow(top20cust))
      {
        num=paste(top20cust[row,"Number"])
        
        messageFull <- paste(message," ",row)
        # POST(url,authenticate(AUTH_ID,AUTH_TOKEN),body=list(src=senderNumber,dst=num,text=message))
        
        showNotification(paste(day, " " ,row, " Manual SMS sent to: ",num), type = "message")  
        
      }      
      
    
    
    
    
  })# END of Observe Event for manualCampaign
  
  
  
  observeEvent(input$maxSmsCampaign, {
    
    maxTotalTrans <- totalTransRevenueDF %>% filter(totalTransactions == max(totalTransRevenueDF$totalTransactions))
    maxTotalRev <- totalTransRevenueDF %>% filter(totalRevenue == max(totalTransRevenueDF$totalRevenue))
    
    any5cust<-head(custData, n = 5)
    
    day <- maxTotalRev$dayOfWeek
    # numbers < any5cust$Numbers
    
    message <- paste("Promotion offer for ", day)
    
    
    for (row in 1:nrow(any5cust))
    {
      num=paste(any5cust[row,"Number"])
      POST(url,authenticate(AUTH_ID,AUTH_TOKEN),body=list(src=senderNumber,dst=num,text=message))
      
      
    }
    
    
    showNotification(paste("Max Rev. SMS sent to: ",nrow(any5cust), " Customers"), type = "message")  
    
  })
  
  
  observeEvent(input$minSmsCampaign, {
    
    minTotalTrans <- totalTransRevenueDF %>% filter(totalTransactions == min(totalTransRevenueDF$totalTransactions))
    minTotalRev <- totalTransRevenueDF %>% filter(totalRevenue == min(totalTransRevenueDF$totalRevenue))
    
    any5cust<-head(custData, n = 5)
    
    day <- minTotalRev$dayOfWeek
    # numbers < any5cust$Numbers
    
    message <- paste("Promotion offer for ", day)
    
    
    for (row in 1:nrow(any5cust))
    {
      num=paste(any5cust[row,"Number"])
      POST(url,authenticate(AUTH_ID,AUTH_TOKEN),body=list(src=senderNumber,dst=num,text=message))
    
      # showNotification(paste("Min Rev. SMS sent to: %s",num), type = "message")  
      
    }
    
    showNotification(paste("Min Rev. SMS sent to: ",nrow(any5cust), " Customers"), type = "message")  
    
    
    # POST(url,authenticate(AUTH_ID,AUTH_TOKEN),body=list(src="923335417533",dst="923345505466",text=message))
    
  })
  
  ######Weekday SEGMENTAION###############
  
  # output$dayRangeControl <- renderUI({
  #   
  #   #Getting max date
  #   maxDateRow <- custData[which.max(custData$date),]
  #   maxDate <- maxDateRow$date
  #   
  #   #Getting min date
  #   minDateRow <- custData[which.min(custData$date),]
  #   minDate <- maxDateRow$date
  #   
  #   
  #   sliderInput("sliderDaterange",
  #               "Choose Date Range:",
  #               animate = TRUE,
  #               width = "100%",
  #               min = as.Date(minDate), max = as.Date(maxDate),
  #               value = c(as.Date(minDate),as.Date(maxDate))
  #               
  #   )
  # })
  
  # dayRangeControlInput <- reactive({
  #   
  #   vectorInput <- input$sliderDaterange
  #   # 
  #   # filteredWeekdayDF <- filter(custData, custData$date >= vectorInput[1] & custData$date <= vectorInput[2])
  #   # 
  #   # 
  #   # 
  #   return (vectorInput)
  # })
  # 
  # 
  
  
  output$sidePlot<- renderPlotly({
    shinyjs::show("sliderDaterange")
    shinyjs::show("div_weekdayMarketing")
    
    
    segmenationType <- input$Segmentation
    
    vectorInput <- input$sliderDaterange
    
    # showNotification(vectorInput2,type = "Message")
    
    filteredWeekdayDF <- filter(custData, as.Date(custData$date) >= as.Date(vectorInput[1]) & as.Date(custData$date) <= as.Date(vectorInput[2]))
    # slideredDF <- dayRangeControlInput()
    
    weekdayDF2 <- filteredWeekdayDF %>%
      group_by(date, dayOfWeek) %>%
      summarise(revenue = sum(lineTotal), transactions = n_distinct(InvoiceNo)) %>%
      mutate(aveOrdVal = (round((revenue / transactions),2))) %>%
      ungroup()
      #filter(data, as.Date(date) >= as.Date("2016-07-18") & as.Date(data$date) <= as.Date("2016-07-18"))
    
    revenueDayTitle <- paste("Daily Revenue between ", vectorInput[1], " and ", vectorInput[2])
    transactionsDayTitle <- paste("Daily Transactions between ", vectorInput[1], " and ", vectorInput[2])
    
    
    
    print(
        
        if (segmenationType == "Daily") {
          
          segTypeDaily <- input$segTypeDaily
          
          if (segTypeDaily == "Per Day Revenue") {
            
            ggplotly(weekdayRevenueGraph <- weekdayDF2 %>%
                       group_by(dayOfWeek) %>%
                       summarise(revenue=sum(revenue)) %>%
                       ggplot(aes(x=dayOfWeek,y=revenue, fill=dayOfWeek, text= paste(" Day :",dayOfWeek, "<br>", "Revenue (£):",
                                                                                     revenue)))+geom_col()+labs(x='Day of Week', y='Revenue (£)', title=revenueDayTitle)+theme(axis.text.y = element_text(face="bold", color="#000000",
                                                                                                                                                                                                                   size=10, angle=45)),tooltip = c("text"),width = plotWidth, height = plotHeight )
            #x=reorder(dayOfWeek,-revenue)
            
          #   ggplotly(weekdayRevenueGraph <- weekdayDF %>%
          #              group_by(dayOfWeek) %>%
          #              summarise(revenue=sum(revenue)) %>%
          #              ggplot(aes(x=reorder(dayOfWeek,-revenue),y=revenue, fill=dayOfWeek, text= paste(" Day :",dayOfWeek, "<br>", "Revenue (£):", 
          #                                                                                              revenue)))+geom_col()+labs(x='Day of Week', y='Revenue (£)', title='Revenue by Day of Week')+theme(axis.text.y = element_text(face="bold", color="#000000", 
          #                                                                                                                                                                                                                            size=10, angle=45)),tooltip = c("text"),width = plotWidth, height = plotHeight )
          # 
          }
          else if (segTypeDaily == "Per Day Transactions")
          {
            ggplotly(weekdayTransactions <- weekdayDF2 %>%
              group_by(dayOfWeek) %>%
              summarise(trans= sum(transactions)) %>%
              ggplot(aes(x=reorder(dayOfWeek,-trans), y = trans, fill=dayOfWeek, text= paste(" Day: ", dayOfWeek, "<br>", "Transactions: ", trans ))) + geom_col(position = "stack") +
                                                                                        labs(x = 'Date',
                                                                                          y = 'Transactions',
                                                                                          title = transactionsDayTitle), tooltip = c("text"),width = plotWidth, height = plotHeight)
          }
          
          
        }
        else if (segmenationType == "Hourly")
        {
          segTypeHourly <- input$segTypeHourly
          
          if (segTypeHourly == "Hourly Revenue")
          {
            ggplotly(hourlyRevenueGraph <- custData %>%
              group_by(hourOfDay) %>%
              summarise(revenue = sum(lineTotal)) %>%
              ggplot(aes(x=reorder(hourOfDay,-revenue), y = revenue, fill=hourOfDay, text= paste(" Hour of Day: ", hourOfDay,"<br>", "Revenue (£): ", revenue))) + geom_col() + labs(x = 'Hour Of Day',
                                                                                          y = 'Revenue (£)',
                                                                                          title = 'Revenue by Hour Of Day'), tooltip = c("text"),width = plotWidth, height = plotHeight)
            
            
          }
          else if (segTypeHourly == "Hourly Transaction")
          {
            ggplotly(hourlyTransactionGRaph <- custData %>%
              group_by(hourOfDay) %>%
              summarise(transactions = n_distinct(InvoiceNo)) %>%
              ggplot(aes(x=reorder(hourOfDay,-transactions), y = transactions, fill=hourOfDay, text = paste(" Hour of Day: ", hourOfDay, "<br>", "Transactions: ", transactions))) + geom_col() + labs(x = 'Hour Of Day',
                                                                                               y = 'Number of Transactions',
                                                                                               title = 'Transactions by Hour Of Day'), tooltip = c("text"),width = plotWidth, height = plotHeight)
            
            
          }
        }
          
         
    )
    
  })
  
  ###Automatic trigger###
  # minTotalRev11 <- totalTransRevenueDF %>% filter(totalRevenue == min(totalTransRevenueDF$totalRevenue))
  
  minTriggerRevenue <- totalTransRevenueDF %>% filter(totalRevenue == min(totalTransRevenueDF$totalRevenue))
  
  if(minTriggerRevenue$totalRevenue < 50000)
  {
    Sys.sleep(30)
    any5cust<-head(custData, n = 5)
    
    # # creating vars for email
    day <- minTriggerRevenue$dayOfWeek
    body <- "Dear Customer,
    Special discount offers for %s
    Thankyou!"
    
    
    #creating a new dataframe containing mails
    edat <- any5cust %>%
      mutate(
        To = sprintf('<%s>', any5cust$Email),
        Bcc = optional_bcc,
        From = email_sender,
        Subject = "Promotion Offer Automatic",
        body = sprintf(body, day)) %>%
      select(To, Bcc, From, Subject, body)
    
    # write_csv(edat, "my-composed-emails.csv")
    
    emails <- edat %>%
      pmap(mime)
    # sending mail
    safe_send_message <- safely(send_message)
    sent_mail <- emails %>%
      map(safe_send_message)
    notif <- paste("Campaign launched automatically for sale on ", minTriggerRevenue$dayOfWeek, "as Revenue is below $50,000")
    
    
    showNotification(notif, type = "message", closeButton = TRUE, duration = 30)
    
  }
  
  #### CUSTOMER SEGMENTATION ####
  
  
  output$transactionsPerCustomer <- renderPlotly({print(ggplotly(ggplot(custSummary, aes(transactions, fill = transactions)) + geom_histogram() + scale_x_log10() + labs(x = 'Number of Transactions', y = 'Count of Customers', title = 'Transactions per customer')),width = plotWidth, height = plotHeight)})
  
  output$sidePlot2 <-  renderPlotly({
    print(ggplotly(
      custData %>%
        group_by(date) %>%
        summarise(revenue = sum(lineTotal)) %>%
        ggplot(aes(x = date, y = revenue, label = (paste('Date :', as.Date(date, "%m/%d/%Y"))))) + geom_line(group=1) + geom_smooth(method = 'auto', se = FALSE) + labs(x = 'Date', y = 'Revenue (£)', title = 'Overall Revenue Trend by Date') +
        theme(axis.text.y = element_text(face="bold", color="#000000", 
                                         size=10, angle=45))
      , tooltip = c("label","revenue") , width = plotWidth, height = plotHeight )
    )
    })
  
  
  
  output$datatable <- renderTable({
    
    #filtering
    if(input$productDesc != "")
    {
      filteredData <- custData %>% filter(input$productDesc)
      
    }
    else
    {
      filteredData <- custData
      
    }
    
    
    head(filteredData, n = 5)
  })
  
  
  
  
  
  
  
  output$revenuePerCustomer<- renderPlotly({
    shinyjs::show("div_personalizedCampaign")
    shinyjs::show("div_customerMarketing")
    
    
    segmenationType <- input$Segmentation
    
    top20customers <- head(custSummaryDF,input$numOfCustomers)
    
    myTitle <- paste("Showing Total Revenue of Top ", input$numOfCustomers , " Customers")
    
    top20customers$CustomerID <- as.character(top20customers$CustomerID)

    
    #Event Data
    output$revenuePerCustomerClickInfo <- renderPrint({
      paste("Revenue Per Customer Click: ","<br>")
      # str(input$plot_click)
      
      d <- event_data("plotly_click",source = "revenuePerCustomerEvent")
      if (is.null(d)) 
      {"Click on a state to view event data"
      }
      else
      {
        #Getting customer ID from click event
        customerIDdf <- top20customers[top20customers$revenue == d$y,]
        
        #Getting customer info of clicked customer
        myDF <-  customerBreakdown[ customerBreakdown$CustomerID == customerIDdf$CustomerID,]
        # 
        #Generating a new DF to be used to plot a new graph to show customer products
        # custProdDF <- myDF %>%
        #   group_by(Description, dayOfWeek) %>%
        #   summarise(revenue = sum(lineTotal), transactions = n_distinct(InvoiceNo)) %>%
        #   mutate(aveOrdVal = (round((revenue / transactions),2))) %>%
        #   ungroup() %>%
        #   arrange(desc(revenue))
        
        # Displaying newly generated DF
        output$top20customersDT <- DT::renderDataTable(
          myDF,
          options = list(scrollX = TRUE)
        )
        
        # output$customerClickGraph <- renderPrint({
        # 
        #   print(
        #     ggplotly(
        #       
        #       ggplot(custProdDF, aes(dayOfWeek, revenue)) + geom_bar(aes(fill = Description), position = "dodge", stat="identity")
        #     ),width = plotWidth, height = plotHeight
        #   )
        #   
        #   })
        # f1(custProdDF)
        
        cat(paste(" CustomerID: ", myDF$CustomerID, "\n Favourite Day:", myDF$mostDay, "\n Favourite Product: ", myDF$mostProd))
      }
      
      
      
    })
    
    print(
      ggplotly(
        
        ggplot(top20customers, aes(x=reorder(CustomerID,-revenue), y = revenue, fill=CustomerID)) + geom_col(group=1) + labs(x = 'Customers',
                                                                                                      y = 'Revenue',
                                                                                                      title = myTitle)  +theme(axis.text.x = element_text(face="bold", color="#000000", 
                                                                                                                                                                                size=6, angle=45)) + scale_y_continuous(labels = scales::comma),
        source = "revenuePerCustomerEvent"
        
        
      ),width = plotWidth, height = plotHeight, tooltip = NULL)
    
    
    
  })
  
  
  f1 <- function(x) {
    output$customerClickGraph <- renderPrint({
      
      print(
        ggplotly(
          
          ggplot(x, aes(dayOfWeek, revenue)) + geom_bar(aes(fill = Description), position = "dodge", stat="identity")
        )
      )
      
    })
  }
  
  # jajajaja <- reactive({
  #   
  # })
  
  
  
  # , text=paste("Customer: ", CustomerID, "<br>", " Revenue: ", revenue)
  
   

   
  
     
     
     
  #to show click output of rfm graph
  # output$rfmGraphPlotClickInfo <- renderPrint({
  #   cat("RFM Graph Click: <br>")
  #   #str(input$plot_click)
  #   
  #    e <- event_data("plotly_click")
  #   if (is.null(e)) "Click on a state to view event data" else e
  # })
  
  ##########Customer Email Personalized Campaign############
  
  observeEvent(input$cusPersonalizedCampaignEmail, {
    
    selectedCustomers <- head(customerBreakdown11,input$numOfCustomers)
    
    day <- selectedCustomers$mostDay
    custid<-selectedCustomers$CustomerID
    prod<-selectedCustomers$mostProd
    body <- "Dear Customer %i,
    Special discount offers for %s on %s
    Thankyou!"
    
    
    
    
    #creating a new dataframe containing mails
    edat1 <- selectedCustomers %>%
      mutate(
        To = sprintf('<%s>',selectedCustomers$Email),
        Bcc = optional_bcc,
        From = email_sender,
        Subject = "Customer Promotion Offer",
        body = sprintf(body,custid, day, prod)) %>%
      select(To, Bcc, From, Subject, body)
    
    emails1 <- edat1 %>%
      pmap(mime)
    safe_send_message <- safely(send_message)
    sent_mail <- emails1 %>%
      map(safe_send_message)
    
    output$customertext<-DT::renderDataTable(
      edat1,
      options = list(scrollX = TRUE))
    
    showNotification(paste("Personalized Emails sent to Top ", input$sliderNumOfProducts), type ="message")
    
    # POST(url,authenticate(AUTH_ID,AUTH_TOKEN),body=list(src="923335417533",dst="923345505466",text=message))
    
  })

  ##########Customer Sms Campaign Personalize######################################
  
  observeEvent(input$cusPersonalizedCampaignSms, {
    
    selectedCustomers2 <- head(customerBreakdown11,input$numOfCustomers)
    
    
    day <- selectedCustomers2$mostDay
    custid<-selectedCustomers2$CustomerID
    prod<-selectedCustomers2$mostProd
    body <- "Dear Customer %i,
    Special discount offers for %s on %s
    Thankyou!"
    
    message <- sprintf(body, custid, prod, day)
    
   
    
    for (row in 1:nrow(selectedCustomers2))
    {
      num=paste(selectedCustomers2[row,"Number"])
      r <- POST(url,authenticate(AUTH_ID,AUTH_TOKEN),body=list(src=senderNumber,dst=num,text=message))
      
      output$smsResponse <- renderPrint({
      
      http_status(r)
    
      })
    
      
    }
    
    showNotification(paste(nrow(selectedCustomers2), " Personalized SMS sent to: ",num), type = "message")  
    
    # POST(url,authenticate(AUTH_ID,AUTH_TOKEN),body=list(src="923335417533",dst="923345505466",text=message))
    
  })
  #### Country SEGMENTATION ####
  #helpOut <- input$camCountryMessage
  
  
  
  output$countryPlotly <- renderPlotly({
    shinyjs::show("div_countryCampaign")
    
    
    countrySegmenationType <- input$CountrySegmentation
    
    #+ geom_smooth(method = 'auto', se = FALSE)
    
    p <- ggplot(topFiveCountrySummaryDF, aes(x = date , y = revenue, color= Country, group = 1, text = paste('Revenue ($):', revenue,
                                                                                                  '<br>Date: ', as.Date(date),
                                                                                                  '<br>Location: ', Country))) + geom_line() + labs(x = 'Location', y = 'Revenue (£)', color= 'Location', title = 'Revenue by Location over Time')
    
    
    pp <- ggplotly(p, width = plotWidth, height = plotHeight, tooltip = c("text"))
    # + geom_smooth(method = 'auto', se = FALSE) 
    
    # # p <- add_annotations(p,x=14950.25,y = 37907.0863030, text = "Max Rev")
    # library(grid)
    # my_text <- "This text is at x=0.7 and y=0.8!"
    # my_grob = grid.text(my_text, x=topFiveCountrySummaryDF$date[which.max(topFiveCountrySummaryDF$date)],  y=topFiveCountrySummaryDF$revenue[which.max(topFiveCountrySummaryDF$revenue)], gp=gpar(col="firebrick", fontsize=14, fontface="bold"))
    # p + annotation_custom(my_grob)
    # 
    
    # gg <- plot_ly(topFiveCountrySummaryDF, x = ~date, y = ~revenue) %>%
    #   slice(which.max(revenue)) %>%
    #   add_annotations(text = "Good mileage")
    # 
    # #y=topFiveCountrySummaryDF$revenue[which.max(topFiveCountrySummaryDF$revenue)]
    print(pp)
  })
 
  
  #### Country Specific Campaign Button ####
  output$countryControls <- renderUI({
    selectInput("CountryMarketingFilter", "Please Select Location to Target Marketing", multiple = FALSE ,
                   choices = unique(custData$Country),1)
    
  })
  
  output$productControls <- renderUI({
    
    marketingCountryDF <- marketingCountry2()
    
    # if(is.null(marketingCountryDF))
    # {
    # 
    # }
    # else
    # {
    #   #obj <- marketingCountryDF %>% filter(Description == input$ProductMarketingFilter)
    #   
    #   marketingCountryDF <- marketingCountryDF[marketingCountryDF$Description %in% input$ProductMarketingFilter,]
    #   
    #   showNotification(sprintf("Product(s) Filtered using selected Countries: %s",nrow(obj)), type = "message")
    #   
    #   return(obj) 
    # }
    # 
    
    selectInput("ProductMarketingFilter", "Please Select Product", multiple = FALSE ,
                   choices = unique(marketingCountryDF$Description))
    
  })
  
  output$selCountry <- renderUI({
    selectInput("CountrySelectionFilter", "Please Select Location ", multiple = FALSE ,
                choices = unique(custData$Country), selected = 'Karachi')
  })
  
  
  countryTranData <- reactive({ 
    
    if(isTruthy(input$CountrySelectionFilter))
    {
      obj <- custData %>% filter(Country == input$CountrySelectionFilter)
      return(obj)
      
      # showNotification("Data Loaded", type = "message")
      
    }
    
    
  })
  
 
  
  
  
  
  
  output$mymap <- renderLeaflet({
    countryToCities <- countrySummaryDF
    
    for (row in 1:nrow(countryToCities))
    {
      countryToCities[row, "City"] = as.character(cities[row, "city"])
      countryToCities[row, "Longitude"] = as.double(cities[row, "long"])
      countryToCities[row, "Latitude"] = as.double(cities[row, "lat"])
    }
    
    leaflet(countryToCities) %>% addTiles() %>%
      addCircles(lng = ~Longitude, lat = ~Latitude, weight = 1,
                 radius = ~sqrt(as.integer(countryToCities$revenue)*100) * 30, popup = paste("City: ",
                                                                                              countryToCities$City,
                                                                                              "<br/>",
                                                                                              "Revenue: ",
                                                                                              countryToCities$revenue)
      )
  })
  
  
  
  output$countryTransPlotly <- renderPlotly({
    
    
    if(isTruthy(countryTranData()))
    {
      country <- countryTranData()$Country
      message <- 'Transaction of '
      thisCountry <- paste('Showing Top ', input$sliderNumOfProducts , ' Products in ', country)
      
      df <- custData %>% filter(Country == countryTranData()$Country)
      df1 <- head(df,input$sliderNumOfProducts)
      c_t <- df1 %>% group_by(Description) %>% summarise(sumOfQuatity = sum(Quantity))
      
      print(
        ggplotly(
          ggplot(c_t, aes(x=reorder(Description,-sumOfQuatity), y = sumOfQuatity, fill=Description)) + geom_bar(stat= "identity",width = .5)  + labs(x = "Product Name", y = "Quantity Sold",title = thisCountry) + theme(axis.text.x = element_blank(),axis.text.y = element_text(face="bold", color="#000000", 
                                                                                                                                                                                                                                                             size=8, angle=45))
          
         , width = plotWidth, height = plotHeight 
        ))  
      
      
    }else
    {
      #else show data of france
      country <- 'Karachi'
      message <- 'Transaction of '
      thisCountry <- paste(message, country)
      
      df <- custData %>% filter(Country == "Karachi")
      df1 <- head(df,20)
      c_t <- df1 %>% group_by(Description) %>% summarise(sumOfQuantity = sum(Quantity))
      
      print(
        ggplotly(
          ggplot(c_t, aes(x = reorder(Description, -sumOfQuantity), y = sumOfQuantity, fill=Description)) + geom_col() + labs(x = 'Product', y = 'Transactions', title = thisCountry)
          
          
        ))  
      
      
      
    }
  
  })
  # # Get input of Countries
  # marketingCountry2 <- reactive({
  #   #directly filtering
  #   custData %>% filter(Country == input$CountryMarketingFilter)
  #   
  # })
  # 
  # # Get input of Product
  # marketingProductPerCountry <- reactive({
  #   #directly filtering
  #   custData %>% filter(Description == input$ProductMarketingFilter)
  #   
  # })
  # 
  
  # Get input of Countries
  marketingCountry2 <- reactive({
    #directly filtering
    
    if(is.null(custData))
    {
      
      return(custData)
     
    }
    else
    {
      #obj1 <- custData %>% filter(Country == input$CountryMarketingFilter)
      
      obj1 <- customerPerCountryDF4 <- custData[custData$Country %in% input$CountryMarketingFilter,]
      # showNotification("Countries(s) Selected", type = "message")
      
      return(obj1)
      
    }

  })
  
  #marketingCountryDF <- marketingCountry2()
  
  
  # Get input of Product
  marketingProductPerCountry <- reactive({
    #directly filtering
    
    marketingCountryDF <- marketingCountry2()
    
    if(is.null(marketingCountryDF))
    {
      #obj <- custData %>% filter(Description == input$ProductMarketingFilter)
      obj <- custData <- custData[custData$Description %in% input$ProductMarketingFilter,]
      
      # showNotification("Product(s) Filtered without specific countries", type = "warning")
      
      
      return(obj)
    }
    else
    {
      #obj <- marketingCountryDF %>% filter(Description == input$ProductMarketingFilter)
      
      obj <- marketingCountryDF <- marketingCountryDF[marketingCountryDF$Description %in% input$ProductMarketingFilter,]
      
      
      
      return(obj) 
    }
    
    
    
  })
  
  
 
  ######################Country Emailing########################
  observeEvent(input$manualCountryCampaignEmail, {
   
    if(isTruthy(input$CountryMarketingFilter))
    {
      selectedCountries  <- marketingCountry2()
    
    }
    
    if(isTruthy(input$ProductMarketingFilter))
    {
      selectedCountryWithProducts <- marketingProductPerCountry()
        
    }
   
    
    #Showing data of selected countries and products
    output$countryText <- renderPrint({
      
      print(selectedCountryWithProducts)
      
    
    })
    
    
  
  
     marketingCountryEmails <- unique(selectedCountryWithProducts$Email)
    
     uTop <- subset(selectedCountryWithProducts, !duplicated(Email))
     
     
     top10 <- head(uTop,10)
     
     
     
     autoBody <- paste("Promotion offer on product: %s"," ",input$camCountryMessage)
     
     
     #creating a new dataframe containing mails
     edat <- top10 %>%
       mutate(
         To = sprintf('<%s>', top10$Email),
         Bcc = optional_bcc,
         From = email_sender,
         Subject = sprintf("Promotion Offer %s",top10$Country),
         body = sprintf(autoBody, top10$Description)) %>%
       select(To, Bcc, From, Subject, body)
     
     
     emails <- edat %>%
       pmap(mime)
     
     # sending mail
     safe_send_message <- safely(send_message)
     sent_mail <- emails %>%
       map(safe_send_message)
     
     showNotification(sprintf("Email sent to customers: %s of %s",nrow(top10), top10$Country), type = "message")
     # 
     output$sentMail <- DT::renderDataTable(
       edat,
       options = list(scrollX = TRUE)
     )
     
 
   })
  
  ######################Country SMS ########################
  observeEvent(input$manualCountryCampaignSms, {
   
    #Getting df of selected countries
    if(isTruthy(input$CountryMarketingFilter))
    {
      selectedCountries  <- marketingCountry2()
    }
    
    #Getting df of selected products, filtered by countries
    if(isTruthy(input$ProductMarketingFilter))
    {
      selectedCountryWithProducts <- marketingProductPerCountry()
    }
    
    
    #Showing data of selected countries and products
    # output$countryText <- renderPrint({
    #   
    #   print(selectedCountryWithProducts)
    #   
    # 
    # })
    # 
    
    top10custCountry <- head(selectedCountryWithProducts,10)
    messageText <- paste("Promotion Offer Location: ",top10custCountry$Country, " and product: ", top10custCountry$Description)
    
    
     for (row in 1:nrow(top10custCountry))
    {
      num=paste(top10custCountry[row,"Number"])
      r <- POST(url,authenticate(AUTH_ID,AUTH_TOKEN),body=list(src=senderNumber,dst=num,text=messageText))
      
      output$countryText <- renderPrint({
      
      http_status(r)
    
      })
    
      showNotification(paste(row, " Max Rev. SMS sent to: ",num), type = "message")  
      
    }
    
    
  
     # marketingCountryDF <- marketingCountry2()
     # marketingCountryEmails <- selectedCountryWithProducts %>%
     #                            unique(selectedCountryWithProducts$Email)
     # 
     
     # marketingCountryEmails <- unique(selectedCountryWithProducts$Email)
     
     #emailCount <- count(marketingCountryEmails)
     
     # output$emailText <- renderPrint({
     # 
     #   print(top10custCountry)
     # 
     # 
     # })
     
     #uTop <- unique(selectedCountryWithProducts)
     # uTop <- subset(selectedCountryWithProducts, !duplicated(Email))
     # 
     # 
     # top10 <- head(uTop,10)

 
   })
  
 ########cOUNTRY GRAPH####
  
  output$transactionPlotly <- renderPlotly({
    # 
    # p <- plot_ly(topFiveCountrySummaryDF,
    #   x = Country,
    #   y = transactions,
    #   name = "SF Zoo",
    #   type = "bar"
    # )
  
    # print(
    #   ggplotly(
    #    ggplot(topFiveCountrySummaryDF, aes(x = reorder(Country, -transactions), y = transactions, fill=Country, width = 0.4,  text = paste('Transactions: ',transactions , '<br>'))) + geom_col(group=1) + labs(x = 'Location', y = 'Transactions', title = 'Transactions by Location'),
    #    width=800, height=400, tooltip = c("text","Country")
    #     
    #     
    #   ))
    
    
    print(ggplotly(
      ggplot(topFiveCountrySummaryDF, aes(
        x=reorder(Country,-transactions), y = transactions, fill = Country
      )) + geom_col() + labs(x = 'Location', y = 'Transactions', title = "Transactions per Location")  +
        theme(
          axis.text.y = element_text(
            face = "bold",
            color = "#000000",
            size =
              8,
            angle = 45
          )
        ),
      width = plotWidth,
      height = plotHeight
      
    ))
    
  })

  
  #############RFM#####################
  
  #calculating recency for each transaction as per year 2012
  date2012 <- as.Date("01/01/2012", "%m/%d/%Y")
  
  custSummaryB <- custData %>%
    group_by(InvoiceNo,
             CustomerID,
             Email,
             Country,
             date,
             month,
             year,
             hourOfDay,
             dayOfWeek,
             Number) %>%
    summarise(orderVal = sum(lineTotal)) %>%
    mutate(recent = date2012 - date) %>%
    ungroup()
  
  #filtering
  custSummaryB <- custSummaryB %>%
    filter(orderVal > 50)
  
  custSummaryB$recent <- as.character(custSummaryB$recent)
  custSummaryB$recentDays <-
    sapply(
      custSummaryB$recent,
      FUN = function(x) {
        strsplit(x, split = '[ ]')[[1]][1]
      }
    )
  custSummaryB$recentDays <- as.integer(custSummaryB$recentDays)
  
  
  # totaltrans <- n_distinct(custSummaryB$InvoiceNo)
  totaldays <-
    as.double(max(custSummaryB$date) - min(custSummaryB$date))
  
  
  
  #calculating total days in data to find frequency for each customer
  totaldays <-
    as.double(max(custSummaryB$date) - min(custSummaryB$date))
  
  #meanRevenue = round(mean(orderVal),2),medianRevenue = median(orderVal),
  
  customerBreakdownClass <- custSummaryB %>%
    group_by(CustomerID, Country, Email, Number) %>%
    summarise(
      orders = n_distinct(InvoiceNo),
      revenue = sum(orderVal),
      mostDay = names(which.max(table(dayOfWeek))),
      mostHour = names(which.max(table(hourOfDay))),
      recency = min(recentDays),
      freq = orders / totaldays
    ) %>%
    ungroup()
  
  
  customerRecency <-
    customerBreakdownClass %>% select(customer = CustomerID, recency = recency) %>% arrange(desc(-recency))
  customerFreq <-
    customerBreakdownClass %>% select(customer = CustomerID, freq = freq) %>% arrange(desc(freq))
  customerMonetary <-
    customerBreakdownClass %>% select(customer = CustomerID, monetary = revenue) %>% arrange(desc(monetary))
  
  recencymin <- as.double(min(customerRecency$recency))
  recencymax <- as.double(max(customerRecency$recency))
  recencyavg <-
    as.double((min(customerRecency$recency) + max(customerRecency$recency)) /
                2)
  recencyminavg <-
    as.double((min(customerRecency$recency) + recencyavg) / 2)
  recencymaxavg <-
    as.double((max(customerRecency$recency) + recencyavg) / 2)
  
  freqmin <- as.double(min(customerFreq$freq))
  freqmax <- as.double(max(customerFreq$freq))
  freqavg <-
    as.double((min(customerFreq$freq) + max(customerFreq$freq)) / 2)
  freqminavg <- as.double((min(customerFreq$freq) + freqavg) / 2)
  freqmaxavg <- as.double((max(customerFreq$freq) + freqavg) / 2)
  
  monetarymin <- as.double(min(customerMonetary$monetary))
  monetarymax <- as.double(max(customerMonetary$monetary))
  monetaryavg <-
    as.double((
      min(customerMonetary$monetary) + max(customerMonetary$monetary)
    ) / 2)
  monetaryminavg <-
    as.double((min(customerMonetary$monetary) + monetaryavg) / 2)
  monetarymaxavg <-
    as.double((max(customerMonetary$monetary) + monetaryavg) / 2)
  
  
  recencyrange <-
    data.frame(recencymin,
               recencyminavg,
               recencyavg,
               recencymaxavg,
               recencymax)
  monetaryrange <-
    data.frame(monetarymin,
               monetaryminavg,
               monetaryavg,
               monetarymaxavg,
               monetarymax)
  freqrange <-
    data.frame(freqmin, freqminavg, freqavg, freqmaxavg, freqmax)
  
  
  for (row in 1:nrow(customerRecency))
  {
    if (customerRecency[row, "recency"] >= recencymin &
        customerRecency[row, "recency"] < recencyminavg)
    {
      customerRecency[row, "rank"] = 4
    }
    else if (customerRecency[row, "recency"] >= recencyminavg &
             customerRecency[row, "recency"] < recencyavg)
    {
      customerRecency[row, "rank"] = 3
    }
    else if (customerRecency[row, "recency"] >= recencyavg &
             customerRecency[row, "recency"] < recencymaxavg)
    {
      customerRecency[row, "rank"] = 2
    }
    else if (customerRecency[row, "recency"] >= recencymaxavg &
             customerRecency[row, "recency"] <= recencymax)
    {
      customerRecency[row, "rank"] = 1
    }
  }
  
  for (row in 1:nrow(customerFreq))
  {
    if (customerFreq[row, "freq"] >= freqmin &
        customerFreq[row, "freq"] < freqminavg)
    {
      customerFreq[row, "rank"] = 1
    }
    else if (customerFreq[row, "freq"] >= freqminavg &
             customerFreq[row, "freq"] < freqavg)
    {
      customerFreq[row, "rank"] = 2
    }
    else if (customerFreq[row, "freq"] >= freqavg &
             customerFreq[row, "freq"] < freqmaxavg)
    {
      customerFreq[row, "rank"] = 3
    }
    else if (customerFreq[row, "freq"] >= freqmaxavg &
             customerFreq[row, "freq"] <= freqmax)
    {
      customerFreq[row, "rank"] = 4
    }
  }
  
  for (row in 1:nrow(customerMonetary))
  {
    if (customerMonetary[row, "monetary"] >= monetarymin &
        customerMonetary[row, "monetary"] < monetaryminavg)
    {
      customerMonetary[row, "rank"] = 1
    }
    else if (customerMonetary[row, "monetary"] >= monetaryminavg &
             customerMonetary[row, "monetary"] < monetaryavg)
    {
      customerMonetary[row, "rank"] = 2
    }
    else if (customerMonetary[row, "monetary"] >= monetaryavg &
             customerMonetary[row, "monetary"] < monetarymaxavg)
    {
      customerMonetary[row, "rank"] = 3
    }
    else if (customerMonetary[row, "monetary"] >= monetarymaxavg &
             customerMonetary[row, "monetary"] <= monetarymax)
    {
      customerMonetary[row, "rank"] = 4
    }
  }
  
  
  customerRecency <- customerRecency %>% arrange((customer))
  customerFreq <- customerFreq %>% arrange((customer))
  customerMonetary <- customerMonetary %>% arrange((customer))
  
  customerBreakdownClass <-
    customerBreakdownClass %>% arrange(CustomerID)
  
  for (row in 1:nrow(customerBreakdownClass))
  {
    customerBreakdownClass[row, "RFM"] = (((customerRecency[row, "rank"] * 0.25) + (customerFreq[row, "rank"] * 0.25) + (customerMonetary[row, "rank"] * 0.5)
    ) / 4) * 100
    
    if (customerBreakdownClass[row, "RFM"] >= 50)
    {
      customerBreakdownClass[row, "class"] = "Platinum"
      
    }
    else if (customerBreakdownClass[row, "RFM"] > 43)
    {
      customerBreakdownClass[row, "class"] = "Gold"
    }
    else
    {
      customerBreakdownClass[row, "class"] = "Silver"
    }
    
  }
  
  # output$breakdownTable <- DT::renderDataTable(dt <- customerRFMchoiceInput(),
  #                   options = list(scrollX = TRUE))

  
  ###############Customer Insights Graph & RFM######################################
  
  # display select input for classification graphs
  output$customerRFMchoice <- renderUI({
    
    selectInput("customerInsights", "Please Select Classification",
                choices=c("Total Classification Graph","Gold","Silver","Platinum"),1)
  })
  
  
  
  # this is how we recieve input from customerInsights
  customerRFMchoiceInput <- reactive({
    #directly filtering
    r<-input$customerInsights
    
    return(r)
    
    
  })
  
  ### Display Graph
  output$rfmGraphPlot <- renderPlotly({
    selected <- customerRFMchoiceInput()
    
    
    
   #if nothing selected or basic graph.. then show basic graph
    if(isTruthy(selected))
    {
      if(selected == "Total Classification Graph")
      {
        rfmGraph <- customerBreakdownClass %>%
          group_by(class) %>%
          summarise(customers = n_distinct(CustomerID)) %>%
          ggplot(aes(
            x = reorder(class, -customers), y = customers, fill = class
          )) + geom_bar(stat= "identity",width = .3) + labs(x = 'Customer Class', y = 'No. of Customers', title =
                                                              'RFM of Customer')
        
      
      }
      else
      {
        
        # obj1 <- custData[custData$dayOfWeek %in% input$daysMarketingFilter,]
        customerBreakdownSelected <- customerBreakdownClass[customerBreakdownClass$class %in% selected,]
        
        customerBreakdownFiltered <- customerBreakdownSelected %>%
                              group_by(Country) %>%
                              summarise(customers = n_distinct(CustomerID)) %>%
                              filter(customers > 5)
        
       
        rfmGraph <- ggplot(customerBreakdownFiltered, aes(
          x= reorder(Country, -customers), y = customers, fill = Country
          )) + geom_bar(stat= "identity",width = .3) + labs(x = 'Country', y = 'No. of Customers', title =
                                                              'Customers Per Location')
        
      }
    }
    else
    {
      rfmGraph <- customerBreakdownClass %>%
        group_by(class) %>%
        summarise(customers = n_distinct(CustomerID)) %>%
        ggplot(aes(
          x= reorder(class, -customers), y = customers, fill = class
        )) + geom_bar(stat= "identity",width = .3) + labs(x = 'Customer Class', y = 'No. of Customers', title =
                                                            'RFM of Customer')
      
    }
   
    
    print(
      ggplotly(
        
        if(isTruthy(rfmGraph))
        {
          rfmGraph
          
        }
        , width = plotWidth, height = plotHeight
      )
    )
    
  })
  
  
  ########Customer Insights SMS Campaign#############
  observeEvent(input$manualSmsCampaignCustomer, {
    
    # If user has entered any days for filtering
    if(isTruthy(input$textSmsMarketing))
    {
      textSmsMarketingInput <- input$textSmsMarketing
    }
    
    
    selected <- customerRFMchoiceInput()
    
    # showNotification(paste(selected," is selected"), type = "message")
    
    
    #if nothing selected or basic graph.. then show basic graph
    if(isTruthy(selected))
    {
      if(selected == "Total Classification Graph")
      {
        
        dfWithNumbers <- customerBreakdownClass
        
        #collect Numbers of customers of all classes.. Gold, Silver, Platium
        # showNotification("Showing RFM Graph", type = "message")
        
      }
      else
      {
        #collect numbers of only selected customers
       dfWithNumbers <- customerBreakdownClass[customerBreakdownClass$class %in% selected,]
       
        
      }
    }
    else
    {
      dfWithNumbers <- customerBreakdownClass
      
      # showNotification("Showing RFM Graph", type = "message")
    }
    
    top10cust <- head(dfWithNumbers,10)
    
    output$sentNumbers <- DT::renderDataTable(
      top10cust,
      options = list(scrollX = TRUE)
    )
    
    
    for (row in 1:nrow(top10cust))
    {
        num=paste(top10cust[row,"Number"])
        
        messageFull <- paste(textSmsMarketingInput," ",row)
        r <- POST(url,authenticate(AUTH_ID,AUTH_TOKEN),body=list(src=senderNumber,dst=num,text=messageFull))
        
        output$smsResponse <- renderPrint({
          
          http_status(r)
          
        })
        showNotification(paste("Manual SMS sent to: %i",num), type = "message")  
        
    }      
      
    
    
    
    
    
  })
  
  ############Customer Insights Email Campaign################
  observeEvent(input$manualEmailCampaignCustomer, {
    
    # If user has entered any days for filtering
    if(isTruthy(input$textEmailMarketing))
    {
      textEmailMarketingInput <- input$textEmailMarketing
    }
    
    
    selected <- customerRFMchoiceInput()
  
    #if nothing selected or basic graph.. then show basic graph
    if(isTruthy(selected))
    {
      if(selected == "Total Classification Graph")
      {
        
        dfWithNumbers <- customerBreakdownClass
      
      }
      else
      {
       dfWithNumbers <- customerBreakdownClass[customerBreakdownClass$class %in% selected,]
       
      }
    }
    else
    {
      dfWithNumbers <- customerBreakdownClass
      
      # showNotification("Showing RFM Graph", type = "message")
    }
    
    removeDuplicated <- subset(dfWithNumbers, !duplicated(Email))
    
    top10cust <- head(removeDuplicated,10)
    
    
    
          
    
    
    sub <- "Promotion offer for"
    
    
    edataCustInsights <- top10cust %>%
      mutate(
        To = sprintf('<%s>', top10cust$Email),
        Bcc = optional_bcc,
        From = email_sender,
        Subject = paste(sub," ", selected, " Customers"),
        body = textEmailMarketingInput) %>%
      select(To, Bcc, From, Subject, body)
    
    # converting into mime
    emailsCustInsights <- edataCustInsights %>%
      pmap(mime)
    
    
    # sending mail
     safe_send_message <- safely(send_message)
     sent_mail <- emailsCustInsights %>%
       map(safe_send_message)
    
     
     if(isTruthy(sent_mail))
     {
       showNotification("Emails Send", type = "message")
     }

    
    
    # output$sentNumbers <- DT::renderDataTable(
    #   edataDays,
    #   options = list(scrollX = TRUE)
    # )
    
    
    
    
  })
  
  #######################Product Summary Graphs###########################################
  output$productCountryControls <- renderUI({
    selectInput(
      "ProductCountryFilter",
      "Please Select Product",
      multiple = FALSE,
      choices = unique(topprod$Description),
      1
    )
    
  })
  
  output$prodCountryGraph <- renderPlotly({
    shinyjs::show("div_productAutoCampaign")
    
    print(
      ggplotly(
        ggplot(topprod, aes(x=reorder(Description,-revenue), y = revenue, fill=Description)) + geom_col() + labs(x = 'Product', y = 'Revenue', title = "Top 10 revenue generating products") +theme(axis.text.x = element_blank(),axis.text.y = element_text(face="bold", color="#000000", 
                                                                                                                                                                                                              size=10, angle=45)),
        width = plotWidth, height = plotHeight
        
      ))
  })
  
  # output$prodCountryGraph <- renderPlotly({
  #   print(ggplotly(
  #     ggplot(topprod, aes(
  #       x = Description, y = revenue, fill = Description
  #     )) + geom_col() + labs(x = 'Product', y = 'Revenue', title = "Top 10 revenue generating products") +
  #       theme(
  #         axis.text.y = element_text(
  #           face = "bold",
  #           color = "#000000",
  #           size =
  #             10,
  #           angle = 45
  #         ),
  #         axis.text.x = element_blank()
  #       )+ geom_text(colour = "white", fontface = "bold", size = 2),
  #     width = plotWidth,
  #     height = plotHeight
  #     
  #   ))
  # })
  
  prodData <- reactive({
    if (isTruthy(input$ProductCountryFilter))
    {
      obj <-
        prodCountrySummary %>% filter(Description == input$ProductCountryFilter)
      return(obj)
      
      showNotification("Data Loaded", type = "message")
      
    }
    else
    {
      obj <-
        prodCountrySummary %>% filter(Description == "REGENCY CAKESTAND 3 TIER")
      return(obj)
      
    }
  })
  
  prodData1 <- reactive({
    if (isTruthy(input$ProductCountryFilter))
    {
      obj <-
        productDaySummary %>% filter(Description == input$ProductCountryFilter)
      return(obj)
      
      showNotification("Data Loaded", type = "message")
      
    }
    else
    {
      obj <-
        productDaySummary %>% filter(Description == "REGENCY CAKESTAND 3 TIER")
      return(obj)
      
    }
  })
  
  output$prodCountryGraph1 <- renderPlotly({
    product <- prodData()
    
    prodtitle <- paste(product$Description, " revenue by Location")
    
    print(ggplotly(
      ggplot(product, aes(
        x=reorder(Country,-revenue), y = revenue, fill = Country
      )) + geom_col() + labs(x = 'Location', y = 'Revenue', title = prodtitle)  +
        theme(
          axis.text.y = element_text(
            face = "bold",
            color = "#000000",
            size =
              8,
            angle = 45
          )
        ),
      width = plotWidth,
      height = plotHeight
      
    ))
  })
  
  output$prodDayGraph1 <- renderPlotly({
    product <- prodData1()
    
    prodtitle <-
      paste(product$Description, " revenue by Day of Week")
    
    print(ggplotly(
      ggplot(product, aes(
        x = dayOfWeek, y = revenue, fill = dayOfWeek
      )) + geom_col() + labs(x = 'Day of Week', y = 'Revenue', title = prodtitle)  +
        theme(
          axis.text.y = element_text(
            face = "bold",
            color = "#000000",
            size =
              8,
            angle = 45
          )
        ),
      width = plotWidth,
      height = plotHeight
      
    ))
  })
  
  #####map######
  output$mymap1 <- renderLeaflet({
    product <- prodData()
    for (row1 in 1:nrow(cities))
    {
      for (row2 in 1:nrow(product)) 
        {
        if(as.character(unlist(cities[row1, "city"])) == as.character(unlist(product[row2, "Country"])))
        {
          product[row2, "Longitude"]=cities[row1, "long"]
          product[row2, "Latitude"]=cities[row1, "lat"]
        }
      }
    }
    leaflet(product, width=plotWidth, height = plotHeight) %>% addTiles() %>%
      addCircles(lng = ~Longitude, lat = ~Latitude, weight = 1,
                 radius = ~sqrt(as.integer(product$revenue)*100) * 50, popup = paste("Product: ",product$Description,"<br/>","City: ",
                                                                                             product$Country,
                                                                                             "<br/>",
                                                                                             "Revenue: ",
                                                                                             product$revenue,"<br/>",product$Longitude,product$Latitude))
  })
  
 
  
  
  output$ProductCountryMarketingFilter <- renderUI({
    product <- prodData()
    selectInput("ProductCountryMarketingFilter", "Please Select Location to Target Marketing", multiple = FALSE ,
                choices = unique(product$Country),1)
    
  })
  
  
  
  ######Product Email###########
  
  observeEvent(input$ProductCampaign, {
    
    custProdData <- customerBreakdownClass%>%filter(Country == input$ProductCountryMarketingFilter) 
    
    # output$prodsummary11 <- DT::renderDataTable(
    #   custProdData,
    #   options = list(scrollX = TRUE)
    product <- input$ProductCountryFilter
    top10prod <- head(custProdData, n=10)
    body1=input$ProductCampaignText
    
    edataDays <- top10prod %>%
      mutate(
        To = top10prod$Email,
        Bcc = optional_bcc,
        From = email_sender,
        Subject = paste("Promotion on ", product),
        body = body1) %>%
      select(To, Bcc, From, Subject, body)
    
    # converting into mime
    emailsDaily <- edataDays %>%
      pmap(mime)
    # 
    # output$daysDT2<-DT::renderDataTable(
    #   edataDays,
    #   options = list(scrollX = TRUE, scrolly = TRUE))

    # return(tryCatch( safe_send_message <- safely(send_message), error = function(e) stop(safeError(e))))
    # 
    #  sent_mail <- emails %>%
    #    map(safe_send_message)
    
    # sending mail
    safe_send_message <- safely(send_message)
    sent_mail <- emailsDaily %>%
      map(safe_send_message)
    
    
    showNotification(sprintf("Email sent to customers: %s",nrow(top10prod)), type = "message")
    
  })
  
  
  ######Product SMS###########
  
  observeEvent(input$ProductCampaignSms, {
    
    custProdData <- customerBreakdownClass%>%filter(Country == input$ProductCountryMarketingFilter) 
    
    # output$prodsummary11 <- DT::renderDataTable(
    #   custProdData,
    #   options = list(scrollX = TRUE))
    #   
      
      
    product <- input$ProductCountryFilter
    top10prod <- head(custProdData, n=10)
    body1=input$ProductCampaignText
    
    messageText <- paste("Promotion Offer for: ",product, "<br>", body1)
    
    
    for (row in 1:nrow(top10prod))
    {
      num=paste(top10prod[row,"Number"])
      r <- POST(url,authenticate(AUTH_ID,AUTH_TOKEN),body=list(src=senderNumber,dst=num,text=messageText))
      
    }
    
    
    showNotification(sprintf("SMS sent to customers: %s",nrow(top10prod)), type = "message")
    
  })
  
  
})# End of shiny
  

# options(shiny.sanitize.errors = TRUE)
#   

#shiny::runApp(display.mode="showcase")
