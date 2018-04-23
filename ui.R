


shinyUI( tagList(
  useShinyjs(),

  navbarPage(
    theme = shinytheme("flatly"), 
    inverse = TRUE,
    title = "Campaign Launch Demo",
    
    
    ##############Week Day insights#############################################
    tabPanel("Daily Insights", 
             # tags$head(tags$style(
      # HTML(
        # '#sidebar {
        # background-color: #ebebe0;
        # color: #000000;
        # height: 1100 px;
        # overflow-y: auto;
        # 
        # }

        # body, label, input, button, select {
        # font-family: "Arial";
        # }hr {border-top: 2px solid #696969;}
        # 
        #.selectInput {white-space: nowrap}
        #choice+ div>.selectize-dropdown{width: 660px !important}
        #choices+ div>.selectize-dropdown{width: 300px !important}'
      # )
    # ))
    # ,
    
    sidebarLayout(
      sidebarPanel(
        fluid = TRUE,
        width = 3,
        id = "sidebar",
       
        hr(),
        h3("Insights",  style = "align:center;text-align:center;text-shadow: 2px 2px #FFFFFF"),
        hr(style ="border-top: dotted 1px #FFFFFF"),
        selectInput(
          "Segmentation",
          "Please Select Segmentaion",
          
          choices = c("Daily", "Hourly"),
          1
        ),
        # # option 1
        conditionalPanel(
          condition = "input.Segmentation == 'Daily'",
          selectInput(
            "segTypeDaily",
            "Select Type of Daily Segmentation",
            choices = c("Per Day Revenue", "Per Day Transactions")
          )
        ),
        
        #       # option 2
        conditionalPanel(
          condition = "input.Segmentation == 'Hourly'",
          selectInput(
            "segTypeHourly",
            "Select Type of Hourly Segmentation",
            choices = c("Hourly Revenue", "Hourly Transaction")
          )
        ),
        
        hr(),
        shinyjs::hidden(
          div(id="div_weekdayMarketing",
         
              
              h3("Marketing", style = "align:center;text-align:center;text-shadow: 2px 2px #FFFFFF"),
              hr(style ="border-top: dotted 1px #FFFFFF"),
              selectInput(
                "WeekdayMarketingChoice",
                "Please Select Marketing Type",
                choices = c("Email Marketing" = "emailMarketing", "SMS Marketing" = "smsMarketing"), selected = "smsMarketing"),
              
              
              br(),
              
              #### Condition for SMS Marketing
              conditionalPanel(
                condition = "input.WeekdayMarketingChoice == 'smsMarketing'",
                selectInput("choice_autoManualSMS",
                            "Auto or Manual",
                            choices = c("Automatic","Manual"),selected = "Automatic"),
                
                conditionalPanel("input.choice_autoManualSMS == 'Manual'",
                                 uiOutput("dayFilterControlForSMS"),
                                 helpText("Note: SMS will be sent to all customers on selected Days"),
                                 br(),
                                 textAreaInput(
                                   "textSmsMarketingWeekday",
                                   "Enter campaign text or offers to send: ",
                                   "Sms Marketing Text",
                                   
                                   "100%",
                                   "100px",
                                   resize = "vertical"
                                 ),
                                 helpText("Note: Enter a custom offer that you want to send!"),
                                 br(),
                                 
                                 actionButton(
                                   "manualSmsCampaign",
                                   " Launch SMS Campaign",
                                   icon("envelope"),
                                   style = "color: #fff; background-color: #337ab7; border-color: #2e6da4; width: 100%; padding-left: 2dp: margin-right: 5px"
                                 )
                ),
                
                
                conditionalPanel("input.choice_autoManualSMS == 'Automatic'",
                                 br(),
                                 actionButton("maxSmsCampaign", "Campaign for Max Revenue Day", style =
                                                "color: #fff; background-color: #6C7A89; border-color: #C0C0C0; width: 100%; padding-left: 2dp: margin: 5dp;"),
                                 helpText("Note: Campaigns will lauch for day with Max Revenue"),
                                 
                                 br(),
                                 actionButton("minSmsCampaign", "Campaign for Min Revenue Day", style =
                                                "color: #fff; background-color: #6C7A89; border-color: #C0C0C0; width: 100%; padding-left: 2dp: margin: 5dp;"),
                                 
                                 helpText("Note: Campaigns will lauch for day with Max Revenue")
                )
                
                
                
              ),
              
              #### Condition for Email Marketing
              conditionalPanel(
                condition = "input.WeekdayMarketingChoice == 'emailMarketing'",
                selectInput(
                  "choice_autoManualEmail",
                  "Auto or Manual",
                  choices = c("Automatic","Manual"),1
                )
                ,
                conditionalPanel("input.choice_autoManualEmail == 'Manual'",
                                 
                                 
                                 uiOutput("dayFilterControlForEmail"),
                                 helpText("Note: Emails will be sent to all customers on selected Days"),
                                 br(),
                                 textAreaInput(
                                   "textEmailMarketingWeekday",
                                   "Enter campaign text or offers to send: ",
                                   "Special Discount Offers!",
                                   "100%",
                                   "100px",
                                   resize = "vertical"
                                 ),
                                 helpText("Note: Enter a custom offer that you want to send!"),
                                 br(),
                                 
                                 actionButton(
                                   "manualEmailCampaign",
                                   "Launch Email Campaign",
                                   icon("envelope"),
                                   style = "color: #fff; background-color: #337ab7; border-color: #2e6da4; width: 100%; padding-left: 2dp: margin: 5dp;"
                                 )
                ),
                hr(),
                conditionalPanel("input.choice_autoManualEmail == 'Automatic'",
                                 actionButton("maxEmailCampaign", "Campaign for Max Revenue Day", style =
                                                "color: #fff; background-color: #6C7A89; border-color: #C0C0C0; width: 100%; padding-left: 2dp: margin: 5dp;"),
                                 helpText("Note: Campaigns will lauch for day with Max Revenue"),
                                 
                                 
                                 br(),
                                 actionButton("minEmailCampaign", "Campaign for Min Revenue Day", style =
                                                "color: #fff; background-color: #6C7A89; border-color: #C0C0C0; width: 100%; padding-left: 2dp: margin: 5dp;"),
                                 helpText("Note: Campaigns will lauch for day with Min Revenue"),
                                 br()
                )
                
              )   
          )
        )
        
      ),
      
      mainPanel(
        fluid = TRUE,
        width = 9,
        plotlyOutput('sidePlot') %>% withSpinner(color = "#0dc5c1"),
        
        br(),
        # uiOutput("dayRangeControl"),
        shinyjs::hidden(
          sliderInput("sliderDaterange",
                      "Choose Date Range:",
                      animate = TRUE,
                      width = "100%",
                      min = as.Date("2010-12-01"), max = as.Date("2011-12-09"),
                      value = c(as.Date("2010-12-01"),as.Date("2011-12-09"))
                      
          )  
        ),
        
        br(),
        hr(),
        plotlyOutput('sidePlot2') %>% withSpinner(color = "#0dc5c1"),
        br(),
        br()
        
        
        
      )
      
    )
    ),

###############Customer Insights Tab######################################
tabPanel("Individual Customer Insights",
         sidebarLayout
         (
           sidebarPanel(
             id = "sidebar",
             fluid = TRUE,
             width = 3,
             hr(),
             h3("Individual Insights", style = "align:center;text-align:center;text-shadow: 2px 2px #FFFFFF"),
             hr(style ="border-top: dotted 1px #FFFFFF"),
             sliderInput("numOfCustomers", "Number of Customers:",
                         min = 0, max = 100,
                         value = 20),
             shinyjs::hidden(
               div(
                 id="div_personalizedCampaign",
                 br(),
                 actionButton(
                   "cusPersonalizedCampaignEmail",
                   "Personalised Email Campaign",
                   icon("envelope"),
                   style = "color: #fff; background-color: #337ab7; border-color: #2e6da4; width: 100%; padding-left: 2dp: margin: 5dp;"
                 ),
                 helpText(
                   "Note: Selecting this button will launch an Email Campaign for top Customers"
                 ),
                 
                 br(),
                 actionButton(
                   "cusPersonalizedCampaignSms",
                   "Personalised SMS Campaign",
                   icon("envelope"),
                   style = "color: #fff; background-color: #337ab7; border-color: #2e6da4; width: 100%; padding-left: 2dp: margin: 5dp;"
                 ),
                 helpText(
                   "Note: Selecting this button will launch an SMS Campaign for top Customers"
                 ),
                 
                 
                 br(),
                 hr()    
               )
             ),
             
             h3("Classified Insights", style = "align:center;text-align:center;text-shadow: 2px 2px #FFFFFF"),
             hr(style ="border-top: dotted 1px #FFFFFF"),
             uiOutput("customerRFMchoice") %>% withSpinner(color =
                                                             "#696969", type = 7),
             helpText(
               "Note: Selecting a Classification will display graph of customers of that class in all countries"
             ),
             
             
             hr(),
             shinyjs::hidden(
               div(id="div_customerMarketing",
                   h3("Marketing", style = "align:center;text-align:center;text-shadow: 2px 2px #FFFFFF"),
                   hr(style ="border-top: dotted 1px #FFFFFF"),
                   selectInput(
                     "CustomerMarketingChoice",
                     "Please Select Marketing Type",
                     choices = c("Email Marketing" = "emailMarketing", "SMS Marketing" = "smsMarketing"),
                     2
                   ),
                   helpText("Note: Select how you want to do marketing"),
                   br(),
                   
                   #### Condition for SMS Marketing
                   conditionalPanel(
                     condition = "input.CustomerMarketingChoice == 'smsMarketing'",
                     
                     textAreaInput(
                       "textSmsMarketing",
                       "Enter campaign text or offers to send: ",
                       "Special Discount Offers!",
                       "100%",
                       "100px",
                       resize = "vertical"
                     ),
                     
                     actionButton("manualSmsCampaignCustomer", "Launch SMS Campaign", icon("envelope"), style =
                                    "color: #fff; background-color: #337ab7; border-color: #2e6da4; width: 100%; padding-left: 2dp: margin: 5dp;"),
                     hr()
                     
                     
                   ),
                   
                   #### Condition for Email Marketing
                   conditionalPanel(
                     condition = "input.CustomerMarketingChoice == 'emailMarketing'",
                     
                     # uiOutput("dayFilterControlForEmail"),
                     # helpText("Note: Emails will be sent to all customers on selected Days"),
                     #
                     textAreaInput(
                       "textEmailMarketing",
                       "Enter campaign text or offers to send: ",
                       "Special Discount Offers!",
                       "100%",
                       "100px",
                       resize = "vertical"
                     ),
                     actionButton("manualEmailCampaignCustomer", "Launch Email Campaign",  icon("envelope"), style =
                                    "color: #fff; background-color: #337ab7; border-color: #2e6da4; width: 100%; padding-left: 2dp: margin: 5dp;"),
                     hr()
                     
                   )      
                   
                   )#end of div_customerMarketing
             )#end of shinyjs::hidden
             
             
           ),
           
           mainPanel(
             fluid = TRUE,
             width = 9,
             br(),
             plotlyOutput('revenuePerCustomer') %>% withSpinner(color =
                                                                  "#0dc5c1"),
             
             verbatimTextOutput("revenuePerCustomerClickInfo"),
             
             # DT::dataTableOutput("top20customersDT", width=800)%>% withSpinner(color =
                                                                               # "#0dc5c1"),
             # 
             # 
             br(),
             # plotlyOutput("customerClickGraph") %>% withSpinner(color =
             #                                                      "#0dc5c1"),
             # 
             br(),
             
             hr(),
           
             plotlyOutput('rfmGraphPlot') %>% withSpinner(color = "#0dc5c1"),
             
             br(),
             hr()
             
             # verbatimTextOutput("smsResponse")
             # 
             
           )
         )),


#### Country Segmentation ####
tabPanel("Location Centric Insights",
         sidebarLayout(
           fluid = TRUE,
           sidebarPanel(
             width = 3,
             # CONTROLS conditional panels
             # selectInput("CountrySegmentation", "Please Select Segmentaion",
             #             choices=c("Revenue by Country","Average Order Value over Time", "Numer of Daily Transactions"),1),
             #
             uiOutput("selCountry"),
             
             
             br(),
             sliderInput("sliderNumOfProducts", "Select Number of Products:",
                         min = 0, max = 20,
                         value = 5),
             br(),
             hr(),
             
             shinyjs::hidden(
               div(
                 id="div_countryCampaign",
                 
             
             h3("Marketing",style = "align:center;text-align:center;text-shadow: 2px 2px #FFFFFF"),
             hr(style ="border-top: dotted 1px #FFFFFF"),
             uiOutput("countryControls") %>% withSpinner(color =
                                                              "#696969", type = 7),
             helpText("Note: Emails will be sent to all customers in selected Locations"),
             br(),
             uiOutput("productControls"),
             # choices=c('All','Netherlands','EIRE','Germany','France','Australia'),1),
             helpText("Note: Discounts will be applied only on selected products"),
             
             textAreaInput(
               "camCountryMessage",
               "Enter campaign text or offers to send: ",
               "Special Discount Offers!",
               "100%",
               "100px",
                resize = "vertical"
             ),
             actionButton("manualCountryCampaignEmail", "Launch Email Campaign",icon("envelope"),
                          style = "color: #fff; background-color: #337ab7; border-color: #2e6da4; width: 100%; padding-left: 2dp: margin-right: 5px"),
             br(),
             br(),
             actionButton("manualCountryCampaignSms", "Launch SMS Campaign",icon("envelope"),
                          style = "color: #fff; background-color: #337ab7; border-color: #2e6da4; width: 100%; padding-left: 2dp: margin-right: 5px"),
             br(),
             
             hr()
               )#end of div_countryCampaign
             )#end of shinyjs::hidden
           ),
           
           mainPanel(
             width = 9,
            
             br(),
            
             plotlyOutput("countryPlotly") %>% withSpinner(color =
                                                             "#0dc5c1"),
            
             br(),
             hr(),
             plotlyOutput("countryTransPlotly") %>% withSpinner(color ="#0dc5c1"),
             br(),
             hr(),
             h3("Map Displaying Revenue Generated Per Location",style = "align:center;text-align:center;text-shadow: 2px 2px #FFFFFF"),
             br(),
             
             leafletOutput("mymap",width = "90%"),
             br(),
             hr(),
             
             plotlyOutput("transactionPlotly") %>% withSpinner(color =
                                                                 "#0dc5c1"),
             
             
             
             br(),
             br()
             # verbatimTextOutput("countryText"),
             # verbatimTextOutput("emailText")
             # 
             # 
             
           )
         )),


#######Product Insights#########################

tabPanel(
  "Product Insights",
  sidebarPanel(
    id = "sidebar",
    fluid = TRUE,
    width = 3,
    uiOutput('productCountryControls') ,
    helpText("Select product to view insights and trends related to the product"),
    hr (),
    h3("Product Marketing",style = "align:center;text-align:center;text-shadow: 2px 2px #FFFFFF"),
    hr(style ="border-top: dotted 1px #FFFFFF"),
    uiOutput('ProductCountryMarketingFilter')%>% withSpinner(color =
                                                               "#696969", type = 7),
    textAreaInput(
      'ProductCampaignText',
      "Enter campaign text or offers to send: ",
      "Special Discount Offers!",
      "100%",
      "100px",
       resize = "vertical"
    ),
    shinyjs::hidden(
      div(id="div_productAutoCampaign",
    
          actionButton(
            "ProductCampaign",
            "Launch Email Campaign",
            icon("envelope"),
            style = "color: #fff; background-color: #337ab7; border-color: #2e6da4; width: 100%; padding-left: 2dp: margin: 5dp;"
          ),
          hr(),
          actionButton(
            "ProductCampaignSms",
            "Launch SMS Campaign",
            icon("envelope"),
            style = "color: #fff; background-color: #337ab7; border-color: #2e6da4; width: 100%; padding-left: 2dp: margin: 5dp;"
          ),
          helpText(
            "Note: Selected product campaign will be launched targeting consumers of selected location via Email and SMS"
          )    
      )#end of div_productAutoCampaign
    )#end of shinyjs::hidden
    
  ), 
  mainPanel(
    width = 9,
    # DT::dataTableOutput("prodsummary11", width=800)%>% withSpinner(color =
    #                                                                  "#0dc5c1"),
    # 
    plotlyOutput('prodCountryGraph') %>% withSpinner(color =
                                                       "#0dc5c1"),
    br(),
    hr(),
    plotlyOutput('prodCountryGraph1') %>% withSpinner(color =
                                                        "#0dc5c1"),
    br(),
    hr(),
    h3("Map Displaying Revenue Generated Per Location",style = "align:center;text-align:center;text-shadow: 2px 2px #FFFFFF"),
    br(),
    leafletOutput("mymap1", width = "90%") %>% withSpinner(color =
                                                    "#0dc5c1"),
    br(),
    hr()
    
  )
)
)#end of navBar
)#end of taglist
)#end of shinyUI
options(shiny.error = browser)
options(warn = -1)
