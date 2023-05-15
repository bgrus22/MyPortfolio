#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


#import libraries
##wrangling
library(tidyverse)
library(dplyr)
library(stringr)
library(scales)
library(readxl)

##visualization
library(plotly)
library(ggplot2)
library(maps)
library(ggthemes)
library(ggrepel)

##shiny stuff
library(shiny)
library(shinydashboard)
library(shinythemes)



#load in the data from the data folder
LCOEdta <-  read_csv('CleanData/LCOE.csv')
globalplants <- read_csv('CleanData/globalplants1.csv')
CCoEdta <-  read_csv('CleanData/capital-costs-of-electricity.csv')
Materials <- read_csv('CleanData/Plant_Materials.csv')
Matprice <- read_csv('CleanData/Material_prices.csv')


#add in more data
droplist = c("Country","Plant type","Construction costs (USD/MWh)", "Refurbishment costs (USD/MWh)", "Decommissioning costs (USD/MWh)", "Operations & maintenance costs (USD/MWh)", 'energy_type')
LCOE1 <- LCOEdta[, !(names(LCOEdta) %in% droplist)]
#pivot
LCOE1 <- pivot_longer(
  LCOE1,
  cols = c("Total capital costs (USD/MWh)","Total Fuel cost (USD/MWh)"),
  names_to = "Cost_Type", 
  values_to = "Cost")
expagg <- LCOE1 %>% 
  select(-`Plant category`) %>% 
  group_by(`Energy type`, Cost_Type) %>% 
  summarize(Avg_Cost = mean(Cost))
expagg_lab <- expagg %>% 
  group_by(`Energy type`) %>%
  summarise(total_cost = sum(Avg_Cost)) %>%
  arrange(desc(total_cost)) %>%
  pull(`Energy type`) %>%
  factor(., levels = ., ordered = TRUE)


# Use group_by and summarise functions to calculate the mean of column_b for each unique category in column_a
globalplants_temp <- globalplants %>%
  group_by(primary_fuel) %>%
  summarise(`Average Capacity` = mean(capacity_mw))

# Merge the new column with the original dataframe using the merge function
globalplants <- merge(globalplants, globalplants_temp, by = "primary_fuel")


# Define UI for application 
ui <- fluidPage(theme = shinytheme("superhero"),
                tags$div(
                  class = "banner",
                  div(
                    style = "position: relative;",
                    img(
                      src = "https://www.ferreirapw.com/wp-content/uploads/2019/02/home-banner-power-lines.jpg",
                      height = "150px",
                      width = "100%",
                      style = "display: block;"
                    ),
                    h1(
                      style = "position: absolute; top: 20px; left: 20px;",
                      "An Overview of the Future of Energy"
                    )
                  )
                ),
                
                #make the navbar
                navbarPage("Energy Development Analysis",
                           ######################page 1#########################
                           tabPanel("About",
                                    h2("Existing Powerplants"),
                                    hr(),
                                    # Sidebar  
                                    sidebarLayout(
                                      position = "right", # set sidebar position to right
                                      sidebarPanel(
                                        br(),
                                        h4("Filters:"),
                                        checkboxGroupInput("box_filter",
                                                           h5("Energy Types"),
                                                           choices = unique(globalplants$primary_fuel),
                                                           selected = unique(globalplants$primary_fuel),
                                                           inline = TRUE),
                                        radioButtons("gtype1",
                                                     h5("Supplementary Graph Format"),
                                                     choices = c("Capacity",
                                                                 "Geography"),
                                                     selected = "Capacity",
                                                     inline = FALSE),
                                        selectInput("geo",
                                                    h5("Geographic Area of Interest"),
                                                    choices = c("World",
                                                                "Europe",
                                                                "North America"),
                                                    selected = "World"),
                                        tags$small(
                                          "These visualizations are based on the World Power Plant Database to demonstrate an existing distribution of power plants"
                                        )
                                      ),
                                      
                                      # Show a plot of the generated distribution
                                      mainPanel(
                                        p("To understand the future, we must look at the present. ",
                                          "Below are visualizations based on approximately 35,000 power plants out of the world's roughly 62,500 which have been recorded in the World Power Plant Database. ",
                                          "These are located across 167 different countries, and their distribution can be seen in ",
                                          em("Figure 1.3"),
                                          ". ",
                                          "Using it as an accurate representation of the present, one can see what types of plants have traditionally been built and how much electricity they produce. "),
                                          #br(),
                                          p("When examining these, one should note that when excluding ",
                                          strong("'Hydro'"),
                                          " from ",
                                          em("Figure 1.2"),
                                          ", we can clearly see that ",
                                          strong("Coal, Gas, "),
                                          "and ",
                                          strong("Nuclear"),
                                          " have higher median capacities, a measure of how much electricity can be produced at highest levels of activity consistently, and most plants have comparable levels of capacity overall. "),
                                        p("One should also note these first graphs are ordered in such a way that you can", 
                                          strong("compare them vertically"),
                                          " to better see the relationship between what has been built and plant capacity."),
                                        br(),
                                        plotOutput("distPlot"),
                                        tags$small(
                                          strong("Figure 1.1")
                                        ),
                                        plotOutput("distsupp"),
                                        tags$small(
                                          strong(textOutput("fig1"))
                                        ),
                                        hr(),
                                        p("As we will see, many fuel types are becoming viable alternatives due to changing costs of production, and that what has traditionally been built may not be the most logical option for future development from a fiscal perspective, especially in the world of green energy. ",
                                          "This dashboard will present economic considerations on what direction future energy projects ought to take, as well as touch on tangential issues that preclude other potential fuel types."),
                                        br(),
                                        br(),
                                        br()
                                      )
                                    )
                           ),
                           
                           ######################page 2#########################
                           tabPanel("Raw Material Costs",
                                    h2("Raw Material Costs"),
                                    hr(),
                                    sidebarLayout(
                                      position = "right",
                                      sidebarPanel(
                                        br(),
                                        h4("Filters:"),
                                        checkboxGroupInput("mat_filter",
                                                           h5("Materials Included"),
                                                           choices = unique(Matprice$Component),
                                                           selected = unique(Matprice$Component),
                                                           inline = FALSE),
                                        selectInput("Year_val",
                                                    label = h5("Year Examined"),
                                                    choices = seq(2013,2022,1),
                                                    selected = 2022),
                                        #sliderInput("Year_val",
                                        #            label = h5("Year Examined"),
                                        #            min = 2013,
                                        #            max = 2022,
                                        #            value = 2022,
                                        #            step = 1, 
                                        #            sep = ""),
                                        radioButtons("Construction_type",
                                                     h5("Bar Graph Variable"),
                                                     choices = c("Materials Used",
                                                                 "Construction Cost"),
                                                     selected = "Materials Used",
                                                     inline = FALSE),
                                        tags$small(
                                          "The price data comes from commodity market prices recorded by the International Monetary Fund and the material usage data is from a International Energy Agency material used dataset."
                                        )
                                      ),
                                      mainPanel(
                                        p("The initial conditions for developing these plants vary depending on fuel type. ",
                                          "One obviously cannot utilize hydro power without access to water or have efficient solar energy without sufficient daylight hours. ",
                                          "But just as location matters, so too do technological limitations. ",
                                          "For many nations nuclear energy is difficult to utilize without raising eyebrows at home and abroad due to strategic and safety concerns, and at present there are ",
                                          strong("only 12 vessels in the world"),
                                          "that are capable of building modern offshore wind power plants. ",
                                          a(href="https://www.theverge.com/22296979/us-offshore-ships-wind-boom-installation-vessels",
                                            HTML("<sup>1</sup>")),
                                          " Aside from these more variable costs, rare earth, raw materials used to make plants are often in comparable amounts. ",
                                          "Based on the trends shown in ",
                                          em("Figure 2.1"),
                                          ", such raw material costs will only drive up the overall costs of many power plant designs. "),
                                        br(),
                                        plotOutput("LinePlot"),
                                        tags$small(
                                          strong("Figure 2.1")
                                        ),
                                        hr(),
                                        p("Below are visualizations of rare earth metal usage and costs in the construction of power plants for a variety of energy types that have more standardized construction formats. ",
                                          "Using the interactions, you can see the incorporation of these raw materials into price per MW of capacity and see how rising material costs are disproportionately affecting certain fuel types. ",
                                          "One ought to note how some fuel types are dependent on specific materials and that these prices alone do not include transportation, a serious issue in the case of ",
                                          tags$strong("Offshore Wind.")),
                                        br(),
                                        plotlyOutput("ConstuctBars"),
                                        tags$small(
                                          strong(textOutput("fig2"))
                                        ),
                                        hr(),
                                        p("One should note that this data does not account for the materials that have more variable needs, like aluminum and steel, or metals that are used to a lesser extent, such as molybdenum; even though both are substantial costs. ",
                                          "Despite this gap in the data, two things are apparent. ",
                                          "The cost of raw materials has been increasing substantially over time; and with the prices continuing to rise it is cheaper to develop today rather than later, unless one is hoping for a technological breakthrough to create a cheaper power plant design."),
                                        p("On the next page, we will see that including maintenance, land, and operating costs we will see a more complete cost of development, but that does not negate the fact that the cost of construction is increasing for many of these more established technologies. ",
                                          "Although ",
                                          tags$strong("Natural Gas "),
                                          " appears relatively cheap here, one will see that overall costs of development and production are much higher, but through this one can see that one of the major components of development is going up in price. ",
                                          "For these reasons overall capital costs need to be examined in order to get a fuller grasp of startup price, which will be seen in the next page. "
                                        ),
                                        br(),
                                        br(),
                                        br()
                                        #verbatimTextOutput("summary")
                                      )
                                      
                                    )
                           ),
                           
                           ######################page 3#########################
                           tabPanel("Overall Costs",
                                    h2("Additional Costs"),
                                    hr(),
                                    sidebarLayout(
                                      position = "right", # set sidebar position to right
                                      sidebarPanel(
                                        br(),
                                        h4("Filters:"),
                                        radioButtons("Prod_type",
                                                     h5("Select Format:"),
                                                     choices = c("Grouped",
                                                                 "Stacked"),
                                                     selected = "Grouped",
                                                     inline = FALSE),
                                        tags$small("The cost of production data was obtained via an International Energy Agency dataset which examined over 300 power plants across multiple countries. ",
                                                   "The Levelized costs visualized in ",
                                                   em("Figure 3.1 "),
                                                   " are based on the cost of new plants being built in the U.S. that are expected to begin generating electricity in 2027. ")
                                      ),
                                      mainPanel(
                                        p("While in the previous page rising material prices were examined, here one sees the full cost of power plant construction in ",
                                          em("Figure 3.1"),
                                          ". ",
                                          "It relates the levelized capital cost of energy (LCOE) of new plants being built in the United States with operations projected to start in 2027. ",
                                          "The LCOE is a measure of financial viability that focuses on the capital costs, such as acquiring the land, equipment, and construction resources, like the previously examined materials. ",
                                          "Although this graph examines generation rather than capacity, it better relates the costs needed to begin producing energy at a power plant and demonstrates that many of the previously more popular fuel types are not the cheapest to produce. "),
                                        br(),
                                        plotlyOutput("LevelBar"),
                                        tags$small(
                                          strong("Figure 3.1"),
                                        ),
                                        hr(),
                                        p("From the graphic above, one sees that at present the comparative capital costs of energy would favor developing things such as ",
                                          tags$strong("Solar, Geothermal"),
                                          "or ",
                                          tags$strong("Combined Cycle"),
                                          ". ",
                                          "But capital costs at startup are not the only fiscal considerations that are necessary. ",
                                          "As one can see below, based on the data of over 300 plants around the world, fuel costs and capital maintenance are a burden for many types of plants. ",
                                          " Again though, ",
                                          tags$strong("Geothermal and Solar Photovoltaic (PV)"),
                                          "Appear to have relatively cheap costs to initiate and continue producing from. "),
                                        p("Note that with the sidebar's filter the below visualization's format",
                                        tags$strong("can be changed.")),
                                        br(),
                                        plotOutput("GroupedBar"),
                                        tags$small(
                                          strong(textOutput("fig3")),
                                        ),
                                        hr(),
                                        p("What has been shown are just the financial considerations in developing new energy. ",
                                          "It does not begin to consider political ramifications like the American 1-2-3 Nuclear bilateral agreements many countries in the world must cooperate with in order to develop nuclear energy,",
                                          a(href="https://www.energy.gov/nnsa/123-agreements-peaceful-cooperation",
                                            HTML("<sup>2</sup>")),
                                          " nor does it look at how the reallocated land may result in the disruption of commerce or wildlife, which has become a hot topic for Offshore Wind and land intensive, large scale Solar farms.",
                                          a(href="https://www.fisheries.noaa.gov/topic/offshore-wind-energy/overview",
                                            HTML("<sup>3</sup>")),
                                          a(href="https://ourworldindata.org/land-use-per-energy-source",
                                            HTML("<sup>4</sup>")),
                                          " If, however, one were to include these considerations, then it is in our opinion that the renewable energy source which is most fiscally feasible and without geographic constraints would be ",
                                          strong("Geothermal Energy"),
                                          "."),
                                          br(),
                                        p("Despite its lack of popularity, new technologies are reducing the startup costs which have made it previous unattainable, and new U.S. government initiatives are seeking to reduce the initializing and production costs further.",
                                          a(href="https://www.law.georgetown.edu/environmental-law-review/blog/the-doe-wants-to-cut-the-cost-of-geothermal-by-90-simplifying-geothermal-permitting-could-help-it-get-there/#:~:text=Using%20that%20money%2C%20the%20DOE's,EGS%20technology%20at%20four%20sites.",
                                            HTML("<sup>5</sup>")),
                                          

                                          
                                          "Although viable nonrenewable options like Gas, Coal, and Lignite exist, the availability and affordability of Geothermal would make it appear as a viable candidate despite its lack of popularity.  ",
                                          "Based on the easy access to develop it, and the comparable energy production levels despite cheap costs, it would appear that Geothermal energy would be the best option for green energy development. "),
                                        br(),
                                        br(),
                                        br()
                                      )
                                    )
                           ),
                           
                           ######################page 4#########################                           
                           #sources page
                           tabPanel("Sources",
                                    h2("For More Information"),
                                    hr(),
                                    h3("List of Data Sources"),
                                    tags$ul(
                                      tags$li(
                                        tags$a(href = "https://datasets.wri.org/dataset/globalpowerplantdatabase", "WRI"),
                                        ", Global Power Plant Database"
                                      ),
                                      tags$li(
                                        tags$a(href = "https://www.iea.org/data-and-statistics/data-tools/levelised-cost-of-electricity-calculator", "IEA"),
                                        ", Plant Costs Database"
                                      ),
                                      tags$li(
                                        tags$a(href = "https://www.statista.com/statistics/194327/estimated-levelized-capital-cost-of-energy-generation-in-the-us/", "Statista"),
                                        ", Levelized New Plant Capital Costs"
                                      ),
                                      tags$li(
                                        tags$a(href = "https://data.imf.org/?sk=471DDDF8-D8A7-499A-81BA-5B332C01F8B9", "IMF"),
                                        ", Commodity Database"
                                      ),
                                      tags$li(
                                        tags$a(href = "https://www.iea.org/data-and-statistics/charts/minerals-used-in-clean-energy-technologies-compared-to-other-power-generation-sources", "IEA"),
                                        ", Materials Used In Plant Construction"
                                      )
                                    ),
                                    h3("References"),
                                    tags$ol(
                                      tags$li(
                                        tags$a(href = "https://www.theverge.com/22296979/us-offshore-ships-wind-boom-installation-vessels","The US offshore wind boom will depend on these ships"),
                                        ", by ",
                                        em("The Verge")
                                        ),
                                      tags$li(
                                        tags$a(href = "https://www.energy.gov/nnsa/123-agreements-peaceful-cooperation","123 Agreements for Peaceful Cooperation"),
                                        ", from the ",
                                        em("National Nuclear Security Administration")
                                        ),
                                      tags$li(
                                        tags$a(href = "https://www.fisheries.noaa.gov/topic/offshore-wind-energy/overview","NOAA Fisheries: Offshore Wind Energy"),
                                        ", from the ",
                                        em("National Oceanic and Atmospheric Administration")
                                      ),
                                      tags$li(
                                        tags$a(href = "https://ourworldindata.org/land-use-per-energy-source","How does the land use of different electricity sources compare?"),
                                        ", from ",
                                        em("Our World in Data")
                                      ),
                                      tags$li(
                                        tags$a(href = "https://www.law.georgetown.edu/environmental-law-review/blog/the-doe-wants-to-cut-the-cost-of-geothermal-by-90-simplifying-geothermal-permitting-could-help-it-get-there/#:~:text=Using%20that%20money%2C%20the%20DOE's,EGS%20technology%20at%20four%20sites.","The DOE Wants to Cut the Cost of Geothermal by 90%. Simplifying Geothermal Permitting Could Help it Get There."),
                                        ", by ",
                                        em("The Georgetown Law Review")
                                        )
                                      ),
                                    hr(),
                                    p("For more information, please request a copy of the report.")
                           ) 
                )
)

######################Server#########################                           



# Define server logic required to draw a histogram
server <- function(input, output) {
  
  ######################page 1######################
  
  #subset data using reactive
  globalplants_temp1 <- reactive({
    globalplants_temp <- subset(globalplants, primary_fuel %in% input$box_filter)
    
    # Get count of each fuel type
    fuel_counts <- table(globalplants_temp$primary_fuel)
    
    # Order fuel types by count
    fuel_counts <- names(sort(fuel_counts, decreasing = TRUE))
    
    # Convert primary_fuel to factor with ordered levels
    globalplants_temp$primary_fuel <- factor(globalplants_temp$primary_fuel, levels = fuel_counts)
    
    return(globalplants_temp)
})

  #make first plot
  output$distPlot <- renderPlot({
      ggplot(globalplants_temp1(), aes(x = primary_fuel, fill = `Average Capacity`)) +
      geom_bar() +
      geom_text(stat = "count", aes(label = after_stat(count)), vjust = -.3) +
      theme_hc() +
      scale_fill_gradient(low = "darkblue",high = "darkorange") +
      labs(title = "Global Power Plant Fuel Usage",
           subtitle = "A glimpse at energy types of ~1/2 of global power plants",
           y= "Count",
           x="Power Plant Fuel Type",
           fill = "Fuel Type Average Plant Capacity") +
      theme(legend.key.width = unit(1.5, "cm"))
  })
  
  #make the figure number for below
  output$fig1 <- renderText({
    if (input$gtype1 == "Capacity") {
      paste("Figure 1.2")
    } else {
      paste("Figure 1.3")
    }
  })
  
  #make world map for the visualizations that reacts
  maptemp <- reactive({
    if (input$geo == "World") {
      map_data("world") %>% 
        filter(lat > -60 & lat < 90) %>% 
        as.data.frame()
    } else if (input$geo == "Europe") {
      map_data("world") %>% 
        filter(lat > 30 & lat < 90) %>% 
        filter(long > -30 & long < 43) %>%  # Keep only the Eastern Hemisphere
        filter(region != "Antarctica") %>%  # Remove Antarctica
        as.data.frame()
    } else {
      map_data("world") %>% 
        filter(lat > 5 & lat < 90) %>% 
        filter(long < -55) %>%  # Keep only the Western Hemisphere
        filter(region != "Antarctica") %>%  # Remove Antarctica
        as.data.frame()
    }
  })
  
  #subset the data as needed for the 'zooming function'
  globalplants_temp2 <- reactive({
    holder <- if (input$geo == "World") {
      globalplants_temp1()
    } else if (input$geo == "Europe") {
      globalplants_temp1() %>% 
        filter(longitude > -30 & longitude < 43) %>% 
        filter(latitude > 30)
    } else {
      globalplants_temp1() %>% 
        filter(longitude < -55) %>% 
        filter(latitude > 5)
    } 
    return(holder)
  })
  
  #reactive map title
  maptitle <- reactive({
    maptit <- if (input$geo == "World") {
      paste("Power Plants around the World")
    } else {
      paste("Power  Plants around ",
            input$geo)
    }
    return(maptit)
  })
  
  
  #make box plot
  output$distsupp <- renderPlot({
    if (input$gtype1 == "Capacity") {
      ggplot(globalplants_temp1(), aes(x = primary_fuel, y = capacity_mw, color = `Average Capacity`)) +
        geom_boxplot() +
        theme_hc() +
        scale_color_gradient(low = "darkblue",high = "darkorange") +
        labs(title = "Median Capacity by Power Plant Fuel",
             subtitle = "Visualizing the Capacity Distribution of the Different Fuel Types",
             x = "Power Plant Fuel Type",
             y = "Power Plant Capacity (MW)",
             color = "Fuel Type Average Plant Capacity") +
        theme(legend.key.width = unit(1.5, "cm"))
    } else {
      #make the map
      ggplot() +
        geom_polygon(data = maptemp(), aes(x = long, y = lat, group = group), fill = "snow", color = "black") +
        geom_point(data = globalplants_temp2(), aes(x = longitude, y = latitude, color = primary_fuel)) +
        labs(title = maptitle(),
             subtitle = "Verifying Database Integrity",
             x = NULL, y = NULL,
             color = "Power Plant Fuel Type") +
        theme_void() 
      }
  })
  
  ######################page 2#########################
  
  #subset material list for line graph
  Matprice_temp <- reactive({
    subset(Matprice, Component %in% input$mat_filter)
  }) 
  Mat_temp <- reactive({
    subset(Materials,Component %in% input$mat_filter)
  })
  # Convert input$Year_filter to datetime format
  year_filter <- reactive({
    as.Date(paste(input$Year_val, "-01-01", sep = ""), "%Y-%m-%d")
  })
  #convert input$Year_filter to price format
  price_year <- reactive({
    paste("price in ", input$Year_val, sep="")
  })
  year_title <- reactive({
    paste("Material Prices for ", input$Year_val, sep="")
  })
  
  #make the line graph
  output$LinePlot <- renderPlot({
    #make the key to reorder the legend
    last_year <- Matprice_temp() %>% filter(Year == as.Date('2022-01-01'))
    last_year <- last_year[order(last_year$Price_per_Ton, decreasing = TRUE),]
    
    ggplot(Matprice_temp(),aes(x = Year,
                               y = Price_per_Ton,
                               group = Component,
                               color = factor(Component, levels = last_year$Component))) +
      geom_line() +
      theme_minimal() +
      #vertical line
      geom_vline(xintercept = year_filter(), linetype = "dashed", color = "black") +
      scale_y_continuous(labels = scales::dollar_format(prefix = "$")) +
      labs(title = "Rare Earth Material Prices Over Time",
           subtitle = "Visualizing Rising Material Prices",
           x = "Year", y = "Price of Materials ($/Metric Ton)",
           color = "Materials") +
      #line graph labels
      geom_text_repel(
        data = Matprice_temp() %>% filter(Year == year_filter()),
        aes(label = scales::dollar(Price_per_Ton)),
        x = year_filter(),
        y = Matprice_temp() %>% filter(Year == year_filter()) %>% pull(Price_per_Ton),
        direction = "y",
        nudge_y = 2,
        nudge_x = 2000
        #        color = "black"
      )
  })  
  
  #make the figure number for below
  output$fig2 <- renderText({
    if (input$Construction_type == "Materials Used") {
      paste("Figure 2.2")
    } else {
      paste("Figure 2.3")
    }
  })
  
  
  #make the stacked bar graphs
  output$ConstuctBars <- renderPlotly({
    #make the key to reorder the legend
    last_year <- Matprice_temp() %>% filter(Year == as.Date('2022-01-01'))
    last_year <- last_year[order(last_year$Price_per_Ton, decreasing = TRUE),]
    
    
    if (input$Construction_type == "Materials Used") {
      cb <- ggplot(Mat_temp(), aes(x = reorder(`Plant Type`, `Kg/MW of Capacity`), y = `Kg/MW of Capacity`,
                                   fill = factor(Component, levels = last_year$Component))) +
        #aes text used to make plotly label
        geom_bar(stat = "identity", aes(text = paste("Fuel Type: ", `Plant Type`, "<br>",
                                                     "Raw Material: ", `Component`, "<br>",
                                                     "Kg/MW of Capacity: ", `Kg/MW of Capacity`))) + 
        labs(title = "Materials Needed by Types of Power Plant Fuel",
             #subtitle = "Excluding Steel, Aluminum, and Lesser Used Metals", 
             x = "Power Plant Fuel Type", y = "Kg Needed/MW of Capacity", fill = "Component") +
        coord_flip() +
        theme_bw() 
    } else {
      cb <- ggplot(Mat_temp(), aes(x = reorder(`Plant Type`, `Kg/MW of Capacity`), y = Mat_temp()[[price_year()]],
                                   fill = factor(Component, levels = last_year$Component))) +
        #aes text used to make plotly label
        geom_bar(stat = "identity", aes(text = paste("Fuel Type: ", `Plant Type`, "<br>",
                                                     "Raw Material: ", `Component`, "<br>",
                                                     "Price per Megawatt of Capacity: ", scales::dollar(Mat_temp()[[price_year()]])))) + 
        scale_y_continuous(labels = scales::dollar_format(prefix = "$")) +
        labs(title = year_title(),
             #subtitle = "Excluding Steel, Aluminum, and Lesser Used Metals", 
             x = "Power Plant Fuel Type", y = "Price per Megawatt of Capacity ($/MW)", fill = "Component") +
        coord_flip() +
        theme_bw()
    }
    #aes text used to make plotly label
    ggplotly(cb, tooltip = "text")
    
  })
  
  
  
  
  ######################page 3#########################
  
  #make capital cost of starting construction today graph
  output$LevelBar <- renderPlotly({
    lb_temp <- ggplot(CCoEdta, aes(x = reorder(`Energy Source`, -`Levelized Cost of Generation($ per megawatt hour)`, FUN = sum), 
                        y=`Levelized Cost of Generation($ per megawatt hour)`,fill=reorder(`Energy Source`, -`Levelized Cost of Generation($ per megawatt hour)`))) +
      #aes text used to make plotly label
      geom_bar(stat = "identity", aes(text = paste("Fuel Type: ", `Energy Source`, "<br>",
                                                   "Levelized Capital Construction Costs: ", scales::dollar(`Levelized Cost of Generation($ per megawatt hour)`)))) + 
      labs(title = "Levelized Capital Costs of Powerplants by Energy Source",
           x = "Power Plant Fuel Type", y = "Levelized Capital Construction Costs ($ per MWh)",
           fill = NULL) +
      theme_hc() +  
      scale_y_continuous(labels = scales::dollar_format(prefix = "$")) +
      geom_text(aes(label = scales::dollar(`Levelized Cost of Generation($ per megawatt hour)`)),vjust = -.2) +
      theme(axis.text.x = element_blank(),  # remove x-axis labels
            axis.ticks.x = element_blank())  # remove x-axis ticks)
    
    #aes text used to make plotly label
    ggplotly(lb_temp, tooltip = "text")
  })
  
  
  
  #make the figure number for below
  output$fig3 <- renderText({
    if (input$Prod_type == 'Grouped') {
      paste("Figure 3.2")
    } else {
      paste("Figure 3.3")
    }
  })
  
  #make the if-based bar chart
  output$GroupedBar <- renderPlot({
    #this was the code for simplified fuel types that was removed
    # if(input$group_by_var == "Plant category") {
    #    if (input$Prod_type == 'Grouped') {
    #      ggplot(simpagg, aes(x = `Plant category`, y = Avg_Cost, fill = `Cost_Type`)) +
    #        geom_bar(stat = "identity", position = position_dodge()) +
    #        labs(x = "Category", y = "Value") +
    #        ggtitle("Side-by-Side Bar Graph of Costs of Production") +
    #        theme_hc()
    #   } else {
    #    ggplot(simpagg, aes(x = `Plant category`, y = `Avg_Cost`, fill = `Cost_Type`)) +
    #      geom_bar(stat = "identity", position = position_stack()) +
    #      labs(x = "Category", y = "Value") +
    #      ggtitle("Stacked Bar Graph of Costs of Production") +
    #      theme_hc() 
    #   }
    #     } else {
    if (input$Prod_type == 'Grouped') {
      ggplot(expagg, aes(x = reorder(`Energy type`, -Avg_Cost, FUN=sum), y = Avg_Cost, fill = `Cost_Type`)) +
        #position determines the shape
        geom_bar(stat = "identity", position = position_dodge()) +
        labs(x = "Power Plant Fuel Type", y = "USD per MWh",
             title = "Total Capital and Fuel Production Costs Grouped",
             subtitle = "Note that the fuel intensive options are especially vulnerable to fluctuating costs",
             fill = "Cost Source") +
        theme_hc() +
        scale_y_continuous(labels = scales::dollar_format(prefix = "$")) +
        scale_x_discrete(labels = expagg_lab) +
        scale_fill_discrete(labels = c("Total Capital Cost per MWh", "Total Fuel Cost per MWh"))
    } else {
      ggplot(expagg, aes(x = reorder(`Energy type`, -Avg_Cost, FUN=sum), y = `Avg_Cost`, fill = `Cost_Type`)) +
        #position determines the shape
        geom_bar(stat = "identity", position = position_stack()) +
        labs(x = "Power Plant Fuel Type", y = "USD per MWh",
             title = "Total Capital and Fuel Production Costs Stacked",
             subtitle = "Visualizing overall costs of production per MWh",
             fill = "Cost Source") +
        theme_hc() +
        scale_y_continuous(labels = scales::dollar_format(prefix = "$")) +
        scale_x_discrete(labels = expagg_lab) +
        scale_fill_discrete(labels = c("Total Capital Cost per MWh", "Total Fuel Cost per MWh"))
      #   }
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
