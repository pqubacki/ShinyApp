#Piotr Kubacki summer resit: Dashboard + simulation + regression



#######################################Shiny Dashboard##########################

#install.packages("shiny")
#install.packages("bs4Dash")
#install.packages("echarts4r")
#install.packages("tidyverse")
#install.packages("summarytools")


#load libraries
library(shiny)
library(bs4Dash)
library(echarts4r)
library(tidyverse)
library(summarytools)

#read data
data <- read_tsv("./cars.xls")

#get all variable names
per_name <- data %>% colnames()
#get all character(categorical) variable names
char_vars <- data %>% select_if(is.character) %>% colnames()
#Numerical variables
num_vars <- per_name[!per_name %in% char_vars]



#####################Shiny user interface#######################################
ui <- dashboardPage(
    dashboardHeader(),
    dashboardSidebar(
            minified = FALSE,
            collapsed = FALSE,
            renderer = "svg",
            skin = "light",
            selectizeInput("vax",
                        "Variable for x axis:",
                        choices = per_name,
                        selected = "Price"
                        ),
            selectizeInput("vay",
                        "Variable for y axis:",
                        choices = per_name[per_name != "Price"],
                        selected = "RPM"
                        ),
            checkboxInput("enable_lm",
                          "Linear model",
                          value = FALSE
            ),
            selectizeInput("color",
                        "Variable for color:",
                        choices = char_vars %>% append("None"),
                        selected = "None"
                        )
        ),
    dashboardBody(
      tabsetPanel(type = "tabs",
                  tabPanel("Plot", echarts4rOutput("distPlot", height = 400)),
                  tabPanel("Histograms", 
                           fluidRow(
                             
                           column( width = 3,
                                   echarts4rOutput("origin"),
                                   echarts4rOutput("fuel_tank_capacity"),
                                   echarts4rOutput("cylinders"),
                                   echarts4rOutput("manufacturer"),
                           ),
                           column( width = 3,
                                   echarts4rOutput("price"),
                                   echarts4rOutput("rpm"),
                                   echarts4rOutput("horsepower")
                           ),
                           column( width = 3,
                                   echarts4rOutput("type"),
                                   echarts4rOutput("weight"),
                                   echarts4rOutput("passengers"),
                           ),
                           column( width = 3,
                                   echarts4rOutput("engine_size"),
                                   echarts4rOutput("mpg_city"),
                                   echarts4rOutput("length"),
                           )
                           )
                           ),
                  tabPanel("Summary",
                           h4("Summary statistics of the variables"),
                           verbatimTextOutput("summary"))
                  #render descriptive statistics for vax vay only!
    )
    )


      
)

#########################Shiny server###########################################
server <- function(input, output, session) {
  
  observeEvent(input$vax, {
    updateSelectizeInput(session, "vay",
                         choices = per_name[per_name != input$vax],
                         selected = input$vay)
  })
  
      output$distPlot <- renderEcharts4r({
    if(input$vax %in% char_vars && input$vay %in% num_vars){
        data %>% 
          rename(x = sym(input$vax)) %>% 
          rename(y = sym(input$vay)) %>%
          group_by(x) %>% 
          e_charts() %>% 
          e_boxplot(y, legend = NULL, name = input$vay) %>% 
          e_tooltip(trigger = "axis")
    }else if(input$vax %in% num_vars && input$vay %in% num_vars){
      if(input$color=="None"){
      data %>% 
        rename(x = sym(input$vax)) %>% 
        rename(y = sym(input$vay)) %>% 
        e_charts(x) %>% 
        e_scatter(y, symbol_size = 10, legend = NULL, name = input$vay) %>% 
        e_tooltip()
      }else {
      data %>% 
        rename(x = sym(input$vax)) %>% 
        rename(y = sym(input$vay)) %>%
        rename(z = sym(input$color)) %>%
        group_by(z) %>% 
        e_charts(x) %>% 
        e_scatter(y, symbol_size = 10) %>% 
        e_tooltip()
      }
    }else if(input$vax %in% char_vars && input$vay %in% char_vars){
      data %>% 
        rename(x = sym(input$vax)) %>% 
        rename(y = sym(input$vay)) %>%
        count(x, y) %>%
        group_by(y) %>% 
        e_charts(x,  legend=NULL) %>% 
        e_bar(n, stack="grp") %>% 
        e_tooltip()
    }else if(input$vax %in% num_vars && input$vay %in% char_vars){
      data %>% 
        rename(x = sym(input$vay)) %>% 
        rename(y = sym(input$vax)) %>%
        group_by(x) %>% 
        e_charts() %>% 
        e_boxplot(y, legend = NULL, name = input$vax) %>% 
        e_tooltip(trigger = "axis")
    }
        
      })
    observeEvent(input$enable_lm, {
      if(input$enable_lm){
        
      if(input$vax %in% num_vars && input$vay %in% num_vars) {
      data %>%
          rename(x = sym(input$vax)) %>%
          rename(y = sym(input$vay)) %>%
      echarts4rProxy("distPlot", data = ., x = x) |>  # create a proxy
        e_lm(y~x, name = "lm") |>
        e_execute()
      } else {
        showNotification("Please, choose continous variables", type = "error")
        updateCheckboxInput(session, "enable_lm", value = FALSE)
      }
      }else{
        if(input$vax %in% num_vars && input$vay %in% num_vars){
          echarts4rProxy("distPlot") |>
            e_remove_serie(serie_name = "lm")
        }
      }

    })
    
    observeEvent(input$color, {
      if(input$vax %in% char_vars || input$vay %in% char_vars) {
        if(input$color != "None"){
        showNotification("Please, choose continous variables", type = "error")
        updateSelectizeInput(session, "color", selected = "None")
        }
      } 
    })
    
    
    output$origin <- renderEcharts4r({
        data %>% 
        count(Origin) %>% 
        e_charts(Origin) %>% 
        e_bar(n, legend = NULL) %>% 
        e_title("Origin") %>% 
        e_tooltip()
      })
    output$type <- renderEcharts4r({
        data %>% 
        count(Type) %>% 
        e_charts(Type) %>% 
        e_bar(n, legend = NULL) %>% 
        e_title("Type") %>% 
        e_tooltip()
      })
    output$manufacturer <- renderEcharts4r({
        data %>% 
        count(Manufacturer) %>% 
        e_charts(Manufacturer) %>% 
        e_bar(n, legend = NULL) %>% 
        e_title("Manufacturer") %>% 
        e_tooltip()
      })
    
    output$price <- renderEcharts4r({
      data %>% 
        e_charts() %>% 
        e_histogram(Price) %>% 
        e_title("Price") %>% 
        e_tooltip()
      })
    output$engine_size <- renderEcharts4r({
      data %>% 
        e_charts() %>% 
        e_histogram(EngineSize) %>% 
        e_title("EngineSize") %>% 
        e_tooltip()
      })
    output$fuel_tank_capacity <- renderEcharts4r({
      data %>% 
        e_charts() %>% 
        e_histogram(Fuel.tank.capacity) %>% 
        e_title("Fuel.tank.capacity") %>% 
        e_tooltip()
      })
    output$rpm <- renderEcharts4r({
      data %>% 
        e_charts() %>% 
        e_histogram(RPM) %>% 
        e_title("RPM") %>% 
        e_tooltip()
      })
    output$weight <- renderEcharts4r({
      data %>% 
        e_charts() %>% 
        e_histogram(Weight) %>% 
        e_title("Weight") %>% 
        e_tooltip()
      })
    output$mpg_city <- renderEcharts4r({
      data %>% 
        e_charts() %>% 
        e_histogram(MPG.city) %>% 
        e_title("MPG.city") %>% 
        e_tooltip()
      })
    output$cylinders <- renderEcharts4r({
      data %>% 
        e_charts() %>% 
        e_histogram(Cylinders) %>% 
        e_title("Cylinders") %>% 
        e_tooltip()
      })
    output$horsepower <- renderEcharts4r({
      data %>% 
        e_charts() %>% 
        e_histogram(Horsepower) %>% 
        e_title("Horsepower") %>% 
        e_tooltip()
      })
    output$passengers <- renderEcharts4r({
      data %>% 
        e_charts() %>% 
        e_histogram(Passengers) %>% 
        e_title("Passengers") %>% 
        e_tooltip()
      })
    output$length <- renderEcharts4r({
      data %>% 
        e_charts() %>% 
        e_histogram(Length) %>% 
        e_title("Length") %>% 
        e_tooltip()
      })
    
    output$summary <- renderPrint({
        t(summary(data[,c(input$vax, input$vay)]))
            })
    
    
}

######################## Regression  ###########################################
#Consider two linear models:

#one where we consider y as price and x as horsepower;
model1 <- lm(Price~Horsepower, data)

#and a second with price as dependent variable and both, horsepower and fuel consumption, as independent.  
model2 <- lm(Price~Horsepower+EngineSize, data)

summary(model1)
summary(model2)



#call for app
shinyApp(ui = ui, server = server)




