library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(rvest)
library(ggmap)
library(tidyr)
library(tm)
library(wordcloud)

data = read.csv("master.csv")
CostIndex = read.csv("CostIndex.csv")

library(shiny)

ui <- fluidPage(
  titlePanel(title = "USA H1B VISA Petition Visualization",
             windowTitle = "US H1B Petition"),
  sidebarLayout(
    sidebarPanel(
      helpText("Input your desired job title"),
      textInput("text", 
                label = "Input should be all lower cases",
                value = "postdoctoral research fellow"),
      helpText("The 'Top10 Company list' tab shows the top 10 companies which have offered most H1B certified sponsorships"),
      helpText("the 'state map' shows number of certified H1B of input job title in each state"),
      helpText("the 'cost and salary index' table shows the expected salary index of input job V.S. the local State's cost Index")
      
    ),
    mainPanel(
      tabsetPanel(type = "tab",
                  tabPanel("Top10_Company_List", tableOutput("table")),
                  tabPanel("state map with certified number", plotOutput("map")),
                  tabPanel("Cost_and_salary_Index", tableOutput("index")))
    )
  )
)

server <- function(input, output, session) {
  test = reactive({
    test = data
    test$containtext = str_detect(test$JOB_TITLE,input$text)
    test = test%>%
      filter(containtext == "TRUE")
    test = test[,-2]
    test
  })
  
  output$table = renderTable({
    y = reactive({
      top10Ctable=test()%>%
        filter(CASE_STATUS == "certified")%>%
        group_by(EMPLOYER_NAME) %>%
        summarise(Average_Wage = mean(PREVAILING_WAGE),number_of_Certified_In_Past_8_Years = n())%>%
        arrange(-number_of_Certified_In_Past_8_Years)
      
      top10Ctable = top10Ctable[1:10,]
      top10Ctable
    })
    y()
  })
  
  output$map = renderPlot({
    mapD = reactive({
      m = test() %>%
        select(CASE_STATUS,WORKSITE_STATE)%>%
        group_by(CASE_STATUS,WORKSITE_STATE) %>%
        summarize(count =n())%>%
        spread(key = CASE_STATUS,value  =count)
      
      state_mapz = map_data("state")
      state_mapz = state_mapz[,-6]
      full_join(state_mapz,m,by = c("region"="WORKSITE_STATE"))
    })
    ggplot(mapD(), aes(x =long, 
                  y= lat,
                  group = group,       
                  fill= certified))+
      geom_polygon( color = "black")+
      theme_void()+
      scale_fill_gradient(low = "white",high = "darkred") +
      coord_map("polyconic")+
      ggtitle("CERTIFIED")+
      theme(legend.title = element_blank())
    
  })
  
  output$index = renderTable({
    x = reactive({
      salary = test() %>%
        select(PREVAILING_WAGE,WORKSITE_STATE)
      
      avg_state_a = salary %>%
        filter(! is.na(PREVAILING_WAGE)) %>%
        group_by(WORKSITE_STATE) %>%
        summarize(avg_state = mean(PREVAILING_WAGE))
      
      avg_overall_a = salary %>%
        filter(! is.na(PREVAILING_WAGE)) %>%
        summarize(avg = mean(PREVAILING_WAGE))
      
      avg_state_a$avg_overall = c(rep(avg_overall_a[1,1],nrow(avg_state_a)))
      avg_state_a$salary_index = avg_state_a$avg_state/avg_state_a$avg_overall*100
      avg_state_a$salary_index = round(avg_state_a$salary_index,1)
      
      combtwo = left_join(avg_state_a,CostIndex,by = c("WORKSITE_STATE" = "State"))
      
      x = combtwo %>%
        select("WORKSITE_STATE","salary_index","Index") %>%
        filter(!is.na(WORKSITE_STATE) & !is.na(salary_index) & !is.na(Index))
      
      colnames(x) = c("WORKSITE_STATE", "Salary_Index","Cost_Index")
      x$Difference_OF_Index = x$Salary_Index - x$Cost_Index
      
      x = arrange(x,-Difference_OF_Index)
    })
    
    x()
  })
  
}

shinyApp(ui, server)