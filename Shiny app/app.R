
library(shinythemes)
library(ggplot2)
library(tidyverse)
library(xgboost)
library(gt)
library(shiny)

### PRE APP SET UP

#load model
model <- xgb.load('model.R')
getwd()

#load data set
df <- read_csv("FinalDataset_ratings.csv")

#define round up function
roundUp <- function(x,to=10)
{
    to*(x%/%to + as.logical(x%%to))
}

#add ballpark variables
df$chulls_ballpark <- roundUp(df$chulls)
df$ytg_ballpark <- ifelse(df$yardsToGo <= 5, 
                          "short",
                          ifelse(df$yardsToGo <= 10, 
                                 "med",
                                 ifelse(df$yardsToGo <= 15, 
                                        "long", 
                                        "extra long")))
#pass classifications
df$pass_class <- ifelse(is.na(df$pass_length), 
                        "",
                        ifelse(df$pass_location == "middle", 
                               paste("pass", df$pass_length, df$pass_location),
                               paste("pass", df$pass_length, "side")))

#run classifications
df$run_class <- ifelse(is.na(df$run_location), 
                       "",
                       ifelse(df$run_location == "middle",
                              "run middle", 
                              "run side"))

#combine classifications
df$play_class <- paste(df$run_class, df$pass_class, sep = "")

#filter out "" classif -- for odd plays
df <- df %>% filter(play_class != "") 



## START OF THE APP

ui <- tagList(
    ## UPPER NAVBAR SETUP
    navbarPage(
        title = "VIRGIL CARTER",
        theme = shinytheme("flatly"),
        
        # ## HOME TAB
        # tabPanel("Home",
        #          sidebarPanel(
        #              "SIDEBAR"
        #          ),
        #          mainPanel(
        #                    h1("VIRGIL CARTER")
        #                   )
        # ),

        
        ## MODEL PREDICTIONS TAB
        
        tabPanel("Model Predictions",
             sidebarPanel(
                ## INPUTS FOR THE MODEL
                 
                #down input
                selectInput(inputId = "down",
                             label = "Choose Down",
                             selected = 1,
                             choices = c(1, 2, 3, 4)),  #numericInput end
                #distance (yards to go) input
                numericInput(inputId = "ytg",
                             label = "Enter Yards To Go",
                             value = 10,
                             min = 1,
                             max = 50),  #numericInput end
                #defendersInTheBox input
                numericInput(inputId = "def_in_box",
                             label = "Enter Number of Defenders In The Box",
                             value = 6,
                             min = 1,
                             max = 11),  #numericInput end
                #safety depth input
                numericInput(inputId = "saf_dep",
                             label = "Enter the Average Safety Depth",
                             value = 11.5,
                             min = 0,
                             max = 60),  #numericInput end
                #corner depth input
                numericInput(inputId = "cor_dep",
                             label = "Enter the Average Corner Depth",
                             value = 4.5,
                             min = 0,
                             max = 60),  #numericInput end
                #half seconds remaining input
                numericInput(inputId = "half_sec_rem",
                             label = "Enter Total Seconds Remining in the Half",
                             value = 1800,
                             min = 0,
                             max = 1800),  #numericInput end
                
                #chull_ratio input
                numericInput(inputId = "chull_rat",
                             label = "Enter Chull Ratio",
                             value = .35,
                             min = 0,
                             max = 5),  #numericInput end
                #chull input
                numericInput(inputId = "chull",
                             label = "Enter Chull",
                             value = 120,
                             min = 0,
                             max = 100),  #numericInput end
                
                submitButton("Update")
             ),  #sidebarPanel end 
             
             mainPanel(
                 h3("Offensive Playcall Prediction", align = "center"),
                 br(),
                 gt_output("model_pred"),
                 br(),
                 br(),
                 br(),
                 gt_output("history_props"),
                 br(),
                 br(),
                 br()
             )  #mainPanel end
        ),  #tabPanel end
        
        ## OUTLIER PLAYS TAB
        
        tabPanel("Outlier Plays",
            sidebarPanel(
                selectInput(inputId = "outlier_down",
                            label = "Enter Down",
                            selected = 1,
                            choices = c(1, 2, 3, 4)),
                selectInput(inputId = "outlier_distance", 
                            label = "Enter Yards to Go",
                            selected = "6-10", 
                            choices = c("0-5", "6-10", "11-15", "16+")),
                selectInput(inputId = "outlier_team",
                          label = "Enter Team Abbreviaton",
                          selected = "All Teams",
                          choices = df$posteam %>% unique() %>% str_sort() %>% prepend("All Teams")),
                submitButton("Update"),
                br(),
                br(),
            ),#sidebarPanel end
            mainPanel(
                h3("Team Tendencies", align = "center"),
                br(),
                gt_output("outlier_situation"),
                br(),
                br(),
                br()
            ), #mainPanel end
        )#tabPanel end
        
        
        # ## MORE ABOUT THE PROJECT TAB
        # 
        # tabPanel("More About the Project",
        #          mainPanel(
        #              tabsetPanel(
        #                  tabPanel("Project Concepts",
        #                           mainPanel(
        #                               h3("Can a defense impact an offensive play call?"),
        #                               br(),  #br() is just added whitespace
        #                               br(),
        #                               br(),
        #                               h3("How much impact does a defensive formation have on an offensive play call?"),
        #                               br(),
        #                               br(),
        #                               br(),
        #                               h3("Can a defensive formation impact offensive decision making?")
        #                           ) # mainPanel end
        #                  ),  #tabPanel end
        #                  tabPanel("Variable Explanation",
        #                           sidebarPanel(
        #                               "ENTER SIDEBAR INFORMATION"
        #                           ),  #sidebarPanel end
        #                           mainPanel(
        #                               h3("Convex Hull:"),
        #                               h5("Explanation of Variable"),
        #                               h5("Graph explanation"),
        #                               br(),
        #                               br(),
        #                               br(),
        #                               h3("Distance Between WR and CB"),
        #                               h5("Explanation of Variable"),
        #                               h5("Graph explanation"),
        #                               br(),
        #                               br(),
        #                               br(),
        #                               h3("Add Other Variables...")
        #                           )  # mainPanel end
        #                  )  #tabPanel end
        #              ) #tabsetPanel end
        #          )  #mainPanel end
        # )  #tabPanel end
    )#navbarPage end
)#tagList end

# Define server logic required to draw a histogram
server <- function(input, output) {

    ## Project Concepts Tab
    
    
    ## Variable Explanation
    
    
    ## Model Predictions Tab
    output$model_pred <- render_gt({
        
        #find correct down
        if (input$down == 1){
            down_1 = 1
            down_2 = 0
            down_3 = 0
            down_4 = 0
        }
        else if (input$down == 2){
            down_1 = 0
            down_2 = 1
            down_3 = 0
            down_4 = 0
        }
        else if (input$down == 3) {
            down_1 = 0
            down_2 = 0
            down_3 = 1
            down_4 = 0
        }
        else{
            down_1 = 0
            down_2 = 0
            down_3 = 0
            down_4 = 1
        }
        
        
        #get play to predict from user input
        play_to_predict <- data.frame(defendersInTheBox = input$def_in_box,
                                      numDL = 4,
                                      numLB = 3,
                                      numDB = 4,
                                      safety_depth = input$saf_dep,
                                      avg_corner_depth = input$cor_dep,
                                      chulls = input$chull,
                                      chulls_ratio = input$chull_rat,
                                      defRank = 16,
                                      isBlitz = 0,
                                      totalSacksGame = 3,
                                      totalIntGame = 0,
                                      avgPassRushers = 5,
                                      totalPassBlitzGame = 0,
                                      half_seconds_remaining = input$half_sec_rem,
                                      yardsToGo = input$ytg,
                                      first_down = down_1,
                                      second_down = down_2,
                                      third_down = down_3,
                                      fourth_down = down_4
        )
        #get prediction
        prediction <- data.frame(predict(model,
                                         as.matrix(play_to_predict),
                                         reshape = T))
        #change col names
        colnames(prediction) <- c("Pass Deep Side", "Pass Deep Middle", "Pass Short Side", "Pass Short Middle", "Run Middle", "Run Side")
        
        #output table
        prediction %>% 
            mutate_if(is.numeric, round, digits = 2) %>%
            gt() %>% 
            tab_header(#title = "Offensive Playcall Prediction", 
                                   title = "Predicted Probability") %>%
            cols_align(align = "center")
        
            })
    output$history_props <- render_gt({
        #get info from input
        ytg_class <- ifelse(input$ytg <= 5, 
                            "short",
                            ifelse(input$ytg <= 10, 
                                   "med",
                                   ifelse(input$ytg <= 15, 
                                          "long", 
                                          "extra long")))
        chls <- roundUp(input$chull)
        
        #manipulate data
        agg <- df %>% filter(down.x == as.integer(input$down)) %>%
                      filter(ytg_ballpark == ytg_class) %>%
                      filter(chulls_ballpark == chls) %>% 
                      group_by(play_class) %>%
                      summarize(count = n())
        total <- sum(agg$count)
        agg$prop <- agg$count / total
        
        agg <- agg %>% t() %>% data.frame() %>% janitor::row_to_names(row_number = 1)
        
        agg <- cbind(data.frame("Category" = c("Count", "Proportion")), agg)
        
        agg %>% mutate_if(is.numeric, round, digits = 2) %>%
            gt() %>%
            tab_header(title = "Historical Proportions") %>%
            cols_align(align = "center")
    })
    
    ## OUTLIER PLAYS TAB
    
    output$outlier_situation <- render_gt({
        
        #get correct yards to go
        if (input$outlier_distance == "0-5"){
            data <- df %>% filter(ydstogo <= 5)
        }
        else if (input$outlier_distance == "16+"){
            data <- df %>% filter(ydstogo >= 16)
        }
        else if (input$outlier_distance == "11-15")
        {
            data <- df %>% filter((ydstogo >= 11) & (ydstogo <= 15))
        }
        else
        {
            data <- df %>% filter((ydstogo >= 6) & (ydstogo <= 10))
        }
        #get right team(s)
        if (input$outlier_team != "All Teams"){
            data <- data %>% filter(posteam == input$outlier_team )
        }
        #get right down and group/summarize data
        table <- data %>% filter(down.x == input$outlier_down) %>%
                            group_by(posteam) %>%
                            summarize(PropPass = round(mean(pass), 2),
                                      Total = n(),
                                      NumPasses = round(PropPass * n()),
                                      NumRuns = Total - NumPasses) %>%
                            arrange(desc(PropPass))
        
        table %>% gt() %>%
            cols_label(PropPass = "Proportion Pass", 
                       posteam = "Team", 
                       Total = "Total Plays", 
                       NumPasses = "Number Pass Plays", 
                       NumRuns = "Number Run Plays")  %>%
            data_color(columns = "PropPass", 
                       colors = scales::col_numeric(palette = "RdBu", 
                                                    domain = NULL)) %>%
            tab_style(style = list(cell_text(weight = "bold"), 
                                   cell_borders(sides = c("left", "right"),
                                                color = "gray70",
                                                weight = px(2))),
                      locations = cells_column_labels(columns = c("PropPass", "posteam", "Total", "NumPasses", "NumRuns"))) %>%
            cols_align(align = "center") 
        
            
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
