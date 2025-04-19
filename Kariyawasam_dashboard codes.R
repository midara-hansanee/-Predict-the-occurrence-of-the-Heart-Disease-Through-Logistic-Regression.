# Load required libraries
library(shiny)
library(plotly)
library(ggplot2)
library(readr)
library(dplyr)

# Load the heart disease dataset
heart_data <- read_csv("C:\\Users\\Midara\\Downloads\\Kariyawasam_Midara\\Kariyawasam_Dashboard\\Kariyawasam_dashboard data.csv")
heart_data$target <- as.factor(heart_data$target)

# Define UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .title-wrapper {
        text-align: center;
        font-size: 3em;
        color:#FF0000; /* Red color */ 
        -webkit-text-stroke: 1px black;
        width: 100%;
        height: 100px;
        display: flex;
        justify-content: center;
        align-items: center;
      }
      .background {
        background-image: url('https://d.newsweek.com/en/full/1032264/heart-stock.jpg');
        background-size: cover;
        background-position: center;
        background-repeat: no-repeat;
        background-attachment: fixed;
        height: 100vh; /* Set the background height to cover the entire page */
        width: 100%; /* Set the background width to cover the entire page */
        opacity: 0.9;
      }
      .about-content {
        height: 100%;
      }
      .plot-row {
        height: 100%;
      }
      .plot-tile {
        padding-bottom: 1cm;
      }
      .plotly .plot-container {
        background-color: rgba(255, 255, 255, 0.8) !important;
      }
    "))
  ),
  div(class = "title-wrapper",
      titlePanel("Heart Disease Prediction")),
  mainPanel(
    style = "width: 100%;",
    style = "height=20px;",
    tags$style(".plot-row { margin-bottom: 1cm; }"),
    tags$style(".plot-tile { padding-bottom: 1cm; }"),
    tabsetPanel(
      tabPanel("overview", 
               class = "background",
               align = "center",
               style = "height: 100vh;",
               h2(style = "color: black;-webkit-text-stroke: 1px white;"," Heart Disease "),
               p(style = "font-size: 18px; line-height: 1.5; margin-bottom: 10px; color: white; border: 1px solid white; padding: 10px;", "What is Heart Disease?"),
               p(style = "font-size: 18px; line-height: 1.5; margin-bottom: 10px; color: white; border: 1px solid white; padding: 10px;", "Heart disease refers to a range of conditions that affect the heart's structure and function, often leading to decreased blood flow, chest pain, or heart attacks. Risk factors include high blood pressure, high cholesterol, smoking, and diabetes, making prevention and management crucial for overall cardiovascular health."),
               p(style = "font-size: 18px; line-height: 1.5; margin-bottom: 10px; color: white ; border: 1px solid white; padding: 10px;", "Heart disease stands as a paramount concern in global health, posing significant risks to individuals worldwide."),
               p(style = "font-size: 18px; line-height: 1.5; margin-bottom: 10px; color: white; border: 1px solid white; padding: 10px;", "The onset of heart disease can often be foreseen through various predictive indicators, enabling proactive intervention and management."),
               p(style = "font-size: 18px; line-height: 1.5; margin-bottom: 10px; color: white; border: 1px solid white; padding: 10px;", "A useful technique for predicting the development of heart disease is logistic regression, a statistical technique that is frequently utilized in predictive modeling to evaluate the likelihood of a discrete ending."),
               p(style = "font-size: 18px; line-height: 1.5; margin-bottom: 10px; color: white; border: 1px solid white; padding: 10px;", "In this dashboard, we examine the use of logistic regression in predicting the occurrence of heart disease."),
               p(style = "font-size: 18px; line-height: 1.5; margin-bottom: 10px; color: white; border: 1px solid white; padding: 10px;", "For Data set exploration, visit the UCI Machine Learning Repository"),
               p(style = "font-size: 18px; line-height: 1.5; margin-bottom: 10px; color: white; border: 1px solid white; padding: 10px;", "The 'Heart disease' column in the dataset indicates whether a patient has been diagnosed with heart disease or not. 'Presence' indicates that a patient has the illness, while 'Absence' indicates that a patient does not."),
               p(style = "font-size: 18px; line-height: 1.5; margin-bottom: 10px; color: white; border: 1px solid white; padding: 10px;", "This dashboard endeavors to anticipate heart failure by examining key factors such as age, sex, chest pain type, blood pressure, cholesterol levels, and heart rate."),
               p(style = "font-size: 18px; line-height: 1.5; margin-bottom: 10px; color: white; border: 1px solid white; padding: 10px;", "Let's unravel the diverse components influencing heart health and well-being."),
              
               tags$hr()
               
      ),
      tabPanel("Plots", 
               class = "background",
               h2(style = "color: white;-webkit-text-stroke: 1px black;","Visualizations"),
               fluidRow(
                 style = "height: 40vh;",
                 class = "plot-row",
                 column(width = 4,
                        div(class = "plot-tile",
                            plotlyOutput("plot1", height = "100%", width = "100%"))
                 ),
                 column(width = 4,
                        div(class = "plot-tile",
                            plotlyOutput("plot2", height = "100%", width = "100%"))
                 ),
                 column(width = 4,
                        div(class = "plot-tile",
                            plotlyOutput("plot3", height = "100%", width = "100%"))
                 )
               ),
               fluidRow(
                 style = "height: 40vh;",
                 class = "plot-row",
                 column(width = 4,
                        div(class = "plot-tile",
                            plotlyOutput("plot4", height = "100%", width = "100%"))
                 ),
                 column(width = 4,
                        div(class = "plot-tile",
                            plotlyOutput("plot5", height = "100%", width = "100%"))
                 ),
                 column(width = 4,
                        div(class = "plot-tile",
                            plotlyOutput("plot6", height = "100%", width = "100%"))
                 )
               )
      ),
      tabPanel("Regression Analysis", 
               class = "background",
               h2(style = "color: white;-webkit-text-stroke: 1px black;","Regression Analysis"),
               fluidRow(
                 column(width = 6,
                        plotlyOutput("plot_lm1", height = "500px")
                 ),
                 column(width = 6,
                        verbatimTextOutput("regression_summary1")
                 )
               ),
               tags$hr(),
               fluidRow(
                 column(width = 6,
                        plotlyOutput("regression_plot2", height = "500px")
                 ),
                 column(width = 6,
                        verbatimTextOutput("regression_summary2")
                 )
               ),
               tags$hr(),
               fluidRow(
                 column(width = 6,
                        plotlyOutput("regression_plot3", height = "500px")
                 ),
                 column(width = 6,
                        verbatimTextOutput("regression_summary3")
                 )
               ),
               tags$hr()
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Scatter plot for Age vs. sex with color by target variable
  output$plot1 <- renderPlotly({
    plot_ly(data = heart_data, x = ~age, y = ~slope, type = "bar", mode = "markers",
            marker = list(size = 10, opacity = 1)) %>%
      layout(title = "Type of slope over Ages",
             xaxis = list(title = "Age"),
             yaxis = list(title = "Slope"),
             legend = list(title = "Heart Disease"),
             paper_bgcolor = "rgba(255, 255, 255, 0.5)", # Adjust background color with opacity
             plot_bgcolor = "rgba(255, 255, 255, 1.0)") # Adjust plot area background color with opacity
  })
  
  # Box plot for Oldpeak across Thal categories
  output$plot2 <- renderPlotly({
    plot_ly(data = heart_data, x = ~factor(thal), y = ~oldpeak, type = "box") %>%
      layout(title = "Type of thal according to ST depression",
             xaxis = list(title = "Thal"),
             yaxis = list(title = "Oldpeak"),
             paper_bgcolor = "rgba(255, 255, 255, 0.5)", # Adjust background color with opacity
             plot_bgcolor = "rgba(255, 255, 255, 1.0)") # Adjust plot area background color with opacity
  })
  
  # Pie chart for Gender Distribution
  output$plot3 <- renderPlotly({
    counts <- table(heart_data$sex)
    labels <- c("Female", "Male")
    plot_ly(labels = labels, values = counts, type = "pie") %>%
      layout(title = "Gender Distribution",
             paper_bgcolor = "rgba(255, 255, 255, 0.5)", # Adjust background color with opacity
             plot_bgcolor = "rgba(255, 255, 255, 1.0)") # Adjust plot area background color with opacity
  })
  
  # 3D scatter plot for Thalach, Trestbps, and Chol
  output$plot4 <- renderPlotly({
    plot_ly(data = heart_data, x = ~thalach, y = ~trestbps, z = ~chol, 
            type = "scatter3d", mode = "markers", marker = list(size = 5, opacity = 1)) %>%
      layout(title = "3D Scatter Plot",
             scene = list(xaxis = list(title = "Thalach"),
                          yaxis = list(title = "Trestbps"),
                          zaxis = list(title = "Chol")),
             paper_bgcolor = "rgba(255, 255, 255, 0.5)", # Adjust background color with opacity
             plot_bgcolor = "rgba(255, 255, 255, 1.0)") # Adjust plot area background color with opacity
  })
  
  # Histogram for Max Heart Rate Distribution
  output$plot5 <- renderPlotly({
    plot_ly(data = heart_data, x = ~thalach, type = "histogram") %>%
      layout(title = "Max Heart Rate Distribution",
             xaxis = list(title = "Max Heart Rate"),
             yaxis = list(title = "Frequency"),
             paper_bgcolor = "rgba(255, 255, 255, 0.5)", # Adjust background color with opacity
             plot_bgcolor = "rgba(255, 255, 255, 1.0)") # Adjust plot area background color with opacity
  })
  
  
  # Heatmap for Correlation Matrix
  output$plot6 <- renderPlotly({
    corr_matrix <- cor(select(heart_data, -c(age, sex,target)))
    plot_ly(z = corr_matrix, colorscale = "Viridis", type = "heatmap") %>%
      layout(title = "Correlation Matrix",
             paper_bgcolor = "rgba(255, 255, 255, 0.5)", # Adjust background color with opacity
             plot_bgcolor = "rgba(255, 255, 255, 1.0)") # Adjust plot area background color with opacity
  })
  
  # Add server logic for regression analysis plots and summaries
  # Model 1: 
  output$plot_lm1 <- renderPlotly({
    ggplot(heart_data, aes(x = age, y = trestbps)) + 
      geom_point() + 
      geom_smooth(method = "lm", color = "red")
  })
  
  output$regression_summary1  <- renderPrint({
    lm1 <- lm(age ~trestbps, data = heart_data)
    summary(lm1)
  })
  
  # Model 2: 
  output$regression_plot2 <- renderPlotly({
    ggplot(heart_data, aes(x = age, y = chol)) + 
      geom_point() + 
      geom_smooth(method = "lm", color = "red")
  })
  
  output$regression_summary2<- renderPrint({
    lm2 <- lm(age ~ chol, data = heart_data)
    summary(lm2)
  })
  
  
  
  output$regression_summary3 <- renderPrint({
    model_glm <- glm(target~ age+sex+cp+trestbps+chol+restecg+thalach+thal, 
                     data = heart_data, family = "binomial")
    heart_data$predicted_prob <- predict(model_glm, type = "response")
    summary(model_glm)
  })
  # Model 3: 
  output$regression_plot3<- renderPlotly({
    # Fit logistic regression model
    model_glm <- glm(target ~ age + sex + cp + trestbps + chol + restecg + thalach + thal,
                     data = heart_data, family = "binomial")
    
    # Predict probabilities using the model
    heart_data$predicted_prob <- predict(model_glm, type = "response")
    heart_data %>%
      ggplot(aes(x = thalach, y =predicted_prob,)) +
      geom_point(alpha = 0.15) +
      geom_smooth(method = "glm", method.args = list(family = "binomial")) +
      ggtitle("Logistic Regression Model Fit") +
      xlab("Maximum Heart Rate") +
      ylab("Predicted Probability of Heart Disease")
  })
  
  output$regression_summary3 <- renderPrint({
    model_glm <- glm(target ~ age+sex+cp+trestbps+chol+restecg+thalach+thal,
                     data = heart_data, family = "binomial")
    summary(model_glm)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
