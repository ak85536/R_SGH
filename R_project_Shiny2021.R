# Load packages ------------------

library(devtools)
# install_github('ramnathv/rCharts')
library(rCharts)
library(readr)
library(ggplot2)
library(dplyr)

library(shiny)
library(shinythemes)

library(DBI)
library(plotly)
library(corrplot)
library(scatterplot3d)

library(ggalluvial)
library(ggfittext)

# Load and prepare data ------------------

wages <- read_csv("https://raw.githubusercontent.com/monkeyflwr/R_SGH/main/wages.csv")

wages2 <-wages

wages2$logwage = log(wages2$wage)

wages2 <- wages2 %>% 
  mutate(female = factor(female,
                         levels = c(0, 1),
                         labels = c("male", "female")))

wages2 <- wages2 %>% 
  mutate(black = factor(black,
                        levels = c(0, 1),
                        labels = c("white", "black")))


wages2 <- wages2 %>% 
  mutate(lives_in_city = factor(lives_in_city,
                                levels = c(0, 1),
                                labels = c("No", "Yes")))

wages2 <- wages2 %>% 
  mutate(near_publictrans = factor(near_publictrans,
                                   levels = c(0, 1),
                                   labels = c("No", "Yes")))

wages2 <- wages2 %>% 
  mutate(scholarship = factor(scholarship,
                              levels = c(0, 1),
                              labels = c("No", "Yes")))

wages2 <- wages2 %>% 
  mutate(talentsup = factor(talentsup,
                            levels = c(0, 1),
                            labels = c("No", "Yes")))

wages2 <- wages2  %>% 
  rename(
    Gender = female,
    Color = black
  )

fdata <- wages2 %>%
  count(Gender)


mean_wage <- wages2 %>%
  group_by(Gender) %>%
  summarize(wage = mean(wage))

cdata <- wages2 %>%
  count(Color)


mean_wage_c <- wages2 %>%
  group_by(Color) %>%
  summarize(wage = mean(wage))

wages_table <- wages2 %>%
  group_by(Gender, Color, scholarship, near_publictrans, lives_in_city) %>%
  count()

wages_table$scholarship <- factor(wages_table$scholarship, 
                                  levels = c("Yes", "No"))


wages_table$lives_in_city <- factor(wages_table$lives_in_city, 
                                    levels = c("Yes", "No"))

wages_table$near_publictrans <- factor(wages_table$near_publictrans, 
                                       levels = c("Yes", "No"))

# Define UI ------------------

ui <- fluidPage(theme = shinytheme("lumen"),
                titlePanel("Project on United States wages data prepared by Anastasia Karuzina"),
                mainPanel(
                  tabsetPanel(id = "tabs",
                              tabPanel("1. Introduction",
                                       h2("Introduction"),
                                       div("This report analyses the effect of individual characteristics of 14,320 individual 
                                       in the US on wages. 
                                       
                                       The purpose is to understand which factors mostly
                                       determine the fluctuation of wage dependent variable, to quantify the marginal 
                                       effect and visualize those findings. More specifically the paper intends to 
                                       underline the importance of education, and the effect of an additional year 
                                       of education, while controlling for potential other determinants, by providing 
                                       a 95% confidence interval. 
                                    
                                       Another point raised by the paper is to see if the
                                       data captures significant evidence for wage discrimination due to gender and/or race.", style = "text-align: justify")),
                              
                            
                              
                              tabPanel("2. Histogram",
                                       headerPanel("Check histograms of wage and log of wage:"),
                                       selectInput("Ind","Choose a variable",choices = c("wage", "logwage", "grade_avg", "educ", "iq_score", "ed_mother", "ed_father", "exper" )),
                                       fluidRow(plotOutput("wages_histogram"),
                                       div("We are going to estimate our monthly income by using OLS regression analysis.
                                       We do so because we want to see which variables are significant and important for us, 
                                       therefore we can continue visualize our data with those significant variables.
                                       We want to check whether wage is normally distributed. If not we can generate a natural log 
                                       of wage (we call it logwage) and check its histogram. The histogram produced using R Shiny
                                       package shows that the distribution of wage is not normally distributed, it also has an option 
                                       to pick llogwage, which is a variable we have generated. We can see now normally distributed
                                       log of wage, which we are going to use for our OLS analysis. 
                                       Playing with R Shiny application, we can also check distribution of other variables.", style = "text-align: justify"))),
                              
                              tabPanel("3. Correlation matrix",
                                       fluidRow(plotOutput("Plot1"),
                                       div("Our next step of exploring our data will be to 
                                       see a correlation between variables as it is always good to check multicollinearity
                                       of data before running a regression. For this report, we create a visual representation
                                       of correlation using corrplot package in R. Some interesting findings are following. 
                                       IQ score correlated with wage suggests that intelligence has a positive implication 
                                       on the earnings. Education has positive linear relationship with wage indicating that 
                                       as years of education increasing, we expect that monthly wage increases as well. 
                                       Talent support is positively correlated with education. Education of father has 
                                       moderate positive linear relationship with wage variable of an individual. 
                                       Curious thing: it is higher correlated than education of mother. 
                                       There may be different explanations. For example, having a gender 
                                       wage discrimination, we may assume that father’s years of education 
                                       as well as his gender possibly can have higher salary, meaning that
                                       household where a man brings a significant income can provide more 
                                       schooling years to children. Another curios thing: negative correlation
                                       of experience with education. One of the possible explanations is that more 
                                       years of education an individual has less years she works and vice versa.", style = "text-align: justify"))),
                              
                              
                              tabPanel("4. Regression summary",
                                       h2("Summary:"),
                                       verbatimTextOutput("Summ"), 
                                       div("This table  reports the estimates of the natural
                                       logarithm of monthly wage. From this table, we can conclude that educ is 
                                       statistically significant at 5% significance level and each additional year of education increases the monthly income by 16%. 
                                       We can also see that living near public transport decreases wage by 30%,
                                       each additional point in IQ score of an individual increases her wage by 
                                       only 3%. There are other findings we can see with 5% significance level. 
                                       We can draw our attention at discrimination problems. From the table, we 
                                  can observe that being a woman decreases your wage by 17% while being an African
                                  American decreases your wage only by 3%. Here we can talk about gender
                                  discrimination in USA based on wage data.", style = "text-align: justify")),
                              tabPanel("5. Mosaic 1",
                                       fluidRow(plotOutput("Mosaicplot1")), 
                                       div("From first mosaic plots below we can see that
                                       there are more male white people who live near public transport (this variable
                                       is significant however it almost has no effect on wage according to the 
                                       regression summary)", style = "text-align: justify")),
                              
                              tabPanel("6. Mosaic 2",
                                       fluidRow(plotOutput("Mosaicplot2")), 
                                       div("From the second plot, 
                                       we can see that the majority of people from the data live in the
                                       city and they are also white men. ", style = "text-align: justify")),
                              
                              
                              
                              tabPanel("7. Variables distinguished by gender",
                                       selectInput(inputId = "x",
                                                   label = "Choose X",
                                                   choices = c('educ', 'wage', 'iq_score', 'grade_avg'),
                                                   selected = "wage"),
                                       selectInput(inputId = "y",
                                                   label = "Choose Y",
                                                   choices = c('educ', 'wage', 'iq_score', 'grade_avg'),
                                                   selected = "educ"),
                                       fluidRow(showOutput("Chart1", "polycharts")), 
                                       div("Graph in this tab allows users 
                                       to set different variables on x-axis and on y-axis and observe the gender differences for 
                                       the same variables", style = "text-align: justify")),
                              
                              tabPanel("8. Variables distinguished by scholarship and race",
                                       selectInput(inputId = "x2",
                                                   label = "Choose X",
                                                   choices = c('educ', 'wage', 'iq_score', 'grade_avg'),
                                                   selected = "wage"),
                                       selectInput(inputId = "y2",
                                                   label = "Choose Y",
                                                   choices = c('educ', 'wage', 'iq_score', 'grade_avg'),
                                                   selected = "educ"),
                                       fluidRow(showOutput("Chart2", "polycharts")), 
                                       div("In the regression analysis we can 
                                       see that if an individual had a scholarship in college, her salary is higher by 23% at 5% 
                                       significance level, therefore we can again choose variables on x-axis and y-axis and
                                       analyze how scholarship influences on our graphs. For example, if we pick wage on x-axis
                                       and iq_score on y-axis, we can observe that people with higher IQ score who have 
                                       scholarship has more outliers in terms of bigger salary. 
                                       These graphs are also distinguished by race. Black people who have higher IQ usually have 
                                       scholarships.", style = "text-align: justify")),
                              
                              tabPanel(" 9. Years of education vs. wages",
                                       fluidRow(plotlyOutput("Plotly1"), 
                                       div("This interactive graph shows us how years 
                                       of education relate to wages in the US. In legend we have gender:
                                       male and female. We also can see fitted line. Based on this graph we can conclude that man has to have more 
                                       years of education than woman to have the same salary. We can see it by observing two fitted lines. This graph
                                       created with R Shiny application can be zoomed in, if needed.", style = "text-align: justify"))),
                              
                              tabPanel("10. IQ vs. wages",
                                       fluidRow(plotlyOutput("Plotly2"),
                                       div("Here we can see how monthly income may depend on IQ score of both men and women. Even though the 
                                       regression analysis suggests that IQ score is significant, still each additional point in IQ  test 
                                       score contributes only to 3% of monthly income increase.", style = "text-align: justify"))),
                              
                            
                            
                             
                              
                              tabPanel("11. Years of education by Gender",
                                       fluidRow(plotOutput("Plot2")),
                                       div("Let us pay our attention 
                                       to gender discrimination in US data on wages. As we saw from the regression summary, being a woman
                                       decreases our monthly wage by 17%. We can assume that the reason for that can be a lack of education.
                                       We can draaw a graph to see how number years of education by men differs from number of education 
                                       years by woman. On this graph we can observe  that there is no significant difference. There are a
                                       slightly more men than women in each category by years of education.", style = "text-align: justify")),
                              
                              tabPanel("12. Years of education by skin color",
                                       fluidRow(plotOutput("Plot3")), 
                                       div("This graph suggests that white people usually have more years of education.
                                       This can relate to difference in wages, as we assume people who have more years of education 
                                       have higher salaries. We will explore this argument a bit more . 
                                       However, our regression analysis shows that being a black person only decreases your 
                                       salary by 3%, which is also unjust, but not that much as being a woman.", style = "text-align: justify")),
                              
                              tabPanel("13. Average wage by gender",
                                       fluidRow(plotOutput("Plot4")),
                                       div("Looking at this plot we can conclude 
                                       that men on avergae have much higher wages than women. This proves our discrimination concern again.", style = "text-align: justify")),
                              
                              tabPanel("14. Average wage by race",
                                       fluidRow(plotOutput("Plot5")),
                                       div("This plot suggests that white people have 
                                       on average slighlty higher monthly wages than black people. We can recall that on average white people
                                       have more years of education therefore they can have higher waages on average. 
                                       We can see from the statistics that there is no race discrimination based on US wage data.", style = "text-align: justify")),
                              
                              tabPanel("15. Grade average vs income by race",
                                       fluidRow(plotlyOutput("Plotly3")),
                                       div("This graph explains if there are any  differences on wage depending on grade average at college.
                                       We don't see anything specific on this graph. As the graph is interactive we can zoom in the fitted lines.
                                       But in general we can conclude based on this graph and on the statistics from previous tabs
                                       that there is no race discrimination based on US wage data.", style = "text-align: justify")),
                              
                              tabPanel("16. 3-D Scatterplot: wage, ed_father, educ",
                                       fluidRow(plotOutput("Plot6")), 
                                       div("From the regression statistics we observed that 
                                       that years of father education has some effect on wages. Let's visualize it and see how that is represented by our data.
                                       This cubic scatterplot cannot provide us with visible conclusions.", style = "text-align: justify")),
                              
                              tabPanel("17. 3-D Scatterplot: educ, wage, IQ score",
                                       fluidRow(plotOutput("Plot7")),
                                       div("From this cubic scatterplot we see that more points who have higher wage values,
                                       usually have more than average values in IQ score and years of education", style = "text-align: justify")),
                              
                              tabPanel("18. Importance of living in city",
                                       fluidRow(plotOutput("Plot8")),
                                       div("Lastly, from regression statistics we have variable lives_in_city, which influences a lot on wages.
                                       We can assune that due to living in cities, where prices are higher, wages should be higher as well. Let us see on this graph 
                                       how many people who live in the city are female/male, black/white, lives near public transport or not, and received scholarship at college or not.
                                       Here we can conclude if you are white male living in the city near public transport routes and received a scholarship in college, 
                                       you are better off than other individuals that are included in US wage data.", style = "text-align: justify")),
                              
                              tabPanel("19. Conclusion",
                                       h2("Conclusion"),
                                       div("This report analyses the effect of individual characteristics of 14,320 individual
                                       in the US on wages.

                                       Results of the analysis of this paper based on the above data indicate
                                       a statistically significant impact of years of education on monthly
                                       income of an individual. More precisely, additional year of education
                                       increases one’s monthly wage by 16%. In addition, the report provides
                                       statistical evidence of the lack of race discrimination in the amount
                                       of earnings, but on the other hand an existence of gender discrimination
                                       when it comes to wages per month.", style = "text-align: justify"))

                  )
                )
)
# Define server functions ------------------
server <- function(input, output) {
  
  data1 <- reactive({
    
    input$Ind
  })
  
  output$wages_histogram <- renderPlot({
    req(data1())
    hist(wages2[[data1()]], xlab =input$Ind, main = "Histogram")
  }) 
  
  output$Chart1 <- renderChart({
    names(wages2) = gsub('\\.', '', names(wages2))
    p1 <- rPlot(input$x, input$y, data = wages2, color = "Gender", 
                facet = "Gender", type = 'point') 
    p1$addParams(dom = 'Chart1')
    return(p1)
  })
  
  output$Chart2 <- renderChart({
    names(wages2) = gsub("\\.", "", names(wages2))
    p2 <- rPlot(input$x2, input$y2, data = wages2, color = "Color",
                facet = "scholarship", type = 'point')
    p2$addParams(dom = 'Chart2')
    return(p2)
  })
  
  output$Plotly1 <- renderPlotly({
    ggplotly(
      ggplot(data = wages2,
             mapping = aes(x = wage, 
                           y = educ,
                           color = Gender)) + scale_x_continuous(labels = function(x) format(x, scientific = FALSE)) +
        geom_point(alpha = .7,
                   size = 3) +
        geom_smooth(method = "lm", 
                    se = FALSE, 
                    size = 1.5)  +
        labs(title = "Relationship between education and wage by gender",
             subtitle = "Data of 14,320 individual in the US",
             y = " Years of education",
             x = "Monthly income in US dollars")
    )
  })
  
  output$Plotly2 <- renderPlotly({
    ggplotly(
      ggplot(wages2, aes(x=iq_score, 
                         y=wage, 
                         color=Gender)) + scale_y_continuous(labels = function(y) format(y, scientific = FALSE)) +
        geom_point(size=3) +
        labs(y = "Monthly income",
             x = "IQ Score",
             color = "Gender") + 
        geom_smooth(method = "lm", 
                    se = FALSE, 
                    size = 1.5) + theme_bw()
    )
  })
  
  output$Plot1 <- renderPlot({
    
    a<-data.frame(wages2$wage, wages2$educ, wages2$ed_mother, wages2$iq_score, wages2$ed_father, wages2$exper, wages$lives_in_city, wages$scholarship, wages$talentsup)
    b<-cor(a)
    colnames(b) <- c("wage", "education", "years of ed_mother", "IQ score", "years of ed_father", "experience", "lives in sity", "scholarship", "talent support")
    rownames(b) <- c("wage","education", "years of mother education", "IQ score", "years of father education", "experience", "lives in sity", "scholarship", "talent support")
    col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
    corrplot(b, method = "color", type = "upper", addCoef.col = "blue", tl.srt=90, col=col(200), diag=FALSE)
  })
  
  output$Summ <- renderPrint({
    linearMod <- lm(logwage ~ educ + near_publictrans + 
                      iq_score + grade_avg + lives_in_city + exper + 
                      near_privcollege + scholarship+ talentsup+ near_4ycollege+ 
                      has_siblings + ed_mother + ed_father +has_dog + near_parents + 
                      Gender + Color, data=wages2)
    
    summary(linearMod)
  })
  
  output$Mosaicplot1 <- renderPlot({mosaicplot(~lives_in_city + Gender  + Color, 
                                               data = wages2, color = 4:2, cex.axis = 0.8, type =c("Yes", "No"), 
                                               main = "Living in city by gender and color",
                                               xlab = "Living in the city")})
  
  output$Mosaicplot2 <- renderPlot({mosaicplot(~near_publictrans + Color + Gender ,
                                               data = wages2, color = 4:2, cex.axis = 0.8, type =c("Yes", "No"), 
                                               main = "Living near public transport by color and gender", 
                                               xlab = "Living near public transport")})
  
  output$Plot2 <- renderPlot({
    ggplot(wages2, 
           aes(x = educ, 
               fill = Gender)) + 
      geom_bar(position = "stack") +
      labs(title = "Histogram of years of education per gender",
           x = " Years of education a person has",
           y = "Number of individuals")
    
  })
  
  output$Plot3 <- renderPlot({
    ggplot(wages2, 
           aes(x = educ, 
               fill = Color)) + 
      geom_bar(position = "stack")+
      labs(title = "Histogram of years of education per skin color",
           x = " Years of education a person has",
           y = "Number of individuals")
  })
  
  output$Plot4 <- renderPlot({
    ggplot(mean_wage, 
           aes(x = Gender, 
               y = wage)) +
      geom_bar(stat = "identity") +
      geom_bar(stat = "identity", 
               fill = "cornflowerblue") +
      geom_text(aes(label = wage), 
                vjust = 5) +
      labs(title = "Avergade monthly wage by gender", y = "Monthly wage")
  })
  
  output$Plot5 <- renderPlot({
    ggplot(mean_wage_c, 
           aes(x = Color, 
               y = wage)) +
      geom_bar(stat = "identity") +
      geom_bar(stat = "identity", 
               fill = "cornflowerblue") +
      geom_text(aes(label = wage), 
                vjust = 5) +
      labs(title = "Average monthly wage by race", y = "Monthly wage")
  })
  
  output$Plotly3 <- renderPlotly({
    ggplotly(
      ggplot(wages2, aes(x=grade_avg, 
                         y=wage, 
                         color=Color)) + scale_y_continuous(labels = function(y) format(y, scientific = FALSE)) +
        geom_point(size=3) +
        labs(x = "grade average",
             y = "monthly wage",
             color = "Race") + 
        geom_smooth(method = "lm", 
                    se = FALSE, 
                    size = 1.5) + theme_bw()
    )
  })
  
  output$Plot6 <- renderPlot({
    with(wages2, {
      scatterplot3d(x = wage,
                    y = ed_father, 
                    z = educ,
                    pch=19, 
                    type = "p",
                    highlight.3d = TRUE,
                    main="3-D Scatterplot 1", ylab = "Years of education",
                    xlab = "Monthly income", zlab = "Years of father's education")})
  })
  
  output$Plot7 <- renderPlot({
    with(wages2, {
      scatterplot3d(x = wage,
                    y = educ, 
                    z = iq_score,
                    type = "p",
                    highlight.3d = TRUE, zlab = "IQ Score", xlab ="Monthly income", ylab ="Years of education",
                    pch=19, 
                    main="3-D Scatterplot 2")})
  })
  
  output$Plot8 <- renderPlot({
    ggplot(wages_table,
           aes(
             axis1 = Gender,
             axis2 = Color,
             axis3 = near_publictrans,
             y = n)) +
      geom_alluvium(aes(fill = lives_in_city)) +
      geom_stratum(alpha = 1, width = 1/4) +
      geom_text(stat = "stratum", 
                aes(label = after_stat(stratum))) +
      scale_fill_viridis_d()+
      scale_x_discrete(limits = c("Gender", "Color", "Public Trans"),
                       expand = c(.01, .03)) +
      labs(title = "Wages data",
           subtitle = "Stratified by gedner, color, and scholarship",
           y = "Frequency")  + 
      theme_minimal()
  })
    

  
}

# Create the Shiny app object ------------------

shinyApp(ui = ui, server = server)