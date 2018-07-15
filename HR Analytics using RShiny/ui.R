library(shiny)
library(shinydashboard)
library(plotly)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(lattice)
library(DT)
library(caTools)
library(highcharter)
library(pROC)
library(ROCR)
library(e1071)
library(gplots)

shinyUI(
  dashboardPage(
            title ="Human Resource Analytics Dashboard", skin="blue",
                  dashboardHeader(
                                   title = h3("HR Analytics")
                                  ),
                
  dashboardSidebar( width="350px",
                    sidebarMenu(
                                   
                                   br(),
                                   menuItem("About Project:",tabName = "about",icon=icon("fa fa-info-circle")),
                                   menuItem("Dashboard",tabName = "Dashboard",icon=icon("home",lib="glyphicon")),
                                   menuItem("Resource Hiring Analytics",tabName = "Hiring",icon=icon("fire",lib="glyphicon")),
                                   menuItem("Exploratory Data Analysis for Attrition of Employees",tabName = "Attritionusingeda",icon = icon("fa fa-flag-checkered")),
                                   menuItem("Does people leaving after certain time?",tabName = "certaintimeleft",icon = icon("fa fa-hourglass-half")),
                                   menuItem("Does low satisfaction level making employees leave?",tabName = "lowsatisfy",icon = icon("fa fa-exclamation-triangle")),
                                   menuItem("Which Department Have High Attrition?",tabName = "departmentattrition",icon = icon("fa fa-line-chart")),
                                   menuItem("Predict how many employees will leave company?",tabName = "predictiveanalysis",icon = icon("fa fa-search-plus")),
                                   menuItem("Predict Who will leave??",tabName = "predictwholeave",icon = icon("fa fa-bookmark")),
                                   menuItem("GenderPay Gap Analysis",tabName = "Genderpaygap",icon = icon("fa fa-users")),
                                   menuItem("Salary Analytics",tabName = "salaryanalytics",icon = icon("fa fa-money")),
                                   menuItem("Sentimental Analysis:Employee reviews to company",tabName = "Review_Analysis",icon = icon("fa fa-comments"))
                                 )
                  ),        
                                   
                                  
  dashboardBody(
          tabItems
                (
#About project
                  tabItem( 
                    tabName = "about",
                    h2("Purpose Of The Project : HR Analytics"),
                    h4("The main objective of this research is to predict the how many employees will leave company prematurely by focusing on different factors which are affecting the employee satisfaction levels , lowering his performance in the company. As well as to predict which employee will leave  first by observing and evaluating certain factors."),
                    br(),
                    h2("Specific objectives:"),
                    h4(".	The second main objective of this research project is to perform sentimental analysis to understand the reviews and comments of the employee about company which can help HR to determine different strategies to improve satisfaction among employees."),
                    h4(".	To Determine how much is past attrition and influence of different factors on the company attrition."),
                    h4(".	To perform gender pay gap analysis to know is there any gap of wages/salary between the female and male  employees in company, which can lead to female employee attrition."),
                    h4(".	To perform Salary analytics to analyze salary of employees to know the upcoming salary hikes, as well as to know the salary satisfaction and salary classes among the employees."),
                    h4(".	To analyze which department of the company is on high attrition of employees."),
                    h4(".	To perform analysis on resource hiring sources to find out which hiring resources are giving the best performing employees to our company.")
                  
                  
                  ),
                  #-====================================================================================
  #Main dashboard tab                
                  tabItem( 
                        tabName = "Dashboard",
                                    h2("Welcome To Human Resource Analytics Dashboard..!!"),
                                    fluidRow(
                                    infoBox(h6("Number of Employees"),15000,icon=icon("fa fa-users")),
                                    infoBox(h6("Number of Projects"),168,icon=icon("fa fa-cubes")),
                                    infoBox(h6("Number of Department"),10,icon=icon("fa fa-building")),
                                    br(),
                                    br(),
                                    br()
                                         ),
                                    fluidRow(
                                    h3("Departmentwise Company Employees") ,
                                    plotlyOutput("Plotofemployee",width = "1350px")
                                             ) 
                      
                      
                      ),
  #-====================================================================================
                  
  #Resource Hiring Analytics tab
                    
                tabItem( 
                      tabName = "Hiring",h3("Resource Hiring Analytics") ,
                        fluidPage(
                        selectInput("EmployementSource","Select and see hirings by employement sources:",recruiting_source$`Employment Source`)
                        
                                  ),
                        mainPanel(
                                    tabsetPanel(type="tab",
                                    tabPanel("Data:",tableOutput("resource"))
                                                )
                                  ),
                                      br(),
                                      br(),
                                      br(),
                                      br(),
                                      br(),
                                      br(),
                                      br(),
                                      br(),
                                      br(),
                                      br(),
                                      br(),
                                      br(), 
                      fluidRow(
                      h3("Lets see which employement source is giving best employees?"),
                      plotlyOutput("recruitmentwiseperformance",width = "1300px"),
                      h3("By looking above graph, We can see that employees hired by using Employee Referral are the best employees.Performance of this employees is fully meet.")
                             )
                          ),
 # =================================================================================================     
 # Exploratory Data Analysis for Attrition of Employees
              tabItem(
                    tabName = "Attritionusingeda",
                      fluidRow(
                      h2("Let us look at each variable using EDA and see its influence on the Attrition of the organization"),
                      plotlyOutput("Attrition1",width = "1300px"),
                      mainPanel(                     
                               )
                          ),
                      h4("Percentage of Attrition"),tableOutput("Attrition"),
                      h4("If we look at the dataset the minority is Attrition - Yes cases ( 24%)."),     
                      br(),
                      fluidRow(
                      h2("Let us look at each variable and see its influence on the Attrition of the organization"),
                      box(title="Age:majority of employees leaving the org are around 30 Years",status="primary",solidHeader=T,plotlyOutput("Ageplot")),
                      box(title="Business Travel Attrition",status="primary",solidHeader=T,plotlyOutput("BusinessTravel")),
                      box(title="Departmentwise Attrition",status="primary",solidHeader=T,plotlyOutput("Department")),
                      box(title="Gender: We see that majority of separated employees are Male and the reason might be because around 61% of employees in our dataset are Male.",status="primary",solidHeader=T,plotlyOutput("gender")),
                      box(title="Satisfaction level.We see higher attrition levels in among lower Job Satisfaction levels.:0.10",status="primary",solidHeader=T,plotlyOutput("jobsatisfaction")),
                      box(title="Marital Status:Attrition is higher on Married employees.",status="primary",solidHeader=T,plotlyOutput("marrigestatus"))
                               ),
                   
                    
                      fluidRow(
                      box(title="Number of Companies Worked: We see a clear indication that many people who have worked only in One company before quit a lot.",status="primary",solidHeader=T,plotlyOutput("numberofcompnyworked")),
                      box(title="Performance Rating: 1 'Low' 2 'Good' 3 'Excellent' 4 'Outstanding'. We see that we have employees of only 3 and 4 ratings. Lesser proportion of 4 raters quit.",status="primary",solidHeader=T,plotlyOutput("performancerate")),
                      box(title="Years at Company: Larger proportion of new comers are quitting the organization. Which sidelines the recruitment efforts of the organization.",status="primary",solidHeader=T,plotlyOutput("yearatcompany")),
                      box(title="Years In Current Role: Plot shows a larger proportion with just 0 years quitting. May be a role change is a trigger for Quitting.",status="primary",solidHeader=T,plotlyOutput("yearatcurrentrole")),
                      box(title="Years Since Last Promotion: Larger proportion of people who have been promoted many years ago have quit the organization.",status="primary",solidHeader=T,plotlyOutput("yearsincelastpromotion")),
                      box(title="Years With Current Manager: As expected a new Manager is a big cause for quitting.",status="primary",solidHeader=T,plotlyOutput("yearwithcurrentmanager"))
                      
                           ),
                    
                    fluidRow(
                    box(title="Work Life Balance:",status="primary",solidHeader=T,plotlyOutput("worklifebal")),
                    box(title="Relationship Satisfaction: 1 'Low' 2 'Medium' 3 'High' 4 'Very High'. Larger proportions of 1 & 2 rating are quitting.",status="primary",solidHeader=T,plotlyOutput("relationsatisfy"))
                            ),
                    
                    fluidRow(
                    box(title="Number of Projects",status="primary",solidHeader=T,plotlyOutput("numproj")),
                    box(title="Average monthly hours",status="primary",solidHeader=T,plotlyOutput("monthlyhr")) 
                             ),       
   
                    h2("Lets check correlation betweeen variables using Exploratory Data Analysis (EDA)"),
                    fluidRow(
                      plotOutput("corr",height = "600px",width = "800px"),
                      h4("Correlation Matrix uses to predict correlation between numeric varibales.These are highly correlated variables:"),
                      h5("1. Avg_monthly_hour is correlated with Number of peojects"),
                      h5("2. Years at company correlated with years since last promotion"),
                      h5("3. Years at company correlated with years with current manager"),
                      h5("4. years with current manager correlated with Years with current role"),
                      h5("5. Years at company correlated with years since last promotion"),
                      h5("6. Years with current role correlated with Years at company"),
                      mainPanel(
                                 ) 
                      
                             )
                 ),  
#  ===================================
# Does people leaving after certain time?                      

             tabItem(
                  tabName = "certaintimeleft",
                      fluidRow(  
                      h2("Is there a certain time period after which employees change the company?"),
                      highchartOutput("Plotoftimeofleave",width = "1300px"),
                      h4("Most of the employees who have left company have spent three years in company so it might possible that they have considered three years as enough time span."),
                      br(),
                      h3("lets see count of departmentwise years at company of employee count"),
                      plotlyOutput("Whosetimeis3year",width = "1300px"),
                      h3("Lets count how many employees spent 3 years at company so that it will be high probabilty that this people can leave if we see the above history"),
                      tableOutput("yearsatcomapnycount")
                              )
                    ),      
                    
 #==============================================================================================================================                 
 # Does low satisfaction level making employees leave?  
            tabItem(
                 tabName = "lowsatisfy",
                    h2("Does low Average satisfaction level making employees leave the company?"),
                    plotlyOutput("Plotofsatisfaction",width = "1300px"),
                    br(),
                    h4("As per above graph"),
                    h4("So It is clearly visible that majority people who left company were having satisfaction level less then 0.5. so low Satisfaction level might be a reason to leave company."),
                    br(),
                    h4("How last evaluation is related?"),
                    plotlyOutput("plotofLastEvaulation",width = "1300px"),
                    h4("It might possible that last evaluation is having impact on satisfaction level , if so then we can say that last evaluation is indirectly impacting left or not left.")
                     ),
 #========================================================================================================================
 #Which Department Have High Attrition?               
              tabItem(
                    tabName = "departmentattrition",
                        fluidRow(
                        h2("Is there any issues in particular Department?"),
                        highchartOutput("Plotofdeptprob",width = "1300px"),
                        h4("Most of the people who have left company are from sales department so it might possible that sales department employee are having some issue.")
                                ),
                      
                        fluidRow(
                        h2("Departmentwise Total Projects"),
                        plotlyOutput("departmentproject") ,
                        h4("We can see HR department having more projects than other department.This may be the reason for employee attrition of HR department")
                                ),
                        h4("Lets dive a little deeper in and verify if this is true."),
                        tableOutput("deep"),
                        h4("From above results it is clear that sales department has more people so the left employee count is more and we can not compare departments as it is,"),
                        h4("We can compare them by taking a ratio of the number of people who left and the number of people in each department."),
                        tableOutput("deep1"),
                        h4("Now it is clear that our initial analysis was wrong, The rate of attrition in HR department is high")
                      
                    ),
                   
 #================== ===========================================================================================================
 #Predict how many employees will leave company?
             tabItem(
                  tabName = "predictiveanalysis",
                      h2("Predictive Analysis using CART (Classification and Regression Tree)"),
                      h4("It works for both continuous and classification prediction and The complexity parameter (cp) is used to control the size of the decision tree and to select the optimal tree size. "),
                      h4("It is a way that can be used to show the probability of being in any hierarchical group. "),
                      br(),
                      h4("Classification tree:"),
                      h4("rpart(formula = left ~ ., data = train, method = class, minbucket = 25)"),
                      h4("Variables actually used in tree construction:"),
                      h4("1.average_montly_hours","2.last_evaluation","3.number_project","4.satisfaction_level","5.time_spend_company "),
                      tableOutput("CART"),
                      h4("Decision tree "),
                      plotOutput("decisiontree",height = "700px"),
                      h4("lets calculate Accuracy for CART model :"),
                      tableOutput("tablepred"),
                      h4(" % Accuracy of our prediction using CART is : "),
                      tableOutput("tablepredpercent"),
                      br(),
                      h4("Now lets try another algorithm"),
                      h2("Predictive analysis using Logistic regression"),
                      h4("Logistic Regression is a classification algorithm. It is used to predict a binary outcome (1 / 0, Yes / No, True / False) given a set of independent variables. "),
                      h4("You can also think of logistic regression as a special case of linear regression when the outcome variable is categorical, where we are using log of odds as dependent variable."),
                      h4(" In simple words, it predicts the probability of occurrence of an event by fitting data to a logit function."),
                      br(),
                      h3("Summary of model:"),
                      verbatimTextOutput("logistictable1"),
                      h4("Now we predict using our test data set and then we will compare predicted Vs actual."),
                      h3("Prediction Table:"),
                      tableOutput("logistic2"),
                      h4("% Accuracy of our prediction using logistic regression"),
                      textOutput("logistic3"),
                      br(),
                      h4("Compression between Logistic and CART model"),
                      plotOutput("roc"),
                      h4("Accurancy of Logistic Regression Model Is:"),
                      textOutput("accuracyoflogistic"),
                      h4("Accurancy of CART  Model Is:"),
                      textOutput("accuracyofcart"),
                      h4("As we can see, Accuracy as well as Area under the curve is more for CART(classification and regression tree) is more than logistic regression.")
                      
                    ),
# ===================================================================================================
#Predict Who will leave??
            tabItem(
                tabName = "predictwholeave",
                     h2("Lets see with graph and datatable Which employee will leave the company??"),
                     br(),
                     h3("plot that show the probability to leave of the employees and their performance. We need to focus on the top right."),
                     plotlyOutput("predictwhowilleave",width = "1300px"),
                     br(),
                     h3("we build a data table were we rank the probability to leave found in the logistic regression model and the performance, we therefore find the priority for the company."),
                     DT::dataTableOutput("dataofleavingprobability"),
                     br(),
                     h4("Here we display the first 300 employees that the company should retain. After grouping them per department we could email the different managers to tell them which valuable employees might leave soon.")
                  ),
                  
 #==================================================================================================
 #GenderPay Gap Analysis
            tabItem(
                tabName = "Genderpaygap",
                    h2("Is there a significant difference in income between men and women?"),
                    br(),
                    h3("Total male and females in the company:"),
                    tableOutput("totalmalefemale"),
                    h3("Departmentwise female and male count:"),
                    plotlyOutput("totalfemalemale",width = '1300px'),
                    br(),
                    h2("Genderwise Salary Classes:"),
                    plotlyOutput("Genderwisesalary",width = '1300px'),
                    br(),
                    h2("Mean of salary of male and female in company"),
                    tableOutput("meansalgenderwise"),
                    h3("Here we can easily see that, there is difference in income of men and women as men getting paid with high salary.")
                    
                  ),
#===========================================================================================================================                  
#Salary Analytics               
         tabItem(
              tabName = "salaryanalytics",
                  fluidRow(
                  h2("Is salary is having any direct impact?"),
                  highchartOutput("Plotofsalempact",width = "1300px"),
                  h4("above pie chart indicates"),
                  h4("maximum employees who have left company having low salary but it might possible reason behind low salary that most of the people are having less experience so we have look at the relation between salary and total no of years of experience also before stating any conclusion about salary."),
                  h3("Departmentwise Salary of Employee:"),
                  plotlyOutput("deptsal",width = "1300px")
                          ),
                    
                  fluidRow(
                  h2("How many employees need salary hike?"),
                  plotlyOutput("salaryhike",width = "1300px"),
                  h4("More Number of Employees from sales department needs salary hike.")
                           )
                  ),
                 
  #===========================================================================================================================                 
  #Sentimental Analysis:Employee reviews to company                
                
       tabItem(
              tabName = "Review_Analysis",
                  h1("Reviews about company from employees "),
                  tabsetPanel(type="tabs",
                  tabPanel("wordcloud",plotOutput("wordcloud")),
                  tabPanel("sentimental",plotOutput("sentimental"),
                  h3("As we can see after performing sentimental analysis,The positive review are more because most of employees are satisfied with company because as they have ggiven positive reviews.")
                            )
                                      
                            )
                          
                          
                          
              )
             
    )
)
  
))