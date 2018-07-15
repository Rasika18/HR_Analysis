library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(htmltools)
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(DT)
library(dplyr)
library(corrplot)
library(dplyr)
library(recommenderlab)
library(tidyverse)
library(gplots)
library(caret)
library(caTools)
library(highcharter)
library(pROC)
library(ROCR)
library(e1071)
library(rpart)
library(rpart.plot)
library(stringr)
library(wordcloud)
library(plotly)
library(tm)
shinyServer(function(input,output)
{
  
  
  # Dashboard
  output$Plotofemployee <- renderPlotly({
    plot_ly(Employee_details, x=Employee_details$Department,color =Employee_details$Department,marker = list(line = list(color = 'rgb(8,48,107)', width = 1.5)))
    
  })
  #===============================================================================================================================
  #Resource Hiring Analytics
  output$resource <- renderTable({
    subset(recruiting_source,recruiting_source$`Employment Source`==input$EmployementSource)
  })
  
  output$recruitmentwiseperformance <- renderPlotly({
    AttritionData$EmployeeSource <-  AttritionData$EmployeeSource
    AttritionData$PerformanceScore <- factor( AttritionData$PerformanceScore)
    
    AttritionData$PerformanceScore[  AttritionData$PerformanceScore == 1] <- 'Fully Meets'
    AttritionData$PerformanceScore[  AttritionData$PerformanceScore == 2] <- 'Too early to review'
    AttritionData$PerformanceScore[ AttritionData$PerformanceScore == 3] <- 'Needs Improvement'
    
    p <- ggplot( AttritionData, aes(PerformanceScore, fill = AttritionData$EmployeeSource)) + 
      geom_bar(width=0.8, position="dodge",color="black")+
      labs(x = "Month", y = "Count", fill = "Performance")+
      theme(legend.text = element_text(colour="blue", size=10, 
                                       face="bold"))
    
    ggplotly(p)
    
  })
  #===============================================================================================================================
  #Exploratory Data Analysis for Attrition of Employees
  
  output$Attrition <- renderTable({
    set.seed(12345)
    inTrain <- createDataPartition(AttritionData$Attrition,p=0.75,list = FALSE)
    Training <- AttritionData[inTrain,]
    Testing <- AttritionData[-inTrain,]
    prop.table(table(Training$Attrition))
  })
  
  
  output$Attrition1 <- renderPlotly({
    set.seed(12345)
    inTrain <- createDataPartition(AttritionData$Attrition,p=0.75,list = FALSE)
    Training <- AttritionData[inTrain,]
    Testing <- AttritionData[-inTrain,]
    p <- ggplot(Training,aes(Attrition,fill=Attrition))+geom_bar()
    
    ggplotly()
    
    
    
    
  })
  
  output$Ageplot <- renderPlotly({
    set.seed(12345)
    inTrain <- createDataPartition(AttritionData$Attrition,p=0.75,list = FALSE)
    Training <- AttritionData[inTrain,]
    Testing <- AttritionData[-inTrain,]
    agePlot <- ggplot(Training,aes(Age,fill=Attrition))+geom_density()+facet_grid(~Attrition)
    # print(agePlot)
    ggplotly(agePlot)
  })
  
  output$BusinessTravel <- renderPlotly({
    set.seed(12345)
    inTrain <- createDataPartition(AttritionData$Attrition,p=0.75,list = FALSE)
    Training <- AttritionData[inTrain,]
    Testing <- AttritionData[-inTrain,]
    travelPlot <- ggplot(Training,aes(BusinessTravel,fill=Attrition))+geom_bar()
    #print(travelPlot)
    ggplotly(travelPlot)
  })
  output$Department <- renderPlotly({
    set.seed(12345)
    inTrain <- createDataPartition(AttritionData$Attrition,p=0.75,list = FALSE)
    Training <- AttritionData[inTrain,]
    Testing <- AttritionData[-inTrain,]
    depPlot <- ggplot(Training,aes(Department,fill = Attrition))+geom_bar()
    #print(depPlot)
    ggplotly(depPlot)
  })
  
  
  
  output$gender <- renderPlotly({
    set.seed(12345)
    inTrain <- createDataPartition(AttritionData$Attrition,p=0.75,list = FALSE)
    Training <- AttritionData[inTrain,]
    Testing <- AttritionData[-inTrain,]
    genPlot <- ggplot(Training,aes(Gender,fill=Attrition))+geom_bar()
    #print(genPlot)
    ggplotly(genPlot)
  })
  output$joblevel <- renderPlotly({
    set.seed(12345)
    inTrain <- createDataPartition(AttritionData$Attrition,p=0.75,list = FALSE)
    Training <- AttritionData[inTrain,]
    Testing <- AttritionData[-inTrain,]
    jobLevelPlot <- ggplot(Training,aes(Job_level,fill=Attrition))+geom_bar()
    #print(jobLevelPlot)
    ggplotly(jobLevelPlot)
  })
  output$hourlysalary <- renderPlotly({
    set.seed(12345)
    inTrain <- createDataPartition(AttritionData$Attrition,p=0.75,list = FALSE)
    Training <- AttritionData[inTrain,]
    Testing <- AttritionData[-inTrain,]
    hourlyPlot <- ggplot(Training,aes(hourly_salary,fill=Attrition))+geom_bar()
    #print(hourlyPlot)
    ggplotly(hourlyPlot)
    
  })
  
  
  output$jobsatisfaction <- renderPlotly({
    set.seed(12345)
    inTrain <- createDataPartition(AttritionData$Attrition,p=0.75,list = FALSE)
    Training <- AttritionData[inTrain,]
    Testing <- AttritionData[-inTrain,]
    jobSatPlot <- ggplot(Training,aes(satisfaction_level,fill=Attrition))+geom_bar()
    #print(jobSatPlot)
    ggplotly(jobSatPlot)
  })
  
  
  output$marrigestatus <- renderPlotly({
    set.seed(12345)
    inTrain <- createDataPartition(AttritionData$Attrition,p=0.75,list = FALSE)
    Training <- AttritionData[inTrain,]
    Testing <- AttritionData[-inTrain,]
    marPlot <- ggplot(Training,aes(MaritalDesc,fill=Attrition))+geom_bar()
    #print(marPlot)
    ggplotly(marPlot)
  })
  
  
  
  output$monthincome <- renderPlotly({
    set.seed(12345)
    inTrain <- createDataPartition(AttritionData$Attrition,p=0.75,list = FALSE)
    Training <- AttritionData[inTrain,]
    Testing <- AttritionData[-inTrain,]
    monthlyIncPlot <- ggplot(Training,aes(monthly_salary,fill=Attrition))+geom_density()
    #print(monthlyIncPlot)
    ggplotly(monthlyIncPlot)
  })
  
  output$numberofcompnyworked <- renderPlotly({
    set.seed(12345)
    inTrain <- createDataPartition(AttritionData$Attrition,p=0.75,list = FALSE)
    Training <- AttritionData[inTrain,]
    Testing <- AttritionData[-inTrain,]
    numCompPlot <- ggplot(Training,aes(NumCompaniesWorked,fill=Attrition))+geom_bar()
    # print( numCompPlot)
    ggplotly( numCompPlot)
  })
  
  
  
  output$performancerate <- renderPlotly({
    set.seed(12345)
    inTrain <- createDataPartition(AttritionData$Attrition,p=0.75,list = FALSE)
    Training <- AttritionData[inTrain,]
    Testing <- AttritionData[-inTrain,]
    perfPlot <- ggplot(Training,aes(performance,fill = Attrition))+geom_bar()
    #print(perfPlot)
    ggplotly(perfPlot)
  })
  
  output$yearatcompany <- renderPlotly({
    set.seed(12345)
    inTrain <- createDataPartition(AttritionData$Attrition,p=0.75,list = FALSE)
    Training <- AttritionData[inTrain,]
    Testing <- AttritionData[-inTrain,]
    YearAtComPlot <- ggplot(Training,aes(YearsAtCompany,fill = Attrition))+geom_bar()
    #print(YearAtComPlot)
    ggplotly(YearAtComPlot)
    
  })
  
  output$yearatcurrentrole <- renderPlotly({
    set.seed(12345)
    inTrain <- createDataPartition(AttritionData$Attrition,p=0.75,list = FALSE)
    Training <- AttritionData[inTrain,]
    Testing <- AttritionData[-inTrain,]
    YearInCurrPlot <- ggplot(Training,aes(YearsInCurrentRole,fill = Attrition))+geom_bar()
    #print(YearInCurrPlot)
    ggplotly(YearInCurrPlot)
  })
  
  output$yearsincelastpromotion <- renderPlotly({
    set.seed(12345)
    inTrain <- createDataPartition(AttritionData$Attrition,p=0.75,list = FALSE)
    Training <- AttritionData[inTrain,]
    Testing <- AttritionData[-inTrain,]
    YearsSinceProm <- ggplot(Training,aes(YearsSinceLastPromotion,fill = Attrition))+geom_bar()
    # print(YearsSinceProm)
    ggplotly(YearsSinceProm)
  })
  
  output$yearwithcurrentmanager <- renderPlotly({
    set.seed(12345)
    inTrain <- createDataPartition(AttritionData$Attrition,p=0.75,list = FALSE)
    Training <- AttritionData[inTrain,]
    Testing <- AttritionData[-inTrain,]
    YearsCurrManPlot <- ggplot(Training,aes(YearsWithCurrManager,fill = Attrition))+geom_bar()
    #print(YearsCurrManPlot)
    ggplotly(YearsCurrManPlot)
  })
  
  output$worklifebal <- renderPlotly({
    set.seed(12345)
    inTrain <- createDataPartition(AttritionData$Attrition,p=0.75,list = FALSE)
    Training <- AttritionData[inTrain,]
    Testing <- AttritionData[-inTrain,]
    WLBPlot <- ggplot(Training,aes(WorkLifeBalance,fill = Attrition))+geom_bar()
    # print(WLBPlot)
    ggplotly(WLBPlot)
  })
  
  output$relationsatisfy <- renderPlotly({
    set.seed(12345)
    inTrain <- createDataPartition(AttritionData$Attrition,p=0.75,list = FALSE)
    Training <- AttritionData[inTrain,]
    Testing <- AttritionData[-inTrain,]
    RelSatPlot <- ggplot(Training,aes(RelationshipSatisfaction,fill = Attrition))+geom_bar()
    #print(RelSatPlot)
    ggplotly(RelSatPlot)
  })
  
  output$numproj <- renderPlotly({
    set.seed(12345)
    inTrain <- createDataPartition(AttritionData$Attrition,p=0.75,list = FALSE)
    Training <- AttritionData[inTrain,]
    Testing <- AttritionData[-inTrain,]
    numproject <- ggplot(Training,aes(number_project,fill = Attrition))+geom_bar()
    #print( numproject)
    ggplotly( numproject)
  })
  output$monthlyhr <- renderPlotly({
    set.seed(12345)
    inTrain <- createDataPartition(AttritionData$Attrition,p=0.75,list = FALSE)
    Training <- AttritionData[inTrain,]
    Testing <- AttritionData[-inTrain,]
    monthlyhrs <- ggplot(Training,aes(average_montly_hours,fill = Attrition))+geom_bar()
    #print( monthlyhrs)
    ggplotly(monthlyhrs )
  })
  
  output$corr <- renderPlot({
    
    
    
    my_data <-AttritionData[, c(1,2,5,6,7,18,20,21,22,23,24,32,33,34)]
    numeric.var <- sapply(my_data, is.numeric)
    corr.matrix <- cor(my_data[,numeric.var])
    corrplot(corr.matrix, 
             method = "number")
  })
  
  
  #===============================================================================================================================
  #Does people leaving after certain time?
  
  output$Plotoftimeofleave <- renderHighchart({
    Employee_details$time_spend_company=as.factor(Employee_details$time_spend_company)
    hr_left = subset(Employee_details,left==1)
    hr_not_left = subset(Employee_details,left==0)
    hrtime<-data.frame(table(Employee_details$time_spend_company))
    data = Employee_details$time_spend_company
    str(data)
    hcbar <- highchart() %>% 
      hc_xAxis(categories = hrtime$Var1,title="Department") %>% 
      hc_yAxis(title = NULL) %>% 
      hc_add_series(data = hr_left$time_spend_company,  showInLegend = FALSE,
                    name ="Left")%>%
      hc_add_series(data = hr_not_left$time_spend_company,showInLegend = FALSE,
                    name ="Not left")
    
    hcbar
    
  })
  
  
  output$Whosetimeis3year <- renderPlotly({
    Employee_details$YearsAtCompany <- Employee_details$YearsAtCompany
    Employee_details$Department <- factor( Employee_details$Department )
    
    
    
    
    p <- ggplot(Employee_details, aes(YearsAtCompany, fill =   Employee_details$Department )) + 
      geom_bar(width=0.8, position="dodge")+
      labs(x = "Departments", y = "Count", fill = "Salary Hike")+
      theme(legend.text = element_text(colour="blue", size=10, 
                                       face="bold"))
    
    ggplotly(p)
  })
  
  output$yearsatcomapnycount <- renderTable({
    Time_spend_Atcompany <- table(Employee_details$YearsAtCompany)
    threeyears <- Time_spend_Atcompany[names(Time_spend_Atcompany)=="3"]
    twoyears <- Time_spend_Atcompany[names(Time_spend_Atcompany)=="2"]
    df_year <- data.frame(YearsAtCompany= c("3", "2"), Count = c(twoyears, threeyears))
    df_year
  })
  #===============================================================================================================================
  #Does low satisfaction level making employees leave?
  output$plotofSatisfactionLevel <- renderPlotly({
    levels(Employee_details$left) <- c("0", "1")
    Employee_details$left = as.numeric(as.character(Employee_details$left))
    data_left = Employee_details %>% filter(left==1) # Create this before as.factor() call
    Employee_details$left = as.factor(Employee_details$left) # Need to do this for fill reasons
    
    g1 <- ggplot(Employee_details, aes(x=Employee_details$satisfaction_level, fill=left)) + 
      geom_histogram(binwidth=.01, alpha=.8, position="identity") +
      scale_fill_manual(values=c("green","red")) +
      theme(legend.position = 'none') +    
      labs(x='Satisfaction Level')
    print(g1)
    ggplotly(g1)
  })
  
  
  output$plotofLastEvaulation <- renderPlotly({
    tr <- function(a){
      ggplot(data = Employee_details, aes(x= a, y=..density..)) + geom_histogram(fill="blue",color="red",alpha = 0.5,bins =100) +
        geom_density()
      ggplotly()
    }
    tr(Employee_details$last_evaluation)
  })
  #===============================================================================================================================
  #Which Department Have High Attrition?
  
  output$Plotofdeptprob <- renderHighchart({
    Employee_details$Department=as.factor(Employee_details$Department)
    hr_left = subset(Employee_details,left==1)
    hr_not_left = subset(Employee_details,left==0)
    #hrtime<-data.frame(table(Employee_details$time_spend_company))
    
    hrdepartment<-data.frame(table(Employee_details$Department))
    str(hrdepartment)
    hc <- highchart() %>% 
      hc_xAxis(categories = hrdepartment$Var1,title="Department") %>% 
      hc_yAxis(title = NULL) %>% 
      hc_add_series(name = "Left", data = hr_left$Department, showInLegend = FALSE,
                    type = "column") %>% 
      hc_add_series(name = "Not left", data = hr_not_left$Department,showInLegend = FALSE,
                    type = 'column')
    hc
  })
  
  output$departmentproject <- renderPlotly({
    plot_ly(Department_Details, x=Department_Details$Department, y =Department_Details$`Total projects`, color =Department_Details$Department, type = "bar",marker = list(line = list(color = 'rgb(8,48,107)', width = 1.5)))   
  })
  
  output$deep <- renderTable({
    round(prop.table(table(Employee_details$Department))*100)
  })
  
  
  output$deep1 <- renderTable({
    left_dept=subset(Employee_details,Employee_details$left==1)
    (table(left_dept$Department))/(table(Employee_details$Department))
  }) 
  
  #===============================================================================================================================
  #Predict how many employees will leave company?
  
  output$CART <- renderTable({
    split=sample.split(Employee_details$left,SplitRatio = 0.8)
    train=subset(Employee_details,split==TRUE)
    test=subset(Employee_details,split==FALSE)
    table(train$left)
    table(test$left)
    set.seed(1234)
    model_dt <- rpart(left ~ ., data=train, method="class", minbucket=35)
    printcp(model_dt) 
    
    
  })
  
  
  output$decisiontree <- renderPlot({
    model_dt <- rpart(left ~ ., data=train, method="class", minbucket=25)
    bestcp <- model_dt$cptable[which.min(model_dt$cptable[,"xerror"]),"CP"]
    model_dt.pruned <- prune(model_dt, cp = bestcp)
    rpart.plot(model_dt.pruned)
  })
  
  
  output$tablepred <- renderTable({
    model_dt <- rpart(left ~ ., data=train, method="class", minbucket=25)
    bestcp <- model_dt$cptable[which.min(model_dt$cptable[,"xerror"]),"CP"]
    model_dt.pruned <- prune(model_dt, cp = bestcp)
    predicted_dt <- predict(model_dt.pruned, test, type="class")
    table(test$left, predicted_dt)
  })
  
  
  output$tablepredpercent <- renderTable({
    train=subset(Employee_details,split==TRUE)
    test=subset(Employee_details,split==FALSE)
    table(train$left)
    table(test$left)
    set.seed(100)
    model_dt <- rpart(left ~ ., data=train, method="class", minbucket=25)
    bestcp <- model_dt$cptable[which.min(model_dt$cptable[,"xerror"]),"CP"]
    model_dt.pruned <- prune(model_dt, cp = bestcp)
    predicted_dt <- predict(model_dt.pruned, test, type="class")
    mean(predicted_dt==test$left)
  })
  # =================================
  
  
  output$logistictable1 <- renderPrint({
    # split=sample.split(Employee_details$left,SplitRatio = 0.8)
    train=subset(Employee_details,split==TRUE)
    test=subset(Employee_details,split==FALSE)
    table(train$left)
    set.seed(100)
    hr_lr_modal=glm(left ~ satisfaction_level + number_project
                    ,data=train, family = 'binomial',maxit=100)
    summary(hr_lr_modal)
  })
  
  output$logistic2 <- renderTable({
    train=subset(Employee_details,split==TRUE)
    test=subset(Employee_details,split==FALSE)
    table(train$left)
    set.seed(100)
    hr_lr_modal=glm(left ~ satisfaction_level + number_project
                    ,data=train, family = 'binomial',maxit=100)
    prediction=predict(hr_lr_modal,newdata = test)
    #converting probabilities to 1 or 0 
    pred1=ifelse(predict(hr_lr_modal, newdata = test, type='response')>0.5,1,0)
    table(as.factor(test$left),pred1)
  })
  
  
  output$logistic3 <- renderText({
    train=subset(Employee_details,split==TRUE)
    test=subset(Employee_details,split==FALSE)
    table(train$left)
    set.seed(100)
    hr_lr_modal=glm(left ~ satisfaction_level + number_project
                    ,data=train, family = 'binomial',maxit=100)
    prediction=predict(hr_lr_modal,newdata = test)
    #converting probabilities to 1 or 0 
    pred1=ifelse(predict(hr_lr_modal, newdata = test, type='response')>0.5,1,0)
    mean(pred1==test$left)
  })
  
  
  output$roc <- renderPlot({
    split=sample.split(Employee_details$left,SplitRatio = 0.8)
    train=subset(Employee_details,split==TRUE)
    test=subset(Employee_details,split==FALSE)
    set.seed(100)
    hr_lr_modal=glm(left ~ satisfaction_level + number_project
                    ,data=train, family = 'binomial',maxit=100)
    glm_response_scores <- predict(hr_lr_modal, test, type="response")
    pred_glm_th <- prediction(glm_response_scores, test$left)
    perf_glm <- performance(pred_glm_th, "tpr", "fpr")
    predict_dt_ROC <- predict(model_dt.pruned, test)
    pred_dt <- prediction(predict_dt_ROC[,2], test$left)
    perf_dt <- performance(pred_dt, "tpr", "fpr")
    plot(perf_glm, main = "ROC curves for the models", col='blue')
    plot(perf_dt,add=TRUE, col='red')
    legend('bottom', c("Logistic Regression", "Decision Tree"), fill = c('blue','red'), bty='n')
  })
  
  output$accuracyoflogistic <- renderText({
    train=subset(Employee_details,split==TRUE)
    test=subset(Employee_details,split==FALSE)
    table(train$left)
    set.seed(100)
    hr_lr_modal=glm(left ~ satisfaction_level + number_project
                    ,data=train, family = 'binomial',maxit=100)
    prediction=predict(hr_lr_modal,newdata = test)
    #converting probabilities to 1 or 0 
    pred1=ifelse(predict(hr_lr_modal, newdata = test, type='response')>0.5,1,0)
    mean(pred1==test$left)
    
  })
  
  output$accuracyofcart <- renderText({
    train=subset(Employee_details,split==TRUE)
    test=subset(Employee_details,split==FALSE)
    table(train$left)
    table(test$left)
    set.seed(100)
    model_dt <- rpart(left ~ ., data=train, method="class", minbucket=25)
    bestcp <- model_dt$cptable[which.min(model_dt$cptable[,"xerror"]),"CP"]
    model_dt.pruned <- prune(model_dt, cp = bestcp)
    predicted_dt <- predict(model_dt.pruned, test, type="class")
    mean(predicted_dt==test$left)
  })
  #===============================================================================================================================
  #Predict Who will leave??
  output$predictwhowilleave <- renderPlotly({
    set.seed(100)
    # Keep some data to test again the final model
    inTraining <- createDataPartition(AttritionData$left, p = .75, list = FALSE)
    training <- AttritionData[ inTraining,]
    testing  <- AttritionData[-inTraining,]
    # Estimate the drivers of attrition
    logreg = glm(left ~ satisfaction_level + number_project, family=binomial(logit), data=training)
    # Make predictions on the out-of-sample data
    probaToLeave=predict(logreg,newdata=testing,type="response")
    # Structure the prediction output in a table
    predattrition = data.frame(probaToLeave)
    # Add a column to the predattrition dataframe containing the performance
    predattrition$performance=testing$last_evaluation
    plot_ly(predattrition,x=~probaToLeave,y=~performance,text = ~paste('Probabilty of leaving: ', predattrition$probaToLeave,'\n Performance:',predattrition$performance)) 
    
  })
  output$dataofleavingprobability <- DT::renderDataTable({
    set.seed(100)
    # Keep some data to test again the final model
    inTraining <- createDataPartition(AttritionData$left, p = .75, list = FALSE)
    training <- AttritionData[ inTraining,]
    testing  <- AttritionData[-inTraining,]
    # Estimate the drivers of attrition
    logreg = glm(left ~ satisfaction_level + number_project, family=binomial(logit), data=training)
    # Make predictions on the out-of-sample data
    probaToLeave=predict(logreg,newdata=testing,type="response")
    # Structure the prediction output in a table
    predattrition = data.frame(probaToLeave)
    # Add a column to the predattrition dataframe containing the performance
    predattrition$performance=testing$last_evaluation
    predattrition$priority=predattrition$performance*predattrition$probaToLeave
    orderpredattrition=predattrition[order(predattrition$priority,decreasing = TRUE),]
    orderpredattrition <- head(orderpredattrition, n=300)
    DT::datatable(orderpredattrition)
    
  })
  
  
  #===============================================================================================================================
  #GenderPay Gap Analysis
  
  output$totalfemalemale <- renderPlotly({
    plot_ly(Employee_details,x=Employee_details$Department,color = Employee_details$Gender,marker = list(line = list(color = 'rgb(8,48,107)', width = 1.5)))
  })
  output$Genderwisesalary <- renderPlotly({
    Employee_details$Department <- Employee_details$Department
    Employee_details$salary <- factor(Employee_details$salary )
    
    Employee_details$salary[ Employee_details$salary == 1] <- 'Low'
    Employee_details$salary[ Employee_details$salary == 2] <- 'High'
    Employee_details$salary[ Employee_details$salary == 3] <- 'Medium'
    
    
    
    p <- ggplot(Employee_details, aes(Employee_details$Gender, fill =  Employee_details$salary)) + 
      geom_bar(width=0.8, position="dodge",color="black")+
      labs(x = "Departments", y = "Count", fill = "Salary Hike")+
      theme(legend.text = element_text(colour="blue", size=10, 
                                       face="bold"))
    
    ggplotly(p)
    
  })
  
  output$meansalgenderwise <- renderTable({
    aggregate(AttritionData$`Monthly salary`, by=list(Gender = AttritionData$Gender), mean)
  })
  output$totalmalefemale <- renderTable({
    gender <- table(Employee_details$Gender)
    males <- gender[names(gender)=="Male"]
    females <- gender[names(gender)=="Female"]
    df_gender <- data.frame(Gender = c("Males", "Females"), Count = c(males, females))
    df_gender
  })
  
  #===============================================================================================================================
  #Salary Analytics
  
  output$Plotofsatisfaction <- renderPlotly({
    Employee_left=subset(Employee_details,Employee_details$left==1)
    tr <- function(a){
      ggplot(data = Employee_left, aes(x= a, y=..density..)) + geom_histogram(fill="blue",color="red",alpha = 0.5,bins =100) +
        geom_density()
      ggplotly()
    }
    tr(Employee_left$satisfaction_level)
  })
  
  
  output$Plotofsalempact <- renderHighchart({
    hchart=hchart(as.character(Employee_details$salary),type = "pie",showInLegend =FALSE,name ="Left")    
    hchart
    
  })
  output$deptsal <- renderPlotly({
    Employee_details$Department <- Employee_details$Department
    Employee_details$salary <- factor(Employee_details$salary)
    
    Employee_details$salary[ Employee_details$salary == 1] <- 'low'
    Employee_details$salary[ Employee_details$salary == 2] <- 'high'
    Employee_details$salary[ Employee_details$salary == 3] <- 'medium'
    
    p <- ggplot(Employee_details, aes(Department, fill =  Employee_details$salary)) + 
      geom_bar(width=0.8, position="dodge",color="black")+
      labs(x = "Departments", y = "Count", fill = "salary")+
      theme(legend.text = element_text(colour="blue", size=10, 
                                       face="bold"))
    
    ggplotly(p)
  })
  
  
  
  output$salaryhike <- renderPlotly({
    Employee_details$Department <- Employee_details$Department
    Employee_details$`Salary Hike` <- factor(Employee_details$`Salary Hike`)
    
    Employee_details$`Salary Hike`[ Employee_details$`Salary Hike` == 1] <- 'Require'
    Employee_details$`Salary Hike`[ Employee_details$`Salary Hike` == 2] <- 'Not Now'
    Employee_details$`Salary Hike`[ Employee_details$`Salary Hike` == 3] <- 'Hike  In Next Month'
    Employee_details$`Salary Hike`[ Employee_details$`Salary Hike` == 3] <- 'Exceptional'
    Employee_details$`Salary Hike`[ Employee_details$`Salary Hike` == 3] <- 'Needs Improvement'
    Employee_details$`Salary Hike`[ Employee_details$`Salary Hike` == 3] <- 'Hike After Two Months'
    
    
    p <- ggplot(Employee_details, aes(Department, fill =  Employee_details$`Salary Hike`)) + 
      geom_bar(width=0.8, position="dodge",color="black")+
      labs(x = "Departments", y = "Count", fill = "Salary Hike")+
      theme(legend.text = element_text(colour="blue", size=10, 
                                       face="bold"))
    
    ggplotly(p)
  })
  #===============================================================================================================================
  #Sentimental Analysis:Employee reviews to company
  
  
  output$wordcloud <- renderPlot({
    
    text <- paste(readLines("content.csv"), collapse = " ")
    text
    text2 <- gsub(pattern = "\\W", replace =" ", text)
    text2
    text2 <- gsub(pattern = "\\d", replace =" ", text2)
    text2
    text2 <- tolower(text2)
    text2
    library(tm)
    #stopwords()
    #text2
    #removeWords(text2, stopwords())
    #text2 <- removeWords(text2, stopwords())
    #text2
    text2 <- gsub(pattern = "\\b[A-z]\\b{1}}", replace=" ", text2)
    text2
    text2 <- stripWhitespace(text2)
    text2
    library(stringr)
    library(wordcloud)
    library(plotly)
    text2
    textbag <- str_split(text2, pattern="\\s+")
    textbag
    class(textbag)
    textbag <- unlist(textbag)
    class(textbag)
    textbag
    str(textbag)
    getwd()
    readLines("positive-word.txt")
    poswords <- scan("positive-word.txt", what = 'character', comment.char = ";")
    str(poswords)
    negwords <- scan("negative-word.txt", what = 'character', comment.char = ";")
    str(negwords)
    match(textbag, poswords)
    match(textbag, negwords)
    ##posword <- c(pos, 'interesting')
    ##negwords <- c(neg, 'not interesting') 
    posscore <- sum(!is.na(match(textbag, poswords)))
    posscore
    negscore <- sum(!is.na(match(textbag, negwords)))
    negscore
    score <- sum(!is.na(match(textbag, poswords))) - sum(!is.na(match(textbag, negwords)))
    score
    mean(score)
    hist(score)
    sd(score)
    wordcloud(textbag)
    wordcloud(textbag,min.freq = 4, random.order = FALSE, scale = c(3, 0.5), colors = rainbow(3))
    
  })
  
  output$sentimental<- renderPlot({
    
    
    AttritionData$comments[  AttritionData$comments== 'positive'] = 'positive'
    AttritionData$comments[ AttritionData$comments == 'negative'] = 'negative'
    
    qplot(factor( AttritionData$comments), data= AttritionData, geom="bar", fill=factor( AttritionData$comments))
    
    
    
    
  })
  
})


