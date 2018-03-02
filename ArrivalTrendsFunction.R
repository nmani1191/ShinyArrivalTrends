# This is an R Shiny Application helping to plot Ticket/Transactions trends & distributions

trend_distribution <-  function()
{
  #Package Loading
  #######################################################
  library(shiny)
  library(dplyr)
  library(DT)
  library(lubridate)
  library(ggplot2)
  library(reshape2)
  library(ggrepel)
  library(reshape)
  library(shinydashboard)
  library(zoo)
  
  #Function Loading
  #######################################################
  #Summary of Input Dataset
  Unique_NA_counts <- function(input_data)
  {
    df1 <- data.frame(character(),character(),integer(),integer(),integer(),numeric())
    
    for (name in colnames(input_data)) {
      
      df1 <- rbind(df1,data.frame(ColName=name,Datatype=class(input_data[,name]),Total_Records=nrow(input_data),
                                  Unique_Counts=length(unique(input_data[,name])),
                                  NA_Counts=sum(is.na(input_data[,name])),
                                  NA_Percent=round(sum(is.na(input_data[,name]))/nrow(input_data),2)))
      
    }
    
    df1 <- as.data.frame(df1 %>% arrange(-NA_Counts))
    
    return(df1)
  }
  
  #######################################################
  #Preprocessing the dataset by extracting additional date columns:
  pre_processing <- function(dataset,dataFormat)
  {
    dataset$Mod_OpenDate <- parse_date_time(dataset$m_Tck_Open_Date,dataFormat)
    
    dataset$Mod_MMYYY<- as.yearmon(dataset$Mod_OpenDate,"%b-%Y")
    dataset$Mod_OpenHour <- format(dataset$Mod_OpenDate,"%H")
    dataset$Mod_OpenHour <- factor(dataset$Mod_OpenHour,levels = c("06","07","08","09","10","11","12","13",
                                                                   "14","15","16","17","18","19","20","21",
                                                                   "22","23","00","01","02","03","04","05"))
    dataset$Mod_OpenWeekDay <- format(dataset$Mod_OpenDate,"%A")
    dataset$Mod_OpenWeekDay <- factor(dataset$Mod_OpenWeekDay,levels=c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))
    dataset$Mod_Week <- format(dataset$Mod_OpenDate,"%W")
    
    dataset$Mod_Shift[dataset$Mod_OpenHour %in% c("06","07","08","09","10","11","12","13")] <- "Shift1"
    dataset$Mod_Shift[dataset$Mod_OpenHour %in% c("14","15","16","17","18","19","20","21")] <- "Shift2"
    dataset$Mod_Shift[dataset$Mod_OpenHour %in% c("22","23","00","01","02","03","04","05")] <- "Shift3"
    
    return(dataset)
  }
  
  #######################################################
  #Get Hourly Tck Distribution plot:
  Hourly_tck_distribution <- function(dataset)
  {
    hr_tck_count <- as.data.frame(dataset %>% dplyr::group_by(Mod_OpenHour,Mod_Shift) %>% dplyr::summarise(Count=n()))
    
    write.csv(hr_tck_count,"Hourly_tck_distribution_plotData.csv",row.names = F)
    
    Plot<- ggplot(hr_tck_count,aes(Mod_OpenHour,Count,fill=Mod_Shift)) + 
      geom_bar(stat = "identity",width = 0.5)+
      ggtitle("Hourly Arrival") + 
      xlab("Hour(CST)")+
      ylab("Count of tickets")+
      geom_text(aes(label=Count),hjust=1,vjust=0.1,size=3,angle=90) +
      scale_x_discrete(breaks=hr_tck_count$Mod_OpenHour,labels = hr_tck_count$Mod_OpenHour )+
      theme(panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_blank(),
            legend.position="right",
            panel.background=element_rect(fill="white",colour="black",size=0.5,linetype = 1),
            panel.grid.minor = element_blank(),
            axis.text.x = element_text(angle = 60, vjust = 1,size = 10, hjust = 1),
            strip.text.x = element_text(face = "bold.italic")) + scale_fill_discrete(name = "Shift")
    
    return(Plot)
    
  }
  
  #######################################################
  #Get Hourly Tck Distribution by Priority plot: 
  Hourly_tck_distribution_priority <- function(dataset)
  {
    hr_tck_count1 <- as.data.frame(dataset %>% dplyr::group_by(Mod_OpenHour,Mod_Shift,m_Tck_Priority) %>% dplyr::summarise(Count=n()))
    
    write.csv(hr_tck_count1,"Hourly_tck_distribution_priority_plotData.csv",row.names = F)
    
    Plot <- ggplot(hr_tck_count1,aes(Mod_OpenHour,Count,fill=Mod_Shift)) + 
      geom_bar(stat = "identity",width = 0.5)+
      ggtitle("Priority by Hourly Arrival") + 
      xlab("Hour(CST)")+
      ylab("Count of tickets")+
      geom_text(aes(label=Count),hjust=1,vjust=0.1,size=3,angle=90) +
      scale_x_discrete(breaks=hr_tck_count1$Mod_OpenHour,labels = hr_tck_count1$Mod_OpenHour )+
      theme(panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_blank(),
            legend.position="right",
            panel.background=element_rect(fill="white",colour="black",size=0.5,linetype = 1),
            panel.grid.minor = element_blank(),
            axis.text.x = element_text(angle = 60, vjust = 1,size = 10, hjust = 1),
            strip.text.x = element_text(face = "bold.italic")) + scale_fill_discrete(name = "Shift") +
      facet_wrap(~m_Tck_Priority)
    
    return(Plot)
  }
  
  #######################################################
  #Get Monthly Tck Distribution Plot:
  Monthly_tck_distribution <- function(dataset)
  {
    monthly_dist <- as.data.frame(dataset %>% dplyr::group_by(Mod_MMYYY) %>% dplyr::summarise(Count=n()))
    monthly_dist$unique <- 1
    monthly_dist$Dev_MeanCount <- round((monthly_dist$Count - mean(monthly_dist$Count))/mean(monthly_dist$Count)*100,2)
    
    write.csv(monthly_dist,"Monthly_tck_distribution_plotData.csv",row.names = F)
    
    Plot <- ggplot(monthly_dist,aes(x=as.factor(Mod_MMYYY),y=Count,group=unique)) +
      geom_line() + geom_point()+ 
      theme_bw() +
      geom_text_repel(aes(label=Count),size=3) +
      labs(title="Monthly Arrival",y="Total Tickets",x="Months") +
      theme(title=element_text(face = "bold.italic"),
            plot.title = element_text(hjust=0.5),
            panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.minor.y = element_blank(),
            legend.position="none",
            axis.text.x=element_text(angle = 45, hjust = 0.9, vjust = 0.9,size = 10)) + 
      geom_hline(aes(yintercept = mean(monthly_dist$Count),color="red")) + 
      geom_text(aes(x=as.factor(unique(monthly_dist$Mod_MMYYY)[round(length(unique(monthly_dist$Mod_MMYYY))/2)]),y=mean(monthly_dist$Count),
                    label=paste("Mean =",round(mean(monthly_dist$Count),2)),
                    color="red",vjust=-0.1),size=3)
    
    return(Plot)
  }
  
  #######################################################
  #Get Monthly Tck Distribution by Priority Plot:
  Monthly_tck_distribution_priority <- function(dataset)
  {
    monthly_priority_dist_mod <- as.data.frame(dataset %>% dplyr::group_by(Mod_MMYYY,m_Tck_Priority) %>% dplyr::summarise(Count=n()))
    
    write.csv(monthly_priority_dist_mod,"Monthly_tck_distribution_priority_plotData.csv",row.names = F)
    
    Plot <- ggplot(monthly_priority_dist_mod,aes(as.factor(Mod_MMYYY),Count,group=m_Tck_Priority)) + 
      geom_line() + geom_point()+
      ggtitle("Priority by Month Arrival") + 
      ylab("Count of tickets")+
      geom_text_repel(aes(label=Count),size=3)+
      theme(panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_blank(),
            legend.position="right",
            panel.background=element_rect(fill="white",colour="black",size=0.5,linetype = 1),
            panel.grid.minor = element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_text(angle = 60, vjust = 1,size = 9, hjust = 1),
            strip.text.x = element_text(face = "bold.italic")) +facet_wrap(~m_Tck_Priority)
    
    return(Plot)
  }
  
  #######################################################
  #Get Weekly Tck Distribution Plot:
  Weekday_tck_distribution <- function(dataset)
  {
    weekday_dist_mod <- as.data.frame(dataset %>% dplyr::group_by(Mod_OpenWeekDay) %>% dplyr::summarise(Count=n()))
    weekday_dist_mod$unique <- 1
    
    write.csv(weekday_dist_mod,"Weekday_tck_distribution_plotData.csv",row.names = F)
    
    Plot <- ggplot(weekday_dist_mod,aes(Mod_OpenWeekDay,Count,group=unique)) + 
      geom_line() + geom_point()+
      ggtitle("WeekDays Arrival") + 
      xlab("WeekDays")+
      ylab("Count of tickets")+
      geom_text_repel(aes(label=Count),size=3)+
      theme(panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_blank(),
            legend.position="right",
            panel.background=element_rect(fill="white",colour="black",size=0.5,linetype = 1),
            panel.grid.minor = element_blank(),
            axis.text.x = element_text(angle = 60, vjust = 1,size = 9, hjust = 1),
            strip.text.x = element_text(face = "bold.italic"))
    
    return(Plot)
  }
  
  ######################################################
  #Get Weekly Tck Distribution by Month Plot:
  Weekday_tck_distribution_month <- function(dataset)
  {
    weekday_monthly_dist_mod <- as.data.frame(dataset %>% dplyr::group_by(Mod_MMYYY,Mod_OpenWeekDay) %>% dplyr::summarise(Count=n()))
    
    write.csv(weekday_monthly_dist_mod,"Weekday_tck_distribution_month_plotData.csv",row.names = F)
    
    Plot <- ggplot(weekday_monthly_dist_mod,aes(Mod_OpenWeekDay,Count,group=Mod_MMYYY)) + 
      geom_line() + geom_point()+
      ggtitle("WeekDays by Monthly Arrival") + 
      xlab("WeekDays")+
      ylab("Count of tickets")+
      geom_text_repel(aes(label=Count),size=3)+
      theme(panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_blank(),
            legend.position="right",
            panel.background=element_rect(fill="white",colour="black",size=0.5,linetype = 1),
            panel.grid.minor = element_blank(),
            axis.text.x = element_text(angle = 60, vjust = 1,size = 9, hjust = 1),
            strip.text.x = element_text(face = "bold.italic")) +facet_wrap(~Mod_MMYYY)
    
    return(Plot)
  }
  
  #####################################################
  #App Tck Distribution Plot
  Application_tck_distribution <- function(dataset,top_App_count)
  {
    top_App_count <- as.numeric(top_App_count)
    if(is.na(top_App_count)) {top_App_count <- 91}
    
    App_split <- as.data.frame(dataset %>% dplyr::group_by(m_Tck_App_Name) %>% dplyr::summarise(Count=n()) %>% dplyr::arrange(-Count))
    App_split$Prt <- round(App_split$Count/nrow(dataset)*100,2)
    App_split$CumPrt <- cumsum(App_split$Prt)
    App_split$m_Tck_App_Name <- ifelse(is.na(App_split$m_Tck_App_Name),"No App Name",as.character(App_split$m_Tck_App_Name))
    
    App_split$m_Tck_App_Name <- factor(App_split$m_Tck_App_Name,
                                       levels = unique(App_split[order(App_split$Count,decreasing=T),"m_Tck_App_Name"]))
    
    App_split1 <-  App_split[1:top_App_count,]
    
    write.csv(App_split1,"Application_tck_distribution_plotData.csv",row.names = F)
    
    Plot <- ggplot(App_split1,aes(m_Tck_App_Name,Prt,fill=m_Tck_App_Name)) + geom_bar(stat="identity") +
      geom_point(y=App_split1$CumPrt)+
      geom_line(y=App_split1$CumPrt,group="1",color="darkblue",linetype = "dashed")+
      ggtitle("Top Contributing Applications") + 
      ylab("Tickets(%)")+
      geom_text(aes(label=Prt),size=3,angle=90,hjust=0)+
      geom_text(aes(y=App_split1$CumPrt,label=CumPrt),size=3,angle=90,hjust=-0.1)+
      theme(legend.position="none",
            panel.background=element_rect(fill="white",colour="black",size=0.5,linetype = 1),
            panel.grid.minor = element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_text(angle = 90, vjust = 0.5,size = 9, hjust = 1),
            strip.text.y = element_text(face = "bold.italic",angle=0)) +ylim(0,100)
    
    return(Plot)
    
  }
  
  #####################################################
  
  ########################################################
  options(shiny.maxRequestSize = 1024*1024^2)
  #Shiny App
  shinyApp (
    
    ui =dashboardPage(skin = "green",

      dashboardHeader(title = "Trends & Distribution",titleWidth = 300),
      dashboardSidebar(width = 300,
                       conditionalPanel(condition="input.conditionedPanels==1",
                                        fileInput('dataset', 'Choose Dataset CSV File',accept=c('.csv')),
                                        actionButton("Validate","validate"),
                                        
                                        tags$hr(),
                                        
                                        uiOutput("OpenDate"),
                                        uiOutput("DateFormat"),
                                        uiOutput("Priority"),
                                        uiOutput("AppName"),
                                        uiOutput("AppCount"),
                                        uiOutput("runbutton"))
      ),
      
      dashboardBody(

        tabsetPanel(id="conditionedPanels",
                    tabPanel("Output",value = 1,
                             h4("1) Data Summary",style="color:darkgreen"),
                             DT::dataTableOutput("dataSummary"),
                             br(),
                             h4("2) Hourly Plot",style="color:darkgreen"),
                             plotOutput("HourlyPlot"),
                             br(),
                             h4("3) Hourly Plot by Priority",style="color:darkgreen"),
                             plotOutput("HourlyPlotByPriority"),
                             br(),
                             h4("4) Monthly Plot",style="color:darkgreen"),
                             plotOutput("MonthlyPlot"),
                             br(),
                             h4("5) Monthly Plot by Priority",style="color:darkgreen"),
                             plotOutput("MonthlyPlotByPriority"),
                             br(),
                             h4("6) Weekday Plot",style="color:darkgreen"),
                             plotOutput("WeekdayPlot"),
                             br(),
                             h4("7) Weekday Plot by Month",style="color:darkgreen"),
                             plotOutput("WeekdayPlotByMonth"),
                             br(),
                             h4("8) Ticket Distribution by App",style="color:darkgreen"),
                             plotOutput("TckDisByApp"),
                             br(),
                             uiOutput("processedDataDownload")),
                    tabPanel("Help",value = 2,
                             h3("How to choose Date Formats to compute MTTR ?",style="color:darkgreen"),
                             DT::dataTableOutput("dateFormatSample"))
        )
      )
    ),
    server= function(input, output, session) {
      
      observeEvent(input$Validate,{
        
        validate( need(input$dataset!=0,"Please Upload CSV File"))
        input$Validate # Re-run when button is clicked
        
        withProgress(message = 'Validating...', value = 0, {
          
          incProgress(0.25, detail = " 25%")
          
          inFile <- input$dataset
          ins_data_set <- read.csv(inFile$datapath,header = T,strip.white = T,
                                   na.strings = c(""," ","NA","NULL","na","null"))
          Column_names <- colnames(ins_data_set)
          
          output$OpenDate <- renderUI({
            selectInput("openDate", label = "Select OpenDate:",  c("--select--", Column_names))
          })
          
          date_formats <- c("ydm HM","mdY HM","mdY HMS","dmY HMS",
                            "dby HMS","dbY HMS","dBy HMS","dBY HMS",
                            "ymd HMS","Ymd HMS","BdY HMS","bdY HMS",
                            "Bdy HMS","mdy HMS","dmy HMS","dmY IMSp")
          
          date_formats_eg <- c(format(Sys.time(),"%y-%d-%m %H:%M"),format(Sys.time(),"%m-%d-%Y %H:%M"),format(Sys.time(),"%m-%d-%Y %H:%M:%S"),
                               format(Sys.time(),"%d-%m-%Y %H:%M:%S"),format(Sys.time(),"%d-%b-%y %H:%M:%S"),format(Sys.time(),"%d-%b-%Y %H:%M:%S"),format(Sys.time(),"%d-%B-%y %H:%M:%S"),
                               format(Sys.time(),"%d-%B-%Y %H:%M:%S"),format(Sys.time(),"%y-%m-%d %H:%M:%S"),format(Sys.time(),"%Y-%m-%d %H:%M:%S"),format(Sys.time(),"%B-%d-%Y %H:%M:%S"),
                               format(Sys.time(),"%b-%d-%Y %H:%M:%S"),format(Sys.time(),"%B-%d-%y %H:%M:%S"),format(Sys.time(),"%m-%d-%y %H:%M:%S"),format(Sys.time(),"%d-%m-%y %H:%M:%S"),
                               format(Sys.time(),"%d-%m-%Y %I:%M:%S %p"))
          
          sample_format <- data.frame(DateFormat=date_formats,DateExample=date_formats_eg)
          
          output$DateFormat <- renderUI({
            selectInput("dateFormat", label = "Select DateFormat:", c("--select--",date_formats))
          })
          
          output$Priority <- renderUI({
            selectInput("priority", label = "Select Priority:",  c("--select--", Column_names))
          })
          
          output$AppName <- renderUI({
            selectInput("appName", label = "Select AppName:",  c("--select--", Column_names))
          })
          
          output$AppCount <- renderUI({
            numericInput("appCount", label = "Select Top AppCount:",10,min = 10)
          })
          
          output$runbutton <- renderUI({
            actionButton("run","Run")
          })
          
          incProgress(0.5, detail = " 50%")
          
          dataSummary <- reactive({
            
            validate(
              need(input$Validate != 0, "Please Upload Date & Validate")
            )
            
            isolate({
              summary<- Unique_NA_counts(ins_data_set)
              return(summary)
            })
          })
          
          incProgress(0.75, detail = " 75%")
          
          output$dataSummary<- DT::renderDataTable(datatable(dataSummary(),filter='top',options=list(autoWidth=TRUE)))
          output$dateFormatSample<- DT::renderDataTable(datatable(sample_format,filter='top',options=list(autoWidth=TRUE)))
          
          incProgress(1, detail = " 100%")
        })
      })
      
      observeEvent(input$run,{
        
        input$run 
        
        withProgress(message = 'Main Processing...', value = 0, {
          
          incProgress(0.05, detail = " 5%") 
          
          inFile <- input$dataset
          ins_data_set <- read.csv(inFile$datapath,header = T,strip.white = T,
                                   na.strings = c(""," ","NA","NULL","na","null"))
          Column_names <- colnames(ins_data_set)
          
          incProgress(0.1, detail = " 10%")
          
          date_processing <- reactive({
            
            validate(
              need(input$run != 0 & 
                     (!input$openDate %in% c("--select--","")) &
                     (!input$dateFormat %in% c("--select--","")) ,"Please update Mandatory Fileds & Run it")
            )
            isolate({
              withProgress(message = 'Date Processing...', value = 0, {

                incProgress(0.1, detail = " 10%")
                
                ins_data_set$m_Tck_Open_Date <- ins_data_set[,which(colnames(ins_data_set)==input$openDate)]
                
                processed_dataset <- pre_processing(ins_data_set,input$dateFormat)
                hourly_plot <- Hourly_tck_distribution(processed_dataset)
                monthly_plot <- Monthly_tck_distribution(processed_dataset)
                weekday_plot <- Weekday_tck_distribution(processed_dataset) 
                weekday_month_plot <- Weekday_tck_distribution_month(processed_dataset)
                
                incProgress(1, detail = " 100%")
                
                return(list(processed_dataset=processed_dataset,hourly_plot=hourly_plot,monthly_plot=monthly_plot,
                            weekday_plot=weekday_plot,weekday_month_plot=weekday_month_plot))
                
              })
            })
          })
          
          priority_processing <- reactive({
            
            validate(
              need(input$run != 0 & 
                     (!input$priority %in% c("--select--","")),"Please update Mandatory Fileds & Run it")
            )
            isolate({
              withProgress(message = 'Priority Processing...', value = 0, {
                
                incProgress(0.1, detail = " 10%")
                
                ins_data_set$m_Tck_Open_Date <- ins_data_set[,which(colnames(ins_data_set)==input$openDate)]
                ins_data_set$m_Tck_Priority <- ins_data_set[,which(colnames(ins_data_set)==input$priority)]
                
                processed_dataset <- pre_processing(ins_data_set,input$dateFormat)
                hourly_priority_plot <- Hourly_tck_distribution_priority(processed_dataset)
                monthly_priority_plot <- Monthly_tck_distribution_priority(processed_dataset)
                
                incProgress(1, detail = " 100%")
                
                return(list(hourly_priority_plot=hourly_priority_plot,monthly_priority_plot=monthly_priority_plot))
                
              })
            })
          })
          
          app_processing <- reactive({
            
            validate(
              need(input$run != 0 & (!input$appName %in% c("--select--","")) & 
                     (!input$appCount %in% c("--select--","")),"Please update Mandatory Fileds & Run it")
            )
            isolate({
              withProgress(message = 'App Processing...', value = 0, {
                
                incProgress(0.1, detail = " 10%")
                
                ins_data_set$m_Tck_App_Name <- ins_data_set[,which(colnames(ins_data_set)==input$appName)]
                
                app_tck_dis_plot <- Application_tck_distribution(ins_data_set,input$appCount)
                
                incProgress(1, detail = " 100%")
                
                return(list(app_tck_dis_plot=app_tck_dis_plot))
                
              })
            })
          })
        
          incProgress(0.25, detail = " 25%")
          
          if((!input$openDate %in% c("--select--","")) & (!input$dateFormat %in% c("--select--","")))
          {
            date_outputs <- date_processing()
          }
          
          if((!input$openDate %in% c("--select--","")) & (!input$dateFormat %in% c("--select--","")) & (!input$priority %in% c("--select--","")))
          {
            priority_outputs <- priority_processing()  
          }
          
          if((!input$appName %in% c("--select--","")) & (!input$appCount %in% c("--select--","")))
          {
            app_outputs <- app_processing()  
          }
          
          incProgress(0.5, detail = " 50%")
          
          output$HourlyPlot <- renderPlot({date_outputs$hourly_plot})
          output$HourlyPlotByPriority <- renderPlot({priority_outputs$hourly_priority_plot})
          output$MonthlyPlot <- renderPlot({date_outputs$monthly_plot})
          output$MonthlyPlotByPriority <- renderPlot({priority_outputs$monthly_priority_plot})
          output$WeekdayPlot <- renderPlot({date_outputs$weekday_plot})
          output$WeekdayPlotByMonth <- renderPlot({date_outputs$weekday_month_plot})
          output$TckDisByApp <- renderPlot({app_outputs$app_tck_dis_plot})
  
          
          incProgress(0.75, detail = " 75%")
          
          output$processedDataDownload <- renderUI({
            fluidRow(
              column(width=6,h4("Download Processed Data",style="color:darkgreen")),
              column(width=3,downloadButton('data_download',"Download Data"))
            )
          })
          
          output$data_download <- downloadHandler(
            filename = "ProcessedData.csv",
            content = function(file) {
              write.csv(date_outputs$processed_dataset, file,row.names = F) })
          
          incProgress(1, detail = " 100%")
          
        })
      })
    }
  )
  
}

###########################################################

###########################################################
#Get Hourly Distribution plot:
get_hourly_tck_distribution <- function(dataset)
{
  library(ggplot2)
  
  dataset$Mod_OpenHour <- factor(dataset$Mod_OpenHour,levels = c("6","7","8","9","10","11","12","13",
                                                                 "14","15","16","17","18","19","20","21",
                                                                 "22","23","0","1","2","3","4","5")) 
  Plot<- ggplot(dataset,aes(Mod_OpenHour,Count,fill=Mod_Shift)) + 
    geom_bar(stat = "identity",width = 0.5)+
    ggtitle("Hourly Arrival") + 
    xlab("Hour(CST)")+
    ylab("Count of tickets")+
    geom_text(aes(label=Count),hjust=1,vjust=0.1,size=3,angle=90) +
    scale_x_discrete(breaks=dataset$Mod_OpenHour,labels = dataset$Mod_OpenHour )+
    theme(panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(),
          legend.position="right",
          panel.background=element_rect(fill="white",colour="black",size=0.5,linetype = 1),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 60, vjust = 1,size = 10, hjust = 1),
          strip.text.x = element_text(face = "bold.italic")) + scale_fill_discrete(name = "Shift")
  
  return(Plot)
  
}

#######################################################
#Get Hourly Distribution by Priority plot: 
get_hourly_tck_distribution_priority <- function(dataset)
{
  library(ggplot2)
  
  dataset$Mod_OpenHour <- factor(dataset$Mod_OpenHour,levels = c("6","7","8","9","10","11","12","13",
                                                                 "14","15","16","17","18","19","20","21",
                                                                 "22","23","0","1","2","3","4","5"))
  
  Plot <- ggplot(dataset,aes(Mod_OpenHour,Count,fill=Mod_Shift)) + 
    geom_bar(stat = "identity",width = 0.5)+
    ggtitle("Priority by Hourly Arrival") + 
    xlab("Hour(CST)")+
    ylab("Count of tickets")+
    geom_text(aes(label=Count),hjust=1,vjust=0.1,size=3,angle=90) +
    scale_x_discrete(breaks=dataset$Mod_OpenHour,labels = dataset$Mod_OpenHour )+
    theme(panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(),
          legend.position="right",
          panel.background=element_rect(fill="white",colour="black",size=0.5,linetype = 1),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 60, vjust = 1,size = 10, hjust = 1),
          strip.text.x = element_text(face = "bold.italic")) + scale_fill_discrete(name = "Shift") +
    facet_wrap(~m_Tck_Priority)
  
  return(Plot)
}

#######################################################
#Get Monthly Distribution Plot:
get_monthly_tck_distribution <- function(dataset)
{
  library(ggplot2)
  library(ggrepel)
  library(lubridate)
  library(zoo)
  
  dataset$Mod_MMYYY <- as.yearmon(dmy(paste("01-", dataset$Mod_MMYYY , sep ="")),"%b-%Y")
  
  Plot <- ggplot(dataset,aes(x=as.factor(Mod_MMYYY),y=Count,group=unique)) +
    geom_line() + geom_point()+ 
    theme_bw() +
    geom_text_repel(aes(label=Count),size=3) +
    labs(title="Monthly Arrival",y="Total Tickets",x="Months") +
    theme(title=element_text(face = "bold.italic"),
          plot.title = element_text(hjust=0.5),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          legend.position="none",
          axis.text.x=element_text(angle = 45, hjust = 0.9, vjust = 0.9,size = 10)) + 
    geom_hline(aes(yintercept = mean(dataset$Count),color="red")) + 
    geom_text(aes(x=as.factor(unique(dataset$Mod_MMYYY)[round(length(unique(dataset$Mod_MMYYY))/2)]),y=mean(dataset$Count),
                  label=paste("Mean =",round(mean(dataset$Count),2)),
                  color="red",vjust=-0.1),size=3)
  
  return(Plot)
}

#######################################################
#Get Monthly Distribution by Priority Plot:
get_monthly_tck_distribution_priority <- function(dataset)
{
  library(ggplot2)
  library(ggrepel)
  library(lubridate)
  library(zoo)
  
  dataset$Mod_MMYYY <- as.yearmon(dmy(paste("01-", dataset$Mod_MMYYY , sep ="")),"%b-%Y")
  
  Plot <- ggplot(dataset,aes(as.factor(Mod_MMYYY),Count,group=m_Tck_Priority)) + 
    geom_line() + geom_point()+
    ggtitle("Priority by Month Arrival") + 
    ylab("Count of tickets")+
    geom_text_repel(aes(label=Count),size=3)+
    theme(panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(),
          legend.position="right",
          panel.background=element_rect(fill="white",colour="black",size=0.5,linetype = 1),
          panel.grid.minor = element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = element_text(angle = 60, vjust = 1,size = 9, hjust = 1),
          strip.text.x = element_text(face = "bold.italic")) +facet_wrap(~m_Tck_Priority)
  
  return(Plot)
}

#######################################################
#Get Weekly Distribution Plot:
get_weekday_tck_distribution <- function(dataset)
{
  library(ggplot2)
  library(ggrepel)
  
  dataset$Mod_OpenWeekDay <- factor(dataset$Mod_OpenWeekDay,levels=c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))
  
  Plot <- ggplot(dataset,aes(Mod_OpenWeekDay,Count,group=unique)) + 
    geom_line() + geom_point()+
    ggtitle("WeekDays Arrival") + 
    xlab("WeekDays")+
    ylab("Count of tickets")+
    geom_text_repel(aes(label=Count),size=3)+
    theme(panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(),
          legend.position="right",
          panel.background=element_rect(fill="white",colour="black",size=0.5,linetype = 1),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 60, vjust = 1,size = 9, hjust = 1),
          strip.text.x = element_text(face = "bold.italic"))
  
  return(Plot)
}

######################################################
#Get Weekly Distribution by Month Plot:
get_weekday_tck_distribution_month <- function(dataset)
{
  library(ggplot2)
  library(ggrepel)
  
  dataset$Mod_MMYYY <- as.yearmon(dmy(paste("01-", dataset$Mod_MMYYY , sep ="")),"%b-%Y")
  dataset$Mod_OpenWeekDay <- factor(dataset$Mod_OpenWeekDay,levels=c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))

  Plot <- ggplot(dataset,aes(Mod_OpenWeekDay,Count,group=Mod_MMYYY)) + 
    geom_line() + geom_point()+
    ggtitle("WeekDays by Monthly Arrival") + 
    xlab("WeekDays")+
    ylab("Count of tickets")+
    geom_text_repel(aes(label=Count),size=3)+
    theme(panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(),
          legend.position="right",
          panel.background=element_rect(fill="white",colour="black",size=0.5,linetype = 1),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 60, vjust = 1,size = 9, hjust = 1),
          strip.text.x = element_text(face = "bold.italic")) +facet_wrap(~Mod_MMYYY)
  
  return(Plot)
}

######################################################
#App Distribution Plot
get_application_tck_distribution <- function(dataset,top_App_count)
{
  library(ggplot2)
  library(ggrepel)
  
  dataset$m_Tck_App_Name <- factor(dataset$m_Tck_App_Name,
                                     levels = unique(dataset[order(dataset$Count,decreasing=T),"m_Tck_App_Name"]))
  
  Plot <- ggplot(dataset,aes(m_Tck_App_Name,Prt,fill=m_Tck_App_Name)) + geom_bar(stat="identity") +
    geom_point(y=dataset$CumPrt)+
    geom_line(y=dataset$CumPrt,group="1",color="darkblue",linetype = "dashed")+
    ggtitle("Top Contributing Applications") + 
    ylab("Tickets(%)")+
    geom_text(aes(label=Prt),size=3,angle=90,hjust=0)+
    geom_text(aes(y=dataset$CumPrt,label=CumPrt),size=3,angle=90,hjust=-0.1)+
    theme(legend.position="none",
          panel.background=element_rect(fill="white",colour="black",size=0.5,linetype = 1),
          panel.grid.minor = element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5,size = 9, hjust = 1),
          strip.text.y = element_text(face = "bold.italic",angle=0)) +ylim(0,100)
  
  return(Plot)
  
}
######################################################
