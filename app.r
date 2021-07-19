dataSet=read.csv("Data/Consol2.csv")
dataSet=dataSet[,-c(1,2)]
library(shiny)
library(tidyverse)
library(scales)
library(GGally)
library(questionr)
library(shinyWidgets)
library(dplyr)
library(palmerpenguins)
library(gtsummary)
library(gt)
library(vcd)
library(ggpubr)
library(Hmisc)
library(hrbrthemes)
library(viridis)
library(glue)
theme_gtsummary_language("fr",decimal.mark = ",",big.mark = "")
#a=dataSet

ui <- fluidPage(
  headerPanel("Consol 2"),
  sidebarPanel(
    tabsetPanel(id = "tabset",
                tabPanel("Une variable",
                         selectInput("UnVar", "Variable", names(dataSet), selected = "V001.Sexe"),
                         textInput("Titre_", label="Titre", placeholder = "ex : Graohique de ..."),
                         textInput("Xaxis", label="X-axis", placeholder = "ex : Sexe"),
                         textInput("Yaxis", label="Y-axis", placeholder = "ex : effectifs"),
                         textInput("TiLege", label="Titre legend", placeholder = "ex : PCS"),
                         textInput("legendd", label="Legends", placeholder = "ex : Homme, Femmes, autre "),
                ),
                tabPanel("Deux Variables",
                         pickerInput("slsp", "Choisir", choices = c("Table croisée","Margin - Mosaicplot"), selected =NULL, options = list('actions-box' = TRUE), multiple = F),
                         selectInput("row", "Variable en line", names(dataSet), selected = "V001.Sexe"),
                         actionButton("toInverse","Inverser"),
                         selectInput("column", "Variable rn column", names(dataSet), selected = "V006.Pcs"),
                         textInput("Titre_1", label="Titre", placeholder = "ex : effectifs"),
                         textInput("Xaxis1", label="X-axis", placeholder = "ex : effectifs"),
                         textInput("Yaxis1", label="Y-axis", placeholder = "ex : effectifs"),
                         textInput("TiLege1", label="Titre legend", placeholder = "ex : effectifs"),
                         textInput("legendd1", label="Legends", placeholder = "ex : Homme, Femmes, "),
                ),
                tabPanel("Trois Variables",
                         pickerInput("var3Prem", "Variable 1 en tete  :", choices = c(names(dataSet)), selected ="V001.Sexe", options = list('actions-box' = TRUE), multiple = F),
                         pickerInput("var3Sec", "Variable 2 integré (Y-axis):", choices = c(names(dataSet)), selected ="V006.Pcs", options = list('actions-box' = TRUE), multiple = F),
                         pickerInput("var3Troi", "Variable 3 coté gauche (X-axis)", choices = c(names(dataSet)), selected ="V004.StatutMatrimonial", options = list('actions-box' = TRUE), multiple = F),
                         textInput("Titre_", label="Titre", placeholder = "ex : effectifs"),
                         textInput("Xaxis", label="X-axis", placeholder = "ex : effectifs"),
                         textInput("Yaxis", label="Y-axis", placeholder = "ex : effectifs"),
                         textInput("TiLege", label="Titre legend", placeholder = "ex : effectifs"),
                         textInput("legendd", label="Legends", placeholder = "ex : Homme, Femmes, "),
                ),
                tabPanel("Ensemble des tables",
                         pickerInput("by_", "Choisir la variable :", choices = c(names(dataSet)), selected ="", options = list('actions-box' = TRUE), multiple = F),
                         pickerInput("varTables", "Par rapport aux : ", choices = c(names(dataSet)), selected =c("V001.Sexe","V006.Pcs"), options = list('actions-box' = TRUE), multiple = T),
                         pickerInput("percent_", "Pourcentage", choices = c("row","col"), selected ="row", options = list('actions-box' = TRUE), multiple = F),
                         pickerInput("only_", "Sortis", choices = c("Les deux","Numbres","Poucentage"), selected ="Les deux", options = list('actions-box' = TRUE), multiple = F),
                         actionButton("butUpdate","Update"),
                         checkboxInput("pasgraphe","Graphes ? " , F),
                         textInput("Titre_", label="Titre", placeholder = "ex : effectifs"),
                         textInput("TiLege", label="Titre legend", placeholder = "ex : effectifs"),
                         textInput("legendd", label="Legends", placeholder = "ex : Homme, Femmes, "),
                ),
                tabPanel("Box plot et Pyramide",
                         pickerInput("box1", "Variable 1 Quanti", choices = c(names(dataSet)), selected ="V002.Age", options = list('actions-box' = TRUE), multiple = F),
                         pickerInput("box2", "Variable 2 Quali(plusieurs modelités)", choices = c(names(dataSet)), selected ="V006.Pcs", options = list('actions-box' = TRUE), multiple = F),
                         pickerInput("box3", "Variable 3 Quali (2 modalités pour la Pyramide)", choices = c(names(dataSet)), selected ="V001.Sexe", options = list('actions-box' = TRUE), multiple = F),
                         
                         textInput("Titre_5", label="Titre", placeholder = "ex : effectifs"),
                         textInput("Xaxis5", label="X-axis", placeholder = "ex : effectifs"),
                         textInput("Yaxis5", label="Y-axis", placeholder = "ex : effectifs"),
                         textInput("TiLege5", label="Titre legend", placeholder = "ex : effectifs"),
                         textInput("legendd5", label="Legends", placeholder = "ex : Homme, Femmes, "),
                )
    ),checkboxInput("pasNa","Pas de NA" , F),
    
    sliderInput("Tit1", label="Taille de Titre", min = 5,max=40 ,value = 25),
    sliderInput("Tit2", label="Taille de X-axis",  min = 5,max=40,value = 15),
    sliderInput("Tit3", label="Taille de Y-axis", min = 5,max=40,value = 15),
    sliderInput("Tit4", label="Taille de Titre de legend",  min = 5,max=40,value = 16),
    sliderInput("Tit5", label="Taille des legends", min = 5,max=40,value = 15),
  ),
  #main page 
  mainPanel(
    
    conditionalPanel(
      condition = "input.tabset  == 'Une variable' ",
      h2("Effectifs"),
      h4(textOutput("selected_var1")),
      tableOutput('tableVar1'),
      plotOutput("plot2var1"),
      plotOutput("plot2var2"),
    ),
    conditionalPanel(
      condition = "input.tabset  == 'Deux Variables' ",
      conditionalPanel(
        condition = "input.slsp == 'Table croisée'   ",
        h3('Cross table'),
        tableOutput('effectif12'),
        checkboxInput("AffichText","Afficher le text" , T),
        plotOutput('plot2effecti'),
        plotOutput("plotplus2"),
        textOutput("SummaryPva"),
      ),
      conditionalPanel(
        condition = " input.slsp  == 'Margin - Mosaicplot'",
        h4("Pourcentage en ligne en %"),
        tableOutput('tableMarginRow'),
        h4("Pourcentage en col en %"),
        tableOutput('tableMarginCol'),
        plotOutput("plot2MarginRow"),
        plotOutput("plot2pour"),
        
      ),
    ),
    
    conditionalPanel(
      
      condition ="input.tabset  == 'Ensemble des tables' ",
      h3('Tables croisées'),
      gt_output('tables33'),
      plotOutput("plot2pour22"),
      
    ),
    conditionalPanel(
      condition ="input.tabset  == 'Trois Variables' ",
      h3('Trois tables croisées'),
      checkboxInput("show_tabl","Voir la table",F),
      tableOutput('Table3var'),
      plotOutput('plot3var2'),
      plotOutput("plot3var"),
      plotOutput('plot3varMozi'),
      
    ),
    conditionalPanel(
      condition ="input.tabset  == 'Box plot et Pyramide' ",
      h3('Box plot et  Pyramide'),
      radioButtons("boxvilon","choisissez la representation :",
                   choices = list("Violin"="vio","BoxPlot"="box"),selected = "vio"),
      plotOutput("boxPlot"),
      plotOutput('Pyramide'),
    )
  )
)

server <- function(input, output,session) {
  #Data pour NA
  dataNa<- reactive({
    if(input$pasNa){
      subset(dataSet,!is.na(dataSet[,input$column])&!is.na(dataSet[,input$row]))
    }else{dataSet}
  })
  #table à deux variables 
  xtable<- reactive({
    xtabs(as.formula(paste0("~",input$row,"+",input$column)), dataNa(),addNA = T)
  })
  
  
  #___________________________________________________________________________________________
  #Une variable 
  output$selected_var1 <- renderText({ 
    paste(input$UnVar,' :')
    #paste(as.vector(input$slsssp)[1])
  })
  output$tableVar1 <- renderTable(
    addmargins(xtabs(as.formula(paste0("~",input$UnVar)), dataNa(),addNA = !input$pasNa),margin=1)
    , striped=TRUE, bordered = TRUE,rownames = T)
  
  output$plot2var1<- renderPlot({
    
    ggplot(dataNa(),aes_string(x = input$UnVar, fill =input$UnVar)) + geom_bar()+
      theme(
        plot.title = element_text( size=as.numeric(input$Tit1)),
        axis.title.x = element_text( size=as.numeric(input$Tit2)),
        axis.title.y = element_text(size=as.numeric(input$Tit3 )),
        legend.title = element_text( size = as.numeric(input$Tit4)),
        legend.text = element_text( size = as.numeric(input$Tit5)) 
      )+
      labs(y= ifelse(input$Yaxis=="",input$UnVar ,isolate({input$Yaxis}))
           , x=ifelse(input$Xaxis=="","Effectif", isolate({input$Xaxis}))
           ,fill=ifelse(input$TiLege=="",input$UnVar, isolate({input$TiLege})),
           title= ifelse(input$Titre_  ==""," ", isolate({input$Titre_}))
      )
  })
  
  output$plot2var2<- renderPlot({ 
    count.data=as.data.frame(prop.table(xtabs(as.formula(paste0("~",input$UnVar)),
                                              dataNa(),addNA = !input$pasNa)))
    count.data$Freq=round(count.data$Freq*100,2)
    ggplot(count.data, aes(x = "", y = Freq, fill =  count.data[,input$UnVar])) +
      geom_bar(width = 1, stat = "identity") +
      coord_polar("y")+geom_text(aes( label = Freq))+
      theme(
        plot.title = element_text( size=as.numeric(input$Tit1)),
        axis.title.x = element_text( size=as.numeric(input$Tit2)),
        axis.title.y = element_text(size=as.numeric(input$Tit3 )),
        legend.title = element_text( size = as.numeric(input$Tit4)),
        legend.text = element_text( size = as.numeric(input$Tit5)) 
      )+
      labs(y= ifelse((input$Yaxis=="")," " ,isolate(input$Yaxis))
           , x=ifelse((input$Xaxis==""),"Effectif", isolate(input$Xaxis))
           ,fill=ifelse((input$TiLege==""),input$UnVar, isolate(input$TiLege)),
           title= ifelse(input$Titre_==""," ", isolate({input$Titre_}))
      )+
      theme_minimal()
  })
  
  #deux variables
  output$effectif12 <- renderTable(
    as.data.frame.matrix(addmargins(xtable()))
    , striped=TRUE, bordered = TRUE,rownames = T)
  
  output$plot2effecti<- renderPlot({
    ggplot(dataNa()) +
      aes_string(x = input$row, fill = input$column) +
      geom_bar() +
      theme(
        plot.title = element_text( size=as.numeric(input$Tit1)),
        axis.title.x = element_text( size=as.numeric(input$Tit2)),
        axis.title.y = element_text(size=as.numeric(input$Tit3 )),
        legend.title = element_text( size = as.numeric(input$Tit4)),
        legend.text = element_text( size = as.numeric(input$Tit5)) 
      )+
      labs(y= ifelse(input$Yaxis1=="","Effectifs",isolate(input$Yaxis1))
           , x=ifelse(input$Xaxis1=="",input$row, isolate(input$Xaxis1))
           ,fill=ifelse(input$TiLege1=="",input$column, isolate(input$TiLege1)),
           title= ifelse(input$Titre_1==""," ", isolate({input$Titre_1})))+
      if (input$AffichText){isolate({geom_text(aes(label = after_stat(count)), stat = "count", position = position_stack(.5))})}
  })
  output$plotplus2<-renderPlot(
    ggplot(data=dataNa(), aes_string(x=input$row, fill=input$column)) +
      geom_bar( position=position_dodge())+coord_flip() +
      theme(
        plot.title = element_text( size=as.numeric(input$Tit1)),
        axis.title.x = element_text( size=as.numeric(input$Tit2)),
        axis.title.y = element_text(size=as.numeric(input$Tit3 )),
        legend.title = element_text( size = as.numeric(input$Tit4)),
        legend.text = element_text( size = as.numeric(input$Tit5)) 
      )+
      labs(y= ifelse(input$Yaxis1=="","Effectifs",isolate(input$Yaxis1))
           , x=ifelse(input$Xaxis1=="",input$row, isolate(input$Xaxis1))
           ,fill=ifelse(input$TiLege1=="",input$column, isolate(input$TiLege1)),
           title= ifelse(input$Titre_1==""," ", isolate({input$Titre_1})))
  )
  output$SummaryPva<-renderPrint({
    summ= summary(xtable())
    cat(paste0("Les variables sont ",ifelse(summ$p.value>=0.05,"independentes","dependentes"),
               "
         ",", Number of cases in table: " ,summ$n.cases ,
               "
         \n" ,", Number of factors: " ,summ$n.vars ,
               "
         \n", ", Test for independence of all factors: ",
               "
         \n", ", Chisq = ", summ$statistic, ", df = ", summ$parameter,", p-value = ", summ$p.value,""))
  })
  #Trois variables
  
  output$tableMarginCol <- renderTable(as.data.frame.matrix(cprop(xtable())), striped=TRUE, bordered = TRUE,rownames = T)
  output$plot2MarginRow<- renderPlot({
    mosaicplot(data=dataSet,dataSet[,input$column]~dataSet[,input$row],shade=T,main = "Mosaicplot",ylab=input$row,xlab= input$column )
  })
  
  
  output$tableMarginRow <- renderTable(as.data.frame.matrix(lprop(xtable())), striped=TRUE, bordered = TRUE,rownames = T)
  
  output$plot2pour<- renderPlot({
    
    ggplot(dataNa())+aes_string(x =input$column, fill = input$row) + geom_bar(position = "fill") +xlab(input$column) +
      theme(
        plot.title = element_text( size=as.numeric(input$Tit1)),
        axis.title.x = element_text( size=as.numeric(input$Tit2)),
        axis.title.y = element_text(size=as.numeric(input$Tit3 )),
        legend.title = element_text( size = as.numeric(input$Tit4)),
        legend.text = element_text( size = as.numeric(input$Tit5)) 
      )+
      ylab("Pourcentage")+ scale_y_continuous(labels = scales::percent) #+geom_text(aes(by = a[,input$row]), stat = "prop", position = position_fill(.5))
    
  })
  
  #ensemble de tables
  output$tables33<- render_gt({
    # a_list=list("Les deux"="{n}({p}%)","Numbres"="{n}","Poucentage"="{p}%")
    if (!v$doPlot) return()
    as_gt(tbl_summary(dataSet,
                      include=press()[[1]],
                      by=press()[[2]],
                      percent = press()[[3]],
                      statistic = list(all_categorical()~ press()[[4]]),
                      missing = ifelse(input$pasNa,"no","ifany")
    )%>% add_overall(last = T)%>%add_p())
  })
  
  output$plot2pour22<-renderPlot({
    if(input$pasgraphe){
      
      ggbivariate(data = dataNa(), outcome =input$by_, explanatory = as.vector(input$varTables))+
        labs(#y= ifelse(input$Yaxis=="","Effectifs",isolate(input$Yaxis)),
          #x=ifelse(input$Xaxis=="",input$row, isolate(input$Xaxis)),
          fill=ifelse(input$TiLege=="",input$column, isolate(input$TiLege)),
          title= ifelse(input$Titre_==""," ", isolate({input$Titre_})))}
  })
  # variables 3 
  
  
  output$plot3var<- renderPlot({
    
    ggplot(data3var55(), aes_string(x =input$var3Troi,y = "Freq"))+
      geom_bar(
        aes_string(fill =input$var3Sec),stat = "identity", color = "white",
        position = position_dodge(0.9)
      )+
      theme(
        plot.title = element_text( size=as.numeric(input$Tit1)),
        axis.title.x = element_text( size=as.numeric(input$Tit2)),
        axis.title.y = element_text(size=as.numeric(input$Tit3 )),
        legend.title = element_text( size = as.numeric(input$Tit4)),
        legend.text = element_text( size = as.numeric(input$Tit5)) 
      )+
      facet_grid(data3var55()[,input$var3Prem]~.)+labs(y="Effectifs",x=names(data3var55()[3]),
                                                       title=names(data3var55()[1]))+coord_flip()+ 
      fill_palette("jco")
  })
  
  output$plot3var2<- renderPlot({
    
    ggballoonplot(data3var55(), x = input$var3Sec, y = input$var3Troi, size = "Freq",
                  fill = "Freq", facet.by =input$var3Prem,
                  ggtheme = theme_bw()) +
      theme(
        plot.title = element_text( size=as.numeric(input$Tit1)),
        axis.title.x = element_text( size=as.numeric(input$Tit2)),
        axis.title.y = element_text(angle = 90,size=as.numeric(input$Tit3 )),
        legend.title = element_text( size = as.numeric(input$Tit4)),
        legend.text = element_text( size = as.numeric(input$Tit5)) 
      )+
      scale_fill_viridis_c(option = "C")+labs(y=input$var3Troi,x=input$var3Sec,
                                              title=input$var3Prem)
    
    
  })
  output$plot3varMozi<- renderPlot({
    mosaic(data3var(), shade = TRUE, legend = TRUE) 
    
  })
  data3var55<- reactive({as.data.frame(data3var()) })
  data3var<- reactive({
    choix=c(input$var3Prem,input$var3Sec,input$var3Troi)
    data99=dataSet[complete.cases(dataSet[,choix]),choix]
    struct <- structable(data99)
    return(struct)
  })
  
  
  
  
  
  
  output$Table3var <- renderTable(if(input$show_tabl)as.data.frame.matrix(data3var()),striped=TRUE, bordered = TRUE,rownames = T)
  
  # Box plot 
  output$boxPlot<-renderPlot({
    plot_1<-switch (input$boxvilon,
                    "box" = geom_boxplot(),
                    "vio"=geom_violin())
    
    data87=dataNa()
    data87[,input$box1]=as.numeric(data87[,input$box1])
    data87%>% ggplot( aes_string(x=input$box3, y=input$box1, fill=input$box2)) +
      scale_fill_viridis(discrete = T,alpha = 0.6,option="A")+
      plot_1+
      theme_ipsum() +
      
      theme(
        plot.title = element_text( size=as.numeric(input$Tit1)),
        axis.title.x = element_text( size=as.numeric(input$Tit2)),
        axis.title.y = element_text(size=as.numeric(input$Tit3 )),
        legend.title = element_text( size = as.numeric(input$Tit4)),
        legend.text = element_text( size = as.numeric(input$Tit5)) 
      )+labs(y= ifelse(input$Yaxis5=="",input$box1 ,isolate({input$Yaxis5}))
             , x=ifelse(input$Xaxis5=="",input$box3, isolate({input$Xaxis5}))
             ,fill=ifelse(input$TiLege5=="",input$box2, isolate({input$TiLege5})),
             title= ifelse(input$Titre_5  ==""," ", isolate({input$Titre_5}))
      )
  })
  
  output$Pyramide<-renderPlot({
    DataPymida=dataSet
    Bi=input$box3
    Type=input$box1
    Inclu=input$box2
    DataPymida[,Type]=as.numeric(DataPymida[,Type])
    
    pyTable=xtabs(as.formula(paste0("~",Type,"+",Bi,"+",Inclu)), DataPymida)
    
    Pyr_data=as.data.frame(pyTable)
    Pyr_data["FreqPour"]=Pyr_data$Freq/sum(Pyr_data$Freq)
    Pyr_data["FreqPourM"]=-Pyr_data$Freq/sum(Pyr_data$Freq)
    
    the_max=0
    for(i in 1:length(unique(Pyr_data[,Inclu]))){
      if(Bi==Inclu){
        maxi =max(subset(Pyr_data,Pyr_data[,Inclu]==unique(Pyr_data[,Inclu])[i],FreqPour))
        the_max=the_max-maxi}
      maxi =max(subset(Pyr_data,Pyr_data[,Inclu]==unique(Pyr_data[,Inclu])[i],FreqPour))
      the_max=the_max+maxi
    }
    string_="{unique(Pyr_data[,Bi])[1]}                   {unique(Pyr_data[,Bi])[2]}"
    
    ggplot(Pyr_data, aes_string(x=Type),show.legend = T) +
      geom_bar(data=Pyr_data[Pyr_data[,Bi]==unique(Pyr_data[,Bi])[1],], 
               aes_string(y="FreqPourM", fill=Inclu),position = position_stack(reverse=F),
               stat="identity",show.legend = T) +
      geom_bar(data=Pyr_data[Pyr_data[,Bi]==unique(Pyr_data[,Bi])[2],],
               position = position_stack(reverse=F),
               aes_string(y="FreqPour", fill=Inclu), stat="identity",show.legend = T) +
      geom_hline(yintercept=0, colour="white", lwd=1) +
      coord_flip(ylim=c(-the_max,the_max)) + scale_y_continuous(breaks=round(seq(-1,1,.01),2),
                                                                labels= round(abs(seq(-1,1,.01)),2))+
      
      labs(y= ifelse(input$Xaxis5=="",glue(string_) ,isolate({input$Xaxis5}))
           , x=ifelse(input$Yaxis5=="",Type, isolate({input$Yaxis5}))
           ,fill=ifelse(input$TiLege5=="",Inclu, isolate({input$TiLege5})),
           title= ifelse(input$Titre_5  ==""," ", isolate({input$Titre_5}))
      )+
      theme(
        plot.title = element_text( size=as.numeric(input$Tit1)),
        axis.title.x = element_text( size=as.numeric(input$Tit2)),
        axis.title.y = element_text(size=as.numeric(input$Tit3 )),
        legend.title = element_text( size = as.numeric(input$Tit4)),
        legend.text = element_text( size = as.numeric(input$Tit5)) )
  })
  
  #button 
  v <- reactiveValues(doPlot = FALSE)
  
  observeEvent(input$butUpdate, {
    v$doPlot <- input$butUpdate
  })
  
  observeEvent(
    input$toInverse,{
      ddd=input$row
      updateSelectInput(session,"row",selected =input$column )
      updateSelectInput(session,"column",selected =ddd )
      
    })
  a_list=list("Les deux"="{n}({p}%)","Numbres"="{n}","Poucentage"="{p}%")
  press<-eventReactive(
    input$butUpdate,{
      list(as.vector(input$varTables),input$by_,input$percent_,as.character(a_list[input$only_]))
    })
  
  
}
                                  
                                  
                                  
shinyApp(ui = ui, server = server)





