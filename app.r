dataSet=read.csv("Consol2.csv")
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





