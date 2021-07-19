dataSet=read.csv("Data/Consol2.csv")
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
dataSet[dataSet==" "]=NA
dataSet[dataSet=="NA"]=NA
shinyApp(ui = ui, server = server)





