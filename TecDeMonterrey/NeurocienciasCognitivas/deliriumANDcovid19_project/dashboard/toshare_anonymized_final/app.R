library(shiny)
library(openxlsx)
library(tidyverse)
library(xfun)
library("mxmaps")
library("viridis")

delirium_db <- read.xlsx('data.xlsx','Escalas')
delirium_db$Marca.temporal <- convertToDate(delirium_db$Marca.temporal, origin = "1900-01-01")
delirium_db$Marcatemp_sinhora <- convertToDate(delirium_db$Marcatemp_sinhora, origin = "1900-01-01")
delirium_db$Fecha_ingreso <- convertToDate(delirium_db$Fecha_ingreso, origin = "1900-01-01")
delirium_db$Fecha_escala <- as.Date(delirium_db$Fecha_escala, format =  "%m/%d/%Y")

delirium_db$ID <- as.numeric(as.character(delirium_db$ID))
delirium_db_sort <- arrange(delirium_db, ID, Marca.temporal)
delirium_db_sort_onlyID <- delirium_db_sort[-c(1:3,15312:15333),]

delirium_db_sort_onlyID <- mutate(delirium_db_sort_onlyID, num_4AT=NA)
for(k in unique(delirium_db_sort_onlyID$ID)) {
  delirium_db_sort_onlyID$num_4AT[delirium_db_sort_onlyID$ID==k & !is.na(delirium_db_sort_onlyID$`4AT_inter`)] <- 1:sum(delirium_db_sort_onlyID$ID==k & !is.na(delirium_db_sort_onlyID$`4AT_inter`), na.rm=TRUE)
}

delirium_db_sort_onlyID <- mutate(delirium_db_sort_onlyID, num_CAM_ICU=NA)
for(k in unique(delirium_db_sort_onlyID$ID)) {
  delirium_db_sort_onlyID$num_CAM_ICU[delirium_db_sort_onlyID$ID==k & !is.na(delirium_db_sort_onlyID$`CAM-ICU_inter`)] <- 1:sum(delirium_db_sort_onlyID$ID==k & !is.na(delirium_db_sort_onlyID$`CAM-ICU_inter`), na.rm=TRUE)
}

delirium_db_sort_onlyID <- mutate(delirium_db_sort_onlyID, num_Dolor_num=NA)
for(k in unique(delirium_db_sort_onlyID$ID)) {
  delirium_db_sort_onlyID$num_Dolor_num[delirium_db_sort_onlyID$ID==k & !is.na(delirium_db_sort_onlyID$Dolor_num_raw)] <- 1:sum(delirium_db_sort_onlyID$ID==k & !is.na(delirium_db_sort_onlyID$Dolor_num_raw), na.rm=TRUE)
}

delirium_db_sort_onlyID <- mutate(delirium_db_sort_onlyID, num_Dolor_cond=NA)
for(k in unique(delirium_db_sort_onlyID$ID)) {
  delirium_db_sort_onlyID$num_Dolor_cond[delirium_db_sort_onlyID$ID==k & !is.na(delirium_db_sort_onlyID$Dolor_cond_raw)] <- 1:sum(delirium_db_sort_onlyID$ID==k & !is.na(delirium_db_sort_onlyID$Dolor_cond_raw), na.rm=TRUE)
}

delirium_db_sort_onlyID <- mutate(delirium_db_sort_onlyID, num_RCSQ=NA)
for(k in unique(delirium_db_sort_onlyID$ID)) {
  delirium_db_sort_onlyID$num_RCSQ[delirium_db_sort_onlyID$ID==k & !is.na(delirium_db_sort_onlyID$RCSQ_inter)] <- 1:sum(delirium_db_sort_onlyID$ID==k & !is.na(delirium_db_sort_onlyID$RCSQ_inter), na.rm=TRUE)
}

delirium_db_sort_onlyID <- mutate(delirium_db_sort_onlyID, num_NEWS2=NA)
for(k in unique(delirium_db_sort_onlyID$ID)) {
  delirium_db_sort_onlyID$num_NEWS2[delirium_db_sort_onlyID$ID==k & !is.na(delirium_db_sort_onlyID$NEWS2_inter)] <- 1:sum(delirium_db_sort_onlyID$ID==k & !is.na(delirium_db_sort_onlyID$NEWS2_inter), na.rm=TRUE)
}

delirium_db_sort_onlyID <- mutate(delirium_db_sort_onlyID, num_HADS=NA)
for(k in unique(delirium_db_sort_onlyID$ID)) {
  delirium_db_sort_onlyID$num_HADS[delirium_db_sort_onlyID$ID==k & !is.na(delirium_db_sort_onlyID$HADS_D_inter)] <- 1:sum(delirium_db_sort_onlyID$ID==k & !is.na(delirium_db_sort_onlyID$HADS_D_inter), na.rm=TRUE)
}

delirium_db_sort_onlyID <- mutate(delirium_db_sort_onlyID,Dolor_num_nominal=NA)
delirium_db_sort_onlyID$Dolor_num_nominal[delirium_db_sort_onlyID$Dolor_num_raw==0]<- "very satisfied"
delirium_db_sort_onlyID$Dolor_num_nominal[delirium_db_sort_onlyID$Dolor_num_raw>=1 & delirium_db_sort_onlyID$Dolor_num_raw<=5]<- "satisfied"
delirium_db_sort_onlyID$Dolor_num_nominal[delirium_db_sort_onlyID$Dolor_num_raw>=6 & delirium_db_sort_onlyID$Dolor_num_raw<=10]<- "not satisfied"

delirium_db_sort_onlyID <- mutate(delirium_db_sort_onlyID,Dolor_cond_nominal=NA)
delirium_db_sort_onlyID$Dolor_cond_nominal[delirium_db_sort_onlyID$Dolor_cond_raw==3]<- "very satisfied"
delirium_db_sort_onlyID$Dolor_cond_nominal[delirium_db_sort_onlyID$Dolor_cond_raw>=4 & delirium_db_sort_onlyID$Dolor_cond_raw<=5]<- "satisfied"
delirium_db_sort_onlyID$Dolor_cond_nominal[delirium_db_sort_onlyID$Dolor_cond_raw>=6 ]<- "not satisfied"

delirium_registro <- read.xlsx('data.xlsx','Registro')
delirium_registro$Marca.temporal <- convertToDate(delirium_registro$Marca.temporal, origin = "1900-01-01")
delirium_registro$Marca.temporal <- as.Date(format(delirium_registro$Marca.temporal, format="%Y-%m-%d"))

data("df_mxstate_2020")

intervenciones <- read.xlsx('data.xlsx','Intervenciones')
intervenciones$Marca.temporal <- convertToDate(intervenciones$Marca.temporal, origin = "1900-01-01")
intervenciones$Marca.temporal <- as.Date(format(intervenciones$Marca.temporal, format="%Y-%m-%d"))

intervenciones$ID <- as.numeric(intervenciones$ID)

delirium_news2 <- read.xlsx('data.xlsx','NEWS2')
delirium_news2$Marca.temporal <- convertToDate(delirium_news2$Marca.temporal, origin = "1900-01-01")
delirium_news2$Marca.temporal <- as.Date(format(delirium_news2$Marca.temporal, format="%Y-%m-%d"))

delirium_notas <- read.xlsx('data.xlsx','Notas')
delirium_notas$Marca.temporal <- convertToDate(delirium_notas$Marca.temporal, origin = "1900-01-01")
delirium_notas$Marca.temporal <- as.Date(format(delirium_notas$Marca.temporal, format="%Y-%m-%d"))

delirium_altas <- read.xlsx('data.xlsx','Altas')
delirium_altas$Marca.temporal <- convertToDate(delirium_altas$Marca.temporal, origin = "1900-01-01")
delirium_altas$Marca.temporal <- as.Date(format(delirium_altas$Marca.temporal, format="%Y-%m-%d"))



ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      absolutePanel(
        wellPanel(checkboxGroupInput("place", "Hotel",
                                    c("Building A" = "Building A",
                                      "Building B" = "Building B"),
                                    selected=c("Building A", "Building B")),
                 dateRangeInput("dates","Date range:",
                                start="2013-06-01",
                                end="2014-10-31")),fixed=TRUE)),
    mainPanel(
      tabsetPanel(
        tabPanel(title="1st service",
          fluidRow(
            column(
              width = 6,
              plotOutput("serviceA",height="200px")
            ),
            column(
              width = 6,
              plotOutput("serviceB",height="200px")
            )
          ),
          fluidRow(
            column(
              width = 6,
              plotOutput("serviceC",height="200px")
            ),
            column(
              width = 6,
              plotOutput("serviceD",height="200px")
            )
          ),
          fluidRow(
            column(
              width = 6,
              plotOutput("serviceE",height="200px")
            ),
            column(
              width = 6,
              plotOutput("serviceF",height="200px")
            )
          ),
          fluidRow(
            column(
              width = 6,
              plotOutput("sleep_quality",height="200px")
            ),
            column(
              width = 6,
              plotOutput("time_outsideRoom",height="200px")
            )
          ),
          fluidRow(
            column(
              width = 6,
              plotOutput("serviceG",height="200px")
            ),
            column(
              width = 6,
              plotOutput("serviceG_goal",height="200px")
            )
          )
        ),
        tabPanel(title="Longitudinal",
          fluidRow(
            plotOutput("serviceA_long_bar",height="400px")
          ),
          fluidRow(
            plotOutput("serviceA_long_line",height="200px")
          ),
          hr(style = "border-top: 1px solid #000000;"),
          fluidRow(
            plotOutput("serviceB_long_bar",height="400px")
          ),
          hr(style = "border-top: 1px solid #000000;"),
          fluidRow(
            plotOutput("serviceC_long_bar",height="400px")
          ),
          fluidRow(
            plotOutput("serviceC_long_line",height="200px")
          ),
          hr(style = "border-top: 1px solid #000000;"),
          fluidRow(
            plotOutput("serviceD_long_bar",height="400px")
          ),
          fluidRow(
            plotOutput("serviceD_long_line",height="200px")
          ),
          hr(style = "border-top: 1px solid #000000;"),
          fluidRow(
            plotOutput("serviceE_long_bar",height="400px")
          ),
          fluidRow(
            plotOutput("serviceE_long_line",height="200px")
          ),
          hr(style = "border-top: 1px solid #000000;"),
          fluidRow(
            plotOutput("serviceF_long_bar",height="400px")
          ),
          fluidRow(
            plotOutput("serviceF_long_line",height="200px")
          ),
          hr(style = "border-top: 1px solid #000000;"),
          fluidRow(
            plotOutput("sleep_quality_long_bar",height="400px")
          ),
          fluidRow(
            plotOutput("sleep_quality_long_line",height="200px")
          ),
          hr(style = "border-top: 1px solid #000000;"),
          fluidRow(
            plotOutput("time_outsideRoom_long_bar",height="400px")
          ),
          fluidRow(
            plotOutput("time_outsideRoom_long_line",height="200px")
          ),
          hr(style = "border-top: 1px solid #000000;"),
          fluidRow(
            plotOutput("serviceG_long_bar",height="400px")
          ),
          fluidRow(
            plotOutput("serviceG_long_line",height="200px")
          )
        ),
        tabPanel(
          title = "Demographics",
          fluidRow(
            column(
              width = 6,
              plotOutput("sex",height="200px")
            ),
            column(
              width = 6,
              plotOutput("age",height="200px")
              )
            ),
            fluidRow(
              column(
                width = 6,
                plotOutput("estatus",height="200px")
              ),
              column(
                width = 6,
                plotOutput("edocivil",height="200px")
              )
            ),
            fluidRow(
              column(
                width = 6,
                plotOutput("mapA",height="200px")
              ),
              column(
                width = 6,
                plotOutput("mapB",height="200px")
              )
            ),
            fluidRow(
              column(
                width = 6,
                plotOutput("edu",height="200px")
              ),
              column(
                width = 6,
                plotOutput("reli",height="200px")
              )
            ),
            fluidRow(
              column(
                width = 6,
                plotOutput("ocu",height="200px")
              ),
              column(
                width = 6,
                plotOutput("rewards",height="200px")
              )
            ),
            fluidRow(
              column(
                width = 6,
                plotOutput("bags",height="200px")
              ),
              column(
                width = 6,
                plotOutput("income",height="200px")
              )
            )
          ),
        tabPanel(
          title = "Amenities",
          fluidRow(
            column(
              width = 6,
              plotOutput("amenityA",height="200px")
            ),
            column(
              width = 6,
              plotOutput("amenityB",height="200px")
            )
          ),
          fluidRow(
            column(
              width = 6,
              plotOutput("amenityC",height="200px")
            ),
            column(
              width = 6,
              plotOutput("amenityD",height="200px")
            )
          ),
          fluidRow(
            column(
              width = 6,
              plotOutput("amenityE",height="200px")
            ),
            column(
              width = 6,
              plotOutput("amenityF",height="200px")
            )
          ),
          fluidRow(
            column(
              width = 6,
              plotOutput("amenityG",height="200px")
            )
          )
          ),
        tabPanel(
          title = "Products",
          fluidRow(
            column(
              width = 6,
              plotOutput("hygiene_products_A",height="200px")
            ),
            column(
              width = 6,
              plotOutput("hygiene_products_B",height="200px")
            )
          ),
          fluidRow(
            column(
              width = 6,
              plotOutput("food_products_A",height="200px")
            ),
            column(
              width = 6,
              plotOutput("food_products_B",height="200px")
            )
          ),
          fluidRow(
            column(
              width = 6,
              plotOutput("drinks_A",height="200px")
            ),
            column(
              width = 6,
              plotOutput("drinks_B",height="200px")
            )
          ),
          fluidRow(
            column(
              width = 6,
              plotOutput("hygiene_products_A_perGuest",height="200px")
            ),
            column(
              width = 6,
              plotOutput("hygiene_products_B_perGuest",height="200px")
            )
          ),
          fluidRow(
            column(
              width = 6,
              plotOutput("food_products_A_perGuest",height="200px")
            ),
            column(
              width = 6,
              plotOutput("food_products_B_perGuest",height="200px")
            )
          )
        ),
        tabPanel(
          title = "Data input personnel",
          fluidRow(
            column(
              width = 9,
              plotOutput("evalplot")
            ),
            column(
              width = 3,
              selectInput("evaluador",label="Employee:",
                          choices = sort(unique(delirium_registro$`Colaborador.que.registra.al.paciente:`))
              )
            )
          )
        )
        )
      )
    )
  )


server <- function(input, output) {
  num_entry_1_fourAT_r <- reactive({
    num_entry_1_fourAT <- filter(delirium_db_sort_onlyID, num_4AT == 1)
    num_entry_1_fourAT %>%
      filter(`Place` == input$place[1] | `Place` == input$place[2]) %>%
      filter(Marca.temporal >= input$dates[1] & Marca.temporal <= input$dates[2])
  })
  
  num_entry_1_fourAT_r_freq <- reactive({
    num_entry_1_fourAT_r() %>%
      filter(`4AT_inter` != 'NA') %>%
      mutate(`4AT_inter` = factor(`4AT_inter`, levels=c("very satisfied", "satisfied", "not satisfied"))) %>%
      group_by(`Place`) %>%
      summarise(Freq = n())
  })
  
  num_entry_1_CAM_r <- reactive({
    num_entry_1_CAM <- filter(delirium_db_sort_onlyID, num_CAM_ICU == 1)
    num_entry_1_CAM %>%
      filter(`Place` == input$place[1] | `Place` == input$place[2]) %>%
      filter(Marca.temporal >= input$dates[1] & Marca.temporal <= input$dates[2])
  })
  
  num_entry_1_CAM_r_freq <- reactive({
    num_entry_1_CAM_r() %>%
      filter(`CAM-ICU_inter` != 'NA') %>%
      mutate(`CAM-ICU_inter` = factor(`CAM-ICU_inter`, levels=c("not evaluated", "very satisfied", "not satisfied"))) %>%
      group_by(`Place`) %>%
      summarise(Freq = n())
  })
  
  num_entry_1_DolNum_r <- reactive({
    num_entry_1_DolNum <- filter(delirium_db_sort_onlyID, num_Dolor_num == 1)
    num_entry_1_DolNum <- mutate(num_entry_1_DolNum,Dolor_num_nominal=NA)
    num_entry_1_DolNum$Dolor_num_nominal[num_entry_1_DolNum$Dolor_num_raw==0]<- "very satisfied"
    num_entry_1_DolNum$Dolor_num_nominal[num_entry_1_DolNum$Dolor_num_raw>=1 & num_entry_1_DolNum$Dolor_num_raw<=5]<- "satisfied"
    num_entry_1_DolNum$Dolor_num_nominal[num_entry_1_DolNum$Dolor_num_raw>=6 & num_entry_1_DolNum$Dolor_num_raw<=10]<- "not satisfied"
    num_entry_1_DolNum <- mutate(num_entry_1_DolNum, Dolor_num_nominal = factor(Dolor_num_nominal, levels=c("very satisfied", "satisfied","not satisfied")))
    num_entry_1_DolNum %>%
      filter(`Place` == input$place[1] | `Place` == input$place[2]) %>%
      filter(Marca.temporal >= input$dates[1] & Marca.temporal <= input$dates[2])
  })
  
  num_entry_1_DolNum_r_freq <- reactive({
    num_entry_1_DolNum_r() %>%
      filter(Dolor_num_nominal != 'NA') %>%
      group_by(`Place`) %>%
      summarise(Freq = n())
  })
  
  num_entry_1_DolCond_r <- reactive({
    num_entry_1_DolCond <- filter(delirium_db_sort_onlyID, num_Dolor_cond == 1)
    num_entry_1_DolCond <- mutate(num_entry_1_DolCond,Dolor_cond_nominal=NA)
    num_entry_1_DolCond$Dolor_cond_nominal[num_entry_1_DolCond$Dolor_cond_raw==3]<- "very satisfied"
    num_entry_1_DolCond$Dolor_cond_nominal[num_entry_1_DolCond$Dolor_cond_raw>=4 & num_entry_1_DolCond$Dolor_cond_raw<=5]<- "satisfied"
    num_entry_1_DolCond$Dolor_cond_nominal[num_entry_1_DolCond$Dolor_cond_raw>=6 ]<- "not satisfied"
    num_entry_1_DolCond <- mutate(num_entry_1_DolCond, Dolor_cond_nominal = factor(Dolor_cond_nominal, levels=c("very satisfied", "satisfied","not satisfied")))
    num_entry_1_DolCond %>%
      filter(`Place` == input$place[1] | `Place` == input$place[2]) %>%
      filter(Marca.temporal >= input$dates[1] & Marca.temporal <= input$dates[2])
  })
  
  num_entry_1_DolCond_r_freq <- reactive({
    num_entry_1_DolCond_r() %>%
      filter(Dolor_cond_nominal != 'NA') %>%
      group_by(`Place`) %>%
      summarise(Freq = n())
  })
  
  num_entry_1_RCSQ_r <- reactive({
    num_entry_1_RCSQ <- filter(delirium_db_sort_onlyID, num_RCSQ == 1)
    num_entry_1_RCSQ %>%
      filter(`Place` == input$place[1] | `Place` == input$place[2]) %>%
      filter(Marca.temporal >= input$dates[1] & Marca.temporal <= input$dates[2])
  })
  
  num_entry_1_RCSQ_r_freq <- reactive({
    num_entry_1_RCSQ_r() %>%
      filter(RCSQ_inter != 'NA') %>%
      mutate(RCSQ_inter = factor(RCSQ_inter, levels=c("very satisfied", "satisfied", "not satisfied"))) %>%
      group_by(`Place`) %>%
      summarise(Freq = n())
  })
  
  num_entry_1_NEWS2_r <- reactive({
    num_entry_1_NEWS2 <- filter(delirium_db_sort_onlyID, num_NEWS2 == 1)
    num_entry_1_NEWS2 %>%
      filter(`Place` == input$place[1] | `Place` == input$place[2]) %>%
      filter(Marca.temporal >= input$dates[1] & Marca.temporal <= input$dates[2])
  })
  
  num_entry_1_NEWS2_r_freq <- reactive({
    num_entry_1_NEWS2_r() %>%
      filter(NEWS2_inter != 'NA') %>%
      mutate(NEWS2_inter = factor(NEWS2_inter, levels=c("very satisfied", "NA", "satisfied","not satisfied"))) %>%
      group_by(`Place`) %>%
      summarise(Freq = n())
  })
  
  num_entry_1_HADS_r <- reactive({
    num_entry_1_HADS <- filter(delirium_db_sort_onlyID, num_HADS == 1)
    num_entry_1_HADS %>%
      filter(`Place` == input$place[1] | `Place` == input$place[2]) %>%
      filter(Marca.temporal >= input$dates[1] & Marca.temporal <= input$dates[2])
  })
  
  num_entry_1_HADS_r_freq <- reactive({
    num_entry_1_HADS_r() %>%
      filter(HADS_D_inter != 'NA') %>%
      group_by(`Place`) %>%
      summarise(Freq = n())
  })
  
  num_entry_1_TamMeta_r <- reactive({
    num_entry_1_TamMeta <- filter(delirium_db_sort_onlyID, Input_method=="Service G")
    num_entry_1_TamMeta %>%
      filter(`Place` == input$place[1] | `Place` == input$place[2]) %>%
      filter(Marca.temporal >= input$dates[1] & Marca.temporal <= input$dates[2])
  })
  
  num_entry_1_Tam_r_freq <- reactive({
    num_entry_1_TamMeta_r() %>%
      filter(!is.na(K10_21omas_legend)) %>%
      mutate(K10_21omas_legend = factor(K10_21omas_legend, levels=c("satisfied", "not satisfied"))) %>%
      group_by(`Place`) %>%
      summarise(Freq = n())
  })
  
  num_entry_1_Meta_r_freq <- reactive({
    num_entry_1_TamMeta_r() %>%
      filter(Meta_cumplida_legend != 'NA') %>%
      mutate(Meta_cumplida_legend = factor(Meta_cumplida_legend, levels=c("Less than 24 hours", "More than 24 hours"))) %>%
      group_by(`Place`) %>%
      summarise(Freq = n())
  })
  
  output$serviceA <- renderPlot({
    num_entry_1_fourAT_r() %>%
      filter(`4AT_inter` != 'NA') %>%
      mutate(`4AT_inter` = factor(`4AT_inter`, levels=c("very satisfied", "satisfied", "not satisfied"))) %>%
      ggplot(aes(`Place`)) +
      geom_bar(position = "fill",aes(fill = `4AT_inter`)) +
      scale_y_continuous(labels = scales::percent) +
      scale_fill_manual(name = "Hotel guest:",values = c("#009E73", "#E7B800","#FF0000")) +
      ylab("Percentage") +
      xlab("") +
      geom_text(aes(fill = `4AT_inter`,label = scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..])),
                position = position_fill(vjust = 0.5),
                stat = "count")+
      ggtitle("Service A") +
      theme(plot.title = element_text(hjust = 0.5)) +
      geom_text(data = num_entry_1_fourAT_r_freq(), aes(label = paste("n=",Freq,sep=""), y=1), vjust = -.15,size = 3)
  })
  
  output$serviceB <- renderPlot({
    num_entry_1_CAM_r() %>%
      filter(`CAM-ICU_inter` != 'NA') %>%
      mutate(`CAM-ICU_inter` = factor(`CAM-ICU_inter`, levels=c("not evaluated", "very satisfied", "not satisfied"))) %>%
      ggplot(aes(`Place`)) +
      geom_bar(position = "fill",aes(fill = `CAM-ICU_inter`)) +
      scale_y_continuous(labels = scales::percent) +
      scale_fill_manual(name = "Hotel guest:",values = c("cadetblue","#009E73","#FF0000")) +
      ylab("Percentage") +
      xlab("") +
      geom_text(aes(fill = `CAM-ICU_inter`,label = scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..])),
                position = position_fill(vjust = 0.5),
                stat = "count")+
      ggtitle("Service B") +
      theme(plot.title = element_text(hjust = 0.5)) +
      geom_text(data = num_entry_1_CAM_r_freq(), aes(label = paste("n=",Freq,sep=""), y=1), vjust = -.15,size = 3)
  })
  
  output$serviceC <- renderPlot({
    num_entry_1_DolNum_r() %>%
      filter(Dolor_num_nominal != 'NA') %>%
      ggplot(aes(`Place`)) +
      geom_bar(position = "fill",aes(fill = Dolor_num_nominal)) +
      scale_y_continuous(labels = scales::percent) +
      scale_fill_manual(name = "Hotel guest:",values = c("#009E73", "#E7B800","#FF0000")) +
      ylab("Percentage") +
      xlab("") +
      geom_text(aes(fill = Dolor_num_nominal,label = scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..])),
                position = position_fill(vjust = 0.5),
                stat = "count")+
      ggtitle("Service C") +
      theme(plot.title = element_text(hjust = 0.5)) +
      geom_text(data = num_entry_1_DolNum_r_freq(), aes(label = paste("n=",Freq,sep=""), y=1), vjust = -.15,size = 3)
  })
  
  output$serviceD <- renderPlot({
    num_entry_1_DolCond_r() %>%
      filter(Dolor_cond_nominal != 'NA') %>%
      ggplot(aes(`Place`)) +
      geom_bar(position = "fill",aes(fill = Dolor_cond_nominal)) +
      scale_y_continuous(labels = scales::percent) +
      scale_fill_manual(name = "Hotel guest:",values = c("#009E73", "#E7B800","#FF0000")) +
      ylab("Percentage") +
      xlab("") +
      geom_text(aes(fill = Dolor_cond_nominal,label = scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..])),
                position = position_fill(vjust = 0.5),
                stat = "count")+
      ggtitle("Service D") +
      theme(plot.title = element_text(hjust = 0.5)) +
      geom_text(data = num_entry_1_DolCond_r_freq(), aes(label = paste("n=",Freq,sep=""), y=1), vjust = -.15,size = 3)
  })
  
  output$serviceE <- renderPlot({
    num_entry_1_RCSQ_r() %>%
      filter(RCSQ_inter != 'NA') %>%
      mutate(RCSQ_inter = factor(RCSQ_inter, levels=c("very satisfied", "satisfied", "not satisfied"))) %>%
      ggplot(aes(`Place`)) +
      geom_bar(position = "fill",aes(fill = RCSQ_inter)) +
      scale_y_continuous(labels = scales::percent) +
      scale_fill_manual(name = "Hotel guest:",values = c("#009E73", "#E7B800","#FF0000")) +
      ylab("Percentage") +
      xlab("") +
      geom_text(aes(fill = RCSQ_inter,label = scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..])),
                position = position_fill(vjust = 0.5),
                stat = "count")+
      ggtitle("Service E") +
      theme(plot.title = element_text(hjust = 0.5)) +
      geom_text(data = num_entry_1_RCSQ_r_freq(), aes(label = paste("n=",Freq,sep=""), y=1), vjust = -.15,size = 3)
  })
  
  output$serviceF <- renderPlot({
    num_entry_1_NEWS2_r() %>%
      filter(NEWS2_inter != 'NA') %>%
      mutate(NEWS2_inter = factor(NEWS2_inter, levels=c("very satisfied", "NA", "satisfied","not satisfied"))) %>%
      ggplot(aes(`Place`)) +
      geom_bar(position = "fill", aes(fill = NEWS2_inter)) +
      scale_y_continuous(labels = scales::percent) +
      scale_fill_manual(name = "Hotel guest:",values = c("#009E73", "#E7B800","#FF0000")) +
      ylab("Percentage") +
      xlab("") +
      geom_text(aes(fill = NEWS2_inter,label = scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..])),
                position = position_fill(vjust = 0.5),
                stat = "count")+
      ggtitle("Service F") +
      theme(plot.title = element_text(hjust = 0.5)) +
      geom_text(data = num_entry_1_NEWS2_r_freq(), aes(label = paste("n=",Freq,sep=""), y=1), vjust = -.15,size = 3)
  })
  
  output$sleep_quality <- renderPlot({
    num_entry_1_HADS_r() %>%
      filter(HADS_A_inter != 'NA') %>%
      mutate(HADS_A_inter = factor(HADS_A_inter, levels=c("high-quality sleep", "regular-quality sleep", "low-quality sleep"))) %>%
      ggplot(aes(`Place`)) +
      geom_bar(position = "fill", aes(fill = HADS_A_inter)) +
      scale_y_continuous(labels = scales::percent) +
      scale_fill_manual(name = "Hotel guest:",values = c("#009E73", "#E7B800","#FF0000")) +
      ylab("Percentage") +
      xlab("") +
      geom_text(aes(fill = HADS_A_inter,label = scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..])),
                position = position_fill(vjust = 0.5),
                stat = "count")+
      ggtitle("Quality of sleep") +
      theme(plot.title = element_text(hjust = 0.5)) +
      geom_text(data = num_entry_1_HADS_r_freq(), aes(label = paste("n=",Freq,sep=""), y=1), vjust = -.15,size = 3)
  })
  
  output$time_outsideRoom <- renderPlot({
    num_entry_1_HADS_r() %>%
      filter(HADS_D_inter != 'NA') %>%
      mutate(HADS_D_inter = factor(HADS_D_inter, levels=c("most time outside room", "moderate time outside room", "almost no time outside room"))) %>%
      ggplot(aes(`Place`)) +
      geom_bar(position = "fill", aes(fill = HADS_D_inter)) +
      scale_y_continuous(labels = scales::percent) +
      scale_fill_manual(name = "Hotel guest:",values = c("#009E73", "#E7B800","#FF0000")) +
      ylab("Percentage") +
      xlab("") +
      geom_text(aes(fill = HADS_D_inter,label = scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..])),
                position = position_fill(vjust = 0.5),
                stat = "count")+
      ggtitle("Time spent outside hotel room") +
      theme(plot.title = element_text(hjust = 0.5)) +
      geom_text(data = num_entry_1_HADS_r_freq(), aes(label = paste("n=",Freq,sep=""), y=1), vjust = -.15,size = 3)
  })
  
  
  output$serviceG <- renderPlot({
    num_entry_1_TamMeta_r() %>%
      filter(K10_21omas_legend != 'NA') %>%
      mutate(K10_21omas_legend = factor(K10_21omas_legend, levels=c("satisfied", "not satisfied"))) %>%
      ggplot(aes(`Place`)) +
      geom_bar(position = "fill", aes(fill = K10_21omas_legend)) +
      scale_y_continuous(labels = scales::percent) +
      scale_fill_manual(name = "Hotel guest:",values = c("#009E73", "#FF0000")) +
      ylab("Percentage") +
      xlab("") +
      geom_text(aes(fill = K10_21omas_legend,label = scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..])),
                position = position_fill(vjust = 0.5),
                stat = "count")+
      ggtitle("Service G") +
      theme(plot.title = element_text(hjust = 0.5)) +
      geom_text(data = num_entry_1_Tam_r_freq(), aes(label = paste("n=",Freq,sep=""), y=1), vjust = -.15,size = 3)
  })
  
  output$serviceG_goal <- renderPlot({
    num_entry_1_TamMeta_r() %>%
      filter(Meta_cumplida_legend != 'NA') %>%
      mutate(Meta_cumplida_legend = factor(Meta_cumplida_legend, levels=c("Less than 24 hours", "More than 24 hours"))) %>%
      ggplot(aes(`Place`)) +
      geom_bar(position = "fill",aes(fill = Meta_cumplida_legend)) +
      scale_y_continuous(labels = scales::percent) +
      scale_fill_manual(name = "Time:",values = c("#009E73", "#FF0000")) +
      ylab("Percentage") +
      xlab("") +
      geom_text(aes(fill = Meta_cumplida_legend,label = scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..])),
                position = position_fill(vjust = 0.5),
                stat = "count")+
      ggtitle("Time in which 1st service G was provided") +
      theme(plot.title = element_text(hjust = 0.5)) +
      geom_text(data = num_entry_1_Meta_r_freq(), aes(label = paste("n=",Freq,sep=""), y=1), vjust = -.15,size = 3)
  })
  
  
  delirium_db_sort_onlyID_fourAT <- reactive({
    delirium_db_sort_onlyID %>%
      mutate(grupo="Hotel guests") %>%
      mutate(`4AT_raw`=as.numeric(`4AT_raw`)) %>%
      filter(!is.na(`4AT_raw`)) %>%
      filter(`Place` == input$place[1] | `Place` == input$place[2]) %>%
      filter(Marca.temporal >= input$dates[1] & Marca.temporal <= input$dates[2]) %>%
      add_row(`Place` = "Building A",DEIH=0,`4AT_raw`=0) %>%
      filter(DEIH>=0 & DEIH <=20) %>%
      mutate(DEIH=as.factor(DEIH))
  })
  
  delirium_db_sort_onlyID_fourAT2 <- reactive({
    delirium_db_sort_onlyID_fourAT() %>%
      group_by(DEIH,`Place`) %>%
      summarise(sd=sd(`4AT_raw`),`4AT_raw` = mean(`4AT_raw`))
  })
  
  delirium_db_sort_onlyID_fourAT3 <- reactive({
    delirium_db_sort_onlyID_fourAT() %>%
      filter(`4AT_inter`=="not satisfied")
  })
  
  fourAT_freq <- reactive({
    delirium_db_sort_onlyID_fourAT() %>%
      filter(!is.na(`4AT_inter`)) %>%
      mutate(`4AT_inter` = factor(`4AT_inter`, levels=c("very satisfied", "satisfied", "not satisfied"))) %>%
      group_by(`Place`, DEIH) %>%
      summarise(Freq = n())
  })
  
  fourAT_sum <- reactive({
    delirium_db_sort_onlyID_fourAT() %>%
      filter(!is.na(`4AT_inter`)) %>%
      mutate(`4AT_inter` = factor(`4AT_inter`, levels=c("very satisfied", "satisfied", "not satisfied"))) %>%
      group_by(`Place`, DEIH, `4AT_inter`) %>%
      summarise(Freq = n()) %>%
      group_by(`Place`, DEIH) %>%
      mutate(Prop = Freq / sum(Freq))
  })

  delirium_db_sort_onlyID_CAM <- reactive({
    delirium_db_sort_onlyID %>%
      mutate(grupo="Hotel guests") %>%
      filter(!is.na(`CAM-ICU_inter`)) %>%
      filter(`Place` == input$place[1] | `Place` == input$place[2]) %>%
      filter(Marca.temporal >= input$dates[1] & Marca.temporal <= input$dates[2]) %>%
      filter(DEIH>=0 & DEIH <=20) %>%
      mutate(DEIH=as.factor(DEIH))
  })
  
  CAM_freq <- reactive({
    delirium_db_sort_onlyID_CAM() %>%
      filter(!is.na(`CAM-ICU_inter`)) %>%
      mutate(`CAM-ICU_inter` = factor(`CAM-ICU_inter`, levels=c("not evaluated", "very satisfied", "not satisfied"))) %>%
      group_by(`Place`, DEIH) %>%
      summarise(Freq = n())
  })
  
  CAM_sum <- reactive({
    delirium_db_sort_onlyID_CAM() %>%
      filter(!is.na(`CAM-ICU_inter`)) %>%
      mutate(`CAM-ICU_inter` = factor(`CAM-ICU_inter`, levels=c("not evaluated", "very satisfied", "not satisfied"))) %>%
      group_by(`Place`, DEIH, `CAM-ICU_inter`) %>%
      summarise(Freq = n()) %>%
      group_by(`Place`, DEIH) %>%
      mutate(Prop = Freq / sum(Freq))
  })
  
  delirium_db_sort_onlyID_dolnum <- reactive({
    delirium_db_sort_onlyID %>%
      mutate(grupo="Hotel guests") %>%
      mutate(Dolor_num_raw=as.numeric(Dolor_num_raw)) %>%
      filter(!is.na(Dolor_num_raw)) %>%
      filter(`Place` == input$place[1] | `Place` == input$place[2]) %>%
      filter(Marca.temporal >= input$dates[1] & Marca.temporal <= input$dates[2]) %>%
      add_row(`Place` = "Building A",DEIH=0,Dolor_num_raw=0) %>%
      filter(DEIH>=0 & DEIH <=20) %>%
      mutate(DEIH=as.factor(DEIH))
  })
  
  delirium_db_sort_onlyID_dolnum2 <- reactive({
    delirium_db_sort_onlyID_dolnum() %>%
      group_by(DEIH,`Place`) %>%
      summarise(sd=sd(Dolor_num_raw),Dolor_num_raw = mean(Dolor_num_raw))
  })
  
  delirium_db_sort_onlyID_dolnum3 <- reactive({
    delirium_db_sort_onlyID_dolnum() %>%
      filter(Dolor_num_raw >= 6)
  })
  
  dolnum_freq <- reactive({
    delirium_db_sort_onlyID_dolnum() %>%
      filter(!is.na(Dolor_num_nominal)) %>%
      mutate(Dolor_num_nominal = factor(Dolor_num_nominal, levels=c("very satisfied", "satisfied","not satisfied"))) %>%
      group_by(`Place`, DEIH) %>%
      summarise(Freq = n())
  })
  
  dolnum_sum <- reactive({
    delirium_db_sort_onlyID_dolnum() %>%
      filter(!is.na(Dolor_num_nominal)) %>%
      mutate(Dolor_num_nominal = factor(Dolor_num_nominal, levels=c("very satisfied", "satisfied","not satisfied"))) %>%
      group_by(`Place`, DEIH, Dolor_num_nominal) %>%
      summarise(Freq = n()) %>%
      group_by(`Place`, DEIH) %>%
      mutate(Prop = Freq / sum(Freq))
  })
  
  delirium_db_sort_onlyID_dolcond <- reactive({
    delirium_db_sort_onlyID %>%
      mutate(grupo="Hotel guests") %>%
      mutate(Dolor_cond_raw=as.numeric(Dolor_cond_raw)) %>%
      filter(!is.na(Dolor_cond_raw)) %>%
      filter(`Place` == input$place[1] | `Place` == input$place[2]) %>%
      filter(Marca.temporal >= input$dates[1] & Marca.temporal <= input$dates[2]) %>%
      add_row(`Place` = "Building A",DEIH=0,Dolor_cond_raw=3) %>%
      filter(DEIH>=0 & DEIH <=20) %>%
      mutate(DEIH=as.factor(DEIH))
  })
  
  delirium_db_sort_onlyID_dolcond2 <- reactive({
    delirium_db_sort_onlyID_dolcond() %>%
      group_by(DEIH,`Place`) %>%
      summarise(sd=sd(Dolor_cond_raw),Dolor_cond_raw = mean(Dolor_cond_raw))
  })
  
  delirium_db_sort_onlyID_dolcond3 <- reactive({
    delirium_db_sort_onlyID_dolcond() %>%
      filter(Dolor_cond_raw >= 6)
  })
  
  dolcond_freq <- reactive({
    delirium_db_sort_onlyID_dolcond() %>%
      filter(!is.na(Dolor_cond_nominal)) %>%
      mutate(Dolor_cond_nominal = factor(Dolor_cond_nominal, levels=c("very satisfied", "satisfied","not satisfied"))) %>%
      group_by(`Place`, DEIH) %>%
      summarise(Freq = n())
  })
  
  dolcond_sum <- reactive({
    delirium_db_sort_onlyID_dolcond() %>%
      filter(!is.na(Dolor_cond_nominal)) %>%
      mutate(Dolor_cond_nominal = factor(Dolor_cond_nominal, levels=c("very satisfied", "satisfied","not satisfied"))) %>%
      group_by(`Place`, DEIH, Dolor_cond_nominal) %>%
      summarise(Freq = n()) %>%
      group_by(`Place`, DEIH) %>%
      mutate(Prop = Freq / sum(Freq))
  })
  
  delirium_db_sort_onlyID_rcsq <- reactive({
    delirium_db_sort_onlyID %>%
      mutate(grupo="Hotel guests") %>%
      mutate(RCSQ_raw=as.numeric(RCSQ_raw)) %>%
      filter(!is.na(RCSQ_raw)) %>%
      filter(`Place` == input$place[1] | `Place` == input$place[2]) %>%
      filter(Marca.temporal >= input$dates[1] & Marca.temporal <= input$dates[2]) %>%
      add_row(`Place` = "Building A",DEIH=0,RCSQ_raw=100) %>%
      filter(DEIH>=0 & DEIH <=20) %>%
      mutate(DEIH=as.factor(DEIH))
  })
  
  delirium_db_sort_onlyID_rcsq2 <- reactive({
    delirium_db_sort_onlyID_rcsq() %>%
      group_by(DEIH,`Place`) %>%
      summarise(sd=sd(RCSQ_raw),RCSQ_raw = mean(RCSQ_raw))
  })
  
  delirium_db_sort_onlyID_rcsq3 <- reactive({
    delirium_db_sort_onlyID_rcsq() %>%
      filter(RCSQ_inter == "not satisfied" | RCSQ_inter == "satisfied")
  })
  
  rcsq_freq <- reactive({
    delirium_db_sort_onlyID_rcsq() %>%
      filter(!is.na(RCSQ_inter)) %>%
      mutate(RCSQ_inter = factor(RCSQ_inter, levels=c("very satisfied", "satisfied", "not satisfied"))) %>%
      group_by(`Place`, DEIH) %>%
      summarise(Freq = n())
  })
  
  rcsq_sum <- reactive({
    delirium_db_sort_onlyID_rcsq() %>%
      filter(!is.na(RCSQ_inter)) %>%
      mutate(RCSQ_inter = factor(RCSQ_inter, levels=c("very satisfied", "satisfied", "not satisfied"))) %>%
      group_by(`Place`, DEIH, RCSQ_inter) %>%
      summarise(Freq = n()) %>%
      group_by(`Place`, DEIH) %>%
      mutate(Prop = Freq / sum(Freq))
  })
  
  delirium_db_sort_onlyID_news <- reactive({
    delirium_db_sort_onlyID %>%
      mutate(grupo="Hotel guests") %>%
      mutate(NEWS2_raw=as.numeric(NEWS2_raw)) %>%
      filter(!is.na(NEWS2_raw)) %>%
      filter(`Place` == input$place[1] | `Place` == input$place[2]) %>%
      filter(Marca.temporal >= input$dates[1] & Marca.temporal <= input$dates[2]) %>%
      add_row(`Place` = "Building A",DEIH=0,NEWS2_raw=0) %>%
      filter(DEIH>=0 & DEIH <=20) %>%
      mutate(DEIH=as.factor(DEIH))
  })
  
  delirium_db_sort_onlyID_news2 <- reactive({
    delirium_db_sort_onlyID_news() %>%
      group_by(DEIH,`Place`) %>%
      summarise(sd=sd(NEWS2_raw),NEWS2_raw = mean(NEWS2_raw))
  })
  
  delirium_db_sort_onlyID_news3 <- reactive({
    delirium_db_sort_onlyID_news() %>%
      filter(NEWS2_inter == "satisfied" | NEWS2_inter == "not satisfied")
  })
  
  news_freq <- reactive({
    delirium_db_sort_onlyID_news() %>%
      filter(!is.na(NEWS2_inter)) %>%
      mutate(NEWS2_inter = factor(NEWS2_inter, levels=c("very satisfied", "NA", "satisfied","not satisfied"))) %>%
      group_by(`Place`, DEIH) %>%
      summarise(Freq = n())
  })
  
  news_sum <- reactive({
    delirium_db_sort_onlyID_news() %>%
      filter(!is.na(NEWS2_inter)) %>%
      mutate(NEWS2_inter = factor(NEWS2_inter, levels=c("very satisfied", "NA", "satisfied","not satisfied"))) %>%
      group_by(`Place`, DEIH, NEWS2_inter) %>%
      summarise(Freq = n()) %>%
      group_by(`Place`, DEIH) %>%
      mutate(Prop = Freq / sum(Freq))
  })
  
  delirium_db_sort_onlyID_hads <- reactive({
    delirium_db_sort_onlyID %>%
      mutate(grupo="Hotel guests") %>%
      mutate(HADS_A_raw=as.numeric(HADS_A_raw)) %>%
      mutate(HADS_D=as.numeric(HADS_D)) %>%
      filter(!is.na(HADS_A_raw)) %>%
      filter(`Place` == input$place[1] | `Place` == input$place[2]) %>%
      filter(Marca.temporal >= input$dates[1] & Marca.temporal <= input$dates[2]) %>%
      add_row(`Place` = "Building A",DEIH=0,HADS_A_raw=0,HADS_D=0) %>%
      filter(DEIH>=0 & DEIH <=20) %>%
      mutate(DEIH=as.factor(DEIH))
  })
  
  delirium_db_sort_onlyID_hadsA2 <- reactive({
    delirium_db_sort_onlyID_hads() %>%
      group_by(DEIH,`Place`) %>%
      summarise(sd=sd(HADS_A_raw),HADS_A_raw = mean(HADS_A_raw))
  })
  
  delirium_db_sort_onlyID_hadsA3 <- reactive({
    delirium_db_sort_onlyID_hads() %>%
      filter(HADS_A_inter == "regular-quality sleep" | HADS_A_inter == "low-quality sleep")
  })
  
  delirium_db_sort_onlyID_hadsD2 <- reactive({
    delirium_db_sort_onlyID_hads() %>%
      group_by(DEIH,`Place`) %>%
      summarise(sd=sd(HADS_D),HADS_D = mean(HADS_D))
  })
  
  delirium_db_sort_onlyID_hadsD3 <- reactive({
    delirium_db_sort_onlyID_hads() %>%
      filter(HADS_D_inter == "moderate time outside room" | HADS_D_inter == "almost no time outside room")
  })
  
  hadsA_freq <- reactive({
    delirium_db_sort_onlyID_hads() %>%
      filter(!is.na(HADS_A_inter)) %>%
      mutate(HADS_A_inter = factor(HADS_A_inter, levels=c("high-quality sleep", "regular-quality sleep", "low-quality sleep"))) %>%
      group_by(`Place`, DEIH) %>%
      summarise(Freq = n())
  })
  
  hadsA_sum <- reactive({
    delirium_db_sort_onlyID_hads() %>%
      filter(!is.na(HADS_A_inter)) %>%
      mutate(HADS_A_inter = factor(HADS_A_inter, levels=c("high-quality sleep", "regular-quality sleep", "low-quality sleep"))) %>%
      group_by(`Place`, DEIH, HADS_A_inter) %>%
      summarise(Freq = n()) %>%
      group_by(`Place`, DEIH) %>%
      mutate(Prop = Freq / sum(Freq))
  })
  
  hadsD_freq <- reactive({
    delirium_db_sort_onlyID_hads() %>%
      filter(!is.na(HADS_D_inter)) %>%
      mutate(HADS_D_inter = factor(HADS_D_inter, levels=c("most time outside room", "moderate time outside room", "almost no time outside room"))) %>%
      group_by(`Place`, DEIH) %>%
      summarise(Freq = n())
  })
  
  hadsD_sum <- reactive({
    delirium_db_sort_onlyID_hads() %>%
      filter(!is.na(HADS_D_inter)) %>%
      mutate(HADS_D_inter = factor(HADS_D_inter, levels=c("most time outside room", "moderate time outside room", "almost no time outside room"))) %>%
      group_by(`Place`, DEIH, HADS_D_inter) %>%
      summarise(Freq = n()) %>%
      group_by(`Place`, DEIH) %>%
      mutate(Prop = Freq / sum(Freq))
  })
  
  delirium_db_sort_onlyID_k10 <- reactive({
    delirium_db_sort_onlyID %>%
      mutate(grupo="Hotel guests") %>%
      mutate(K10_raw=as.numeric(K10_raw)) %>%
      mutate(DEIH=as.numeric(DEIH)) %>%
      filter(!is.na(K10_raw)) %>%
      filter(`Place` == input$place[1] | `Place` == input$place[2]) %>%
      filter(Marca.temporal >= input$dates[1] & Marca.temporal <= input$dates[2]) %>%
      add_row(`Place` = "Building A",DEIH=0,K10_raw=0) %>%
      filter(DEIH>=0 & DEIH <=20) %>%
      mutate(DEIH=factor(DEIH, levels=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)))
  })
  
  delirium_db_sort_onlyID_k102 <- reactive({
    delirium_db_sort_onlyID_k10() %>%
      group_by(DEIH,`Place`) %>%
      summarise(sd=sd(K10_raw),K10_raw = mean(K10_raw))
  })
  
  delirium_db_sort_onlyID_k103 <- reactive({
    delirium_db_sort_onlyID_k10() %>%
      filter(K10_raw >= 21)
  })
  
  k10_freq <- reactive({
    delirium_db_sort_onlyID_k10() %>%
      filter(!is.na(K10_21omas_legend)) %>%
      mutate(K10_21omas_legend = factor(K10_21omas_legend, levels=c("satisfied", "not satisfied"))) %>%
      group_by(`Place`, DEIH) %>%
      summarise(Freq = n())
  })
  
  k10_sum <- reactive({
    delirium_db_sort_onlyID_k10() %>%
      filter(!is.na(K10_21omas_legend)) %>%
      mutate(K10_21omas_legend = factor(K10_21omas_legend, levels=c("satisfied", "not satisfied"))) %>%
      group_by(`Place`, DEIH, K10_21omas_legend) %>%
      summarise(Freq = n()) %>%
      group_by(`Place`, DEIH) %>%
      mutate(Prop = Freq / sum(Freq))
  })
  
  output$serviceA_long_bar <- renderPlot({
    ggplot(fourAT_sum()) +
      aes(x = DEIH, y = Prop*100) +
      geom_col(aes(x = DEIH, y = Prop*100,fill = `4AT_inter`)) +
      scale_fill_manual(name = "Hotel guest:",values = c("#009E73", "#E7B800","#FF0000")) +
      ylab("Percentage") +
      xlab("Hotel stay day number")  +
      ggtitle("Service A") +
      #theme(plot.title = element_text(hjust = 0.5))+
      #geom_text(aes(label = paste(Freq,"(",scales::percent(Prop,accuracy= 1),")",sep="")), position = position_stack(1)) +
      geom_text(aes(fill = `4AT_inter`,label = scales::percent(Prop,accuracy= 0.1)), position = position_stack(.5),size = 3) + 
      facet_wrap(~ `Place`,dir="v") +
      geom_text(data = fourAT_freq(), aes(label = paste("n=",Freq,sep=""), y=100), vjust = -.15,size = 3)
  })
  
  output$serviceA_long_line <- renderPlot({
    delirium_db_sort_onlyID_fourAT2() %>%
      ggplot(aes(x=DEIH, y=`4AT_raw`, group=`Place`, color=`Place`)) + 
      geom_line(width=.5) +
      geom_point()  +
      ylab("Insatisfaction score") +
      xlab("Hotel stay day number")  +
      ggtitle("Service A") +
      geom_errorbar(aes(ymin=`4AT_raw`-sd, ymax=`4AT_raw`+sd), width=.5,
                    position=position_dodge(0.05)) #+
      #geom_jitter(data=delirium_db_sort_onlyID_fourAT3())
  })
  
  output$serviceC_long_bar <- renderPlot({
    ggplot(dolnum_sum()) +
      aes(x = DEIH, y = Prop*100) +
      geom_col(aes(x = DEIH, y = Prop*100,fill = Dolor_num_nominal)) +
      scale_fill_manual(name = "Hotel guest:",values = c("#009E73", "#E7B800","#FF0000")) +
      ylab("Percentage") +
      xlab("Hotel stay day number")  +
      ggtitle("Service C") +
      #theme(plot.title = element_text(hjust = 0.5))+
      #geom_text(aes(label = paste(Freq,"(",scales::percent(Prop,accuracy= 1),")",sep="")), position = position_stack(1)) +
      geom_text(aes(fill = Dolor_num_nominal,label = scales::percent(Prop,accuracy= 0.1)), position = position_stack(.5),size = 3) + 
      facet_wrap(~ `Place`,dir="v") +
      geom_text(data = dolnum_freq(), aes(label = paste("n=",Freq,sep=""), y=100), vjust = -.15,size = 3)
  })
  
  output$serviceC_long_line <- renderPlot({
    delirium_db_sort_onlyID_dolnum2() %>%
      ggplot(aes(x=DEIH, y=Dolor_num_raw, group=`Place`, color=`Place`)) + 
      geom_line() +
      geom_point()  +
      ylab("Insatisfaction score") +
      xlab("Hotel stay day number")  +
      ggtitle("Service C") +
      geom_errorbar(aes(ymin=Dolor_num_raw-sd, ymax=Dolor_num_raw+sd), width=.2,
                    position=position_dodge(0.05)) #+
      #geom_jitter(data=delirium_db_sort_onlyID_dolnum3())
  })
  
  output$serviceB_long_bar <- renderPlot({
    ggplot(CAM_sum()) +
      aes(x = DEIH, y = Prop*100) +
      geom_col(aes(x = DEIH, y = Prop*100,fill = `CAM-ICU_inter`)) +
      scale_fill_manual(name = "Hotel guest:",values = c("cadetblue","#009E73","#FF0000")) +
      ylab("Percentage") +
      xlab("Hotel stay day number")  +
      ggtitle("Service B") +
      #theme(plot.title = element_text(hjust = 0.5))+
      #geom_text(aes(label = paste(Freq,"(",scales::percent(Prop,accuracy= 1),")",sep="")), position = position_stack(1)) +
      geom_text(aes(fill = `CAM-ICU_inter`,label = scales::percent(Prop,accuracy= 0.1)), position = position_stack(.5),size = 3) + 
      facet_wrap(~ `Place`,dir="v") +
      geom_text(data = CAM_freq(), aes(label = paste("n=",Freq,sep=""), y=100), vjust = -.15,size = 3)
  })
  
  output$serviceD_long_bar <- renderPlot({
    ggplot(dolcond_sum()) +
      aes(x = DEIH, y = Prop*100) +
      geom_col(aes(x = DEIH, y = Prop*100,fill = Dolor_cond_nominal)) +
      scale_fill_manual(name = "Hotel guest:",values = c("#009E73", "#E7B800","#FF0000")) +
      ylab("Percentage") +
      xlab("Hotel stay day number")  +
      ggtitle("Service D") +
      #theme(plot.title = element_text(hjust = 0.5))+
      #geom_text(aes(label = paste(Freq,"(",scales::percent(Prop,accuracy= 1),")",sep="")), position = position_stack(1)) +
      geom_text(aes(fill = Dolor_cond_nominal,label = scales::percent(Prop,accuracy= 0.1)), position = position_stack(.5),size = 3) + 
      facet_wrap(~ `Place`,dir="v") +
      geom_text(data = dolcond_freq(), aes(label = paste("n=",Freq,sep=""), y=100), vjust = -.15,size = 3)
  })
  
  output$serviceD_long_line <- renderPlot({
    delirium_db_sort_onlyID_dolcond2() %>%
      ggplot(aes(x=DEIH, y=Dolor_cond_raw, group=`Place`, color=`Place`)) + 
      geom_line() +
      geom_point()  +
      ylab("Insatisfaction score") +
      xlab("Hotel stay day number")  +
      ggtitle("Service D") +
      geom_errorbar(aes(ymin=Dolor_cond_raw-sd, ymax=Dolor_cond_raw+sd), width=.2,
                    position=position_dodge(0.05)) #+
      #geom_jitter(data=delirium_db_sort_onlyID_dolcond3())
  })
  
  output$serviceE_long_bar <- renderPlot({
    ggplot(rcsq_sum()) +
      aes(x = DEIH, y = Prop*100) +
      geom_col(aes(x = DEIH, y = Prop*100,fill = RCSQ_inter)) +
      scale_fill_manual(name = "Hotel guest:",values = c("#009E73", "#E7B800","#FF0000")) +
      ylab("Percentage") +
      xlab("Hotel stay day number")  +
      ggtitle("Service E") +
      #theme(plot.title = element_text(hjust = 0.5))+
      #geom_text(aes(label = paste(Freq,"(",scales::percent(Prop,accuracy= 1),")",sep="")), position = position_stack(1)) +
      geom_text(aes(fill = RCSQ_inter,label = scales::percent(Prop,accuracy= 0.1)), position = position_stack(.5),size = 3) + 
      facet_wrap(~ `Place`,dir="v") +
      geom_text(data = rcsq_freq(), aes(label = paste("n=",Freq,sep=""), y=100), vjust = -.15,size = 3)
  })
  
  output$serviceE_long_line <- renderPlot({
    delirium_db_sort_onlyID_rcsq2() %>%
      ggplot(aes(x=DEIH, y=RCSQ_raw, group=`Place`, color=`Place`)) + 
      geom_line() +
      geom_point()  +
      ylab("Insatisfaction score") +
      xlab("Hotel stay day number")  +
      ggtitle("Service E") +
      geom_errorbar(aes(ymin=RCSQ_raw-sd, ymax=RCSQ_raw+sd), width=.2,
                    position=position_dodge(0.05)) #+
      #geom_jitter(data=delirium_db_sort_onlyID_rcsq3())
  })
  
  output$serviceF_long_bar <- renderPlot({
    ggplot(news_sum()) +
      aes(x = DEIH, y = Prop*100) +
      geom_col(aes(x = DEIH, y = Prop*100,fill = NEWS2_inter)) +
      scale_fill_manual(name = "Hotel guest:",values = c("#009E73", "#E7B800","#FF0000")) +
      ylab("Percentage") +
      xlab("Hotel stay day number")  +
      ggtitle("Service F") +
      #theme(plot.title = element_text(hjust = 0.5))+
      #geom_text(aes(label = paste(Freq,"(",scales::percent(Prop,accuracy= 1),")",sep="")), position = position_stack(1)) +
      geom_text(aes(fill = NEWS2_inter,label = scales::percent(Prop,accuracy= 0.1)), position = position_stack(.5),size = 3) + 
      facet_wrap(~ `Place`,dir="v") +
      geom_text(data = news_freq(), aes(label = paste("n=",Freq,sep=""), y=100), vjust = -.15,size = 3)
  })
  
  output$serviceF_long_line <- renderPlot({
    delirium_db_sort_onlyID_news2() %>%
      ggplot(aes(x=DEIH, y=NEWS2_raw, group=`Place`, color=`Place`)) + 
      geom_line() +
      geom_point()  +
      ylab("Insatisfaction score") +
      xlab("Hotel stay day number")  +
      ggtitle("Service F") +
      geom_errorbar(aes(ymin=NEWS2_raw-sd, ymax=NEWS2_raw+sd), width=.2,
                    position=position_dodge(0.05)) #+
      #geom_jitter(data=delirium_db_sort_onlyID_news3())
  })
  
  output$sleep_quality_long_bar <- renderPlot({
    ggplot(hadsA_sum()) +
      aes(x = DEIH, y = Prop*100) +
      geom_col(aes(x = DEIH, y = Prop*100,fill = HADS_A_inter)) +
      scale_fill_manual(name = "Hotel guest:",values = c("#009E73", "#E7B800","#FF0000")) +
      ylab("Percentage") +
      xlab("Hotel stay day number")  +
      ggtitle("Quality of sleep") +
      #theme(plot.title = element_text(hjust = 0.5))+
      #geom_text(aes(label = paste(Freq,"(",scales::percent(Prop,accuracy= 1),")",sep="")), position = position_stack(1)) +
      geom_text(aes(fill = HADS_A_inter,label = scales::percent(Prop,accuracy= 0.1)), position = position_stack(.5),size = 3) + 
      facet_wrap(~ `Place`,dir="v") +
      geom_text(data = hadsA_freq(), aes(label = paste("n=",Freq,sep=""), y=100), vjust = -.15,size = 3)
  })
  
  output$sleep_quality_long_line <- renderPlot({
    delirium_db_sort_onlyID_hadsA2() %>%
      ggplot(aes(x=DEIH, y=HADS_A_raw, group=`Place`, color=`Place`)) + 
      geom_line() +
      geom_point()  +
      ylab("Sleep quality score") +
      xlab("Hotel stay day number")  +
      ggtitle("Quality of sleep") +
      geom_errorbar(aes(ymin=HADS_A_raw-sd, ymax=HADS_A_raw+sd), width=.2,
                    position=position_dodge(0.05)) #+
      #geom_jitter(data=delirium_db_sort_onlyID_hadsA3())
  })
  
  output$time_outsideRoom_long_bar <- renderPlot({
    ggplot(hadsD_sum()) +
      aes(x = DEIH, y = Prop*100) +
      geom_col(aes(x = DEIH, y = Prop*100,fill = HADS_D_inter)) +
      scale_fill_manual(name = "Hotel guest:",values = c("#009E73", "#E7B800","#FF0000")) +
      ylab("Percentage") +
      xlab("Hotel stay day number")  +
      ggtitle("Time spent outside hotel room") +
      #theme(plot.title = element_text(hjust = 0.5))+
      #geom_text(aes(label = paste(Freq,"(",scales::percent(Prop,accuracy= 1),")",sep="")), position = position_stack(1)) +
      geom_text(aes(fill = HADS_D_inter,label = scales::percent(Prop,accuracy= 0.1)), position = position_stack(.5),size = 3) + 
      facet_wrap(~ `Place`,dir="v") +
      geom_text(data = hadsD_freq(), aes(label = paste("n=",Freq,sep=""), y=100), vjust = -.15,size = 3)
  })
  
  output$time_outsideRoom_long_line <- renderPlot({
    delirium_db_sort_onlyID_hadsD2() %>%
      ggplot(aes(x=DEIH, y=HADS_D, group=`Place`, color=`Place`)) + 
      geom_line() +
      geom_point()  +
      ylab("Hours") +
      xlab("Hotel stay day number")  +
      ggtitle("Time spent outside hotel room") +
      geom_errorbar(aes(ymin=HADS_D-sd, ymax=HADS_D+sd), width=.2,
                    position=position_dodge(0.05)) #+
      #geom_jitter(data=delirium_db_sort_onlyID_hadsD3())
  })
  
  output$serviceG_long_bar <- renderPlot({
    ggplot(k10_sum()) +
      aes(x = DEIH, y = Prop*100) +
      geom_col(aes(x = DEIH, y = Prop*100,fill = K10_21omas_legend)) +
      scale_fill_manual(name = "Hotel guest:",values = c("#009E73", "#FF0000")) +
      ylab("Percentage") +
      xlab("Hotel stay day number")  +
      ggtitle("Service G") +
      #theme(plot.title = element_text(hjust = 0.5))+
      #geom_text(aes(label = paste(Freq,"(",scales::percent(Prop,accuracy= 1),")",sep="")), position = position_stack(1)) +
      geom_text(aes(fill = K10_21omas_legend,label = scales::percent(Prop,accuracy= 0.1)), position = position_stack(.5),size = 3) + 
      facet_wrap(~ `Place`,dir="v") +
      geom_text(data = k10_freq(), aes(label = paste("n=",Freq,sep=""), y=100), vjust = -.15,size = 3)
  })
  
  output$serviceG_long_line <- renderPlot({
    delirium_db_sort_onlyID_k102() %>%
      ggplot(aes(x=DEIH, y=K10_raw, group=`Place`, color=`Place`)) + 
      geom_line() +
      geom_point()  +
      ylab("Insatisfaction score") +
      xlab("Hotel stay day number")  +
      ggtitle("Service G") +
      geom_errorbar(aes(ymin=K10_raw-sd, ymax=K10_raw+sd), width=.2,
                    position=position_dodge(0.05)) #+
      #geom_jitter(data=delirium_db_sort_onlyID_k103())
  })
  
  
  delirium_registro_r <- reactive({
    delirium_registro %>%
      filter(`Place` == input$place[1] | `Place` == input$place[2]) %>%
      filter(Marca.temporal >= input$dates[1] & Marca.temporal <= input$dates[2])
  })
  
  output$sex <- renderPlot({
    delirium_registro_r() %>%
      filter(Sex != 'NA') %>%
      mutate(Sex = factor(Sex, levels=c("female", "male"))) %>%
      ggplot(aes(`Place`, fill = Sex)) +
      geom_bar(position = "fill") +
      scale_y_continuous(labels = scales::percent) +
      ylab("Percentage") +
      xlab("") +
      geom_text(aes(label = scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..])),
                position = position_fill(vjust = 0.5),
                stat = "count")+
      ggtitle("Sex") +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$age <- renderPlot({
    delirium_registro_r() %>%
      mutate(`Edad:` = as.numeric(`Edad:`)) %>%
      filter(`Edad:` != 'NA') %>%
      filter(`Edad:` <= 100) %>%
      ggplot(aes(x=`Place`,y=`Edad:`,fill=`Place`)) + 
      geom_violin(trim=FALSE) +
      labs(x="",y = "Years") +
      geom_boxplot(width=0.1, fill="white") +
      theme_classic() +
      ggtitle("Age") +
      theme(plot.title = element_text(hjust = 0.5)) +
      labs(fill = "Guests in")
  })
  
  output$estatus <- renderPlot({
    delirium_registro_r() %>%
      filter(`Reason.for.visit` != 'NA') %>%
      mutate(`Reason.for.visit` = factor(`Reason.for.visit`, levels=c("business", "leisure"))) %>%
      ggplot(aes(`Place`, fill = `Reason.for.visit`)) +
      geom_bar(position = "fill") +
      scale_y_continuous(labels = scales::percent) +
      ylab("Percentage") +
      xlab("") +
      geom_text(aes(label = scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..])),
                position = position_fill(vjust = 0.5),
                stat = "count")+
      ggtitle("Reason for visit") +
      theme(plot.title = element_text(hjust = 0.5)) +
      labs(fill = "Reason for visit")
  })
  
  output$edocivil <- renderPlot({
    delirium_registro_r() %>%
      filter(Estado.civil != 'NA') %>%
      mutate(Estado.civil = factor(Estado.civil, levels=c("single", "married", "widowed", "divorced", "separated", "registered partnership"))) %>%
      ggplot(aes(`Place`, fill = Estado.civil)) +
      geom_bar(position = "fill") +
      scale_y_continuous(labels = scales::percent) +
      ylab("Percentage") +
      xlab("") +
      geom_text(aes(label = scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..])),
                position = position_fill(vjust = 0.5),
                stat = "count")+
      ggtitle("Marital status") +
      theme(plot.title = element_text(hjust = 0.5)) +
      labs(fill = "Marital status")
  })
  
  delirium_registro_r_AandB <- reactive({
    delirium_registro %>%
      filter(Marca.temporal >= input$dates[1] & Marca.temporal <= input$dates[2])
  })
  
  delirium_registro_r_A <- reactive({
    delirium_registro_r_AandB() %>%
      filter(`Place` == input$place[1])
  })
  
  delirium_registro_r_B <- reactive({
    if (length(input$place)==2) {
      delirium_registro_r_AandB() %>%
        filter(`Place` == input$place[2])
    }
  })
  
  output$mapA <- renderPlot({
    values <- as.data.frame(table(delirium_registro_r_A()$Residencia))
    colnames(values) <- c("state_name","value")
    df_mxstate_2020 <- merge(x=df_mxstate_2020,y=values,by="state_name",all.x=TRUE)
    df_mxstate_2020$value[is.na(df_mxstate_2020$value)] <- 0
    
    mxstate_choropleth(df_mxstate_2020, num_colors = 1,title = "Origin of guests in building A") +
      scale_fill_viridis("guests",option="H")
  })
  
  output$mapB <- renderPlot({
    if (length(input$place)==2) {
      values <- as.data.frame(table(delirium_registro_r_B()$Residencia))
      colnames(values) <- c("state_name","value")
      df_mxstate_2020 <- merge(x=df_mxstate_2020,y=values,by="state_name",all.x=TRUE)
      df_mxstate_2020$value[is.na(df_mxstate_2020$value)] <- 0
      
      mxstate_choropleth(df_mxstate_2020, num_colors = 1,title = "Origin of guests in building B") +
        scale_fill_viridis("guests",option="H")
    }
  })
  
  output$edu <- renderPlot({
    delirium_registro_r() %>%
      filter(Educación != 'NA') %>%
      mutate(Educación = factor(Educación, levels=c("none", "elementary", "middle school", "high school", "secondary school", "postgraduate"))) %>%
      ggplot(aes(`Place`, fill = Educación)) +
      geom_bar(position = "fill") +
      scale_y_continuous(labels = scales::percent) +
      ylab("Percentage") +
      xlab("") +
      geom_text(aes(label = scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..])),
                position = position_fill(vjust = 0.5),
                stat = "count")+
      ggtitle("Education") +
      theme(plot.title = element_text(hjust = 0.5)) +
      labs(fill = "Education")
  })
  
  output$reli <- renderPlot({
    delirium_registro_r() %>%
      filter(Religión != 'NA') %>%
      ggplot(aes(`Place`, fill = Religión)) +
      geom_bar(position = "fill") +
      scale_y_continuous(labels = scales::percent) +
      ylab("Percentage") +
      xlab("") +
      geom_text(aes(label = scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..])),
                position = position_fill(vjust = 0.5),
                stat = "count")+
      ggtitle("Religion") +
      theme(plot.title = element_text(hjust = 0.5)) +
      labs(fill = "Religion")
  })
  
  output$ocu <- renderPlot({
    delirium_registro_r() %>%
      filter(Ocupación != 'NA') %>%
      ggplot(aes(`Place`, fill = Ocupación)) +
      geom_bar(position = "fill") +
      scale_y_continuous(labels = scales::percent) +
      ylab("Percentage") +
      xlab("") +
      geom_text(aes(label = scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..])),
                position = position_fill(vjust = 0.5),
                stat = "count")+
      ggtitle("Occupation") +
      theme(plot.title = element_text(hjust = 0.5)) +
      labs(fill = "Occupation")
  })
  
  output$rewards <- renderPlot({
    delirium_registro_r() %>%
      mutate(`Peso.al.ingreso.(kg):` = as.numeric(`Peso.al.ingreso.(kg):`)) %>%
      filter(`Peso.al.ingreso.(kg):` != 'NA') %>%
      filter(`Peso.al.ingreso.(kg):` >= 10) %>%
      filter(`Peso.al.ingreso.(kg):` <= 400) %>%
      ggplot(aes(x=`Place`,y=`Peso.al.ingreso.(kg):`,fill=`Place`)) + 
      geom_violin(trim=FALSE) +
      labs(x="",y = "Reward points") +
      geom_boxplot(width=0.1, fill="white") +
      theme_classic() +
      ggtitle("Reward points at arrival") +
      theme(plot.title = element_text(hjust = 0.5)) +
      labs(fill = "Guests in")
  })
  
  output$bags <- renderPlot({
    delirium_registro_r() %>%
      mutate(`Talla.al.ingreso.(cm):` = as.numeric(`Talla.al.ingreso.(cm):`)) %>%
      filter(`Talla.al.ingreso.(cm):` != 'NA') %>%
      mutate(`Talla.al.ingreso.(cm):` = ifelse(`Talla.al.ingreso.(cm):`<2, `Talla.al.ingreso.(cm):`, `Talla.al.ingreso.(cm):`/100)) %>%
      filter(`Talla.al.ingreso.(cm):` > 0.3) %>%
      ggplot(aes(x=`Place`,y=`Talla.al.ingreso.(cm):`,fill=`Place`)) + 
      geom_violin(trim=FALSE) +
      labs(x="",y = "Number of bags") +
      geom_boxplot(width=0.1, fill="white") +
      theme_classic() +
      ggtitle("Number of bags at arrival") +
      theme(plot.title = element_text(hjust = 0.5)) +
      labs(fill = "Guests in")
  })
  
  output$income <- renderPlot({
    delirium_registro_r() %>%
      mutate(`Talla.al.ingreso.(cm):` = as.numeric(`Talla.al.ingreso.(cm):`)) %>%
      mutate(`Peso.al.ingreso.(kg):` = as.numeric(`Peso.al.ingreso.(kg):`)) %>%
      filter(`Talla.al.ingreso.(cm):` != 'NA') %>%
      filter(`Peso.al.ingreso.(kg):` != 'NA') %>%
      mutate(`Talla.al.ingreso.(cm):` = ifelse(`Talla.al.ingreso.(cm):`<2, `Talla.al.ingreso.(cm):`, `Talla.al.ingreso.(cm):`/100)) %>%
      filter(`Talla.al.ingreso.(cm):` > 0.3) %>%
      filter(`Peso.al.ingreso.(kg):` >= 10) %>%
      filter(`Peso.al.ingreso.(kg):` <= 400) %>%
      mutate(IMC = `Peso.al.ingreso.(kg):`/`Talla.al.ingreso.(cm):`^2) %>%
      filter(IMC <= 50) %>%
      filter(IMC >= 15) %>%
      ggplot(aes(x=`Place`,y=IMC,fill=`Place`)) + 
      geom_violin(trim=FALSE) +
      labs(x="",y = "Thousands of MXN") +
      geom_boxplot(width=0.1, fill="white") +
      theme_classic() +
      ggtitle("Monthly income of guests") +
      theme(plot.title = element_text(hjust = 0.5)) +
      geom_hline(yintercept=c(18.5,25,30,35,40), linetype="dashed") +
      annotate("text", x = c(2,2,2,2,2,2), y = c(17,22,27.5,32.5,37.5,42.5), hjust = 1,label = c("middle-low income      ",
                                                                          "middle income      ",
                                                                          "middle-high income      ",
                                                                          "high-low income      ",
                                                                          "high income      ",
                                                                          "high-high income       "
                                                                          ))
  })
  
  output$amenityA <- renderPlot({
    num_entry_1_NEWS2_r() %>%
      mutate(FR_raw = as.numeric(as.character(FR))) %>%
      filter(FR_raw>=12 & FR_raw<=38) %>%
      ggplot(aes(x=`Place`,y=FR_raw, fill=`Place`)) + 
      geom_violin(trim=FALSE)+
      labs(x="",y = "Minutes")+
      geom_boxplot(width=0.1, fill="white")+
      theme_classic()+
      ggtitle("Time spent in amenity A") +
      theme(plot.title = element_text(hjust = 0.5)) +
      labs(fill = "Guests in") #+
      #geom_hline(yintercept=c(12,20), linetype="dashed") +
      #annotate("text", x = c(2,2,2), y = c(10,16,22), hjust = 1,label = c("Bradipnea      ",
      #                                                                    "Normal      ",
      #                                                                    "Taquipnea      "))
  })
  
  output$amenityB <- renderPlot({
    num_entry_1_NEWS2_r() %>%
      mutate(Sat_raw = as.numeric(as.character(Sat))) %>%
      filter(Sat_raw>=75, Sat_raw<=100) %>%
      ggplot(aes(x=`Place`,y=Sat_raw, fill=`Place`)) + 
      geom_violin(trim=FALSE)+
      labs(x="",y = "Minutes")+
      geom_boxplot(width=0.1, fill="white")+
      theme_classic()+
      ggtitle("Time spent in amenity B") +
      theme(plot.title = element_text(hjust = 0.5)) +
      labs(fill = "Guests in")# +
      #geom_hline(yintercept=c(90,95), linetype="dashed") +
      #annotate("text", x = c(2,2,2), y = c(87.5,92.5,97.5), size=3, hjust = 1,label = c("Saturación baja      ",
      #                                                                                  "Saturación limítrofe      ",
      #                                                                                  "Normal      "))
  })
  
  output$amenityC <- renderPlot({
    num_entry_1_NEWS2_r() %>%
      mutate(FC_raw = as.numeric(as.character(FC))) %>%
      filter(FC_raw>26) %>%
      ggplot(aes(x=`Place`,y=FC_raw, fill=`Place`)) + 
      geom_violin(trim=FALSE)+
      labs(x="",y = "Minutes")+
      geom_boxplot(width=0.1, fill="white")+
      theme_classic()+
      ggtitle("Time spent in amenity C") +
      theme(plot.title = element_text(hjust = 0.5)) +
      labs(fill = "Guests in") #+
      #geom_hline(yintercept=c(50,100), linetype="dashed") +
      #annotate("text", x = c(2,2,2), y = c(45,75,105), hjust = 1,label = c("Bradicardia      ",
      #                                                                     "Normal      ",
      #                                                                     "Taquicardia      "))
  })
  
  output$amenityD <- renderPlot({
    num_entry_1_NEWS2_r() %>%
      mutate(Temp_raw = as.numeric(as.character(Temp))) %>%
      filter(Temp_raw>35 & Temp_raw<40) %>%
      ggplot(aes(x=`Place`,y=Temp_raw,fill=`Place`)) + 
      geom_violin(trim=FALSE)+
      labs(x="",y = "Minutes")+
      geom_boxplot(width=0.1, fill="white")+
      theme_classic()+
      ggtitle("Time spent in amenity D") +
      theme(plot.title = element_text(hjust = 0.5)) +
      labs(fill = "Guests in") #+
      #geom_hline(yintercept=c(35,37,38), linetype="dashed") +
      #annotate("text", x = c(2,2,2,2), y = c(34.5,36,37.5,38.5), hjust = 1,label = c("Hipotermia      ",
      #                                                                               "Normal      ",
      #                                                                               "Febrícula      ",
      #                                                                               "Fiebre      "))
  })
  
  output$amenityE <- renderPlot({
    num_entry_1_NEWS2_r() %>%
      mutate(PAsis_raw = as.numeric(as.character(PAsis))) %>%
      filter(PAsis_raw>57) %>%
      ggplot(aes(x=`Place`,y=PAsis_raw, fill=`Place`)) + 
      geom_violin(trim=FALSE)+
      labs(x="",y = "Minutes")+
      geom_boxplot(width=0.1, fill="white")+
      theme_classic()+
      ggtitle("Time spent in amenity E") +
      theme(plot.title = element_text(hjust = 0.5)) +
      labs(fill = "Guests in") #+
      #geom_hline(yintercept=c(90,120,130,140,180), linetype="dashed") +
      #annotate("text", x = c(2,2,2,2,2,2), y = c(85,105,125,135,160,185), size=3, hjust = 1,label = c("Hipotensión      ",
      #                                                                                                "Normal      ",
      #                                                                                                "Elevada      ",
      #                                                                                                "Hipertensión grado I      ",
      #                                                                                                "Hipertensión grado II      ",
      #                                                                                                "Crisis hipertensiva      "))
      #
  })
  
  output$amenityF <- renderPlot({
    num_entry_1_NEWS2_r() %>%
      mutate(PAdias_raw = as.numeric(as.character(PAdias))) %>%
      filter(PAdias_raw>30 & PAdias_raw<169) %>%
      ggplot(aes(x=`Place`,y=PAdias_raw, fill=`Place`)) + 
      geom_violin(trim=FALSE)+
      labs(x="",y = "Minutes")+
      geom_boxplot(width=0.1, fill="white")+
      theme_classic()+
      ggtitle("Time spent in amenity F") +
      theme(plot.title = element_text(hjust = 0.5)) +
      labs(fill = "Guests in") #+
      #geom_hline(yintercept=c(60,80,90,120), linetype="dashed") +
      #annotate("text", x = c(2,2,2,2,2), y = c(55,70,85,105,125),size=3, hjust = 1,label = c("Hipotensión      ",
      #                                                                                       "Normal      ",
      #                                                                                       "Hipertensión grado I      ",
      #                                                                                       "Hipertensión grado II      ",
      #                                                                                       "Crisis hipertensiva      "))
  })
  
  output$amenityG <- renderPlot({
    num_entry_1_NEWS2_r() %>%
      mutate(PAsis_raw = as.numeric(as.character(PAsis))) %>%
      mutate(PAdias_raw = as.numeric(as.character(PAdias))) %>%
      mutate(PAM = (2/3)*PAdias_raw + (1/3)*PAsis_raw) %>%
      filter(PAM>52 & PAM<150) %>%
      ggplot(aes(x=`Place`,y=PAM, fill=`Place`)) + 
      geom_violin(trim=FALSE)+
      labs(x="",y = "Minutes")+
      geom_boxplot(width=0.1, fill="white")+
      theme_classic()+
      ggtitle("Time spent in amenity G") +
      theme(plot.title = element_text(hjust = 0.5)) +
      labs(fill = "Guests in") #+
      #geom_hline(yintercept=c(60,70,100), linetype="dashed") +
      #annotate("text", x = c(2,2,2,2), y = c(55,65,85,105), hjust = 1,label = c("Hipoperfusión      ",
      #                                                                          "Baja      ",
      #                                                                          "Normal      ",
      #                                                                          "Alta      "))
  })
  
  
  intervenciones_r_A_hyg <- reactive({
    selection_IDs <- data.frame(unique(delirium_registro_r_A()$ID))
    colnames(selection_IDs) <- "ID"
    
    intervenciones <- filter(intervenciones, ID %in% selection_IDs$ID)
    
    #Intervenciones con pacientes
    A <- str_detect(intervenciones$`Estrategias.realizadas.con.el.paciente.(elegir.todas.las.que.apliquen)`,
                    "Toothpaste")
    A_count <- sum(A,na.rm = TRUE)
    
    B <- str_detect(intervenciones$`Estrategias.realizadas.con.el.paciente.(elegir.todas.las.que.apliquen)`,
                    "Mouthrinse")
    B_count <- sum(B,na.rm = TRUE)
    
    C <- str_detect(intervenciones$`Estrategias.realizadas.con.el.paciente.(elegir.todas.las.que.apliquen)`,
                    "Condoms")
    C_count <- sum(C,na.rm = TRUE)
    
    D <- str_detect(intervenciones$`Estrategias.realizadas.con.el.paciente.(elegir.todas.las.que.apliquen)`,
                    "Nail polish remover")
    D_count <- sum(D,na.rm = TRUE)
    
    E <- str_detect(intervenciones$`Estrategias.realizadas.con.el.paciente.(elegir.todas.las.que.apliquen)`,
                    "Shaving razor")
    E_count <- sum(E,na.rm = TRUE)
    
    F <- str_detect(intervenciones$`Estrategias.realizadas.con.el.paciente.(elegir.todas.las.que.apliquen)`,
                    "Dental floss")
    F_count <- sum(F,na.rm = TRUE)
    
    G <- str_detect(intervenciones$`Estrategias.realizadas.con.el.paciente.(elegir.todas.las.que.apliquen)`,
                    "Hand cream")
    G_count <- sum(G,na.rm = TRUE)
    
    H <- str_detect(intervenciones$`Estrategias.realizadas.con.el.paciente.(elegir.todas.las.que.apliquen)`,
                    "Hair comb")
    H_count <- sum(H,na.rm = TRUE)
    
    I <- str_detect(intervenciones$`Estrategias.realizadas.con.el.paciente.(elegir.todas.las.que.apliquen)`,
                    "Shaving foam")
    I_count <- sum(I,na.rm = TRUE)
    
    J <- str_detect(intervenciones$`Estrategias.realizadas.con.el.paciente.(elegir.todas.las.que.apliquen)`,
                    "Aftershave lotion")
    J_count <- sum(J,na.rm = TRUE)
    
    K <- str_detect(intervenciones$`Estrategias.realizadas.con.el.paciente.(elegir.todas.las.que.apliquen)`,
                    "Hair styling gel")
    K_count <- sum(K,na.rm = TRUE)
    
    L <- str_detect(intervenciones$`Estrategias.realizadas.con.el.paciente.(elegir.todas.las.que.apliquen)`,
                    "Sun cream")
    L_count <- sum(L,na.rm = TRUE)
    
    M <- str_detect(intervenciones$`Estrategias.realizadas.con.el.paciente.(elegir.todas.las.que.apliquen)`,
                    "Toothbrush")
    M_count <- sum(M,na.rm = TRUE)
    
    V <- str_detect(intervenciones$`Estrategias.realizadas.con.el.paciente.(elegir.todas.las.que.apliquen)`,
                    "Psicoeducación")
    V_count <- sum(V,na.rm = TRUE)
    
    W <- str_detect(intervenciones$`Estrategias.realizadas.con.el.paciente.(elegir.todas.las.que.apliquen)`,
                    "Deodorant")
    W_count <- sum(W,na.rm = TRUE)
    
    X <- str_detect(intervenciones$`Estrategias.realizadas.con.el.paciente.(elegir.todas.las.que.apliquen)`,
                    "Hand sanitizer")
    X_count <- sum(W,na.rm = TRUE)
    
    Y <- str_detect(intervenciones$`Estrategias.realizadas.con.el.paciente.(elegir.todas.las.que.apliquen)`,
                    "Medical face mask")
    Y_count <- sum(W,na.rm = TRUE)
    
    
    
    data.frame(Intervencion = c("Toothpaste",
                                      "Mouthrinse",
                                      "Condoms",
                                      "Nail polish remover",
                                      "Shaving razor",
                                      "Dental floss",
                                      "Hand cream",
                                      "Hair comb",
                                      "Shaving foam",
                                      "Aftershave lotion",
                                      "Hair styling gel",
                                      "Sun cream",
                                      "Toothbrush",
                                      "Nail clipper",
                                      "Deodorant",
                                      "Hand sanitizer",
                                      "Medical face mask"),
                     Amount = c(A_count,
                                  B_count,
                                  C_count,
                                  D_count,
                                  E_count,
                                  F_count,
                                  G_count,
                                  H_count,
                                  I_count,
                                  J_count,
                                  K_count,
                                  L_count,
                                  M_count,
                                  V_count,
                                  W_count,
                                  X_count,
                                  Y_count))
  })
  
  intervenciones_r_B_hyg <- reactive({
    selection_IDs <- data.frame(unique(delirium_registro_r_B()$ID))
    colnames(selection_IDs) <- "ID"
    
    intervenciones <- filter(intervenciones, ID %in% selection_IDs$ID)
    
    #Intervenciones con pacientes
    A <- str_detect(intervenciones$`Estrategias.realizadas.con.el.paciente.(elegir.todas.las.que.apliquen)`,
                    "Toothpaste")
    A_count <- sum(A,na.rm = TRUE)
    
    B <- str_detect(intervenciones$`Estrategias.realizadas.con.el.paciente.(elegir.todas.las.que.apliquen)`,
                    "Mouthrinse")
    B_count <- sum(B,na.rm = TRUE)
    
    C <- str_detect(intervenciones$`Estrategias.realizadas.con.el.paciente.(elegir.todas.las.que.apliquen)`,
                    "Condoms")
    C_count <- sum(C,na.rm = TRUE)
    
    D <- str_detect(intervenciones$`Estrategias.realizadas.con.el.paciente.(elegir.todas.las.que.apliquen)`,
                    "Nail polish remover")
    D_count <- sum(D,na.rm = TRUE)
    
    E <- str_detect(intervenciones$`Estrategias.realizadas.con.el.paciente.(elegir.todas.las.que.apliquen)`,
                    "Shaving razor")
    E_count <- sum(E,na.rm = TRUE)
    
    F <- str_detect(intervenciones$`Estrategias.realizadas.con.el.paciente.(elegir.todas.las.que.apliquen)`,
                    "Dental floss")
    F_count <- sum(F,na.rm = TRUE)
    
    G <- str_detect(intervenciones$`Estrategias.realizadas.con.el.paciente.(elegir.todas.las.que.apliquen)`,
                    "Hand cream")
    G_count <- sum(G,na.rm = TRUE)
    
    H <- str_detect(intervenciones$`Estrategias.realizadas.con.el.paciente.(elegir.todas.las.que.apliquen)`,
                    "Hair comb")
    H_count <- sum(H,na.rm = TRUE)
    
    I <- str_detect(intervenciones$`Estrategias.realizadas.con.el.paciente.(elegir.todas.las.que.apliquen)`,
                    "Shaving foam")
    I_count <- sum(I,na.rm = TRUE)
    
    J <- str_detect(intervenciones$`Estrategias.realizadas.con.el.paciente.(elegir.todas.las.que.apliquen)`,
                    "Aftershave lotion")
    J_count <- sum(J,na.rm = TRUE)
    
    K <- str_detect(intervenciones$`Estrategias.realizadas.con.el.paciente.(elegir.todas.las.que.apliquen)`,
                    "Hair styling gel")
    K_count <- sum(K,na.rm = TRUE)
    
    L <- str_detect(intervenciones$`Estrategias.realizadas.con.el.paciente.(elegir.todas.las.que.apliquen)`,
                    "Sun cream")
    L_count <- sum(L,na.rm = TRUE)
    
    M <- str_detect(intervenciones$`Estrategias.realizadas.con.el.paciente.(elegir.todas.las.que.apliquen)`,
                    "Toothbrush")
    M_count <- sum(M,na.rm = TRUE)
    
    V <- str_detect(intervenciones$`Estrategias.realizadas.con.el.paciente.(elegir.todas.las.que.apliquen)`,
                    "Psicoeducación")
    V_count <- sum(V,na.rm = TRUE)
    
    W <- str_detect(intervenciones$`Estrategias.realizadas.con.el.paciente.(elegir.todas.las.que.apliquen)`,
                    "Deodorant")
    W_count <- sum(W,na.rm = TRUE)
    
    X <- str_detect(intervenciones$`Estrategias.realizadas.con.el.paciente.(elegir.todas.las.que.apliquen)`,
                    "Hand sanitizer")
    X_count <- sum(W,na.rm = TRUE)
    
    Y <- str_detect(intervenciones$`Estrategias.realizadas.con.el.paciente.(elegir.todas.las.que.apliquen)`,
                    "Medical face mask")
    Y_count <- sum(W,na.rm = TRUE)
    
    
    
    data.frame(Intervencion = c("Toothpaste",
                                "Mouthrinse",
                                "Condoms",
                                "Nail polish remover",
                                "Shaving razor",
                                "Dental floss",
                                "Hand cream",
                                "Hair comb",
                                "Shaving foam",
                                "Aftershave lotion",
                                "Hair styling gel",
                                "Sun cream",
                                "Toothbrush",
                                "Nail clipper",
                                "Deodorant",
                                "Hand sanitizer",
                                "Medical face mask"),
               Amount = c(A_count,
                            B_count,
                            C_count,
                            D_count,
                            E_count,
                            F_count,
                            G_count,
                            H_count,
                            I_count,
                            J_count,
                            K_count,
                            L_count,
                            M_count,
                            V_count,
                            W_count,
                            X_count,
                            Y_count))
  })
  
  output$hygiene_products_A <- renderPlot({
    ggplot(intervenciones_r_A_hyg(), aes(reorder(Intervencion, -Amount), Amount)) +
      geom_col(fill = c("coral", "cadetblue","burlywood","skyblue","bisque","beige","azure","aquamarine","antiquewhite","aliceblue","dodgerblue","dimgray","deeppink","darkviolet","darkturquoise","coral", "cadetblue")) +
      coord_flip() + labs(x = "Extra hygiene products provided") + geom_text(aes(label = Amount), hjust = 0.5) +
      ggtitle(input$place[1])
  })
  
  output$hygiene_products_B <- renderPlot({
    if (length(input$place)==2) {
      ggplot(intervenciones_r_B_hyg(), aes(reorder(Intervencion, -Amount), Amount)) +
        geom_col(fill = c("coral", "cadetblue","burlywood","skyblue","bisque","beige","azure","aquamarine","antiquewhite","aliceblue","dodgerblue","dimgray","deeppink","darkviolet","darkturquoise","coral", "cadetblue")) +
        coord_flip() + labs(x = "Extra hygiene products provided") + geom_text(aes(label = Amount), hjust = 0.5) +
        ggtitle("Building B")
    }
  })
  
  intervenciones_r_A_food <- reactive({
    selection_IDs <- data.frame(unique(delirium_registro_r_A()$ID))
    colnames(selection_IDs) <- "ID"
    
    intervenciones <- filter(intervenciones, ID %in% selection_IDs$ID)
    #Intervenciones con familiares
    N <- str_detect(intervenciones$`Estrategias.realizadas.con.los.familiares.(elegir.todas.las.que.apliquen)`,
                    "Product A")
    N_count <- sum(N,na.rm = TRUE)
    
    O <- str_detect(intervenciones$`Estrategias.realizadas.con.los.familiares.(elegir.todas.las.que.apliquen)`,
                    "Product B")
    O_count <- sum(O,na.rm = TRUE)
    
    P <- str_detect(intervenciones$`Estrategias.realizadas.con.los.familiares.(elegir.todas.las.que.apliquen)`,
                    "Product C")
    P_count <- sum(P,na.rm = TRUE)
    
    Q <- str_detect(intervenciones$`Estrategias.realizadas.con.los.familiares.(elegir.todas.las.que.apliquen)`,
                    "Product D")
    Q_count <- sum(Q,na.rm = TRUE)
    
    R <- str_detect(intervenciones$`Estrategias.realizadas.con.los.familiares.(elegir.todas.las.que.apliquen)`,
                    "Product E")
    R_count <- sum(R,na.rm = TRUE)
    
    S <- str_detect(intervenciones$`Estrategias.realizadas.con.los.familiares.(elegir.todas.las.que.apliquen)`,
                    "Product F")
    S_count <- sum(S,na.rm = TRUE)
    
    T <- str_detect(intervenciones$`Estrategias.realizadas.con.los.familiares.(elegir.todas.las.que.apliquen)`,
                    "Product G")
    T_count <- sum(T,na.rm = TRUE)
    
    U <- str_detect(intervenciones$`Estrategias.realizadas.con.los.familiares.(elegir.todas.las.que.apliquen)`,
                    "Product H")
    U_count <- sum(U,na.rm = TRUE)
    
    data.frame(Intervencion = c("Product A",
                                       "Product B",
                                       "Product C",
                                       "Product D",
                                       "Product E",
                                       "Product F",
                                       "Product G",
                                       "Product H"),
                      Amount = c(N_count,
                                   O_count,
                                   P_count,
                                   Q_count,
                                   R_count,
                                   S_count,
                                   T_count,
                                   U_count))
  })
  
  intervenciones_r_B_food <- reactive({
    selection_IDs <- data.frame(unique(delirium_registro_r_B()$ID))
    colnames(selection_IDs) <- "ID"
    
    intervenciones <- filter(intervenciones, ID %in% selection_IDs$ID)
    
    #Intervenciones con familiares
    N <- str_detect(intervenciones$`Estrategias.realizadas.con.los.familiares.(elegir.todas.las.que.apliquen)`,
                    "Product A")
    N_count <- sum(N,na.rm = TRUE)
    
    O <- str_detect(intervenciones$`Estrategias.realizadas.con.los.familiares.(elegir.todas.las.que.apliquen)`,
                    "Product B")
    O_count <- sum(O,na.rm = TRUE)
    
    P <- str_detect(intervenciones$`Estrategias.realizadas.con.los.familiares.(elegir.todas.las.que.apliquen)`,
                    "Product C")
    P_count <- sum(P,na.rm = TRUE)
    
    Q <- str_detect(intervenciones$`Estrategias.realizadas.con.los.familiares.(elegir.todas.las.que.apliquen)`,
                    "Product D")
    Q_count <- sum(Q,na.rm = TRUE)
    
    R <- str_detect(intervenciones$`Estrategias.realizadas.con.los.familiares.(elegir.todas.las.que.apliquen)`,
                    "Product E")
    R_count <- sum(R,na.rm = TRUE)
    
    S <- str_detect(intervenciones$`Estrategias.realizadas.con.los.familiares.(elegir.todas.las.que.apliquen)`,
                    "Product F")
    S_count <- sum(S,na.rm = TRUE)
    
    T <- str_detect(intervenciones$`Estrategias.realizadas.con.los.familiares.(elegir.todas.las.que.apliquen)`,
                    "Product G")
    T_count <- sum(T,na.rm = TRUE)
    
    U <- str_detect(intervenciones$`Estrategias.realizadas.con.los.familiares.(elegir.todas.las.que.apliquen)`,
                    "Product H")
    U_count <- sum(U,na.rm = TRUE)
    
    data.frame(Intervencion = c("Product A",
                                       "Product B",
                                       "Product C",
                                       "Product D",
                                       "Product E",
                                       "Product F",
                                       "Product G",
                                       "Product H"),
                      Amount = c(N_count,
                                   O_count,
                                   P_count,
                                   Q_count,
                                   R_count,
                                   S_count,
                                   T_count,
                                   U_count))
  })
  
  output$food_products_A <- renderPlot({
    ggplot(intervenciones_r_A_food(), aes(reorder(Intervencion, -Amount), Amount)) +
      geom_col(fill = c("coral", "cadetblue","burlywood","skyblue","bisque","beige","azure","aquamarine")) +
      coord_flip() + labs(x = "Food products sold") + geom_text(aes(label = Amount), hjust = 0.5) +
      ggtitle(input$place[1])
  })
  
  output$food_products_B <- renderPlot({
    if (length(input$place)==2) {
      ggplot(intervenciones_r_B_food(), aes(reorder(Intervencion, -Amount), Amount)) +
        geom_col(fill = c("coral", "cadetblue","burlywood","skyblue","bisque","beige","azure","aquamarine")) +
        coord_flip() + labs(x = "Food products sold") + geom_text(aes(label = Amount), hjust = 0.5) +
        ggtitle("Building B")
    }
  })
  
  intervenciones_r_A_drinks <- reactive({
    selection_IDs <- data.frame(unique(delirium_registro_r_A()$ID))
    colnames(selection_IDs) <- "ID"
    
    intervenciones <- filter(intervenciones, ID %in% selection_IDs$ID)
    
    #Intervenciones de rehabilitación con pacientes
    A1 <- str_detect(intervenciones$`Intervenciones.de.rehabilitación.realizadas.con.el.paciente.(elegir.todas.las.que.apliquen)`,
                     "Drink A")
    A1_count <- sum(A1,na.rm = TRUE)
    
    B1 <- str_detect(intervenciones$`Intervenciones.de.rehabilitación.realizadas.con.el.paciente.(elegir.todas.las.que.apliquen)`,
                     "Drink B")
    B1_count <- sum(B1,na.rm = TRUE)
    
    C1 <- str_detect(intervenciones$`Intervenciones.de.rehabilitación.realizadas.con.el.paciente.(elegir.todas.las.que.apliquen)`,
                     "Drink C")
    C1_count <- sum(C1,na.rm = TRUE)
    
    D1 <- str_detect(intervenciones$`Intervenciones.de.rehabilitación.realizadas.con.el.paciente.(elegir.todas.las.que.apliquen)`,
                     "Drink D")
    D1_count <- sum(D1,na.rm = TRUE)
    
    E1 <- str_detect(intervenciones$`Intervenciones.de.rehabilitación.realizadas.con.el.paciente.(elegir.todas.las.que.apliquen)`,
                     "Drink E")
    E1_count <- sum(E1,na.rm = TRUE)
    
    F1 <- str_detect(intervenciones$`Intervenciones.de.rehabilitación.realizadas.con.el.paciente.(elegir.todas.las.que.apliquen)`,
                     "Drink F")
    F1_count <- sum(F1,na.rm = TRUE)
    
    data.frame(Intervencion = c("Drink A",
                                       "Drink B",
                                       "Drink C",
                                       "Drink D",
                                       "Drink E",
                                       "Drink F"),
                      Amount = c(A1_count,
                                   B1_count,
                                   C1_count,
                                   D1_count,
                                   E1_count,
                                   F1_count))
  })
  
  intervenciones_r_B_drinks <- reactive({
    selection_IDs <- data.frame(unique(delirium_registro_r_B()$ID))
    colnames(selection_IDs) <- "ID"
    
    intervenciones <- filter(intervenciones, ID %in% selection_IDs$ID)
    
    #Intervenciones de rehabilitación con pacientes
    A1 <- str_detect(intervenciones$`Intervenciones.de.rehabilitación.realizadas.con.el.paciente.(elegir.todas.las.que.apliquen)`,
                     "Drink A")
    A1_count <- sum(A1,na.rm = TRUE)
    
    B1 <- str_detect(intervenciones$`Intervenciones.de.rehabilitación.realizadas.con.el.paciente.(elegir.todas.las.que.apliquen)`,
                     "Drink B")
    B1_count <- sum(B1,na.rm = TRUE)
    
    C1 <- str_detect(intervenciones$`Intervenciones.de.rehabilitación.realizadas.con.el.paciente.(elegir.todas.las.que.apliquen)`,
                     "Drink C")
    C1_count <- sum(C1,na.rm = TRUE)
    
    D1 <- str_detect(intervenciones$`Intervenciones.de.rehabilitación.realizadas.con.el.paciente.(elegir.todas.las.que.apliquen)`,
                     "Drink D")
    D1_count <- sum(D1,na.rm = TRUE)
    
    E1 <- str_detect(intervenciones$`Intervenciones.de.rehabilitación.realizadas.con.el.paciente.(elegir.todas.las.que.apliquen)`,
                     "Drink E")
    E1_count <- sum(E1,na.rm = TRUE)
    
    F1 <- str_detect(intervenciones$`Intervenciones.de.rehabilitación.realizadas.con.el.paciente.(elegir.todas.las.que.apliquen)`,
                     "Drink F")
    F1_count <- sum(F1,na.rm = TRUE)
    
    data.frame(Intervencion = c("Drink A",
                                "Drink B",
                                "Drink C",
                                "Drink D",
                                "Drink E",
                                "Drink F"),
               Amount = c(A1_count,
                            B1_count,
                            C1_count,
                            D1_count,
                            E1_count,
                            F1_count))
  })
  
  
  output$drinks_A <- renderPlot({
    ggplot(intervenciones_r_A_drinks(), aes(reorder(Intervencion, -Amount), Amount)) +
      geom_col(fill = c("coral", "cadetblue","burlywood","skyblue","bisque","beige")) +
      coord_flip() + labs(x = "Drinks sold") + geom_text(aes(label = Amount), hjust = 0.5) +
      ggtitle(input$place[1])
  })
  
  output$drinks_B <- renderPlot({
    if (length(input$place)==2) {
      ggplot(intervenciones_r_B_drinks(), aes(reorder(Intervencion, -Amount), Amount)) +
        geom_col(fill = c("coral", "cadetblue","burlywood","skyblue","bisque","beige")) +
        coord_flip() + labs(x = "Drinks sold") + geom_text(aes(label = Amount), hjust = 0.5) +
        ggtitle("Building B")
    }
  })
  
  intervenciones_r_A_hygXguest <- reactive({
    selection_IDs <- data.frame(unique(delirium_registro_r_A()$ID))
    colnames(selection_IDs) <- "ID"
    
    intervenciones <- filter(intervenciones, ID %in% selection_IDs$ID)
    
    intervenciones_pac_por_paciente <- data.frame(ID = unique(intervenciones$ID))
    intervenciones_pac_por_paciente <- mutate(intervenciones_pac_por_paciente, A = 0,B = 0,C = 0,D = 0,E = 0,F = 0,G = 0,H = 0,I = 0,J = 0,K = 0,L = 0,M = 0,V = 0,W = 0)
    for(i in intervenciones_pac_por_paciente$ID) {
      intevenciones_LC <- filter(intervenciones, ID==i)
      intervenciones_pac_por_paciente$A[intervenciones_pac_por_paciente$ID == i] <- sum(str_detect(intevenciones_LC$`Estrategias.realizadas.con.el.paciente.(elegir.todas.las.que.apliquen)`,"Toothpaste"),na.rm = TRUE)
      intervenciones_pac_por_paciente$B[intervenciones_pac_por_paciente$ID == i] <- sum(str_detect(intevenciones_LC$`Estrategias.realizadas.con.el.paciente.(elegir.todas.las.que.apliquen)`,"Mouthrinse"),na.rm = TRUE)
      intervenciones_pac_por_paciente$C[intervenciones_pac_por_paciente$ID == i] <- sum(str_detect(intevenciones_LC$`Estrategias.realizadas.con.el.paciente.(elegir.todas.las.que.apliquen)`,"Condoms"),na.rm = TRUE)
      intervenciones_pac_por_paciente$D[intervenciones_pac_por_paciente$ID == i] <- sum(str_detect(intevenciones_LC$`Estrategias.realizadas.con.el.paciente.(elegir.todas.las.que.apliquen)`,"Nail polish remover"),na.rm = TRUE)
      intervenciones_pac_por_paciente$E[intervenciones_pac_por_paciente$ID == i] <- sum(str_detect(intevenciones_LC$`Estrategias.realizadas.con.el.paciente.(elegir.todas.las.que.apliquen)`,"Shaving razor"),na.rm = TRUE)
      intervenciones_pac_por_paciente$F[intervenciones_pac_por_paciente$ID == i] <- sum(str_detect(intevenciones_LC$`Estrategias.realizadas.con.el.paciente.(elegir.todas.las.que.apliquen)`,"Dental floss"),na.rm = TRUE)
      intervenciones_pac_por_paciente$G[intervenciones_pac_por_paciente$ID == i] <- sum(str_detect(intevenciones_LC$`Estrategias.realizadas.con.el.paciente.(elegir.todas.las.que.apliquen)`,"Hand cream"),na.rm = TRUE)
      intervenciones_pac_por_paciente$H[intervenciones_pac_por_paciente$ID == i] <- sum(str_detect(intevenciones_LC$`Estrategias.realizadas.con.el.paciente.(elegir.todas.las.que.apliquen)`,"Hair comb"),na.rm = TRUE)
      intervenciones_pac_por_paciente$I[intervenciones_pac_por_paciente$ID == i] <- sum(str_detect(intevenciones_LC$`Estrategias.realizadas.con.el.paciente.(elegir.todas.las.que.apliquen)`,"Shaving foam"),na.rm = TRUE)
      intervenciones_pac_por_paciente$J[intervenciones_pac_por_paciente$ID == i] <- sum(str_detect(intevenciones_LC$`Estrategias.realizadas.con.el.paciente.(elegir.todas.las.que.apliquen)`,"Aftershave lotion"),na.rm = TRUE)
      intervenciones_pac_por_paciente$K[intervenciones_pac_por_paciente$ID == i] <- sum(str_detect(intevenciones_LC$`Estrategias.realizadas.con.el.paciente.(elegir.todas.las.que.apliquen)`,"Hair styling gel"),na.rm = TRUE)
      intervenciones_pac_por_paciente$L[intervenciones_pac_por_paciente$ID == i] <- sum(str_detect(intevenciones_LC$`Estrategias.realizadas.con.el.paciente.(elegir.todas.las.que.apliquen)`,"Sun cream"),na.rm = TRUE)
      intervenciones_pac_por_paciente$M[intervenciones_pac_por_paciente$ID == i] <- sum(str_detect(intevenciones_LC$`Estrategias.realizadas.con.el.paciente.(elegir.todas.las.que.apliquen)`,"Toothbrush"),na.rm = TRUE)
      intervenciones_pac_por_paciente$V[intervenciones_pac_por_paciente$ID == i] <- sum(str_detect(intevenciones_LC$`Estrategias.realizadas.con.el.paciente.(elegir.todas.las.que.apliquen)`,"Nail clipper"),na.rm = TRUE)
      intervenciones_pac_por_paciente$W[intervenciones_pac_por_paciente$ID == i] <- sum(str_detect(intevenciones_LC$`Estrategias.realizadas.con.el.paciente.(elegir.todas.las.que.apliquen)`,"Deodorant"),na.rm = TRUE)
    }
    
    intervenciones_pac_por_paciente_contotal <- mutate(intervenciones_pac_por_paciente,
                                                       total = A+B+C+D+E+F+G+H+I+J+K+L+M+V+W)
    mutate(intervenciones_pac_por_paciente_contotal, grupo = 'Hotel guests')
  })
  
  intervenciones_r_B_hygXguest <- reactive({
    selection_IDs <- data.frame(unique(delirium_registro_r_B()$ID))
    colnames(selection_IDs) <- "ID"
    
    intervenciones <- filter(intervenciones, ID %in% selection_IDs$ID)
    
    intervenciones_pac_por_paciente <- data.frame(ID = unique(intervenciones$ID))
    intervenciones_pac_por_paciente <- mutate(intervenciones_pac_por_paciente, A = 0,B = 0,C = 0,D = 0,E = 0,F = 0,G = 0,H = 0,I = 0,J = 0,K = 0,L = 0,M = 0,V = 0,W = 0)
    for(i in intervenciones_pac_por_paciente$ID) {
      intevenciones_LC <- filter(intervenciones, ID==i)
      intervenciones_pac_por_paciente$A[intervenciones_pac_por_paciente$ID == i] <- sum(str_detect(intevenciones_LC$`Estrategias.realizadas.con.el.paciente.(elegir.todas.las.que.apliquen)`,"Toothpaste"),na.rm = TRUE)
      intervenciones_pac_por_paciente$B[intervenciones_pac_por_paciente$ID == i] <- sum(str_detect(intevenciones_LC$`Estrategias.realizadas.con.el.paciente.(elegir.todas.las.que.apliquen)`,"Mouthrinse"),na.rm = TRUE)
      intervenciones_pac_por_paciente$C[intervenciones_pac_por_paciente$ID == i] <- sum(str_detect(intevenciones_LC$`Estrategias.realizadas.con.el.paciente.(elegir.todas.las.que.apliquen)`,"Condoms"),na.rm = TRUE)
      intervenciones_pac_por_paciente$D[intervenciones_pac_por_paciente$ID == i] <- sum(str_detect(intevenciones_LC$`Estrategias.realizadas.con.el.paciente.(elegir.todas.las.que.apliquen)`,"Nail polish remover"),na.rm = TRUE)
      intervenciones_pac_por_paciente$E[intervenciones_pac_por_paciente$ID == i] <- sum(str_detect(intevenciones_LC$`Estrategias.realizadas.con.el.paciente.(elegir.todas.las.que.apliquen)`,"Shaving razor"),na.rm = TRUE)
      intervenciones_pac_por_paciente$F[intervenciones_pac_por_paciente$ID == i] <- sum(str_detect(intevenciones_LC$`Estrategias.realizadas.con.el.paciente.(elegir.todas.las.que.apliquen)`,"Dental floss"),na.rm = TRUE)
      intervenciones_pac_por_paciente$G[intervenciones_pac_por_paciente$ID == i] <- sum(str_detect(intevenciones_LC$`Estrategias.realizadas.con.el.paciente.(elegir.todas.las.que.apliquen)`,"Hand cream"),na.rm = TRUE)
      intervenciones_pac_por_paciente$H[intervenciones_pac_por_paciente$ID == i] <- sum(str_detect(intevenciones_LC$`Estrategias.realizadas.con.el.paciente.(elegir.todas.las.que.apliquen)`,"Hair comb"),na.rm = TRUE)
      intervenciones_pac_por_paciente$I[intervenciones_pac_por_paciente$ID == i] <- sum(str_detect(intevenciones_LC$`Estrategias.realizadas.con.el.paciente.(elegir.todas.las.que.apliquen)`,"Shaving foam"),na.rm = TRUE)
      intervenciones_pac_por_paciente$J[intervenciones_pac_por_paciente$ID == i] <- sum(str_detect(intevenciones_LC$`Estrategias.realizadas.con.el.paciente.(elegir.todas.las.que.apliquen)`,"Aftershave lotion"),na.rm = TRUE)
      intervenciones_pac_por_paciente$K[intervenciones_pac_por_paciente$ID == i] <- sum(str_detect(intevenciones_LC$`Estrategias.realizadas.con.el.paciente.(elegir.todas.las.que.apliquen)`,"Hair styling gel"),na.rm = TRUE)
      intervenciones_pac_por_paciente$L[intervenciones_pac_por_paciente$ID == i] <- sum(str_detect(intevenciones_LC$`Estrategias.realizadas.con.el.paciente.(elegir.todas.las.que.apliquen)`,"Sun cream"),na.rm = TRUE)
      intervenciones_pac_por_paciente$M[intervenciones_pac_por_paciente$ID == i] <- sum(str_detect(intevenciones_LC$`Estrategias.realizadas.con.el.paciente.(elegir.todas.las.que.apliquen)`,"Toothbrush"),na.rm = TRUE)
      intervenciones_pac_por_paciente$V[intervenciones_pac_por_paciente$ID == i] <- sum(str_detect(intevenciones_LC$`Estrategias.realizadas.con.el.paciente.(elegir.todas.las.que.apliquen)`,"Nail clipper"),na.rm = TRUE)
      intervenciones_pac_por_paciente$W[intervenciones_pac_por_paciente$ID == i] <- sum(str_detect(intevenciones_LC$`Estrategias.realizadas.con.el.paciente.(elegir.todas.las.que.apliquen)`,"Deodorant"),na.rm = TRUE)
    }
    
    intervenciones_pac_por_paciente_contotal <- mutate(intervenciones_pac_por_paciente,
                                                       total = A+B+C+D+E+F+G+H+I+J+K+L+M+V+W)
    mutate(intervenciones_pac_por_paciente_contotal, grupo = 'Hotel guests')
  })
  
  output$hygiene_products_A_perGuest <- renderPlot({
    ggplot(intervenciones_r_A_hygXguest(), aes(x=grupo,y=total)) + 
      geom_violin(trim=FALSE, fill="coral")+
      labs(x="",y = "Extra hygiene products provided\nper guest")+
      geom_boxplot(width=0.1, fill="white")+
      theme_classic()+
      ggtitle(input$place[1]) +
      theme(plot.title = element_text(hjust = 0.5)) +
      geom_hline(yintercept=mean(intervenciones_r_A_hygXguest()$total), linetype="dashed") +
      annotate("text", x = 1, y = mean(intervenciones_r_A_hygXguest()$total)+15, hjust = 1,label = c(paste("mean =",round(mean(intervenciones_r_A_hygXguest()$total),2),"       ")))
  })
  
  output$hygiene_products_B_perGuest <- renderPlot({
    if (length(input$place)==2) {
      ggplot(intervenciones_r_B_hygXguest(), aes(x=grupo,y=total)) + 
        geom_violin(trim=FALSE, fill="coral")+
        labs(x="",y = "Extra hygiene products provided\nper guest")+
        geom_boxplot(width=0.1, fill="white")+
        theme_classic()+
        ggtitle("Building B") +
        theme(plot.title = element_text(hjust = 0.5)) +
        geom_hline(yintercept=mean(intervenciones_r_B_hygXguest()$total), linetype="dashed") +
        annotate("text", x = 1, y = mean(intervenciones_r_B_hygXguest()$total)+1, hjust = 1,label = c(paste("mean =",round(mean(intervenciones_r_B_hygXguest()$total),2),"       ")))
    }
  })
  
  intervenciones_r_A_foodXguest <- reactive({
    selection_IDs <- data.frame(unique(delirium_registro_r_A()$ID))
    colnames(selection_IDs) <- "ID"
    
    intervenciones <- filter(intervenciones, ID %in% selection_IDs$ID)
    
    intervenciones_fam_por_paciente <- data.frame(ID = unique(intervenciones$ID))
    intervenciones_fam_por_paciente <- mutate(intervenciones_fam_por_paciente,N = 0,O = 0,P = 0,Q = 0,R = 0,S = 0,T = 0,U = 0)
    for(i in intervenciones_fam_por_paciente$ID) {
      intevenciones_LC <- filter(intervenciones, ID==i)
      intervenciones_fam_por_paciente$N[intervenciones_fam_por_paciente$ID == i] <- sum(str_detect(intevenciones_LC$`Estrategias.realizadas.con.los.familiares.(elegir.todas.las.que.apliquen)`,"Product A"),na.rm = TRUE)
      intervenciones_fam_por_paciente$O[intervenciones_fam_por_paciente$ID == i] <- sum(str_detect(intevenciones_LC$`Estrategias.realizadas.con.los.familiares.(elegir.todas.las.que.apliquen)`,"Product B"),na.rm = TRUE)
      intervenciones_fam_por_paciente$P[intervenciones_fam_por_paciente$ID == i] <- sum(str_detect(intevenciones_LC$`Estrategias.realizadas.con.los.familiares.(elegir.todas.las.que.apliquen)`,"Product C"),na.rm = TRUE)
      intervenciones_fam_por_paciente$Q[intervenciones_fam_por_paciente$ID == i] <- sum(str_detect(intevenciones_LC$`Estrategias.realizadas.con.los.familiares.(elegir.todas.las.que.apliquen)`,"Product D"),na.rm = TRUE)
      intervenciones_fam_por_paciente$R[intervenciones_fam_por_paciente$ID == i] <- sum(str_detect(intevenciones_LC$`Estrategias.realizadas.con.los.familiares.(elegir.todas.las.que.apliquen)`,"Product E"),na.rm = TRUE)
      intervenciones_fam_por_paciente$S[intervenciones_fam_por_paciente$ID == i] <- sum(str_detect(intevenciones_LC$`Estrategias.realizadas.con.los.familiares.(elegir.todas.las.que.apliquen)`,"Product F"),na.rm = TRUE)
      intervenciones_fam_por_paciente$T[intervenciones_fam_por_paciente$ID == i] <- sum(str_detect(intevenciones_LC$`Estrategias.realizadas.con.los.familiares.(elegir.todas.las.que.apliquen)`,"Product G"),na.rm = TRUE)
      intervenciones_fam_por_paciente$U[intervenciones_fam_por_paciente$ID == i] <- sum(str_detect(intevenciones_LC$`Estrategias.realizadas.con.los.familiares.(elegir.todas.las.que.apliquen)`,"Product H"),na.rm = TRUE)
    }
    intervenciones_fam_por_paciente_contotal <- mutate(intervenciones_fam_por_paciente,
                                                       total = N+O+P+Q+R+S+T+U)
    
    mutate(intervenciones_fam_por_paciente_contotal, grupo = 'Hotel guests')
  })
  
  
  intervenciones_r_B_foodXguest <- reactive({
    selection_IDs <- data.frame(unique(delirium_registro_r_B()$ID))
    colnames(selection_IDs) <- "ID"
    
    intervenciones <- filter(intervenciones, ID %in% selection_IDs$ID)
    
    intervenciones_fam_por_paciente <- data.frame(ID = unique(intervenciones$ID))
    intervenciones_fam_por_paciente <- mutate(intervenciones_fam_por_paciente,N = 0,O = 0,P = 0,Q = 0,R = 0,S = 0,T = 0,U = 0)
    for(i in intervenciones_fam_por_paciente$ID) {
      intevenciones_LC <- filter(intervenciones, ID==i)
      intervenciones_fam_por_paciente$N[intervenciones_fam_por_paciente$ID == i] <- sum(str_detect(intevenciones_LC$`Estrategias.realizadas.con.los.familiares.(elegir.todas.las.que.apliquen)`,"Product A"),na.rm = TRUE)
      intervenciones_fam_por_paciente$O[intervenciones_fam_por_paciente$ID == i] <- sum(str_detect(intevenciones_LC$`Estrategias.realizadas.con.los.familiares.(elegir.todas.las.que.apliquen)`,"Product B"),na.rm = TRUE)
      intervenciones_fam_por_paciente$P[intervenciones_fam_por_paciente$ID == i] <- sum(str_detect(intevenciones_LC$`Estrategias.realizadas.con.los.familiares.(elegir.todas.las.que.apliquen)`,"Product C"),na.rm = TRUE)
      intervenciones_fam_por_paciente$Q[intervenciones_fam_por_paciente$ID == i] <- sum(str_detect(intevenciones_LC$`Estrategias.realizadas.con.los.familiares.(elegir.todas.las.que.apliquen)`,"Product D"),na.rm = TRUE)
      intervenciones_fam_por_paciente$R[intervenciones_fam_por_paciente$ID == i] <- sum(str_detect(intevenciones_LC$`Estrategias.realizadas.con.los.familiares.(elegir.todas.las.que.apliquen)`,"Product E"),na.rm = TRUE)
      intervenciones_fam_por_paciente$S[intervenciones_fam_por_paciente$ID == i] <- sum(str_detect(intevenciones_LC$`Estrategias.realizadas.con.los.familiares.(elegir.todas.las.que.apliquen)`,"Product F"),na.rm = TRUE)
      intervenciones_fam_por_paciente$T[intervenciones_fam_por_paciente$ID == i] <- sum(str_detect(intevenciones_LC$`Estrategias.realizadas.con.los.familiares.(elegir.todas.las.que.apliquen)`,"Product G"),na.rm = TRUE)
      intervenciones_fam_por_paciente$U[intervenciones_fam_por_paciente$ID == i] <- sum(str_detect(intevenciones_LC$`Estrategias.realizadas.con.los.familiares.(elegir.todas.las.que.apliquen)`,"Product H"),na.rm = TRUE)
    }
    intervenciones_fam_por_paciente_contotal <- mutate(intervenciones_fam_por_paciente,
                                                       total = N+O+P+Q+R+S+T+U)
    
    mutate(intervenciones_fam_por_paciente_contotal, grupo = 'Hotel guests')
  })
  
  
  output$food_products_A_perGuest <- renderPlot({
    ggplot(intervenciones_r_A_foodXguest(), aes(x=grupo,y=total)) + 
      geom_violin(trim=FALSE, fill="cadetblue")+
      labs(x="",y = "Food products sold per guest")+
      geom_boxplot(width=0.1, fill="white")+
      theme_classic()+
      ggtitle(input$place[1]) +
      theme(plot.title = element_text(hjust = 0.5)) +
      geom_hline(yintercept=mean(intervenciones_r_A_foodXguest()$total), linetype="dashed") +
      annotate("text", x = 1, y = mean(intervenciones_r_A_foodXguest()$total)+2, hjust = 1,label = c(paste("mean =",round(mean(intervenciones_r_A_foodXguest()$total),2),"        ")))
  })
  
  output$food_products_B_perGuest <- renderPlot({
    if (length(input$place)==2) {
      ggplot(intervenciones_r_B_foodXguest(), aes(x=grupo,y=total)) + 
        geom_violin(trim=FALSE, fill="cadetblue")+
        labs(x="",y = "Food products sold per guest")+
        geom_boxplot(width=0.1, fill="white")+
        theme_classic()+
        ggtitle("Building B") +
        theme(plot.title = element_text(hjust = 0.5)) +
        geom_hline(yintercept=mean(intervenciones_r_B_foodXguest()$total), linetype="dashed") +
        annotate("text", x = 1, y = mean(intervenciones_r_B_foodXguest()$total)+0.5, hjust = 1,label = c(paste("mean =",round(mean(intervenciones_r_B_foodXguest()$total),2),"        ")))
    }
  })
  
  forms_viejos_freq <- reactive({
    delirium_db_sort_onlyID %>%
      filter(Input_method == "Data input method v1") %>%
      filter(`Evaluador:` == input$evaluador) %>%
      group_by(Marcatemp_sinhora) %>%
      summarise(`Data input method v1` = n())
  })
  
  forms_nuevos_freq <- reactive({
    delirium_db_sort_onlyID %>%
      filter(Input_method == "Data input method v2") %>%
      filter(`Evaluador:` == input$evaluador) %>%
      group_by(Marcatemp_sinhora) %>%
      summarise(`Data input method v2` = n())
  })
  
  registro_freq <- reactive({
    delirium_registro  %>%
      filter(`Colaborador.que.registra.al.paciente:` == input$evaluador) %>%
      group_by(Marca.temporal) %>%
      summarise(Registros = n())
  })
  
  news2_freq <- reactive({
    delirium_news2  %>%
      filter(Evaluador_NEWS2 == input$evaluador) %>%
      group_by(Marca.temporal) %>%
      summarise(NEWS2 = n())
  })
  
  prevencion_freq <- reactive({
    delirium_db_sort_onlyID %>%
      filter(Input_method == "Data input method v3") %>%
      filter(`Evaluador:` == input$evaluador) %>%
      group_by(Marcatemp_sinhora) %>%
      summarise(`Data input method v3` = n())
  })
  
  tamizaje_freq <- reactive({
    delirium_db_sort_onlyID %>%
      filter(Input_method == "Service G") %>%
      filter(`Evaluador:` == input$evaluador) %>%
      group_by(Marcatemp_sinhora) %>%
      summarise(`Service G` = n())
  })
  
  intervenciones_freq <- reactive({
    intervenciones %>%
      filter(`Clínico:` == input$evaluador) %>%
      group_by(Marca.temporal) %>%
      summarise(Intervenciones = n())
  })
  
  altas_freq <- reactive({
    delirium_altas %>%
      filter(Persona.que.registra.el.cambio == input$evaluador) %>%
      group_by(Marca.temporal) %>%
      summarise(`Room changes and check-outs` = n())
  })
  
  notas_freq <- reactive({
    delirium_notas %>%
      filter(`Clínico:` == input$evaluador) %>%
      group_by(Marca.temporal) %>%
      summarise(`Daily notes` = n())
  })
  
  output$evalplot <- renderPlot({
    ggplot() + 
      #geom_line(data = forms_viejos_freq(), aes(x=Marcatemp_sinhora, y=`Data input method v1`,colour="Data input method v1")) +
      {if(dim(forms_viejos_freq())[1]!=0)geom_point(data = forms_viejos_freq(), aes(x=Marcatemp_sinhora, y=`Data input method v1`,colour="Data input method v1"))} +
      {if(dim(forms_viejos_freq())[1]!=0)geom_segment(data = forms_viejos_freq(),aes(x=Marcatemp_sinhora, xend=Marcatemp_sinhora, y=0, yend=`Data input method v1`),colour="coral")} +
      #geom_line(data = forms_nuevos_freq(), aes(x=Marcatemp_sinhora, y=`Data input method v2`,colour="Data input method v2")) +
      {if(dim(forms_nuevos_freq())[1]!=0)geom_point(data = forms_nuevos_freq(), aes(x=Marcatemp_sinhora, y=`Data input method v2`,colour="Data input method v2"))} +
      {if(dim(forms_nuevos_freq())[1]!=0)geom_segment(data = forms_nuevos_freq(),aes(x=Marcatemp_sinhora, xend=Marcatemp_sinhora, y=0, yend=`Data input method v2`),colour="cadetblue")} +
      {if(dim(registro_freq())[1]!=0)geom_point(data = registro_freq(), aes(x=Marca.temporal, y=Registros,colour="Admission demographics"))} +
      {if(dim(registro_freq())[1]!=0)geom_segment(data = registro_freq(),aes(x=Marca.temporal, xend=Marca.temporal, y=0, yend=Registros),colour="burlywood")} +
      {if(dim(news2_freq())[1]!=0)geom_point(data = news2_freq(), aes(x=Marca.temporal, y=NEWS2,colour="Amenities"))} +
      {if(dim(news2_freq())[1]!=0)geom_segment(data = news2_freq(),aes(x=Marca.temporal, xend=Marca.temporal, y=0, yend=NEWS2),colour="skyblue")} +
      {if(dim(prevencion_freq())[1]!=0)geom_point(data = prevencion_freq(), aes(x=Marcatemp_sinhora, y=`Data input method v3`,colour="Data input method v3"))} +
      {if(dim(prevencion_freq())[1]!=0)geom_segment(data = prevencion_freq(),aes(x=Marcatemp_sinhora, xend=Marcatemp_sinhora, y=0, yend=`Data input method v3`),colour="darkviolet")} +
      {if(dim(tamizaje_freq())[1]!=0)geom_point(data = tamizaje_freq(), aes(x=Marcatemp_sinhora, y=`Service G`,colour="Service G"))} +
      {if(dim(tamizaje_freq())[1]!=0)geom_segment(data = tamizaje_freq(),aes(x=Marcatemp_sinhora, xend=Marcatemp_sinhora, y=0, yend=`Service G`),colour="deeppink")} +
      {if(dim(intervenciones_freq())[1]!=0)geom_point(data = intervenciones_freq(), aes(x=Marca.temporal, y=Intervenciones,colour="Products"))} +
      {if(dim(intervenciones_freq())[1]!=0)geom_segment(data = intervenciones_freq(),aes(x=Marca.temporal, xend=Marca.temporal, y=0, yend=Intervenciones),colour="dimgray")} +
      {if(dim(altas_freq())[1]!=0)geom_point(data = altas_freq(), aes(x=Marca.temporal, y=`Room changes and check-outs`,colour="Room changes and check-outs"))} +
      {if(dim(altas_freq())[1]!=0)geom_segment(data = altas_freq(),aes(x=Marca.temporal, xend=Marca.temporal, y=0, yend=`Room changes and check-outs`),colour="gold")} +
      {if(dim(notas_freq())[1]!=0)geom_point(data = notas_freq(), aes(x=Marca.temporal, y=`Daily notes`,colour="Daily notes"))} +
      {if(dim(notas_freq())[1]!=0)geom_segment(data = notas_freq(),aes(x=Marca.temporal, xend=Marca.temporal, y=0, yend=`Daily notes`),colour="green")} +
      ylab("Amount") +
      xlab("Date") +
      ggtitle("Data inputs sent from guests") +
      scale_color_manual(name = "Type of data input", values = c("Data input method v1" = "coral","Data input method v2" = "cadetblue","Admission demographics"="burlywood","Amenities"="skyblue",
                                                              "Data input method v3"="darkviolet","Service G"="deeppink","Products"="dimgray",
                                                              "Room changes and check-outs"="gold","Daily notes"="green"))
  })
  #c(,"","","","","","","antiquewhite","aliceblue","dodgerblue","","","","darkturquoise","coral", "cadetblue")
}

shinyApp(ui = ui, server = server)