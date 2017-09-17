library(shiny)
library(ggplot2)
library(DT)
library(grid)
library(plyr)
library(tidyr)
library(rmarkdown)
library(gridExtra)
#install.packages('cowplot') need this package and MikTex installed to produce the pdf report download


source("corefx.r")
shinyServer(function(input, output, session) {

  ###############################
  ######### Parameters ##########
  ###############################
  #Stock and quotas
  curr_stock <- 10

  observe({ # maxalpha
    curr_maxalpha <- input$maxalpha
    updateSliderInput(session, "alpha_1",
                       value = 10, min = 0, max = curr_maxalpha, step = .1)
  })

  # Conditional quota input
  TAC<-3.5

  # Shares
  alphaZero<-c(10/100,1-10/100)


  betaZero<-
    matrix(
      c(10/100, 1-10/100,
        20/100, 1-20/100),
      nrow=2,ncol=2,byrow = TRUE)

  # Marginal costs

  marg.cost<-matrix(
      c(1/(curr_stock^.1), .8/(curr_stock^.1),
        .5/(curr_stock^.1), .1/(curr_stock^.1)),
      nrow=2,ncol=2,byrow = TRUE)

  # Capacity
  cap<-matrix(
      c(6, 8,
        30, 120),
      nrow=2,ncol=2,byrow = TRUE)

  # Crew
  crew<-matrix(
      c(3, 5,
        10, 15),
      nrow=2,ncol=2,byrow = TRUE)



  # Quotas

  Q <-reactive({# Discount rate per sector (j) of fleet (i)
    matrix(
      c(alphaZero[1]*betaZero[1,1]*3.5, alphaZero[1]*betaZero[1,2]*3.5,
        alphaZero[2]*betaZero[2,1]*3.5, alphaZero[2]*betaZero[2,2]*3.5),
      nrow=2,ncol=2,byrow = TRUE)
  })

  # Demand Chareacterististics

  # Choke price
  a=c(.2,.5)


  # Slope
  b<-c(0.2,.25)

  # Process Percentages (canning, curing, etc.)
  pctg<-reactive({
    input$ProcessPctg
  })

  # Discard Percentage Landings
  disc1<-reactive({
    input$LandingDiscard
  })

  # Discard Percentage Processing
  disc2<-reactive({
    input$ProcDiscard
  })

  # Summary inputs
  summ1=reactive({
    validate(
      need(input$ProcessingSummary, "Por favor seleccione al menos un proceso")
    )
    input$ProcessingSummary
  })

  # max share for artisanals

  maxalpha<-reactive({
    input$maxalpha/100
  })


  # Recomendation range

  rec.range<-reactive({
    input$rec.range/100
  })


  ###############################
  ########### Results ###########
  ###############################

  # Descriptive demands

  D<-reactive({
    demandSimulator(a,b)
  })

  output$plotDemand<-renderPlot(
    ggplot(D(),aes(Captura,CHD,color="CHD"))+
      geom_line(size=2)+
      geom_line(data=D(),aes(Captura,HP,color="CHI"),size=2,lineend = "round")+
      labs(color="Mercado")+
      ylab('Precio')+
      xlab('Captura')+
      ggtitle("Demanda de Mercado")
  )

  # Allocation
  CUR.ALL<-reactive({
    data.frame(Flota=c("Artesanal","Menor Escala", "Industrial de Madera", "Industrial de Acero","Total"),
               Cuota=c(Q()[1,1],Q()[1,2],Q()[2,1],Q()[2,2],sum(Q()))
    )
  })

  output$tableCurrAll = renderDataTable({
    datatable(CUR.ALL(),rownames=FALSE,
              options=list(paging=FALSE,ordering=FALSE,searching=FALSE,
                           info=FALSE,searchable=FALSE)
    ) %>% formatRound('Cuota', 3)
  })


  # Current evaluation of the scenarios

  # Evaluate
  DB.CURRENT<-reactive({
    singleEvaluation(a,b,marg.cost,3.5,alphaZero,betaZero,cap,crew,pctg(),disc1(),disc2())
  })

  DB.CURRENT.UPPER<-reactive({
    singleEvaluation(a,b,marg.cost,3.5*(1+rec.range()),alphaZero,betaZero,cap,crew)
  })

  DB.CURRENT.BOTTOM<-reactive({
    singleEvaluation(a,b,marg.cost,3.5*(1-rec.range()),alphaZero,betaZero,cap,crew)
  })

  # Stack results into a data frame
  DB.RANGE<-reactive({
    data.frame(
      Value=DB.CURRENT()$Value_F,
      Labor=DB.CURRENT()$Labor_F,
      Upper_V=DB.CURRENT.UPPER()$Value_F,
      Lower_V=DB.CURRENT.BOTTOM()$Value_F,
      Upper_L=DB.CURRENT.UPPER()$Labor_F,
      Lower_L=DB.CURRENT.BOTTOM()$Labor_F,
      Escenario=DB.CURRENT.BOTTOM()$Escenario
    )
  })


  DB.TRADEOFF<-reactive({
    tradeoffFunction(a,b,marg.cost,3.5,betaZero,cap,crew,maxalpha())
  })


  # Gotta tell it the scenarios otherwise it crashes
  SCTP<-data.frame(
    Escenario=c("Status Quo","Cuota MC","Cuota MA","Transferible")
  )
  SCTP<-factor(SCTP$Escenario,levels = c("Status Quo","Cuota MC","Cuota MA","Transferible"))

  # Value per fleet
  VF <-reactive({
    data.frame(
      Value=c(DB.CURRENT()$Value_A,DB.CURRENT()$Value_I),
      Flota=c(replicate(4,"CHD"),replicate(4,"CHI")),
      Escenario=SCTP
    )
  })

  # Labor per fleet
  LF <-reactive({
    data.frame(
      Labor=c(DB.CURRENT()$Labor_A,DB.CURRENT()$Labor_I),
      Flota=c(replicate(4,"CHD"),replicate(4,"CHI")),
      Escenario=SCTP
    )
  })

  # Harvest per fleet
  HF <-reactive({
    data.frame(
      Harvest=c(DB.CURRENT()$Harvest_AS+DB.CURRENT()$Harvest_AL,
              DB.CURRENT()$Harvest_IS+DB.CURRENT()$Harvest_IL),
      Flota=c(replicate(4,"CHD"),replicate(4,"CHI")),
      Escenario=SCTP
    )
  })

  # Processing Dataframe
  PR <-reactive({
    data.frame(
      Profit=c(DB.CURRENT()$Profit_PC,
               DB.CURRENT()$Profit_PF,
               DB.CURRENT()$Profit_PU,
               DB.CURRENT()$Profit_PS),
      Labor=c(DB.CURRENT()$Labor_PC,
              DB.CURRENT()$Labor_PF,
              DB.CURRENT()$Labor_PU,
              DB.CURRENT()$Labor_PS),
      Production=c(DB.CURRENT()$Production_PC,
                   DB.CURRENT()$Production_PF,
                   DB.CURRENT()$Production_PU,
                   DB.CURRENT()$Production_PS),
      Process=c(replicate(4,"Canning"),replicate(4,"Freezing"),replicate(4,"Curing"),replicate(4,"Salted/Fresh")),
      Escenario=SCTP
    )
  })

  # Processing Discard Dataframe
  DISC <-reactive({
    data.frame(
      Profit=c(DB.CURRENT()$Profit_DH,
               DB.CURRENT()$Profit_DA,
               DB.CURRENT()$Profit_DL),
      Labor=c(DB.CURRENT()$Labor_DH,
              DB.CURRENT()$Labor_DA,
              DB.CURRENT()$Labor_DL),
      Production=c(DB.CURRENT()$Production_DH,
                   DB.CURRENT()$Production_DA,
                   DB.CURRENT()$Production_DL),
      Product=c(replicate(4,"Harina de Alta Calidad"),replicate(4,"Aceite de Pescado"),replicate(4,"Harina de Baja Calidad")),
      Escenario=SCTP
    )
  })

  # Industrial Fleet Processing
  IND <-reactive({
    data.frame(
      Profit=c(DB.CURRENT()$Profit_FM,
               DB.CURRENT()$Profit_FO),
      Labor=c(DB.CURRENT()$Labor_FM,
              DB.CURRENT()$Labor_FO),
      Production=c(DB.CURRENT()$Production_FM,
                   DB.CURRENT()$Production_FO),
      Product=c(replicate(4,"Harina"),replicate(4,"Aceite")),
      Escenario=SCTP
    )
  })

  # Processing Summary Dataframe
  SUMMARY <-reactive({
    data.frame(
      Profit=unlist(DB.CURRENT()[,paste("Profit",summ1(),sep="_")]),
      Labor=unlist(DB.CURRENT()[,paste("Labor",summ1(),sep="_")]*1000),
      Production=unlist(DB.CURRENT()[,paste("Production",summ1(),sep="_")]),
      Process=rep(summ1(),4)[order(match(rep(summ1(),4),c("PC","PF","PU","PS","DH","DL","DA","FM","FO")))],
      Escenario=SCTP
    ) #note that this takes the inputs and finds which columns of DB.CURRENT() match to generate the table for making the graphics and table
  })

  # And the final act, the optimization

  gamma<-reactive({
    input$gamma/100
  })

  DB.OPTIMAL<-reactive({
    optimizationLoop(a,b,marg.cost,3.5,betaZero,gamma(),cap,crew)
  })


  SCOP<-data.frame(
    Escenario=c("Cuota MC","Cuota MA","Transferible",
               "Cuota MC","Cuota MA","Transferible",
               "Cuota MC","Cuota MA","Transferible",
               "Cuota MC","Cuota MA","Transferible")
  )
  SCOP<-factor(SCOP$Escenario,levels = c("Cuota MC","Cuota MA","Transferible"))

  DB.COMPARISON<-reactive({
    data.frame(
      Value=c(DB.CURRENT()$Value_A[2:4],DB.OPTIMAL()$Value_A,DB.CURRENT()$Value_I[2:4],DB.OPTIMAL()$Value_I),
      Flota=c(replicate(6,"CHD"),replicate(6,"CHI")),
      Allocation=c(replicate(3,"Actual"),replicate(3,"Óptimo"),
                   replicate(3,"Actual"),replicate(3,"Óptimo")),
      Escenario=SCOP
             )
    })

  #########################
  # Plots and stuff
  #########################

  # Total Value
  output$plotSingle1<-renderPlot(
    ggplot(data=DB.CURRENT(), aes(x=Escenario, y=Value_F,fill=Escenario)) +
      geom_bar(colour="black", stat="identity")+
      ggtitle("Valor Económico Total")+
      ylab('Valor Total (Millones USD)')+
      geom_text(aes(label = round(Value_F,2), y = Value_F/2), size = 5)+
      xlab('Escenario')+
      guides(fill=FALSE)
  )

  # Total Labor
  output$plotSingle2<-renderPlot(
    ggplot(data=DB.CURRENT(), aes(x=Escenario, y=Labor_F,fill=Escenario)) +
      geom_bar(colour="black", stat="identity")+
      ggtitle("Empleo Total")+
      ylab('Puestos de Trabajo (Miles)')+
      geom_text(aes(label = round(Labor_F,2), y = Labor_F/2), size = 5)+
      xlab('Escenario')+
      guides(fill=FALSE)
  )

  # Value per fleet
  output$plotSingle3<-renderPlot(
    ggplot(data=VF(), aes(Escenario,Value,fill=Flota)) +
      geom_bar(stat="identity")+
      ggtitle("Valor Económico Total por Flota")+
      ylab('Valor (Millones USD)')+
      geom_text(aes(label = round(Value,2),
                    y = ifelse(Flota=='CHI',Value/2,ave(Value,Escenario,FUN=sum)-(Value/2))),
                    size = 5)+
      xlab('Escenario')
  )

  # Labor per fleet
  output$plotSingle4<-renderPlot(
    ggplot(data=LF(), aes(Escenario,Labor,fill=Flota)) +
      geom_bar(stat="identity")+
      ggtitle("Empleo por Flota")+
      ylab('Puestos de Trabajo (Miles)')+
      geom_text(aes(label = round(Labor,2),
                    y = ifelse(Flota=='CHI',Labor/2,ave(Labor,Escenario,FUN=sum)-(Labor/2))),
                    size = 5)+
      xlab('Escenario')
  )

  # Harvest per fleet
  output$plotSingle5<-renderPlot(
    ggplot(data=HF(), aes(Escenario,Harvest,fill=Flota)) +
      geom_bar(stat="identity")+
      ggtitle("Captura Total por Flota")+
      ylab('Captura (Millones de toneladas)')+
      geom_text(aes(label = round(Harvest,2),
                    y = ifelse(Flota=='CHI',Harvest/2,ave(Harvest,Escenario,FUN=sum)-(Harvest/2))),
                    size = 5)+
      xlab('Escenario')
  )

  # Tradeoff for value

  output$plotTradeoff1<-renderPlot(
    ggplot(DB.TRADEOFF(),aes(Value_A,Value_I,color=Escenario))+
      geom_path(size=2)+
      geom_point(data=DB.CURRENT(), size=7,aes(Value_A,Value_I,color=Escenario))+
      ggtitle("Tradeoffs de Valor entre Flotas")+
      ylab("Valor CHI (Millones USD)")+
      xlab("Valor CHD (Millones USD)")
  )

  output$tableTradeoff1 = renderDataTable({
    datatable(data.frame( Escenario = DB.CURRENT()$Escenario,
                          PE = DB.CURRENT()$Value_A,
                          CHI = DB.CURRENT()$Value_I,
                          Total = DB.CURRENT()$Value_F),
              rownames=FALSE,
              options=list(paging=FALSE,ordering=FALSE,searching=FALSE,
                           info=FALSE,searchable=FALSE),
              colnames = c("Escenario","CHD","CHI","Total")
    ) %>% formatRound(c('PE','CHI','Total'), 3)
  })

  # Tradeoff labor and value

  output$plotTradeoff2<-renderPlot(
    ggplot(DB.TRADEOFF(),aes(Labor_F,Value_F,color=Escenario))+
      geom_path(size=2)+
      geom_point(data=DB.CURRENT(), size=7,aes(Labor_F,Value_F,color=Escenario))+
      ggtitle("Tradeoff entre  Empleo y Valor")+
      ylab("Valor Total (Millones de USD)")+
      xlab("Puestos de Trabajo (Miles)")
  )

  output$tableTradeoff2 = renderDataTable({
    datatable(data.frame( Escenario = DB.CURRENT()$Escenario,
                          Labor = DB.CURRENT()$Labor_F,
                          Value = DB.CURRENT()$Value_F),
              rownames=FALSE,
              options=list(paging=FALSE,ordering=FALSE,searching=FALSE,
                           info=FALSE,searchable=FALSE),
              colnames = c("Escenario","Puestos de Trabajo","Valor")
    ) %>% formatRound(c('Value','Labor'), 2)
  })

  #Tradeoff labor-labor

  output$plotTradeoff3<-renderPlot(
    ggplot(DB.TRADEOFF(),aes(Labor_A,Labor_I,color=Escenario))+
      geom_path(size=2)+
      geom_point(data=DB.CURRENT(), size=7,aes(Labor_A,Labor_I,color=Escenario))+
      ggtitle("Tradeoff de Empleo entre Flotas")+
      ylab("Puestos de Trabajo CHI (Miles)")+
      xlab("Puestos de Trabajo CHD (Miles)")
  )

  output$tableTradeoff3 = renderDataTable({
    datatable(data.frame( Escenario = DB.CURRENT()$Escenario,
                          Artisanal = DB.CURRENT()$Labor_A,
                          Industrial = DB.CURRENT()$Labor_I,
                          Total = DB.CURRENT()$Labor_F),
              rownames=FALSE,
              options=list(paging=FALSE,ordering=FALSE,searching=FALSE,
                           info=FALSE,searchable=FALSE),
              colnames = c("Escenario","CHD","CHI","Total")
    ) %>% formatRound(c('Artisanal','Industrial','Total'), 2)
  })

  # Range on value
  output$plotRange1<-renderPlot(
    ggplot(data=DB.RANGE(), aes(x=Escenario, y=Value,fill=Escenario)) +
      geom_bar(colour="black", stat="identity")+
      geom_errorbar(aes(ymax = Upper_V,
                        ymin = Lower_V),
                    width=.75)+
      ggtitle("Valor Económico Total")+
      ylab('Valor Económico (Millones de USD)')+
      geom_text(aes(label = round(Value,2), y = Value/2), size = 5)+
      xlab('Escenario')+
      guides(fill=FALSE)
  )

  output$tableRange1 = renderDataTable({
    datatable(data.frame( Escenario = DB.RANGE()$Escenario,
                          Minimum = DB.RANGE()$Lower_V,
                          Estimated = DB.RANGE()$Value,
                          Maximum = DB.RANGE()$Upper_V),
              rownames=FALSE,
              options=list(paging=FALSE,ordering=FALSE,searching=FALSE,
                           info=FALSE,searchable=FALSE),
              colnames = c("Escenario","Mínimo","Estimado","Máximo")
    ) %>% formatRound(c('Minimum','Estimated','Maximum'), 2)
  })

  # Range on labor

  output$plotRange2<-renderPlot(
    ggplot(data=DB.RANGE(), aes(x=Escenario, y=Labor,fill=Escenario)) +
      geom_bar(colour="black", stat="identity")+
      geom_errorbar(aes(ymax = Upper_L,
                        ymin = Lower_L),
                    width=.75)+
      ggtitle("Empleo Total")+
      ylab('Puestos de Trabajo (Miles)')+
      geom_text(aes(label = round(Labor,2), y = Labor/2), size = 5)+
      xlab('Escenario')+
      guides(fill=FALSE)
  )

  output$tableRange2 = renderDataTable({
    datatable(data.frame( Escenario = DB.RANGE()$Escenario,
                          Minimum = DB.RANGE()$Lower_L,
                          Estimated = DB.RANGE()$Labor,
                          Maximum = DB.RANGE()$Upper_L),
              rownames=FALSE,
              options=list(paging=FALSE,ordering=FALSE,searching=FALSE,
                           info=FALSE,searchable=FALSE),
              colnames = c("Escenario","Mínimo","Estimado","Máximo")
    ) %>% formatRound(c('Minimum','Estimated','Maximum'), 2)
  })

  #Optimizer

  output$tableOptimizer1 = renderDataTable({
    datatable(data.frame( Cuota_MC = DB.OPTIMAL()$alpha_art[1]*100,
                          Cuota_MA = DB.OPTIMAL()$alpha_art[2]*100,
                          Transferible = DB.OPTIMAL()$alpha_art[3]*100),
              rownames=FALSE,
              options=list(paging=FALSE,ordering=FALSE,searching=FALSE,
                           info=FALSE,searchable=FALSE),
              colnames = c("Cuota MC","Cuota MA","Transferible")
    ) %>% formatRound(c('Cuota_MC','Cuota_MA','Transferible'), 2)
  })

  output$plotOptimizer1<-renderPlot(
    ggplot(DB.COMPARISON(),aes(x=Escenario,y=Value,fill=Allocation))+
      geom_bar(stat="identity",position = "dodge")+
      facet_wrap(~Flota)+
      ggtitle("Rendimiento Actual y Rendimiento Óptimo")+
      guides(fill=guide_legend(title="Distribución"))+
      ylab("Valor (Millones de USD)")+
      xlab("Escenario")
  )

#### Processing Tab ####

  output$plotProcessing1<-renderPlot({
    ggplot(data=PR(), aes(x=Escenario,y=Profit,fill=Process))+
      geom_bar(stat="identity")+
      geom_bar(stat="identity",color="black")+
      scale_fill_discrete(name="Producto",
                          labels= c("Enlatado","Curado","Congelado","Fresco/Salado"))+
      ggtitle("Valor Total del Proceso")+
      ylab('Valor (Millones de USD)')+
      xlab('Escenario') 
  })

  output$tableProcessing1 = renderDataTable({
    datatable(data.frame( Escenario = DB.CURRENT()$Escenario,
                          Enlatado=DB.CURRENT()$Profit_PC,
                          Congelado=DB.CURRENT()$Profit_PF,
                          Curado=DB.CURRENT()$Profit_PU,
                          Fresco=DB.CURRENT()$Profit_PS,
                          Total = DB.CURRENT()$Profit_PC+DB.CURRENT()$Profit_PF+DB.CURRENT()$Profit_PU+DB.CURRENT()$Profit_PS),
              rownames=FALSE,
              options=list(paging=FALSE,ordering=FALSE,searching=FALSE,
                           info=FALSE,searchable=FALSE)
    ) %>% formatRound(c('Enlatado','Congelado','Curado','Fresco','Total'), 2)
  })

  output$plotProcessing2<-renderPlot({
    ggplot(data=PR(), aes(x=Escenario,y=Labor*1000,fill=Process)) +
      geom_bar(stat="identity")+
      geom_bar(stat="identity",color="black")+
      scale_fill_discrete(name="Producto",
                          labels= c("Enlatado","Curado","Congelado","Fresco/Salado"))+
      ggtitle("Empleo Total por Proceso")+
      ylab('Puestos de Trabajo (Miles)')+
      xlab('Escenario')
  })

  output$tableProcessing2 = renderDataTable({
    datatable(data.frame( Escenario = DB.CURRENT()$Escenario,
                          Enlatado=DB.CURRENT()$Labor_PC*1000,
                          Congelado=DB.CURRENT()$Labor_PF*1000,
                          Curado=DB.CURRENT()$Labor_PU*1000,
                          Fresco=DB.CURRENT()$Labor_PS*1000,
                          Total = DB.CURRENT()$Labor_PC*1000+DB.CURRENT()$Labor_PF*1000+DB.CURRENT()$Labor_PU*1000+DB.CURRENT()$Labor_PS*1000),
              rownames=FALSE,
              options=list(paging=FALSE,ordering=FALSE,searching=FALSE,
                           info=FALSE,searchable=FALSE)
    ) %>% formatRound(c('Enlatado','Congelado','Curado','Fresco','Total'), 2)
  })

  output$plotProcessing3<-renderPlot({
    ggplot(data=PR(), aes(x=Escenario,y=Production,fill=Process)) +
      geom_bar(stat="identity")+
      geom_bar(stat="identity",color="black")+
      scale_fill_discrete(name="Producto",
                          labels= c("Enlatado","Curado","Congelado","Fresco/Salado"))+
      ggtitle("Producción Total por Proceso")+
      ylab('Producción (Millones de toneladas)')+
      xlab('Escenario')
  })

  output$tableProcessing3 = renderDataTable({
    datatable(data.frame( Escenario = DB.CURRENT()$Escenario,
                          Enlatado=DB.CURRENT()$Production_PC,
                          Congelado=DB.CURRENT()$Production_PF,
                          Curado=DB.CURRENT()$Production_PU,
                          Fresco=DB.CURRENT()$Production_PS,
                          Total = DB.CURRENT()$Production_PC+DB.CURRENT()$Production_PF+DB.CURRENT()$Production_PU+DB.CURRENT()$Production_PS),
              rownames=FALSE,
              options=list(paging=FALSE,ordering=FALSE,searching=FALSE,
                           info=FALSE,searchable=FALSE)
    ) %>% formatRound(c('Enlatado','Congelado','Curado','Fresco','Total'), 2)
  })

  #### Artisanal Discard Tab ####

  output$plotDiscard1<-renderPlot({
    ggplot(data=DISC(), aes(x=Escenario,y=Profit,fill=Product))+
      geom_bar(stat="identity")+
      geom_bar(stat="identity",color="black",show.legend=F)+
      scale_fill_discrete(name="Producto",
                          breaks=c("Harina de Baja Calidad","Aceite de Pescado","Harina de Alta Calidad"))+
      ggtitle("Valor Total por Producto Descartado")+
      ylab('Valor (Millones de USD)')+
      xlab('Escenario')
  })

  output$tableDiscard1 = renderDataTable({
    D1=data.frame( Escenario = DB.CURRENT()$Escenario,
                   DL1=DB.CURRENT()$Profit_DL,
                   DA1=DB.CURRENT()$Profit_DA,
                   DH1=DB.CURRENT()$Profit_DH,
                   Total = DB.CURRENT()$Profit_DL+DB.CURRENT()$Profit_DA+DB.CURRENT()$Profit_DH)
    names(D1)[2:4]=c("Harina de Baja Calidad","Aceite de Pescado","Harina de Alta Calidad")
    datatable(D1,
              rownames=FALSE,
              options=list(paging=FALSE,ordering=FALSE,searching=FALSE,
                           info=FALSE,searchable=FALSE)
    ) %>% formatRound(2:5, 2)
  })

  output$plotDiscard2<-renderPlot(
    ggplot(data=DISC(), aes(x=Escenario,y=Labor*1000,fill=Product)) +
      geom_bar(stat="identity")+
      geom_bar(stat="identity",color="black",show.legend=F)+
      scale_fill_discrete(name="Producto",
                          breaks=c("Harina de Baja Calidad","Aceite de Pescado","Harina de Alta Calidad"))+
      ggtitle("Empleos Totales por Producto Descartado")+
      ylab('Puestos de Trabajo (Miles)')+
      xlab('Escenario')
  )

  output$tableDiscard2 = renderDataTable({
    D1=data.frame( Escenario = DB.CURRENT()$Escenario,
                   DL1=DB.CURRENT()$Labor_DL*1000,
                   DA1=DB.CURRENT()$Labor_DA*1000,
                   DH1=DB.CURRENT()$Labor_DH*1000,
                   Total = DB.CURRENT()$Labor_DL*1000+DB.CURRENT()$Labor_DA*1000+DB.CURRENT()$Labor_DH*1000)
    names(D1)[2:4]=c("Harina de Baja Calidad","Aceite de Pescado","Harina de Alta Calidad")
    datatable(D1,
              rownames=FALSE,
              options=list(paging=FALSE,ordering=FALSE,searching=FALSE,
                           info=FALSE,searchable=FALSE)
    ) %>% formatRound(2:5, 3)
  })

  output$plotDiscard3<-renderPlot(
    ggplot(data=DISC(), aes(x=Escenario,y=Production,fill=Product)) +
      geom_bar(stat="identity")+
      geom_bar(stat="identity",color="black",show.legend=F)+
      scale_fill_discrete(name="Producto",
                          breaks=c("Harina de Baja Calidad","Aceite de Pescado","Harina de Alta Calidad"))+
      ggtitle("Producción Total por Producto Descartado")+
      ylab('Producción (Millones de toneladas)')+
      xlab('Escenario')
  )

  output$tableDiscard3 = renderDataTable({
    D1=data.frame( Escenario = DB.CURRENT()$Escenario,
                   DL1=DB.CURRENT()$Production_DL,
                   DA1=DB.CURRENT()$Production_DA,
                   DH1=DB.CURRENT()$Production_DH,
                   Total = DB.CURRENT()$Production_DL+DB.CURRENT()$Production_DA+DB.CURRENT()$Production_DH)
    names(D1)[2:4]=c("Harina de Baja Calidad","Aceite de Pescado","Harina de Alta Calidad")
    datatable(D1,
              rownames=FALSE,
              options=list(paging=FALSE,ordering=FALSE,searching=FALSE,
                           info=FALSE,searchable=FALSE)
    ) %>% formatRound(2:5, 2)
  })

  #### Industrial Tab ####
  
  output$plotIndustrial1<-renderPlot(
    ggplot(data=IND(), aes(x=Escenario,y=Profit,fill=Product))+
      geom_bar(stat="identity")+
      geom_bar(stat="identity",color="black",show.legend=F)+
      scale_fill_discrete(name="Producto") +
      ggtitle("Valor Total de La Producción Industrial")+
      ylab('Valor (Millones de USD)')+
      geom_text(aes(label = round(Profit,2),
                    y = ifelse(Product=='Harina',Profit/2,ave(Profit,Escenario,FUN=sum)-(Profit/2))),
                size = 5)+
      xlab('Escenario')
  )
  
  output$plotIndustrial2<-renderPlot(
    ggplot(data=IND(), aes(x=Escenario,y=Labor*1000,fill=Product)) +
      geom_bar(stat="identity")+
      geom_bar(stat="identity",color="black",show.legend=F)+
      scale_fill_discrete(name="Producto") +
      ggtitle("Empleo Total de La Producción Industrial")+
      ylab('Puestos de Trabajo (Miles)')+
      geom_text(aes(label = round(Labor*1000,3),
                    y = ifelse(Product=='Harina',Labor*1000/2,ave(Labor*1000,Escenario,FUN=sum)-(Labor*1000/2))),
                size = 5)+
      xlab('Escenario')
  )
  
  output$plotIndustrial3<-renderPlot(
    ggplot(data=IND(), aes(x=Escenario,y=Production,fill=Product)) +
      geom_bar(stat="identity")+
      geom_bar(stat="identity",color="black",show.legend=F)+
      scale_fill_discrete(name="Producto") +
      ggtitle("Producción Industrial Total")+
      ylab('Producción (Millones de toneladas)')+
      geom_text(aes(label = round(Production,2),
                    y = ifelse(Product=='Harina',Production/2,ave(Production,Escenario,FUN=sum)-(Production/2))),
                size = 5)+
      xlab('Escenario')
  )
  
  
  #### Processing Summary Tab ####

  Pabbv=c("PC","PF","PU","PS","DH","DL","DA","FM","FO")
  Pcolors=c("#E16A86","#CD7E37","#A69100","#65A100","#00AB66","#00ACA5","#00A2D3","#7D89E6","#C86DD7")
  names(Pcolors)=Pabbv

  output$plotPSUMM1<-renderPlot(
    ggplot(data=SUMMARY(), aes(x=Escenario,y=Profit,fill=Process))+
      geom_bar(stat="identity")+
      geom_bar(stat="identity",color="black",show.legend=F)+
      scale_fill_manual(name="Producto",
                        breaks=c("PC","PF","PU","PS","DH","DL","DA","FM","FO"),labels=c("PC"="Enlatado","PF"="Congelado","PU"="Curado","PS"="Fresco/Salado","DH"="Harina\nAlta Calidad","DL"="Harina\nBaja Calidad","DA"="Aceite Artesanal","FM"="Harina Industrial","FO"="Aceite Industrial"),
                        values=Pcolors,drop=T,guide=guide_legend(reverse=T))+
      ggtitle("Valor Total por Proceso")+
      ylab('Valor (Millones USD)')+
      xlab('Escenario')+
      theme(legend.key.size = unit(1.5, "lines"))
  )

  output$tableSUMM1 = renderDataTable({
    SummDat1=SUMMARY()
    SummOrder1=match(sort(unique(SummDat1$Process)),unique(SummDat1$Process))
    SummDat1$Process=suppressMessages(revalue(SummDat1$Process,c("PC"="Enlatado","PF"="Congelado","PU"="Curado","PS"="Fresco/Salado","DH"="Harina\nAlta Calidad","DL"="Harina\nBaja Calidad","DA"="Aceite Artesanal","FM"="Harina Industrial","FO"="Aceite Industrial")))
    SummDat1=spread(SummDat1[c("Profit","Process","Escenario")],key=Escenario,value=Profit)
    SummDat1=SummDat1[SummOrder1,]
    SummDat1$Process=as.character(SummDat1$Process)
    if(nrow(SummDat1)>1){SummDat1[nrow(SummDat1)+1,]=c("Valor Total",colSums(SummDat1[2:5]))}
    datatable(SummDat1,
              rownames=FALSE,
              options=list(paging=FALSE,ordering=FALSE,searching=FALSE,
                           info=FALSE,searchable=FALSE)
    ) %>% formatRound(2:5,3)
  })


  output$plotPSUMM2<-renderPlot(
    ggplot(data=SUMMARY(), aes(x=Escenario,y=Labor,fill=Process))+
      geom_bar(stat="identity")+
      geom_bar(stat="identity",color="black",show.legend=F)+
      scale_fill_manual(breaks=c("PC","PF","PU","PS","DH","DL","DA","FM","FO"),labels=c("PC"="Enlatado","PF"="Congelado","PU"="Curado","PS"="Fresco/Salado","DH"="Harina\nAlta Calidad","DL"="Harina\nBaja Calidad","DA"="Aceite Artesanal","FM"="Harina Industrial","FO"="Aceite Industrial"),values=Pcolors,drop=T,guide=guide_legend(reverse=T))+
      ggtitle("Empleos Totales por Proceso")+
      ylab('Puestos de Trabajo (Miles)')+
      xlab('Escenario')+
      theme(legend.key.size = unit(1.5, "lines"))
  )

  output$tableSUMM2 = renderDataTable({
    SummDat2=SUMMARY()
    SummOrder2=match(sort(unique(SummDat2$Process)),unique(SummDat2$Process))
    SummDat2$Process=suppressMessages(revalue(SummDat2$Process,c("PC"="Enlatado","PF"="Congelado","PU"="Curado","PS"="Fresco/Salado","DH"="Harina\nAlta Calidad","DL"="Harina\nBaja Calidad","DA"="Aceite Artesanal","FM"="Harina Industrial","FO"="Aceite Industrial")))
    SummDat2=spread(SummDat2[c("Labor","Process","Escenario")],key=Escenario,value=Labor)
    SummDat2=SummDat2[SummOrder2,]
    SummDat2$Process=as.character(SummDat2$Process)
    if(nrow(SummDat2)>1){SummDat2[nrow(SummDat2)+1,]=c("Empleo Total",colSums(SummDat2[2:5]))}
    datatable(SummDat2,
              rownames=FALSE,
              options=list(paging=FALSE,ordering=FALSE,searching=FALSE,
                           info=FALSE,searchable=FALSE)
    ) %>% formatRound(2:5,3)
  })

  output$plotPSUMM3<-renderPlot(
    ggplot(data=SUMMARY(), aes(x=Escenario,y=Production,fill=Process))+
      geom_bar(stat="identity")+
      geom_bar(stat="identity",color="black",show.legend=F)+
      scale_fill_manual(breaks=c("PC","PF","PU","PS","DH","DL","DA","FM","FO"),labels=c("PC"="Enlatado","PF"="Congelado","PU"="Curado","PS"="Fresco/Salado","DH"="Harina\nAlta Calidad","DL"="Harina\nBaja Calidad","DA"="Aceite Artesanal","FM"="Harina Industrial","FO"="Aceite Industrial"),values=Pcolors,drop=T,guide=guide_legend(reverse=T))+
      ggtitle("Producción Total por Proceso")+
      ylab('Producción (Millones de toneladas)')+
      xlab('Escenario')+
      theme(legend.key.size = unit(1.5, "lines"))
  )

  output$tableSUMM3 = renderDataTable({
    SummDat3=SUMMARY()
    SummOrder3=match(sort(unique(SummDat3$Process)),unique(SummDat3$Process))
    SummDat3$Process=suppressMessages(revalue(SummDat3$Process,c("PC"="Enlatado","PF"="Congelado","PU"="Curado","PS"="Fresco/Salado","DH"="Harina\nAlta Calidad","DL"="Harina\nBaja Calidad","DA"="Aceite Artesanal","FM"="Harina Industrial","FO"="Aceite Industrial")))
    SummDat3=spread(SummDat3[c("Production","Process","Escenario")],key=Escenario,value=Production)
    SummDat3=SummDat3[SummOrder3,]
    SummDat3$Process=as.character(SummDat3$Process)
    if(nrow(SummDat3)>1){SummDat3[nrow(SummDat3)+1,]=c("Producción Total",colSums(SummDat3[2:5]))}
    datatable(SummDat3,
              rownames=FALSE,
              options=list(paging=FALSE,ordering=FALSE,searching=FALSE,
                           info=FALSE,searchable=FALSE)
    ) %>% formatRound(2:5,3)
  })


  #Summary Graphics

  # Total Value Summary (fishery)
  output$plotFsummary1<-renderPlot(
    ggplot(data=DB.CURRENT(), aes(x=Escenario, y=Value_F,fill=Escenario)) +
      geom_bar(colour="black", stat="identity")+
      ggtitle("Valor Económico Total")+
      ylab('Valor Económico Total (Millones USD)')+
      geom_text(aes(label = round(Value_F,2), y = Value_F/2), size = 5)+
      xlab('Escenario')+
      guides(fill=FALSE)
  )

  # Total Labor Summary (fishery)
  output$plotFsummary2<-renderPlot(
    ggplot(data=DB.CURRENT(), aes(x=Escenario, y=Labor_F,fill=Escenario)) +
      geom_bar(colour="black", stat="identity")+
      ggtitle("Número Total de Puestos de Trabajo")+
      ylab('Puestos de Trabajo (Miles)')+
      geom_text(aes(label = round(Labor_F,2), y = Labor_F/2), size = 5)+
      xlab('Escenario')+
      guides(fill=FALSE)
  )

  # Production Summary (fishery)
  output$plotFsummary3<-renderPlot(
    ggplot(data=DB.CURRENT(), aes(Escenario,Harvest_F,fill=Escenario)) +
      geom_bar(colour="black",stat="identity")+
      ggtitle("Captura Total")+
      ylab('Captura (Millones de  toneladas)')+
      geom_text(aes(label = round(Harvest_F,2), y = Harvest_F/2), size = 5)+
      xlab('Escenario')+
      guides(fill=FALSE)
  )

  # Total Value Summary (processing) NOTE THIS IS PROFIT RIGHT NOW
  output$plotPsummary1<-renderPlot(
    ggplot(data=DB.CURRENT(), aes(x=Escenario, y=Profit_P,fill=Escenario)) +
      geom_bar(colour="black", stat="identity")+
      ggtitle("Valor Económico Total")+
      ylab('Valor Económico Total (Millones de USD)')+
      geom_text(aes(label = round(Profit_P,2), y = Profit_P/2), size = 5)+
      xlab('Escenario')+
      guides(fill=FALSE)
  )

  # Total Labor Summary (processing)
  output$plotPsummary2<-renderPlot(
    ggplot(data=DB.CURRENT(), aes(x=Escenario, y=Labor_P*1000,fill=Escenario)) +
      geom_bar(colour="black", stat="identity")+
      ggtitle("Número Total de Puestos de Trabajo")+
      ylab('Puestos de Trabajo (Miles)')+
      geom_text(aes(label = round((Labor_P*1000),2), y = (Labor_P*1000)/2), size = 5)+
      xlab('Escenario')+
      guides(fill=FALSE)
  )

  # Production Summary (processing)
  
  output$plotPsummary3<-renderPlot(
    ggplot(data=DB.CURRENT(), aes(x=Escenario,y=Production_P,fill=Escenario)) +
      geom_bar(colour="black",stat="identity")+
      ggtitle("Producción Total")+
      ylab('Producción (Millones de toneladas)')+
      geom_text(aes(label = round(Production_P,2), y = Production_P/2), size = 5)+
      xlab('Escenario')+
      guides(fill=FALSE)
  )


  #### Creation of the pdf download file ####
  
  output$downloadReport <- downloadHandler(
    filename = "Reporte Anchoveta.pdf",
    
    content = function(file) {
      src <- normalizePath('Reporte.Rmd')
      
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'Reporte.Rmd')
      
      library(rmarkdown)
      out <- render('Reporte.Rmd', pdf_document())
      file.rename(out, file)
    }
  )
  
  ResumenGeneral <-reactive({
    data.frame(
      Production=c(DB.CURRENT()$Harvest_F[1:2],
                   DB.CURRENT()$Production_P[1:2]),
      Profit=c(DB.CURRENT()$Value_F[1:2],
               DB.CURRENT()$Profit_P[1:2]),
      Labor=c(DB.CURRENT()$Labor_F[1:2],
              DB.CURRENT()$Labor_P[1:2]),
      Sector=c("Fishery","Fishery","Processing","Processing"),
      Escenario=SCTP[1:2] #Cambio Renato 
    )
  })
  
  ##### END OF SCRIPT
})
