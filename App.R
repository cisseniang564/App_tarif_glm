library(shiny)
library(shinydashboard)
library(plotly)
#install.packages("DT")
library(shinythemes)
library(DT)
library(shinydashboard)
library(corrplot)
library(questionr)
library(shinythemes)
#library(CASdatasets)
library(xts)
library(sp)
library(ggplot2)
library(tidyr)
library(stats)
#require(MASS)
# data(freMTPLfreq)
# data(freMTPLsev)
# DATAF = freMTPLfreq
# DATACM = freMTPLsev
#setwd("/Users/cisseniang/Desktop/Data")

ui <- dashboardPage(skin = "purple",
                    dashboardHeader(title = "Author : Cissé NIANG"
                    ),
                    
                    
                    dashboardSidebar(),
                    
                    dashboardBody(
                      box(width = 5),
                      
                      navbarPage("", collapsible = TRUE, inverse = TRUE, theme = shinytheme("spacelab"),header = NULL,
                                 footer = NULL,
                                 tabPanel("TARIFICATION EN IARD",
                                          fluidPage(
                                            
                                            tabsetPanel(
                                              tabPanel("Apperçu des données", br(),
                                                       fluidRow(

                                                         tabBox(width = 14,
                                                                navbarMenu("Lectures des données",
                                                                  tabPanel("Données CM",
                                                                           DT::dataTableOutput("tableCM")),
                                                                  tabPanel("Données FREQ",
                                                                           
                                                                           DT::dataTableOutput("tableFREQ")),

                                                                  
                                                                ),
                                                                
                                                                navbarMenu("Statistiques descriptives",
                                                                           tabPanel("Summary des CM",
                                                                                    verbatimTextOutput("summaryCM")),
                                                                           tabPanel("Summary des Freq",
                                                                                    verbatimTextOutput("summaryFREQ")),
                                                                           tabPanel("Matrice de Corrélation",
                                                                                    verbatimTextOutput("Mat_Corr"))
                                                                  
                                                                ),
                                                                navbarMenu("graphiques",
                                                                           tabPanel("Corrélation des var numériques",
                                                                                    plotOutput("Mat_Corrplot")),
                                                                           tabPanel("Corrélation des var non numériques",
                                                                                    plotOutput("Mat_Corrplot1")),
                                                                           tabPanel("Box plot entre DriverAge and Brand",
                                                                                    plotOutput("box1")),
                                                                           tabPanel("Box plot entre Power et DriverAge",
                                                                                    plotOutput("box2")),
                                                                           tabPanel("Box plot entre Gas et DriverAge",
                                                                                    plotOutput("box3")),
                                                                           tabPanel("Box plot entre region et DriverAge",
                                                                                    plotOutput("box4"))
                                                                             
                                                                           ),
                                                                
                                                                
                                                                  
                                                                )),
                                                       ),

                                              tabPanel("Analyse graphique de la Fréquence", br(),

                                                       fluidRow(

                                                         tabBox(width = 14,
                                                                navbarMenu("graphiques",
                                                                           tabPanel("Puissance du véhicule",
                                                                                      plotOutput("freq1")),
                                                                           tabPanel("Région du véhicule",
                                                                                    plotOutput("freq2")),
                                                                           tabPanel("Age du conducteur",
                                                                                    plotOutput("freq3")),
                                                                           tabPanel("Marque du véhicule",
                                                                                    plotOutput("freq4")),
                                                                           tabPanel("Age du véhicule",
                                                                                    plotOutput("freq5")),
                                                                           tabPanel("Type de carburant",
                                                                                    plotOutput("freq6")),
                                                                           tabPanel("Densité de la population",
                                                                                    plotOutput("freq7")),
                                                                  ),
                                                                navbarMenu("Nombre de sinistres",
                                                                           tabPanel("Nombres de sinistres sur la base freq",
                                                                                    verbatimTextOutput("sinistres")),
                                                                           tabPanel("Nombre de réclamations par modalités dans la base Freq",
                                                                                    verbatimTextOutput("reclam")),
                                                                           # tabPanel("Nombre de réclamations par modalités dans la base Freq (Pie)",
                                                                           #          verbatimTextOutput("reclam1")),
                                                                             
                                                                           ),
                                                         
                                                                  
                                                                ),
                                                                  
                                                                ),



                                                       ),
                                              tabPanel("Modélisation statistique", br(),
                                                fluidRow(
                                                  tabBox(width = 20,
                                                         navbarMenu("Modèles",
                                                                    tabPanel("Modèle glm de la constante",
                                                                             verbatimTextOutput("glm")),
                                                                    tabPanel("Modèle glm de toutes les variables",
                                                                             verbatimTextOutput("glm2")),
                                                                    tabPanel("Modèle forward",
                                                                             verbatimTextOutput("forward")),
                                                                    tabPanel("Modèle forward coefficients",
                                                                             verbatimTextOutput("coeff_f")),
                                                                    tabPanel("Modèle backward",
                                                                             verbatimTextOutput("backward")),
                                                                    tabPanel("Modèle backward coefficients",
                                                                             verbatimTextOutput("coeff_b")),
                                                                    tabPanel("Modèle both",
                                                                             verbatimTextOutput("both")),
                                                                    tabPanel("Modèle both coefficients",
                                                                             verbatimTextOutput("coeff_bt")),
                                                                      
                                                                    ),
                                                         
                                                                             
                                                                    
                                                                    
                                                  
                                                         ),
                                                  ),
                                                ),
                                              tabPanel("Découpage et création des datasets",
                                                       fluidRow(
                                                         tabBox(width = 12,
                                                           navbarMenu("Découpage des datasets",
                                                                      tabPanel("Découper la variable DriverAge",
                                                                               verbatimTextOutput("DriverAge")),
                                                                      tabPanel("Découper la variable CarAge",
                                                                               verbatimTextOutput("CarAge")),
                                                                      tabPanel("Découper la variable Density",
                                                                               verbatimTextOutput("Density")),
                                                                        
                                                                      ),
                                                           navbarMenu("Identifier les modalités les plus fréquentes",
                                                                      tabPanel("modalités plus fréquentes pour la variable DriverAge",
                                                                               verbatimTextOutput("sortDriverAge")),
                                                                      tabPanel("modalités plus fréquentes pour la variable CarAge",
                                                                               verbatimTextOutput("sortCarAge")),
                                                                      tabPanel("modalités plus fréquentes pour la variable Density",
                                                                               verbatimTextOutput("sortDensity")),
                                                                      tabPanel("modalités plus fréquentes pour la variable Power",
                                                                               verbatimTextOutput("sortPower")),
                                                                      tabPanel("modalités plus fréquentes pour la variable Gas",
                                                                               verbatimTextOutput("sortGas")),
                                                                      tabPanel("modalités plus fréquentes pour la variable Region",
                                                                               verbatimTextOutput("sortRegion")),
                                                                      tabPanel("modalités plus fréquentes pour la variable Brand",
                                                                               verbatimTextOutput("sortBrand")),
                                                                      
                                                                      ),
                                                           navbarMenu("Régression après Sélection des modalités de référence",
                                                                      tabPanel("Première régression",
                                                                               verbatimTextOutput("Reg1")),
                                                                      tabPanel("Deuxième régression",
                                                                               verbatimTextOutput("Reg2")),
                                                                      tabPanel("Troixième régression",
                                                                               verbatimTextOutput("Reg3")),
                                                                      tabPanel("Quatrième régression",
                                                                               verbatimTextOutput("Reg4")),
                                                                      tabPanel("Prédiction",
                                                                               verbatimTextOutput("Pred")),
                                                             
                                                           ),
                                                             
                                                           ),
                                                         ),
                                                       ),
                                                
                                              )
                                              )
                                            )
                                              
                                                       
                                              )
                                          )
                                 )
                      

                                
                    

server <- function(input, output){ 
  #library(CASdatasets)
  freMTPLsev<-read.csv("freMTPLsev.csv")
  # data("freMTPLsev")
  # freMTPLsev<-freMTPLsev
  output$tableCM <- renderDataTable({freMTPLsev
  
  },
  extensions = c('Buttons') , class = 'stripe compact', selection = 'none',
  options = list(
    dom = 'Bfrtip',
    buttons = list(
      'copy',
      'print',
      list(
        extend = 'collection',
        buttons = c('csv', 'excel', 'pdf'),
        text = 'Download'
      )
    )
    ,
    scrollY = TRUE,
    scrollX = TRUE,
    ordering = TRUE,
    paging = TRUE,
    searching = TRUE,
    info = TRUE,
    columnDefs = list(list(
      className = 'dt-left', targets = '_all'
    ))
  ))
  
  freMTPLfreq<-read.csv("freMTPLfreq.csv")
  # data("freMTPLfreq")
  # freMTPLfreq<-freMTPLfreq
  output$tableFREQ <- renderDataTable({freMTPLfreq
  },
  
  extensions = c('Buttons') , class = 'stripe compact', selection = 'none',
  options = list(
    dom = 'Bfrtip',
    buttons = list(
      'copy',
      'print',
      list(
        extend = 'collection',
        buttons = c('csv', 'excel', 'pdf'),
        text = 'Download'
      )
    )
    ,
    scrollY = TRUE,
    scrollX = TRUE,
    ordering = TRUE,
    paging = TRUE,
    searching = TRUE,
    info = TRUE,
    columnDefs = list(list(
      className = 'dt-left', targets = '_all'
    ))
  ))
  
  output$summaryCM<-renderPrint({
    
    summary(freMTPLsev)
  })
  
  output$summaryFREQ<-renderPrint({
    
    summary(freMTPLfreq)
  })
  
  output$Mat_Corr<-renderPrint({
    cor(freMTPLfreq[,c(5,6,10)])
    
  })
  
  output$Mat_Corrplot<-renderPlot({
    corrplot(cor(freMTPLfreq[,c(5,6,10)]))
    
  })
  
  output$Mat_Corrplot1<-renderPlot({
    Nom<-colnames(freMTPLfreq[, c(4, 7, 8 ,9)])
    n<-dim(freMTPLfreq[, c(4, 7, 8 ,9)])[2]
    Pvalue<-matrix(rep(0, n*n), ncol = n)
    colnames(Pvalue)<-Nom
    rownames(Pvalue)<-Nom
    
    for (i in (1:n)){
      for (j in (1:n)){
        
        temp<-table(freMTPLfreq[, c(4, 7, 8 ,9)][,i], freMTPLfreq[, c(4, 7, 8 ,9)][,j])
        Pvalue[i,j]<-cramer.v(temp)
      }
    }
    corrplot.mixed(Pvalue)
    
  })
  output$box1<-renderPlot({
    boxplot(freMTPLfreq$DriverAge~freMTPLfreq$Brand)
    
  })
  output$box2<-renderPlot({
    boxplot(freMTPLfreq$DriverAge~freMTPLfreq$Power)
    
  })
  
  output$box3<-renderPlot({
    boxplot(freMTPLfreq$DriverAge~freMTPLfreq$Gas)
    
  })
  
  output$box4<-renderPlot({
    boxplot(freMTPLfreq$DriverAge~freMTPLfreq$Region)
    
  })
  output$freq1<-renderPlot({

  PLOT_FREQ_VAR <<- function(VECT, N=freMTPLfreq$ClaimNb, EXP = freMTPLfreq$Exposure,nm){

    A<-by(N, VECT, sum)
    B<-by(EXP, VECT, sum)
    name_A<-names(A)
    name_B<-names(B)
    A<-as.vector(A)
    B<-as.vector(B)
    FREQ<-A/B
    DATA<-data.frame(FREQ=FREQ, EXP=B)
    rownames(DATA)<-name_A
    plot.new()
    plot.window(xlim = range(c(1:dim(DATA)[1])), ylim = range(DATA[,1]), col.main = "darkblue",
                col.lab ="darkblue", cex.main =1.5, cex.lab=1.2,panel.first=grid())
    lines(c(1:dim(DATA)[1]),DATA[,1], type='h', col ='blue4', lwd = 3)
    lines(c(1:dim(DATA)[1]),DATA[,1], type='h', col= 'blue4', lwd =3)
    axis(side =1 , at =c(1:dim(DATA)[1]), labels = rownames(DATA))
    axis(side = 2, col = 'gold', lty =2, lwd = 0.5)
    title(xlab=paste(nm),cex.lab=1.2, col.lab = 'darkblue')
    title(ylab = 'Frequence',cex.lab=1.2, col.lab="darkblue")
    plot.window(xlim=range(c(1:dim(DATA)[1])), ylim = range(DATA[,2]))
    lines(c(1:dim(DATA)[1]),DATA[,2],type='b',col='darkgoldenrod',lwd=3)
    axis(4)
    title(main=nm,col.main="darkblue",cex.main=1.5)
    box()
    legend("top", c('Freq', 'Exp'), col = c('blue4','darkgoldenrod', pch=19,lty=1,cex.main =1))
  }


  PLOT_FREQ_VAR(freMTPLfreq$Power, nm = "Puissance du Vehicule")

  })
  
  output$freq2<-renderPlot({
    PLOT_FREQ_VAR(freMTPLfreq$Region, nm = "Region du Vehicule")
    
  })
  
  output$freq3<-renderPlot({
    PLOT_FREQ_VAR(freMTPLfreq$DriverAge, nm = "Age du Conducteur")
    
  })
  
  output$freq4<-renderPlot({
    PLOT_FREQ_VAR(freMTPLfreq$Brand, nm = "Marque du Vehicule")
    
  })
  
  output$freq5<-renderPlot({
    PLOT_FREQ_VAR(freMTPLfreq$CarAge, nm = "Age du Vehicule")
    
  })
  
  output$freq6<-renderPlot({
    PLOT_FREQ_VAR(freMTPLfreq$Gas, nm = "Type de Carburant")
    PLOT_FREQ_VAR(freMTPLfreq$Density, nm = "Densite de population")
    
  })
  
  output$freq7<-renderPlot({
    PLOT_FREQ_VAR(freMTPLfreq$Density, nm = "Densite de population")
    
  })

  output$sinistres<-renderPrint({
    db <- freMTPLfreq %>%
      filter(ClaimNb > 0)

    nrow(db)
    #mean(db)
    #var(db)
  })
  
  output$reclam<-renderPrint({
    a <- table(freMTPLfreq$ClaimNb)
    a
    
  })
  
  #  output$reclam1<-renderPlotly({
  #    
  #    paste(rownames(table(freMTPLfreq$ClaimNb)),table(freMTPLfreq$ClaimNb))
  #    
  #   pie(x = table(freMTPLfreq$ClaimNb), main = "Repartition du nombre de réclamations par modalités",
  #        col = palette()[2:5], labels = paste(rownames(table(freMTPLfreq$ClaimNb)),table(freMTPLfreq$ClaimNb)))
  #   
  # 
  # 
  # })
  
  output$glm<-renderPrint({
    
    
    Intercept_only<<-glm(ClaimNb ~ 1, family = poisson(link = 'log'),
              data = freMTPLfreq)
    summary(Intercept_only)
    
  })
  output$glm2<-renderPrint({

    # step(Reg1, direction = c("both","backward", "forward"))

    all<-glm(ClaimNb ~Power + CarAge + Brand + Gas + DriverAge + Region + Density + offset(log(Exposure)), family = poisson(link = 'log'),
               data = freMTPLfreq)
    summary(all)
  })
  
  output$forward<-renderPrint({
  
  ### Forward stepwise regression
  forward_<- step(Intercept_only, direction=c("forward"),scope=~Power + CarAge+ Brand + Gas + DriverAge + Region + Density +offset(log(Exposure)), trace=0)
  #resultats de l'analyse forward
  forward_$anova
  })
  
  output$coeff_f<-renderPrint({
    
    forward_$coefficients
  })
  
  output$backward<-renderPrint({
  ### Backward stepwise regression
  backward_<<- step((glm(ClaimNb ~Power + CarAge + Brand + Gas + DriverAge + Region + Density + offset(log(Exposure)), family = poisson(link = 'log'),
                      data = freMTPLfreq)), direction=c("backward"),trace=1)
  
  backward_$anova
  })
  
  output$coeff_b<-renderPrint({
    
    backward_$coefficients
  })
  
  output$both<-renderPrint({
    ### Backward stepwise regression
    both_<<- step((glm(Intercept_only,family = poisson(link = 'log'),
                           data = freMTPLfreq)), direction=c("both"), scope=~Power + CarAge+ Brand + Gas + DriverAge + Region + Density +offset(log(Exposure)),
                  trace=1)
    
    both_$anova
  })
  
  output$coeff_bt<-renderPrint({
    
    both_$coefficients
  })
  
  output$DriverAge<-renderPrint({
  freMTPLfreq_b<<-freMTPLfreq
  
  
  freMTPLfreq_b$DriverAge<-cut(freMTPLfreq$DriverAge, c(18,21,24,29,34,44,56, Inf), include.lowest = TRUE)
  head(freMTPLfreq_b$DriverAge)
  
  
  })
  
  output$CarAge<-renderPrint({
    
     
  freMTPLfreq_b$CarAge<-cut(freMTPLfreq$CarAge, c(0, 6, 18, 20, Inf), include.lowest = TRUE)
  head(freMTPLfreq_b$CarAge)
  
  
 })
  
  output$Density<-renderPrint({
    
  freMTPLfreq_b$Density<-cut(freMTPLfreq$Density, c(0, 50, 150, 250, 350, 450, 550, 1000, 4000, Inf), include.lowest =TRUE)
  head(freMTPLfreq_b$Density)
  
  
  
  })
  
  output$sortDriverAge<-renderPrint({
    
  sort(table(freMTPLfreq_b$DriverAge))
    
  })
  
  output$sortCarAge<-renderPrint({
    #attach(freMTPLfreq_b)
    #sort(table(freMTPLfreq_b$CarAge))
    sort(table(freMTPLfreq_b$CarAge<-cut(freMTPLfreq$CarAge, c(0, 6, 18, 20, Inf), include.lowest = TRUE)))
    
  })
  
  output$sortDensity<-renderPrint({
    #attach(freMTPLfreq_b)
    #sort(table(freMTPLfreq_b$Density))
    sort(table(freMTPLfreq_b$Density<-cut(freMTPLfreq$Density, c(0, 50, 150, 250, 350, 450, 550, 1000, 4000, Inf), include.lowest =TRUE)))
    
  })
  
  output$sortPower<-renderPrint({
    #attach(freMTPLfreq_b)
    sort(table(freMTPLfreq_b$Power))
    
  })
  
  output$sortGas<-renderPrint({
    sort(table(freMTPLfreq_b$Gas))
    
  })
  
  output$sortRegion<-renderPrint({
    #attach(freMTPLfreq_b)
    sort(table(freMTPLfreq_b$Region))
    
  })
  
  output$sortBrand<-renderPrint({
    #attach(freMTPLfreq_b)
    sort(table(freMTPLfreq_b$Brand))
    
  })
  
  output$Reg1<-renderPrint({

  Power2<-factor(freMTPLfreq_b$Power)
  Power2<-relevel(freMTPLfreq_b$Power, ref = 'f')
  Gas<-factor(freMTPLfreq_b$Gas)
  Gas<-relevel(freMTPLfreq_b$Gas, ref = 'Regular')
  Brand2<-factor(freMTPLfreq_b$Brand)
  Brand2<-relevel(freMTPLfreq_b$Brand, ref = 'Renault, Nissan or Citroen')
  Density<-factor(freMTPLfreq_b$Density)
  Density<-relevel(freMTPLfreq_b$Density, ref = '[0,50]')
  Region2<-factor(freMTPLfreq_b$Region)
  Region2<-relevel(freMTPLfreq_b$Region, ref = 'Centre')
  DriverAge<-factor(freMTPLfreq_b$DriverAge)
  DriverAge<-relevel(freMTPLfreq_b$DriverAge, ref = '(44,56]')
  CarAge<-factor(freMTPLfreq_b$CarAge)
  CarAge<-relevel(freMTPLfreq_b$CarAge, ref ='[0,6]')

  Reg1<-glm(ClaimNb ~ Power2 + CarAge + Brand2 + Gas + DriverAge + Region2 + Density + offset(log(Exposure)),
            family = poisson(link = 'log'))
  summary(Reg1)
  })
  
  output$Reg2<-renderPrint({
    
    Region2<-relevel(as.factor(freMTPLfreq_b$Region), ref = 'Centre')
    Brand2<-relevel(as.factor(freMTPLfreq_b$Brand), ref = 'Renault, Nissan or Citroen')
  
  #regroupement d,e,f,g,h;i,j;k,l,m,o
  freMTPLfreq_b$Power3=NULL
  freMTPLfreq_b$Power3[freMTPLfreq_b$Power %in% c('d', 'e', 'f', 'g', 'h' )]<-"P1"
  freMTPLfreq_b$Power3[freMTPLfreq_b$Power %in% c('i', 'j' )]<-"P2"
  freMTPLfreq_b$Power3[freMTPLfreq_b$Power %in% c('k', 'l', 'm','n','o' )]<-"P3"
  
  sort(table(freMTPLfreq_b$Power3))
  
  ###tansformer la variable catégolielle freMTPLfreq_b$Power3<-relevel(freMTPLfreq_b$Power3, ref = 'P1')
  freMTPLfreq_b$Power3 <- as.factor(freMTPLfreq_b$Power3)
  Reg2<-glm(ClaimNb ~ Power3 + CarAge + Brand2 + Gas + DriverAge + Region2 + Density + offset(log(Exposure)),
            family = poisson(link = 'log'), data=freMTPLfreq_b)
  summary(Reg2)
})
  
  output$Reg3<-renderPrint({
    
    freMTPLfreq_b$Power3=NULL
    freMTPLfreq_b$Power3[freMTPLfreq_b$Power %in% c('d', 'e', 'f', 'g', 'h' )]<-"P1"
    freMTPLfreq_b$Power3[freMTPLfreq_b$Power %in% c('i', 'j' )]<-"P2"
    freMTPLfreq_b$Power3[freMTPLfreq_b$Power %in% c('k', 'l', 'm','n','o' )]<-"P3"
    
    sort(table(freMTPLfreq_b$Power3))
    
    ###tansformer la variable cat?golielle freMTPLfreq_b$Power3<-relevel(freMTPLfreq_b$Power3, ref = 'P1')
    freMTPLfreq_b$Power3 <- as.factor(freMTPLfreq_b$Power3)
  
  #### regroupement brand
  freMTPLfreq_b$Brand3=NULL
  freMTPLfreq_b$Brand3[freMTPLfreq_b$Brand %in% c('other','Renault, Nissan or Citroen')]<-"B1"
  freMTPLfreq_b$Brand3[freMTPLfreq_b$Brand %in% c('Fiat','Mercedes, Chrysler or BMW')]<-"B2"
  freMTPLfreq_b$Brand3[freMTPLfreq_b$Brand %in% c('Volkswagen,Audi,skoda or seat','Opel, General Motors or Ford')]<-"B3"
  freMTPLfreq_b$Brand3[freMTPLfreq_b$Brand %in% c('Japanese (except Nissan) or Korean')]<-"B4"
  
  sort(table(freMTPLfreq_b$Brand3))
  
  freMTPLfreq_b$Brand3 = as.factor(freMTPLfreq_b$Brand3)
  freMTPLfreq_b$Brand3<-relevel(freMTPLfreq_b$Brand3, ref='B1')
  
  
  Reg3<-glm(ClaimNb ~ Power3 + CarAge + Brand3 + Gas + DriverAge + Region2 + Density + offset(log(Exposure)),
            family = poisson(link = 'log'), data=freMTPLfreq_b)
  summary(Reg3)

  })
  
  output$Reg4<-renderPrint({
    freMTPLfreq_b$Power3=NULL
    freMTPLfreq_b$Power3[freMTPLfreq_b$Power %in% c('d', 'e', 'f', 'g', 'h' )]<-"P1"
    freMTPLfreq_b$Power3[freMTPLfreq_b$Power %in% c('i', 'j' )]<-"P2"
    freMTPLfreq_b$Power3[freMTPLfreq_b$Power %in% c('k', 'l', 'm','n','o' )]<-"P3"
    
    sort(table(freMTPLfreq_b$Power3))
    
    ###tansformer la variable cat?golielle freMTPLfreq_b$Power3<-relevel(freMTPLfreq_b$Power3, ref = 'P1')
    freMTPLfreq_b$Power3 <- as.factor(freMTPLfreq_b$Power3)
    
    #### regroupement brand
    freMTPLfreq_b$Brand3=NULL
    freMTPLfreq_b$Brand3[freMTPLfreq_b$Brand %in% c('other','Renault, Nissan or Citroen')]<-"B1"
    freMTPLfreq_b$Brand3[freMTPLfreq_b$Brand %in% c('Fiat','Mercedes, Chrysler or BMW')]<-"B2"
    freMTPLfreq_b$Brand3[freMTPLfreq_b$Brand %in% c('Volkswagen,Audi,skoda or seat','Opel, General Motors or Ford')]<-"B3"
    freMTPLfreq_b$Brand3[freMTPLfreq_b$Brand %in% c('Japanese (except Nissan) or Korean')]<-"B4"
    
    sort(table(freMTPLfreq_b$Brand3))
    
    freMTPLfreq_b$Brand3 = as.factor(freMTPLfreq_b$Brand3)
    freMTPLfreq_b$Brand3<-relevel(freMTPLfreq_b$Brand3, ref='B1')
    
    
    #### regroupement region
    freMTPLfreq_b$Region3=NULL
    #freMTPLfreq_b$Region3=as.character(freMTPLfreq_b$Region)
    freMTPLfreq_b$Region3[freMTPLfreq_b$Region %in% c('Centre','Basse-Normandie','Pays-de-la-Loire','Bretagne','Haute-Normandie')]<-"C1"
    freMTPLfreq_b$Region3[freMTPLfreq_b$Region %in% c('Ile-de-France','Limousin','Poitou-Charentes')]<-"C2"
    freMTPLfreq_b$Region3[freMTPLfreq_b$Region %in% c('Nord-Pas-de-Calais','Aquitaine')]<-"C3"
    
    sort(table(freMTPLfreq_b$Region3))
    
    
    freMTPLfreq_b$Region3 = as.factor(freMTPLfreq_b$Region3)
    freMTPLfreq_b$Region3<-relevel(freMTPLfreq_b$Region3, ref='C1')
    
    
    Reg4<-glm(ClaimNb ~ Power3 + CarAge + Brand3 + Gas + DriverAge + Region3 + Density + offset(log(Exposure)),
              family = poisson(link = 'log'), data=freMTPLfreq_b)
    summary(Reg4)
  
})
  output$Pred<-renderPrint({
    
    
    freMTPLfreq_b$Power3=NULL
    freMTPLfreq_b$Power3[freMTPLfreq_b$Power %in% c('d', 'e', 'f', 'g', 'h' )]<-"P1"
    freMTPLfreq_b$Power3[freMTPLfreq_b$Power %in% c('i', 'j' )]<-"P2"
    freMTPLfreq_b$Power3[freMTPLfreq_b$Power %in% c('k', 'l', 'm','n','o' )]<-"P3"
    
    sort(table(freMTPLfreq_b$Power3))
    
    ###tansformer la variable cat?golielle freMTPLfreq_b$Power3<-relevel(freMTPLfreq_b$Power3, ref = 'P1')
    freMTPLfreq_b$Power3 <<- as.factor(freMTPLfreq_b$Power3)
    
    #### regroupement brand
    freMTPLfreq_b$Brand3=NULL
    freMTPLfreq_b$Brand3[freMTPLfreq_b$Brand %in% c('other','Renault, Nissan or Citroen')]<-"B1"
    freMTPLfreq_b$Brand3[freMTPLfreq_b$Brand %in% c('Fiat','Mercedes, Chrysler or BMW')]<-"B2"
    freMTPLfreq_b$Brand3[freMTPLfreq_b$Brand %in% c('Volkswagen,Audi,skoda or seat','Opel, General Motors or Ford')]<-"B3"
    freMTPLfreq_b$Brand3[freMTPLfreq_b$Brand %in% c('Japanese (except Nissan) or Korean')]<-"B4"
    
    sort(table(freMTPLfreq_b$Brand3))
    
    freMTPLfreq_b$Brand3 <- as.factor(freMTPLfreq_b$Brand3)
    freMTPLfreq_b$Brand3<-relevel(freMTPLfreq_b$Brand3, ref='B1')
    
    
    #### regroupement region
    freMTPLfreq_b$Region3=NULL
    #freMTPLfreq_b$Region3=as.character(freMTPLfreq_b$Region)
    freMTPLfreq_b$Region3[freMTPLfreq_b$Region %in% c('Centre','Basse-Normandie','Pays-de-la-Loire','Bretagne','Haute-Normandie')]<-"C1"
    freMTPLfreq_b$Region3[freMTPLfreq_b$Region %in% c('Ile-de-France','Limousin','Poitou-Charentes')]<-"C2"
    freMTPLfreq_b$Region3[freMTPLfreq_b$Region %in% c('Nord-Pas-de-Calais','Aquitaine')]<-"C3"
    
    sort(table(freMTPLfreq_b$Region3))
    
    
    freMTPLfreq_b$Region3 <- as.factor(freMTPLfreq_b$Region3)
    freMTPLfreq_b$Region3<-relevel(freMTPLfreq_b$Region3, ref='C1')
    
    freMTPLfreqpred=predict((glm(ClaimNb ~ Power3 + CarAge + Brand3 + Gas + DriverAge + Region3 + Density + offset(log(Exposure)),
                           family = poisson(link = 'log'), data=freMTPLfreq_b)), data=freMTPLfreq_b, type = 'response')
    #freMTPLfreqpred
  })
  
  
  
}


shinyApp(ui, server)