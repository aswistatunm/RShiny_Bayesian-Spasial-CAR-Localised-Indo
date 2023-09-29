#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(rgdal)
library(spdep)
library(CARBayes)
library(readxl)
library(leaflet)
library(DT) #Tampilan data Tabel
library(coda) #uji konvergensi menggunakan trace plot

# ui Objek
#Tampilan Halaman
#dashboardHeader=Judul Kolom
#dashboardSidebar=Judul Baris
ui <- dashboardPage(
  dashboardHeader (title =  "BAYESIAN SPATIAL CAR LOCALISED"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data",
               menuSubItem("Managing Data",tabName = "sdata"),
               menuSubItem("Data Layout", tabName = "ldata")),
      menuItem(" Model Comparison", tabName = "model"),
      menuItem("Spatial Model", tabName = "model1"),
      menuItem("Convergence Test", tabName = "plot"),
      menuItem("Map of Relative Risk", tabName="peta")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName="sdata",
              fluidRow(
                box(
                  fileInput(
                    inputId = "filedata1",
                    label = "Upload Data (Excel File)",
                    accept = c(".xlsx")),
                  helpText("Select column name of the areas in the Data"),
                  selectInput("AreaCass", "Area Name",
                              choices=c(), selected =c())
                )
              ),
              fluidRow(
                box(width = 12,
                    helpText("Select columns cases and population in the Data"),
                    column(width = 6,
                           selectInput("Cass", "Cases",
                                       choices=c(), selected =c())),
                    column(width = 6,
                           selectInput("Popn", "Population",
                                       choices=c(), selected =c()))
                )
              ),
              fluidRow(
                box(width = 12,
                    helpText(div("Specify the values of burnin and nsample to model")),
                    column(width = 6,
                           numericInput("burn",label = "Burnin",1000, min = 1, max = 100000)),
                    column(width = 6,
                           numericInput("nsample",label = "n.Sample",10000, min = 1, max = 200000))
                )
              ),
              fluidRow(
                box(width = 12,
                    helpText(div("Optional. Select columns Covariate 1, covariate 2, covariate 3, covariate 4."),
                             div("Leave the boxes with - if the data do not contain covariates.")),
                    column(width = 3,
                           selectInput("Cova1", "Covariate 1",
                                       choices=c(), selected =c())),
                    column(width = 3,
                           selectInput("Cova2", "Covariate 2",
                                       choices=c(), selected =c())),
                    column(width = 3,
                           selectInput("Cova3", "Covariate 3",
                                       choices=c(), selected =c())),
                    column(width = 3,
                           selectInput("Cova4", "Covariate 4",
                                       choices=c(), selected =c()))
                )
              )
      ),
      tabItem(tabName = "ldata",
              fluidRow(titlePanel("Table Data"),DTOutput("TAB"))
      ),
      tabItem(tabName="model",
              fluidRow(titlePanel("Model Comparison"), DTOutput("modelx"))
      ),
      tabItem(tabName = "model1",
              fluidRow(titlePanel("Hyperprior"),
                       box(width = 3,numericInput("prior",NULL,0.1, min = 0, max = 1)),
                       box(width = 3,numericInput("tau",NULL,0.1, min = 0, max = 1)),
                       box(width = 3,numericInput("G",NULL,2, min = 2, max = 5))
              ),
              fluidRow(titlePanel("Parameter Estimation"), DTOutput("mo")),
              fluidRow( titlePanel("Model fit"), DTOutput("mo1"))
      ),
      tabItem(tabName = "plot",
              fluidRow( titlePanel("Trace Plot for tau2 Samples"),
                        plotOutput("plot1")),
              fluidRow( titlePanel("Trace Plot for phi Samples"),
                        plotOutput("plot2"))
      ),
      
      tabItem(tabName="peta",
              fluidRow( box(width = 12, height = 430,leafletOutput("petak"))
              ),
              fluidRow( titlePanel("Data"), DTOutput("table1"))
      )
    )),
  title = "Bayesian Spatial CAR Localised"
)

# Server
server <- function(input, output, session) {
  
  # ===== Input =====
  
  Data <- reactive({
    req(input$filedata1)
    read_excel (input$filedata1$datapath)
  })
  
  Map <- readOGR("Indonesia_Province/Indonesia_Province.shp")
  names(Map@data)[1] <-"Districts"
  
  observeEvent(Data(), {
    updateSelectInput(session, "AreaCass", choices = c(names(Data())),
                      selected = head(c(names(Data())),1))
  })
  observeEvent(Data(), {
    updateSelectInput(session, "Cass", choices = c("-",names(Data())),
                      selected = head(c("-",names(Data())),1))
  })
  observeEvent(Data(), {
    updateSelectInput(session, "Popn", choices = c("-",names(Data())),
                      selected = head(c("-",names(Data())),1))
  })
  observeEvent(Data(), {
    updateSelectInput(session, "Cova1", choices = c("-",names(Data())),
                      selected = head(c("-",names(Data())),1))
  })
  observeEvent(Data(), {
    updateSelectInput(session, "Cova2", choices = c("-",names(Data())),
                      selected = head(c("-",names(Data())),1))
  })
  observeEvent(Data(), {
    updateSelectInput(session, "Cova3", choices = c("-",names(Data())),
                      selected = head(c("-",names(Data())),1))
  })
  observeEvent(Data(), {
    updateSelectInput(session, "Cova4", choices = c("-",names(Data())),
                      selected = head(c("-",names(Data())),1))
  })
  
  # ===== Proses =====
  
  Datafix <- reactive({
    Datafix <- c(input$AreaCass,input$Cass,input$Popn) 
    cov1 <- input$Cova1 ; cov2 <- input$Cova2
    cov3 <- input$Cova3 ; cov4 <- input$Cova4
    if(cov1!="-"){Datafix <- c(Datafix,cov1)}
    if(cov2!="-"){Datafix <- c(Datafix,cov2)}
    if(cov3!="-"){Datafix <- c(Datafix,cov3)}
    if(cov4!="-"){Datafix <- c(Datafix,cov4)}
    Datafix
    
    D = Data()[,Datafix]
    order <- match(as.character(as.matrix(Map@data[,1])),
                   as.character(as.matrix(D[input$AreaCass])))
    Dt = D[order,]
    Dt
  })
  
  bobot <- reactive({
    W.nb <- poly2nb(Map)
    W <- nb2mat(W.nb, style = "B",zero.policy = TRUE)
    rownames(W)=NULL
    W[2,c(11,21)]=1 ; W[c(11,21),2] =1
    W[3,32] = 1 ; W[32,3] = 1
    W[21,c(2,22)] = 1; W[c(2,22),21] = 1
    W
  })
  
  
  Kasus <- reactive({
    Data1 <- Datafix()
    Exp<- sum(Data1[input$Cass])*Data1[input$Popn]/sum(Data1[input$Popn])
    Data1$Exp <- Exp[1:dim(Data1)[1],]
    Data1 <- as.data.frame(Data1)
    Data1
  })
  
  # Define user-defined function Residuals MMI
  get.morans.I.modified <- function(x, h){
    
    Wm <- t(h / colSums(h))
    
    # Compute modified moran's I
    Z<-(x-mean(x))
    ZW <- Wm %*% x-mean(x)
    a <- t(Z) %*% ZW
    b <- (t(Z) %*% Z)^(1/2)
    c <- (t(ZW) %*% ZW)^(1/2)
    
    I.modified <- (a/(b*c))
    return(I.modified)
  }
  
  Formu <- reactive({
    Data2 = Kasus()
    G = names(Data2)
    F2 = "offset(log(Exp))"
    if(length(G)==4){
      K = as.formula(paste(G[2],"~",F2))
      K
    } else {
      for (h in 4:(length(G)-1)) {
        l = G[h]
        F2 = paste( F2,"+",l)
      }
      K = as.formula(paste(G[2],"~",F2))
      K
    }
  })
  
  G <- reactive({
    Data3 = Kasus()
    c = matrix(c(1,1,0.1,0.5,0.5,0.01,0.1,0.1,0.05,0.0005), nrow = 5, byrow = FALSE)
    v = matrix(0,nrow = 3, ncol = 4)
    D = c(2,3,5)
    Gr= list()
    for (i in 1:dim(c)[1]) {
      f = as.vector(c[i,])
      for (g in 1:3) {
        set.seed(1)
        ModelLocalised <-S.CARlocalised(formula=Formu(),
                                        data=Data3,
                                        G=D[g],
                                        family="poisson",
                                        W=bobot(),
                                        burnin=input$burn,
                                        prior.tau2=f,
                                        n.sample=input$nsample)
        
        x <-ModelLocalised$residuals[,1]
        v[g,1] <- D[g]
        v[g,2] <- as.matrix(round(ModelLocalised$modelfit,2))[1,1]
        v[g,3] <- as.matrix(round(ModelLocalised$modelfit,2))[3,1]
        v[g,4] <- as.matrix(round(get.morans.I.modified(x, bobot()),2))
      }
      Gr[[i]]=v
    }
    Model=rbind(Gr[[1]],Gr[[2]],Gr[[3]],Gr[[4]],Gr[[5]])
    colnames(Model) = c("G","DIC","WAIC","MMI")
    row.names(Model) = c(rep("IG (1, 0.01)",3),
                         rep("IG (1, 0.1)",3),
                         rep("IG (0.1, 0.1)",3),
                         rep("IG (0.5, 0.05)",3),
                         rep("IG (0.5, 0.0005)",3))
    Model
  })
  
  
  ModelLocal <- reactive({
    set.seed(1)
    ModelLocal <-S.CARlocalised(formula=Formu(),
                                data=Kasus(),
                                G=input$G,
                                family="poisson",
                                W=bobot(),
                                burnin=input$burn,
                                prior.tau2=c(input$prior,input$tau),
                                n.sample=input$nsample)
    
    ModelLocal   
  })
  
  par(mfrow=c(1,2),mar=c(4,4,1.5,1.5),mex=0.8,tcl=0.3)
  
  tau2.samples<- reactive({
    mcmc.list(ModelLocal()$samples$tau2)
  })
  
  phi.samples<- reactive({
    par(mfrow=c(4,2),mar=c(4,4,1.5,1.5),mex=0.8,tcl=0.3)
    mcmc.list(ModelLocal()$samples$phi)
  })  
  
  # Relative Risk
  MapS <- reactive({
    Data4 <- Kasus()
    Data5 <- Datafix()
    Map1 = Map
    Data5$Grup <- ModelLocal()$localised.structure
    Data5$RR   <- round(ModelLocal()$fitted.values/Data4$Exp,2)
    m = ModelLocal()$samples$fitted
    h = matrix(0,nrow=ncol(m),ncol =3 ,byrow = TRUE)
    colnames(h) = c(1,2,3)
    for (d in 1:ncol(m)) {
      h[d,1] = round(mean(m[,d])/Data4$Exp[d],3)
      h[d,2] = round(quantile(m[,d],0.025)/Data4$Exp[d],3)
      h[d,3] = round(quantile(m[,d],0.975)/Data4$Exp[d],3)
    }
    h
    Data5$Cridible_Interval = paste0("(",h[,2],",",h[,3],")")
    Map1@data  <- Data5
    Map1
  }) 
  
  
  
  
  # ===== Output =====
  output$TAB <- renderDT(Datafix())
  
  output$modelx <- renderDT(G())
  
  output$mo <- renderDT(as.data.frame(ModelLocal()$summary.results))
  
  output$mo1 <- renderDT({
    Modelfit <- round(ModelLocal()$modelfit,2)
    t(as.data.frame( Modelfit))
  })
  
  output$plot1 <- renderPlot( plot(tau2.samples()))
  
  output$plot2 <- renderPlot( plot(phi.samples()) )
  
  output$table1 <- renderDT(MapS()@data)
  
  output$petak <- renderLeaflet({
    if (is.null(Data()) | is.null(MapS())) {
      return(NULL)
    }
    Map1 <- MapS()
    
    # Create variableplot
    Map1$variableplot <- as.numeric(
      Map1@data$RR)
    
    # Create leaflet
    pal <- colorBin(colorRamp(c("Blue","White","Red")), domain = Map1$variableplot,
                    bins = 7)
    labels <- sprintf("%s: %g",as.character(as.matrix(Map1@data[input$AreaCass])) , Map1$variableplot) %>%
      lapply(htmltools::HTML)
    
    leaflet(Map1) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~ pal(variableplot),
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        label = labels
      ) %>%
      leaflet::addLegend(
        pal = pal, values = ~variableplot,
        opacity = 0.7, title = NULL
      )
  })
}


# ShinyApp
shinyApp(ui = ui, server = server)



