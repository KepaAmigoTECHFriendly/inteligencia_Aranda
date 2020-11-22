# SHINY APP INTELIGENCIA COMPETITIVA ARANDA DE DUERO

############################################
# LIBRERÍAS
############################################

library(shiny)
library(shinyjs)  # Para eecutar comandos JS
library(DT)
library(htmltools)
library(httr)
library(timeDate)
library(anytime)
library(jsonlite)
library(stringr)
library(dplyr)
library(ggplot2)
library(rjson)
library(jsonlite)
library(leaflet)  #Libreía mapa
library(RCurl)
library(utils)
library(tidyr)
library(RColorBrewer)
library(scales)
library(openxlsx)
library(janitor)
library(reshape2)
library(hrbrthemes)
library(ggthemes)
library(plotly)
library(htmlwidgets)
library(shinyalert)
library(lubridate)
library(shinybusy)

library(magrittr)
library(igraph)
library(xlsx)

library(tm)
library(SnowballC)
library(wordcloud)

library(RPostgres)
library(DBI)

db          <- 'datawarehouse'
host_db     <- '82.223.243.42'
db_port     <- '5432'
db_user     <- 'postgres'
db_password <- 'postgressysadmin_2019'

con <- dbConnect(RPostgres::Postgres(), dbname = db, host=host_db, port=db_port, user=db_user, password=db_password) 

fecha_ref <- as.Date("2020-11-11")

df_establecimientos <- dbGetQuery(con,paste("SELECT * FROM t_establecimientos WHERE fecha_carga = TO_DATE('",fecha_ref,"', 'YYYY-MM-DD')" ,sep = ""))
colnames(df_establecimientos) <- c("Establecimiento","Valoración","Reviews","Categoría","Latitud","Longitud","Dirección","Código","Web","Teléfono","Regentada_mujeres","Horarios","Cierre temporal","URL Google Maps","Fecha")

#==================
# ESTABLECIMIENTOS
#==================

# ESTABLECIMIENTOS
#df_establecimientos <- read.csv("tabla_total.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE, encoding = "UTF-8", dec = ",")
#colnames(df_establecimientos) <- c("Establecimiento","Valoración","Reviews","Categoría","Latitud","Longitud","Dirección","Código","Web","Teléfono","Regentada_mujeres","Horarios","Cierre temporal","URL Google Maps")
#df_establecimientos <- df_establecimientos[grep("aranda de duero",tolower(df_establecimientos$Código)),]

# TOPICS
#df_topics <- read.csv("topics_total.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE, encoding = "UTF-8", dec = ",")
#df_topics <- df_topics[,1:(ncol(df_topics)-1)]
#colnames(df_topics) <- c("Establecimiento","Valoración","Reviews","Categoría","Latitud","Longitud","Código","URL Google Maps","Topics")
#df_topics <- df_topics[grep("aranda de duero",tolower(df_topics$Código)),]
#df_topics <- df_topics %>% 
#  group_by(Establecimiento) %>%
#  mutate(Topics = paste(Topics, collapse = ", "))

#df_establecimientos$Topics <- df_topics$Topics[match(df_establecimientos$Establecimiento, df_topics$Establecimiento)]

#==================
# CENSO EMPRESAS
#==================

# CENSO
df_censo <- read.csv("censo_aranda_duero.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE, encoding = "latin1", dec = ",")
#df_censo$Índice_crec_interanual_ventas <- ifelse(any(grepl("[0-9]",df_censo$Índice_crec_interanual_ventas)),df_censo$Índice_crec_interanual_ventas,"-")

df_censo$Latitud[df_censo$Denominación_social == "Calidad Pascual Sau"] <- 41.677336
df_censo$Longitud[df_censo$Denominación_social == "Calidad Pascual Sau"] <- -3.700250


df_censo_bodegas <- read.csv("censo_bodegas.csv", header = TRUE, stringsAsFactors = FALSE, encoding = "UTF-8", dec = ",")
df_censo_bodegas <- df_censo_bodegas[,-1]

#========================
# TABLA INPUT-OUTPUT CYL
#========================
archivo <- "TIO_ARANDA_2010.xlsx"

#Limpieza
datos_raw <- xlsx::read.xlsx(archivo,5,sep=".",stringsAsFactors = FALSE)
TIO <- datos_raw[,1:(length(datos_raw)-5)]
TIO[is.na(TIO)] <- 0.00

TIO <- TIO[,-c(97,96,92,91,90,89,85,84)]
columnas <- TIO$Sector.al.que.se.compra %>% as.character() %>% t()
colnames(TIO) <- c("Sector",columnas,"CONSUMO hogares","CONSUMO instituciones sin ánimo de lucro", "CONSUMO AAPPs",
                     "EXPORTACIONES resto España", "EXPORTACIONES UE","EXPORTACIONES mundo")
Encoding(TIO$Sector) <- "UTF-8"
Encoding(colnames(TIO)) <- "UTF-8"

for(i in 2:length(TIO))
{
  TIO[,i] <- round(TIO[,i]/sum(TIO[,i])*100,2)
}

TIO[is.na(TIO)] <- 0.00
TIO <- TIO[order(TIO$`CONSUMO hogares`,decreasing = TRUE),]


#====================
# REFERENCIA CNAES
#====================
df_cnae <- read.csv("referencia_CNAEs.csv", header = TRUE, sep = ";")
df_cnae <- df_cnae[,c(1,3)]
df_cnae$completo <- paste(df_cnae[,1],df_cnae[,2], sep = " ")

# MUNICIPIOS
# ------------------------
municipios <- openxlsx::read.xlsx(xlsxFile = "Municipios.xlsx", sheet = 1, skipEmptyRows = TRUE)
municipios <- municipios[3:nrow(municipios),]
colnames(municipios) <- c("Código provincia", "Código municipio", "DC","Municipio","Comarca")
mun <- municipios$Municipio

municipios$M <- municipios$Municipio %>% gsub(", La","",.) %>%
  gsub(", Las","",.) %>%
  gsub(", Los","",.)

incorporar_cero <- function(x)
{
    if(nchar(as.character(x))==4){x <- paste0("0",x)}else{x}
}


# MODIFICACIONES DE AYUDA
# Ayuda selección variables BORME
uno <- c("Constitución comienzo operaciones","Constitución objeto social","Constitución domicilio social","Constitución capital")
dos <- c("Cambio domicilio social","Cambio objeto social")
tres <- c("Nombramiento Adm. Único","Nombramiento vicepresidente","Nombramiento presidente")
cuatro <- c("Ampliación capital","Ampliación capital suscrito","Ampliación capital desembolsado","Ampliación capital resultante desembolsado","Reducción capital importe reducción","Reducción capital resultante suscrito")
cinco <- "Fusión sociedades absorbidas"
seis <- "Disolución"
siete <- "Extinción"
ocho <- "Escisión"
nueve <- "Transformación"
diez <- "Situación Concursal Resoluciones"
cuatro_uno <- c("Ampliación capital","Ampliación capital suscrito","Ampliación capital desembolsado","Ampliación capital resultante desembolsado")
cuatro_dos <- c("Reducción capital importe reducción","Reducción capital resultante suscrito")

lista_variables_borme <- list(uno,dos,tres,cuatro,cinco,seis,siete,ocho,nueve,diez,cuatro_uno,cuatro_dos)
names(lista_variables_borme) <- c("uno","dos","tres","cuatro","cinco","seis","siete","ocho","nueve","diez","cuatro_uno","cuatro_dos")

nombres_variables <- c(1,2,3,4,5,6,7,8,9,10,11,12)
names(nombres_variables) <- c("Constitución","Cambios","Nombramientos","Nada","Fusión","Disolución","Extinción","Escisión","Transformación","Situación concursal","Ampliación de capital","Reducción de capital")

#=====================================================
# INTERFAZ DE USUARIO
#=====================================================
ui <- fluidPage(style = "width: 100%; height: 100%;",
                
                #use_busy_spinner(spin = "fading-circle"),
                
                # Inicialización shinyjs
                useShinyjs(),
                useShinyalert(),
                withMathJax(),
                
                tags$script('
                        var dimension = [0, 0];
                        $(document).on("shiny:connected", function(e) {
                        dimension[0] = window.innerWidth;
                        dimension[1] = window.innerHeight;
                        Shiny.onInputChange("dimension", dimension);
                        });
                        $(window).resize(function(e) {
                        dimension[0] = window.innerWidth;
                        dimension[1] = window.innerHeight;
                        Shiny.onInputChange("dimension", dimension);
                        });
                        '),
                
                #titlePanel(title=div(
                #  a(href="http://www.amb.cat/",
                #    img(src="img/AMB_logo.png",style = 'width: 300px; high: 600px;')
                #    ),
                #  p("INTELIGENCIA COMPETITIVA EMPRESAS",style = "font-size:22px; display: inline-block; margin-left: 15px;  color: #463c32;"))),
                
                #titlePanel(title=div(
                #a(href="https://www.arandadeduero.es",
                #img(src="img/logo_aranda.png",style = 'width: 110px; high: 200px;')
                #)
                #)),
                
                
                navbarPage(id ="menu", "Menú",
                           
                           tabPanel("BORME",
                                    sidebarLayout(
                                      # Menú de datos
                                      sidebarPanel(
                                        
                                        selectInput("Municipio_principal", "Seleccione el territorio de referencia",
                                                    c(as.character(mun), "Burgos provincia"),
                                                    selected = "Aranda de Duero"
                                        ),
                                        selectInput("Municipio_principal_mapa", "Seleccione el territorio de referencia",
                                                    c(as.character(mun), "Comarca Aranda de Duero","Comarca Aranda de Duero sin Aranda de Duero","Burgos provincia","Burgos provincia sin Aranda de Duero"),
                                                    selected = "Aranda de Duero"
                                        ),
                                        radioButtons(inputId="comparaciones",label="Seleccione el territorio de comparación",choices=c("Burgos provincia", "Otro municipio"),
                                                     selected = "Burgos provincia"),
                                        selectInput("Municipio_comparaciones", "Seleccione el municipio de comparación",
                                                    municipios$Municipio),
                                        
                                        dateRangeInput("fechas_listado_borme","Seleccione el intervalo de fechas",start = "2020-01-01", end = "2020-02-01"),
                                        
                                        checkboxGroupInput("variables_borme_listado", label = "Selección variables",
                                                           choices = list("Constitución" = 1,
                                                                          "Cambios" = 2,
                                                                          "Nombramientos" = 3,
                                                                          "Ampliación de capital" = 11,
                                                                          "Reducción de capital" = 12,
                                                                          "Fusión" = 5,
                                                                          "Disolución" = 6,
                                                                          "Extinción" = 7,
                                                                          "Escisión" = 8,
                                                                          "Transformación" = 9,
                                                                          "Situación concursal" = 10),
                                                           selected = 1
                                        ),
                                        
                                        radioButtons("variables_mapa", "Selección variables",
                                                     c("Constitución" = 1,
                                                       "Cambio de domicilio social" = 2)),
                                        
                                        # Búsqueda por objeto social
                                        #textInput("palabra_clave_listado_borme", "Búsqueda por objeto social"),
                                        
                                        # Boton descarga borme
                                        downloadButton("descarga_borme_csv", "Descargar datos en .csv"),
                                        br(),
                                        downloadButton("descarga_borme_xlsx", "Descargar datos en .xlsx"),
                                        width=3
                                      ),
                                      
                                      mainPanel(
                                        tabsetPanel(id = "tabs_borme",
                                                    
                                                    tabPanel(id = "tab_listado",
                                                             "Listado informativo",   #Con span genero un popup de ayuda.
                                                             
                                                             # Panel DATAFRAME
                                                             fluidRow(style='padding-top: 18px;',
                                                                      uiOutput("texto_tabla_borme_listado"),
                                                                      dataTableOutput("tabla_borme_listado"),
                                                             ),
                                                             fluidRow(style='padding-top: 18px;',
                                                                      uiOutput("texto_tabla_borme_censo"),
                                                                      dataTableOutput("tabla_borme_censo"),
                                                             )
                                                    ),
                                                    
                                                    tabPanel(id = "tab_estadistica_basica",
                                                             "Estadística básica 1",   #Con span genero un popup de ayuda.
                                                             
                                                             # Panel DATAFRAME
                                                             fluidRow(style='padding-top: 18px;',
                                                                      uiOutput("texto_tabla_borme_eb1"),
                                                                      dataTableOutput("tabla_borme_eb1"),
                                                             ),
                                                             fluidRow(
                                                               column(width = 9,
                                                                      plotlyOutput("queso_borme_agregado_eb1", height = 400)
                                                               )
                                                               
                                                             ),
                                                             fluidRow(
                                                               column(width = 9,
                                                                      plotlyOutput("queso_borme_desagregado_eb1", height = 400)
                                                               )
                                                               
                                                             ),
                                                             fluidRow(
                                                               column(width = 9,
                                                                      plotlyOutput("queso_borme_desagregado_forma_eb1", height = 800)
                                                               )
                                                               
                                                             ),
                                                    ),
                                                    
                                                    tabPanel(id = "tab_estadistica_basica_2", 
                                                             "Estadística básica 2",   #Con span genero un popup de ayuda.
                                                             
                                                             # Panel DATAFRAME
                                                             fluidRow(style='padding-top: 18px;',
                                                                      uiOutput("texto_tabla_borme_eb2"),
                                                                      dataTableOutput("tabla_borme_eb2"),
                                                             ),
                                                             fluidRow(
                                                               column(width = 12,
                                                                      plotlyOutput("lineas_borme_eb2", height = 500)
                                                               )
                                                             ),
                                                             br(),
                                                             fluidRow(
                                                               column(width = 12,
                                                                      plotlyOutput("lineas_borme_anual_anterior_eb2", height = 500)
                                                               )
                                                             )
                                                    ),
                                                    
                                                    tabPanel(id = "tab_capital",
                                                             "Modificaciones capital",
                                                             
                                                             # Panel DATAFRAME
                                                             fluidRow(style='padding-bottom: 25px;',
                                                                      uiOutput("texto_tabla_borme_capital"),
                                                                      dataTableOutput("tabla_borme_capital"),
                                                             ),
                                                             fluidRow(
                                                               column(width = 9,
                                                                      plotlyOutput("lineas_borme_capital_ampliaciones", height = 400),
                                                                      br()
                                                               )
                                                               
                                                             ),
                                                             fluidRow(
                                                               column(width = 9,
                                                                      plotlyOutput("lineas_borme_capital_reducciones", height = 400)
                                                               )
                                                             ),
                                                             
                                                    ),
                                                    
                                                    tabPanel(id = "tab_mapa",
                                                             "Mapa",
                                                             # Panel MAPA
                                                             fluidRow(style='padding-bottom: 25px;',
                                                                      uiOutput("texto_borme_mapa"),
                                                                      leafletOutput("mapa_borme"),
                                                             ),
                                                             
                                                             # Panel DATAFRAME
                                                             fluidRow(
                                                               uiOutput("texto_tabla_borme_mapa"),
                                                               dataTableOutput("tabla_borme_mapa"),
                                                             ),
                                                             fluidRow(style='padding-top: 18px;',
                                                                      uiOutput("texto_tabla_borme_censo_mapa"),
                                                                      dataTableOutput("tabla_borme_censo_mapa"),
                                                             )
                                                    )
                                        )
                                        
                                      )
                                    )
                           ), # Cierre panel Borme
                           tabPanel("Censo de empresas",
                                    sidebarLayout(
                                      sidebarPanel(
                                        dateRangeInput("fechas","Filtro por intervalo de fechas",start = "1960-01-01", end = Sys.Date()),
                                        #numericInput("lat_max", "Latitud máxima", 39.5, min = 1, max = 100),
                                        #br(),
                                        #numericInput("lat_min","Latitud mínima",39, min = 1, max = 100),
                                        #br(),
                                        #numericInput("long_max","Longitud máxima",-0.2, min = 1, max = 100),
                                        #br(),
                                        #numericInput("long_min","Longitud mínima",-0.7, min = 1, max = 100),
                                        #br(),
                                        textInput("palabra_clave", "Búsqueda por palabra clave"),
                                        selectInput("calle", "Filtro por ubicación",
                                                    c("Todas",gsub(",.*","",unique(df_censo$`Domicilio_social`))[order(gsub(",.*","",unique(df_censo$`Domicilio_social`)))])),
                                        selectInput("municipio_bodegas", "Filtro por municipio",
                                                    c("Todos",unique(as.character(df_censo_bodegas$Municipio))[order(unique(as.character(df_censo_bodegas$Municipio)))]),
                                                    multiple = TRUE, selected = "Todos"),
                                        sliderInput("empleados", "Filtro por rango de empleados",0,max(na.omit(as.numeric(unique(gsub("[ (].*","",df_censo$Empleados))))),c(0,100),step = 1
                                        ),
                                        selectInput("div_cnae", "Filtro por CNAE",
                                                    #c("Todos",substring(unique(df_censo$CNAE),1,2)[order(substring(unique(df_censo$CNAE),1,2))][-1])
                                                    c("Todos",df_cnae$completo), multiple = TRUE, selected = "Todos"
                                        ),
                                        #div(style = "color: black; font-size:14px; font-weight: bold;","Filtro redes sociales"),
                                        radioButtons("RRSS", "Filtro por redes sociales",
                                                     choices = list("Con RRSS" = 1, "Sin RRSS" = 2,
                                                                    "Todas" = 3), selected = 3),
                                        div(style = "color: black; font-size:14px; font-weight: bold;","Filtro empresas extintas"),
                                        checkboxInput("extinguidas", "Ocultar empresas extintas", value = TRUE, width = NULL),
                                        downloadButton("downloadDatacenso_csv", "Descargar CSV"),
                                        width = 3,
                                      ),
                                      
                                      mainPanel(
                                        tabsetPanel(id = "tabs_censo",
                                                    tabPanel(id = "censo_aranda","Aranda de Duero",
                                                             fluidRow(
                                                               leafletOutput("mapa", height = 500)
                                                             ),
                                                             fluidRow(
                                                               dataTableOutput("tabla")
                                                             )
                                                    ),
                                                    
                                                    tabPanel(id = "censo_bodegas","Bodegas comarca",
                                                             fluidRow(
                                                               leafletOutput("mapa_bodegas", height = 500)
                                                             ),
                                                             fluidRow(
                                                               dataTableOutput("tabla_bodegas")
                                                             )
                                                    )
                                        )
                                      )
                                    )
                           ), #Cierre panel censo
                           
                           tabPanel("Establecimientos",
                                    sidebarLayout(
                                      sidebarPanel(
                                        dateRangeInput("fechas_establecimientos","Seleccione el intervalo de fechas",start = Sys.Date()-30, end = Sys.Date()),
                                        selectInput("categoria", "Filtro por categoría",
                                                    #c("Todos",substring(unique(df_censo$CNAE),1,2)[order(substring(unique(df_censo$CNAE),1,2))][-1])
                                                    c("Todos",unique(df_establecimientos$Categoría)[order(unique(df_establecimientos$Categoría))]),
                                                    multiple = TRUE, selected = "Todos"
                                        ),
                                        sliderInput("reviews", "Filtro por número de reseñas",min(as.numeric(unique(df_establecimientos$Reviews))),max(as.numeric(unique(df_establecimientos$Reviews))),c(5,100),step = 10
                                        ),
                                        sliderInput("valoracion", "Filtro por valoración",min(as.numeric(unique(df_establecimientos$Valoración))),max(as.numeric(unique(df_establecimientos$Valoración))),c(0,5),step = 0.5
                                        ),
                                        #div(style = "color: black; font-size:14px; font-weight: bold;","Filtro redes sociales"),
                                        radioButtons("web", "Filtro por web",
                                                     choices = list("Con web" = 1, "Sin web" = 2, "Todos" = 3), selected = 3
                                        ),
                                        radioButtons("cerradas", "Filtro por estado",
                                                     choices = list("Abierto" = 1, "Cerrado temporalmente" = 2, "Todos" = 3), selected = 3
                                        ),
                                        downloadButton("downloadDataestablecimientos_csv", "Descargar CSV"),
                                        width = 3,
                                      ),
                                      
                                      mainPanel(
                                        tabsetPanel(id = "tabs_establecimientos",
                                                    
                                                    tabPanel(id = "Estado_actual",
                                                             "Estado actual",   

                                                             fluidRow(style='padding-top: 18px;',
                                                                      leafletOutput("mapa_establecimientos", height = 500),
                                                             ),
                                                             fluidRow(style='padding-top: 18px;',
                                                                      dataTableOutput("tabla_establecimientos"),
                                                             )
                                                    ),
                                                    
                                                    tabPanel(id = "evolucion_temporal",
                                                             "Evolución temporal",   
                                                             fluidRow(style='padding-top: 18px;',
                                                               plotlyOutput("evolucion_categorias", height = 400),
                                                             ),
                                                             fluidRow(style='padding-top: 18px;',
                                                               plotlyOutput("evolucion_cerrados", height = 400),
                                                             ),
                                                             fluidRow(
                                                               plotOutput("nube_topics", height = 800),
                                                             )
                                                    )
                                        )
                                      )
                                    )
    
                           ), #Cierre panel establecimientos
                           
                           tabPanel("Tabla input-output Castilla y León",
                                    sidebarLayout(
                                      sidebarPanel(
                                        selectInput("sector", "Seleccione un sector", TIO$Sector[order(TIO$Sector)], selected = TIO$Sector[1]),
                                        sliderInput("grupos", "Grupos",1,10,4,step = 1),
                                        checkboxInput("inducido", "Inducido", value = FALSE),
                                        width = 2,
                                      ),
                                      
                                      mainPanel(
                                        plotOutput("graf_tio",height = 1400)
                                      )
                                    )
                           ), #Cierre tabla I-O
                           
                           tags$style(type = 'text/css',
                                      '.dataTables_scrollBody {transform:rotateX(180deg);}',
                                      '.dataTables_scrollBody table {transform:rotateX(180deg);}'
                           )
                           
                ) # Cierre navbarPage
) # Cierre UI

######################################################
# LÓGICA DE SERVIDOR
######################################################

server <- function(input, output, session) {
    
    datos <- reactiveValues(borme=NULL,borme_anterior=NULL)
    datos_estblecimientos_actual <- reactiveValues(establecimientos=NULL)
    datos_estblecimientos_temporal <- reactiveValues(establecimientos=NULL,flag=0)

    ###############################################
    # INICIALIZACIÓN LÓGICA DE VISUALIZACIÓN OBJETOS SHINY

    # Lógica selección territorio de comparación
    observeEvent(input$comparaciones, {
        if(input$comparaciones == "Otro municipio"){
            shinyjs::show("Municipio_comparaciones")
        }else{
            shinyjs::hide("Municipio_comparaciones")
        }
    })
    
    #Lógica visualización selección variables en mapa
    observeEvent(input$tabs_borme, {
      if(input$tabs_borme == "Mapa"){
        shinyjs::hide("Municipio_principal")
        shinyjs::hide("comparaciones")
        shinyjs::hide("variables_borme_listado")
        shinyjs::show("variables_mapa")
        shinyjs::show("Municipio_principal_mapa")
      }else if(input$tabs_borme == "Modificaciones capital"){
        shinyjs::hide("Municipio_principal_mapa")
        shinyjs::hide("comparaciones")
        shinyjs::hide("variables_borme_listado")
        shinyjs::hide("variables_mapa")
        shinyjs::show("Municipio_principal")
      }else if(input$tabs_borme == "Listado informativo"){
        shinyjs::hide("Municipio_principal_mapa")
        shinyjs::hide("comparaciones")
        shinyjs::show("variables_borme_listado")
        shinyjs::hide("variables_mapa")
        shinyjs::show("Municipio_principal")
      }else if(input$tabs_borme == "Estadística básica 2"){
        shinyjs::hide("Municipio_principal_mapa")
        shinyjs::show("comparaciones")
        shinyjs::show("variables_borme_listado")
        shinyjs::hide("variables_mapa")
        shinyjs::show("Municipio_principal")
      }else{
        shinyjs::hide("Municipio_principal_mapa")
        shinyjs::show("comparaciones")
        shinyjs::show("variables_borme_listado")
        shinyjs::hide("variables_mapa")
        shinyjs::show("Municipio_principal")
      }
    })
    
    #Lógica visualización selección variables en establecimientos
    observeEvent(input$tabs_establecimientos, {
      if(input$tabs_establecimientos == "Evolución temporal"){
        shinyjs::show("fechas_establecimientos")
      }else{
        shinyjs::hide("fechas_establecimientos")
      }
    })
    
    #Lógica visualización selección filtros en censo bodegas
    observeEvent(input$tabs_censo, {
      if(input$tabs_censo == "Bodegas comarca"){
        shinyjs::hide("div_cnae")
        shinyjs::hide("calle")
        shinyjs::show("municipio_bodegas")
      }else{
        shinyjs::show("div_cnae")
        shinyjs::show("calle")
        shinyjs::hide("municipio_bodegas")
      }
    })
    
    # Lógica obsrvación gráfico queso_borme_desagregado_forma_eb1 
    observeEvent(input$variables_borme_listado, {
      if(length(input$variables_borme_listado) > 1){
        shinyjs::show("queso_borme_desagregado_forma_eb1")
      }else{
        shinyjs::hide("queso_borme_desagregado_forma_eb1")
      }
    })
    
    # Lógica visualización gráfica de lineas año a año
    observeEvent(input$fechas_listado_borme, {
      fecha_inicial <- input$fechas_listado_borme[1]
      fecha_final <- input$fechas_listado_borme[2]
      
      años <- year(seq(fecha_inicial,fecha_final,"years"))
      
      # Si hay más de un año no se prepara la gráfica
      if(length(años) > 2){
        shinyjs::hide("lineas_borme_anual_anterior_eb2")
      }else{
        shinyjs::show("lineas_borme_anual_anterior_eb2")
      }
      
    })
    
    observeEvent(input$fechas_listado_borme, {
      print("Entro fechas")
      #show_spinner() # show the spinner
      #show_modal_spinner() # show the modal window
      
      fecha_inicial <- input$fechas_listado_borme[1]
      fecha_final <- input$fechas_listado_borme[2]
      
      if(fecha_final - fecha_inicial < 32){
        progress <- Progress$new(session)
        progress$set(value = 0.5, message = 'Cargando datos...')
        #datos$borme = llamada_api(as.character(input$fechas_listado_borme[1]), as.character(input$fechas_listado_borme[2]))

        fecha_1 <- format(as.Date(fecha_inicial),"%d/%m/%Y")
        fecha_2 <- format(as.Date(fecha_final),"%d/%m/%Y")
        
        datos$borme <- dbGetQuery(con,paste("SELECT * FROM borme WHERE TO_DATE(fecha, 'DD/MM/YYYY') >= TO_DATE('",fecha_1,"', 'DD/MM/YYYY') AND TO_DATE(fecha, 'DD/MM/YYYY') <= TO_DATE('",fecha_2,"', 'DD/MM/YYYY')" ,sep = ""))
        
        progress$close()
      }else{
        num_meses <- floor(12*as.double(difftime(fecha_final,fecha_inicial))/365)
        num_meses <- ifelse(num_meses == 0,1,num_meses)
        
        #datos$borme = llamada_api(as.character(input$fechas_listado_borme[1]), as.character(input$fechas_listado_borme[2]))
        fecha_ini_consulta <- fecha_inicial
        fecha_fin_consulta <- fecha_final
        progress <- Progress$new(session)
        long <- 1:num_meses
        avance_barra <- rescale(long,c(0.2,1.0))
        df <- data.frame(NULL)
        for(i in long){
          progress$set(value = avance_barra[i], message = 'Cargando datos...')
          if(i == 1){
            month(fecha_ini_consulta) <- month(fecha_fin_consulta)-1
          }else if(i == num_meses){
            fecha_fin_consulta <- fecha_ini_consulta-1
            fecha_ini_consulta <- fecha_inicial
            
          }else{
            fecha_fin_consulta <- fecha_ini_consulta-1
            month(fecha_ini_consulta) <- month(fecha_ini_consulta)-1
          }

          #df_mes <- llamada_api(as.character(fecha_ini_consulta), as.character(fecha_fin_consulta))
          fecha_inicial_2 <- as.Date(fecha_ini_consulta)
          fecha_final_2 <- as.Date(fecha_fin_consulta)
          fecha_1 <- format(as.Date(fecha_inicial_2),"%d/%m/%Y")
          fecha_2 <- format(as.Date(fecha_final_2),"%d/%m/%Y")
          
          df_mes <- dbGetQuery(con,paste("SELECT * FROM borme WHERE TO_DATE(fecha, 'DD/MM/YYYY') >= TO_DATE('",fecha_1,"', 'DD/MM/YYYY') AND TO_DATE(fecha, 'DD/MM/YYYY') <= TO_DATE('",fecha_2,"', 'DD/MM/YYYY')" ,sep = ""))
          
          df <- rbind(df,df_mes)
        }
        datos$borme = df
        progress$close()
      }
      
      #remove_modal_spinner() # remove it when done
      #hide_spinner() # hide the spinner
      print("LLEGO 1")
    })
    
    # CARGA DE DATOS AÑO ANTERIOR PARA GRÁFICA LINEAL
    observeEvent(input$tabs_borme, {
      
      if(input$tabs_borme == "Estadística básica 2"){
        # Consulta a BBDD
        fecha_inicial <- input$fechas_listado_borme[1]
        fecha_final <- input$fechas_listado_borme[2]
        
        años <- year(seq(fecha_inicial,fecha_final,"years"))
        
        # Si hay más de un año no se prepara la gráfica
        if(length(años) > 2){
          return(0)
        }
        
        # Restamos un año al periodo de consultas actual
        year(fecha_inicial) <- year(fecha_inicial) - 1
        year(fecha_final) <- year(fecha_final) - 1
        
        if(fecha_final - fecha_inicial < 32){
          progress <- Progress$new(session)
          progress$set(value = 0.5, message = 'Cargando datos...')
          #datos$borme = llamada_api(as.character(input$fechas_listado_borme[1]), as.character(input$fechas_listado_borme[2]))
          
          fecha_1 <- format(as.Date(fecha_inicial),"%d/%m/%Y")
          fecha_2 <- format(as.Date(fecha_final),"%d/%m/%Y")
          
          datos$borme <- dbGetQuery(con,paste("SELECT * FROM borme WHERE TO_DATE(fecha, 'DD/MM/YYYY') >= TO_DATE('",fecha_1,"', 'DD/MM/YYYY') AND TO_DATE(fecha, 'DD/MM/YYYY') <= TO_DATE('",fecha_2,"', 'DD/MM/YYYY')" ,sep = ""))
          
          progress$close()
        }else{
          num_meses <- floor(12*as.double(difftime(fecha_final,fecha_inicial))/365)
          num_meses <- ifelse(num_meses == 0,1,num_meses)
          
          #datos$borme = llamada_api(as.character(input$fechas_listado_borme[1]), as.character(input$fechas_listado_borme[2]))
          fecha_ini_consulta <- fecha_inicial
          fecha_fin_consulta <- fecha_final
          progress <- Progress$new(session)
          long <- 1:num_meses
          avance_barra <- rescale(long,c(0.2,1.0))
          df <- data.frame(NULL)
          for(i in long){
            progress$set(value = avance_barra[i], message = 'Cargando datos...')
            if(i == 1){
              month(fecha_ini_consulta) <- month(fecha_fin_consulta)-1
            }else if(i == num_meses){
              fecha_fin_consulta <- fecha_ini_consulta-1
              fecha_ini_consulta <- fecha_inicial
              
            }else{
              fecha_fin_consulta <- fecha_ini_consulta-1
              month(fecha_ini_consulta) <- month(fecha_ini_consulta)-1
            }
            
            #df_mes <- llamada_api(as.character(fecha_ini_consulta), as.character(fecha_fin_consulta))
            fecha_inicial_2 <- as.Date(fecha_ini_consulta)
            fecha_final_2 <- as.Date(fecha_fin_consulta)
            fecha_1 <- format(as.Date(fecha_inicial_2),"%d/%m/%Y")
            fecha_2 <- format(as.Date(fecha_final_2),"%d/%m/%Y")
            
            df_mes <- dbGetQuery(con,paste("SELECT * FROM borme WHERE TO_DATE(fecha, 'DD/MM/YYYY') >= TO_DATE('",fecha_1,"', 'DD/MM/YYYY') AND TO_DATE(fecha, 'DD/MM/YYYY') <= TO_DATE('",fecha_2,"', 'DD/MM/YYYY')" ,sep = ""))
            
            df <- rbind(df,df_mes)
          }
          progress$close()
        }
        
        #=========================
        # 1) DATOS ESTRUCTURADOS
        #=========================
        datos_borme <- df
        nombres <- c("Denominación social","Fusión sociedades absorbidas", "Modificaciones estatutarias",
                     "Cambio denominación social", "Cambio domicilio social", "Cambio objeto social",
                     "Ceses liquiSoli", "Ceses apoderado", "Ceses Adm. Único",
                     "Ceses liquidador", "Ceses liquidador mancomunado", "Ceses adminSolid",
                     "Ceses Adm. Mancomunado", "Ceses Soc. Prof", "Ceses depositorio",
                     "Ceses entid. Deposit.", "Ceses entid. Promo.", "Ceses consejero",
                     "Ceses vicepresidente", "Ceses presidente", "Ceses secretario",
                     "Nombramiento liquiSoli", "Nombramiento apoderado", "Nombramiento Adm. Único",
                     "Nombramiento liquidador", "Nombramiento liquidador mancomunado", "Nombramiento Adm. Solid",
                     "Nombramiento Soc. Prof", "Nombramiento auditor","Nombramiento Adm. Mancomunado",
                     "Nombramiento Entid. Deposit.", "Nombramiento Entid. Promo.", "Nombramiento consejero",
                     "Nombramiento vicepresidente","Nombramiento presidente", "Nombramiento secretario",
                     "Ampliación capital suscrito", "Ampliación capital resultante suscrito", "Ampliación capital desembolsado",
                     "Ampliación capital resultante desembolsado", "Ampliación capital", "Declaración unipersonalidad socio único",
                     "Reducción capital importe reducción","Reducción capital resultante suscrito", "Reelecciones Adm. Único",
                     "Reelecciones auditor", "Reelecciones auditor suplente", "Revocaciones auditor",
                     "Revocaciones apoderado", "Revocaciones apoderado mancomunado", "Revocaciones apoderadoSol",
                     "Situación Concursal Procedimiento", "Situación Concursal Resolución firme","Situación Concursal Fecha Resolución",
                     "Situación Concursal Proceso", "Situación Concursal Juzgado", "Situación Concursal Juez",
                     "Situación Concursal Resoluciones", "Escisión", "Transformación", "Disolución", "Extinción",
                     "Constitución comienzo operaciones", "Constitución objeto social","Constitución domicilio social",
                     "Constitución capital", "Otros conceptos","Datos registrales",
                     "Coordenadas empresa","Latitud", "Longitud","Municipio",
                     "Distancia respecto municipio en km","Dentro", "Provincia","Fecha"
        )
        
        
        if(nrow(datos_borme) == 0){
          return(0)
        }
        
        colnames(datos_borme) <- nombres
        datos_borme <- datos_borme[,c(1,2,5,6,24,34,35,59,60,61,62,63,64,65,66,41,37,39,40,43,44,58,70,71,72,76)]
        
        progress <- Progress$new(session)
        long <- 1:2
        avance_barra <- rescale(long,c(0.5,1.0))
        progress$set(value = 0.5, message = 'Procesando datos...')
        
        # ASIGNACIÓN DE MUNICIPIO ARANDA DE DUERO
        for(i in 1:nrow(datos_borme)){
          datos_borme$Municipio[i] <- ifelse(datos_borme$Municipio[i] == "-" & any(grepl(tolower(datos_borme$`Denominación social`)[i], tolower(gsub("[.]","",df_censo$Denominación_social)))), 
                                             "Aranda de Duero",
                                             datos_borme$Municipio[i])
        }
        progress$set(value = 1, message = 'Procesando datos...')
        progress$close()
        
        #Generación forma jurídica
        forma_juridica <- c()
        for(i in 1:length(datos_borme$`Denominación social`)){
          pos_ultimo_espacio <- gregexpr(" ",datos_borme$`Denominación social`[i])[[1]][length(gregexpr(" ",datos_borme$`Denominación social`[i])[[1]])]
          forma_juridica1 <- str_trim(substring(datos_borme$`Denominación social`[i],pos_ultimo_espacio,nchar(datos_borme$`Denominación social`[i])))
          if(nchar(forma_juridica1) > 3){
            nuevo_nombre <- gsub(" EN LIQUIDACION","",datos_borme$`Denominación social`[i])
            pos_ultimo_espacio <- gregexpr(" ",nuevo_nombre)[[1]][length(gregexpr(" ",nuevo_nombre)[[1]])]
            forma_juridica1 <- str_trim(substring(nuevo_nombre,pos_ultimo_espacio,nchar(nuevo_nombre)))
            
            if(nchar(forma_juridica1) > 3){
              forma_juridica1 <- "Otras"
            }
          }
          forma_juridica <- c(forma_juridica, forma_juridica1)
        }
        
        datos_borme$`Forma Jurídica` <- gsub("\\.","",forma_juridica)
        
        # Fechas
        datos_borme$Fecha <- as.character(datos_borme$Fecha)
        
        # Capital social de consituticion a num para orden en tabla.
        datos_borme$`Constitución capital`[datos_borme$`Constitución capital` != "-"] <- str_match(datos_borme$`Constitución capital`[datos_borme$`Constitución capital` != "-"], " \\s*(.*?)\\s* euros")[,2]
        datos_borme$`Constitución capital`[!grepl("[a-d]",datos_borme$`Constitución capital`) & datos_borme$`Constitución capital` != "-"] <- gsub("[,].*","",unlist(gsub("[ ].*","", str_extract_all(datos_borme$`Constitución capital`[!grepl("[a-d]",datos_borme$`Constitución capital`) & datos_borme$`Constitución capital` != "-"],"\\(?[0-9,.]+\\)?"))))
        datos_borme$`Constitución capital`[!grepl("[a-d]",datos_borme$`Constitución capital`) & datos_borme$`Constitución capital` != "-"] <- as.numeric(gsub("[.]","",datos_borme$`Constitución capital`[!grepl("[a-d]",datos_borme$`Constitución capital`) & datos_borme$`Constitución capital` != "-"]))
        datos_borme$`Constitución capital`[!grepl("[a-d]",datos_borme$`Constitución capital`) & datos_borme$`Constitución capital` != "-"] <- format(as.numeric(datos_borme$`Constitución capital`[!grepl("[a-d]",datos_borme$`Constitución capital`) & datos_borme$`Constitución capital` != "-"]), big.mark = ".")
        
        datos$borme_anterior = datos_borme
      }
    })
    
    # LLAMADA BBDD ESTABLECIMIENTOS
    # 1) Llamada para estado actual
    observeEvent(input$menu, {
      if(input$menu == "Establecimientos"){
        fecha_inicial <- input$fechas_establecimientos[1]
        fecha_final <- input$fechas_establecimientos[2]
        
        fechas <- dbGetQuery(con, "SELECT DISTINCT fecha_carga FROM t_establecimientos")
        fechas <- fechas[,1]
        fecha <- as.Date(fechas[length(fechas)])
        
        progress <- Progress$new(session)
        progress$set(value = 0.4, message = 'Cargando datos...')
        
        df_establecimientos <- dbGetQuery(con,paste("SELECT * FROM t_establecimientos WHERE fecha_carga >= TO_DATE('",fecha_inicial,"', 'YYYY-MM-DD') AND fecha_carga <= TO_DATE('",fecha_final,"', 'YYYY-MM-DD')" ,sep = ""))
        progress$set(value = 0.6, message = 'Cargando datos...')
        df_topics <- dbGetQuery(con,paste("SELECT * FROM t_topics WHERE fecha_carga >= TO_DATE('",fecha_inicial,"', 'YYYY-MM-DD') AND fecha_carga <= TO_DATE('",fecha_final,"', 'YYYY-MM-DD')" ,sep = ""))
        
        colnames(df_establecimientos) <- c("Establecimiento","Valoración","Reviews","Categoría","Latitud","Longitud","Dirección","Código","Web","Teléfono","Regentada_mujeres","Horarios","Cierre temporal","URL Google Maps","Fecha")
        #df_establecimientos <- df_establecimientos[grep("aranda de duero",tolower(df_establecimientos$Código)),]
        df_topics <- df_topics[,c(1,2,3,4,5,6,7,8,9,11)]
        colnames(df_topics) <- c("Establecimiento","Valoración","Reviews","Categoría","Latitud","Longitud","Código","URL Google Maps","Topics","Fecha")
        #df_topics <- df_topics[grep("aranda de duero",tolower(df_topics$Código)),]
        df_topics <- df_topics %>% 
          group_by(Establecimiento) %>%
          mutate(Topics = paste(Topics, collapse = ", "))
        
        progress$set(value = 0.8, message = 'Cargando datos...')
        
        df_establecimientos$Topics <- df_topics$Topics[match(df_establecimientos$Establecimiento, df_topics$Establecimiento)]
        
        progress$set(value = 1, message = 'Cargando datos...')
        progress$close()
        
        datos_estblecimientos_actual$establecimientos = df_establecimientos
        
      }
    })
    
    # 2) Llamada para estado temporal
    observeEvent(input$fechas_establecimientos, {
      if(input$tabs_establecimientos == "Evolución temporal"){
        fecha_inicial <- input$fechas_establecimientos[1]
        fecha_final <- input$fechas_establecimientos[2]
        
        fechas <- dbGetQuery(con, "SELECT DISTINCT fecha_carga FROM t_establecimientos")
        fechas <- fechas[,1]
        fechas <- fechas[fechas >= fecha_inicial & fechas <= fecha_final]
        long_fechas <- length(fechas)
        
        progress <- Progress$new(session)
        progress$set(value = 0.3, message = 'Cargando datos...')
        
        df_establecimientos <- dbGetQuery(con,paste("SELECT * FROM t_establecimientos WHERE fecha_carga >= TO_DATE('",fecha_inicial,"', 'YYYY-MM-DD') AND fecha_carga <= TO_DATE('",fecha_final,"', 'YYYY-MM-DD')" ,sep = ""))
        df_topics <- dbGetQuery(con,paste("SELECT * FROM t_topics WHERE fecha_carga >= TO_DATE('",fecha_inicial,"', 'YYYY-MM-DD') AND fecha_carga <= TO_DATE('",fecha_final,"', 'YYYY-MM-DD')" ,sep = ""))

        progress$set(value = 0.6, message = 'Cargando datos...')
        
        colnames(df_establecimientos) <- c("Establecimiento","Valoración","Reviews","Categoría","Latitud","Longitud","Dirección","Código","Web","Teléfono","Regentada_mujeres","Horarios","Cierre temporal","URL Google Maps","Fecha")
        #df_establecimientos <- df_establecimientos[grep("aranda de duero",tolower(df_establecimientos$Código)),]
        df_topics <- df_topics[,c(1,2,3,4,5,6,7,8,9,11)]
        colnames(df_topics) <- c("Establecimiento","Valoración","Reviews","Categoría","Latitud","Longitud","Código","URL Google Maps","Topics","Fecha")
        #df_topics <- df_topics[grep("aranda de duero",tolower(df_topics$Código)),]
        df_topics <- df_topics %>% 
          group_by(Establecimiento) %>%
          mutate(Topics = paste(Topics, collapse = ", "))
        
        progress$set(value = 0.8, message = 'Cargando datos...')
        
        df_establecimientos$Topics <- df_topics$Topics[match(df_establecimientos$Establecimiento, df_topics$Establecimiento)]
        
        progress$set(value = 1, message = 'Cargando datos...')
        progress$close()
        
        datos_estblecimientos_temporal$establecimientos = df_establecimientos
        datos_estblecimientos_temporal$flag=1
        
      }
    })
    
    
    
    
    

    #==========================================================================
    # DATOS REACTIVOS
    #==========================================================================


    # Datos estructurados BORME
    datos_estructurados_borme <- reactive({
        
        datos_borme <- datos$borme
        
        nombres <- c("Denominación social","Fusión sociedades absorbidas", "Modificaciones estatutarias",
                     "Cambio denominación social", "Cambio domicilio social", "Cambio objeto social",
                     "Ceses liquiSoli", "Ceses apoderado", "Ceses Adm. Único",
                     "Ceses liquidador", "Ceses liquidador mancomunado", "Ceses adminSolid",
                     "Ceses Adm. Mancomunado", "Ceses Soc. Prof", "Ceses depositorio",
                     "Ceses entid. Deposit.", "Ceses entid. Promo.", "Ceses consejero",
                     "Ceses vicepresidente", "Ceses presidente", "Ceses secretario",
                     "Nombramiento liquiSoli", "Nombramiento apoderado", "Nombramiento Adm. Único",
                     "Nombramiento liquidador", "Nombramiento liquidador mancomunado", "Nombramiento Adm. Solid",
                     "Nombramiento Soc. Prof", "Nombramiento auditor","Nombramiento Adm. Mancomunado",
                     "Nombramiento Entid. Deposit.", "Nombramiento Entid. Promo.", "Nombramiento consejero",
                     "Nombramiento vicepresidente","Nombramiento presidente", "Nombramiento secretario",
                     "Ampliación capital suscrito", "Ampliación capital resultante suscrito", "Ampliación capital desembolsado",
                     "Ampliación capital resultante desembolsado", "Ampliación capital", "Declaración unipersonalidad socio único",
                     "Reducción capital importe reducción","Reducción capital resultante suscrito", "Reelecciones Adm. Único",
                     "Reelecciones auditor", "Reelecciones auditor suplente", "Revocaciones auditor",
                     "Revocaciones apoderado", "Revocaciones apoderado mancomunado", "Revocaciones apoderadoSol",
                     "Situación Concursal Procedimiento", "Situación Concursal Resolución firme","Situación Concursal Fecha Resolución",
                     "Situación Concursal Proceso", "Situación Concursal Juzgado", "Situación Concursal Juez",
                     "Situación Concursal Resoluciones", "Escisión", "Transformación", "Disolución", "Extinción",
                     "Constitución comienzo operaciones", "Constitución objeto social","Constitución domicilio social",
                     "Constitución capital", "Otros conceptos","Datos registrales",
                     "Coordenadas empresa","Latitud", "Longitud","Municipio",
                     "Distancia respecto municipio en km","Dentro", "Provincia","Fecha"
        )

        if(nrow(datos_borme) == 0){
          return(0)
        }

        colnames(datos_borme) <- nombres
        datos_borme <- datos_borme[,c(1,2,5,6,24,34,35,59,60,61,62,63,64,65,66,41,37,39,40,43,44,58,70,71,72,76)]
        
        progress <- Progress$new(session)
        long <- 1:2
        avance_barra <- rescale(long,c(0.5,1.0))
        progress$set(value = 0.5, message = 'Procesando datos...')
        
        # ASIGNACIÓN DE MUNICIPIO ARANDA DE DUERO
        for(i in 1:nrow(datos_borme)){
          datos_borme$Municipio[i] <- ifelse(datos_borme$Municipio[i] == "-" & any(grepl(tolower(datos_borme$`Denominación social`)[i], tolower(gsub("[.]","",df_censo$Denominación_social)))), 
                                             "Aranda de Duero",
                                             datos_borme$Municipio[i])
        }
        progress$set(value = 1, message = 'Procesando datos...')
        progress$close()

        #Generación forma jurídica
        forma_juridica <- c()
        for(i in 1:length(datos_borme$`Denominación social`)){
            pos_ultimo_espacio <- gregexpr(" ",datos_borme$`Denominación social`[i])[[1]][length(gregexpr(" ",datos_borme$`Denominación social`[i])[[1]])]
            forma_juridica1 <- str_trim(substring(datos_borme$`Denominación social`[i],pos_ultimo_espacio,nchar(datos_borme$`Denominación social`[i])))
            if(nchar(forma_juridica1) > 3){
                nuevo_nombre <- gsub(" EN LIQUIDACION","",datos_borme$`Denominación social`[i])
                pos_ultimo_espacio <- gregexpr(" ",nuevo_nombre)[[1]][length(gregexpr(" ",nuevo_nombre)[[1]])]
                forma_juridica1 <- str_trim(substring(nuevo_nombre,pos_ultimo_espacio,nchar(nuevo_nombre)))

                if(nchar(forma_juridica1) > 3){
                    forma_juridica1 <- "Otras"
                }
            }
            forma_juridica <- c(forma_juridica, forma_juridica1)
        }

        datos_borme$`Forma Jurídica` <- gsub("\\.","",forma_juridica)

        # Fechas
        datos_borme$Fecha <- as.character(datos_borme$Fecha)
        
        # Capital social de consituticion a num para orden en tabla.
        datos_borme$`Constitución capital`[datos_borme$`Constitución capital` != "-"] <- str_match(datos_borme$`Constitución capital`[datos_borme$`Constitución capital` != "-"], " \\s*(.*?)\\s* euros")[,2]
        datos_borme$`Constitución capital`[!grepl("[a-d]",datos_borme$`Constitución capital`) & datos_borme$`Constitución capital` != "-"] <- gsub("[,].*","",unlist(gsub("[ ].*","", str_extract_all(datos_borme$`Constitución capital`[!grepl("[a-d]",datos_borme$`Constitución capital`) & datos_borme$`Constitución capital` != "-"],"\\(?[0-9,.]+\\)?"))))
        datos_borme$`Constitución capital`[!grepl("[a-d]",datos_borme$`Constitución capital`) & datos_borme$`Constitución capital` != "-"] <- as.numeric(gsub("[.]","",datos_borme$`Constitución capital`[!grepl("[a-d]",datos_borme$`Constitución capital`) & datos_borme$`Constitución capital` != "-"]))
        datos_borme$`Constitución capital`[!grepl("[a-d]",datos_borme$`Constitución capital`) & datos_borme$`Constitución capital` != "-"] <- format(as.numeric(datos_borme$`Constitución capital`[!grepl("[a-d]",datos_borme$`Constitución capital`) & datos_borme$`Constitución capital` != "-"]), big.mark = ".")
        
        return(datos_borme)
    })

    #==========================================================================
    # FILTRADOS
    #==========================================================================

    # 1) Filtrado datos BORME
    datos_filtrados_borme <- reactive({

        df <- datos_estructurados_borme()  #Llamada a API
        
        if(df == 0){
          return(0)
        }
        
        if(input$tabs_borme == "Mapa"){
          variables_entrada <- input$variables_mapa
        }else if(input$tabs_borme == "Modificaciones capital"){
          variables_entrada <- "4"
        }else{
          variables_entrada <- input$variables_borme_listado
        }
    
        #1)Filtrado en funcón de tab
        df <- df[,c("Denominación social",unlist(lista_variables_borme[as.numeric(variables_entrada)]),"Latitud","Longitud","Municipio","Fecha","Forma Jurídica")]

        #df <- na.omit(df)
        
        if(ncol(df) == 0){
          return(0)
        }else if(is.null(variables_entrada)){
          return(1)
        }else if(is.null(colnames(df[,2:(ncol(df)-5)])) & ncol(df) != 7 ){
          return(2)
        }
        
        # 2)Filtro Agrupaciones
        input_municipio <- ifelse(input$tabs_borme == "Mapa",input$Municipio_principal_mapa,input$Municipio_principal)
        municipio_entrada <- input_municipio %>% gsub(", La","",.) %>%
          gsub(", Las","",.) %>% 
          gsub(", Los","",.)
        df$M = df$Municipio %>% gsub("La ","",.) %>%
          gsub("Las ","",.) %>% 
          gsub("Los ","",.)

        # 2)Filtro Agrupaciones
        if(input_municipio == "Burgos provincia"){
            df <- df
        }else if(input_municipio == "Burgos provincia sin Aranda de Duero"){
          df <- df[df$Municipio != "Aranda de Duero",]
        }else if(input_municipio == "Comarca Aranda de Duero"){
          df <- df[which(df$M %in% municipios$M[municipios$Comarca == 1]),]
        }else if(input_municipio == "Comarca Aranda de Duero sin Aranda de Duero"){
          df <- df[which(df$M %in% municipios$M[municipios$Comarca == 1 & municipios$M != "Aranda de Duero"]),]
        }else{
          df <- df[df$M == municipio_entrada,]
        }
        df <- df[,1:(ncol(df)-1)]
        
        if(nrow(df) == 0){
          return(0)
        }
        
        #Eliminación filas con "-" en todas las columnas
        df[df=="-"] <- NA
        if(ncol(df) == 7){
          df <- df[!is.na(df[,2]),]
        }else{
          df <- df[rowSums(is.na(df[,2:(ncol(df)-5)])) != (ncol(df[,2:(ncol(df)-5)])), ]  #Se elimina si el número de columnas de la fila (las que se pueden seleccionar) con todo NA == al número de columnas (las que se pueden seleccionar)
        }
        
        if(nrow(df) == 0){
          return(0)
        }
        
        return(df)
    })

    # 2) Filtrado datos BORME TERRITORIO DE COMPARACIÓN
    datos_filtrados_borme_comparativa <- reactive({

        df <- datos_estructurados_borme()  #Llamada a API
        
        if(df == 0 | df == 1 | df == 2){
          return(df)
        }

        if(input$tabs_borme == "Mapa"){
          variables_entrada <- input$variables_mapa
        }else if(input$tabs_borme == "Modificaciones capital"){
          variables_entrada <- "4"
        }else{
          variables_entrada <- input$variables_borme_listado
        }
        
        #1)Filtrado en funcón de tab
        df <- df[,c("Denominación social",unlist(lista_variables_borme[as.numeric(variables_entrada)]),"Latitud","Longitud","Municipio","Fecha","Forma Jurídica")]
        
        
        if(ncol(df) == 0){
          return(0)
        }else if(is.null(variables_entrada)){
          return(1)
        }else if(is.null(colnames(df[,2:(ncol(df)-5)])) & ncol(df) != 7 ){
          return(2)
        }

        # Manejo de error: "inexistencia de datos para los filtros seleccionados" de cara al usuario
        shiny::validate(
            need(ncol(df) != 0,
                 "")
        )
        #shiny::validate(
        #need(ncol(df[,2:(ncol(df)-6)]) != 0,
        #"¡Atención!\nNo existen datos disponibles para el valor de los filtros seleccionados.\nModifica el valor de los filtros si lo desea.")
        #)

        # 2)Filtro Agrupaciones
        municipio_entrada <- input$Municipio_comparaciones %>% gsub(", La","",.) %>%
          gsub(", Las","",.) %>% 
          gsub(", Los","",.)
        df$M = df$Municipio %>% gsub("La ","",.) %>%
          gsub("Las ","",.) %>% 
          gsub("Los ","",.)
        
        if(input$comparaciones == "Burgos provincia"){
          df <- df
        }else{
          df <- df[df$M == municipio_entrada,]
        }
        df <- df[,1:(ncol(df)-1)]
        
        if(nrow(df) == 0){
          return(0)
        }else if(is.null(variables_entrada)){
          return(1)
        }else if(is.null(colnames(df[,2:(ncol(df)-5)])) & ncol(df) != 7 ){
          return(2)
        }

        #Eliminación "-" si están en todas las columnas
        df[df=="-"] <- NA
        if(ncol(df) == 7){
          df <- df[!is.na(df[,2]),]
        }else{
          df <- df[rowSums(is.na(df[,2:(ncol(df)-5)])) != (ncol(df[,2:(ncol(df)-5)])), ]  #Se elimina si el número de columnas (las que se pueden seleccionar) con todo NA == al número de columnas (las que se pueden seleccionar)
        }
        
        # Manejo de error: "inexistencia de datos para los filtros seleccionados" de cara al usuario
        #shiny::validate(
        #need(nrow(df) != 0,
        #        "")
        #)

        return(df)
    })

    # ==========================================
    # FUNCIONES
    # ==========================================
    
    # 1) Función estadística básica 1: RECUENTO
    func_estadistica_basica <- function(df){
        df <- df
        
        if(!is.data.frame(df)){
          return(df)
        }else if(nrow(df) == 0){
          return(df)
        }

        # Recuento empresas por formas jurídicas
        df <- df %>%
            group_by(`Forma Jurídica`) %>%
            summarise(n = n())

        #Generación DF
        nombre_columnas <- as.vector(df[,1])
        df <- as.data.frame(t(df),row.names = FALSE,stringsAsFactors = FALSE)

        for(i in 1:ncol(df)){
            colnames(df)[i] <- nombre_columnas[i,]
            df[,i] <- as.numeric(df[,i])
        }

        df <- na.omit(df)

        if(ncol(df) > 1){
            #Suma a Otras formas jurídicas no reconocidas
            pos_no_sociedades <- grep("[0-9]",colnames(df))
            if(!identical(pos_no_sociedades,integer(0))){
                df$Otras <- df$Otras + as.numeric(df[,pos_no_sociedades])
                df <- df[,-pos_no_sociedades]
            }
            pos_no_sociedades <- grep("FP",colnames(df))
            if(!identical(pos_no_sociedades,integer(0))){
                df$Otras <- df$Otras + as.numeric(df[,pos_no_sociedades])
                df <- df[,-pos_no_sociedades]
            }

            #Orden de Otras al final en columnas
            df <- df[,order(colnames(df))]
            pos_otras <- grep("Otras",colnames(df))
            if(!identical(pos_otras,integer(0)) & ncol(df) > 2){
                orden <- order(colnames(df[,-pos_otras]))
                for(i in pos_otras:length(orden)){
                    orden[i] <- orden[i] + 1
                }
                df <- df[,c(orden,pos_otras)]
            }else{
              df <- df
            }
        }
        
        return(df)
    }
    
    # 2) Función estadística básica 2: MEDIA, MAX, MIN Y PERCENTILES
    recuento_estadistica_basica_2 <- function(df, flag_return){
      df <- df
      
      if(df == 0 | df == 1 | df == 2){
        return(df)
      }
      
      #Recuento por mes y forma jurídica
      df <- df %>%
        group_by(`Forma Jurídica`, Mes) %>%
        summarise(Recuento = n())
      
      if(flag_return == 1){  # El valor de 1 es para la realización del gráfico de evolución.
        return(df)
      }
      
      # Cálculo estadístivas
      df <- df %>%
        group_by(`Forma Jurídica`) %>%
        summarise(
          Media = round(mean(Recuento),0),
          Máx = max(Recuento),
          Mín = min(Recuento),
          `Desviación típica` = round(sd(Recuento),2),
          `Percentil 25` = round(quantile(Recuento,0.25),2),
          `Percentil 75` = round(quantile(Recuento,0.75),2)
        )
      
      #Generación DF
      nombre_columnas <- as.vector(df[,1])
      df <- as.data.frame(t(df),row.names = FALSE,stringsAsFactors = FALSE)
      
      for(i in 1:ncol(df)){
        colnames(df)[i] <- nombre_columnas[i,]
        df[,i] <- as.numeric(df[,i])
      }
      
      #df <- na.omit(df)
      if(ncol(df) > 1){
        #Suma a Otras formas jurídicas no reconocidas
        pos_no_sociedades <- grep("[0-9]",colnames(df))
        if(!identical(pos_no_sociedades,integer(0))){
          df$Otras <- df$Otras + as.numeric(df[,pos_no_sociedades])
          df <- df[,-pos_no_sociedades]
        }
        pos_no_sociedades <- grep("FP",colnames(df))
        if(!identical(pos_no_sociedades,integer(0))){
          df$Otras <- df$Otras + as.numeric(df[,pos_no_sociedades])
          df <- df[,-pos_no_sociedades]
        }
        
        #Orden de Otras al final en columnas
        df <- df[,order(colnames(df))]
        pos_otras <- grep("Otras",colnames(df))
        if(!identical(pos_otras,integer(0)) & ncol(df) > 2){
          orden <- order(colnames(df[,-pos_otras]))
          for(i in pos_otras:length(orden)){
            orden[i] <- orden[i] + 1
          }
          df <- df[,c(orden,pos_otras)]
        }else{
          df <- df
        }
      }
      nombre_columnas <- colnames(df)
      df <- as.data.frame(df[-1,]) #La primera fila no es un valor numérico (resultado de pasar a númerico la forma jurídica)
      colnames(df) <- nombre_columnas
      rownames(df) <- c("Media", "Máximo", "Mínimo","Desviación típica","Percentil 25", "Percentil 75")
      
      return(df)
    }
    
    # Función de obtención de representaciones porcentuales
    representacion_por_variables <- function(df){
      df <- df
      
      # Logica selector variables
      if(input$tabs_borme == "Mapa"){
        variables_entrada <- input$variables_mapa
      }else if(input$tabs_borme == "Modificaciones capital"){
        variables_entrada <- "4"
      }else{
        variables_entrada <- input$variables_borme_listado
      }
      
      # Extracción recuento de filas por variables
      if(is.data.frame(df)){
        recuento <- c()
        for(i in 1:length(variables_entrada)){
          if(is.data.frame(df)){
            df2 <- df[,unlist(lista_variables_borme[as.numeric(variables_entrada[i])])]
            if(is.data.frame(df2)){
              df2 <- df2[rowSums(is.na(df2)) != ncol(df2), ]  # Elimna las filas donde todas sus columnas son NA
              recuento <- c(recuento, nrow(df2))
            }else if(is.vector(df2)){
              valor <- ifelse(is.na(na.omit(df2)[1]),0,length(na.omit(df2)))
              recuento <- c(recuento, valor)
            }else{
              recuento <- c(recuento, 0)
            }
          }
        }
      }else{
        recuento <- 0
      }
      
      if(sum(recuento) == 0){
        return(0)
      }
      Recuento <- recuento[recuento > 0]
      Representación <- round((recuento/sum(Recuento))*100,2)
      Representación <- Representación[Representación > 0]
      df <- data.frame(Recuento,Representación,stringsAsFactors = FALSE)
      df$Variable <- names(nombres_variables)[as.numeric(variables_entrada[seq(1,length(Recuento))])]
      return(df)
    }
    
    # Función de obtención de representaciones porcentuales por forma jurídica
    representacion_por_variables_y_forma_juridica <- function(df){
      df <- df
      
      # Logica selector variables
      if(input$tabs_borme == "Mapa"){
        variables_entrada <- input$variables_mapa
      }else if(input$tabs_borme == "Modificaciones capital"){
        variables_entrada <- "4"
      }else{
        variables_entrada <- input$variables_borme_listado
      }
      
      for(cont_forma in 1:length(unique(df$`Forma Jurídica`))){  # Bucle por cada forma jurídica
        # Extracción recuento de filas por variables
        if(is.data.frame(df)){
          recuento <- c()
          df_forma <- df[df$`Forma Jurídica` == unique(df$`Forma Jurídica`)[cont_forma],]
          for(i in 1:length(variables_entrada)){
            if(is.data.frame(df_forma)){
              df2 <- df_forma[,c("Forma Jurídica",unlist(lista_variables_borme[as.numeric(variables_entrada[i])]))]
              if(is.data.frame(df2)){
                df2 <- df2[rowSums(is.na(df2)) != (ncol(df2)-1), ]  # Elimna las filas donde todas sus columnas son NA
                recuento <- c(recuento, nrow(df2))
              }else if(is.vector(df2)){
                valor <- ifelse(is.na(na.omit(df2)[1]),0,length(na.omit(df2)))
                recuento <- c(recuento, valor)
              }else{
                recuento <- c(recuento, 0)
              }
            }
          }
        }else{
          recuento <- 0
        }
        
        if(sum(recuento) == 0){
          return(0)
        }
        
        #recuento[recuento == 0] <- NA
        Recuento <- recuento
        Variable <- names(nombres_variables)[as.numeric(variables_entrada[seq(1,length(Recuento))])]
        if(cont_forma == 1){
          df_final_parcial <- data.frame(Variable,Recuento,stringsAsFactors = FALSE)
          colnames(df_final_parcial) <- c("Variable",unique(df$`Forma Jurídica`)[cont_forma])
        }else{
          df_final_parcial$Recuento <- Recuento
          colnames(df_final_parcial)[1+cont_forma] <- unique(df$`Forma Jurídica`)[cont_forma]
        }
        
        recuento <- c()
      }

      return(df_final_parcial)
    }
      

    # ===================================
    # REACTIVE APOYO FUNCIONES
    # ===================================
    
    # 1) Estadistica básica 1
    estadistica_basica_1 <- reactive({
        
        df_ref <- func_estadistica_basica(datos_filtrados_borme())
        
        if((df_ref == 0 | df_ref == 1 | df_ref == 2) & !is.data.frame(df_ref)){
          return(df_ref)
        }
        
        # Cálculo representación en el territorio de referencia
        df_representacion_en_territorio <- df_ref
        #Llenado con NAs
        for(i in 1:ncol(df_ref)){
          df_representacion_en_territorio[,i] <- rep(NA,nrow(df_ref))
        }
        for(i in 1:ncol(df_ref)){
          df_representacion_en_territorio[,i] <- round(100*(df_ref[,i]/sum(as.numeric(df_ref[1,]))),2)
        }
        
        df_comparativa <- func_estadistica_basica(datos_filtrados_borme_comparativa())
        
        if(!is.data.frame(df_comparativa)){
          df <- rbind(df_ref,df_representacion_en_territorio)
          df$Total <- c(sum(as.numeric(df[1,])),100)
          rownames(df) <- c("Recuento", "Representación en territorio de referencia (%)")
        }else if(nrow(df_comparativa) == 0){
          df <- rbind(df_ref,df_representacion_en_territorio)
          df$Total <- c(sum(as.numeric(df[1,])),100)
          rownames(df) <- c("Recuento", "Representación en territorio de referencia (%)")
        }else{
          # Cálculo representación por formas jurídicas respecto terrritorio comparación
          df_representacion <- df_ref
          #Llenado con NAs
          for(i in 1:ncol(df_ref)){
            df_representacion[,i] <- rep(NA,nrow(df_ref))
          }
          for(i in 1:ncol(df_ref)){
            if(any(colnames(df_comparativa) %in% colnames(df_ref)[i])){
              df_representacion[,i] <- round(100*(df_ref[,i]/df_comparativa[,which(colnames(df_comparativa) %in% colnames(df_ref)[i])]),2)
            }
          }
          
          # Cálculo representación en el territorio de comparativa
          df_representacion_en_territorio_comparativa <- df_ref
          #Llenado con NAs
          for(i in 1:ncol(df_ref)){
            df_representacion_en_territorio_comparativa[,i] <- rep(NA,nrow(df_ref))
          }
          for(i in 1:ncol(df_ref)){
            df_representacion_en_territorio_comparativa[,i] <- round(100*(df_ref[,i]/sum(as.numeric(df_comparativa[1,]))),2)
          }
          
          df <- rbind(df_ref,df_representacion_en_territorio,df_representacion_en_territorio_comparativa,df_representacion)
          
          df$Total <- c(sum(as.numeric(df[1,])),100)
          if(is.na(sum(df[3,1:(ncol(df)-1)]))){
            df$Total[3] <- NA
          }else{
            df$Total[3] <- sum(df[3,1:(ncol(df)-1)])
          }
          if(is.na(sum(df[4,1:(ncol(df)-1)]))){
            df$Total[4] <- NA
          }else{
            df$Total[4] <- NA
          }
          
          if(nrow(df) == 1){
            rownames(df) <- c("Recuento")
          }else{
            rownames(df) <- c("Recuento", "Representación en territorio de referencia (%)", "Representación en territorio de comparación (%)","Representación sobre forma jurídica en territorio de comparación (%)")
          }
        }
        
        df
    })


    # 2) Estadistica básica 2
    estadistica_basica_2 <- reactive({

      #Validación de fechas
      flag_Avís <- ifelse(abs(month(input$fechas_listado_borme[1])-month(input$fechas_listado_borme[2])) >= 2 |
                             as.numeric(input$fechas_listado_borme[2] - input$fechas_listado_borme[1]) > 90,
                           0,1)
      if(flag_Avís == 1){return(0)}

      #Ajuste fechas usuario para cuadrar meses. Inicio de mes.
      fecha_inicial <- input$fechas_listado_borme[1]
      day(fecha_inicial) <- day(fecha_inicial)-(day(fecha_inicial)-1)
      fecha_final <- input$fechas_listado_borme[2]
      day(fecha_final) <- day(fecha_final)-(day(fecha_final)-1)

      df <- datos_filtrados_borme()
      if((df == 0 | df == 1 | df == 2) & !is.data.frame(df)){
        return(df)
      }
      
      df$Fecha <- as.Date(df$Fecha, format="%d/%m/%Y")
      df$Fecha <- as.Date(df$Fecha, format="%Y/%m/%d")
      df$Mes <- format(as.Date(df$Fecha), "%Y-%m")  #Extracción de meses
      df_ref <- recuento_estadistica_basica_2(df,2)  #Flag 2 para devolución con cálculos estadísticos

      if((df_ref == 0 | df_ref == 1 | df_ref == 2) & !is.data.frame(df_ref)){
        return(df_ref)
      }
      
      df2 <- datos_filtrados_borme_comparativa()
      
      if(!is.data.frame(df2)){
        df <- df_ref
      }else if(nrow(df2) == 0){
        df <- df_ref
      }else{
        df2$Fecha <- as.Date(df2$Fecha, format="%d/%m/%Y")
        df2$Fecha <- as.Date(df2$Fecha, format="%Y/%m/%d")
        df2$Mes <- format(as.Date(df2$Fecha), "%Y-%m")  #Extracción de meses
        df_comparativa <- recuento_estadistica_basica_2(df2, 2) #Flag 2 para devolución con cálculos estadísticos
        
        df_representacion <- df_ref
        #Llenado con NAs
        for(i in 1:ncol(df_ref)){
          df_representacion[,i] <- rep(NA,nrow(df_ref))
        }
        
        # Cálculo representación
        for(i in 1:ncol(df_ref)){
          if(any(colnames(df_comparativa) %in% colnames(df_ref)[i])){
          df_representacion[,i] <- round(100*(df_ref[,i]/df_comparativa[,which(colnames(df_comparativa) %in% colnames(df_ref)[i])]),2)
          }
        }
        
        df <- rbind(df_ref,df_representacion)
        
        if(nrow(df) == 1){
          rownames(df) <- c("Media", "Máximo", "Mínimo", "Desviación típica", "Percentil 25", "Percentil 75")
        }else{
          rownames(df) <- c("Media", "Máximo", "Mínimo", "Desviación típica", "Percentil 25", "Percentil 75",
                            "Representación Media (%)", "Representación Máximo (%)", "Representación Mínimo (%)", "Representación Desviación típica (%)", "Representación Percentil 25 (%)", "Representación Percentil 75 (%)")
        }
      }
      
      df
    })
    
    

    #==========================================================================
    # GENERACIÓN TABLAS
    #==========================================================================

    # 0) Manejo de tablas presentes en los tabs BORME. Su función es agrupar las modificaciones comunes del conjunto de tablas de la aplicación
    manejo_tablas_borme <- reactive({

        df_tabla <- datos_filtrados_borme()

        # En caso de solo visualizar la variable empresa, se convierte a objeto tipo entero y hay que volver a reconvertirlo a data.frame
        if(typeof(df_tabla) == "integer"){
            df_tabla <- as.data.frame(df_tabla)
            names(df_tabla) <- "`Denominación social`"
        }
        
        if(is.null(nrow(df_tabla))){
          return(0)
        }

        #Nombres de filas igual a ID entero ascendente
        #Sí no hay datos no modificar el nombre de las filas
        if(nrow(df_tabla) > 0){
            row.names(df_tabla) <- seq(1,nrow(df_tabla))
        }

        return(df_tabla)

    })

    # 1) Tabla BORME listado informativo
    output$tabla_borme_listado <- renderDataTable({
      
        df_tabla <- manejo_tablas_borme()
        
        # Manejo de error: "inexistencia de datos para los filtros seleccionados" de cara al usuario
        shiny::validate(
          need(is.data.frame(df_tabla) & nrow(df_tabla) != 0,
               "¡Atención!\nNo existen datos disponibles para el valor de los filtros seleccionados.\nModifica el valor de los filtros si lo desea.")
        )
        
        posiciones <- which(colnames(df_tabla) %in% c("Latitud","Longitud","Municipio","Fecha","Forma Jurídica"))
        df_tabla <- df_tabla[,-posiciones]

        # Manejo de error: "inexistencia de datos para los filtros seleccionados" de cara al usuario
        shiny::validate(
            need(!is.null(nrow(df_tabla) & is.data.frame(df_tabla)),
                 "¡Atención!\nNo existen datos disponibles para el valor de los filtros seleccionados.\nModifica el valor de los filtros si lo desea.")
        )
        #Límite visualización registros tabla
        tabla <- datatable(df_tabla, options = list(pageLength = 5,
                                                    columnDefs = list(list(className = 'dt-center', targets = "_all")),
                                                    scrollX=TRUE,
                                                    scrollCollapse=TRUE),
                           escape = FALSE)

        return(tabla)

    },options = list(scrollX = T))
    
    # TABLA BORME-CENSO Listado informativo
    output$tabla_borme_censo <- renderDataTable({
      
      df_tabla_borme <- manejo_tablas_borme()
      
      if(!is.data.frame(df_tabla_borme)){
        # Manejo de error: "inexistencia de datos para los filtros seleccionados" de cara al usuario
        shiny::validate(
          need(is.data.frame(df_tabla_borme),
               switch(as.character(df_tabla_borme),
                      "0"={
                        "¡Atención!\nNo existen datos disponibles para el valor de los filtros seleccionados.\nModifica el valor de los filtros si lo desea."
                      },
                      "1"={
                        "¡Atención!\nNo hay ninguna variable seleccionada.\nPor favor, selecciona al menos una variable."
                      },
                      "2"={
                        "¡Atención!\nNo existen datos disponibles para el valor de los filtros seleccionados.\nModifica el valor de los filtros si lo desea."
                      }
               )
          )
        )
      }
      
      # Filtrado por selección de registro en la tabla
      filtrado_tabla <- input$tabla_borme_listado_rows_selected
      if(length(filtrado_tabla)){
        fichas_empresas <-  df_tabla_borme$`Denominación social`[filtrado_tabla]
      }else{
        fichas_empresas <- 0
      }

      shiny::validate(
        need(fichas_empresas != 0,
             "")
      )
      
      df_tabla_censo_filtrado <- df_censo[which(tolower(gsub("[.]","",df_censo$Denominación_social)) %in% tolower(gsub("[.]","",fichas_empresas))),]
      
      #Límite visualización registros tabla
      tabla <- datatable(df_tabla_censo_filtrado, options = list(pageLength = 5,
                                                  columnDefs = list(list(className = 'dt-center', targets = "_all")),
                                                  scrollX=TRUE,
                                                  scrollCollapse=TRUE),
                         escape = FALSE)
      
      return(tabla)
      
    },options = list(scrollX = T))
    
    # TABLA BORME-CENSO mapa
    output$tabla_borme_censo_mapa <- renderDataTable({
      
      df_tabla_borme <- manejo_tablas_borme()
      
      if(!is.data.frame(df_tabla_borme)){
        # Manejo de error: "inexistencia de datos para los filtros seleccionados" de cara al usuario
        shiny::validate(
          need(is.data.frame(df_tabla_borme),
               switch(as.character(df_tabla_borme),
                      "0"={
                        "¡Atención!\nNo existen datos disponibles para el valor de los filtros seleccionados.\nModifica el valor de los filtros si lo desea."
                      },
                      "1"={
                        "¡Atención!\nNo hay ninguna variable seleccionada.\nPor favor, selecciona al menos una variable."
                      },
                      "2"={
                        "¡Atención!\nNo existen datos disponibles para el valor de los filtros seleccionados.\nModifica el valor de los filtros si lo desea."
                      }
               )
          )
        )
      }
      
      # Filtrado por selección de registro en la tabla
      filtrado_tabla <- input$tabla_borme_mapa_rows_selected
      if(length(filtrado_tabla)){
        fichas_empresas <-  df_tabla_borme$`Denominación social`[filtrado_tabla]
      }else{
        fichas_empresas <- 0
      }
      
      shiny::validate(
        need(fichas_empresas != 0,
             "")
      )
      
      df_tabla_censo_filtrado <- df_censo[which(tolower(gsub("[.]","",df_censo$Denominación_social)) %in% tolower(gsub("[.]","",fichas_empresas))),]

      #Límite visualización registros tabla
      tabla <- datatable(df_tabla_censo_filtrado, options = list(pageLength = 5,
                                                                 columnDefs = list(list(className = 'dt-center', targets = "_all")),
                                                                 scrollX=TRUE,
                                                                 scrollCollapse=TRUE),
                         escape = FALSE)
      
      return(tabla)
      
    },options = list(scrollX = T))

    # 2) Tabla BORME ESTADÍSTICA BÁSICA 1
    output$tabla_borme_eb1 <- renderDataTable({

        df_tabla <- estadistica_basica_1()
 
        if(!is.data.frame(df_tabla)){
          # Manejo de error: "inexistencia de datos para los filtros seleccionados" de cara al usuario
          shiny::validate(
            need(is.data.frame(df_tabla),
                 switch(as.character(df_tabla),
                        "0"={
                          "¡Atención!\nNo existen datos disponibles para el valor de los filtros seleccionados.\nModifica el valor de los filtros si lo desea."
                        },
                        "1"={
                          "¡Atención!\nNo hi ha cap variable seleccionada.\nSi us plau, selecciona el menys una variable."
                        },
                        "2"={
                          "¡Atención!\nNo existen datos disponibles para el valor de los filtros seleccionados.\nModifica el valor de los filtros si lo desea."
                        }
                 )
            )
          )
        }
        
        df_tabla$Total <- round(df_tabla$Total,0)

        #Límite visualización registros tabla
        tabla <- datatable(df_tabla, options = list(pageLength = 5,
                                                    columnDefs = list(list(className = 'dt-center', targets = "_all")),
                                                    scrollX=TRUE,
                                                    scrollCollapse=TRUE),
                           escape = FALSE)
        return(tabla)
    },options = list(scrollX = T))


    # 3) Tabla BORME ESTADÍSTICA BÁSICA 2
    output$tabla_borme_eb2 <- renderDataTable({

        df <- estadistica_basica_2()
        
        # Manejo de error: "inexistencia de datos para los filtros seleccionados" de cara al usuario
        shiny::validate(
          need(df != 0,
               "¡Atención!\nEs necesario selecionar un periodo de 2 meses para el cálculo de estadísticas mensuales.\nModifica el valor de los filtros si lo desea.")
        )
        
        # Manejo de error: "inexistencia de datos para los filtros seleccionados" de cara al usuario
        shiny::validate(
          need(df != 1,
               "¡Atención!\nNo existen datos disponibles para el valor de los filtros seleccionados.\nModifica el valor de los filtros si lo desea.")
        )
        
        #Límite visualización registros tabla
        tabla <- datatable(df, options = list(pageLength = 25,
                                                    columnDefs = list(list(className = 'dt-center', targets = "_all")),
                                                    scrollX=TRUE,
                                                    scrollCollapse=TRUE),
                           escape = FALSE)
        return(tabla)
    },options = list(scrollX = T))

    # 4) Tabla BORME MAPA
    output$tabla_borme_mapa <- renderDataTable({

      df_tabla <- manejo_tablas_borme()
      
      if(input$variables_mapa == 1){
        variable <- "Constitución objeto social"
      }else{
        variable <- "Cambio domicilio social"
      }
      shiny::validate(
        need(any(grepl(variable,colnames(na.omit(df_tabla)))) & nrow(na.omit(df_tabla)) != 0,
             "¡Atención!\nNo existen datos disponibles para el valor de los filtros seleccionados.\nModifica el valor de los filtros si lo desea."
        )
      )
      
      if(input$variables_mapa == 1){
        df_tabla <- df_tabla[,c(1,2,3,4,5)]
      }else{
        df_tabla <- df_tabla[,c(1,2,3)]
      }
      
      #Límite visualización registros tabla
      tabla <- datatable(df_tabla, options = list(pageLength = 5,
                                                  columnDefs = list(list(className = 'dt-center', targets = "_all")),
                                                  scrollX=TRUE,
                                                  scrollCollapse=TRUE),
                         escape = FALSE)
      return(tabla)
    })
    
    #5.1) Reactive ayuda a tabla modificaciones capital
    ayuda_borme_capital <- reactive({
      
      df <- manejo_tablas_borme()
      
      shiny::validate(
        need(nrow(df) != 0 & is.data.frame(df),
             "¡Atención!\nNo hay datos suficientes como para generar el gráfico."
        )
      )
      
      # Paso a númerico capital
      for(i in 2:7){
        df[,i] <- gsub("[a-z].","",df[,i])
        df[,i] <- gsub("[.]","",df[,i])
        df[,i] <- gsub("[.]","",df[,i])
        df[,i] <- as.numeric(gsub("[,]",".",df[,i]))
      }
      
      #Incluir desembolsado (es la alternativa a capital, a veces aparece así)
      for(i in 1:nrow(df)){
        if(is.na(df$`Ampliación capital`[i])){
          df$`Ampliación capital`[i] <- df$`Ampliación capital desembolsado`[i]
          df$`Ampliación capital suscrito`[i] <- df$`Ampliación capital resultante desembolsado`[i]
        }
      }
      
      # Cálculo evoluciones ampliaciones y reducciones de capital
      df <- df %>%
        mutate(`Evolución ampliación` = 100*((`Ampliación capital suscrito` - (`Ampliación capital suscrito` - `Ampliación capital`))/(`Ampliación capital suscrito` - `Ampliación capital`))) %>%
        mutate(`Evolución reducción` = 100*(((`Reducción capital resultante suscrito` + `Reducción capital importe reducción`)-`Reducción capital resultante suscrito`)/(`Reducción capital resultante suscrito` + `Reducción capital importe reducción`)))
      
      `Evolución ampliación` <- na.omit(df$`Evolución ampliación`)
      `Evolución reducción` <- na.omit(df$`Evolución reducción`)

      # Llamada a función para filtrado de atípicos
      df <- df %>%
        group_by(`Forma Jurídica`) %>% 
        mutate(`Evolución ampliación` = detectar_atipicios(`Evolución ampliación`)) %>%
        mutate(`Evolución reducción` = detectar_atipicios(`Evolución reducción`))

      df3 <- df   # Data frame de referencia sin atípicos para el c'alculo de recuentos y medias
      
      # Cálculo medias de evolución y reducciones de capital
      df <- df3 %>%
        group_by(`Forma Jurídica`) %>%
        summarise(
          `Media evolución ampliación` = round(mean(`Evolución ampliación`, na.rm = TRUE),2),
          `Media evolución reducción` = round(mean(`Evolución reducción`, na.rm = TRUE),2)
        )
      
      #Generación DF
      nombre_columnas <- as.vector(df[,1])
      df <- as.data.frame(t(df),row.names = FALSE,stringsAsFactors = FALSE)
      
      for(i in 1:ncol(df)){
        colnames(df)[i] <- nombre_columnas[i,]
        df[,i] <- as.numeric(df[,i])
      }
      
      nombre_columnas <- colnames(df)
      df <- as.data.frame(df[-1,]) #La primera fila no es un valor numérico (resultado de pasar a númerico la forma jurídica)
      colnames(df) <- nombre_columnas
      
      #df <- na.omit(df)
      
      if(ncol(df) > 1){
        #Suma a Otras formas jurídicas no reconocidas
        pos_no_sociedades <- grep("[0-9]",colnames(df))
        if(!identical(pos_no_sociedades,integer(0))){
          df$Otras <- df$Otras + as.numeric(df[,pos_no_sociedades])
          df <- df[,-pos_no_sociedades]
        }
        pos_no_sociedades <- grep("FP",colnames(df))
        if(!identical(pos_no_sociedades,integer(0))){
          df$Otras <- df$Otras + as.numeric(df[,pos_no_sociedades])
          df <- df[,-pos_no_sociedades]
        }
        
        #Orden de Otras al final en columnas
        df <- df[,order(colnames(df))]
        pos_otras <- grep("Otras",colnames(df))
        if(!identical(pos_otras,integer(0)) & ncol(df) > 2){
          orden <- order(colnames(df[,-pos_otras]))
          for(i in pos_otras:length(orden)){
            orden[i] <- orden[i] + 1
          }
          df <- df[,c(orden,pos_otras)]
        }else{
          df <- df[,c(2,1)]
        }
      }
      
      #df <- df[,colSums(is.na(df))<nrow(df)]
      rownames(df) <- c("Evolución ampliación (%)", "Evolución reducción (%)")
      
      df$Total <- rowMeans(df, na.rm=TRUE)
      
      # Genearación df recuento ampliación
      df_recuento_ampliacion <- df3[,1:(ncol(df3)-1)]
      df_recuento_ampliacion <- df_recuento_ampliacion[!is.na(df_recuento_ampliacion$`Evolución ampliación`),]
      df_recuento_ampliacion <- df_recuento_ampliacion %>%
        group_by(`Forma Jurídica`) %>%
        summarise(`Recuento ampliació` = n()
        )
      
      if(nrow(df_recuento_ampliacion) != 0){
        #Generación df_recuento_ampliacion
        nombre_columnas <- as.vector(df_recuento_ampliacion[,1])
        df_recuento_ampliacion <- as.data.frame(t(df_recuento_ampliacion),row.names = FALSE,stringsAsFactors = FALSE)
        
        for(i in 1:ncol(df_recuento_ampliacion)){
          colnames(df_recuento_ampliacion)[i] <- nombre_columnas[i,]
          df_recuento_ampliacion[,i] <- as.numeric(df_recuento_ampliacion[,i])
        }
        
        nombre_columnas <- colnames(df_recuento_ampliacion)
        df_recuento_ampliacion <- as.data.frame(df_recuento_ampliacion[-1,]) #La primera fila no es un valor numérico (resultado de pasar a númerico la forma jurídica)
        colnames(df_recuento_ampliacion) <- nombre_columnas
        
        #df_recuento_ampliacion <- na.omit(df_recuento_ampliacion)
        
        if(ncol(df_recuento_ampliacion) > 1){
          #Suma a Otras formas jurídicas no reconocidas
          pos_no_sociedades <- grep("[0-9]",colnames(df_recuento_ampliacion))
          if(!identical(pos_no_sociedades,integer(0))){
            df_recuento_ampliacion$Otras <- df_recuento_ampliacion$Otras + as.numeric(df_recuento_ampliacion[,pos_no_sociedades])
            df_recuento_ampliacion <- df_recuento_ampliacion[,-pos_no_sociedades]
          }
          pos_no_sociedades <- grep("FP",colnames(df_recuento_ampliacion))
          if(!identical(pos_no_sociedades,integer(0))){
            df_recuento_ampliacion$Otras <- df_recuento_ampliacion$Otras + as.numeric(df_recuento_ampliacion[,pos_no_sociedades])
            df_recuento_ampliacion <- df_recuento_ampliacion[,-pos_no_sociedades]
          }
          
          #Orden de Otras al final en columnas
          df_recuento_ampliacion <- df_recuento_ampliacion[,order(colnames(df_recuento_ampliacion))]
          pos_otras <- grep("Otras",colnames(df_recuento_ampliacion))
          if(!identical(pos_otras,integer(0)) & ncol(df_recuento_ampliacion) > 2){
            orden <- order(colnames(df_recuento_ampliacion[,-pos_otras]))
            for(i in pos_otras:length(orden)){
              orden[i] <- orden[i] + 1
            }
            df_recuento_ampliacion <- df_recuento_ampliacion[,c(orden,pos_otras)]
          }else{
            df_recuento_ampliacion <- df_recuento_ampliacion
          }
        }
        
        rownames(df_recuento_ampliacion) <- "Recuento ampliaciones"
        df_recuento_ampliacion$Total <- rowSums(df_recuento_ampliacion, na.rm=TRUE)
      }
      
      
      
      # Genearación df recuento reducción
      df_recuento_reduccion <- df3[,c(1:(ncol(df3)-2),ncol(df3))]
      df_recuento_reduccion <- df_recuento_reduccion[!is.na(df_recuento_reduccion$`Evolución reducción`),]
      df_recuento_reduccion <- df_recuento_reduccion %>%
        group_by(`Forma Jurídica`) %>%
        summarise(`Recuento reducció` = n()
        )
      
      if(nrow(df_recuento_reduccion) != 0){
        #Generación df_recuento_reduccion
        nombre_columnas <- as.vector(df_recuento_reduccion[,1])
        df_recuento_reduccion <- as.data.frame(t(df_recuento_reduccion),row.names = FALSE,stringsAsFactors = FALSE)
        
        for(i in 1:ncol(df_recuento_reduccion)){
          colnames(df_recuento_reduccion)[i] <- nombre_columnas[i,]
          df_recuento_reduccion[,i] <- as.numeric(df_recuento_reduccion[,i])
        }
        
        nombre_columnas <- colnames(df_recuento_reduccion)
        df_recuento_reduccion <- as.data.frame(df_recuento_reduccion[-1,]) #La primera fila no es un valor numérico (resultado de pasar a númerico la forma jurídica)
        colnames(df_recuento_reduccion) <- nombre_columnas
        
        #df_recuento_reduccion <- na.omit(df_recuento_reduccion)
        
        if(ncol(df_recuento_reduccion) > 1){
          #Suma a Otras formas jurídicas no reconocidas
          pos_no_sociedades <- grep("[0-9]",colnames(df_recuento_reduccion))
          if(!identical(pos_no_sociedades,integer(0))){
            df_recuento_reduccion$Otras <- df_recuento_reduccion$Otras + as.numeric(df_recuento_reduccion[,pos_no_sociedades])
            df_recuento_reduccion <- df_recuento_reduccion[,-pos_no_sociedades]
          }
          pos_no_sociedades <- grep("FP",colnames(df_recuento_reduccion))
          if(!identical(pos_no_sociedades,integer(0))){
            df_recuento_reduccion$Otras <- df_recuento_reduccion$Otras + as.numeric(df_recuento_reduccion[,pos_no_sociedades])
            df_recuento_reduccion <- df_recuento_reduccion[,-pos_no_sociedades]
          }
          
          #Orden de Otras al final en columnas
          df_recuento_reduccion <- df_recuento_reduccion[,order(colnames(df_recuento_reduccion))]
          pos_otras <- grep("Otras",colnames(df_recuento_reduccion))
          if(!identical(pos_otras,integer(0)) & ncol(df_recuento_reduccion) > 2){
            orden <- order(colnames(df_recuento_reduccion[,-pos_otras]))
            for(i in pos_otras:length(orden)){
              orden[i] <- orden[i] + 1
            }
            df_recuento_reduccion <- df_recuento_reduccion[,c(orden,pos_otras)]
          }else{
            df_recuento_reduccion <- df_recuento_reduccion
          }
        }
        
        #df_recuento_reduccion <- df_recuento_reduccion[,colSums(is.na(df_recuento_reduccion))<nrow(df_recuento_reduccion)]
        rownames(df_recuento_reduccion) <- "Recuento reducciones"
        df_recuento_reduccion$Total <- rowSums(df_recuento_reduccion, na.rm=TRUE)
      }
      
      recuento_ref_ampl <- data.frame(rep(NA,ncol(df)),stringsAsFactors = FALSE)
      recuento_ref_ampl <- as.data.frame(t(recuento_ref_ampl))
      colnames(recuento_ref_ampl) <- colnames(df)
      rownames(recuento_ref_ampl) <- "Recuento ampliaciones"
      recuento_ref_reducc <- data.frame(rep(NA,ncol(df)),stringsAsFactors = FALSE)
      recuento_ref_reducc <- as.data.frame(t(recuento_ref_reducc))
      colnames(recuento_ref_reducc) <- colnames(df)
      rownames(recuento_ref_reducc) <- "Recuento reducciones"
      
      
      # Generación data frame referencia recuento ampliaciones
      namestoChange <- colnames(recuento_ref_ampl)[colnames(df_recuento_ampliacion) %in% colnames(recuento_ref_ampl)]
      for(i in 1:length(namestoChange)){
        if(any(colnames(df_recuento_ampliacion) %in% namestoChange[i])){
          recuento_ref_ampl[,namestoChange[i]] <- df_recuento_ampliacion[,namestoChange[i]]
        }
      }
      
      # Generación data frame referencia recuento reducciones
      namestoChange <- colnames(recuento_ref_reducc)[colnames(df_recuento_ampliacion) %in% colnames(recuento_ref_reducc)]
      for(i in 1:length(namestoChange)){
        if(any(colnames(df_recuento_reduccion) %in% namestoChange[i])){
          recuento_ref_reducc[,namestoChange[i]] <- df_recuento_reduccion[,namestoChange[i]]
        }
      }
      
      df <- rbind(recuento_ref_ampl,df[1,],recuento_ref_reducc,df[2,])
      df[2,] <- round(df[2,],2)
      df[4,] <- round(df[4,],2)
      
      return(df)
      
    })
    
    # 5.2) Tabla modificaciones de capital
    output$tabla_borme_capital <- renderDataTable({
      
      df <- ayuda_borme_capital()

      #Límite visualización registros tabla
      tabla <- datatable(df, options = list(pageLength = 5,
                                                  columnDefs = list(list(className = 'dt-center', targets = "_all")),
                                                  scrollX=TRUE,
                                                  scrollCollapse=TRUE),
                         escape = FALSE)
      return(tabla)
    },options = list(scrollX = T))
  


    #==========================================================================
    # GENERACIÓN GRÁFICOS
    #==========================================================================
    
    # 1) Gráfico queso para estadística básica 1
    output$queso_borme_agregado_eb1 <- renderPlotly({
      
      df <- estadistica_basica_1()
      if(!is.data.frame(df)){
        return(NULL)
      }
      shiny::validate(
        need(ncol(df) > 0,
             "¡Atención!\nNo hay datos suficientes como para generar el gráfico."
        )
      )
      
      if(ncol(df) > 1){df <- df[ , colnames(df)[1:(ncol(df)-1)], drop = FALSE]}
      df <- as.data.frame(t(df))
      if(ncol(df) == 1){
        colnames(df) <- c("Recuento")
        texto <- paste("Forma jurídica: ", df$`Forma jurídica`,"<br>Recuento: ",df$Recuento,sep = "")
      }else{
        colnames(df) <- c("Recuento","Representació")
        texto <- paste("Forma jurídica: ", df$`Forma jurídica`,"<br>Recuento: ",df$Recuento, "<br>Representació (%): ",df$Representació,sep = "")
      }
      df$`Forma jurídica` <- rownames(df)
      
      #Gráfico
      g <- plot_ly(type='pie', labels=df$`Forma jurídica`, values=df$Recuento, 
                     textinfo='label+percent',
                     insidetextorientation='auto',
                     textposition='inside')
      g <- g %>% layout(
        title = list(text = paste('<b>Representación porcentual por forma jurídica ', input$Municipio_principal, '</b>',sep = ''), y = -0.1)
      )
      g
      
    })
    
    #EB I
    # 2) Gráfico queso represetnación por varaible EBI
    output$queso_borme_desagregado_eb1 <- renderPlotly({
      
      df <- representacion_por_variables(datos_filtrados_borme())
      
      shiny::validate(
        need(df != 0,
             "¡Atención!\nNo hay datos suficientes como para generar el gráfico."
        )
      )
      
      #Gráfico
      g <- plot_ly(type='pie', labels=df$Variable, values=df$Recuento, 
                   textinfo='label+percent',
                   insidetextorientation='auto',
                   textposition='inside')
      g <- g %>% layout(
        title = list(text = paste('<b>Representación porcentual por Variable ', input$Municipio_principal, '</b>',sep = ''), y = -0.1)
      )
      g
      
    })
    
    #EB I
    # 3) Gráfico queso represetnación por varaible EBI
    output$queso_borme_desagregado_forma_eb1 <- renderPlotly({
      df <- representacion_por_variables_y_forma_juridica(datos_filtrados_borme())

      n <- ceiling((ncol(df)-1)/2) #Grid = matriz de nxn
      n <- ifelse(n == 1,2,n)

      # Inicialización filas
      fila <- 0
      columna <- 0
      
      # Subplots de gráfico de quesos automático
      g <- plot_ly()
      for(i in 1:(ncol(df) - 1)){
        
        columna <- ifelse(i <= n, (i-1), (i-(n + 1))) 
        fila <- floor((i-1)/n)
        
        g <- g %>% add_pie(data = df[,(1+i)], labels = paste(df$Variable,"-",colnames(df)[1+i],sep = ""), values = df[,(1+i)],
                           name = "Variable", domain = list(row = fila, column = columna),
                           textinfo='label+percent',insidetextorientation='auto',textposition='inside')
      }
      
      titulo <- ifelse(input$dimension[1] < 1600 | nchar(paste('<b>Representación porcentual por Variable y forma jurídica ', input$Municipio_principal, '</b>',sep = '')) > 70,
                       paste("<b>Representación porcentual por Variable","<br> y forma jurídica ",input$Municipio_principal, "</b>",sep = ""),
                       paste('<b>Representación porcentual por Variable y forma jurídica ', input$Municipio_principal, '</b>',sep = ''))
      
      g <- g %>% layout(title = list(text = titulo, y = -5), showlegend = F, 
                     grid=list(rows=n, columns=n),
                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      
      
      g
    })
    
    
    # EB II
    # 4) Gráfico lineas para estadística básica 2
    output$lineas_borme_eb2 <- renderPlotly({

      df <- datos_filtrados_borme()
      
      # Manejo de error
      shiny::validate(
        need(is.data.frame(df),
             "")
      )
      shiny::validate(
        need(nrow(df) != 0,
             "")
      )
      
      # Manejo de error
      df2 <- estadistica_basica_2()
      # Manejo de error
      shiny::validate(
        need(is.data.frame(df2),
             "")
      )
      shiny::validate(
        need(nrow(df2) != 0,
             "")
      )
      
      df$Fecha <- as.Date(df$Fecha, format="%d/%m/%Y")
      df$Fecha <- as.Date(df$Fecha, format="%Y/%m/%d")
      df$Mes <- format(as.Date(df$Fecha), "%Y-%m")  #Extracción de meses

      df <- recuento_estadistica_basica_2(df,1)  #Flag 1 para devolución recuento
      
      df <- df %>% 
        dplyr::group_by(`Forma Jurídica`,Mes) %>% 
        dplyr::ungroup() %>%
        tidyr::complete(`Forma Jurídica`, Mes, fill = list(Recuento = 0))
      
      p <- df
      p <- p %>% plot_ly(x = ~Mes, y = ~Recuento, fill= ~`Forma Jurídica`, color = ~`Forma Jurídica`)
      p <- p %>% add_trace(type = 'scatter', mode = 'lines+markers',text = ~paste('Forma jurídica:', `Forma Jurídica`,'<br>Recuento:', Recuento))
      p <- p %>% layout(
        title = paste("<b>Recuento mensual ",input$Municipio_principal, " por forma jurídica</b>",sep = "")
      )

      p
    })
    
    
    # Reactive gráfico lineas AÑO a AÑO estadistica básica 2
    df_lineas_ano_a_ano_anterior <- reactive({
      
      #=========================
      # 2) DATOS FILTRADOS
      #=========================
      df <- datos$borme_anterior #Llamada a API
      
      if(df == 0){
        return(0)
      }
      
      if(input$tabs_borme == "Mapa"){
        variables_entrada <- input$variables_mapa
      }else if(input$tabs_borme == "Modificaciones capital"){
        variables_entrada <- "4"
      }else{
        variables_entrada <- input$variables_borme_listado
      }
      
      #1)Filtrado en funcón de tab
      df <- df[,c("Denominación social",unlist(lista_variables_borme[as.numeric(variables_entrada)]),"Latitud","Longitud","Municipio","Fecha","Forma Jurídica")]
      
      if(ncol(df) == 0){
        return(0)
      }else if(is.null(variables_entrada)){
        return(1)
      }else if(is.null(colnames(df[,2:(ncol(df)-5)])) & ncol(df) != 7 ){
        return(2)
      }
      
      # 2)Filtro Agrupaciones
      input_municipio <- ifelse(input$tabs_borme == "Mapa",input$Municipio_principal_mapa,input$Municipio_principal)
      municipio_entrada <- input_municipio %>% gsub(", La","",.) %>%
        gsub(", Las","",.) %>% 
        gsub(", Los","",.)
      df$M = df$Municipio %>% gsub("La ","",.) %>%
        gsub("Las ","",.) %>% 
        gsub("Los ","",.)
      
      # 2)Filtro Agrupaciones
      if(input_municipio == "Burgos provincia"){
        df <- df
      }else if(input_municipio == "Burgos provincia sin Aranda de Duero"){
        df <- df[df$Municipio != "Aranda de Duero",]
      }else if(input_municipio == "Comarca Aranda de Duero"){
        df <- df[which(df$M %in% municipios$M[municipios$Comarca == 1]),]
      }else if(input_municipio == "Comarca Aranda de Duero sin Aranda de Duero"){
        df <- df[which(df$M %in% municipios$M[municipios$Comarca == 1 & municipios$M != "Aranda de Duero"]),]
      }else{
        df <- df[df$M == municipio_entrada,]
      }
      df <- df[,1:(ncol(df)-1)]
      
      if(nrow(df) == 0){
        return(0)
      }
      
      #Eliminación filas con "-" en todas las columnas
      df[df=="-"] <- NA
      if(ncol(df) == 7){
        df <- df[!is.na(df[,2]),]
      }else{
        df <- df[rowSums(is.na(df[,2:(ncol(df)-5)])) != (ncol(df[,2:(ncol(df)-5)])), ]  #Se elimina si el número de columnas de la fila (las que se pueden seleccionar) con todo NA == al número de columnas (las que se pueden seleccionar)
      }
      
      if(nrow(df) == 0){
        return(0)
      }
      
      

      #=========================
      # 3) PREPARACIÓN DE GRÁFICA
      #=========================
      
      # Manejo de error
      shiny::validate(
        need(is.data.frame(df),
             "")
      )
      shiny::validate(
        need(nrow(df) != 0,
             "")
      )
      
      df$Fecha <- as.Date(df$Fecha, format="%d/%m/%Y")
      df$Fecha <- as.Date(df$Fecha, format="%Y/%m/%d")
      df$Mes <- format(as.Date(df$Fecha), "%Y-%m")  #Extracción de meses
      
      df <- recuento_estadistica_basica_2(df,1)  #Flag 1 para devolución recuento
      
      df <- df %>% 
        dplyr::group_by(`Forma Jurídica`,Mes) %>% 
        dplyr::ungroup() %>%
        tidyr::complete(`Forma Jurídica`, Mes, fill = list(Recuento = 0))
      
      df

    })
    
    # EB II
    # 5) Gráfico lineas AÑO A AÑO para estadística básica 2
    output$lineas_borme_anual_anterior_eb2 <- renderPlotly({
      
      df <- df_lineas_ano_a_ano_anterior()

      df8 <- datos_filtrados_borme()
      
      # Manejo de error
      shiny::validate(
        need(is.data.frame(df8),
             "")
      )
      shiny::validate(
        need(nrow(df) != 0,
             "")
      )
      
      # Manejo de error
      df9 <- estadistica_basica_2()
      # Manejo de error
      shiny::validate(
        need(is.data.frame(df9),
             "")
      )
      shiny::validate(
        need(nrow(df9) != 0,
             "")
      )
      
      df8$Fecha <- as.Date(df8$Fecha, format="%d/%m/%Y")
      df8$Fecha <- as.Date(df8$Fecha, format="%Y/%m/%d")
      df8$Mes <- format(as.Date(df8$Fecha), "%Y-%m")  #Extracción de meses
      
      df8 <- recuento_estadistica_basica_2(df8,1)  #Flag 1 para devolución recuento
      
      df8 <- df8 %>% 
        dplyr::group_by(`Forma Jurídica`,Mes) %>% 
        dplyr::ungroup() %>%
        tidyr::complete(`Forma Jurídica`, Mes, fill = list(Recuento = 0))

      # generación de nuvas formas jurídicas (forma + año) y paso de año-mes a mes
      df8$`Forma Jurídica` <- paste(df8$`Forma Jurídica`,substring(df8$Mes,1,4),sep = "-")
      df$`Forma Jurídica` <- paste(df$`Forma Jurídica`,substring(df$Mes,1,4),sep = "-")
      df8$Mes <- substring(df8$Mes,6,7)
      df$Mes <- substring(df$Mes,6,7)
      
      df <- rbind(df8,df)
      
      # Manejo de error: "inexistencia de datos para los filtros seleccionados" de cara al usuario
      shiny::validate(
        need(df != 0,
             "¡Atención!\nEs necesario selecionar un periodo de 2 meses para el cálculo de estadísticas mensuales.\nModifica el valor de los filtros si lo desea.")
      )
      
      # Manejo de error: "inexistencia de datos para los filtros seleccionados" de cara al usuario
      shiny::validate(
        need(df != 1,
             "¡Atención!\nNo existen datos disponibles para el valor de los filtros seleccionados.\nModifica el valor de los filtros si lo desea.")
      )
      
      #año <- year(input$fechas_listado_borme[1]) - 1
      año <- "comparativa"

      p <- df
      p <- p %>% plot_ly(x = ~Mes, y = ~Recuento, fill= ~`Forma Jurídica`, color = ~`Forma Jurídica`)
      p <- p %>% add_trace(type = 'scatter', mode = 'lines+markers',text = ~paste('Forma jurídica:', `Forma Jurídica`,'<br>Recuento:', Recuento))
      p <- p %>% layout(
        title = paste("<b>Recuento mensual ",input$Municipio_principal, " por forma jurídica (",año,")</b>",sep = "")
      )

      p
    })
    

    
    # Función detección atipicos
    detectar_atipicios <- function(x, na.rm = TRUE) {
      cuantiles <- quantile(x, probs=c(.25, .75), na.rm = na.rm)
      limite <- 1.5 * IQR(x, na.rm = na.rm)
      y <- x
      y[x < (cuantiles[1] - limite)] <- NA
      y[x > (cuantiles[2] + limite)] <- NA
      y
    }
    
    
    # APOYO A GRÁFICOS LÍNEA CAPITAL
    apoyo_graficos_capital <- reactive({
      
      df <- manejo_tablas_borme()
      
      shiny::validate(
        need(nrow(df) != 0 & is.data.frame(df),
             ""
        )
      )
      
      # Paso a númerico capital
      for(i in 2:7){
        df[,i] <- gsub("[a-z].","",df[,i])
        df[,i] <- gsub("[.]","",df[,i])
        df[,i] <- gsub("[.]","",df[,i])
        df[,i] <- as.numeric(gsub("[,]",".",df[,i]))
      }
      
      #Incluir desembolsado (es la alternativa a capital, a veces aparece así)
      for(i in 1:nrow(df)){
        if(is.na(df$`Ampliación capital`[i])){
          df$`Ampliación capital`[i] <- df$`Ampliación capital desembolsado`[i]
          df$`Ampliación capital suscrito`[i] <- df$`Ampliación capital resultante desembolsado`[i]
        }
      }
      
      # Cálculo evoluciones ampliaciones y reducciones de capital
      df <- df %>%
        mutate(`Evolución ampliación` = 100*((`Ampliación capital suscrito` - (`Ampliación capital suscrito` - `Ampliación capital`))/(`Ampliación capital suscrito` - `Ampliación capital`))) %>%
        mutate(`Evolución reducción` = 100*(((`Reducción capital resultante suscrito` + `Reducción capital importe reducción`)-`Reducción capital resultante suscrito`)/(`Reducción capital resultante suscrito` + `Reducción capital importe reducción`)))
      
      `Evolución ampliación` <- na.omit(df$`Evolución ampliación`)
      `Evolución reducción` <- na.omit(df$`Evolución reducción`)
      
      df$Fecha <- as.Date(df$Fecha, format="%d/%m/%Y")
      df$Fecha <- as.Date(df$Fecha, format="%Y/%m/%d")
      df$Mes <- format(as.Date(df$Fecha), "%Y-%m")  #Extracción de meses
      df <- df[!is.na(df$`Evolución ampliación`) | !is.na(df$`Evolución reducción`),]
      
      # Llamada a función para filtrado de atípicos
      df <- df %>%
        group_by(`Forma Jurídica`) %>% 
        mutate(`Evolución ampliación` = detectar_atipicios(`Evolución ampliación`)) %>%
        mutate(`Evolución reducción` = detectar_atipicios(`Evolución reducción`))
      
      # Cálculo medias de evolución y reducciones de capital
      df <- df %>%
        group_by(`Forma Jurídica`,Mes) %>%
        summarise(
          `Media evolución ampliación` = round(mean(`Evolución ampliación`, na.rm = TRUE),0),
          `Media evolución reducción` = round(mean(`Evolución reducción`, na.rm = TRUE),0)
        )
      
      colnames(df)[3] <- "Evolución ampliación"
      colnames(df)[4] <- "Evolución reducción"
      
      df <- df %>% 
        dplyr::group_by(`Forma Jurídica`,Mes) %>% 
        dplyr::ungroup() %>%
        tidyr::complete(`Forma Jurídica`, Mes, fill = list(`Evolución ampliación` = 0, `Evolución reducción` = 0))
      
      return(df)
    })
    
    # 3.1) Líneas borme CAPITAL AMPLIACIONES
    output$lineas_borme_capital_ampliaciones <- renderPlotly({
      
      df <- apoyo_graficos_capital()
      
      p <- df
      p <- p %>% plot_ly(x = ~Mes, y = df$`Evolución ampliación`, fill= ~`Forma Jurídica`, color = ~`Forma Jurídica`)
      p <- p %>% add_trace(type = 'scatter', mode = 'lines+markers')
      p <- p %>% layout(
        title = paste("<b>Evolución mensual ampliaciones de capital ",input$Municipio_principal, "</b>",sep = ""),
        yaxis = list(
          title = 'Evolució porcentual (%)'
        )
      )
      
      p
    })
    
    # 3.2) Líneas borme CAPITAL REDUCCIONES
    output$lineas_borme_capital_reducciones <- renderPlotly({
      
      df <- apoyo_graficos_capital()
      
      p <- df
      p <- p %>% plot_ly(x = ~Mes, y = df$`Evolución reducción`, fill= ~`Forma Jurídica`, color = ~`Forma Jurídica`)
      p <- p %>% add_trace(type = 'scatter', mode = 'lines+markers')
      p <- p %>% layout(
        title = paste("<b>Evolució mensual reducció de capital ",input$Municipio_principal, "</b>",sep = ""),
        yaxis = list(
          title = 'Evolució porcentual (%)'
        )
      )
      
      p
    })

    #==========================================================================
    # GENERACIÓN MAPAS
    #==========================================================================
    
    # Generación mapa Leaflet CAMBIO DOMICILIO SOCIAL BORME
    output$mapa_borme <- renderLeaflet({
      
      df_filtrados <- datos_filtrados_borme()
      
      shiny::validate(
        need(nrow(na.omit(df_filtrados)) != 0,
             "¡Atención!\nNo existen datos disponibles para el valor de los filtros seleccionados.\nModifica el valor de los filtros si lo desea."
        )
      )
      df_filtrados <- df_filtrados[as.numeric(df_filtrados$Latitud) > 41 & as.numeric(df_filtrados$Longitud) < -1.3,]
      
      
      if(input$variables_mapa == 1){
        variable <- "Constitución objeto social"
        pos <- 4
      }else{
        variable <- "Cambio domicilio social"
        pos <- 2
      }
      shiny::validate(
        need(any(grepl(variable,colnames(na.omit(df_filtrados)))) & nrow(na.omit(df_filtrados)) != 0,
             "¡Atención!\nNo existen datos disponibles para el valor de los filtros seleccionados.\nModifica el valor de los filtros si lo desea."
        )
      )
      
      # Filtrado por selección de registro en la tabla
      filtrado_tabla <- input$tabla_borme_mapa_rows_selected
      if(length(filtrado_tabla)){
        df_filtrados <-  df_filtrados[filtrado_tabla, , drop = F]
      }
      
      df_filtrados <- df_filtrados[!is.na(df_filtrados$Latitud),]
      
      #Inicialización popup
      empresas_popup <- df_filtrados$`Denominación social`
      domicilio_social_popup <- df_filtrados[,pos]
      domicilio_social_popup <- domicilio_social_popup
      popup <- paste("Denominación social: ", empresas_popup, "<br/>",
                     "Domicilio social: ", domicilio_social_popup, sep = "") %>% lapply(htmltools::HTML)

      #Lat y long a numérico
      latitud <- as.numeric(df_filtrados$Latitud)
      longitud <- as.numeric(df_filtrados$Longitud)
      
      #Creación mapa
      leaflet() %>% addTiles() %>% addMarkers(lng = longitud,
                                              lat = latitud,
                                              popup = popup)
    })


    #==========================================================================
    # DESCARGA DE DATOS
    #==========================================================================

    # DESCARGA DATOS BORME csv
    output$descarga_borme_csv <- downloadHandler(

        filename = function() {
            paste("Datos_borme_", as.character(input$fechas_listado_borme[1]),"_", as.character(input$fechas_listado_borme[2]),".csv", sep="")
        },
        content = function(file) {
          if(input$tabs_borme == "Listado informativo"){
            df_tabla <- manejo_tablas_borme()
            df <- df_tabla[,1:(ncol(df_tabla)-6)] #Evita la visualización de las variables lat,long,municipio y provincia.
          }else if(input$tabs_borme == "Estadística básica 1"){
            df <- estadistica_basica_1()
          }else if(input$tabs_borme == "Estadística básica 2"){
            df <- estadistica_basica_2()
            df <- recuento_estadistica_basica_2(df,2)  #Flag 2 para devolución con cálculos estadísticos
          }else if(input$tabs_borme == "Modificaciones capital"){
            df <- ayuda_borme_capital()
          }else{
            df <- datos_filtrados_borme() 
          }
          write.csv(df, file, eol="\n", sep = ",")  # eol="\n" es para el encoding de caracteres en .csv
        }
    )

    #Descarga contratos en XLSX
    output$descarga_borme_xlsx <- downloadHandler(

        filename = paste0("Datos_borme_",as.character(input$fechas_listado_borme[1]),"_", as.character(input$fechas_listado_borme[2]),".xlsx"),
        content  = function(file) {
          
          if(input$tabs_borme == "Listado informativo"){
            df_tabla <- manejo_tablas_borme()
            df <- df_tabla[,1:(ncol(df_tabla)-6)] #Evita la visualización de las variables lat,long,municipio y provincia.
          }else if(input$tabs_borme == "Estadística básica 1"){
            df <- estadistica_basica_1()
          }else if(input$tabs_borme == "Estadística básica 2"){
            df <- estadistica_basica_2()
            df <- recuento_estadistica_basica_2(df,2)  #Flag 2 para devolución con cálculos estadísticos
          }else if(input$tabs_borme == "Modificaciones capital"){
            df <- ayuda_borme_capital()
          }else{
            df <- datos_filtrados_borme() #Evita la visualización de las variables lat,long,municipio y provincia.
          }
          
          wb <- openxlsx::createWorkbook()
          openxlsx::addWorksheet(wb, sheetName = "Datos")
          openxlsx::writeData(wb, sheet = 1, x = df)
          openxlsx::saveWorkbook(wb, file)
        }
    )
    
    #============================
    # TÍTULOS TABLAS
    #============================
    
    output$texto_tabla_borme_listado <- renderUI(
      tags$div(id = "1",tags$h4(tags$b(paste("Listado informativo ", input$Municipio_principal, sep = ""))))
    )
    output$texto_tabla_borme_eb1 <- renderUI(
      tags$div(id = "1",tags$h4(tags$b(paste("Recuento ", input$Municipio_principal, " y comparativa respecto ",
                                             ifelse(input$comparaciones == "Otro municipio",input$Municipio_comparaciones, input$comparaciones)
                                             , sep = ""))))
    )
    output$texto_tabla_borme_eb2 <- renderUI(
      tags$div(id = "1",tags$h4(tags$b(paste("Estadística descriptiva mensual ", input$Municipio_principal, sep = ""))))
    )
    output$texto_tabla_borme_capital <- renderUI(
      tags$div(id = "1",tags$h4(tags$b(paste("Media de evoluciones modificacions de capital ", input$Municipio_principal, sep = ""))))
    )
    output$texto_borme_mapa <- renderUI(
      tags$div(id = "1",tags$h4(tags$b(paste("Información geolocalizada ", ifelse(input$variables_mapa == 1,"Constituciones ", "Cambios de domicilio social "), input$Municipio_principal,sep = ""))))
    )
    output$texto_tabla_borme_mapa <- renderUI(
      tags$div(id = "1",tags$h4(tags$b(paste("Listado informativo ", ifelse(input$variables_mapa == 1,"Constituciones ", "Cambios de domicilio social "), input$Municipio_principal,sep = ""))))
    )
    
    
    
    
    # =================================================================
    # =================================================================
    # =================================================================
    # CENSO DE EMRPESAS
    # =================================================================
    # =================================================================
    # =================================================================
    
    datos_filtrado <- reactive({
      
      df <- df_censo
      
      # 1) Filtro por intervalo de fechas
      df <- df[as.Date(as.character(df$`Fecha_constitución`), "%d/%m/%Y") >= as.Date(input$fechas[1]) &
                 as.Date(as.character(df$`Fecha_constitución`)) <= as.Date(input$fechas[2]) |
                 df$`Fecha_constitución` == "-",]

      # 2) Filtro por num empleados
      # Lógica selección empleados == 2- cuando input empleados == 0
      if(input$empleados[1] == 0){
        inicial_0 <- "-"
        df_sin_info_empleados <- df[df$Empleados == inicial_0,] 
        df <- df[as.numeric(gsub("[ (].*","",df$Empleados)) >= input$empleados[1] & as.numeric(gsub("[ (].*","",df$Empleados)) <= input$empleados[2],]
        df <- rbind(df,df_sin_info_empleados)
      }else{
        df <- df[as.numeric(gsub("[ (].*","",df$Empleados)) >= input$empleados[1] & as.numeric(gsub("[ (].*","",df$Empleados)) <= input$empleados[2],]
      }

      # 3) Filtro por división CNAE
      # Manejo de error: "inexistencia de datos para los filtros seleccionados" de cara al usuario
      shiny::validate(
        need(!is.null(input$div_cnae),
             "Atención!\nNo se ha seleccionado ningún CNAE.\nSeleccione un CNAE por favor.")
      )
      
      if(input$div_cnae == "Todos"){  
        df <- df
      }else if(length(input$div_cnae) > 1){
        codigos_a_extraer <- c()
        for(i in 1:length(input$div_cnae)){
          if(grepl("[A-Z]",substring(input$div_cnae[i],1,1))){
            
            pos_letra_demandada <- grep(substring(input$div_cnae[i],1,1),letters,ignore.case = TRUE)
            codigos_2 <- gsub("([0-9]+).*$", "\\1",
                                      df_cnae$completo[c((grep(letters[pos_letra_demandada],df_cnae$COD_CNAE2009,ignore.case = TRUE) + 1):
                                                           (grep(letters[pos_letra_demandada + 1],df_cnae$COD_CNAE2009,ignore.case = TRUE) - 1)
                                      )]
            )
            #codigos_2 <-  codigos_2[nchar(codigos_2) > 3]
          }else{
            codigo_seleccionado <- gsub("([0-9]+).*$", "\\1", input$div_cnae[i])
            numero_de_carct <- nchar(codigo_seleccionado)
            codigos_2 <- gsub("([0-9]+).*$", "\\1",
                                      df_cnae$completo[grep(codigo_seleccionado,substring(df_cnae$COD_CNAE2009,1,numero_de_carct))]
            )
            #codigos_a_extraer <- codigos_a_extraer[nchar(codigos_a_extraer) > 3]
            #codigos_2 <- codigos_2[nchar(codigos_2) > 3]
          }
          codigos_a_extraer <- c(codigos_a_extraer, codigos_2)
        }
        df <- df[which(gsub("([0-9]+).*$", "\\1",df$CNAE) %in% codigos_a_extraer),]

      }else{
        if(grepl("[A-Z]",substring(input$div_cnae,1,1))){
          
          pos_letra_demandada <- grep(substring(input$div_cnae,1,1),letters,ignore.case = TRUE)
          codigos_a_extraer <- gsub("([0-9]+).*$", "\\1",
                                    df_cnae$completo[c((grep(letters[pos_letra_demandada],df_cnae$COD_CNAE2009,ignore.case = TRUE) + 1):
                                                         (grep(letters[pos_letra_demandada + 1],df_cnae$COD_CNAE2009,ignore.case = TRUE) - 1)
                                    )]
          )
          #codigos_a_extraer <- codigos_a_extraer[nchar(codigos_a_extraer) > 3]
        }else{
          codigo_seleccionado <- gsub("([0-9]+).*$", "\\1", input$div_cnae)
          numero_de_carct <- nchar(codigo_seleccionado)
          codigos_a_extraer <- gsub("([0-9]+).*$", "\\1",
                                    df_cnae$completo[grep(codigo_seleccionado,substring(df_cnae$COD_CNAE2009,1,numero_de_carct))]
          )
          #codigos_a_extraer <- codigos_a_extraer[nchar(codigos_a_extraer) > 3]
        }
        df <- df[which(gsub("([0-9]+).*$", "\\1",df$CNAE) %in% codigos_a_extraer),]
      }
      
      # 4) Filtro por ubicación
      if(input$calle == "Todas"){
        df <- df
      }else{
        df <- df[grep(input$calle,gsub(",.*","",df$`Domicilio_social`)),]
      }
      
      # 5) Filtro extinguidas
      if(input$extinguidas == TRUE){
        df <- df[df$Estado == "Activa",]
      }else{
        df <- df
      }
      
      df[df == ""] <- "-"
      
      # 6) Filtrad por existencia RRSS
      if(input$RRSS == 1){
        df <- df[df$RRSS != "-" ,]
      }else if(input$RRSS == 2){
        df <- df[df$RRSS == "-",]
      }else{
        df <- df
      }
      
      # 7) Filtrad por palabra clave
      if(input$palabra_clave != ""){
        #Filtrado por búsqueda de palabras clave. Se realiza máscara OR con resultados booleanos.
        filtrado_palabra_clave <- apply(as.data.frame(mapply(grepl, input$palabra_clave, df, ignore.case = T)),1,any)
        filtrado_palabra_clave <- apply(as.data.frame(mapply(grepl, input$palabra_clave, df[,c(1:4)], ignore.case = T)),1,any)
        df <- subset(df, filtrado_palabra_clave)
      }else{
        df <- df
      }
      
      df <- df[,c(1,2,5,6,7,8,10,9,21,11,12,20,23,19,18,16,15,17,13,14,3,4)]
      
      df
      
    })
    
    datos_filtrado_mapa <- reactive({
      df <- datos_filtrado()
      
      # Filtrado por selección de registro en la tabla
      filtrado_tabla <- input$tabla_rows_selected
      if(length(filtrado_tabla)){
        df <-  df[filtrado_tabla, , drop = F]
      }
      
      return(df)
    })
    
    
    # MAPA
    output$mapa <- renderLeaflet({
      
      df <- datos_filtrado_mapa()
      
      df <- df[as.numeric(df$Latitud) > 41,]
      
      latitud <- as.numeric(df$Latitud)
      longitud <- as.numeric(df$Longitud)
      
      popup <- paste(
        "Empresa: ", df$`Denominación_social`,"<br/>", 
        "CNAE: ", df$CNAE, "<br/>", 
        "Empleados ", df$Tamaño_empresa_por_empleados, "<br/>",
        "Facturación: ", df$Tamaño_empresa_por_facturación, "<br/>",
        sep="") %>%
        lapply(htmltools::HTML)
      
      #leaflet() %>% addProviderTiles(providers$CartoDB.Positron) %>% addMarkers(lng = longitud, lat = latitud, popup = popup)
      leaflet() %>% addTiles() %>% addMarkers(lng = longitud, lat = latitud, popup = popup)
    })
    
    # TABLA
    output$tabla <- renderDataTable({
      
      df <- datos_filtrado()
      
      # Manejo de error: "inexistencia de datos para los filtros seleccionados" de cara al usuario
      shiny::validate(
        need(nrow(df) != 0,
             "Atención!\nNo existen datos disponibles para el valor de los filtros seleccionados.\nModifique el valor de los filtros si lo desea.")
      )

      # Links URL y RRSS
      df$URL <- paste0("<a href='", df$URL,"' target='_blank'>", df$URL,"</a>")
      #df$RRSS <- paste0("<a href='", df$RRSS,"' target='_blank'>", df$RRSS,"</a>")
      
      pos_urls <- grep("https",df$RRSS)
      for(i in pos_urls){
        rrss_separadas <- str_split(df$RRSS[i], ",")[[1]]
        rrss_separadas <- str_trim(rrss_separadas)
        rrss_separadas <- unique(rrss_separadas)
        for(j in 1:length(rrss_separadas)){
          rrss_separadas[j] <- paste0("<a href='", rrss_separadas[j],"' target='_blank'>", rrss_separadas[j],"</a>")
        }
        df$RRSS[i] <- paste(rrss_separadas, collapse = ", ")
      }
      
      # Manejo de error: "inexistencia de datos para los filtros seleccionados" de cara al usuario
      shiny::validate(
        need(nrow(df) != 0,
             "Atención!\nNo existen datos disponibles para el valor de los filtros seleccionados.\nModifique el valor de los filtros si lo desea.")
      )
      
      df <- datatable(df, options = list(pageLength = 5,
                                         columnDefs = list(list(className = 'dt-center', targets = "_all")),
                                         scrollX=TRUE, 
                                         scrollCollapse=TRUE),escape = F)
      
    })
    
    #Descarga de datos
    output$downloadDatacenso_csv <- downloadHandler(
      
      filename = paste0("Censo_filtrado","_",Sys.Date(),".csv"),
      content  = function(file) {
        df <- datos_filtrado_mapa()
        
        write.csv(df, file, eol="\n", sep = ",")
      })
    
    
    
    #=============================================
    # CENSO BODEGAS COMARCA ARANDA DE DUERO
    #=============================================
    
    datos_filtrado_bodegas <- reactive({
      
      df <- df_censo_bodegas
      
      # 1) Filtro por intervalo de fechas
      df <- df[as.Date(as.character(df$`Fecha_constitución`), "%d/%m/%Y") >= as.Date(input$fechas[1]) &
                 as.Date(as.character(df$`Fecha_constitución`)) <= as.Date(input$fechas[2]) |
                 df$`Fecha_constitución` == "-",]
      
      # Manejo de error: "inexistencia de datos para los filtros seleccionados" de cara al usuario
      shiny::validate(
        need(nrow(df) != 0,
             "Atención!\nNo existen datos disponibles para el valor de los filtros seleccionados.\nModifique el valor de los filtros si lo desea.")
      )
      
      # 2) Filtro por num empleados
      # Lógica selección empleados == 2- cuando input empleados == 0
      if(input$empleados[1] == 0){
        print("entro")
        inicial_0 <- "-"
        df_sin_info_empleados <- df[df$Empleados == inicial_0,] 
        df <- df[as.numeric(gsub("[ (].*","",df$Empleados)) >= input$empleados[1] & as.numeric(gsub("[ (].*","",df$Empleados)) <= input$empleados[2],]
        df <- na.omit(df)
        df <- rbind(df,df_sin_info_empleados)
      }else{
        df <- df[as.numeric(gsub("[ (].*","",df$Empleados)) >= input$empleados[1] & as.numeric(gsub("[ (].*","",df$Empleados)) <= input$empleados[2],]
      }
      
      # Manejo de error: "inexistencia de datos para los filtros seleccionados" de cara al usuario
      shiny::validate(
        need(nrow(df) != 0 & !is.null(input$municipio_bodegas),
             "Atención!\nNo existen datos disponibles para el valor de los filtros seleccionados.\nModifique el valor de los filtros si lo desea.")
      )
      
      # 4) Filtro por municipio
      
      if(input$municipio_bodegas == "Todos"){
        df <- df
      }else{
        df <- df[grep(input$municipio_bodegas,df$Municipio),]
      }
      
      # Manejo de error: "inexistencia de datos para los filtros seleccionados" de cara al usuario
      shiny::validate(
        need(nrow(df) != 0,
             "Atención!\nNo existen datos disponibles para el valor de los filtros seleccionados.\nModifique el valor de los filtros si lo desea.")
      )
      
      df[df == ""] <- "-"
      
      # 6) Filtrad por existencia RRSS
      if(input$RRSS == 1){
        df <- df[df$RRSS != "-" ,]
      }else if(input$RRSS == 2){
        df <- df[df$RRSS == "-",]
      }else{
        df <- df
      }
      
      # Manejo de error: "inexistencia de datos para los filtros seleccionados" de cara al usuario
      shiny::validate(
        need(nrow(df) != 0,
             "Atención!\nNo existen datos disponibles para el valor de los filtros seleccionados.\nModifique el valor de los filtros si lo desea.")
      )
      
      # 7) Filtrad por palabra clave
      if(input$palabra_clave != ""){
        #Filtrado por búsqueda de palabras clave. Se realiza máscara OR con resultados booleanos.
        filtrado_palabra_clave <- apply(as.data.frame(mapply(grepl, input$palabra_clave, df, ignore.case = T)),1,any)
        filtrado_palabra_clave <- apply(as.data.frame(mapply(grepl, input$palabra_clave, df[,c(1:4)], ignore.case = T)),1,any)
        df <- subset(df, filtrado_palabra_clave)
      }else{
        df <- df
      }
      
      # Manejo de error: "inexistencia de datos para los filtros seleccionados" de cara al usuario
      shiny::validate(
        need(nrow(df) != 0,
             "Atención!\nNo existen datos disponibles para el valor de los filtros seleccionados.\nModifique el valor de los filtros si lo desea.")
      )
      
      df <- df[,c(1,2,5,6,7,8,10,9,21,11,12,20,23,19,18,16,15,17,13,14,3,4)]
      
      df
      
    })
    
    datos_filtrado_mapa_bodegas <- reactive({
      df <- datos_filtrado_bodegas()
      
      # Filtrado por selección de registro en la tabla
      filtrado_tabla <- input$tabla_rows_selected
      if(length(filtrado_tabla)){
        df <-  df[filtrado_tabla, , drop = F]
      }
      
      return(df)
    })
    
    
    # MAPA
    output$mapa_bodegas <- renderLeaflet({
      
      df <- datos_filtrado_mapa_bodegas()
      
      df <- df[as.numeric(df$Latitud) > 41,]
      
      latitud <- as.numeric(df$Latitud)
      longitud <- as.numeric(df$Longitud)
      
      popup <- paste(
        "Empresa: ", df$`Denominación_social`,"<br/>", 
        "CNAE: ", df$CNAE, "<br/>", 
        "Empleados ", df$Tamaño_empresa_por_empleados, "<br/>",
        "Facturación: ", df$Tamaño_empresa_por_facturación, "<br/>",
        sep="") %>%
        lapply(htmltools::HTML)
      
      #leaflet() %>% addProviderTiles(providers$CartoDB.Positron) %>% addMarkers(lng = longitud, lat = latitud, popup = popup)
      leaflet() %>% addTiles() %>% addMarkers(lng = longitud, lat = latitud, popup = popup)
    })
    
    # TABLA
    output$tabla_bodegas <- renderDataTable({
      
      df <- datos_filtrado_bodegas()
      
      # Manejo de error: "inexistencia de datos para los filtros seleccionados" de cara al usuario
      shiny::validate(
        need(nrow(df) != 0,
             "Atención!\nNo existen datos disponibles para el valor de los filtros seleccionados.\nModifique el valor de los filtros si lo desea.")
      )
      
      # Links URL y RRSS
      df$URL <- paste0("<a href='", df$URL,"' target='_blank'>", df$URL,"</a>")
      #df$RRSS <- paste0("<a href='", df$RRSS,"' target='_blank'>", df$RRSS,"</a>")
      
      pos_urls <- grep("https",df$RRSS)
      for(i in pos_urls){
        rrss_separadas <- str_split(df$RRSS[i], ",")[[1]]
        rrss_separadas <- str_trim(rrss_separadas)
        rrss_separadas <- unique(rrss_separadas)
        for(j in 1:length(rrss_separadas)){
          rrss_separadas[j] <- paste0("<a href='", rrss_separadas[j],"' target='_blank'>", rrss_separadas[j],"</a>")
        }
        df$RRSS[i] <- paste(rrss_separadas, collapse = ", ")
      }
      
      # Manejo de error: "inexistencia de datos para los filtros seleccionados" de cara al usuario
      shiny::validate(
        need(nrow(df) != 0,
             "Atención!\nNo existen datos disponibles para el valor de los filtros seleccionados.\nModifique el valor de los filtros si lo desea.")
      )
      
      df <- datatable(df, options = list(pageLength = 5,
                                         columnDefs = list(list(className = 'dt-center', targets = "_all")),
                                         scrollX=TRUE, 
                                         scrollCollapse=TRUE),escape = F)
      
    })
    
    
    
    
    
    
    # =================================================================
    # =================================================================
    # =================================================================
    # ESTABLECIMIENTOS
    # =================================================================
    # =================================================================
    # =================================================================
    
    datos_filtrado_establecimientos <- reactive({
      
      df <- datos_estblecimientos_actual$establecimientos[datos_estblecimientos_actual$establecimientos$Fecha == max(datos_estblecimientos_actual$establecimientos$Fecha),]
      df[is.na(df)] <- "-"

      shiny::validate(
        need(!is.null(input$categoria),
             "Atención!\nNo se ha seleccionado ninguna categoría.\nSeleccione una categoría por favor.")
      )

      # 1) Filtro por categoria
      if(input$categoria == "Todos"){
        df <- df
      }else{
        df <- df[which(df$Categoría %in% input$categoria),]
      }
      
      # 2) Filtro por número de REVIEWS
      df <- df[df$Reviews >= input$reviews[1] & df$Reviews <= input$reviews[2],]

      # 3) Filtro por valoración
      df <- df[df$Valoración >= input$valoracion[1] & df$Valoración <= input$valoracion[2],]
      
      # 4) Filtro web
      if(as.numeric(input$web) == 1){
        df <- df[df$Web != "-" ,]
      }else if(as.numeric(input$web) == 2){
        df <- df[df$Web == "-",]
      }else{
        df <- df
      }
      
      # 5) Filtro estado
      if(as.numeric(input$cerradas) == 1){
        df <- df[df$`Cierre temporal` == "-" ,]
      }else if(as.numeric(input$cerradas) == 2){
        df <- df[df$`Cierre temporal` != "-",]
      }else{
        df <- df
      }

    })
    
    datos_filtrado_mapa_establecimientos <- reactive({
      df <- datos_filtrado_establecimientos()
      
      # Filtrado por selección de registro en la tabla
      filtrado_tabla <- input$tabla_establecimientos_rows_selected
      if(length(filtrado_tabla)){
        df <-  df[filtrado_tabla, , drop = F]
      }
      
      return(df)
    })
    
    # Preparacion datos para evolucion temporal
    datos_filtados_evolucion_temporal <- reactive({
      if(datos_estblecimientos_temporal$flag == 1){
        df <- datos_estblecimientos_temporal$establecimientos
      }else{
        df <- datos_estblecimientos_actual$establecimientos
      }
      
      df[is.na(df)] <- "-"
      
      shiny::validate(
        need(!is.null(input$categoria),
             "Atención!\nNo se ha seleccionado ninguna categoría.\nSeleccione una categoría por favor.")
      )
      
      # 1) Filtro por categoria
      if(input$categoria == "Todos"){
        df <- df
      }else{
        df <- df[which(df$Categoría %in% input$categoria),]
      }
      
      # 2) Filtro por número de REVIEWS
      df <- df[df$Reviews >= input$reviews[1] & df$Reviews <= input$reviews[2],]
      
      # 3) Filtro por valoración
      df <- df[df$Valoración >= input$valoracion[1] & df$Valoración <= input$valoracion[2],]
      
      # 4) Filtro web
      if(as.numeric(input$web) == 1){
        df <- df[df$Web != "-" ,]
      }else if(as.numeric(input$web) == 2){
        df <- df[df$Web == "-",]
      }else{
        df <- df
      }
      
      # 5) Filtro estado
      if(as.numeric(input$cerradas) == 1){
        df <- df[df$`Cierre temporal` == "-" ,]
      }else if(as.numeric(input$cerradas) == 2){
        df <- df[df$`Cierre temporal` != "-",]
      }else{
        df <- df
      }
      
    })
    
    
    #==============================
    # GRAFICOS EVOLUCION TEMPORAL
    
    # 1) Recuento por cartegoria
    output$evolucion_categorias <- renderPlotly({
      df <- datos_filtados_evolucion_temporal()
      
      df <- df %>% 
        dplyr::group_by(Categoría,Fecha) %>%
        dplyr::summarise(Recuento = n())
      
      p <- df
      p <- p %>% plot_ly(x = ~Fecha, y = ~Recuento, fill= ~Categoría, color = ~Categoría)
      p <- p %>% add_trace(type = 'scatter', mode = 'lines+markers')
      p <- p %>% layout(
        title = paste("<b>Evolución temporal por categoría</b>",sep = ""),
        yaxis = list(range = c(0,(5+max(df$Recuento))))
      )
      
      p
        
        
    })
    
    
    # 2) Recuento cerrados temporalmente
    output$evolucion_cerrados <- renderPlotly({
      df <- datos_filtados_evolucion_temporal()
      
      df <- df %>% 
        dplyr::group_by(`Cierre temporal`,Fecha) %>%
        dplyr::summarise(Recuento = n()) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(Representación=Recuento/sum(Recuento)*100)

      df <- df[df$`Cierre temporal` != "-",]
      
      shiny::validate(
        need(nrow(df) != 0,
             "Atención!\nNo existen datos disponibles para el valor de los filtros seleccionados.\nModifique el valor de los filtros si lo desea.")
      )
      
      df$Representación <- round(df$Representación,2)
      
      p <- df
      p <- p %>% plot_ly(x = ~Fecha, y = ~Recuento)
      p <- p %>% add_trace(type = 'scatter', mode = 'lines+markers',text = ~paste('Representación:', Representación))
      p <- p %>% layout(
        title = paste("<b>Evolución temporal del estado de los establecimientos</b>",sep = ""),
        yaxis = list(range = c(0,(5+max(df$Recuento))))
      )
      
      p
      
    })
    
    # NUBE PALABRAS TOPICS
    output$nube_topics <- renderPlot({
      
      df <- datos_filtados_evolucion_temporal()
      
      topics <- df$Topics[!is.na(df$Topics)]
      topics <- str_trim(unlist(strsplit(topics, ",")))
      topics <- Corpus(VectorSource(topics))
      #topics_recuento <- table(topics)
      
      dtm <- TermDocumentMatrix(topics)
      m <- as.matrix(dtm)
      v <- sort(rowSums(m),decreasing=TRUE)
      d <- data.frame(word = names(v),freq=v)
      
      # Manejo de error: "inexistencia de datos para los filtros seleccionados" de cara al usuario
      shiny::validate(
        need(nrow(d) != 0,
             "Atención!\nNo existen datos disponibles para el valor de los filtros seleccionados.\nModifique el valor de los filtros si lo desea.")
      )
      
      w <- wordcloud(words = d$word, freq = d$freq, min.freq = 1,
                max.words=100, scale=c(8,0.5),
                colors=brewer.pal(8, "Dark2"))
      
      w
      
    })
    
    
    # MAPA
    output$mapa_establecimientos <- renderLeaflet({
      
      df <- datos_filtrado_mapa_establecimientos()
      
      #df <- df[as.numeric(df$Latitud) > 41,]
      
      latitud <- as.numeric(df$Latitud)
      longitud <- as.numeric(df$Longitud)
      
      popup <- paste(
        "Establecimiento: ", df$Establecimiento,"<br/>", 
        "Categoría: ", df$Categoría, "<br/>",
        "Valoración: ", df$Valoración, "<br/>",
        "Topics: ", df$Topics, "<br/>",
        sep="") %>%
        lapply(htmltools::HTML)
      
      #leaflet() %>% addProviderTiles(providers$CartoDB.Positron) %>% addMarkers(lng = longitud, lat = latitud, popup = popup)
      leaflet() %>% addTiles() %>% addMarkers(lng = longitud, lat = latitud, popup = popup)
    })
    
    # TABLA
    output$tabla_establecimientos <- renderDataTable({
      
      df <- datos_filtrado_establecimientos()
      
      # Manejo de error: "inexistencia de datos para los filtros seleccionados" de cara al usuario
      shiny::validate(
        need(nrow(df) != 0,
             "Atención!\nNo existen datos disponibles para el valor de los filtros seleccionados.\nModifique el valor de los filtros si lo desea.")
      )
      
      df$Web[df$Web != "-"] <- paste("https://www.",df$Web, sep= "")
      
      # Links URL y RRSS
      df$Web <- paste0("<a href='", df$Web,"' target='_blank'>", df$Web,"</a>")
      df$`URL Google Maps` <- paste0("<a href='", df$`URL Google Maps`,"' target='_blank'>", "Google Maps","</a>")

      
      # Manejo de error: "inexistencia de datos para los filtros seleccionados" de cara al usuario
      shiny::validate(
        need(nrow(df) != 0,
             "Atención!\nNo existen datos disponibles para el valor de los filtros seleccionados.\nModifique el valor de los filtros si lo desea.")
      )
      
      df <- df[,c(1:13,15,14)]
      
      df <- datatable(df, options = list(pageLength = 5,
                                         columnDefs = list(list(className = 'dt-center', targets = "_all")),
                                         scrollX=TRUE, 
                                         scrollCollapse=TRUE),escape = F)
      
    })
    
    #Descarga de datos
    output$downloadDatacenso_csv <- downloadHandler(
      
      filename = paste0("Censo_filtrado","_",Sys.Date(),".csv"),
      content  = function(file) {
        df <-  datos_filtrado_establecimientos()
        
        write.csv(df, file, eol="\n", sep = ",")
      })
    
    
    #==============================================================
    # TABLA IO CYL
    #==============================================================
    
    filtros_tio <- reactive({
      df <- TIO
      
      sectores_afectados <- df$Sector[df$Sector == input$sector]
      flujos_a_sectores  <- df$`CONSUMO hogares`[grep(input$sector,df$Sector)]

      relaciones_totales <- data.frame(origen="consumo_HOGAR",destino=sectores_afectados,flujo=flujos_a_sectores)
      
      # Crear relaciones en un data.frame
      
      for(j in 1:length(sectores_afectados))
      {
        sector_1 <- grep(sectores_afectados[j],colnames(df))
        df <- df[order(df[,sector_1],decreasing = TRUE),]
        sectores_afectados_1 <- df$Sector[1:input$grupos]
        flujos_a_sectores_1  <- df[1:input$grupos,sector_1]
        relaciones_1 <- data.frame(origen=sectores_afectados[j],destino=sectores_afectados_1,flujo=flujos_a_sectores_1)
        relaciones_totales <- rbind(relaciones_totales,relaciones_1)
        
        for(k in 1:length(sectores_afectados_1))
        {
          sector_2 <- grep(sectores_afectados_1[k],colnames(df))
          df <- df[order(df[,sector_2],decreasing = TRUE),]
          sectores_afectados_2 <- df$Sector[1:input$grupos]
          flujos_a_sectores_2  <- df[1:input$grupos,sector_2]
          relaciones_2 <- data.frame(origen=sectores_afectados_1[k],destino=sectores_afectados_2,flujo=flujos_a_sectores_2)
          relaciones_totales <- rbind(relaciones_totales,relaciones_2)
        }
        
        if(input$inducido==TRUE)
        {
          for(m in 1:length(sectores_afectados_2))
          {
            sector_3 <- grep(sectores_afectados_2[m],colnames(df))
            df <- df[order(df[,sector_3],decreasing = TRUE),]
            sectores_afectados_3 <- df$Sector[1:input$grupos]
            flujos_a_sectores_3  <- df[1:input$grupos,sector_3]
            relaciones_3 <- data.frame(origen=sectores_afectados_2[m],destino=sectores_afectados_3,flujo=flujos_a_sectores_3)
            relaciones_totales <- rbind(relaciones_totales,relaciones_3)
          }
        }
      }
      
      relaciones_totales <- relaciones_totales[relaciones_totales$flujo>=1,]
      relaciones_totales <- relaciones_totales %>% unique()
      
    })
    
    output$graf_tio <- renderPlot({
      
      df <- filtros_tio()
      
      vertices <- c(unique(as.character(df$origen)),unique(as.character(df$destino))) %>% unique()
      
      grafo <- graph_from_data_frame(df,directed=TRUE,vertices)
      
      # Page rank
      
      ranking <- page_rank(grafo, directed=TRUE, weights = E(grafo)$ventas,damping=0.85)
      ranking <- ranking$vector[order(as.numeric(ranking$vector),decreasing=TRUE)] %>% head(10)
      ranking <- ranking %>% as.data.frame()
      ranking$sector <- rownames(ranking)
      ranking
      
      # Centralidad Eigen
      
      centralidad_eigen <- centr_eigen(grafo,directed=TRUE)
      centralidad_eigen <- centralidad_eigen$vector %>% as.data.frame()
      centralidad_eigen$sector <- vertices
      centralidad_eigen <- centralidad_eigen[order(centralidad_eigen$.,decreasing = TRUE),]
      centralidad_eigen <- head(centralidad_eigen,10)
      centralidad_eigen
      
      #INTERMEDIACION
      #La intermediación («betweenness centrality») es una medida que cuantifica 
      #la frecuencia o el número de veces que un nodo actúa como un puente a lo largo 
      #del camino más corto entre otros dos nodos.
      #Los nodos que poseen una posición de intermediarios de alguna manera son 
      #también controladores o reguladores del flujo y suelen jugar un rol crítico 
      #en la estructura de la red, cuando hay grandes flujos que son transportados 
      #por nodos pertenecientes a grupos compactos
      
      betweenness_grafo <- betweenness(grafo,weights = E(grafo)$flujo)
      betweenness_grafo <- betweenness_grafo[order(-betweenness_grafo)]
      betweenness_grafo <- head(betweenness_grafo,15)
      betweenness_grafo
      
      #clusters
      
      #sgc <- spinglass.community(grafo, weights = E(grafo)$flujo, spins=25)
      #membership(sgc)
      #clusters <- data.frame(sgc$membership,vertices)
      
      sgc_2 <- cluster_spinglass(grafo, weights = E(grafo)$flujo,spins=25)
      clusters_2 <- data.frame(sgc_2$membership,vertices)
      
      #plot(grafo,vertex.size=0.8,edge.width=0.1,edge.arrow.size=0.02,label.cex=12)
      
      l <- layout_with_kk(grafo)
      
      g <- plot(sgc_2,grafo,
           layout=l,
           vertex.size=20,
           vertex.label.family = "Verdana",
           vertex.label.cex = 1,
           vertex.label.color = "Black",
           edge.arrow.size=1,
           edge.label=paste(E(grafo)$flujo,"%"),
           #edge.width=E(grafo)$flujo/10,
           edge.label.family = "Verdana",
           edge.label.cex = 1,
           edge.label.color = "Black",
           edge.curved=curve_multiple(grafo))
      
      title(paste("Nodo atractor:",ranking[1,2]),cex.main=1,col.main="Black")
      
      g
      
    })
}

# Run the application
shinyApp(ui = ui, server = server)
