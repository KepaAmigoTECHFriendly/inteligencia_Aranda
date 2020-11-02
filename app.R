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


library(RPostgres)
library(DBI)

db          <- 'datawarehouse'
host_db     <- '82.223.243.42'
db_port     <- '5432'
db_user     <- 'postgres'
db_password <- 'postgressysadmin_2019'

con <- dbConnect(RPostgres::Postgres(), dbname = db, host=host_db, port=db_port, user=db_user, password=db_password) 


# ESTABLECIMIENTOS
df_establecimientos <- read.csv("tabla_total.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE, encoding = "UTF-8", dec = ",")
colnames(df_establecimientos) <- c("Establecimiento","Valoración","Reviews","Categoría","Latitud","Longitud","Dirección","Código","Web","Teléfono","Regentada_mujeres","Horarios","Cierre temporal","URL Google Maps")
df_establecimientos <- df_establecimientos[grep("aranda de duero",tolower(df_establecimientos$Código)),]

# TOPICS
df_topics <- read.csv("topics_total.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE, encoding = "UTF-8", dec = ",")
df_topics <- df_topics[,1:(ncol(df_topics)-1)]
colnames(df_topics) <- c("Establecimiento","Valoración","Reviews","Categoría","Latitud","Longitud","Código","URL Google Maps","Topics")
df_topics <- df_topics[grep("aranda de duero",tolower(df_topics$Código)),]
df_topics <- df_topics %>% 
  group_by(Establecimiento) %>%
  mutate(Topics = paste(Topics, collapse = ", "))

df_establecimientos$Topics <- df_topics$Topics[match(df_establecimientos$Establecimiento, df_topics$Establecimiento)]




# CENSO
df_censo <- read.csv("censo_aranda_duero.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE, encoding = "latin1", dec = ",")

# REFERENCIA CNAES
df_cnae <- read.csv("referencia_CNAEs.csv", header = TRUE, sep = ";")
df_cnae <- df_cnae[,c(1,3)]
df_cnae$completo <- paste(df_cnae[,1],df_cnae[,2], sep = " ")

# MUNICIPIOS
# ------------------------
municipios <- read.xlsx(xlsxFile = "Municipios.xlsx", sheet = 1, skipEmptyRows = TRUE)
municipios <- municipios[3:nrow(municipios),]
colnames(municipios) <- c("Código provincia", "Código municipio", "DC","Municipio")
mun <- municipios$Municipio

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


#=====================================================
# INTERFAZ DE USUARIO
#=====================================================
ui <- fluidPage(style = "width: 100%; height: 100%;",
                
                #use_busy_spinner(spin = "fading-circle"),
                
                # Inicialización shinyjs
                useShinyjs(),
                useShinyalert(),
                withMathJax(),
                
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
                                        radioButtons(inputId="comparaciones",label="Seleccione el territorio de comparación",choices=c("Burgos provincia", "Otro municipio"),
                                                     selected = "Burgos provincia"),
                                        selectInput("Municipio_comparaciones", "Seleccione el municipio de comparación",
                                                    municipios$Municipio),
                                        
                                        dateRangeInput("fechas_listado_borme","Seleccione el intervalo de fechas",start = "2020-01-01", end = Sys.Date()),
                                        
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
                                                                      plotlyOutput("barras_borme_eb1", height = 400)
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
                                                               column(width = 9,
                                                                      plotlyOutput("lineas_borme_eb2", height = 400)
                                                               )
                                                               
                                                             ),
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
                                        selectInput("empleados", "Filtro por rango de empleados",
                                                    c("Todos",unique(df_censo$Tamaño_empresa_por_empleados)[c(1,2,3,6,4,5)])
                                        ),
                                        selectInput("div_cnae", "Filtro por CNAE",
                                                    #c("Todos",substring(unique(df_censo$CNAE),1,2)[order(substring(unique(df_censo$CNAE),1,2))][-1])
                                                    c("Todos",df_cnae$completo)
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
                                        fluidRow(
                                          leafletOutput("mapa", height = 500)
                                        ),
                                        fluidRow(
                                          dataTableOutput("tabla")
                                        )
                                      )
                                    )
                           ), #Cierre panel censo
                           
                           tabPanel("Establecimientos",
                                    sidebarLayout(
                                      sidebarPanel(
                                        #textInput("palabra_clave", "Búsqueda por palabra clave"),
                                        selectInput("categoria", "Filtro por categoría",
                                                    #c("Todos",substring(unique(df_censo$CNAE),1,2)[order(substring(unique(df_censo$CNAE),1,2))][-1])
                                                    c("Todos",unique(df_establecimientos$Categoría)[order(unique(df_establecimientos$Categoría))])
                                        ),
                                        sliderInput("reviews", "Filtro por número de reseñas",min(as.numeric(unique(df_establecimientos$Reviews))),max(as.numeric(unique(df_establecimientos$Reviews))),c(5,100),step = 10
                                        ),
                                        sliderInput("valoracion", "Filtro por valoración",min(as.numeric(unique(df_establecimientos$Valoración))),max(as.numeric(unique(df_establecimientos$Valoración))),c(0,3),step = 0.5
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
                                        fluidRow(
                                          leafletOutput("mapa_establecimientos", height = 500)
                                        ),
                                        fluidRow(
                                          dataTableOutput("tabla_establecimientos")
                                        )
                                      )
                                    )
                           ) #Cierre panel establecimientos
                           
                ) # Cierre navbarPage
) # Cierre UI

######################################################
# LÓGICA DE SERVIDOR
######################################################

server <- function(input, output, session) {
    
    datos <- reactiveValues(borme=NULL)
    

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
        shinyjs::hide("comparaciones")
        shinyjs::hide("variables_borme_listado")
        shinyjs::show("variables_mapa")
      }else if(input$tabs_borme == "Modificaciones capital"){
        shinyjs::hide("comparaciones")
        shinyjs::hide("variables_borme_listado")
        shinyjs::hide("variables_mapa")
      }else if(input$tabs_borme == "Listado informativo"){
        shinyjs::hide("comparaciones")
        shinyjs::show("variables_borme_listado")
        shinyjs::hide("variables_mapa")
      }else if(input$tabs_borme == "Estadística básica 2"){
        shinyjs::show("comparaciones")
        shinyjs::show("variables_borme_listado")
        shinyjs::hide("variables_mapa")
      }else{
        shinyjs::show("comparaciones")
        shinyjs::show("variables_borme_listado")
        shinyjs::hide("variables_mapa")
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

    #==========================================================================
    # LLAMADAS API THINGSBOARD
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
        if(input$Municipio_principal == "Burgos provincia"){
            df <- df
        }else{
          df <- df[df$Municipio == input$Municipio_principal,]
        }
        
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
        if(input$comparaciones == "Burgos provincia"){
          df <- df
        }else{
          df <- df[df$Municipio == input$Municipio_comparaciones,]
        }
        
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
              df <- df[,c(2,1)]
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
        
        print(df_comparativa)

        if(!is.data.frame(df_comparativa)){
          df <- rbind(df_ref,df_representacion_en_territorio)
          df$Total <- c(sum(as.numeric(df[1,])),100)
          rownames(df) <- c("Recuento", "Representación en territori de referencia (%)")
        }else if(nrow(df_comparativa) == 0){
          df <- rbind(df_ref,df_representacion_en_territorio)
          df$Total <- c(sum(as.numeric(df[1,])),100)
          rownames(df) <- c("Recuento", "Representación en territori de referencia (%)")
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
            rownames(df) <- c("Recuento", "Representación en territori de referencia (%)", "Representació en territori de comparació (%)","Representació sobre forma jurídica en territori de comparació (%)")
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

    # 1) Gráfico barras para estadística básica 1
    output$barras_borme_eb1 <- renderPlotly({

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
        
        
        # Definición de anchura de barra
        if(ncol(df) == 1){
          ancho <- 400
        }else{
          ancho <- 1
        }
        
        df <- as.data.frame(t(df))
        if(ncol(df) == 1){
          colnames(df) <- c("Recuento")
          texto <- paste("Forma jurídica: ", df$`Forma jurídica`,"<br>Recuento: ",df$Recuento,sep = "")
        }else{
          colnames(df) <- c("Recuento","Representació")
          texto <- paste("Forma jurídica: ", df$`Forma jurídica`,"<br>Recuento: ",df$Recuento, "<br>Representació (%): ",df$Representació,sep = "")
        }
        df$`Forma jurídica` <- rownames(df)
        
        g <- plot_ly(df, x = df$`Forma jurídica`, y = ~Recuento, type = 'bar', color = I("red"), width = ancho,
                     text = texto,
                     hoverinfo = 'text')
        g <- g %>% layout(
            title = list(text = paste('<b>Recuento por forma jurídica ', input$Municipio_principal, '</b>',sep = ''), y = -0.1),
            xaxis = list(
                title = "Forma jurídica")
        )

        g
    })

    # 2) Gráfico lineas para estadística básica 2
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
      p <- p %>% add_trace(type = 'scatter', mode = 'lines+markers')
      p <- p %>% layout(
        title = paste("<b>Recuento mensual ",input$Municipio_principal, " por forma jurídica</b>",sep = "")
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
      
      df_filtrados <- df_filtrados[as.numeric(df_filtrados$Latitud) > 41 & as.numeric(df_filtrados$Longitud) < -1.3,]
      
      shiny::validate(
        need(nrow(na.omit(df_filtrados)) != 0,
             "¡Atención!\nNo existen datos disponibles para el valor de los filtros seleccionados.\nModifica el valor de los filtros si lo desea."
        )
      )
      
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
                     "Domicili social: ", domicilio_social_popup, sep = "") %>% lapply(htmltools::HTML)

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
          
          wb <- createWorkbook()
          addWorksheet(wb, sheetName = "Datos")
          writeData(wb, sheet = 1, x = df)
          saveWorkbook(wb, file)
        }
    )
    
    #============================
    # TÍTULOS TABLAS
    #============================
    
    output$texto_tabla_borme_listado <- renderUI(
      tags$div(id = "1",tags$h4(tags$b(paste("Listado informativo ", input$Municipio_principal, sep = ""))))
    )
    output$texto_tabla_borme_eb1 <- renderUI(
      tags$div(id = "1",tags$h4(tags$b(paste("Recuento ", input$Municipio_principal, " i comparativa respecte ",
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
      tags$div(id = "1",tags$h4(tags$b(paste("Informació geolocalitzada ", ifelse(input$variables_mapa == 1,"Constitucions ", "Canvis de domicili social "), input$Municipio_principal,sep = ""))))
    )
    output$texto_tabla_borme_mapa <- renderUI(
      tags$div(id = "1",tags$h4(tags$b(paste("Listado informativo ", ifelse(input$variables_mapa == 1,"Constitucions ", "Canvis de domicili social "), input$Municipio_principal,sep = ""))))
    )
    
  
    
    
    
    
    
    
    
    
    
    
    #==========================================================================
    #==========================================================================
    #==========================================================================
    #==========================================================================
    #                                 ERTES
    #==========================================================================
    #==========================================================================
    #==========================================================================
    #==========================================================================
    
    #======================
    # FILTRADO
    #======================
    
    # 1) Filtros
    datos_filtrados_ertes_expedientes <- reactive({
      
      df <- df_tipo_expediente
   
      # 1) Filtro por fecha
      fecha_inicial <- as.Date(paste(substring(as.character(input$fechas_listado_ertes[1]),1,8),"01",sep = ""))
      fecha_final <- as.Date(paste(substring(as.character(input$fechas_listado_ertes[2]),1,8), "28",sep = ""))
      df <- df[df$fecha >= fecha_inicial & df$fecha <= fecha_final,]
      
      df$provincia <- municipios_ertes$Provincia[match(df$M, municipios_ertes$M)]
      
      # 2) Filtro territorio
      if(input$Municipio_principal_ertes == "Burgos provincia"){
        #df <- df[match(m,df$M),]
        df <- df[df$provincia == "Barcelona",]
      }else{
        switch(input$Municipio_principal_ertes,
               "AMB"={
                 df <- df[df$AMB == 1,]
               },
               "AMB sense Burgos ciudad"={
                 df <- df[df$AMB == 1 & df$Municipi != "Barcelona",]
               },
               "Burgos ciudad"={
                 df <- df[df$Municipi == "Barcelona",]
               },
               "Catalunya"={
                 df <- df
               },
               {
                 municipio_prinicpal <- input$Municipio_principal_ertes %>%
                   gsub(", els","",.) %>%
                   gsub("els ","",.) %>%
                   gsub(", la","",.) %>%
                   gsub(", el","",.) %>%
                   gsub("el ","",.) %>%
                   gsub(", les","",.) %>%
                   gsub("les ","",.) %>%
                   gsub("la ","",.) %>%
                   gsub(", l'","",.) %>%
                   gsub("l'","",.) %>%
                   gsub(", l","",.)
                 df <- df[df$Municipi == municipio_prinicpal,]
               }
        )
      }
      
      #3) Filtro expedientes/personas
      if(input$variables_ertes == 1){  # 1 corresponde a expedientes
        df <- df[,c(3,4,6,8,11,12)]
      }else{
        df <- df[,c(3,5,7,9,11,12)]
      }
      
      df <- df[c(1:949),]
      
      #Gestión errores
      if(nrow(df) == 0){
        return(0)
      }
      
      return(df)
    })
    
    # 2) Filtro sección económica
    datos_filtrados_ertes_econom <- reactive({
      
      #1) Filtro expedientes/personas. Selección de un df u otro.
      if(input$variables_ertes == 1){  # 1 corresponde a expedientes
        df <- df_expediente_econom
      }else{
        df <- df_expediente_trabajo
      }
      
      # 2) Filtro por fecha
      fecha_inicial <- as.Date(paste(substring(as.character(input$fechas_listado_ertes[1]),1,8),"01",sep = ""))
      fecha_final <- as.Date(paste(substring(as.character(input$fechas_listado_ertes[2]),1,8), "28",sep = ""))
      df <- df[df$fecha >= fecha_inicial & df$fecha <= fecha_final,]

      df$provincia <- municipios_ertes$Provincia[match(df$M, municipios_ertes$M)]
      
      # 2) Filtro territorio
      if(input$Municipio_principal_ertes == "Burgos provincia"){
        #df <- df[match(m,df$M),]
        df <- df[df$provincia == "Barcelona",]
      }else{
        switch(input$Municipio_principal_ertes,
               "AMB"={
                 df <- df[df$AMB == 1,]
               },
               "AMB sense Burgos ciudad"={
                 df <- df[df$AMB == 1 & df$Municipi != "Barcelona",]
               },
               "Burgos ciudad"={
                 df <- df[df$Municipi == "Barcelona",]
               },
               "Catalunya"={
                 df <- df
               },
               {
                 municipio_prinicpal <- input$Municipio_principal_ertes %>%
                   gsub(", els","",.) %>%
                   gsub("els ","",.) %>%
                   gsub(", la","",.) %>%
                   gsub(", el","",.) %>%
                   gsub("el ","",.) %>%
                   gsub(", les","",.) %>%
                   gsub("les ","",.) %>%
                   gsub("la ","",.) %>%
                   gsub(", l'","",.) %>%
                   gsub("l'","",.) %>%
                   gsub(", l","",.)
                 df <- df[df$M == municipio_prinicpal,]
               }
        )
      }
      
      df <- df[c(1:949),]
      
      #Gestión errores
      if(nrow(df) == 0){
        return(0)
      }
      
      return(df)
    })
    
    # 3) Filtrado para mapa
    datos_filtrados_mapa <- reactive({
      
      df <- datos_filtrados_ertes_econom() 
      
      # Manejo de error
      shiny::validate(
        need(df != 0, "¡Atención!\nNo existen datos disponibles para los filtros seleccionados.\nModifica los filtros si lo deseas.")
      )
      
      df <- func_recuento_econom(df,2) #Flag 0 para devolución recuento
      
      df$M <- df$Municipi %>%
        gsub(", els","",.) %>%
        gsub("els ","",.) %>%
        gsub(", la","",.) %>%
        gsub(", el","",.) %>%
        gsub("el ","",.) %>%
        gsub(", les","",.) %>%
        gsub("les ","",.) %>%
        gsub("la ","",.) %>%
        gsub(", l'","",.) %>%
        gsub("l'","",.) %>%
        gsub(", l","",.)
      
      df$latitud <- municipios_ertes$lat[match(df$M, municipios_ertes$M)]
      df$longitud <- municipios_ertes$lon[match(df$M, municipios_ertes$M)]
      
      if(any(grepl("Tots",input$variables_ertes_sec_ecc))){
        secciones <- input$variables_ertes_sec_ecc[1:(length(input$variables_ertes_sec_ecc)-1)]
      }else if(any(grepl("Cap",input$variables_ertes_sec_ecc))){
        secciones <- NULL
      }else if(any(grepl("Top 3",input$variables_ertes_sec_ecc))){
        secciones_economicas <- c("A. Agricultura, ramaderia i pesca","B. Indústries extractives", "C. Indústries manufactureres",               
                                  "D. Energia elèctrica i gas", "E. Aigua, sanejament i gestió de residus", "F.Construcció",                            
                                  "G. Comerç a l’engròs i al detall", "H. Transport i emmagatzematge", "I. Hostaleria",                             
                                  "J. Informació i comunicacions", "K. Activitats financeres i d’assegurances", "L. Activitats immobiliàries",                
                                  "M. Activitats professionals i tècniques", "N. Activitats administratives i auxiliars", "O. Adm. pública, Defensa i SS obligatòria",  
                                  "P. Educació", "Q. Activitats sanitàries i serveis socials", "R. Activitats artístiques i d’entreteniment",
                                  "S. Altres serveis", "T. Activitats de les llars", "U. Organismes extraterritorials",           
                                  "SE. Sense especificar")
        
        df2 <- df[,c("Municipi",secciones_economicas,"latitud","longitud")]
        
        Municipio <- c()
        seccion_economica <- c()
        recuento <- c()
        latitud <- c()
        longitud <- c()
        for(i in 1:nrow(df2)){
          Municipio <- c(Municipio, rep(df2$Municipi[i],(ncol(df2)-3)))  # Menos municipio, lat y long
          seccion_economica <- c(seccion_economica, colnames(df2)[2:(ncol(df2)-2)])  # Las 2 'ultimas son lat y long
          recuento <- c(recuento, as.numeric(df2[i,2:(ncol(df2)-2)]))
          latitud <- c(latitud, rep(df2$latitud[i],(ncol(df2)-3)))
          longitud <- c(longitud, rep(df2$longitud[i],(ncol(df2)-3)))
        }
        df2 <- data.frame(Municipio,seccion_economica,recuento,latitud,longitud,stringsAsFactors = FALSE)
        colnames(df2) <- c("Territori","Secció econòmica","Recuento","Latitud","Longitud")

        #Orden decreciente para visualizar los labels superpuestos en el mapa. Se ordena en base al territorio y al recuento
        a <- as.numeric(order(as.data.frame(df2[,1]),as.data.frame(df2[,3]),decreasing = TRUE))
        df2 <- df2[a,]
        
        # Cálculo df3 para el cálculo de top 3 secciones económicas
        df3 <- df2 %>%
          group_by(`Secció econòmica`) %>%
          summarise(suma = sum(Recuento))
        df3 <- df3[order(df3$suma,decreasing = TRUE),]
        
        secciones <- df3$`Secció econòmica`[1:3]
      }else{
        secciones <- input$variables_ertes_sec_ecc
      }
      
      df <- df[,c("Municipi",secciones,"latitud","longitud")]
      
      # Manejo de error: "inexistencia de datos para el intervalo de fechas seleccionado" de cara al usuario
      shiny::validate(
        need(ncol(df) > 3, "¡Atención!\nNo existen datos disponibles para los filtros seleccionados.\nModifica los filtros si lo deseas.")
      )
      
      return(df)
    })
    
    #======================
    # FUNCIONES
    #======================
    
    # 1) Recuento tipo
    func_recuento_tipos <- function(df, flag_evol){
      df <- df
      
      df$fecha <- paste(year(df$fecha),"/",month(df$fecha),sep = "")  #Extracción de meses
      
      # Cálculo suma
      colnames(df) <- c("Territori","2","3","4","AMB","Fecha")
      
      if(flag_evol == 1){
        #Recuento por mes y forma jurídica
        df <- df %>%
          group_by(Fecha) %>%
          summarise(
            `Suma Força mayor` = sum(as.numeric(`2`),na.rm = TRUE),
            `Suma causes` = sum(as.numeric(`3`),na.rm = TRUE),
            `Suma Total` = sum(as.numeric(`4`),na.rm = TRUE)
            )
        
        df <- df[df$Fecha != "NA/NA",]
      
       # El valor de 1 es para la realización del gráfico de evolución.
        return(df)
      }
      
      fuerza_mayor <- sum(df$`2`,na.rm = TRUE)
      causas <- sum(df$`3`,na.rm = TRUE)
      total <- sum(df$`4`,na.rm = TRUE)
      Territori <- input$Municipio_principal_ertes
      df <- data.frame(Territori,fuerza_mayor,causas,total,stringsAsFactors = FALSE)

      colnames(df) <- c("Territori","Força mayor","Causes","Total")
      
      return(df)
    }
    
    # 2) Recuento sección económica
    func_recuento_econom <- function(df, flag_evol){
      df <- df
      
      df$fecha <- paste(year(df$fecha),"/",month(df$fecha),sep = "")  #Extracción de meses
      
      if(flag_evol == 1){  #Recuento por fecha
        
        df <- df[,c(4:26,29)]
        df <- df %>%
          group_by(fecha) %>%
          summarise_each(funs(sum(.,na.rm = TRUE)))
        
        df <- df[df$fecha != "NA/NA",]
        return(df)
        
      }else if(flag_evol == 2){ #Recuento por territorio
        df <- df[,c(3:26)]
        df <- df %>%
          group_by(Municipi) %>%
          summarise_each(funs(sum(.,na.rm = TRUE)))
        
        df <- df[!is.na(df$Municipi),]
        return(df)
      } 
      
      df2 <- as.data.frame(colSums(df[,4:26],na.rm = TRUE),row.names = NULL)
      df2 <- as.data.frame(t(df2),row.names = NULL)
      colnames(df2) <- colnames(df)[4:26]
      df2$Territori <- input$Municipio_principal_ertes
      df2 <- df2[,c(ncol(df2),1:(ncol(df2)-1))]

      return(df2)
    }
    
    
    
    #======================
    # TABLAS
    #======================
    
    # 1) Tabla ertes por tipo de expediente recuento
    output$tabla_ertes_expedientes_recuento <- renderDataTable({
      
      df <- datos_filtrados_ertes_expedientes() 
      
      # Manejo de error
      shiny::validate(
        need(df != 0, "¡Atención!\nNo existen datos disponibles para el intervalo de fechas seleccionado.\nSelecciona otro intervalo si lo deseas.")
      )
      
      df <- func_recuento_tipos(df,0) 
      
      tabla <- datatable(df, options = list(pageLength = 5,
                                                  columnDefs = list(list(className = 'dt-center', targets = "_all")),
                                                  scrollX=TRUE,
                                                  scrollCollapse=TRUE),
                         rownames= FALSE,
                         escape = FALSE)
      return(tabla)
    })
    
    # 2) Tabla ertes media
    output$tabla_ertes_media <- renderDataTable({
      
      df <- datos_filtrados_ertes_expedientes()
      
      # Manejo de error
      shiny::validate(
        need(df != 0, "")
      )
      
      # Cálculo suma
      colnames(df) <- c("Territori","2","3","4","AMB","Fecha")
      
      fuerza_mayor <- round(mean(as.numeric(df$`2`),na.rm = TRUE),0)
      causas <- round(mean(as.numeric(df$`3`),na.rm = TRUE),0)
      total <- round(mean(as.numeric(df$`4`),na.rm = TRUE),0)
      Territori <- input$Municipio_principal_ertes
      df <- data.frame(Territori,fuerza_mayor,causas,total,stringsAsFactors = FALSE)
      
      colnames(df) <- c("Territori","Força mayor","Causes","Total")
      
      tabla <- datatable(df, options = list(pageLength = 5,
                                            columnDefs = list(list(className = 'dt-center', targets = "_all")),
                                            scrollX=TRUE,
                                            scrollCollapse=TRUE),
                         rownames= FALSE,
                         escape = FALSE)
      return(tabla)
    })
    
    # 3) tabla recuento sección económica
    output$tabla_ertes_secc_recuento <- renderDataTable({
      
      df <- datos_filtrados_ertes_econom() 
      
      # Manejo de error
      shiny::validate(
        need(df != 0, "¡Atención!\nNo existen datos disponibles para el intervalo de fechas seleccionado.\nSelecciona otro intervalo si lo deseas.")
      )
      
      df <- func_recuento_econom(df,0) 
      
      tabla <- datatable(df, options = list(pageLength = 5,
                                            columnDefs = list(list(className = 'dt-center', targets = "_all")),
                                            scrollX=TRUE,
                                            scrollCollapse=TRUE),
                         rownames= FALSE,
                         escape = FALSE)
      return(tabla)
    })
    
    # 4) Tabla ertes sección económica MAPA
    output$tabla_ertes_mapa <- renderDataTable({
      
      df <- datos_filtrados_mapa()
      df <- df[,c(1:(ncol(df) - 2))]
      
      tabla <- datatable(df, options = list(pageLength = 5,
                                            columnDefs = list(list(className = 'dt-center', targets = "_all")),
                                            scrollX=TRUE,
                                            scrollCollapse=TRUE),
                         rownames= FALSE,
                         escape = FALSE)
      return(tabla)
    })
  
    
    #======================
    # GRÁFICOS
    #======================
    
    # 1) Líneas recuento mes tipo expediente
    output$lineas_ertes_expedientes <- renderPlotly({
      
      df <- datos_filtrados_ertes_expedientes() 
      
      # Manejo de error
      shiny::validate(
        need(df != 0, "")
      )
      
      df <- func_recuento_tipos(df,1) #Flag 1 para devolución recuento
      
      #Generación df para gráfico de líneas
      tipo <- c()
      fecha <- c()
      recuento <- c()
      for(i in 1:nrow(df)){
        tipo <- c(tipo,"Força mayor")
        tipo <- c(tipo,"Causes")
        tipo <- c(tipo,"Total")
        
        fecha <- c(fecha, rep(df$Fecha[i]))
        recuento <- c(recuento, as.numeric(df[i,2]))
        recuento <- c(recuento, as.numeric(df[i,3]))
        recuento <- c(recuento, as.numeric(df[i,4]))
      }
      df <- data.frame(tipo,fecha,recuento,stringsAsFactors = FALSE)
      colnames(df) <- c("Tipo","Mes","Recuento")

      p <- df
      p <- p %>% plot_ly(x = ~Mes, y = ~Recuento, fill= ~Tipo, color = ~Tipo)
      p <- p %>% add_trace(type = 'scatter', mode = 'lines+markers')
      p <- p %>% layout(
        title = list(text = paste('<b>Evolució mensual ', ifelse(input$variables_ertes == 1, "Expedients", "Treballadors"), " ", input$Municipio_principal_ertes, '</b>',sep = ''), y = -0.1)
      )
      
      p
    })
    
    
    # 2) TOP 5 secciones económicas con más expedientes o personas
    output$barras_ertes_secc <- renderPlotly({
      
      df <- datos_filtrados_ertes_econom() 
      
      # Manejo de error
      shiny::validate(
        need(df != 0, "")
      )
      
      df <- func_recuento_econom(df,0) #Flag 0 para devolución recuento
      df <- df[,c(2:(ncol(df)-1))]
      df <- df[,order(df,decreasing = TRUE)]
      df <- df[,c(1:5)]
      
      #Generación df para gráfico de líneas
      seccion_economica <- c()
      recuento <- c()
      for(i in 1:nrow(df)){
        seccion_economica <- c(seccion_economica,colnames(df)[1:ncol(df)])
        recuento <- c(recuento, as.numeric(df[i,1:ncol(df)]))
      }
      df <- data.frame(seccion_economica,recuento,stringsAsFactors = FALSE)
      colnames(df) <- c("Secció econòmica", "Recuento")
      
      df$`Secció econòmica` <- factor(df$`Secció econòmica`, levels = c(as.character(df$`Secció econòmica`)))
      
      variable_expediente <- ifelse(input$variables_ertes == 1, "Expedients", "Treballadors")
      
      g <- plot_ly(df, x = df$`Secció econòmica`, y = ~Recuento, type = 'bar', colors = 'red',
                   text = paste("Secció econòmica: ", df$`Secció econòmica`,"<br>Recuento: ",df$Recuento,sep = ""),
                   hoverinfo = 'text')
      g <- g %>% layout(
        title = list(text = paste('<b>Top 5 seccions econòmiques ', ifelse(input$variables_ertes == 1, "Expedients", "Treballadors")," ", input$Municipio_principal_ertes, '</b>',sep = ''), y = -0.1),
        xaxis = list(
          title = "Secció econòmica")
      )
      
      g
      
    })
    
    # 3) Líneas recuento mes sección económica
    output$lineas_ertes_secc <- renderPlotly({
      
      df <- datos_filtrados_ertes_econom() 
      
      # Manejo de error
      shiny::validate(
        need(df != 0, "")
      )
      
      df <- func_recuento_econom(df,1) #Flag 1 para devolución recuento
      
      #Generación df para gráfico de líneas
      tipo <- c()
      fecha <- c()
      recuento <- c()
      for(i in 1:nrow(df)){
        tipo <- c(tipo, colnames(df)[2:ncol(df)])
        fecha <- c(fecha, rep(df$fecha[i]))
        recuento <- c(recuento, as.numeric(df[i,2:ncol(df)]))
      }
      df <- data.frame(tipo,fecha,recuento,stringsAsFactors = FALSE)
      colnames(df) <- c("Tipo","Mes","Recuento")
      
      p <- df
      p <- p %>% plot_ly(x = ~Mes, y = ~Recuento, fill= ~Tipo, color = ~Tipo)
      p <- p %>% add_trace(type = 'scatter', mode = 'lines+markers')
      p <- p %>% layout(
        title = list(text = paste('<b>Evolució mensual ', ifelse(input$variables_ertes == 1, "Expedients", "Treballadors")," ", input$Municipio_principal_ertes, '</b>',sep = ''), y = -0.1),
        xaxis = list(
          title = "Secció econòmica")
      )
      
      p
    })
    
    
    #======================
    # MAPA
    #======================
    
    output$mapa_ertes <- renderLeaflet({

      df <- datos_filtrados_mapa()
      
      #Generación df para mapa
      Municipio <- c()
      seccion_economica <- c()
      recuento <- c()
      latitud <- c()
      longitud <- c()
      for(i in 1:nrow(df)){
        Municipio <- c(Municipio, rep(df$Municipi[i],(ncol(df)-3)))  # Menos municipio, lat y long
        seccion_economica <- c(seccion_economica, colnames(df)[2:(ncol(df)-2)])  # Las 2 'ultimas son lat y long
        recuento <- c(recuento, as.numeric(df[i,2:(ncol(df)-2)]))
        latitud <- c(latitud, rep(df$latitud[i],(ncol(df)-3)))
        longitud <- c(longitud, rep(df$longitud[i],(ncol(df)-3)))
      }
      df <- data.frame(Municipio,seccion_economica,recuento,latitud,longitud,stringsAsFactors = FALSE)
      colnames(df) <- c("Territori","Secció econòmica","Recuento","Latitud","Longitud")
      
      #Orden decreciente para visualizar los labels superpuestos en el mapa. Se ordena en base al territorio y al recuento
      a <- as.numeric(order(as.data.frame(df[,1]),as.data.frame(df[,3]),decreasing = TRUE))
      df <- df[a,]
      
      label <- paste("Territori: ", df$Territori, "<br/>",
                     "Secció econòmica: ", df$`Secció econòmica`, "<br/>",
                     "Recuento: ", df$Recuento, sep = "") %>% lapply(htmltools::HTML)
      
      
      if(max(round(df$Recuento)) < 1000){
        radios <-round(df$Recuento) * 3
      }else if(max(round(df$Recuento)) > 10000){
        radios <- rescale(round(df$Recuento), to = c(0.1, 1)) * 3000
      }else{
        radios <-round(df$Recuento)
      }
      
      pal <- colorFactor(palette = 'Set1', domain = df$gran_grup_CCO)
      
      leaflet() %>% addProviderTiles(providers$CartoDB.Positron) %>% addCircles(data=df,lng=as.numeric(df$Longitud), lat=as.numeric(df$Latitud), weight = 2,
                                                                                label= label, radius=radios, color = ~pal(`Secció econòmica`)) %>% 
        addLegend(position = "bottomright", pal = pal, values = df$`Secció econòmica`)
    })
    
    #==========================================================================
    # DESCARGA DE DATOS
    #==========================================================================
    
    # DESCARGA DATOS BORME csv
    output$descarga_ertes_csv <- downloadHandler(
      
      filename = function() {
        paste("Datos_ERTOS_", as.character(input$fechas_listado_ertes[1]),"_", as.character(input$fechas_listado_ertes[2]),".csv", sep="")
      },
      content = function(file) {
        if(input$tabs_borme == "Expedientes por tipo"){
          df <- datos_filtrados_ertes_expedientes() 
          df <- func_recuento_tipos(df,0)
        }else if(input$tabs_borme == "Expedientes por sección económica"){
          df <- datos_filtrados_ertes_econom() 
          df <- func_recuento_econom(df,0)
        }else{
          df <- datos_filtrados_mapa()
          df <- df[,c(1:(ncol(df) - 2))]
        }

        write.csv(df, file, eol="\n", sep = ",")  # eol="\n" es para el encoding de caracteres en .csv
      }
    )
    

    
    
    
    # =================================================================
    # CENSO DE EMRPESAS
    # =================================================================
    
    datos_filtrado <- reactive({
      
      df <- df_censo
      
      # 1) Filtro por intervalo de fechas
      df <- df[as.Date(as.character(df$`Fecha_constitución`), "%d/%m/%Y") >= as.Date(input$fechas[1]) &
                 as.Date(as.character(df$`Fecha_constitución`)) <= as.Date(input$fechas[2]) |
                 df$`Fecha_constitución` == "-",]
 
      # 2) Filtros por lat-long
      #if(!is.na(input$lat_max) | !is.na(input$long_max) | !is.na(input$lat_min) | !is.na(input$long_min)){
      #  print("ENTRO")
      #  df <- df[(df$Longitud <= input$long_max &
      #              df$Longitud >= input$long_min &
      #              df$Latitud <= input$lat_max &
      #              df$Latitud >= input$lat_min),
      #           ]
      #}
      
      # 2) Filtro por num empleados
      if(input$empleados == "Todos"){
        df <- df
      }else{
        df <- df[df$Tamaño_empresa_por_empleados == input$empleados,]
      }
      
      # 3) Filtro por división CNAE
      if(input$div_cnae == "Todos"){
        df <- df
      }else{
        if(grepl("[A-Z]",substring(input$div_cnae,1,1))){
          
          pos_letra_demandada <- grep(substring(input$div_cnae,1,1),letters,ignore.case = TRUE)
          codigos_a_extraer <- gsub("([0-9]+).*$", "\\1",
                                    df_cnae$completo[c((grep(letters[pos_letra_demandada],df_cnae$COD_CNAE2009,ignore.case = TRUE) + 1):
                                                         (grep(letters[pos_letra_demandada + 1],df_cnae$COD_CNAE2009,ignore.case = TRUE) - 1)
                                    )]
          )
          codigos_a_extraer <- codigos_a_extraer[nchar(codigos_a_extraer) > 3]
        }else{
          codigo_seleccionado <- gsub("([0-9]+).*$", "\\1", input$div_cnae)
          numero_de_carct <- nchar(codigo_seleccionado)
          codigos_a_extraer <- gsub("([0-9]+).*$", "\\1",
                                    df_cnae$completo[grep(codigo_seleccionado,substring(df_cnae$COD_CNAE2009,1,numero_de_carct))]
          )
          codigos_a_extraer <- codigos_a_extraer[nchar(codigos_a_extraer) > 3]
        }
        df <- df[match(codigos_a_extraer,gsub("([0-9]+).*$", "\\1",df$CNAE)),]
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
      }else{
        filtrado_palabra_clave <- df
      }
      
      filtrado_palabra_clave <- apply(as.data.frame(mapply(grepl, input$palabra_clave, df[,c(1:4)], ignore.case = T)),1,any)
      
      df <- subset(df, filtrado_palabra_clave)
      
      df <- df[,c(1,2,5,6,7,8,10,9,21,11,12,20,23,19,18,16,15,17,13,14,3,4)]
      df <- df[!is.na(df$URL),]
      
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
        "Facturación: ", df$Tamaño_empresa_por_empleados, "<br/>",
        sep="") %>%
        lapply(htmltools::HTML)
      
      #leaflet() %>% addProviderTiles(providers$CartoDB.Positron) %>% addMarkers(lng = longitud, lat = latitud, popup = popup)
      leaflet() %>% addTiles() %>% addMarkers(lng = longitud, lat = latitud, popup = popup)
    })
    
    # TABLA
    output$tabla <- renderDataTable({
      
      df <- datos_filtrado()

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
    
    
    
    
    # =================================================================
    # ESTABLECIMIENTOS
    # =================================================================
    
    datos_filtrado_establecimientos <- reactive({
      
      df <- df_establecimientos

      # 1) Filtro por categoria
      if(input$categoria == "Todos"){
        df <- df
      }else{
        df <- df[df$Categoría == input$categoria,]
      }
      
      # 2) Filtro por número de REVIEWS
      print(input$reviews)
      df <- df[df$Reviews >= input$reviews[1] & df$Reviews <= input$reviews[2],]

      # 3) Filtro por valoración
      df <- df[df$Valoración >= input$valoracion[1] & df$Valoración <= input$valoracion[2],]
      
      # 4) Filtro web
      if(as.numeric(input$web) == 1){
        df <- df[df$Web != 0 ,]
      }else if(as.numeric(input$web) == 2){
        df <- df[df$Web == 0,]
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
      
      df$Web <- paste("https://www.",df$Web, sep= "")
      
      
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
}

# Run the application
shinyApp(ui = ui, server = server)
