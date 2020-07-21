

library(tidyverse)
library(shiny)
library(readxl)
library(knitr)
library(plotly)
library(DT)
library(leaflet)
library(sp)

vesikasvirajat <- read_csv("C:/Users/Rasmusbo/Documents/R_shiny/2020VELMU_readxl_tarkistus/Read_surveydata_in/external_data/VesikasviSummary.csv")
hertta_lajit_excel <- read_xlsx(path = "C:/Users/Rasmusbo/Documents/R_shiny/2020VELMU_readxl_tarkistus/Read_surveydata_in/external_data/lajinimet_hertta.xlsx")
hertan_lajinimet <- as.list(hertta_lajit_excel)[[1]] # The äs didn't want to import correctly, thus the extra steps.

# Define UI for data upload app ----
ui = fluidPage(
  navbarPage("VELMU-tarkastus",
             tabPanel("Lataa kartoitustaulukko",
                      
                      # Sidebar layout with input and output definitions ----
                      sidebarLayout(
                        
                        # Sidebar panel for inputs ----
                          sidebarPanel(
                          
                          # Input: Select a file ----
                          fileInput("file1", "ALoita lataamalla excel-taulukko:",
                                    multiple = FALSE,
                                    accept = c(".xls", ".xlsx", ".xlsm")),
                          
                          p(paste("Voit myös drag&drop taulukon yllä olevaan kenttään. Huomioithan, että LajiGIS-yhteensopivassa",
                          "taulukossa on 5 tyhjää riviä, eli otsikot on rivillä 6 ja aineisto alkaa riviltä 7.")),
                          
                          # Horizontal line ----
                          tags$hr(),
                          
                          # Input: Select quotes ----
                          numericInput(inputId = "tyhj_rivien_maara",
                                       label = "Tyhjien rivien m??r? taulukon yl?reunassa:",
                                       value = 5,
                                       min = 0,
                                       max = 10
                                       ),
                        
                          # Horizontal line ----
                          tags$hr(),
                          
                          paste("Tämä ohjelma on tarkoitettu VELMU-muodossa olevan kartoitusaineiston tarkistamiseen.",
                                "Välilehdet on asetettu järjestykseen, jossa aineistoa kannattaa käydä läpi:"),
                          p(""),
                          
                          p("1. Yleiskatsaus aineistoon ja sijainteihin"),
                          p("2. Poikkeavuuksien ja selkeiden virheiden etsintä kuvaajien avulla"),
                          p("3. Lajihavaintojen läpikäynti"),
                          p("4. Sijaintien tarkempi läpikäynti")
                          
                          ),
                        
                        
                        
                        # Main panel for displaying outputs ----
                        mainPanel(
                          h2("Linjat kartalla"),
                          
                          # Output: Data file ----
                          
                          leafletOutput(outputId = "kokoomalinjaMap"),
                          tags$hr(),
                          h2("Kokoomalinjojen yleiskatsaus"),
                          DTOutput(outputId = "yleiskatsaus_kokoomalinjat")
                          )
                        )
                      ),
             
             # EDA kuvaajat ----
             tabPanel("Kuvaajat",
                      
                      sidebarLayout(
                        
                        sidebarPanel(checkboxInput(inputId = "kartoittajaVaritys",
                                                   label = "Erottele kartoittajan mukaan",
                                                   value = T),
                                     tags$hr()),
                        mainPanel(tags$hr(),
                                  plotlyOutput(outputId = "ruudunSyvyysBoxPlot"),
                                  plotlyOutput(outputId = "ruudunSyvyysHist"),
                                  plotlyOutput(outputId = "etaisyysLinjallaPlot"))
                        )
                      ),
             
             # Tables to show summaries and unique values ----
             tabPanel('Taulukot',
                      fluidRow(
                        column(2, 'sidebar'),
                        column(10,
                               "Uniikit arvot kategorisista muuttujista:",
                               wellPanel(DTOutput(outputId = "kat_variables")))
                        )
                      ),
             # Tab to check on species data ----
             tabPanel("Lajidata",
                      fluidRow(
                        column(6,
                               "Lista lajeista, jotka eivät ole yhteensopivia LajiGISsin kanssa:",
                               wellPanel(DTOutput(outputId = "lajitasmaavyys")),
                               "Lajihavainnot ja havaintojen määrä taulukossa:",
                               wellPanel(DTOutput(outputId = "lajilista"))),
                        column(6,
                               "Lajistokartta",
                               wellPanel(leafletOutput(outputId = "lajikartta")),
                               wellPanel(selectInput(inputId = "filt_laji",
                                                     label = "Suodata kartalla näytettävät lajit:",
                                                     choices = character())))
                        ),
                      fluidRow(
                        column(12,
                               "Lista lajeista, joiden syvyys on paljon tai vähän",
                               wellPanel())
                      )
                      ),
             # Maps ----
             tabPanel("Kartat",
                      fluidRow(
                        column(2, 
                               "Suodata kartoittajan perusteella:",
                               wellPanel(selectInput(inputId = "filt_kartoittaja",
                                                     label = "Kartoittaja:",
                                                     choices = character()))),
                        column(10,
                               h2("Linjat kartalla"))
                        )
                      )
             )
  )



# Define server logic to read selected file ----
server <- function(input, output, session) {
  
  observeEvent(kart_df(), {
    updateSelectInput(session, "filt_kartoittaja", choices = unique(kart_df()$sukelluslinjan.kartoittaja))
  })
  
  observeEvent(kart_df(), {
    updateSelectInput(session, "filt_laji", choices = lajilista()$lajihavainto)
  })
  
  #Read in the file and name variables to be consistant
  kart_df <- reactive({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    df_to_mod <- read_xlsx(input$file1$datapath, 
              skip = input$tyhj_rivien_maara,
              .name_repair = "universal")
    
    aineisto <- df_to_mod %>%
      select(kohteen.nro = 1,
             kohteen.taso = 2,
             kartoituksen.tarkoitus = 3, 
             kohteen.nimi = 4, 
             alkukoordinaatti.N = 5, # 5-8 vain kokoomalinjoilla (62)
             alkukoordinaatti.E = 6,
             loppukoordinaatti.N = 7,
             loppukoordinaatti.E = 8,
             ruudun.koordinaatti.N = 9,
             ruudun.koordinaatti.E = 10,
             EI_TIETOA = 11, #
             EI_TIETOA2 =12, #
             pisteen.id = 13,
             kartoitusmenetelma = 14, 
             kartoitusmenetelman.tarkennus = 15, 
             runsausarvioinnin.menetelma = 16,
             avoimuusindeksi = 17,
             kohteen.huomautukset = 18, 
             otantamenetelma = 19, 
             peittavyyden.arviointi = 20,
             kart.tarkistustarve = 21,
             SYKEID = 22, #
             kartoituskerta = 23,
             kartoituspvm = 24,
             aloitusaika = 25,
             kenttahenkilot = 26, 
             vene = 27, 
             veden.lampotila = 28,
             lampotilan.mittaussyvyys = 29,
             secchi.syvyys = 30, 
             levakukinta = 31, 
             tuulen.suunta = 32, 
             tuulen.voimakkuus = 33,
             sedimentin.koodisto = 34,
             sedimentin.maara = 35, 
             saliniteetti = 36, #
             
             TALLENTAJAN.NIMI = 37,
             TALLENTAJAN.ORG = 38,
             
             ## secchi.levyn.koko = 39, tama uudessa versiossa ilmeisesti.. ##
             kartoituskerran.huomautukset = 39, 
             YMPARISTOTYYPPI = 40, #
             REHEV.HERKKA = 41, #
             JOKIP.TYYPPI = 42, #
             RANNAN.KALT = 43, #
             VESIALUE.KMUOTO = 44, #
             RANNAN.KASV = 45, #
             R.KASV.TARK = 46, #
             videon.tallennuslaite = 47,
             videon.ID = 48, 
             videon.kesto = 49,
             videon.syvyyden.korjaus = 50,
             videon.alkusyvyys = 51,
             videon.loppusyvyys = 52,
             videon.analysointipvm = 53,
             videon.analysoija = 54,
             videon.laatu = 55,
             VIDEON.VARI = 56, #
             
             sukelluslinjan.kartoittaja = 57,
             sukelluslinjan.pituus = 58,
             sukelluslinjan.kompassisuunta = 59,
             sukelluslinjan.etaisyys.rannasta = 60,
             sukelluslinjan.alkusyvyys = 61,
             sukelluslinjan.loppusyvyys = 62,
             sukelluslinjan.syvyyden.korjaus = 63, 
             pohjan.kaltevuus = 64, 
             arviointiruudun.pinta.ala = 65, 
             arviointiruudun.syvyys = 66,
             SYVYYDEN.TARKENNE = 67, #
             arviointiruudun.etaisyys = 68,
             ETAIS.MITMEN = 69, #
             
             kasv.alarajan.lajit = 70,
             kasv.alarajan.syvyys = 71,
             kasv.alarajan.etaisyys = 72,
             SYVIN.LAJI = 73, #
             SYVIN.LAJI.SYVYYS = 74, #
             SYVIN.LAJI.ETAISYYS = 75, #
             matalin.fucus.syvyys = 76,
             matalin.fucus.etaisyys = 77,
             syvin.fucus.syvyys = 78,
             syvin.fucus.etaisyys = 79,
             
             vyohykkeen.muodostaja.ylataso = 80,
             vyohykkeen.valtalaji = 81,
             vyohykkeen.alaraja.syvyys = 82,
             vyohykkeen.alaraja.etaisyys = 83,
             vyohykkeen.ylaraja.syvyys = 84,
             vyohykkeen.ylaraja.etaisyys = 85,
             runsain.vyohyke.alaraja.syvyys = 86,
             runsain.vyohyke.alaraja.etaisyys = 87,
             runsain.vyohyke.ylaraja.syvyys = 88,
             runsain.vyohyke.ylaraja.etaisyys = 89,
             
             kallio = 90,
             lohkareab = 91, 
             lohkarebb = 92,
             lohkarecb = 93,
             glasiaalisavi = 94,
             kivias = 95,
             liikkumaton.pohja = 96,
             kivibs = 97,
             sora = 98,
             hiekka = 99,
             siltti = 100,
             savi = 101,
             muta = 102,
             liikkuva.pohja = 103,
             konkreetiot = 104,
             hiekkakivi = 105,
             keinotekoinen.alusta = 106,
             turve = 107,
             puun.rungot = 108,
             pohjanlaadut.yhteensa = 109,
             
             
             roskat.koodisto = 110,
             roskat.kpl = 111,
             # epavarma.pohja = 112 ? Mihin uudessa taulukossa? #
             
             VELMU.SORA = 112, #
             VELMU.HIEKKA = 113, #
             VELMU.LIEJU = 114, #
             VELMU.SAVI = 115, #
             
             havainnon.tarkistustarve = 116,   
             lajihavainto = 117, 
             lajin.peittavyys = 118, 
             lajin.lukumaara = 119,
             lajin.maaran.yksikko = 120,
             lajin.korkeus = 121,
             LAJIN.BIOMASSA = 122, #
             lajihavainnon.laatu = 123,
             laji.huomautukset = 124,
             #laji.epifyyttinen = 1XX Mihin uudessa taulukossa? #
             
             125:133, # LUTU-TYYPPEJ? #
             
             naytteen.numero = 134,
             naytteen.tyyppi = 135,
             naytteen.keraaja = 136,
             naytteen.maarittaja = 137,
             naytteen.maaritys.pvm = 138,
             naytteen.maaritysteos = 139,
             naytteen.sijainti = 140,
             naytteen.museonumero = 141,
             naytteen.URI = 142,
             naytteen.lisatiedot = 143,
             
             144:147, # Hankkeen tietoja, ei kaytossa
             
             hanke.ID = 148,
             
             149:157)
    
    return(aineisto)
    
  })
  
  # Filter only kartoitusruudut, without kokoomalinjat
  linjat_filtered_63 <- reactive({kart_df() %>%
      filter(kohteen.taso == 63) 
  })
  
  # Filter only kokoomalinjat, without the kartoitusruudut
  kokoomalinjat <- reactive({kart_df() %>%
      filter(kohteen.taso == 62)})

  # List and count of species
  lajilista <- reactive({
    linjat_filtered_63() %>%
      select(lajihavainto) %>%
      group_by(lajihavainto) %>%
      tally(sort = T)
  })
  
  #lajisto_kartalle <- reactive({
  ##  kart:
  #})
  
  # General overview of the kokoomalinjat
  output$yleiskatsaus_kokoomalinjat <- renderDT({kokoomalinjat() %>%
      select(nr. = kohteen.nro,
             kohteen.nimi,
             kartoituspvm,
             kartoittaja = sukelluslinjan.kartoittaja,
             kenttahenkilot,
             alkusyvyys = sukelluslinjan.alkusyvyys,
             loppusyvyys = sukelluslinjan.loppusyvyys
             )})

  # Koko taulukko suodattamista ym varten
  output$taulukko <- renderDT({
    kart_df()
  })
  
  # Unique values from categorical variables
  output$kat_variables <- renderDT({
    
    cat_var <- kart_df() %>%
      select(kohteen.taso,
             kartoituksen.tarkoitus, 
             kartoitusmenetelma, 
             kartoitusmenetelman.tarkennus, 
             runsausarvioinnin.menetelma,
             otantamenetelma, 
             peittavyyden.arviointi,
             kenttahenkilot, 
             vene, 
             levakukinta, 
             tuulen.suunta, 
             sedimentin.maara, 
             sukelluslinjan.kartoittaja,
             arviointiruudun.pinta.ala, 
             lajihavainnon.laatu,
             hanke.ID)
    
    map(cat_var, unique) %>%
      map(as.data.frame)
  })
  
  # Lajisto 
  output$lajilista <- renderDT({
    lajilista()
  })
  
  output$lajitasmaavyys <- renderDT({
    kart_df() %>%
      filter(!lajihavainto %in% hertan_lajinimet & !is.na(lajihavainto)) %>%
      select(kohteen.nro, kohteen.nimi, sukelluslinjan.kartoittaja, lajihavainto)
    
  })
  
  # Boxplot to check ruudun syvyydet:
  output$ruudunSyvyysBoxPlot <- renderPlotly({
    linjat_filtered_63() %>%
      plot_ly(y =~arviointiruudun.syvyys,
              color = ~sukelluslinjan.kartoittaja,
              hoverinfo = "text",
              text = ~paste("Kohteen nimi: ", kohteen.nimi),
              type = "box")
  })

  output$ruudunSyvyysHist <- renderPlotly({
    linjat_filtered_63() %>%
      plot_ly(x = ~arviointiruudun.syvyys,
              type = "histogram") %>%
      layout(title = "Ruudun syvyys",
             xaxis = list(title = "Pisteiden määrä"),
             yaxis = list(title = "Ruudun syvyys"))
  })
    
  output$etaisyysLinjallaPlot <- renderPlotly({
    kart_df() %>%
      plot_ly(y =~arviointiruudun.etaisyys,
              color = ~sukelluslinjan.kartoittaja,
              hoverinfo = "text",
              text = ~paste("Kohteen nimi: ", kohteen.nimi),
              type = "box") 
  })
  
  # Kartat
  output$kokoomalinjaMap <- renderLeaflet({leaflet(kokoomalinjat()) %>%
    addTiles() %>%
    addAwesomeMarkers(~alkukoordinaatti.E, 
                      ~alkukoordinaatti.N,
                      #icon = "arrow-down-circle",
                      popup = ~as.character(kohteen.nimi)) %>%
    addMarkers(~loppukoordinaatti.E,
               ~loppukoordinaatti.N,
               popup = ~as.character("Loppu"),
               label = ~as.character(kohteen.nimi)) %>%
    setView(23, 63, zoom = 5)
  })  
  
  output$lajikartta <- renderLeaflet({
    
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)

