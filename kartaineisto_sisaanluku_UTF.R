

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
options(shiny.maxRequestSize=100*1024^2,
        digits = 6)

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
                          
                          tags$hr(),
                          
                          # Input: Select quotes ----
                          numericInput(inputId = "tyhj_rivien_maara",
                                       label = p("Tyhjien rivien määrä taulukon yläreunassa:"),
                                       value = 5,
                                       min = 0,
                                       max = 10
                                       ),
                        
                          tags$hr(),
                          
                          paste("Tämä ohjelma on tarkoitettu VELMU-muodossa olevan kartoitusaineiston tarkistamiseen.",
                                "Välilehdet on asetettu järjestykseen, jossa aineistoa kannattaa käydä läpi:"),
                          p(""),
                          
                          p("1. Yleiskatsaus aineistoon ja sijainteihin"),
                          p("2. Poikkeavuuksien ja selkeiden virheiden etsintä kuvaajien avulla"),
                          p("3. Lajihavaintojen läpikäynti"),
                          p("4. Sijaintien tarkempi läpikäynti")
                          
                          ),
                        
                        
                        
                        # Main panel for displaying maps and tab data ----
                        mainPanel(
                          h2("Linjat kartalla"),
                          
                          # Output: Data file ----
                          
                          leafletOutput(outputId = "kokoomalinjaMap",
                                        height = "600px"),
                          tags$hr(),
                          h2("Kokoomalinjojen yleiskatsaus"),
                          DTOutput(outputId = "yleiskatsaus_kokoomalinjat")
                          )
                        )
                      ),
             
             # Kuvaajat ----
             tabPanel("Kuvaajat",
                      
                      sidebarLayout(
                        
                        sidebarPanel(h4("Poikkeavuuksien etsintä kuvaajien avulla"),
                                     tags$hr(),
                                     paste("Tältä välilehdeltä on tarkoitus poimia selkeät virheet. Näitä saattavat olla",
                                           "virheet mittayksikössä (cm vs. m) tai esimerkiksi lyöntivirheistä johtuvat",
                                           "ylimääräiset nollat.")),
                        mainPanel(tags$hr(),
                                  wellPanel(plotlyOutput(outputId = "ruudunSyvyysBoxPlot")),
                                  wellPanel(plotlyOutput(outputId = "ruudunSyvyysHist")),
                                  tags$hr(),
                                  wellPanel(plotlyOutput(outputId = "etaisyysLinjallaPlot")),
                                  tags$hr(),
                                  wellPanel(plotlyOutput(outputId = "lajinKorkeusBoxPlot")),
                                  tags$hr(),
                                  wellPanel(plotlyOutput(outputId = "lajinPeittavyysBoxPlot")),
                                  tags$hr(),
                                  wellPanel(plotlyOutput(outputId = "secchiSyvyysBoxPlot")),
                                  tags$hr(),
                                  wellPanel(plotlyOutput(outputId = "syvyydenKorjausBoxPlot")),
                                  tags$hr(),
                                  wellPanel(plotlyOutput(outputId = "vedenLampotilaBoxPlot")))
                        )
                      ),
             
             # Tables to show summaries and unique values ----
             tabPanel('Taulukot',
                      fluidRow(
                        column(2, 'sidebar'),
                        column(10,
                               h3("Uniikit arvot kategorisista muuttujista:"),
                               p(paste("Kuhunkin listaan on kerätty uniikit arvot kyseisestä sarakkeesta.",
                                       "Koodien osalta tarkasta tarvittaessa excelistä mihin kyseinen numero viittaa.",
                                       "Taulukossa on kaikki rivit (kokooma & kartoituspisteet), joten sarakkeista löytyy myös tyhjiä (NA) arvoja.",
                                       "Näistä ei tarvitse välittää.")),
                               wellPanel(verbatimTextOutput(outputId = "kat_variables")),
                               
                               h3("Yhteenvedot jatkuvista muuttujista"),
                               p(paste("Kustakin sarakkeesta yhteenveto, jossa minimi- ja maksimiarvot kyseisestä sarakkeesta.",
                                       "NA viittaa tyhjään soluun, ja kertoo tyhjien arvojen määrän. Mikäli numeerinen sarake on 'character'-muodossa,",
                                       "tämä viittaa siihen, että kyseisessä sarakkeessa on teksti-muodossa olevia tietoja myös Excelissä.",
                                       "Esimerkiksi ',' ja '.' käyttö desimaaleina sekaisin aiheuttaa tämän.")),
                               p(paste("Mikäli sarake on tyhjä, R lukee sarakkeen loogisena TRUE/FALSE-muodossa",
                                       "Loogisia sarakkeita on esimerkiksi harvinaisempien pohjanlaatujen kohdalla.",
                                       "Näihin ei tarvitse kiinnittää sen enempää huomiota.")),
                               h4("Kokoomalinjat"),
                               p(paste("Kokoomalinjojen tiedoissa ei tulisi olla tyhjiä arvoja.")),
                               wellPanel(verbatimTextOutput(outputId = "summary_var")),
                               h4("Arviointiruudut"),
                               p(paste("Arviointiruutujen osalta tyhjiä arvoja löytyy ainakin pohjanlaaduista.")),
                               wellPanel(verbatimTextOutput(outputId = "summary_ruudut")))
                        )
                      ),
             # Tab to check on species data ----
             tabPanel("Lajidata",
                      fluidRow(
                        column(6,
                               # Lajihavaintojen tarkistuksia ----
                               h4("Lajihavainnot ja havaintojen määrä taulukossa:"),
                               p(paste("Kiinnitä erityistä huomiota lajeihin, joita on 1-2 kpl.", 
                                       "sillä näissä yksittäisten virheellisten syöttöjen mahdollisuus on suurin.")),
                               wellPanel(DTOutput(outputId = "lajilista")),
                               
                               
                               h4("Lista lajeista, jotka eivät ole yhteensopivia LajiGISsin kanssa:"),
                               p("Mikäli alla olevassa listassa on lajeja: "),
                               p(paste("A) Lajinimeä ei löydy lomakkeen lajilistasta. Lomakkeen lista ei ole täysin yhtäläinen LajiGISsin lajilistan ",
                                       "kanssa, joten laji saattaa löytyä tietokannasta, vaikka sitä ei Excelissä olisikaan.",
                                       "Tällöin kartoitustaulukossa lajihavainnon kohdalla tulisi lukea 'Lajia ei listassa'",
                                       "ja lajihavainto kommenteissa. Nämä tarkastetaan sisäänsyötön yhteydessä.")),
                               p(paste("B) Lajin kirjoitusasu on jotenkin väärin. Tarkasta erityisesti ISOT ja pienet kirjaimet",
                                       "sp.-päätteen kirjoitusasu, ylimääräiset välilyönnit (myös lajin lopussa) ja yleiset typot.")),
                               wellPanel(DTOutput(outputId = "lajitasmaavyys"))
                               ),
                        
                        column(6,
                               h4("Lajistokartta"),
                               p("Kartasta voit tarkistaa eri lajihavaintojen sijainnit."),
                               wellPanel(leafletOutput(outputId = "lajikartta",
                                                       height = "600px")),
                               wellPanel(selectInput(inputId = "filt_laji",
                                                     label = "Suodata kartalla näytettävät lajit:",
                                                     choices = character())))
                        ),
                      fluidRow(
                        column(12,
                               h4("Outlier-lajit"),
                               p(paste("Alla olevassa listassa on lajihavainnot, jotka ovat syvemmällä tai matalammalla kuin 98%",
                                       "kaikista aiemmista saman lajin havainnoista. Tämä lista perustuu siis 'todennäköisyyksiin',",
                                       "ja sen lajihavainnoissa ei välttämättä ole mitään kummallista.",
                                       "Tarkoituksena on kuitenkin kiinnittää huomiota havaintoihin, jotka ovat",
                                       "syvällä tai matalalla suhteessa edellisten vuosien havaintoihin.",
                                       "Tällaisia voisivat olla esimerkiksi Fucus 10 metrissä, tai Sphacelaria 0.2 metrissä.",
                                       "Mikäli näitä löytyy, kannattaa lajihavainnon konteksti aina tarkistaa excelistä.")),
                               wellPanel(DTOutput(outputId = "outlier_spec"))
                               )
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
                               h2("Linjat kartalla"),
                               wellPanel(leafletOutput(outputId = "linjakartta")))
                        )
                      ),
             # Whole table -----
             tabPanel("Koko taulukko",
                      fluidRow(
                        column(12,
                               h2("Koko taulukko"),
                               wellPanel(DTOutput(outputId = "taulukko")
                                         )
                               )
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
    
    #if (!is.null(input$file1)){
    #  read.csv(inFile$datapath, header=input$header, sep=input$sep, 
    #           quote=input$quote)
    
    #req(input$file1)
    if (!is.null(input$file1)){
      df_to_mod <- read_xlsx(input$file1$datapath,
                             skip = input$tyhj_rivien_maara,
                             .name_repair = "universal",
                             guess_max = 5000)
    }
    else {
      df_to_mod <- read_xlsx("C:/Users/Rasmusbo/Documents/R_shiny/Testi_data/Testilinjat_nonames.xlsx",
                             skip = 5,
                             .name_repair = "universal")
    }
    
    aineisto <- df_to_mod %>%
      select(kohteen.nro = 1,
             kohteen.taso = 2,
             kartoituksen.tarkoitus = 3, 
             kohteen.nimi = 4, 
             alkukoordinaatti.N = 5, # 5-8 vain kokoomalinjoilla (62)
             alkukoordinaatti.E = 6,
             loppukoordinaatti.N = 7,
             loppukoordinaatti.E = 8,
             ruudun.koordinaatti.N = as.numeric(9),
             ruudun.koordinaatti.E = as.numeric(10),
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
  
  linjatiedot_kartalle <- reactive({
    linjat_filtered_63() %>%
      select(kohteen.nimi,
             sukelluslinjan.kartoittaja,
             ruudun.koordinaatti.N,
             ruudun.koordinaatti.E,
             arviointiruudun.syvyys,
             lajihavainto,
             90:109) %>%
      group_by(across(c(-lajihavainto))) %>%
      summarise(lajisto = toString(lajihavainto)) %>%
      ungroup()
  })
  
  # Dataframe to check outlier species based on quantile percentages:
  outlier_species <- reactive({
    
    combine_summary <- linjat_filtered_63() %>%
      left_join(vesikasvirajat, by = c("lajihavainto"  = "Laji"))
    
    outliers <- combine_summary %>%
      mutate(Outlier = case_when(arviointiruudun.syvyys < min_1pros ~ "Matala",
                                 arviointiruudun.syvyys > max_1pros ~ "Syva",
                                 TRUE ~ "NA")) %>%
      select(kohteen.nro,
             kohteen.nimi,
             sukelluslinjan.kartoittaja,
             kohteen.nimi,
             kartoituspvm,
             lajihavainto,
             arviointiruudun.syvyys,
             lajin.peittavyys,
             Outlier,
             Lajin_90_pros_esiintyvyysrajat,
             min_1pros,
             max_1pros) %>%
      filter(Outlier != "NA")
    
    return(outliers)
    
  })

  # List and count of species
  lajilista <- reactive({
    linjat_filtered_63() %>%
      select(lajihavainto) %>%
      group_by(lajihavainto) %>%
      tally(sort = T)
  })
  
  lajisto_kartalle <- reactive({
    linjat_filtered_63() %>%
      filter(lajihavainto %in% input$filt_laji) %>%
      mutate(ruudun.koordinaatti.E = as.numeric(ruudun.koordinaatti.E), 
             ruudun.koordinaatti.N = as.numeric(ruudun.koordinaatti.N))
  })
  
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
    #kart_df()
    linjatiedot_kartalle()
  })
  
  # Unique values from categorical variables
  output$kat_variables <- renderPrint({
    
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
    
    cate_vari <- map(cat_var, unique) 
    print(cate_vari)
    
  })
  
  output$summary_ruudut <- renderPrint({
    
    summ_ruudut <- linjat_filtered_63() %>%
      select(ruudun.koordinaatti.N,
             ruudun.koordinaatti.E,
             kartoituspvm,
             tuulen.voimakkuus,
             arviointiruudun.syvyys,
             arviointiruudun.etaisyys,
             90:109,
             lajin.peittavyys,
             lajin.lukumaara,
             lajin.korkeus)
    
    summ_ruudut_displ <- map(summ_ruudut, summary)
    print(summ_ruudut_displ)
    
  })
  
  output$summary_var <- renderPrint({
    
    summ_var <- kokoomalinjat() %>%
      select(alkukoordinaatti.N,
             alkukoordinaatti.E,
             loppukoordinaatti.N,
             loppukoordinaatti.E,
             kartoituspvm,
             tuulen.voimakkuus,
             sukelluslinjan.pituus,
             sukelluslinjan.kompassisuunta,
             sukelluslinjan.alkusyvyys,
             sukelluslinjan.loppusyvyys,
             sukelluslinjan.syvyyden.korjaus)
    
    summ_vari <- map(summ_var, summary)
    print(summ_vari)
    
  })
  
  # Lajisto 
  output$lajilista <- renderDT({
    lajilista()
  })
  
  # Tarkastetaan tasmaako kaikki lajit tietokannan lajilistaan
  output$lajitasmaavyys <- renderDT({
    kart_df() %>%
      filter(!lajihavainto %in% hertan_lajinimet & !is.na(lajihavainto)) %>%
      select(kohteen.nro, kohteen.nimi, sukelluslinjan.kartoittaja, lajihavainto)
  })
  
  # Datatable outlier-lajistosta, jotka raja-arvojen yli
  output$outlier_spec <- renderDT({
    outlier_species()
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
             xaxis = list(title = "Ruudun syvyys"),
             yaxis = list(title = "Pisteiden määrä"))
    })
    
  output$etaisyysLinjallaPlot <- renderPlotly({
    kart_df() %>%
      plot_ly(y =~arviointiruudun.etaisyys,
              color = ~sukelluslinjan.kartoittaja,
              hoverinfo = "text",
              text = ~paste("Kohteen nimi: ", kohteen.nimi),
              type = "box") %>%
    layout(title = "Etäisyys linjalla",
           xaxis = list(title = "Kartoittaja"),
           yaxis = list(title = "Pisteiden etäisyys linjalla"))
  })
  
  output$lajinPeittavyysBoxPlot <- renderPlotly({
    linjat_filtered_63() %>%
      plot_ly(y = ~lajin.peittavyys,
              hoverinfo = "text",
              text = ~paste("</br> Lajihavainto:", lajihavainto,
                            "</br> Kohteen nimi:", kohteen.nimi),
              type = "box") %>%
    layout(title = "Lajien peittävyydet",
           yaxis = list(title = "Lajien peittävyys")) 
  })
  
# lajinKorkeusBoxPlot
  output$lajinKorkeusBoxPlot <- renderPlotly({
  linjat_filtered_63() %>%
    plot_ly(y = ~lajin.korkeus,
            hoverinfo = "text",
            text = ~paste("</br> Lajihavainto:", lajihavainto,
                          "</br> Kohteen nimi:", kohteen.nimi), 
            type = "box") %>%
    layout(title = "Lajien korkeudet",
           yaxis = list(title = "Lajien korkeus (cm)")) 
})

  output$secchiSyvyysBoxPlot <- renderPlotly({
  linjat_filtered_63() %>%
    plot_ly(y = ~secchi.syvyys,
            hoverinfo = "text",
            text = ~paste("Kohteen nimi:", kohteen.nimi),
            type = "box") %>%
    layout(title = "Secchi-syvyydet")
  })

  output$syvyydenKorjausBoxPlot <- renderPlotly({
  linjat_filtered_63() %>%
    plot_ly(y = ~sukelluslinjan.syvyyden.korjaus,
            hoverinfo = "text",
            text = ~paste("Kohteen nimi:", kohteen.nimi),
            type = "box") %>%
    layout(title = "Syvyyden korjaus metreissä")
})

  output$vedenLampotilaBoxPlot <- renderPlotly({
  linjat_filtered_63() %>%
    plot_ly(y = ~veden.lampotila,
            hoverinfo = ~"text",
            text = ~paste("Kohteen nimi:", kohteen.nimi),
            type="box") %>%
    layout(title = "Veden lampötila")
})

# Kartat
  output$kokoomalinjaMap <- renderLeaflet({leaflet(kokoomalinjat()) %>%
    addTiles() %>%
    addMarkers(~alkukoordinaatti.E,
               ~alkukoordinaatti.N,
               label = ~as.character(paste(kohteen.nimi, "0 m koordinaatti"))) %>%
    addMarkers(~loppukoordinaatti.E,
               ~loppukoordinaatti.N,
               label = ~as.character(paste(kohteen.nimi, "100 m koordinaatti")))
  })  
  
  output$lajikartta <- renderLeaflet({leaflet(lajisto_kartalle()) %>%
      addTiles() %>%
      addCircleMarkers(~ruudun.koordinaatti.E, 
                       ~ruudun.koordinaatti.N,
                       popup = paste("Kohteen nimi:", lajisto_kartalle()$kohteen.nimi, "<br>",
                                     "Kartoittaja:", lajisto_kartalle()$sukelluslinjan.kartoittaja, "<br>",
                                     "Pisteen syvyys", lajisto_kartalle()$arviointiruudun.syvyys),
                       color = "coral4",
                       radius = 7,
                       opacity = 0.7,
                       fillOpacity = 0.7) 
  })
  
  output$linjakartta <- renderLeaflet({leaflet(linjatiedot_kartalle()) %>%
      addTiles() %>%
      addCircleMarkers(~as.numeric(ruudun.koordinaatti.E),
                       ~as.numeric(ruudun.koordinaatti.N),
                       popup = paste("Kartoittaja:", linjatiedot_kartalle()$sukelluslinjan.kartoittaja, "<br>", 
                                     "Ruudun syvyys:", linjatiedot_kartalle()$arviointiruudun.syvyys, "<br>",
                                     "Lajisto:" , linjatiedot_kartalle()$lajisto)
                       )
    })

}


# Run the app ----

shinyApp(ui = ui, server = server)

