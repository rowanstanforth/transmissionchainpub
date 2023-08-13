## load relevant packages

library(htmltools)
library(shiny)
library(shinyjs)
library(DT)
library(lubridate)
library(RMySQL) 
library(shinyWidgets)
library(dplyr)

##Connect to SQL database on your own local server to access and store the data

conreal <- dbConnect(RMySQL::MySQL(), host = "localhost", dbname = "transmissionchain", user = "root", password = "")

## Create the specific connections on the database for my text files and for my data table to store

data <- dbGetQuery(conreal, "SELECT * FROM textsource")
maindata <- dbGetQuery(conreal, "SELECT * FROM maindata")

## CSS code that determines the design of the web page. 

css <- "
body {
  background-color: #E0EAFC;
  font-family: 'Roboto', sans-serif;
}

h1, h2, h3, h4, h5, h6 {
  font-weight: 500;
}

.content {
  background-color: #ffffff;
  padding: 30px;
  border-radius: 5px;
  box-shadow: 0px 0px 10px rgba(0, 0, 0, 0.2);
  max-width: 800px;
  margin: 50px auto;
}

.btn-primary {
  background-color: #007bff;
  border-color: #007bff;
}

.btn-primary:hover {
  background-color: #0069d9;
  border-color: #0062cc;
}

.header {
  background-color: #F2F7FD;
  padding: 20px;
}

.footer {
  background-color: #F2F7FD;
  padding: 10px;
}"

##################################################
## User interface code

## This code describes what the user sees. It breaks down how the pages will be structured,
## the text they will contain, the questions and the buttons. 


ui <- fluidPage(
  
  tags$head(
    tags$style(
      HTML("
        body {
          background-color: #E0EAFC;
          font-family: 'Roboto', sans-serif;
        }
        
        h1, h2, h3, h4, h5, h6 {
          font-weight: 500;
        }
        
        .content {
          background-color: #ffffff;
          padding: 30px;
          border-radius: 5px;
          box-shadow: 0px 0px 10px rgba(0, 0, 0, 0.2);
          max-width: 800px;
          margin: 50px auto;
        }
        
        .btn-primary {
          background-color: #007bff;
          border-color: #007bff;
        }
        
        .btn-primary:hover {
          background-color: #0069d9;
          border-color: #0062cc;
        }
        
        .header {
          background-color: #F2F7FD;
          padding: 20px;
        }
        
        .footer {
          background-color: #F2F7FD;
          padding: 10px;
        } 
          
          ")
    ),
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css?family=Roboto")
  ),
  
  tags$div(class = "header", style = "font-size: 2em;", "L'histoire", textOutput("id_text"))
  ,
  
  shinyjs::useShinyjs(),
  shinyjs::inlineCSS(css),
  ## disabling this line blow will allow the navbar at the top to work, and participants could move through the tabs that way
  tags$head(tags$style(HTML(".nav-tabs>li>a { pointer-events: none; }"))),
  tags$div(class = "content",
           
           
           tabsetPanel(id = "inTabset",
                       
                       tabPanel(title = "Démarrer", value = "panel0",
   
        # Launch page                             
                                fluidPage(  h1(paste0("Instructions")),
                                            br(),
                                            h3(paste0("Merci de votre participation. Veuillez suivre attentivement les instructions des pages suivantes et répondre aux questions le plus précisément possible. 
                                          Naviguez dans les pages à l'aide des boutons 'page suivante', vous ne pourrez pas revenir en arrière. Il n'y a pas de limite 
                                          de temps, ne vous précipitez donc pas. Cela ne devrait pas prendre plus de 10 minutes.")), 
                                            
                                            h3(paste0("Veuillez signer la feuille fournie pour consentir à l'utilisation de vos données dans le cadre de cette étude. 
                                                       Les données sont totalement anonymes et peuvent être récupérées à tout moment en suivant les instructions de la feuille 
                                                       fournie et en indiquant votre numéro d'identification unique." )),
                                            
                                            h3(paste0("Lorsque vous êtes prêt, appuyez sur le bouton 'démarrer'.")),
                                            br(),
                                            br(),
                                            br(),
                                            br(),
          actionButton('start', 'Démarrer'))
                       ),  
     #Page 1                                 
                       tabPanel(title = "Section 1", value = "panel1",
                                fluidPage(  h1(paste0("Informations personnelles")),
                                            h2(paste0("Veuillez compléter les informations suivantes:")),
                                            selectInput("sn", "Vous êtes étudiant?", choices = c("","Oui", "Non"), selected = NULL),
                                            conditionalPanel(condition = "input.sn == 'Oui'", selectInput("qu", "Quelle université?", choices = c("", "Université de Montpellier", "Université Paul-Valéry-Montpellier", "Autre université"))),
                                            conditionalPanel(condition = "input.sn == 'Oui'", selectInput("qs", "Quel sujet?", choices = c("","Médecine", "Sciences", "Arts", "Sciences humaines", "Autre"
                                            ))),
                                            selectInput("agem", "Mois de naissance:",
                                                        choices = list("", "Janvier" = "01", "Février" = "02", "Mars" = "03",
                                                                       "Avril" = "04", "Mai" = "05", "Juin" = "06",
                                                                       "Juillet" = "07", "Août" = "08", "Septembre" = "09",
                                                                       "Octobre" = "10", "Novembre" = "11", "Décembre" = "12")),
                                            selectInput("agey", "Année de naissance:",
                                                        choices = c("", seq(2005, 1950, -1)),
                                                        selected = NULL),
                                            selectInput("sexe", "Sexe:", choices = c("", "Femme", "Homme", "Intersex", "Autre")),
                                            conditionalPanel(condition = "input.sexe == 'Autre'", textAreaInput("sexeautre", "Si vous le souhaitez, veuillez indiquer comment vous préférez décrire votre sexe", rows=1, cols=30)),
                                            
                                            selectInput("genre", "Genre:", choices = c("", "Femme", "Homme", "Transgenre homme", "Transgenre femme", "Non-Binaire", "Autre")),
                                            conditionalPanel(condition = "input.genre == 'Autre'", textAreaInput("genreautre", "Si vous le souhaitez, veuillez indiquer comment vous préférez décrire votre genre", rows=1, cols=30)),
                                            selectInput("edu", "Niveau d'études maximum que vous avez déjà atteint:", choices= c("","Sans diplome", "Certificat d-etude primaire (CEP)", "Troisieme", "CAP/BEP/BEPC/BEPS/Brevet élémentaire", "Brevet de Technicien, Brevet prof, BEI, BEC, BEA", "Baccalaureat technologique ou professionnel", "Bac general", "Bac +1", "Bac +2","Bac +3","Bac +4","Bac +5")),
                                            selectInput("socio", "Vos parents ont-ils un diplôme universitaire?", choices = c("","Un", "Les deux", "Aucun")),
                                            actionButton('jumpToP2', 'Page suivante'))
        # Page 2               
        ),   
                       
                       tabPanel(title = "Section 2", value = "panel2",
                                
                                fluidPage(  h1(paste0("L'histoire")),
                                            h2(paste0("Veuillez lire attentivement ce texte une seule fois. Cliquez sur le bouton 'page suivante' lorsque vous êtes prêt.")),
                                            br(),
                                            br(),
                                            uiOutput("intro_output", style = "font-size: 18px;"),
                                            br(),
                                            uiOutput("para1_output", style = "font-size: 18px;"),
                                            br(),
                                            uiOutput("para2_output", style = "font-size: 18px;"),
                                            br(),
                                            uiOutput("para3_output", style = "font-size: 18px;"),
                                            br(),
                                            uiOutput("outro_output", style = "font-size: 18px;"),
                                            br(),
                                            actionButton('jumpToP3', 'Page suivante'))
     # Page 3                  
     ),
                       
                       tabPanel(title = "Section 3", value = "panel3",
                                fluidPage(
                                  h1(paste0("Test")),
                                  h2(paste0(
                                    "Veuillez rédiger le texte que vous venez de lire, le 
                                    plus précisément possible, comme si vous racontiez 
                                    l'histoire à un ami. Utilisez des phrases complètes 
                                    et essayez d'écrire tout ce dont vous vous souvenez. 
                                    Lorsque vous aurez terminé, appuyez sur le bouton 'page suivante'."
                                  )),
                                  br(),                                  
                                  textAreaInput("write", "Votre texte ici:", value = "", width = "1000px", height = "500px"),
                                  actionButton('jumpToP4', 'Page suivante')
                                )
                                
                       ),
                       
        # Page 4               
                       tabPanel(title = "Section 4", value = "panel4", 
                                fluidPage(  h1(paste0("Questionnaire")),
                                            h2(paste0("Veuillez compléter les informations suivantes:")),
                                            br(),
                                            selectInput("uca", "Utilisez-vous actuellement des moyens de contraception? Par ex. préservatif, pilule...", choices = c("","Oui", "Non"), selected = NULL),
                                            conditionalPanel(condition = "input.uca == 'Oui'", checkboxGroupInput("tc", "Types de contraceptifs utilisés (Vous pouvez choisir plusieurs options):", choices = c("Préservatif", "Pillule estroprogestative (pilule combinée)","Pilule progestative", "pilule (type exact inconnu)", "Dispositif intra-utérin (DIU) au cuivre", "Dispositif intra-utérin hormonal (DIU)", "Patch contraceptif", "Anneau vaginal", "Injection contraceptive", "Implant contraceptif", "Autre"), selected=NULL)),
                                            selectInput("ucp", "Avez vous utilisé des contraceptifs dans le passé?", choices = c("","Oui", "Non")),
                                            conditionalPanel(condition = "input.ucp == 'Oui'", checkboxGroupInput("tcp", "Types de contraceptifs utilisés (Vous pouvez choisir plusieurs options):", choices = c("Préservatif", "Pillule estroprogestative (pilule combinée)","Pilule progestative", "pilule (type exact inconnu)", "Dispositif intra-utérin (DIU) au cuivre", "Dispositif intra-utérin hormonal (DIU)", "Patch contraceptif", "Anneau vaginal", "Injection contraceptive", "Implant contraceptif", "Autre"), selected=NULL)),
                                            conditionalPanel(condition = "input.uca == 'Oui'", selectInput("cc", "Avez-vous envisagé de changer de mode de contraception (au cours des trois derniers mois)?", choices = c("","Oui", "Non"))),
                                            selectInput("pc", "Est-ce qu'une méthode contraceptive est utilisée par votre partenaire?", choices = c("","Oui", "Non","Je n'ai pas de partenaire")),
                                            conditionalPanel(condition = "input.pc == 'Oui'", checkboxGroupInput("tpc", "Quelle est la méthode (Vous pouvez choisir plusieurs options):", choices = c("Préservatif", "Pillule estroprogestative (pilule combinée)","Pilule progestative", "pilule (type exact inconnu)", "Dispositif intra-utérin (DIU) au cuivre", "Dispositif intra-utérin hormonal (DIU)", "Patch contraceptif", "Anneau vaginal", "Injection contraceptive", "Implant contraceptif", "Autre"), selected=NULL)),
                                            conditionalPanel(
                                              condition = "input.uca == 'Oui' || input.ucp == 'Oui'",
                                              checkboxGroupInput("sec", "Avez-vous déjà subi personnellement des effets secondaires de la contraception dans le passé et/ou actuellement? Vous pouvez choisir plusieurs options.", 
                                                                 choices = c("Oui, dans le passé", "Oui, actuellement", "Non"))),
                                            conditionalPanel(condition = "input.sec.includes('Oui, dans le passé') || input.sec.includes('Oui, actuellement')",
                                                             selectInput("stopcozside", "Avez-vous cessé d'utiliser ce contraceptif en raison de ces effets secondaires?",
                                                                         choices = c("", "Oui", "Non"))),
                                            conditionalPanel(condition = "input.sec.includes('Oui, dans le passé') || input.sec.includes('Oui, actuellement')",
                                                             selectInput("sevside", "Quelle a été la sévérité de ces effets secondaires?",
                                                                         choices = c("", "Fort", "Modéré", "Léger"))),
                                            
                                            selectInput("sexuality", "Quelle est votre orientation sexuelle ?", choices = c("","Hétérosexuel", "Homosexuel", "Bisexuel", "Pansexuel", "Asexuel", "Autre")),
                                            conditionalPanel(condition = "input.sexuality == 'Autre'", textAreaInput("sexuala", "Si vous le souhaitez, veuillez indiquer comment vous préférez décrire votre orientation sexuelle", rows=1, cols=30)),
                                            selectInput("child", "Prévoyez-vous d'avoir des enfants dans les 2, 5 ou 10 prochaines années?", choices = c("","Non","Oui, dans les 2 prochaines années","Oui, dans les 5 prochaines années","Oui, dans les 10 prochaines années", "Oui, mais pas dans les 10 prochaines années", "j'ai des enfants")),
                                            br(),
                                            br(),
                                            br(),
                                            br(),
                                            br(),
                                            actionButton("finish_btn", "Terminer")
                                            ,
                                            useShinyjs(),
                                            tags$head(tags$style(HTML('.navbar-nav a {cursor: default}')))
                                )),
       # Page 5                
                       tabPanel(title = "Fin", value = "panel5",
                                fluidPage(
                                  h1(paste0("Merci")),
                                  h2(paste0("Merci beaucoup d'avoir répondu à 
                                            l'enquête. Veuillez noter qu'aucunes des 
                                            informations relatives à la santé ou à 
                                            la technologie abordées dans cette 
                                            enquête n'est réelle et ne doit être 
                                            considérée comme une source d'information."))
                                  
                                  
                                )))))

#################################################### server #####################

server <- function(input, 
                   output, session) {
  
  ##Generate session ID
  ##This code generates a unique session id, using today's date and then a random number between 1 and 1000. The code checks the data table to see if the ID number already exists, and if it does then it will generate another one.
  
  generate_id <- function() {
    id_unique <- FALSE
    while (!id_unique) {
      id <- paste0("", sample(100:10000, 1), "_", format(Sys.Date(), "%y%j"))
      query <- sprintf("SELECT COUNT(*) FROM maindata WHERE sessionid = '%s'", id)
      result <- dbGetQuery(conreal, query)
      if (result[[1]] == 0) {
        id_unique <- TRUE
      }
    }
    return(id)
  }
  
  ## Store the session ID in a reactive value, meaning that it is generated whenever the app loads.
  
  session_id <- reactive(generate_id())
  output$id_text <- renderText(paste("Votre numéro d'identification unique est:", session_id()))
  
  # Insert submitted data into the new table
  
  ## storing the data from the first page of the survey, e.g. age, sex, date

  observeEvent(input$jumpToP2, {
    
    ## this code allows users to type single quotes without messing with the SQL code to store the data
    sexeautre_input <- gsub("'", "''", input$sexeautre)
    genreautre_input <- gsub("'", "''", input$genreautre)
    qs_input<-gsub("'", "''", input$qs)
    
    ## specifys which part of the inputted data goes in to which column within the data table on the server
    query1 <- sprintf(
      "INSERT INTO maindata (sessionid, treatment, date, timesubmits1, student, university, subject, birth_month, birth_year, gender, genreautre, sex, sexeautre, parent_degree, education_level) VALUES ( '%s','%s', CURDATE(), CURTIME(), '%s','%s','%s', '%s','%s','%s', '%s','%s', '%s', '%s', '%s')",
      session_id(),
      selected_treatment(),
      input$sn,
      input$qu,
      qs_input,
      input$agem,
      input$agey,
      input$genre,
      genreautre_input,
      input$sexe,
      sexeautre_input,
      input$socio,
      input$edu
    )
    dbSendQuery(conreal, query1)
  })
  
  ## store the time in the data table when the user presses the button 'jump to page 3'
  
  observeEvent(input$jumpToP3, {
    query11<- sprintf(
      "UPDATE maindata SET timesubmits2 = CURTIME() WHERE sessionid = '%s'",
      session_id()
    )
    dbSendQuery(conreal, query11)
  })
  
  ## escape single quotes for the text that the users have written
  
  observeEvent(input$jumpToP4, {
    write_input <- gsub("'", "''", input$write) 
    selected_treatment <- isolate(selected_treatment())
    para1_input <- gsub("'", "''", get_para1(selected_treatment))
    para2_input <- gsub("'", "''", get_para2(selected_treatment))
    para3_input <- gsub("'", "''", get_para3(selected_treatment))
    
  ## store the text that users have written, along with the paragraphs that they were shown
    
    query2 <- sprintf(
      "UPDATE maindata SET timesubmits3=CURTIME(), textoutput = '%s', textinputp1 = '%s', textinputp2='%s', textinputp3='%s' WHERE sessionid = '%s'",
      write_input,
      para1_input,
      para2_input,
      para3_input,
      session_id()
    )
    dbSendQuery(conreal, query2)
  })
  
  ## code that will collapse values in to a comma separated string, for multiple check box options
  
  observeEvent(input$finish_btn, {
    
    # Checkboxes for types of contraceptives used
    
    if (is.null(input$tc)) {
      tc <- NA
    } else {
      tc <- paste(input$tc, collapse = ",")
    }
    
    if (is.null(input$tcp)) {
      tcp <- NA
    } else {
      tcp <- paste(input$tcp, collapse = ",")
    }
    
    # Checkboxes for types of contraceptives used by partner
   
     if (is.null(input$tpc)) {
      tpc <- NA
    } else {
      tpc <- paste(input$tpc, collapse = ",")
    }
    
    ## dealing with conditional values and potentially empty answers.
    
    query_values <- list(
      timesubmits4 = "CURTIME()",
      usecontranow = ifelse(is.null(input$uca), NA, input$uca),
      typenow = ifelse(!is.null(input$uca) && input$uca == "Oui", ifelse(!is.null(input$tc), tc, NA), NA),
      usecontrapast = ifelse(is.null(input$ucp), NA, input$ucp),
      typepast = ifelse(!is.null(input$ucp) && input$ucp == "Oui", ifelse(!is.null(input$tcp), tcp, NA), NA),
      changecontra = ifelse(!is.null(input$uca) && input$uca == "Oui", ifelse(is.null(input$cc), NA, input$cc), NA),
      partner = gsub("'", "''", ifelse(is.null(input$pc), NA, input$pc)), # escape single quotes
      type_partner = ifelse(!is.null(input$pc) && input$pc == "Oui", ifelse(!is.null(input$tpc), tpc, NA), NA),
      sideffects = ifelse(length(input$sec) > 0, paste(input$sec, collapse = ","), NA),
      stopcozside =input$stopcozside,
      sevside=input$sevside,
      sexuality = ifelse(is.null(input$sexuality), NA, input$sexuality),
      sexualityautre = gsub("'", "''", ifelse(is.null(input$sexuala), NA, input$sexuala)),
      child = gsub("'", "''", ifelse(is.null(input$child), NA, input$child))
    )
    
    # Replace NULL values with 'NULL' in query values list
    query_values[sapply(query_values, is.null)] <- "NULL"
    
    # Construct the SQL query to upload the data to the table
    
    query4 <- sprintf(
      "UPDATE maindata SET timesubmits4 = CURTIME(), usecontranow = '%s', typenow = '%s', usecontrapast = '%s', typepast = '%s', changecontra = '%s',partner = '%s',type_partner = '%s',sideffects = '%s',stopcozside ='%s', sevside='%s',sexuality = '%s', sexualityautre='%s', child = '%s' WHERE sessionid = '%s'",
      query_values$usecontranow,
      query_values$typenow,
      query_values$usecontrapast,
      query_values$typepast,
      query_values$changecontra,
      query_values$partner,
      query_values$type_partner,
      query_values$sideffects,
      query_values$stopcozside,
      query_values$sevside,
      query_values$sexuality,
      query_values$sexualityautre,
      query_values$child,
      session_id()
    )
    dbSendQuery(conreal, query4)
  })
  
  # get text function
  
  ## Function to get the treatment with the lowest count
  
  get_lowest_count_treatment <- function() {
    ## Query the database to get the treatment counts
    counts <- dbGetQuery(conreal, "SELECT treatment, count FROM textsource")
    
    ## Sort the counts in ascending order
    counts_sorted <- counts[order(counts$count),]
    
    ## Find the treatments with the lowest count
    lowest_counts <- counts_sorted[counts_sorted$count == counts_sorted[1, "count"], "treatment"]
    
    ## If there is only one treatment with the lowest count, return it
    if (length(lowest_counts) == 1) {
      return(lowest_counts)
    }
    
    ## If there are multiple treatments with the lowest count, choose one at random
    else {
      return(sample(lowest_counts, 1))
    }
  }
  
  ## Define the reactive variable to store the selected treatment
  selected_treatment <- reactiveVal()
  
  ## When the start button is pressed, set the selected treatment
  observeEvent(input$start, {
    selected_treatment(get_lowest_count_treatment())
    print(selected_treatment())
  })
  
  ## Get the treatments with the lowest count when the app starts
  
  lowest_count_treatment <- reactive({
    treatments <- get_lowest_count_treatments()
    
    return(treatments)
  })
  
  ## If there are multiple texts with the same count then choose one at random
  
  random_treatment <- reactive({
    treatments <- lowest_count_treatment()
    random_treatment <- get_random_treatment(treatments)
    
    return(random_treatment)
  })
  
  ##Get the text associated with the chosen treatment and display it
  
  output$para1_output <- renderUI({
    req(input$start)
    text <- dbGetQuery(conreal, paste0("SELECT para1 FROM textsource WHERE treatment = '", isolate(selected_treatment()), "'"))
    dbExecute(conreal, paste0("UPDATE textsource SET count = count + 1 WHERE treatment = '", isolate(selected_treatment()), "'"))
    
    HTML(text$para1)
  })
  
  output$para2_output <- renderUI({
    req(input$start)
    text <- dbGetQuery(conreal, paste0("SELECT para2 FROM textsource WHERE treatment = '", isolate(selected_treatment()), "'"))
    HTML(text$para2)
  })
  
  output$para3_output <- renderUI({
    req(input$start)
    text <- dbGetQuery(conreal, paste0("SELECT para3 FROM textsource WHERE treatment = '", isolate(selected_treatment()), "'"))
    HTML(text$para3)
  })
  
  output$intro_output <- renderUI({
    req(input$start)
    text <- dbGetQuery(conreal, paste0("SELECT intro FROM textsource WHERE treatment = '", isolate(selected_treatment()), "'"))
    HTML(text$intro)
  })
  
  output$outro_output <- renderUI({
    req(input$start)
    text <- dbGetQuery(conreal, paste0("SELECT outro FROM textsource WHERE treatment = '", isolate(selected_treatment()), "'"))
    HTML(text$outro)
  })
  
  # Function to get the para1 associated with the chosen treatment to be stored in the data table and cross reference.
  
  get_para1 <- function(selected_treatment) {
    para1 <- data$para1[data$treatment == selected_treatment]
    
    return(para1)
  }
  
  # Function to get the para2 associated with the chosen treatment
  get_para2 <- function(selected_treatment) {
    para2 <- data$para2[data$treatment == selected_treatment]
    
    return(para2)
  }
  
  # Function to get the para3 associated with the chosen treatment
  get_para3 <- function(selected_treatment) {
    para3 <- data$para3[data$treatment == selected_treatment]
    
    return(para3)
  }
  
  
  # Code to move between pages
  
  # Function to validate inputs and switch to the next tab
  validateAndSwitch1 <- function() {
    # Check if all required fields are filled
    if (isTruthy(input$sn) && isTruthy(input$agem) && isTruthy(input$agey) && isTruthy(input$sexe) && isTruthy(input$genre) && isTruthy(input$socio) && isTruthy(input$edu)) {
      # Switch to the next tab
      updateTabsetPanel(session, "inTabset", selected = "panel2")
    } else {
      # Show an error message if any required fields are missing
      showModal(modalDialog(title = "Erreur", "Veuillez répondre à toutes les questions.", easyClose = TRUE))
    }
  }
  observeEvent(input$jumpToP2, {
    validateAndSwitch1()
  })
  
  # When the 'jumpToP2' button is clicked, validate inputs and switch to the next tab
  observeEvent(input$jumpToP2, {
    validateAndSwitch1()
  })
  observeEvent(input$start, {
    updateTabsetPanel(session, "inTabset",
                      selected = "panel1")
  })
  observeEvent(input$jumpToP3, {
    updateTabsetPanel(session, "inTabset",
                      selected = "panel3")
  })
  
  observeEvent(input$jumpToP4, {
    updateTabsetPanel(session, "inTabset",
                      selected = "panel4")
  })
  
  validateAndSwitch2 <- function() {
    # Check if all required fields are filled
    if (isTruthy(input$uca) && isTruthy(input$ucp) && isTruthy(input$pc) && isTruthy(input$sexuality) && isTruthy(input$child)) {
      # Switch to the next tab
      updateTabsetPanel(session, "inTabset", selected = "panel5")
    } else {
      # Show an error message if any required fields are missing
      showModal(modalDialog(title = "Erreur", "Veuillez répondre à toutes les questions.", easyClose = TRUE))
    }
  }
  observeEvent(input$finish_btn, {
    validateAndSwitch2()
    
  }) 
  
  
  
}

# RUN THE APP
## run the below code to run the app, remember if you are hosting on local server you will need to have 'WAMP' running

shinyApp(ui, server) 

# function to kill connections 
## sometimes if there are too many connections to the database running at the same time then the app will stop. To reset these run the code below.

killDbConnections <- function () {
  
  all_cons <- dbListConnections(MySQL())
  
  print(all_cons)
  for(conreal in all_cons)
    +  dbDisconnect(con)
  
  print(paste(length(all_cons), " connections killed."))
  
}

# Code to specify the IP address and port number. Your IP address comes from the wifi you are connected to. These details are used for other computers to access your survey.
options(shiny.host = "192.168.43.165")
options(shiny.port = 7775) 

## run the app again
shinyApp(ui, server)


