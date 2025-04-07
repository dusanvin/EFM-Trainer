loadKontextSheet <- function() {
  kontext <- googlesheets4::read_sheet(gsheetID,  sheet = "Kontext", col_names = TRUE, col_types = "ccccccccccccccccccccccccccc")
  return(kontext)
}

loadVariablenSheet <- function() {
  variablen <- googlesheets4::read_sheet(gsheetID,  sheet = "Variablen", col_names = TRUE, col_types = "cccccccccccccccccccccccccccccccccccii")
  return(variablen)
}

loadAufgabeSheet <- function() {
  aufgabe <- googlesheets4::read_sheet(gsheetID,  sheet = "Aufgabe", col_names = TRUE, col_types = "ccccccc")
  return(aufgabe)
}

loadANOVAGrpSheet <- function() {
  ANOVAGruppen <- googlesheets4::read_sheet(gsheetID,  sheet = "ANOVA-Gruppen", col_names = TRUE, col_types = "cciccc")
  return(ANOVAGruppen)
}

plotHeightVar <- 1
plotWidthVar <- 0.8


server <- function(input, output, session) {
  
  lickert1DF <- data.frame(
    Variable = c("Vari","","","",""),
    Wert = c (1:5),
    Bedeutung = c("sehr niedrig", "niedrig", "mittel", "hoch","sehr hoch"),
    stringsAsFactors = FALSE
  )
  
  karrierestufeDF <- data.frame(
    Variable = c("erzielte Karrierestufe","","","","","",""),
    Wert = c (1:7),
    Bedeutung = c("Hilfskraft", "Arbeiter / Sachbearbeiter", "Vorarbeiter / Gruppenleiter", "Abteilungsleiter","Bereichsleiter / Experte","Vorstand","Eigentümer / Partner") ,
    stringsAsFactors = FALSE
  )
  
  bildungsabschlussDF <- data.frame(
    Variable = c("erzielter Bildungsabschluss","","","","","",""),
    Wert = c (1:7),
    Bedeutung = c("Grundschule", "Qualifizierter Hauptschulabschluss", "Mittlere Reife", "Abgeschlossene Ausbildung","Abitur / Meisterbrief","Hochschulabschluss","Promotion") ,
    stringsAsFactors = FALSE
  )
  
  
  values <- reactiveValues()
  values$kontext <- data.frame()
  values$variablen <- data.frame()
  values$aufgabe <- data.frame()
  values$ANOVAGruppen <- data.frame()
  values$prereqChecked <- logical()
  values$doU_test <- logical()
  values$showU_testSol <- logical()
  values$doWilcoxon <- logical()
  values$showWilcoxonSol <- logical()
  
  values$users_data <- NULL
  
  scaleTableDF <- reactiveVal()
  scaleText <- reactiveVal()
  uvDF <- reactiveVal()
  t_test <- reactiveVal()
  paired_t_test <- reactiveVal()
  corr_test <- reactiveVal()
  reg_test <- reactiveVal()
  logReg_test <- reactiveVal()
  anova <- reactiveVal()
  chiSq_test <- reactiveVal()
  auspr2PosNeg <- reactiveVal()
  UV <- reactiveVal()
  geschlechtUV <- reactiveVal()
  AV <- reactiveVal()
  AVhatAlt <- reactiveVal()
  AValt <- reactiveVal()
  ANOVAGruppe <- reactiveVal()
  ANOVAGruppe2 <- reactiveVal()
  currentTest <- reactiveVal()
  isTwoFac <- reactiveVal()
  
  show_modal_spinner(spin = "orbit")
  values$kontext <- loadKontextSheet()
  values$variablen <- loadVariablenSheet()
  values$aufgabe <- loadAufgabeSheet()
  values$ANOVAGruppen <- loadANOVAGrpSheet()
  remove_modal_spinner()
  
  onSessionStart = isolate({
    values$users_data$SessionStart <- Sys.time()
    values$users_data$NewTaskCount <- -1
  })
  
  session$onSessionEnded(function() {
    isolate({
      
      users_dataOld <- NULL
      currentCount <- NULL
      validJson <- NULL
      if (file.exists("users_data.json")) {
        file <- reader("users_data.json")
        validJson <- validate(file)
      } else {
        validJson <- FALSE
      }
      
      if (validJson) {
        users_dataOld <- fromJSON("users_data.json") 
        currentCount <- tail(users_dataOld$SessionCount, n = 1)
      } else {
        currentCount = 0
      }
      
      values$users_data$SessionCount <- as.numeric(currentCount+1)
      values$users_data$SessionDuration <- round(as.numeric(difftime(Sys.time(), values$users_data$SessionStart), units = "secs"),0)
      sessionDurationPeriod <- paste(seconds_to_period(as.numeric(values$users_data$SessionDuration)))
      values$users_data$SessionDuration <- paste(toJSON(sessionDurationPeriod))
      values$users_data$SessionStart <- paste(toJSON(Sys.time(), POSIXt="ISO8601"))
      values$users_data <- as.data.frame(values$users_data)
      values$users_data <- values$users_data[,c("SessionCount", "SessionStart","SessionDuration","NewTaskCount")]
      
      newUsersData <- NULL
      if (validJson) {
        newUsersData <- rbind.data.frame(as.data.frame(users_dataOld), values$users_data)
      } else {
        newUsersData <- values$users_data
      } 
      
      users_dataJSON <- toJSON(as.list(newUsersData), force = TRUE)
      write(users_dataJSON, "users_data.json")
    })
  })
  
  observe({
    click("newTaskButton")
  })
  
  helperGetPercentage <- function(value) {
    percentage <- NULL
    value <- as.numeric(value)
    value <- round(value, digits = 3)
    value <- as.character(value)
    if (nchar(value)==4) {
      value <- paste(c(value,"0"), collapse = "")  
    } else if (nchar(as.character(value))==3) {
      value <- paste(c(value,"00"), collapse = "")  
    }
    
    value <- strsplit(value, "\\.")
    valuePerDec <- substr(value[[1]][2], start = 1, stop = 2)
    valuePerFrac <- substr(value[[1]][2], start = 3, stop = 4)
    if (as.character(substr(valuePerDec, start = 1, stop = 1))=="0") {
      valuePerDec <- sub(".", "", valuePerDec)
    }
    if (valuePerFrac=="") {
      percentage <- paste(c(valuePerDec,"%"), collapse = "")  
    } else {
      percentage <- paste(c(valuePerDec, ",", valuePerFrac,"%"), collapse = "")     
    }
    
    return(percentage)
  }
  
  getQuestionText <- function() {
    # questionText <- NULL
    # connector <- NULL
    # text2 <- NULL
    # aufgabe <- values$aufgabe
    # text1Src = aufgabe[aufgabe$Analyse == input$selectTest,c("Thema","Text1","Item_AV")] 
    # text1SrcForPrereq <- data.frame()
    # if(input$checkPrereq == TRUE){ 
    #   text1SrcForPrereq = text1Src[,c("Text1","Item_AV")]
    # } else {
    #   text1SrcForPrereq = text1Src[text1Src$Thema != "Voraussetzung für Methode eingehalten",c("Text1","Item_AV")]
    # }
    # 
    # rndtext1Src = as.data.frame(text1SrcForPrereq["Text1"]) #!(is.na(text1SrcForPrereq)
    # rndtext1Src <- rndtext1Src[,1]
    # rndText1 = sample(rndtext1Src,1)
    # itemav = as.data.frame(text1SrcForPrereq[text1SrcForPrereq$Text1 == rndText1,"Item_AV"])
    # itemav <- itemav[1,1]
    # 
    # if (!is.na(itemav)) {
    #   connector = aufgabe[aufgabe$Text1 == rndText1,"Connector"]
    #   connector <- connector[1, 1]
    #   text2 = aufgabe[aufgabe$Text1 == rndText1,"Text2"]
    #   text2 <- text2[1, 1]
    #   questionText <- paste(c(rndText1, UV(), connector, AV(), text2), collapse = " ")
    # } else {
    #   questionText <- rndText1
    # }
    #return(aufgabeText) 
    
    simpleQuestionText <- "Wie würden Sie diese Ergebnisse berichten?"
    return(simpleQuestionText)
  }
  
  getTaskText <- function() {
    set.seed(NULL) 
    kontext <- values$kontext
    variablen <- values$variablen
    
    #Story
    story = kontext[kontext$Analyse == currentTest(),"Story"] 
    story = as.data.frame(story)
    
    story = story[!(is.na(story))]
    rndstory = sample(story,1)
    kontextclass = kontext[kontext$Story == rndstory & kontext$Analyse == currentTest(),"Kontextclass"]
    kontextclass = as.character(kontextclass[1, 1])
    casusVar1 = kontext[kontext$Story == rndstory & kontext$Analyse == currentTest(),"Casus_Var1"]
    
    #Unabhängige Variable
    ANOVAGruppe <- NULL
    if (currentTest() != "ANOVA") {
      
      uVarsForScale <- NULL
      rndAuspr1 <- NULL
      if (currentTest() == "t-Test"){
        uVarsForScale <- variablen[variablen$Skalierung_UV == "binär",c("Kategorie_UV","Variable_UV","Skalierung_UV")] #nur binäre Variablen
      } else if(currentTest() == "Korrelation"){
        uVarsForScale <- variablen[variablen$Skalierung_UV == "rational" | variablen$Skalierung_UV == "intervall" ,c("Kategorie_UV","Variable_UV","Skalierung_UV")] 
      } else if(currentTest() == "Regression"){
        uVarsForScale <- variablen[variablen$Skalierung_UV == "rational" | variablen$Skalierung_UV == "intervall" ,c("Kategorie_UV","Variable_UV","Skalierung_UV")] 
      } else if(currentTest() == "log. Regression"){
        uVarsForScale <- variablen[variablen$Skalierung_UV == "rational" | variablen$Skalierung_UV == "intervall" ,c("Kategorie_UV","Variable_UV","Skalierung_UV")] #
      } else if(currentTest() == "Chi-Quadrat"){
        uVarsForScale <- variablen[variablen$Skalierung_UV == "binär",c("Kategorie_UV","Variable_UV","Skalierung_UV")] 
      } else if (currentTest() == "Paired t-Test"){
        uVarsForScale <- variablen[variablen$VorherNachher_UV == "1",c("Kategorie_UV","Variable_UV","Skalierung_UV")] 
      }
      
      
      uVarsForContext <- data.frame()
      
      if(currentTest() == "Chi-Quadrat") {
        uVarsForContext = uVarsForScale[uVarsForScale$Kategorie_UV == "Krankheit","Variable_UV"] 
      } else if (kontextclass == "Didaktik & Lernen"){ 
        uVarsForContext = uVarsForScale[uVarsForScale$Kategorie_UV == "Didaktik und Lernen","Variable_UV"] 
      } else {
        uVarsForContext = uVarsForScale[uVarsForScale$Kategorie_UV != "Didaktik und Lernen","Variable_UV"]
      } 
      indVars <- NULL
      
      uvSrc <- uVarsForContext
      uvSrc = as.data.frame(uvSrc)
      uvSrc = uvSrc[!(is.na(uvSrc))]
      
      uvDF <- NULL
      indVars <- NULL
      if(currentTest() == "Regression" | currentTest() == "log. Regression"){
        indVars <- sample(2:4, 1)
        uvDF <- data.frame(matrix(ncol = 3, nrow = 0))#
        colnames(uvDF) <- c("Artikel", "Ausprägung", "Variable")
      } else {
        indVars <- 1
      }
      rndUv <- NULL
      rndAuspr1 <- NULL
      for (i in 1:indVars) { 
        set.seed(NULL)  
        rndUv = sample(uvSrc,1)
        
        set.seed(NULL) 
        if(currentTest() == "Regression" | currentTest() == "Korrelation" | currentTest() == "log. Regression"){
          if (currentTest() == "Regression" | currentTest() == "log. Regression") {
            uvDF[i, "Variable"] = rndUv
          }
          
          if (i>=2) {
            set.seed(NULL) 
            while (any(duplicated(uvDF$Variable)) == TRUE) {
              rndUv = sample(uvSrc,1)  
              uvDF[i, "Variable"] = rndUv
            }
          }
          auspr_basis_pos <- kontext[,"Ausprägung_Basis_positiv"]
          auspr_basis_pos <- auspr_basis_pos[!is.na(auspr_basis_pos)]
          validAuspr <- NULL
          
          for (auspr in auspr_basis_pos) {
            auspr <- paste(c(auspr,"_UV"), collapse = "")
            isvalid <- as.logical(as.numeric(variablen[variablen$Variable_UV == rndUv,auspr])) #rndUv
            if (isvalid) {
              auspr <- substr(auspr, 1, nchar(auspr)-3)
              validAuspr <- c(validAuspr, auspr)
            }
          }
          
          ausprBasis <- NULL
          
          if (is.null(validAuspr)) {
            rndAuspr1 <- ""
          } else {
            ausprBasis <- sample(validAuspr,1)
            rndAuspr1 <- kontext[kontext$Ausprägung_Basis_positiv == ausprBasis,"Ausprägung_Basis_X"]
            rndAuspr1 = as.character(rndAuspr1[1, 1])
          }
        }
        
        "Skalierung_UV"
        skalierungUV = variablen[variablen$Variable_UV == rndUv,"Skalierung_UV"]
        skalierungUV = as.character(skalierungUV[1, 1])
        
        geschlechtUV = variablen[variablen$Variable_UV == rndUv,"Geschlecht_UV"]
        geschlechtUV = as.character(geschlechtUV[1, 1])
        geschlechtUV(geschlechtUV)
        
        pluralUV = variablen[variablen$Variable_UV == rndUv,"Plural_UV"]
        pluralUV = as.character(pluralUV[1, 1])
        
        artikelUV = "ein"
        hatArtikelBOOL <- NULL
        if (casusVar1 == "Dativ") { #Trigger Dativ für Variable1
          hatArtikel <- variablen[variablen$Variable_UV == rndUv,"HatArtikelDativ_UV"]
          rndAuspr1 = paste(c(rndAuspr1, "n"), collapse = "")
          if (hatArtikel == "1") { 
            hatArtikelBOOL = TRUE
            if(geschlechtUV == "f") {
              artikelUV = paste(c(artikelUV, "er"), collapse = "")
            } else if(geschlechtUV == "m") {
              artikelUV = paste(c(artikelUV, "em"), collapse = "")
            } else if(geschlechtUV == "n") {
              artikelUV = paste(c(artikelUV, "em"), collapse = "")
            }
          } else {
            hatArtikelBOOL = FALSE
            artikelUV = ""
          } 
          
        } else if(casusVar1 == "Nominativ"){ # Trigger Nominativ für Variable1
          hatArtikel <- variablen[variablen$Variable_UV == rndUv,"HatArtikelNominativ_UV"]
          if (hatArtikel == "1") { 
            if(geschlechtUV == "f") {
              if (pluralUV == "1") {
                hatArtikelBOOL = FALSE
                artikelUV = ""
              } else {
                hatArtikelBOOL = TRUE
                artikelUV = paste(c(artikelUV, "e"), collapse = "")
              }
            } else if(geschlechtUV == "m"){
              rndAuspr1 = paste(c(rndAuspr1, "r"), collapse = "")
              artikelUV = "ein"
              hatArtikelBOOL = TRUE
            } else if(geschlechtUV == "n"){
              rndAuspr1 = paste(c(rndAuspr1, "s"), collapse = "")
              artikelUV = "ein"
              hatArtikelBOOL = TRUE
            }
          } else {
            if(geschlechtUV == "n"){
              rndAuspr1 = paste(c(rndAuspr1, "s"), collapse = "")
            }
            artikelUV = ""
            hatArtikelBOOL = FALSE
          }
        }
        if(currentTest() == "Regression" | currentTest() == "log. Regression"){
          uvDF[i, "Artikel"] = artikelUV
          uvDF[i, "Ausprägung"] = rndAuspr1
          
        } else {
          UV(rndUv)
        }
      }
      
    } else if (currentTest() == "ANOVA") {
      gruppen <- values$ANOVAGruppen$Name
      gruppen = as.data.frame(gruppen)
      gruppen = gruppen[!(is.na(gruppen))]
      ANOVAGruppe <- sample(gruppen,1)
      ANOVAGruppe(ANOVAGruppe)
      ANOVAGruppe2 <- sample(gruppen,1)
      while (ANOVAGruppe2 == ANOVAGruppe) {
        set.seed(NULL)
        ANOVAGruppe2 <- sample(gruppen,1)
      }
      ANOVAGruppe2(ANOVAGruppe2)
      
    }
    
    
    #Connector1
    conn1ColumnSrc <- NULL
    if(currentTest() == "ANOVA") {
      conn1ColumnSrc = c("Connector1_A","Connector1_C","Connector1_D")
    } else if(currentTest() == "log. Regression" ) {
      conn1ColumnSrc = c("Connector1_A","Connector1_E")
    } else if(currentTest() == "Chi-Quadrat" ) {
      conn1ColumnSrc = "Connector1_A"
    } else {
      if (!(is.na(kontext[kontext$Story == rndstory & kontext$Analyse == currentTest(),"Connector1_D"]))) { 
        conn1ColumnSrc = c("Connector1_A","Connector1_B","Connector1_C","Connector1_D","Connector1_E")
      } else {
        conn1ColumnSrc = c("Connector1_A","Connector1_B","Connector1_C","Connector1_E")
      }
    }
    conn1Column = sample(conn1ColumnSrc,1) #"Connector1_D" #
    rndConn1 = kontext[kontext$Story == rndstory & kontext$Analyse == currentTest(),conn1Column] 
    
    if(currentTest() != "Chi-Quadrat"){
      
      #Abhängige Variable
      avSrc <- NULL
      if (currentTest() == "log. Regression") {
        avSrc = variablen[variablen$Skalierung_AV == "Binär","Variable_AV"] 
      } else if(currentTest() == "ANOVA") {
        avSrc = variablen[variablen$Skalierung_AV == "Rational" | variablen$Skalierung_AV == "Ordinal"| variablen$Skalierung_AV == "Intervall","Variable_AV"] 
      } else if(currentTest() == "Korrelation"){
        avSrc = variablen[variablen$Skalierung_AV == "Rational" | variablen$Skalierung_AV == "Intervall","Variable_AV"] 
      } else if(currentTest() == "t-Test" | currentTest() == "Regression") {
        avSrc = variablen[variablen$Skalierung_AV == "Rational" | variablen$Skalierung_AV == "Intervall","Variable_AV"] 
      } else if (currentTest() == "Paired t-Test") {
        avSrc = variablen[variablen$Skalierung_AV == "Rational" | variablen$Skalierung_AV == "Intervall",c("Variable_AV","VorherNachher_AV")] 
        avSrc = avSrc[avSrc$VorherNachher_AV == "1","Variable_AV"]
      } else {
        avSrc = variablen[,"Variable_AV"] 
      }
      
      avSrc = as.data.frame(avSrc)
      avSrc = avSrc[!(is.na(avSrc))]
      rndAv = sample(avSrc,1)
      #AVtest
      # rndAv <- NULL
      # rndAv <- "durchschnittlichen Jahreseinkommen"
      AV(rndAv)
      
      skalierungAV = variablen[variablen$Variable_AV == rndAv,"Skalierung_AV"]
      skalierungAV = as.character(skalierungAV[1, 1])
      
      geschlechtAV = variablen[variablen$Variable_AV == rndAv,"Geschlecht_AV"]
      geschlechtAV = as.character(geschlechtAV[1, 1])
      
      
      pluralAV = variablen[variablen$Variable_AV == rndAv,"Plural_AV"]
      pluralAV = as.character(pluralAV[1, 1])
      
      #Ausprägung2
      
      auspr_basis_pos <- kontext[,"Ausprägung_Basis_positiv"]
      auspr_basis_pos <- auspr_basis_pos[!is.na(auspr_basis_pos)]
      validAuspr <- NULL
      for (auspr in auspr_basis_pos) {
        auspr <- paste(c(auspr,"_AV"), collapse = "")
        isvalid <- variablen[variablen$Variable_AV == rndAv,auspr]
        isvalid = as.numeric(as.character(isvalid[1, 1]))
        
        
        isvalid <- as.logical(isvalid)
        if (isvalid) {
          auspr <- substr(auspr, 1, nchar(auspr)-3)
          validAuspr <- c(validAuspr, auspr)
        }
      }
      
      rndAuspr2 <- NULL
      if (currentTest() == "ANOVA") { 
        rndAuspr2 <- "unterschiedlich"
      } else {
        
        ausprBasis <- NULL
        if (is.null(validAuspr)) {
          rndAuspr1 <- ""
        } else {
          ausprBasis <- sample(validAuspr,1)
          
          if (rndAv=="Anzahl an Fehlern im Diktat") {
            rndAuspr2 <- kontext[kontext$Ausprägung_Basis_positiv == ausprBasis,c("Ausprägung_Comperativ_aktiv_positiv","Ausprägung_Comperativ_passiv_positiv")]
            rndAuspr2 = as.character(rndAuspr2[1, 1])
          } else {
            if (conn1Column == "Connector1_B") {
              rndAuspr2 <- kontext[kontext$Ausprägung_Basis_positiv == ausprBasis,c("Ausprägung_Comperativ_aktiv_negativ","Ausprägung_Comperativ_passiv_negativ")]
              rndAuspr2 = as.character(rndAuspr2[1, 1])
            } else {
              rndAuspr2 <- kontext[kontext$Ausprägung_Basis_positiv == ausprBasis,c("Ausprägung_Comperativ_aktiv_negativ","Ausprägung_Comperativ_aktiv_positiv","Ausprägung_Comperativ_passiv_negativ","Ausprägung_Comperativ_passiv_positiv")]
              rndAuspr2 = as.character(rndAuspr2[1, 1])
            }
          }
          auspr2Index <- which(kontext==rndAuspr2, arr.ind=TRUE)
          auspr2PosNeg <- colnames(kontext)[auspr2Index[2]]
          if (auspr2PosNeg=="Ausprägung_Comperativ_aktiv_negativ" || auspr2PosNeg=="Ausprägung_Comperativ_passiv_negativ") {
            auspr2PosNeg("negativ")
          } else if (auspr2PosNeg=="Ausprägung_Comperativ_aktiv_positiv" || auspr2PosNeg=="Ausprägung_Comperativ_passiv_positiv") {
            auspr2PosNeg("positiv")
          }
          
        }
      }
      
      AValternative =  variablen[variablen$Variable_AV == rndAv,"Variable_AV_alternativ"]
      if (sum(!is.na(AValternative))==1) {
        AValternative = as.character(AValternative[1, 1])
        AVhatAlt(TRUE)
        AValt(AValternative)
        if (conn1Column == "Connector1_C" || conn1Column == "Connector1_E" | conn1Column == "Connector1_A") {
          if ((currentTest() == "ANOVA" | currentTest() == "Korrelation" | currentTest() == "Regression" | currentTest() == "t-Test") & (conn1Column == "Connector1_A")) {
          } else {
            rndAv = AValternative
          }
        }
        
      } else
      { AVhatAlt(FALSE) }
      
      
      artikelAV <- NULL
      if (conn1Column =="Connector1_A" ||  conn1Column =="Connector1_B" ||  conn1Column =="Connector1_D") { #Dativ
        if(geschlechtAV == "f") {
          if (pluralAV == "0") {
            artikelAV = "einer"
            rndAuspr2 = paste(c(rndAuspr2, "en"), collapse = "")
          } else { 
            artikelAV = ""
            rndAuspr2 = paste(c(rndAuspr2, "en"), collapse = "")
          }
        } else if(geschlechtAV == "n"){
          if (pluralAV == "0") {
            artikelAV = "einem"
            rndAuspr2 = paste(c(rndAuspr2, "en"), collapse = "")
          } else { 
            artikelAV = ""
            rndAuspr2 = paste(c(rndAuspr2, "en"), collapse = "")
          }
        } else if(geschlechtAV == "m"){
          artikelAV = "einem"
          rndAuspr2 = paste(c(rndAuspr2, "en"), collapse = "")
        }
      } else { #Connector1_C,Connector1_D,Connector1_E #Akkusativ
        if(geschlechtAV == "f") {
          if (pluralAV == "0") {
            artikelAV = "eine"
            rndAuspr2 = paste(c(rndAuspr2, "e"), collapse = "")
          } else { 
            artikelAV = ""
            rndAuspr2 = paste(c(rndAuspr2, "e"), collapse = "")
          }
        } else if (geschlechtAV == "n") {
          if (pluralAV == "0") {
            artikelAV = "ein"
            rndAuspr2 = paste(c(rndAuspr2, "es"), collapse = "")
          } else { 
            artikelAV = ""
            rndAuspr2 = paste(c(rndAuspr2, "e"), collapse = "")
          }
        } else if (geschlechtAV == "m") {
          artikelAV = "einen"
          rndAuspr2 = paste(c(rndAuspr2, "en"), collapse = "")
        }
      }
      
    }
    
    rndConn2 <- NULL
    #Connector2
    if (conn1Column == "Connector1_A") {
      rndConn2 = kontext[kontext$Story == rndstory & kontext$Analyse == currentTest(),"Connector2_A"] 
    } else if (conn1Column == "Connector1_B") {
      rndConn2 = kontext[kontext$Story == rndstory & kontext$Analyse == currentTest(),"Connector2_B"] 
    } else if (conn1Column == "Connector1_C") {
      rndConn2 = kontext[kontext$Story == rndstory & kontext$Analyse == currentTest(),"Connector2_C"] 
    } else if (conn1Column == "Connector1_D") {
      rndConn2 = kontext[kontext$Story == rndstory & kontext$Analyse == currentTest(),"Connector2_D"] 
    } else if (conn1Column == "Connector1_E") {
      rndConn2 = kontext[kontext$Story == rndstory & kontext$Analyse == currentTest(),"Connector2_E"] 
    }
    
    #Überleitung
    überleitung = kontext[kontext$Story == rndstory & kontext$Analyse == currentTest(),"Überleitung"]
    
    taskText <- NULL
    
    if(currentTest() == "t-Test") { #t-test
      if (hatArtikelBOOL==TRUE) {
        taskText <- paste(c(rndstory, artikelUV, rndUv, rndConn1, artikelAV, rndAuspr2, rndAv, rndConn2, überleitung), collapse = " ")
      } else {
        taskText <- paste(c(rndstory, rndUv, rndConn1, artikelAV, rndAuspr2, rndAv, rndConn2, überleitung), collapse = " ")
      }
      
    }  else if (currentTest() == "Paired t-Test"){
      if (hatArtikelBOOL==TRUE) {
        taskText <- paste(c(rndstory, artikelUV, rndUv, rndConn1, artikelAV, rndAuspr2, rndAv, rndConn2, überleitung), collapse = " ")
      } else {
        taskText <- paste(c(rndstory, rndUv, rndConn1, artikelAV, rndAuspr2, rndAv, rndConn2, überleitung), collapse = " ")
      }
      
    } else if (currentTest() == "Korrelation"){
      if (hatArtikelBOOL==TRUE) {
        taskText <- paste(c(rndstory, artikelUV, rndAuspr1, rndUv, rndConn1, artikelAV, rndAuspr2, rndAv, rndConn2, überleitung), collapse = " ")
      } else {
        taskText <- paste(c(rndstory, rndAuspr1, rndUv, rndConn1, artikelAV, rndAuspr2, rndAv, rndConn2, überleitung), collapse = " ")
      }
    } else if (currentTest() == "Regression" || currentTest() == "log. Regression"){ 
      wholeUVstring <- character()
      for(i in 1:nrow(uvDF)) {
        artikel <- uvDF[i,"Artikel"]
        ausprägung <- uvDF[i,"Ausprägung"]
        variable <- uvDF[i,"Variable"]
        if (ausprägung=="") {
          wholeUVstring <- paste(c(wholeUVstring, artikel, variable), collapse = " ")
        } else {
          wholeUVstring <- paste(c(wholeUVstring, artikel, ausprägung, variable), collapse = " ")
        }
        if (i==nrow(uvDF)-1) {
          wholeUVstring <- paste(c(wholeUVstring, " und"), collapse = "")
        } else if (i!=nrow(uvDF)) {
          wholeUVstring <- paste(c(wholeUVstring, ","), collapse = "")
        }
        uvDF(uvDF)
      }
      if (currentTest() == "Regression") {
        taskText <- paste(c(rndstory, wholeUVstring, rndConn1, artikelAV, rndAuspr2, rndAv, rndConn2, überleitung), collapse = " ")
        
      } else if (currentTest() == "log. Regression") {
        taskText <- paste(c(rndstory, wholeUVstring, rndConn1, artikelAV, rndAv, rndConn2, überleitung), collapse = " ")
      }
      
    } else if (currentTest() == "ANOVA"){ 
      ANOVAGruppen <- values$ANOVAGruppen
      ANOVAGeschlecht <- ANOVAGruppen[ANOVAGruppen$Name == ANOVAGruppe(),"Geschlecht"]
      ANOVAGeschlecht = as.character(ANOVAGeschlecht[1, 1])
      
      Auspr1 <- NULL
      ArtikelANOVA1 <- NULL
      if (ANOVAGeschlecht=="m") {
        ArtikelANOVA1 = "ein"
        Auspr1 = "unterschiedlicher"
      } else if (ANOVAGeschlecht=="f") {
        ArtikelANOVA1 = "eine"
        Auspr1 = "unterschiedliche"
      } else if (ANOVAGeschlecht=="n") {
        ArtikelANOVA1 = "ein"
        Auspr1 = "unterschiedliches"
      }
      
      if (isTwoFac()) {
        ANOVAGeschlecht2 <- ANOVAGruppen[ANOVAGruppen$Name == ANOVAGruppe2(),"Geschlecht"]
        Auspr2 <- NULL
        ArtikelANOVA2 <- NULL
        if (ANOVAGeschlecht2=="m") {
          ArtikelANOVA2 = "einem"
          Auspr2 = "unterschiedlichen"
        } else if (ANOVAGeschlecht2=="f") {
          ArtikelANOVA2 = "einer"
          Auspr2 = "unterschiedlichen"
        } else if (ANOVAGeschlecht2=="n") {
          ArtikelANOVA2 = "einem"
          Auspr2 = "unterschiedlichen"
        }
        
        ANOVA2Conn <- kontext$ANOVA2_Conn
        ANOVA2Conn = as.data.frame(ANOVA2Conn)
        ANOVA2Conn = ANOVA2Conn[!(is.na(ANOVA2Conn))]
        ANOVA2Conn = sample(ANOVA2Conn,1)
        ANOVAGrp2Komma <- paste(c(ANOVAGruppe2(),","), collapse = "")
        
        taskText <- paste(c(rndstory, ArtikelANOVA1, Auspr1, ANOVAGruppe, ANOVA2Conn, ArtikelANOVA2, Auspr2, ANOVAGrp2Komma, rndConn1, artikelAV, rndAuspr2, rndAv, rndConn2, überleitung), collapse = " ")
      } else {
        taskText <- paste(c(rndstory, ArtikelANOVA1, Auspr1, ANOVAGruppe, rndConn1, artikelAV, rndAuspr2, rndAv, rndConn2, überleitung), collapse = " ")
      }
    } else if (currentTest() == "Chi-Quadrat") {
      taskText <- paste(c(rndstory, rndConn1, rndUv, rndConn2, überleitung), collapse = " ")
      
    }
    
    return(taskText)
  }
  
  newScaleInfo <- function() {
    
    if (currentTest() == "t-Test" | currentTest() == "Paired t-Test") {
      lickert1DF[1,"Variable"] = AV()
      variablen <- values$variablen
      einheitAV = variablen[variablen$Variable_AV == AV(),"Einheit_AV"]
      einheitAV = as.character(einheitAV[1, 1])
      if (einheitAV == "Karrierestufe[Tabelle]") {
        scaleTableDF(karrierestufeDF)
        scaleText("<p></p><b>Skalierung:</b>")
        shinyjs::show("scaleTableID")
      } else if (einheitAV == "Bildungsabschluss[Tabelle]") {
        scaleTableDF(bildungsabschlussDF)
        scaleText("<p></p><b>Skalierung:</b>")
        shinyjs::show("scaleTableID")
      } else if (einheitAV == "5pts. Lickert Scale (sehr niedrig --> sehr hoch)") {
        scaleTableDF(lickert1DF)
        scaleText("<p></p><b>Skalierung:</b>")
        shinyjs::show("scaleTableID")
      } else {
        scaleText(paste(c("<p></p><p><b>Skalierung:</b> ", einheitAV,"</p>"), collapse = ""))
        shinyjs::hide("scaleTableID")
        shinyjs::show("scaleTextID")
      }
    } else if (currentTest() == "ANOVA") {
      
      ANOVAGruppen <- values$ANOVAGruppen
      Auspr1 <- ANOVAGruppen[ANOVAGruppen$Name == ANOVAGruppe(),"Auspr_1"]
      Auspr1 <- as.character(Auspr1[1, 1])
      Auspr2 <- ANOVAGruppen[ANOVAGruppen$Name == ANOVAGruppe(),"Auspr_2"]
      Auspr2 <- as.character(Auspr2[1, 1])
      Auspr3 <- ANOVAGruppen[ANOVAGruppen$Name == ANOVAGruppe(),"Auspr_3"]
      Auspr3 <- as.character(Auspr3[1, 1])
      scTxt <- paste(c("<p></p><p><b>",ANOVAGruppe(),":</b> ",Auspr1,", ",Auspr2," oder ",Auspr3,"</p>"), collapse = "")
      
      if (isTwoFac()) {
        Auspr1 <- ANOVAGruppen[ANOVAGruppen$Name == ANOVAGruppe2(),"Auspr_1"]
        Auspr1 <- as.character(Auspr1[1, 1])
        Auspr2 <- ANOVAGruppen[ANOVAGruppen$Name == ANOVAGruppe2(),"Auspr_2"]
        Auspr2 <- as.character(Auspr2[1, 1])
        Auspr3 <- ANOVAGruppen[ANOVAGruppen$Name == ANOVAGruppe2(),"Auspr_3"]
        Auspr3 <- as.character(Auspr3[1, 1])
        scTxt <- paste(c(scTxt,"\n<p></p><p><b>",ANOVAGruppe2(),":</b> ",Auspr1,", ",Auspr2," oder ",Auspr3,"</p>"), collapse = "")
      }
      
      scaleText(scTxt)
      shinyjs::hide("scaleTableID")
      shinyjs::show("scaleTextID")
      
    } else if (currentTest() == "Chi-Quadrat"){ 
      shinyjs::hide("scaleTableID")
      shinyjs::hide("scaleTextID")
    }
    
  }
  
  doChiSquared <- function() { 
    set.seed(NULL)  
    df <- NULL
    UV <- UV()
    geschlechtUV <- geschlechtUV()
    KeinMerkmal <- NULL
    Merkmal <- UV 
    if (geschlechtUV=="f") {
      KeinMerkmal <- paste(c("Keine ",UV), collapse = "") 
    } else if (geschlechtUV=="n") {
      KeinMerkmal <- paste(c("Kein ",UV), collapse = "")
    }
    
    data_a <- "Mann"
    data_b <- "Frau"
    datasrcA <- c("Mann", "Frau")
    datasrcB <- c(Merkmal, KeinMerkmal)
    
    a <- rep(data_a, 50)
    b <- rep(data_b, 50)
    
    pr1 <- NULL
    pr2 <- NULL
    equalProb <- wakefield::r_sample_logical(1, prob = c(0.7, 0.3), name = "Logical") 
    if (equalProb==TRUE) {
      pr1 <- c(0.5, 0.5)
      pr2 <- c(0.5, 0.5)
    } else {
      randomswitch <- wakefield::r_sample_logical(1, prob = c(0.5, 0.5), name = "Logical") 
      
      r1 <- runif(1, min = 0.1, max = 0.9)
      r2 <- 1-r1
      
      r11 <- runif(1, min = 0.1, max = 0.9)
      r22 <- 1-r11
      
      if(randomswitch==TRUE){
        pr1 <- c(r1, r2) 
        pr2 <- c(r11, r22) 
      } else {
        pr1 <- c(r11, r22)  
        pr2 <- c(r1, r2) 
      }
    }
    
    a2 <- sample(datasrcB, 50, replace=TRUE, prob=pr1)
    b2 <- sample(datasrcB, 50, replace=TRUE, prob=pr2)
    
    df1 = data.frame(cbind(a, a2))
    colnames(df1) <- c("Geschlecht",Merkmal)
    
    df2 = data.frame(cbind(b, b2))
    colnames(df2) <- c("Geschlecht",Merkmal)
    
    df3 <- data.frame(rbind(df1, df2))
    
    chiSqTest <- jmv::contTables(
      formula = as.formula(paste("~ ",Merkmal,":Geschlecht")),
      data = df3,
      obs = TRUE,
      pcRow = TRUE,
      exp = TRUE)
    
    chiSq_test(chiSqTest)
    
    return(chiSqTest)
    
  }
  
  doANOVA <- function() {
    shinyjs::hide("scaleTableID")
    anovaGruppe <- ANOVAGruppe()
    anovaGruppe2 <- ANOVAGruppe2()
    variablen <- values$variablen
    
    fac <- NULL
    AV <- AV()
    AValternative =  variablen[variablen$Variable_AV == AV,"Variable_AV_alternativ3"]
    if (sum(!is.na(AValternative))==1) {
      AV = as.character(AValternative[1, 1])
    }
    
    
    alpha = 0 
    beta1 = runif(1, -1, 0.4)
    beta2 = runif(1, -1, 0.4)
    beta3 = runif(1, -1, 0.4)
    
    A = c(0,0,0,1,1,1,2,2,2,3,3,3)
    B = c(1,2,3,1,2,3,1,2,3,1,2,3)
    n <- 10
    A = rep(A, n)
    B = rep(B, n)
    
    variablen <- values$variablen
    minAV = variablen[variablen$Variable_AV == AV(),"Min_AV"]
    minAV = as.integer(minAV[1, 1])
    maxAV = variablen[variablen$Variable_AV == AV(),"Max_AV"]
    maxAV = as.integer(maxAV[1, 1])
    
    SD = runif(n = 1, min = 0.1, max = 2)
    mean =  runif(n = 1, min = maxAV/3, max = maxAV/2)
    e = truncnorm::rtruncnorm(12*n, a=minAV, b=maxAV, mean = mean, sd = SD)
    
    if (isTwoFac()) {
      y = alpha + beta1*A + beta2*B + beta3*A*B + e
      data = data.frame(cbind(A, B, y))
      colnames(data) <- c(anovaGruppe,anovaGruppe2,AV)
      fac <- c(anovaGruppe,anovaGruppe2)
      
    } else { #One Fac
      SD2 = runif(n = 1, min = 1, max = 5)
      
      e = truncnorm::rtruncnorm(12*n, a=minAV, b=maxAV, mean = mean, sd = SD2)
      
      y = beta1*A + beta2*B + beta3*A*B + e
      data = data.frame(cbind(A, y))
      
      colnames(data) <- c(anovaGruppe,AV)
      fac <- c(anovaGruppe)
    }
    
    if (input$checkPrereq) {
      values$prereqChecked <- TRUE
    } else {
      values$prereqChecked <- FALSE
    }
    
    anova <- jmv::ANOVA(
      dep = eval(AV),
      factors = eval(fac),
      data = data,
      effectSize = "eta",
      norm = values$prereqChecked,
      homo = values$prereqChecked,
      emmPlots = TRUE,
      emmTables = TRUE,
    )
    
    anova(anova)
    
    return(anova)
  }
  
  doLogRegression <- function() {
    shinyjs::hide("scaleTableID")
    shinyjs::hide("scaleTextID")
    
    y <- NULL
    if (AVhatAlt()) {
      y <- AValt()
    } else {
      y <- AV()
    }
    
    n_uv <- nrow(uvDF())
    uvDF <- uvDF()
    v1 = uvDF[1,"Variable"]
    v2 = uvDF[2,"Variable"]
    mu.1 <- NULL
    mu.1 <- rep(0,n_uv)
    
    
    n <- 500
    
    var <- 1.7
    coeff <- runif(n = 1, min = 0.1, max = 0.95) # 0.25
    data <- NULL
    sigma.1 <- NULL
    blocks <- NULL
    covs <- NULL
    
    blocksList <- list(v1,v2)
    covsVector <- c(v1,v2)
    
    if (n_uv==2) {
      sigma.1 <- matrix(c(1, coeff,
                          coeff, 1), n_uv, n_uv)
      
      data <- as.data.frame(mvrnorm(n = n, mu.1, sigma.1, empirical=FALSE))
      data$ystar <- 0 + log(2)*data$V1 + log(2)*data$V2
      data$ystar.r <- data$ystar+rnorm(n, 0, var)
      data$prob <- exp(data$ystar.r) / (1 + exp(data$ystar.r))
      data$y <- rbinom(n,size=1,prob=data$prob)
      logreg <- glm(y ~ V1 + V2, data=data, family=binomial)
      blocks <- list(
        list(
          v1, v2))
      covs <- list(
        v1, v2)
      colnames(data) <- c(v1, v2, "ystar", "ystar.r", "prob", y) 
    } else if (n_uv==3) {
      v3 = uvDF[3,"Variable"]
      sigma.1 <- matrix(c(1, coeff, coeff,
                          coeff, 1, coeff,
                          coeff, coeff, 1), n_uv, n_uv)
      data <- as.data.frame(mvrnorm(n = n, mu.1, sigma.1, empirical=FALSE))
      data$ystar <- 0 + log(2)*data$V1 + log(2)*data$V2 + log(2)*data$V3
      data$ystar.r <- data$ystar+rnorm(n, 0, var)
      data$prob <- exp(data$ystar.r) / (1 + exp(data$ystar.r))
      data$y <- rbinom(n,size=1,prob=data$prob)
      logreg <- glm(y ~ V1 + V2 + V3, data=data, family=binomial)
      blocks <- list(
        list(
          v1, v2, v3))
      covs <- list(
        v1, v2, v3)
      colnames(data) <- c(v1, v2, v3, "ystar", "ystar.r", "prob", y) 
    } else if (n_uv==4) {
      v3 = uvDF[3,"Variable"]
      v4 = uvDF[4,"Variable"]
      sigma.1 <- matrix(c(1, coeff, coeff, coeff,
                          coeff, 1, coeff, coeff,
                          coeff, coeff, 1, coeff,
                          coeff, coeff, coeff, 1), n_uv, n_uv)
      data <- as.data.frame(mvrnorm(n = n, mu.1, sigma.1, empirical=FALSE))
      data$ystar <- 0 + log(2)*data$V1 + log(2)*data$V2 + log(2)*data$V3 + log(2)*data$V4
      data$ystar.r <- data$ystar+rnorm(n, 0, var)
      data$prob <- exp(data$ystar.r) / (1 + exp(data$ystar.r))
      data$y <- rbinom(n,size=1,prob=data$prob)
      logreg <- glm(y ~ V1 + V2 + V3 + V4, data=data, family=binomial)
      blocks <- list(
        list(
          v1, v2, v3, v4))
      covs <- list(
        v1, v2, v3, v4)
      colnames(data) <- c(v1, v2, v3, v4, "ystar", "ystar.r", "prob", y) 
    }
    
    relLvls <- list(
      list(
        var=y,
        ref="0"))
    
    
    logReg_test <- jmv::logRegBin(
      data = data,
      dep = eval(y),
      covs = eval(covs), 
      blocks = eval(blocks),
      refLevels = eval(relLvls),
      modelTest = TRUE,
      bic = TRUE,
      OR = TRUE,
      class = TRUE,
      acc = TRUE,
      spec = TRUE,
      sens = TRUE,
      auc = TRUE,
      rocPlot = FALSE,
      collin = FALSE)
    
    logReg_test(logReg_test)
    
    return(logReg_test)
  }
  
  doRegression <- function() { 
    shinyjs::hide("scaleTableID")
    shinyjs::hide("scaleTextID")
    n_uv <- nrow(uvDF())
    uvDF <- uvDF()
    set.seed(NULL)  
    isCorr <- wakefield::r_sample_logical(1, prob = c(0.3, 0.7), name = "Logical") #zweite is ja
    
    df <- NULL
    random_data1 <- NULL
    random_data2 <- NULL
    random_data3 <- NULL
    random_data4 <- NULL
    
    if (isCorr) {
      collTrue <- wakefield::r_sample_logical(1, prob = c(0.3, 0.7), name = "Logical") #zweite is ja
      if (n_uv==2) {
        set.seed(NULL)
        if (collTrue) {
          cor_coeff = runif(n = 1, min = 0.9, max = 0.95)
        } else {
          cor_coeff = runif(n = 1, min = 0.0, max = 0.87)
        }
        cor_matrix <- matrix(c(1, cor_coeff,
                               cor_coeff, 1), n_uv, n_uv)
      } else if (n_uv==3) {
        set.seed(NULL)
        if (collTrue) {
          c1 <- NULL
          c2 <- NULL
          c3 <- NULL
          highColl <- wakefield::r_sample_logical(1, prob = c(0.5, 0.5), name = "Logical")
          if (highColl) {
            c1 <- runif(1, min = 0.24, max = 0.25)*-1
            c2 <- 0.50
            c3 <- runif(1, min = 0.63, max = 0.65)
          } else {
            c1 <- -0.2
            c2 <- runif(1, min = 0.57, max = 0.6)
            c3 <- runif(1, min = 0.57, max = 0.6)
          }
        } else {
          c1 <- runif(1, min = 0, max = 0.3)*-1
          c2 <- runif(1, min = 0, max = 0.5)
          c3 <- runif(1, min = 0, max = 0.5)
        }
        coeffs <- sample(c(c1,c2,c3))
        cor_coeff1 <- coeffs[1]
        cor_coeff2 <- coeffs[2]
        cor_coeff3 <- coeffs[3]
        
        cor_matrix <- matrix(c(1, cor_coeff1, cor_coeff2,
                               cor_coeff1, 1, cor_coeff3,
                               cor_coeff2, cor_coeff3, 1), n_uv, n_uv)
      } else if (n_uv==4) {
        set.seed(NULL)
        if (collTrue) {
          c1 <- runif(1, min = 0.82, max = 0.84)
          c2 <- 0.86
          c3 <- 0.82
          c4 <- 0.88
          c5 <- 0.82
          c6 <- runif(1, min = 0.86, max = 0.9)
        } else {
          c1 <- runif(1, min = 0.11, max = 0.78)
          c2 <- c1+0.01
          c3 <- c1+0.02
          c4 <- c1+0.03
          c5 <- c1+0.04
          c6 <- c1+0.04
        }
        coeffs <- sample(c(c1,c2,c3,c4,c5,c6))
        cor_coeff1 <- coeffs[1]
        cor_coeff2 <- coeffs[2]
        cor_coeff3 <- coeffs[3]
        cor_coeff4 <- coeffs[4]
        cor_coeff5 <- coeffs[5]
        cor_coeff6 <- coeffs[6]
        cor_matrix <- matrix(c(1, cor_coeff1, cor_coeff2, cor_coeff3,
                               cor_coeff1, 1, cor_coeff4, cor_coeff5,
                               cor_coeff2, cor_coeff4, 1, cor_coeff6,
                               cor_coeff3, cor_coeff5, cor_coeff6, 1), n_uv, n_uv)
      }
      
      k_cont <- n_uv
      
      cor_data <- corrvar(n = 100,
                          k_cat = 0,
                          k_cont = k_cont,
                          method = "Polynomial",
                          means = rep(0,k_cont), vars = rep(1,k_cont),
                          skews = rep(0,k_cont), skurts = rep(0,k_cont),
                          fifths = rep(0,k_cont), sixths = rep(0,k_cont),
                          Six = list(),
                          rho = cor_matrix,
                          quiet = TRUE)
      
      cor_data <- cor_data$Y_cont
      random_data1 <- cor_data[,1]
      random_data2 <- cor_data[,2]
      if (n_uv==3) {
        random_data3 <- cor_data[,3]
      } else if (n_uv==4) {
        random_data3 <- cor_data[,3]
        random_data4 <- cor_data[,4]
      }
      
    } else {
      
      random_data1 <- runif(n = 100, min = -2, max = 2)
      random_data2 <- runif(n = 100, min = -2, max = 2)
      if (n_uv==3) {
        random_data3 <- runif(n = 100, min = -2, max = 2)
      } else if (n_uv==4) {
        random_data3 <- runif(n = 100, min = -2, max = 2)
        random_data4 <- runif(n = 100, min = -2, max = 2)
      }
    }
    
    
    set.seed(NULL)
    random_noise <- runif(n = 100, min = -2, max = 2)
    weight1 <- runif(n = 1, min = -1, max = 1)
    weight2 <- runif(n = 1, min = -1, max = 1)
    weight3 <- NULL
    weight4 <- NULL
    paste(c(UV(),": ","ja"), collapse = "")
    
    v1 = uvDF[1,"Variable"]
    v2 = uvDF[2,"Variable"]
    Y = AV()
    df$col1 <- random_data1
    df$col2 <- random_data2
    blocksList <- NULL
    covsVector <- NULL
    if (n_uv==2) {
      df$Y <- random_noise + weight1*random_data1 + weight2*random_data2 
      blocksList <- list(v1,v2)
      covsVector <- c(v1,v2)
      df <- as.data.frame(df)
      colnames(df) <- c(v1, v2, Y) 
    } else if (n_uv==3) {
      v3 = uvDF[3,"Variable"]
      weight3 <- runif(n = 1, min = -1, max = 1)
      df$col3 <- random_data3
      df$Y <- random_noise + weight1*random_data1 + weight2*random_data2 + weight3*random_data3
      blocksList <- list(v1,v2,v3)
      covsVector <- c(v1,v2,v3)
      df <- as.data.frame(df)
      colnames(df) <- c(v1, v2, v3, Y)
    } else if (n_uv==4) {
      v3 = uvDF[3,"Variable"]
      v4 = uvDF[4,"Variable"]
      weight3 <- runif(n = 1, min = -1, max = 1)
      weight4 <- runif(n = 1, min = -1, max = 1)
      df$col3 <- random_data3
      df$col4 <- random_data4
      df$Y <- random_noise + weight1*random_data1 + weight2*random_data2 + weight3*random_data3 + weight4*random_data4
      blocksList <- list(v1,v2,v3,v4)
      covsVector <- c(v1,v2,v3,v4)
      df <- as.data.frame(df)
      colnames(df) <- c(v1, v2, v3, v4, Y)
    }
    
    
    set.seed(NULL) 
    if (input$checkPrereq) {
      values$prereqChecked <- TRUE
    } else {
      values$prereqChecked <- FALSE
    }
    
    
    
    reg_test <- jmv::linReg(
      data = df,
      dep = eval(Y),
      covs = eval(covsVector), 
      blocks = list(blocksList),
      refLevels = list(),
      stdEst = TRUE,
      collin = input$checkPrereq,
      resPlots = input$checkPrereq,
      norm = TRUE)
    
    reg_test(reg_test)
    
    numberOfResPlots <- length(reg_test$models[[1]]$assump$resPlots)
    if (input$checkPrereq) {
      shinyjs::show("plot1ID")
    }
    
    return(reg_test)
  }
  
  doCorrelation <- function() {
    
    shinyjs::hide("scaleTableID")
    shinyjs::hide("scaleTextID")
    set.seed(NULL)  
    
    cor_coeff  <- runif(n = 1, min = -1, max = 1)
    cor_coeff = round(cor_coeff, digits = 2)
    cor_matrix <- matrix(c(1, cor_coeff,
                           cor_coeff, 1), 2, 2)
    k_cont <- 2
    
    cor_data <- corrvar(n = 100,
                        k_cat = 0,
                        k_cont = k_cont,
                        method = "Polynomial",
                        means = rep(0,k_cont), vars = rep(1,k_cont),
                        skews = rep(0,k_cont), skurts = rep(0,k_cont),
                        fifths = rep(0,k_cont), sixths = rep(0,k_cont),
                        Six = list(),
                        rho = cor_matrix,
                        quiet = TRUE)
    
    cor_data <- cor_data$Y_cont
    v1 = UV()
    v2 <- NULL
    if (AVhatAlt()) {
      v2 = AValt()
    } else {
      v2 = AV()
    }
    
    variables <- c(v1,v2)
    colnames(cor_data) <- c(v1, v2)
    set.seed(NULL) 
    israndom <- wakefield::r_sample_logical(1, prob = c(0.7, 0.3), name = "Logical")
    if (israndom) {
      set.seed(NULL) 
      random_data1 <- runif(n = 100, min = -2, max = 2)
      set.seed(NULL) 
      random_data2 <- runif(n = 100, min = -2, max = 2)
      cor_data[, v1] <- random_data1
      cor_data[, v2] <- random_data2
    }
    
    corr_test <- jmv::corrMatrix(data = as.data.frame(cor_data), vars = eval(variables))
    
    corr_test(corr_test)
    
    return(corr_test)
  }
  
  doTtest <- function() {
    variablen <- values$variablen
    einheitAV = variablen[variablen$Variable_AV == AV(),"Einheit_AV"]
    einheitAV = as.character(einheitAV[1, 1])
    minAV = variablen[variablen$Variable_AV == AV(),"Min_AV"]
    minAV = as.integer(minAV[1, 1])
    maxAV = variablen[variablen$Variable_AV == AV(),"Max_AV"]
    maxAV = as.integer(maxAV[1, 1])
    
    meanMin = NULL
    meanMax = NULL
    data1Min = NULL
    data1Max = NULL
    data2Min = NULL
    data2Max = NULL
    sd1Min = NULL
    sd1Max = NULL
    
    #AV Skalierungen
    prozent  = "% richtige Ergebnisse im Test"
    minuten = "Minuten am Stück"
    minutenMarshmellow = "Minuten am Stück (Marshmellow-Test)"
    euroProJahr = "Euro brutto pro Jahr"
    lickert1 = "5pts. Lickert Scale (sehr niedrig --> sehr hoch)"
    schulnoten = "Deutsche Schulnoten: 1 (sehr gut) bis 6 (ungenügend)"
    anzahl = "Anzahl"
    karrierestufe = "Karrierestufe[Tabelle]"
    bildungsabschluss = "Bildungsabschluss[Tabelle]"
    
    if (einheitAV == prozent) { 
      meanMin = 30
      meanMax = 70
      mean1 = runif(n = 1, min = meanMin, max = meanMax)
      mean1 = round(mean1, digits = 0)
      data1Min = 5
      data1Max = maxAV
    } else if (einheitAV == minuten || einheitAV == minutenMarshmellow) {
      meanMin = 36
      meanMax = 84
      mean1 = runif(n = 1, min = meanMin, max = meanMax)
      mean1 = round(mean1, digits = 0)
      data1Min = 6
      data1Max = 120
    } else if (einheitAV == euroProJahr) {
      meanMin = 60000
      meanMax = 140000
      mean1 = runif(n = 1, min = meanMin, max = meanMax)
      mean1 = round(mean1, digits = 0)
      data1Min = minAV
      data1Max = maxAV
    } else if (einheitAV == lickert1) {
      meanMin = 2
      meanMax = 4
      mean1 = runif(n = 1, min = meanMin, max = meanMax)
      mean1 = round(mean1, digits = 0)
      data1Min = minAV 
      data1Max = maxAV
    } else if (einheitAV == schulnoten) {
      meanMin = 2
      meanMax = 5
      mean1 = runif(n = 1, min = meanMin, max = meanMax)
      mean1 = round(mean1, digits = 0)
      data1Min = minAV 
      data1Max = maxAV
    } else if (einheitAV == karrierestufe || einheitAV == bildungsabschluss) {
      meanMin = 2
      meanMax = 6
      mean1 = runif(n = 1, min = meanMin, max = meanMax)
      mean1 = round(mean1, digits = 0)
      data1Min = minAV 
      data1Max = maxAV
    } else if (einheitAV == anzahl) {
      meanMin = 2
      meanMax = 18
      mean1 = runif(n = 1, min = meanMin, max = meanMax)
      mean1 = round(mean1, digits = 0)
      data1Min = minAV 
      data1Max = maxAV
    } else {
      meanMin = 10
      meanMax = 700
      mean1 = runif(n = 1, min = meanMin, max = meanMax)
      mean1 = round(mean1, digits = 0)
      data1Min = 5
      data1Max = mean1*3
    }
    
    #Gleicheit der Varianzen mit 70% Wahrscheinlichkeit
    homovar <- wakefield::r_sample_logical(1, prob = c(0.3, 0.7), name = "Logical")
    
    #Stichprobengröße
    n = runif(n = 1, min = 20, max = 80) #runif: gleichverteilte Zufallszahlen
    n = round(n, digits = 0)
    
    #Standardabweichung Data 1
    if (einheitAV == prozent ) {
      sd1Max = data1Max-mean1 
      sd1Min = sd1Max/4 
    } else if (einheitAV == lickert1 || einheitAV == schulnoten || einheitAV == karrierestufe || einheitAV == bildungsabschluss || einheitAV == anzahl) {
      sd1Max = data1Max-mean1
      sd1Min = 1
    } else {
      sd1Max = data1Max-mean1
      sd1Min = sd1Max/4
      
    }
    
    
    sd1Min = round(sd1Min, digits = 0)
    sd1Max = round(sd1Max, digits = 0)
    sd1 = runif(n = 1, min = sd1Min, max = sd1Max)
    sd1 = round(sd1, digits = 0)
    
    #Mittelwert Data 2
    diffToMean1 <- runif(n = 1, min = 0.5*-sd1, max = 0.5*sd1)
    mean2 = mean1 + diffToMean1
    mean2 = round(mean2, digits = 0)
    
    if (einheitAV == prozent) { 
      data2Min = 5
      data2Max = maxAV
    } else if (einheitAV == minuten || einheitAV == minutenMarshmellow) {
      data2Min = 6
      data2Max = maxAV
    } else if (einheitAV %in% c(euroProJahr,lickert1,schulnoten,karrierestufe,bildungsabschluss,anzahl)) {
      data2Min = minAV
      data2Max = maxAV
      
    } 
    
    #Standardabweichung Data 2
    sd2 <- NULL
    if (homovar) {
      sd2 = sd1
    } else{
      sd2Max = data2Max-mean2
      sd2Min = sd2Max/4
      sd2 = runif(n = 1, min = sd2Min, max = sd2Max) 
      sd2 = round(sd2, digits = 0)
    }
    
    #Daten (Normalverteilung)
    data1 = truncnorm::rtruncnorm(n, a=data1Min, b=data1Max, mean = mean1, sd = sd1)
    data1 = round(data1, digits = 0)
    data2 = truncnorm::rtruncnorm(n, a=data2Min, b=data2Max, mean = mean2, sd = sd2)
    data2 = round(data2, digits = 0)
    
    
    groupname1 = paste(c(UV(),": ","ja"), collapse = "")
    groupname2 = paste(c(UV(),": ","nein"), collapse = "")
    
    df1 <- data.frame("x" = data1, "Gruppe" = groupname1)
    df2 <- data.frame("x" = data2, "Gruppe" = groupname2)
    completeData <- rbind(df1, df2)
    
    t_test_Temp <- jmv::ttestIS(vars = x, group = Gruppe, data = completeData, norm = TRUE, eqv = TRUE, desc = TRUE, mann = TRUE, effectSize = TRUE)
    
    norm <- t_test_Temp$assum$norm$asDF
    eqv <- t_test_Temp$assum$eqv$asDF
    pNorm <- norm[1,"p"]
    pEqv <- eqv[1,"p"]
    
    values$doU_test <- FALSE
    doStudents <- TRUE
    values$showU_testSol = FALSE
    if (input$checkPrereq==TRUE) {
      values$prereqChecked <- TRUE
      if (pNorm > 0.05 && pEqv > 0.05) {
        values$showU_testSol = FALSE
        # values$doU_test = FALSE
        # doStudents = TRUE
      } else {
        values$showU_testSol = TRUE
        # values$doU_test = TRUE
        # doStudents = FALSE
      }
      values$doU_test = TRUE
      doStudents = TRUE
    } else {
      values$prereqChecked <- FALSE
    }
    
    t_test <- jmv::ttestIS(vars = x, group = Gruppe, data = completeData, norm = input$checkPrereq, eqv = input$checkPrereq, desc = TRUE, students = doStudents, mann = values$doU_test, effectSize = TRUE)#students = doStudents, mann = values$doU_test
    t_test(t_test)
    
    
    
    return(t_test)
  }
  
  doPairedTtest <- function() {
    #    "Paired t-test can be used only when the difference d
    # is normally distributed. This can be checked using Shapiro-Wilk test."
    
    set.seed(NULL)
    
    variablen <- values$variablen
    einheitAV = variablen[variablen$Variable_AV == AV(),"Einheit_AV"]
    einheitAV = as.character(einheitAV[1, 1])
    minAV = variablen[variablen$Variable_AV == AV(),"Min_AV"]
    minAV = as.integer(minAV[1, 1])
    maxAV = variablen[variablen$Variable_AV == AV(),"Max_AV"]
    maxAV = as.integer(maxAV[1, 1])
    
    meanMin = NULL
    meanMax = NULL
    data1Min = NULL
    data1Max = NULL
    data2Min = NULL
    data2Max = NULL
    sd1Min = NULL
    sd1Max = NULL
    
    #AV Skalierungen
    prozent  = "% richtige Ergebnisse im Test"
    minuten = "Minuten am Stück"
    minutenMarshmellow = "Minuten am Stück (Marshmellow-Test)"
    euroProJahr = "Euro brutto pro Jahr"
    lickert1 = "5pts. Lickert Scale (sehr niedrig --> sehr hoch)"
    schulnoten = "Deutsche Schulnoten: 1 (sehr gut) bis 6 (ungenügend)"
    anzahl = "Anzahl"
    karrierestufe = "Karrierestufe[Tabelle]"
    bildungsabschluss = "Bildungsabschluss[Tabelle]"
    
    if (einheitAV == prozent) {
      meanMin = 30
      meanMax = 70
      mean1 = runif(n = 1, min = meanMin, max = meanMax)
      mean1 = round(mean1, digits = 0)
      data1Min = 5
      data1Max = maxAV
    } else if (einheitAV == minuten || einheitAV == minutenMarshmellow) {
      meanMin = 36
      meanMax = 84
      mean1 = runif(n = 1, min = meanMin, max = meanMax)
      mean1 = round(mean1, digits = 0)
      data1Min = 6
      data1Max = 120
    } else if (einheitAV == euroProJahr) {
      meanMin = 60000
      meanMax = 140000
      mean1 = runif(n = 1, min = meanMin, max = meanMax)
      mean1 = round(mean1, digits = 0)
      data1Min = minAV
      data1Max = maxAV
    } else if (einheitAV == lickert1) {
      meanMin = 2
      meanMax = 4
      mean1 = runif(n = 1, min = meanMin, max = meanMax)
      mean1 = round(mean1, digits = 0)
      data1Min = minAV
      data1Max = maxAV
    } else if (einheitAV == schulnoten) {
      meanMin = 2
      meanMax = 5
      mean1 = runif(n = 1, min = meanMin, max = meanMax)
      mean1 = round(mean1, digits = 0)
      data1Min = minAV
      data1Max = maxAV
    } else if (einheitAV == karrierestufe || einheitAV == bildungsabschluss) {
      meanMin = 2
      meanMax = 6
      mean1 = runif(n = 1, min = meanMin, max = meanMax)
      mean1 = round(mean1, digits = 0)
      data1Min = minAV
      data1Max = maxAV
    } else if (einheitAV == anzahl) {
      meanMin = 2
      meanMax = 18
      mean1 = runif(n = 1, min = meanMin, max = meanMax)
      mean1 = round(mean1, digits = 0)
      data1Min = minAV
      data1Max = maxAV
    } else {
      meanMin = 10
      meanMax = 700
      mean1 = runif(n = 1, min = meanMin, max = meanMax)
      mean1 = round(mean1, digits = 0)
      data1Min = 5
      data1Max = mean1*3
    }
    
    
    #Gleicheit der Varianzen mit 70% Wahrscheinlichkeit
    homovar <- wakefield::r_sample_logical(1, prob = c(0.3, 0.7), name = "Logical")
    
    #Stichprobengröße
    n = runif(n = 1, min = 20, max = 80) #runif: gleichverteilte Zufallszahlen
    n = round(n, digits = 0)
    
    #Standardabweichung Data 1
    if (einheitAV == prozent ) {
      sd1Max = data1Max-mean1
      sd1Min = sd1Max/4
    } else if (einheitAV == lickert1 || einheitAV == schulnoten || einheitAV == karrierestufe || einheitAV == bildungsabschluss || einheitAV == anzahl) {
      sd1Max = data1Max-mean1
      sd1Min = 1
    } else {
      sd1Max = data1Max-mean1
      sd1Min = sd1Max/4
      
    }
    
    
    sd1Min = round(sd1Min, digits = 0)
    sd1Max = round(sd1Max, digits = 0)
    sd1 = runif(n = 1, min = sd1Min, max = sd1Max)
    sd1 = round(sd1, digits = 0)
    
    #Mittelwert Data 2
    diffToMean1 <- runif(n = 1, min = 0.5*-sd1, max = 0.5*sd1)
    mean2 = mean1 + diffToMean1
    mean2 = round(mean2, digits = 0)
    
    if (einheitAV == prozent) {
      data2Min = 5
      data2Max = maxAV
    } else if (einheitAV == minuten || einheitAV == minutenMarshmellow) {
      data2Min = 6
      data2Max = maxAV
    } else if (einheitAV %in% c(euroProJahr,lickert1,schulnoten,karrierestufe,bildungsabschluss,anzahl)) {
      data2Min = minAV
      data2Max = maxAV
      
    }
    
    #Standardabweichung Data 2
    sd2 <- NULL
    if (homovar) {
      sd2 = sd1
    } else{
      sd2Max = data2Max-mean2
      sd2Min = sd2Max/4
      sd2 = runif(n = 1, min = sd2Min, max = sd2Max)
      sd2 = round(sd2, digits = 0)
    }
    
    #Daten (Normalverteilung)
    data1 = truncnorm::rtruncnorm(n, a=data1Min, b=data1Max, mean = mean1, sd = sd1)
    data1 = round(data1, digits = 0)
    data2 = truncnorm::rtruncnorm(n, a=data2Min, b=data2Max, mean = mean2, sd = sd2)
    data2 = round(data2, digits = 0)
    
    
    data <- data.frame(data1, data2)
    colnames(data) <- c("Prä","Post")
    
    values$prereqChecked <- FALSE
    if (input$checkPrereq) {
      values$prereqChecked <- TRUE
    } else {
      values$prereqChecked <- FALSE
    }
    
    paired_t_test_Temp <- jmv::ttestPS(
      data = data,
      pairs = list(
        list(
          i1="Prä",
          i2="Post")),
      meanDiff = TRUE,
      effectSize = TRUE,
      desc = TRUE,
      norm = values$prereqChecked,
      wilcoxon = values$doWilcoxon)
    
    norm <- paired_t_test_Temp$norm$asDF
    pNorm <- norm[1,"p"]
    
    
    values$doWilcoxon <- FALSE
    values$showWilcoxonSol <- FALSE
    if (input$checkPrereq==TRUE) {
      values$prereqChecked <- TRUE
      if (pNorm > 0.05 ) {
        values$showWilcoxonSol = FALSE
      } else {
        values$showWilcoxonSol = TRUE
      }
      values$doWilcoxon = TRUE
    } else {
      values$prereqChecked <- FALSE
    }
    
    paired_t_test <-jmv::ttestPS(
      data = data,
      pairs = list(
        list(
          i1="Prä",
          i2="Post")),
      meanDiff = TRUE,
      effectSize = TRUE,
      desc = TRUE,
      norm = values$prereqChecked,
      wilcoxon = values$doWilcoxon)
    
    paired_t_test(paired_t_test)
    
    return(paired_t_test)
  }
  
  getTtestSolution <- function() {
    desc <- t_test()$desc$asDF
    ttest <- t_test()$ttest$asDF
    norm <- t_test()$assum$norm$asDF 
    eqv <- t_test()$assum$eqv$asDF 
    
    
    
    mean1 = sprintf("%.2f", round(desc[1,"mean[1]"],2))
    sd1 = sprintf("%.2f", round(desc[1,"sd[1]"],2)) 
    mean2 = sprintf("%.2f", round(desc[1,"mean[2]"],2))
    sd2 = sprintf("%.2f", round(desc[1,"sd[2]"],2)) 
    
    gruppe1 <- paste(c("Gruppe 1 (", desc[1,"group[1]"],"; <i>M</i> = ",mean1,"; <i>SD</i> = ",sd1,")"), collapse = "")
    gruppe2 <- paste(c("Gruppe 2 (", desc[1,"group[2]"],"; <i>M</i> = ",mean2,"; <i>SD</i> = ",sd2,")"), collapse = "")
    solutionText1 <- NULL
    if (auspr2PosNeg() == "negativ") {
      if (desc[1,"mean[1]"]<desc[1,"mean[2]"]) {
        solutionText1 = paste(c("Entsprechend der Hypothese wurde für ", gruppe1," ein niedrigerer Mittelwert beobachtet als für ", gruppe2,"."), collapse = "")
      } else if ((desc[1,"mean[1]"]>desc[1,"mean[2]"])){
        solutionText1 = paste(c("Entgegen der Hypothese wurde für ", gruppe1," ein höherer Mittelwert beobachtet als für ", gruppe2,"."), collapse = "")
      }
      
    } else if(auspr2PosNeg() == "positiv") {
      if (desc[1,"mean[1]"]>desc[1,"mean[2]"]) {
        solutionText1 = paste(c("Entsprechend der Hypothese wurde für ", gruppe1," ein höherer Mittelwert beobachtet als für ", gruppe2,"."), collapse = "")
      } else if ((desc[1,"mean[1]"]<desc[1,"mean[2]"])){
        solutionText1 = paste(c("Entgegen der Hypothese wurde für ", gruppe1," ein niedrigerer Mittelwert beobachtet als für ", gruppe2,"."), collapse = "")
      }
    }
    
    solutionText2 <- NULL
    d <- NULL
    if (values$showU_testSol==TRUE) {
      d = abs(ttest[1,"es[mann]"]) 
    } else {
      d = abs(ttest[1,"es[stud]"])
    }
    
    
    dText <- NULL
    if (d<0.2) {
      dText = "vernachlässigbaren"
    } else if((d>=0.2)&(d<0.5)) {
      dText = "schwachen"
    } else if((d>=0.5)&(d<0.8)) {
      dText = "mittleren"
    } else if(d>=0.8) {
      dText = "starken"
    }
    
    pTtest = NULL
    
    if (values$showU_testSol==TRUE) {
      pTtest = ttest[1,"p[mann]"] 
    } else {
      pTtest = ttest[1,"p[stud]"] 
    }
    pTtestText <- NULL
    if (pTtest<0.05) {
      pTtestText = "statistisch signifikant"
    } else {
      pTtestText = "statistisch nicht signifikant"
    }
    pTtestRounded = round(pTtest, digits = 3)
    pTtestPrint = sprintf("%.3f", round(pTtest,3))
    pTtestApa <- NULL
    if (pTtestRounded==0) {
      pTtestApa = "< .001"
    } else if (pTtestRounded==1.00){
      pTtestApa = "= 1.000"
    } else {
      pTtestApa = sub(".", "", as.character(pTtestPrint))
      pTtestApa = paste(c("= ",pTtestApa), collapse = "")   
    }
    if (values$showU_testSol==TRUE) {
      solutionText2 = paste(c("<br>Dieser Unterschied war mit einer ", dText," Effektstärke ",pTtestText ," ","(<i>p</i> ",pTtestApa,"; <i>d</i> = ",round(ttest[1,"es[mann]"], digits = 3),")."), collapse = "")
      
    } else {
      solutionText2 = paste(c("<br>Dieser Unterschied war mit einer ", dText," Effektstärke ",pTtestText ," ","(<i>t</i>(",ttest[1,"df[stud]"],") = ",round(ttest[1,"stat[stud]"], digits = 2),"; <i>p</i> ",pTtestApa,"; <i>d</i> = ",round(ttest[1,"es[stud]"], digits = 3),")."), collapse = "")
      
    }
    prereqText1 <- NULL
    prereqText2 <- NULL
    pNorm = norm[1,"p"]
    pEqv = eqv[1,"p"]
    
    if (!(is.na(pNorm))) {
      pNormRounded = round(pNorm, digits = 3)
      pNormPrint = sprintf("%.3f", round(pNorm,3))
      pNormApa <- NULL
      if (pNormRounded==0) {
        pNormApa = "< .001"
      } else if (pNormRounded==1.00){
        pNormApa = "= 1.000"
      } else {
        pNormApa = sub(".", "", as.character(pNormPrint))
        pNormApa = paste(c("= ",pNormApa), collapse = "")   
      }
      
      pEqvRounded = round(pEqv, digits = 3)
      pEqvPrint = sprintf("%.3f", round(pEqv,3))
      pEqvApa <- NULL
      if (pEqvRounded==0) {
        pEqvApa = "< .001"
      } else if (pEqvRounded==1.00){
        pEqvApa = "= 1.000"
      } else {
        pEqvApa = sub(".", "", as.character(pEqvPrint))
        pEqvApa = paste(c("= ",pEqvApa), collapse = "")   
      }
      if (pNorm>0.05) {
        prereqText1 = paste(c("<br>Die Voraussetzung der Normalverteilung ist laut Shapiro-Wilk-Test erfüllt (<i>p</i> ",pNormApa,")."), collapse = "")
      } else {
        prereqText1 = paste(c("<br>Die Voraussetzung der Normalverteilung ist laut Shapiro-Wilk-Test verletzt (<i>p</i> ",pNormApa,")."), collapse = "")
      }
      if (pEqv>0.05) {
        prereqText2 = paste(c("Die Voraussetzung der Varianzhomogenität ist laut Levene-Test erfüllt (<i>p</i> ",pEqvApa,")."), collapse = "")
      } else {
        prereqText2 = paste(c("Die Voraussetzung der Varianzhomogenität ist laut Levene-Test verletzt (<i>p</i> ",pEqvApa,")."), collapse = "")
      }
      solutionText = paste(c(solutionText1, solutionText2, prereqText1, prereqText2), collapse = " ")     
    } else {
      solutionText = paste(c(solutionText1, solutionText2), collapse = " ")     
    }
    
    
    return(solutionText)
  }
  
  getPairedTtestSolution <- function() {
    
    pairedttest <- paired_t_test()$ttest$asDF
    norm <- paired_t_test()$norm$asDF
    desc <- paired_t_test()$desc$asDF
    
    meanPre = sprintf("%.2f", round(desc[1,"m"],2))
    sdPre = sprintf("%.2f", round(desc[1,"sd"],2)) 
    meanPost = sprintf("%.2f", round(desc[2,"m"],2))
    sdPost = sprintf("%.2f", round(desc[2,"sd"],2)) 
    
    solutionText <- NULL
    
    gruppePre <- paste(c("der alten Erhebung (Prä; <i>M</i> = ",meanPre,"; <i>SD</i> = ",sdPre,")"), collapse = "")
    gruppePost <- paste(c("der neuen Erhebung (Post; <i>M</i> = ",meanPost,"; <i>SD</i> = ",sdPost,")"), collapse = "")
    solutionText1 <- NULL
    if (auspr2PosNeg() == "negativ") {
      if (meanPost<meanPre) {
        solutionText1 = paste(c("Entsprechend der Hypothese wurde in ", gruppePost," ein niedrigerer Mittelwert beobachtet als in ", gruppePre,"."), collapse = "")
      } else if (meanPost>meanPre){
        solutionText1 = paste(c("Entgegen der Hypothese wurde in ", gruppePost," ein höherer Mittelwert beobachtet als in ", gruppePre,"."), collapse = "")
      }
      
    } else if(auspr2PosNeg() == "positiv") {
      if (meanPost<meanPre) {
        solutionText1 = paste(c("Entsprechend der Hypothese wurde in ", gruppePre," ein höherer Mittelwert beobachtet als in ", gruppePost,"."), collapse = "")
      } else if (meanPost>meanPre){
        solutionText1 = paste(c("Entgegen der Hypothese wurde in ", gruppePre," ein niedrigerer Mittelwert beobachtet als in ", gruppePost,"."), collapse = "")
      }
    }
    
    pTtest = NULL
    if (values$showWilcoxonSol==TRUE) {
      pTtest = pairedttest[1,"p[wilc]"] 
    } else {
      pTtest = pairedttest[1,"p[stud]"] 
    }
    
    pTtestText <- NULL
    if (pTtest<0.05) {
      pTtestText = "konnte inferenzstatistisch abgesichert"
    } else {
      pTtestText = "konnte jedoch inferenzstatistisch nicht abgesichert"
    }
    pTtestRounded = round(pTtest, digits = 3)
    pTtestPrint = sprintf("%.3f", round(pTtest,3))
    pTtestApa <- NULL
    if (pTtestRounded==0) {
      pTtestApa = "< .001"
    } else if (pTtestRounded==1.00){
      pTtestApa = "= 1.000"
    } else {
      pTtestApa = sub(".", "", as.character(pTtestPrint))
      pTtestApa = paste(c("= ",pTtestApa), collapse = "")   
    }
    
    d <- NULL
    if (values$showWilcoxonSol==TRUE) {
      d = abs(pairedttest[1,"es[wilc]"])
    } else {
      d = abs(pairedttest[1,"es[stud]"])
    }
    
    effectSize <- NULL
    
    if (values$showWilcoxonSol==TRUE) { # 0,2; 0,4; 0,6
      if (d<0.2) {
        effectSize = "vernachlässigbaren"
      } else if((d>=0.2)&(d<0.4)) {
        effectSize = "kleinen"
      } else if((d>=0.4)&(d<0.6)) {
        effectSize = "mittleren"
      } else if(d>=0.6) {
        effectSize = "großen"
      }
    } else { # 0,2; 0,5; 0,8
      if (d<0.2) {
        effectSize = "vernachlässigbaren"
      } else if((d>=0.2)&(d<0.5)) {
        effectSize = "kleinen"
      } else if((d>=0.5)&(d<0.8)) {
        effectSize = "mittleren"
      } else if(d>=0.8) {
        effectSize = "großen"
      }
    }
    
    solutionText2 <- paste(c(" Der Unterschied ",pTtestText," werden "), collapse = "")
    solutionText3 <- NULL
    if (values$showWilcoxonSol==TRUE) {
      solutionText3 <- paste(c("(<i>W</i> = ",round(pairedttest[1,"stat[wilc]"], digits = 2),"; <i>p</i> ",pTtestApa,"; <i>r</i> = ",round(pairedttest[1,"es[wilc]"], digits = 3),"). Hierbei handelt es sich um einen ",effectSize," Effekt."), collapse = "")
      
      
    } else {
      solutionText3 <- paste(c("(<i>t</i>(",pairedttest[1,"df[stud]"],") = ",round(pairedttest[1,"stat[stud]"], digits = 2),"; <i>p</i> ",pTtestApa,"; <i>d</i> = ",round(pairedttest[1,"es[stud]"], digits = 3),"). Hierbei handelt es sich um einen ",effectSize," Effekt."), collapse = "") 
      
    }
    
    
    prereqText1 <- NULL
    if (values$prereqChecked) {
      pNorm = norm[1,"p"]
      if (!(is.na(pNorm))) {
        pNormRounded = round(pNorm, digits = 3)
        pNormPrint = sprintf("%.3f", round(pNorm,3))
        pNormApa <- NULL
        if (pNormRounded==0) {
          pNormApa = "< .001"
        } else if (pNormRounded==1.00){
          pNormApa = "= 1.000"
        } else {
          pNormApa = sub(".", "", as.character(pNormPrint))
          pNormApa = paste(c("= ",pNormApa), collapse = "")
        }
      }
      
      wNorm = norm[1,"w"]
      if (!(is.na(wNorm))) {
        wNormRounded = round(wNorm, digits = 2)
      }
      
      
      
      if (pNorm>0.05) {
        prereqText1 = paste(c("<br>Die Differenz dieser Daten war entsprechend der Voraussetzung normalverteilt (<i>W</i> = ",wNormRounded,"; <i>p</i> ",pNormApa,")."), collapse = "")
      } else {
        prereqText1 = paste(c("<br>Die Differenz dieser Daten war allerdings nicht normalverteilt (<i>W</i> = ",wNormRounded,"; <i>p</i> ",pNormApa,"). Deshalb wurde ein Wilcoxon Test berechnet."), collapse = "")
      }
    }
    
    if (values$prereqChecked) {
      solutionText <- paste(c(solutionText1,prereqText1,solutionText2,solutionText3), collapse = "")
    } else {
      solutionText <- paste(c(solutionText1,solutionText2,solutionText3), collapse = "")
    }
    
    return(solutionText)
    
  }
  
  getCorrSolution <- function() { 
    matrixDF <- corr_test()$matrix$asDF
    matrix <- corr_test()$matrix
    corCoeff <- as.numeric(matrixDF[2,paste(c(UV(),"[r]"), collapse = "")])
    absCorCoeff <- abs(corCoeff)
    corText <- NULL
    if(absCorCoeff<0.2) {
      corText <- "kein"
    } else if((absCorCoeff>=0.20)&(absCorCoeff<0.4)) {
      corText <- "kleiner"
    } else if((absCorCoeff>=0.4)&(absCorCoeff<0.6)){
      corText <- "mittelgroßer"
    } else if(absCorCoeff>0.6){
      corText <- "starker"
    }
    
    pValue <- as.numeric(matrixDF[2,paste(c(UV(),"[rp]"), collapse = "")])
    pText <- NULL
    if(pValue<=0.05) {
      pText <- "signifikant"
    } else if(pValue>0.05) {
      pText <- "nicht signifikant"
    }
    pRounded = round(pValue, digits = 3)
    pPrint = sprintf("%.3f", round(pValue,3))
    
    pApa <- NULL
    if (pRounded==0) {
      pApa = "< .001"
    } else if (pRounded==1.00){
      pApa = "= 1.000"
    } else {
      pApa = sub(".", "", as.character(pPrint))
      pApa = paste(c("= ",pApa), collapse = "")     
    }
    
    if(corText!="kein") {
      posOrNeg <- NULL
      if (corCoeff>0) {
        posOrNeg = "positiver"
      } else if(corCoeff<0) {
        posOrNeg = "negativer"
      }
      solutionText1 = paste(c("Es besteht ein ", corText, " ", posOrNeg," Zusammenhang zwischen ", UV(), " und ",AV()," (<i>r</i> = ", round(corCoeff, digits = 2),")."), collapse = "")
    } else {
      solutionText1 = paste(c("Es besteht ", corText," Zusammenhang zwischen ", UV(), " und ",AV()," (<i>r</i> = ", round(corCoeff, digits = 2),")."), collapse = "")
    }
    solutionText2 = paste(c("<br>Das Ergebnis ist statistisch ",pText," (<i>p</i> ",pApa,")."), collapse = "")
    solutionText = paste(c(solutionText1, solutionText2), collapse = " ")     
    
    return(solutionText)
  }
  
  getRegSolution <- function() { 
    reg_test <- reg_test()
    coefDF <- reg_test$models[[1]]$coef$asDF
    sigVars <- NULL
    for(i in 1:nrow(coefDF)) {
      p <- as.numeric(coefDF[i,"p"])
      if (p<0.05) {
        sigVars <- c(sigVars, i)
      }
    }
    sigText <- ""
    i <- 0
    if (length(sigVars)>=1) {
      for (index in sigVars) {
        i <- i+1
        pValue <- as.numeric(coefDF[index,"p"])
        pRounded = round(pValue, digits = 3)
        pPrint = sprintf("%.3f", round(pValue,3))
        pApa <- NULL
        if (pRounded==0) {
          pApa = "< .001"
        } else if (pRounded==1.00){
          pApa = "= 1.000"
        } else {
          pApa = sub(".", "", as.character(pPrint))
          pApa = paste(c("= ",pApa), collapse = "")   
        }
        
        betaVal <- as.numeric(coefDF[index,"stdEst"])
        beta <- round(betaVal, digits = 2)
        sigText <- paste(c(sigText, "- ", coefDF[index,"term"]," (β = ",beta,"; <i>p</i> ",pApa,")<br>"), collapse = "")
        
      }
    }
    
    collDF <- reg_test$models[[1]]$assump$collin$asDF
    VIF <- NULL
    for(i in 1:nrow(collDF)) {
      vif <- as.numeric(collDF[i,"vif"])
      if (vif>5) {
        VIF <- c(VIF, i)
      }
    }
    
    multiVars <- length(VIF)
    
    percentage <- helperGetPercentage(reg_test$modelFit$asDF[1,"r2"])
    
    solution <- NULL
    solutionText1 <- paste(c("Die Prädiktoren erklären ", percentage, " der Varianz. "), collapse = "")     
    
    if (length(sigVars)==0) {
      solutionText1 <- paste(c(solutionText1, "Kein Prädiktor trägt signifikant zur Vorhersage bei. "), collapse = "")
    } else if (length(sigVars)==1) {
      solutionText1 <- paste(c(solutionText1, "Folgender Prädiktor trägt signifikant zur Vorhersage bei:<br>", sigText), collapse = "")
    } else if (length(sigVars)>1) {
      solutionText1 <- paste(c(solutionText1, "Folgende Prädiktoren tragen signifikant zur Vorhersage bei:<br>", sigText), collapse = "")
    }
    
    if (values$prereqChecked==TRUE) {
      
      if (multiVars==0) {
        solutionText2 <- " Es liegt keine Multikollinearität vor. "
      } else if (multiVars==1) {
        solutionText2 <- " Bei einer Variable liegt Multikollinearität vor. "
      } else if (multiVars==2) {
        solutionText2 <- " Bei zwei Variablen liegt Multikollinearität vor. "
      } else if (multiVars==3) {
        solutionText2 <- " Bei drei Variablen liegt Multikollinearität vor. "
      } else if (multiVars==4) {
        solutionText2 <- " Bei vier Variablen liegt Multikollinearität vor. "
      } 
      
      solutionText3 <- "Es liegt keine Heteroskedastizität vor."
      solution <- paste(c(solutionText1, solutionText2, solutionText3), collapse = "")     
      
      
    } else {
      solution <- solutionText1
    }
    
    return(solution)
  }
  
  getLogRegSolution <- function() {
    logReg_test <- logReg_test()
    coefDF <- logReg_test$models[[1]]$coef$asDF
    
    sigVars <- NULL
    for(i in 1:nrow(coefDF)) {
      p <- as.numeric(coefDF[i,"p"])
      if (p<0.05) {
        sigVars <- c(sigVars, i)
      }
    }
    sigText <- ""
    i <- 0
    sigText <- ""
    i <- 0
    if (length(sigVars)>=1) {
      for (index in sigVars) {
        i <- i+1
        pValue <- as.numeric(coefDF[index,"p"])
        pRounded = round(pValue, digits = 3)
        pPrint = sprintf("%.3f", round(pValue,3))
        pApa <- NULL
        if (pRounded==0) {
          pApa = "< .001"
        } else if (pRounded==1.00){
          pApa = "= 1.000"
        } else {
          pApa = sub(".", "", as.character(pPrint))
          pApa = paste(c("= ",pApa), collapse = "")   
        }
        
        betaVal <- as.numeric(coefDF[index,"est"])
        beta <- round(betaVal, digits = 2)
        
        oddsVal <- as.numeric(coefDF[index,"odds"])
        odds <- round(oddsVal, digits = 2)
        sigText <- paste(c(sigText, "- ", coefDF[index,"term"]," (b = ",beta,"; <i>p</i> ",pApa,"; OR = ",odds,")<br>"), collapse = "")
      }
    }
    
    r2mfRaw <- as.numeric(logReg_test$modelFit$asDF[1,"r2mf"])
    r2mfRounded = sprintf("%.2f", round(r2mfRaw,2))
    r2mfSplit <- strsplit(as.character(r2mfRounded), "\\.")
    r2mfDec <- r2mfSplit[[1]][2]
    
    modelfitString <- NULL
    if (r2mfRounded<0.04) {
      modelfitString <- "sehr schlecht"
    } else if (r2mfRounded>=0.04 && r2mfRounded<0.16) {
      modelfitString <- "schlecht"
    } else if (r2mfRounded>=0.16 && r2mfRounded<0.36) {
      modelfitString <- "mittelmäßig"
    } else if (r2mfRounded>=0.36) {
      modelfitString <- "sehr gut"
    }
    
    solution <- NULL
    solutionText1 <- NULL
    
    variablen <- values$variablen
    AV <- AV()
    AValternative =  variablen[variablen$Variable_AV == AV,"Variable_AV_alternativ"]
    AValternative = as.character(AValternative[1, 1])
    R2 <- "R\U00B2-McF"
    solutionText1 <- paste(c("Das Modell klassifizierte ",modelfitString, " hinsichtlich des ",AValternative," (",R2,"=  .", r2mfDec,")"), collapse = "")     
    predDF <- logReg_test$models[[1]]$pred$measures$asDF
    
    accuracy <- helperGetPercentage(predDF[1,"accuracy"])
    spec <- helperGetPercentage(predDF[1,"spec"])
    sens <- helperGetPercentage(predDF[1,"sens"])
    auc <- helperGetPercentage(predDF[1,"auc"])
    solutionText1 <- paste(c(solutionText1,", wobei insgesamt ", accuracy, " der Fälle korrekt zugeordnet wurden (Spezifität = ",spec,"; Sensitivität = ",sens,"; AUC = ",auc,"). <br>"), collapse = "")     
    
    
    if (length(sigVars)==0) {
      solutionText1 <- paste(c(solutionText1, "Kein Prädiktor trägt signifikant zur Vorhersage bei. "), collapse = "")
    } else if (length(sigVars)==1) {
      solutionText1 <- paste(c(solutionText1, "Folgender Prädiktor trägt signifikant zur Vorhersage bei:<br>", sigText), collapse = "")
    } else if (length(sigVars)>1) {
      solutionText1 <- paste(c(solutionText1, "Folgende Prädiktoren tragen signifikant zur Vorhersage bei:<br>", sigText), collapse = "")
    } 
    
    solution <- solutionText1
    
    return(solution)
  }
  
  getANOVASolution <- function() {
    anova <- anova()
    
    
    dfValue <- NULL
    if (isTwoFac()) {
      dfValue <- as.numeric(anova$main$asDF[3,"df"])
    } else {
      dfValue <- as.numeric(anova$main$asDF[1,"df"])
    }
    
    dfResiduals <- NULL 
    if (isTwoFac()) {
      dfResiduals <- as.numeric(anova$main$asDF[4,"df"])
    } else {
      dfResiduals <- as.numeric(anova$main$asDF[2,"df"])
    }
    
    fValue <- NULL 
    if (isTwoFac()) {
      fValue <- as.numeric(anova$main$asDF[3,"F"])
    } else {
      fValue <- as.numeric(anova$main$asDF[1,"F"])
    }
    fPrint = sprintf("%.2f", round(fValue,2))
    
    pValue <- NULL
    if (isTwoFac()) {
      pValue <- as.numeric(anova$main$asDF[3,"p"])
    } else {
      pValue <- as.numeric(anova$main$asDF[1,"p"])
      
    }
    
    pText <- NULL
    if(pValue<=0.05) {
      pText <- "einen"
    } else if(pValue>0.05) {
      pText <- "keinen"
    }
    pRounded = round(pValue, digits = 3)
    pPrint = sprintf("%.3f", round(pValue,3))
    pApa <- NULL
    if (pRounded==0) {
      pApa = "< .001"
    } else if (pRounded==1.00){
      pApa = "= 1.000"
    } else {
      pApa = sub(".", "", as.character(pPrint))
      pApa = paste(c("= ",pApa), collapse = "")     
    }
    
    ANOVAGruppen <- values$ANOVAGruppen
    ANOVAGeschlecht <- ANOVAGruppen[ANOVAGruppen$Name == ANOVAGruppe(),"Geschlecht"]
    ANOVAGeschlecht = as.character(ANOVAGeschlecht[1, 1])
    ArtikelANOVA <- NULL
    Auspr1 <- NULL
    if (ANOVAGeschlecht=="m") {
      ArtikelANOVA = "den"
      Auspr1 = "unterschiedlichen"
    } else if (ANOVAGeschlecht=="f") {
      ArtikelANOVA = "die"
      Auspr1 = "unterschiedliche"
    } else if (ANOVAGeschlecht=="n") {
      ArtikelANOVA = "das"
      Auspr1 = "unterschiedliche"
    }
    
    etaSqPercentage <- NULL
    if (isTwoFac()) {
      etaSqPercentage <- substr(anova$main$asDF[3,"etaSq"], start = 3, stop = 4)
    } else {
      etaSqPercentage <- substr(anova$main$asDF[1,"etaSq"], start = 3, stop = 4)
    }
    
    if (as.character(substr(etaSqPercentage, start = 1, stop = 1))=="0") {
      etaSqPercentage <- sub(".", "", etaSqPercentage)
    }
    percentString <- paste(c(etaSqPercentage,"%"), collapse = "")     
    
    ANOVAGruppeString <- NULL
    if (ANOVAGeschlecht=="m") {
      ANOVAGruppeString <- paste(c("des ",ANOVAGruppe(),"s"), collapse = "")
    } else if (ANOVAGeschlecht=="f") {
      ANOVAGruppeString <- paste(c("der ",ANOVAGruppe(),""), collapse = "")
    } else if (ANOVAGeschlecht=="n") {
      ANOVAGruppeString <- paste(c("des ",ANOVAGruppe(),"s"), collapse = "")
    }
    
    AVString <- NULL
    variablen <- values$variablen
    geschlechtAV = variablen[variablen$Variable_AV == AV(),"Geschlecht_AV"]
    geschlechtAV = as.character(geschlechtAV[1, 1])
    
    AV <- AV()
    AValternative =  variablen[variablen$Variable_AV == AV,"Variable_AV_alternativ"]
    if (sum(!is.na(AValternative))==1) {
      AValternative = as.character(AValternative[1, 1])
      if (AV=="durchschnittlichen Jahreseinkommen") {
        AV = variablen[variablen$Variable_AV == AV,"Variable_AV_alternativ2"]
        AV = as.character(AV[1, 1])
      } else {
        AV = AValternative
      }
    }
    
    if (geschlechtAV=="m") {
      AVString <- paste(c("den ",AV), collapse = "")
    } else if (geschlechtAV=="f") {
      AVString <- paste(c("die ",AV), collapse = "")
    } else if (geschlechtAV=="n") {
      AVString <- paste(c("das ",AV), collapse = "")
    }
    
    pNorm <- anova$assump$norm$asDF[1,"p[sw]"]
    pEqv <- anova$assump$homo$asDF[1,"p"]
    
    if (!(is.na(pNorm))) {
      pNormRounded = round(pNorm, digits = 3)
      pNormPrint = sprintf("%.3f", round(pNorm,3))
      pNormApa <- NULL
      if (pNormRounded==0) {
        pNormApa = "< .001"
      } else if (pNormRounded==1.00){
        pNormApa = "= 1.000"
      } else {
        pNormApa = sub(".", "", as.character(pNormPrint))
        pNormApa = paste(c("= ",pNormApa), collapse = "")  
      }
      
      pEqvRounded = round(pEqv, digits = 3)
      pEqvPrint = sprintf("%.3f", round(pEqv,3))
      pEqvApa <- NULL
      if (pEqvRounded==0) {
        pEqvApa = "< .001"
      } else if (pEqvRounded==1.00){
        pEqvApa = "= 1.000"
      } else {
        pEqvApa = sub(".", "", as.character(pEqvPrint))
        pEqvApa = paste(c("= ",pEqvApa), collapse = "")  
      }
      if (pNorm>0.05) {  
        prereqText1 = paste(c("<br>Die Voraussetzung der Normalverteilung ist laut Shapiro-Wilk-Test erfüllt (<i>p</i> ",pNormApa,")."), collapse = "")
      } else {
        prereqText1 = paste(c("<br>Die Voraussetzung der Normalverteilung ist laut Shapiro-Wilk-Test verletzt (<i>p</i> ",pNormApa,")."), collapse = "")
      }
      if (pEqv>0.05) {
        prereqText2 = paste(c("Die Voraussetzung der Varianzhomogenität ist laut Levene-Test erfüllt (<i>p</i> ",pEqvApa,")."), collapse = "")
      } else {
        prereqText2 = paste(c("Die Voraussetzung der Varianzhomogenität ist laut Levene-Test verletzt (<i>p</i> ",pEqvApa,")."), collapse = "")
      }
    }
    
    solution <- NULL
    if (isTwoFac()) {
      ANOVAGeschlecht2 <- ANOVAGruppen[ANOVAGruppen$Name == ANOVAGruppe2(),"Geschlecht"]
      ANOVAGeschlecht2 = as.character(ANOVAGeschlecht2[1, 1])
      
      ANOVAGruppeString2 <- NULL
      if (ANOVAGeschlecht2=="m") {
        ANOVAGruppeString2 <- paste(c("dem ",ANOVAGruppe2()), collapse = "")
      } else if (ANOVAGeschlecht2=="f") {
        ANOVAGruppeString2 <- paste(c("der ",ANOVAGruppe2()), collapse = "")
      } else if (ANOVAGeschlecht2=="n") {
        ANOVAGruppeString2 <- paste(c("dem ",ANOVAGruppe2()), collapse = "")
      }
      
      solutionText1 <- paste(c("Die Varianzanalyse ergab ",pText," signifikanten Effekt ",ANOVAGruppeString," in Kombination mit ", ANOVAGruppeString2 ," auf ",AVString," (<i>F</i>(",dfValue,",",dfResiduals,") = ",fPrint,"; <i>p</i> ",pApa,")."), collapse = "")
      if(pValue<=0.05) {
        solution <- paste(c(solutionText1," Die Gesamtunterschiedlichkeit aller Messwerte ist zu ",percentString," auf ",ArtikelANOVA," ",Auspr1," ",ANOVAGruppe()," in Kombination mit ",ANOVAGruppeString2," zur\u00FCckzuf\u00FChren (η<sup>2</sup> = ",round(anova$main$asDF[3,"etaSq"], digits = 2),")."), collapse = "")
      } else if(pValue>0.05) {
        solution <- solutionText1
      }
      
    } else {
      solutionText1 <- paste(c("Die Varianzanalyse ergab ",pText," signifikanten Effekt ",ANOVAGruppeString," auf ",AVString," (<i>F</i>(",dfValue,",",dfResiduals,") = ",fPrint,"; <i>p</i> ",pApa,")."), collapse = "") 
      if(pValue<=0.05) {
        solution <- paste(c(solutionText1," Die Gesamtunterschiedlichkeit aller Messwerte ist zu ",percentString," auf ",ArtikelANOVA," ",Auspr1," ",ANOVAGruppe()," zur\u00FCckzuf\u00FChren (η<sup>2</sup> = ",round(anova$main$asDF[1,"etaSq"], digits = 2),")."), collapse = "")
      } else if(pValue>0.05) {
        solution <- solutionText1
      }
    }
    
    
    if (values$prereqChecked) {
      solution <- paste(c(solution,prereqText1,prereqText2), collapse = " ")
    }
    return(solution)
  }
  
  getChiSquaredSolution <- function() {
    chiSq_test <- chiSq_test()
    
    chiSq_val <- chiSq_test$chiSq$asDF[1,"value[chiSq]"]
    chiSq_p <- chiSq_test$chiSq$asDF[1,"p[chiSq]"]
    df <- chiSq_test$chiSq$asDF[1,"df[chiSq]"]
    pChi2 <- chiSq_test$chiSq$asDF[1,"p[chiSq]"] 
    X2 <- "X\U00B2"
    solTextSignifikant <- NULL
    solTextUnabhängig <- NULL
    chiSq_print <- round(chiSq_val, digits = 2)
    
    
    UV <- paste0("\"",UV(),"\"")
    
    posRow <- filter(chiSq_test$freqs$asDF, rownames(chiSq_test$freqs$asDF) == UV)
    posFrauen <- posRow[,"1[count]"]
    posMänner <- posRow[,"2[count]"]
    
    percentageF <- helperGetPercentage(posRow[,"1[pcRow]"])
    percentageM <- helperGetPercentage(posRow[,"2[pcRow]"])
    
    
    solTextProzente <- NULL
    if (posFrauen > posMänner) {
      solTextProzente <- paste(c("",percentageF," Prozent der Frauen, aber nur ",percentageM," Prozent der Männer leiden an ",UV(),". "), collapse = "")  
    } else if (posMänner > posFrauen) {
      solTextProzente <- paste(c("",percentageM," der Männer, aber nur ",percentageF," der Frauen leiden an ",UV(),". "), collapse = "")  
    } else if (posFrauen == posMänner) {
      solTextProzente <- paste(c("",percentageM," der Männer sowie ",percentageF," Frauen leiden an ",UV(),". "), collapse = "")  
    }
    
    
    if (chiSq_val > 3.84) {
      solTextUnabhängig <- "Die Merkmale sind nicht unabhängig voneinander."
      solTextSignifikant <- "statistisch signifikant"
    } else {
      solTextSignifikant <- "statistisch nicht signifikant"
      solTextUnabhängig <- "Die Merkmale sind unabhängig voneinander."
    }
    
    pChi2Rounded = round(pChi2, digits = 3)
    pChi2Print = sprintf("%.3f", round(pChi2,3))
    pChi2Apa <- NULL
    if (pChi2Rounded==0) {
      pChi2Apa = "< .001"
    } else if (pChi2Rounded==1.00){
      pChi2Apa = "= 1.000"
    } else {
      pChi2Apa = sub(".", "", as.character(pChi2Print))
      pChi2Apa = paste(c("= ",pChi2Apa), collapse = "")  
    }
    solution <- paste(c(solTextProzente, solTextUnabhängig," Der Effekt ist ",solTextSignifikant," (",X2,"(",df,") = ",chiSq_print,"; <i>p</i> ",pChi2Apa,")."), collapse = "")
    
    
    return(solution)
  }
  
  newTask <- eventReactive(input$newTaskButton, {
    values$users_data$NewTaskCount = values$users_data$NewTaskCount + 1
    if (input$selectTest=="t-Test") {
      currentTest("t-Test")
    } else if (input$selectTest == "Paired t-Test"){
      currentTest("Paired t-Test")
    } else if(input$selectTest=="Korrelation") {
      currentTest("Korrelation")
    } else if(input$selectTest=="Regression") {
      currentTest("Regression")
    } else if(input$selectTest=="log. Regression") {
      currentTest("log. Regression")
    } else if(input$selectTest=="ANOVA") {
      currentTest("ANOVA")
    } else if (input$selectTest=="Chi-Quadrat"){
      currentTest("Chi-Quadrat")
    }
    
    
    if (currentTest() == "t-Test"){
      newTaskText() 
      shinyjs::enable("checkPrereq")
      newScaleInfo()
      testResult <- NULL
      tryCatch(
        expr = {
          testResult <- doTtest()
        },
        error = function(err){
          click("newTaskButton")
        }
      )
    } else if(currentTest() == "Paired t-Test"){
      newTaskText() 
      newScaleInfo()
      shinyjs::enable("checkPrereq")
      testResult <- NULL
      # tryCatch(
      #   expr = {
      testResult <- doPairedTtest()
      # },
      # error = function(err){
      #   click("newTaskButton")
      # }
      # )
    } else if(currentTest() == "Korrelation"){
      newTaskText() 
      testResult <- NULL
      tryCatch(
        expr = {
          testResult <- doCorrelation()
        },
        error = function(err){
          click("newTaskButton")
        }
      )
    } else if(currentTest() == "Regression"){
      newTaskText() 
      testResult <- NULL
      tryCatch(
        expr = {
          testResult <- doRegression()
        },
        error = function(err){
          click("newTaskButton")
        }
      )
    } else if(currentTest() == "log. Regression"){
      newTaskText() 
      testResult <- NULL
      tryCatch(
        expr = {
          testResult <- doLogRegression()
        },
        error = function(err){
          click("newTaskButton")
        }
      )
    } else if(currentTest() == "ANOVA"){
      isTwoFac <- wakefield::r_sample_logical(1, prob = c(0.5, 0.5), name = "Logical")
      isTwoFac(isTwoFac)
      newTaskText() 
      newScaleInfo()
      testResult <- NULL
      tryCatch(
        expr = {
          testResult <- doANOVA()
        },
        error = function(err){
          click("newTaskButton")
        }
      )
    } else if (currentTest() == "Chi-Quadrat"){
      newScaleInfo()
      newTaskText() 
      testResult <- NULL
      tryCatch(
        expr = {
          testResult <- doChiSquared()
        },
        error = function(err){
          click("newTaskButton")
        }
      )
    }
    return(testResult)
  })
  
  newTaskText <- eventReactive(input$newTaskButton, {
    
    if (currentTest() == "t-Test"){
      taskText <- getTaskText()
    } else if (currentTest() == "Paired t-Test"){
      taskText <- getTaskText() 
    } else if (currentTest() == "Korrelation"){
      taskText <- getTaskText()
    } else if (currentTest() == "Regression"){
      taskText <- getTaskText()
    } else if (currentTest() == "log. Regression"){
      taskText <- getTaskText()
    } else if (currentTest() == "ANOVA"){
      taskText <- getTaskText()
    } else if (currentTest() == "Chi-Quadrat"){
      taskText <- getTaskText()
    }
    return(taskText)
  })
  
  showSolutionText <- eventReactive(input$showSolutionButton, {
    shinyjs::show("textSolution")
    
    # Determine the solution text based on the current test
    if (currentTest() == "t-Test") {
      solutionText <- getTtestSolution()
    } else if (currentTest() == "Paired t-Test") {
      solutionText <- getPairedTtestSolution()
    } else if (currentTest() == "Korrelation") {
      solutionText <- getCorrSolution()
    } else if (currentTest() == "Regression") {
      solutionText <- getRegSolution()
    } else if (currentTest() == "ANOVA") {
      solutionText <- getANOVASolution()
    } else if (currentTest() == "log. Regression") {
      solutionText <- getLogRegSolution()
    } else if (currentTest() == "Chi-Quadrat") {
      solutionText <- getChiSquaredSolution()
    } 
    
    # Check feedback mode
    if (input$feedbackMode == "dynamic") {
      # Dynamic feedback: Send to GPT
      userInput <- input$solutionInput  # Input from user
      gptPrompt <- paste(
        "Folgendes ist der Lösungsversuch des Nutzers:\n\n",
        userInput,
        "\n\nHier ist die korrekte Lösung:\n\n",
        solutionText,
        "\n\nBitte geben Sie Feedback, indem Sie die beiden Lösungen vergleichen, Stärken, Schwächen und Verbesserungspotenziale aufzeigen."
      )
      
      # Replace with OpenAI API or custom HTTP call
      response <- POST(
        url = "https://api.openai.com/v1/chat/completions",
        add_headers(Authorization = paste("Bearer", Sys.getenv("OPENAI_API_KEY"))),
        content_type_json(),
        encode = "json",
        body = list(
          model = "gpt-4o",
          messages = list(list(role = "system", content = gptPrompt)),
          temperature = 0.5  # Adjust as needed
        )
      )
      
      # Extract and return GPT response
      
      feedback_data <- content(response, as = "parsed")
      gptResponse <- feedback_data$choices[[1]]$message$content
      print(gptResponse)
      
      return(gptResponse)
    } else {
      # Static feedback: Return predefined solution text
      return(solutionText)
    }
  })
  
  newQuestionText <- eventReactive(input$newTaskButton, {
    if (currentTest() == "t-Test"){
      questionText <- getQuestionText()
    } else if(currentTest() == "Paired t-Test"){
      questionText <- getQuestionText()
    } else if(currentTest() == "Korrelation"){
      questionText <- getQuestionText()
    } else if(currentTest() == "Regression"){
      questionText <- getQuestionText()
    } else if(currentTest() == "log. Regression"){
      questionText <- getQuestionText()
    } else if(currentTest() == "ANOVA"){
      questionText <- getQuestionText()
    } else if(currentTest() == "Chi-Quadrat"){
      questionText <- paste(c("Sind die Merkmale „",UV(),"“ und „Geschlecht“ unabhängig voneinander?"), collapse = "")  
    } 
    return(questionText)
  })
  
  output$plot1 <- renderPlot({
    reg_test()$models[[1]]$assump$resPlots[[1]]
  },height = function(){
    dev.size("px")[2]*plotHeightVar},width = function(){
      dev.size("px")[1]*plotWidthVar})
  
  output$tTest <- renderPrint({ 
    testResult <- newTask()
    options(digits=4)
    print(testResult)
  })
  
  output$textTask <- renderText({
    newTaskText()
  })
  
  output$scaleTable <- renderTable(
    scaleTableDF(),
    spacing = "xs",
    striped = TRUE
  )
  
  output$textQuestion <- renderText({
    newQuestionText()
  }) 
  
 output$textSolution <- renderUI({
  HTML(markdown::markdownToHTML(text = showSolutionText(), fragment.only = TRUE))
 })
  
  output$scaleText <- renderUI({
    HTML(scaleText())
  }) 
  
  observeEvent(input$showSolutionButton, { 
    if (input$solutionInput=="") {
      showModal(modalDialog(
        "Zunächst muss eine Lösung eingegeben werden.",
        size = "s",
        footer = modalButton("OK"),
        easyClose = TRUE
      ))
    } else {
      showSolutionText()
      shinyjs::show("box3")
      shinyjs::show("textSolution")
    }
    
  })
  
  observeEvent(input$newTaskButton, {
    updateTextAreaInput(session, "solutionInput", value = paste(""))
    if (input$selectTest != "Regression") {
      shinyjs::hide("plot1ID")
    }
    newTask()
    newQuestionText()
    shinyjs::show("solutionInput")
    shinyjs::show("showSolutionButton")
    shinyjs::show("box1")
    shinyjs::show("box2")
    shinyjs::hide("box3")
    shinyjs::hide("textSolution")
  })
  
  observeEvent(input$selectTest, {
    if (input$selectTest == "Korrelation" | input$selectTest == "log. Regression" | input$selectTest == "Chi-Quadrat") {
      shinyjs::disable("checkPrereq")
    } else {
      shinyjs::enable("checkPrereq")
    }
  })
  
  # test runs
  # observe({
  #   click("newTaskButton")
  #   invalidateLater(100)
  # })
}