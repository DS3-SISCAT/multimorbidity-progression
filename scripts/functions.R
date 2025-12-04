# Aquest arxiu cont? les funcions utilitzades en el projecte. S'estructuren per apartats. 


# 1. Processat de dades ####

  # Edat al moment del diagnostic (es tracta d'una aproximacio, ja que no tenim data de naixement)
  getEdatReal <- function(edat, dataDiagnostic, dataCarrega){
    as.numeric(edat - floor(difftime(dataCarrega, dataDiagnostic, units = "days") / 365.25))
  }
  
  # Funcions de diferencia entre diagnostic actual i anterior. Necessari tenir-ho ordenat per CIP, DATA i W!
  getDiesAnteriorDiagnostic <- function(dd){
    vectorTemps <- c(as.character(dd[1,"Data"]), sapply(2:nrow(dd), function(x) dd$Data[(x-1)]))
    dd$DiesAnteriorDiagnostic = as.numeric(as.Date(dd$Data, "%Y%m%d") - as.Date(vectorTemps, "%Y%m%d"))
    dd[which(dd$Disease_number == 1), "DiesAnteriorDiagnostic"] = 0
    return(dd$DiesAnteriorDiagnostic)
  }
  
  # función para calcular la diferencia en días
  diferencia_dias <- function(fecha_columna, fecha_fija) {
    # Convertir las fechas del dataset a formato Date, manteniendo los NA
    fechas_convertidas <- as.Date(as.character(fecha_columna), format = "%Y%m%d")
    
    # Convertir la fecha fija a Date
    fecha_fija <- as.Date(fecha_fija)
    
    # Calcular la diferencia en días, manteniendo los NA
    diferencia <- as.numeric(difftime(fecha_fija, fechas_convertidas, units = "days"))
    
    return(diferencia)
  }
  
  
  getDiesSeguentDiagnostic <- function(t){
    c(sapply(1:(length(t)-1), function(x) t[(x+1)]),0)
  }
  
  getwanterior <- function(dd){
    vector_w <- c(0, sapply(2:nrow(dd), function(x) dd$w[(x-1)]))
    dd$w_anterior = vector_w
    dd[which(dd$Disease_number == 1), "w_anterior"] = 0
    return(dd$w_anterior)
  }
  
  getwseguent <- function(w){
    c(sapply(1:(length(w)-1), function(x) w[(x+1)]),0)
  }

  getCCSseguent <- function(df){
    vector_ccs <- c(sapply(1:(nrow(df)-1), function(x) df$CCS[(x+1)]),0)
    return(vector_ccs)
  }
  
  get_combinations <- function(df, tamany = 2){
    as.data.frame(combinations(n = nrow(unique(df[,1])), r = tamany, v = df$CCS))
  }
    
  deleteCCS <- function(llistat_ccs, df){
    df[which(!df$CCS %in% as.character(llistat_ccs)),]
  }

# 2. Predicci? log?stica de probabilitat nova malaltia ####
  
  # Crea la variable resposta
  contrauNovaMalalita <- function(dd, t, dataLimit = "2020-12-10", eliminarRegistres=TRUE){
    dd <- as.data.frame(dd)
    
    dd[dd$esUltimCCS==1, "DiesSeguentDiagnostic"] = 
      as.numeric(difftime(as.Date(dataLimit), as.Date(dd[dd$esUltimCCS==1, "Data"], "%Y%m%d"), units = "days"))
    
    dd$target = ifelse(dd$DiesSeguentDiagnostic>t, 0, 1)
    
    
    dd[dd$esUltimCCS==1, "target"] = with(dd[dd$esUltimCCS==1,], ifelse(DiesSeguentDiagnostic>t, 0, 99))
    
    #Elimina els registres on no podem asegurar que contragui nova malaltia en temps t
    dd <- dd[which(!dd$target==99),]
    
    #Elimina registres de m?ltiples malalties diagnosticades a la vegada
    if(eliminarRegistres == TRUE){
      dd <- dd[which(!dd$DiesSeguentDiagnostic==0),]
    }
    dd$target = as.factor(dd$target)
    return(dd)
  }
  
  # Divideix dades en train i test, depenent del pacient (un CIP només pot estar a train o a test)
  predictionSetup <- function(proporcio, dd, variables=NA, balanced=F, seed=27, plot=T){
    if(!is.na(variables)) dd <- dd[,c("CIP",variables,"target")]
    pacients <- unique(dd$CIP)
    
    smp_size <- floor(proporcio * length(pacients))
    set.seed(seed)
    train_cips <- pacients[sample(seq_len(length(pacients)), size = smp_size)]
    
    train = dd[dd$CIP %in% train_cips,]
    test=dd[!dd$CIP %in% train_cips,]
    
    # Es redueix la mida de la mostra per contenir el mateix nombre d'observacions de cada classe
    if(balanced == T) train <- downSample(train, train$target, list=T)[[1]]
    
    return(list(train=train,test=test))
  }
  
  # Crear escenari fictici segons "tiempo" (1, 3, 5 anys). 
  generarEscenariTemporal <- function(dd, matrix_ccs, maxNumCCS=10, intervals_temps=c(365,365*3,365*5)){
    #dd: datos     // intervals_temps: d?as vista (3 a?os: 1095, 5 a?os: 1825)
    
    # Comprovacions
    if(maxNumCCS > max(dd$Disease_number)) stop("Nombre maxim de malalties superior al trobat a les dades d'entrada")
    
    # Inicialitzar elements
    n_models = maxNumCCS*length(intervals_temps)
    llista_models <- vector(mode = "list", length = n_models)
    metriques <- data.frame(matrix(nrow = n_models, ncol=9))
    colnames(metriques) <- c("m","t","N","positius","deviance","acc","sens","spec","auc")
    
    # Iteraci? pel nombre de malalties
    i=0
    for(m in 1:maxNumCCS){
      for(t in intervals_temps){
        i=i+1
        
        # Subset dades
        ddaux = cbind(dd %>% filter(Disease_number==m), 
                      matrix_ccs %>% filter(Disease_number==m) %>% dplyr::select(c(-1,-2)))
        
        # Crear variable resposta
        ddaux$target = ifelse(ddaux$DiesSeguentDiagnostic<t,1,0)
        ddaux[ddaux$DiesSeguentDiagnostic == 0, "target"] = 0
        ddaux$target <- as.factor(ddaux$target)
        table(ddaux$target)
        
        #Preparar dades
        if(m==1){
          variablesModel <- c("EdatReal","Sexe","w",
                              colnames(matrix_ccs)[3:52])
          
        }
        else{
          variablesModel <- c("EdatReal","Sexe","DiesPrimerDiagnostic","DiesAnteriorDiagnostic","w_inicial","w_anterior","w",
                              colnames(matrix_ccs)[3:52])
        }
        
        dades.model <- predictionSetup(proporcio = .75, dd = ddaux, variables = variablesModel, balanced = T, seed = 7)
        train <- dades.model[["train"]]
        test <- dades.model[["test"]]
        
        # Entrenar Model
        f = as.formula(paste('target ~', paste(variablesModel,collapse = '+')))
        model_aux <- glm(formula = f, data = train, family = binomial)
        
        # Matriu de confusi?
        cf = confusionMatrix(table(ifelse(predict(model_aux, newdata = test, type="response")>0.5,1,0), test$target))
        
        # Resultats
        metriques[i,1] <- m
        metriques[i,2] <- t/365
        metriques[i,3] <- nrow(ddaux)
        metriques[i,4] <- table(ddaux$target)[2]
        metriques[i,5] <- model_aux$deviance
        metriques[i,6] <- as.numeric(cf$overall[1])
        metriques[i,7] <- as.numeric(cf$byClass[2])
        metriques[i,8] <- as.numeric(cf$byClass[3])
        metriques[i,9] <- as.numeric(auc(test$target, predict(model_aux, newdata = test, type="response")))
        llista_models[[i]] <- model_aux
      }
    }
    return(list(models = llista_models,
                resultats=metriques))
  }
  
  # Avalua models depenent de l'escenari i les variables elegides
  escenarisLogit <- function(dd, matriuccs, m, t, vars, nom,  plot=T, eliminarRegistres=TRUE){
    # Subset de les dades
    dd = cbind(dd %>% filter(Disease_number==m), 
               matriuccs %>% filter(Disease_number==m) %>% dplyr::select(c(-1,-2)))
    
    # Crear variable resposta i netejar registres
    dd = contrauNovaMalalita(dd=dd, t=t, eliminarRegistres = eliminarRegistres)
    
    # Setup
    dadesModel <- predictionSetup(proporcio = .75, dd = dd, variables = vars[!grepl(pattern = "\\*", vars)], balanced = T, seed = 7)
    train <- dadesModel[["train"]]
    test <- dadesModel[["test"]]
    
    # Model de regressio logistica
    f = as.formula(paste('target ~', paste(vars,collapse = '+')))
    model <- glm(formula = f, data = train, family = binomial)
    
    # Resultats Test 
    cf = confusionMatrix(table(ifelse(predict(model, newdata = test, type="response")>0.5,1,0), test$target))
    prob = predict(model, newdata=test, type="response")
    roc = roc.curve(scores.class0 = prob[test$target==1], scores.class1 = prob[test$target==0], curve = T)
    pr = pr.curve(scores.class0 = prob[test$target==1], scores.class1 = prob[test$target==0], curve = T)
    
    if(plot == T){plot(pr)}
    
    c(
      "escenari" = paste0("m = ",m, " ; t = ",t),
      "model" = nom,
      "N" = paste0(nrow(dd), " (", round(100*table(dd$target)[2] / nrow(dd),2),"%)"),
      "BIC" = round(BIC(model)),
      "sensitivity" = round(as.numeric(cf$byClass[1]),4),
      "specificity" = round(as.numeric(cf$byClass[2]),4),
      "Balanced-Acc" =  round(as.numeric(cf$byClass[11]),4),
      "roc-curve" = round(roc$auc,4),
      "pr-curve" = round(pr$auc.integral,4)
    )
  }
  
  # Escenari anual generacio de dades: mmP_model2019.R
  generarDadesescenariAnual <- function(ps,  intervalT = c('20181201', "20191201"), max_ccs = 20, balancejat = T){
    
    if(length(intervalT) != 2) return("Proporcionar 2 valors: data inicial i data final")
    
    # Data processing
      cipsInterval <- unique(ps[ps$Data >=intervalT[1] & ps$Data < intervalT[2],"CIP"])
      length(cipsInterval)
      
      # Obtenir ?ltim problema de salut dels pacients 2019
      poblacio2019 <- ps %>% 
        filter(Data >= intervalT[1], Data < intervalT[2]) %>%
        arrange(CIP,Data,w) %>%  
        group_by(CIP) %>%
        slice(n())
      poblacio2019 <- as.data.frame(poblacio2019)
      
      # Obteniur matriu CCS des de 2019 cap enrere
      matriuccs <- ps %>%
        filter(Data < intervalT[2], CIP %in% cipsInterval) %>%
        select(CIP,CCS,EdatReal) %>%
        pivot_wider(names_from = CCS, values_from=c(EdatReal))
      # Order per CCS mes frequents
      ccsfreq = table(ps$CCS)
      ccsfreq = names(ccsfreq)[order(ccsfreq, decreasing = T)]
      ccsfreq = ccsfreq[ccsfreq %in% unique(poblacio2019$CCS)]
      matriuccs = matriuccs[,ccsfreq]
      colnames(matriuccs) = paste0("X", colnames(matriuccs))
      matriuccs[is.na(matriuccs)] = 0
      
      # Obteniur matriu w i t des de 2019 cap enrere
      matriuw <- ps %>%
        filter(Data < intervalT[2], CIP %in% cipsInterval, Disease_number <= max_ccs) %>%
        select(CIP,Disease_number,w,DiesAnteriorDiagnostic) %>%
        pivot_wider(names_from = Disease_number, values_from=c(w,DiesAnteriorDiagnostic))
      matriuw[is.na(matriuw)] = 0
      
      # Unir dades
      dd <- cbind(poblacio2019, matriuccs, matriuw[,-1])
    
    # # PCA
    #   pca = prcomp(matriuccs[, !names(matriuccs)=="CIP"] , scale=T)
    #   dd <- cbind(dd, pca$x[,1:8])
      
    # Target
      dd$target <- as.factor(dd$esUltimCCS)
      N <- paste0(nrow(dd), " (", round(100*table(dd$target)[2] / nrow(dd),2),"%)")
      
    # TrainTestSplit
      dadesModel <- predictionSetup(proporcio = .75, dd = dd, balanced = balancejat, seed = 7)
      train <- dadesModel[["train"]]
      test <- dadesModel[["test"]]
      
    return(list(train, test, N))
  }
  
  # Avaluaci? de models logistics
  modelatLogistic <- function(train, test, vars, plot_PR=F, model=NA){
    
    if(is.na(model)){
      # Model
      f = as.formula(paste('target ~', paste(vars, collapse = '+')))
      model <- glm(formula = f, data = train, family = binomial)
      summary(model)
    }
    
    # Resultats Test 
    threshold <- length(which(train$target==1)) / nrow(train)
    
    cf = confusionMatrix(table(ifelse(predict(model, newdata = test, type="response")>threshold,1,0), test$target))
    prob = predict(model, newdata=test, type="response")
    roc = roc.curve(scores.class0 = prob[test$target==1], scores.class1 = prob[test$target==0], curve = T)
    pr = pr.curve(scores.class0 = prob[test$target==1], scores.class1 = prob[test$target==0], curve = T)
    
    if(plot_PR) plot(pr)
    c(
      "BIC" = round(BIC(model)),
      "sensitivity" = round(as.numeric(cf$byClass[1]),4),
      "specificity" = round(as.numeric(cf$byClass[2]),4),
      "Balanced-Acc" =  round(as.numeric(cf$byClass[11]),4),
      "roc-curve" = round(roc$auc,4),
      "pr-curve" = round(pr$auc.integral,4)
    )
  }
  
  
  # Odds ratio plot
  generar_ggplot <- function(datos){
    grafic <- 
      ggplot(datos, aes(x=OR, y=Variable))+
      geom_point(col="#0072B2")+
      geom_linerange(aes(xmin = CI_lower, xmax = CI_upper), col="#0072B2")+
      # scale_x_continuous(breaks = c(0.5, 1, 2, 3, 4))+
      scale_y_discrete(limits=rev)+
      coord_trans(x = "log2")+
      geom_vline(xintercept = 1, linetype = "dashed", col = "#D55E00")+
      theme_bw()+
      theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
            axis.text = element_text(size=8))
    
    return(grafic)
  }
  