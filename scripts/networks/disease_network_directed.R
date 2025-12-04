# 0. Set up ###########
  #Clean space
  rm(list=ls())
  
  #Libraries
  suppressWarnings(suppressMessages(library(dplyr)))
  suppressWarnings(suppressMessages(library(tidyr)))
  suppressWarnings(suppressMessages(library(ggplot2)))
  suppressWarnings(suppressMessages(library(data.table)))
  suppressWarnings(suppressMessages(library(colorspace)))
  suppressWarnings(suppressMessages(library(chromote)))
  suppressWarnings(suppressMessages(library(visNetwork)))
    
# 1. Data loading ###########
  inputsDir = "./inputs/"
  scriptsDir = "./scripts/"
  outputsDir = "./outputs/"
  
  #Data
  ps = data.table::fread(paste0(inputsDir,"ps_dataset.txt"), sep="|", header=T)
  dd = data.table::fread(paste0(inputsDir,"mmp_scenaris_full_10y.txt"), sep="|", header=T)
  load(paste0(inputsDir,"labels.RData"))
  
  # Funcions
  source(paste0(scriptsDir,"functions.R"))
  

  
# 2. Parameters ###########
  
  # Number of distinct patients. Include patients without any disease
  n_pacients <- length(unique(dd$CIP))
  
  # Years 
  y = 10
  y_interval = seq(2,8,2)
  
  # End date of clinical follow-up
  end_date = max(ps$Data) # 20221231
  
  # Start date of the clinical follow-up    
  start_date = paste0(as.numeric(substr(end_date, 1,4))-y, 
                      substr(end_date, 5,8))
  
  cutoff_dates = paste0(as.numeric(substr(end_date, 1,4))-y_interval, 
                        substr(end_date, 5,8))
  
  # Q80 Strata cutoff value
  str_threshold = as.numeric(quantile(dd$w_f, probs = 0.8))


# 3. Data processing #########
  
  # Reducimos los datasets para descartar la disease matrix para mejorar espacio en disco
  dd <- dd %>% 
    select(-starts_with("X")) %>% 
    select(-starts_with("PC"))
  
  # Transformamos el dataset de diseases a data.table para mejorar eficiencia
  ps = as.data.table(ps[, c("CIP","Sexe","CCS","Disease_number", "Data", "w")])
  setkey(ps, CIP)
  
  # Añadimos variable para saber si la enfermedad ha aparecido antes o después del salto de estado de GMA
  ps$Strata = ifelse(ps$w > str_threshold, 'Post', 'Pre')
  
  # Añadimos variable para saber si la enfermedad ha aparecido antes o después del inicio del período de estudio
  ps$Period = ifelse(ps$Data >= start_date , 'Baseline', 'End')
  

# 4. Global Disease prevalence #########
  
  # Prevalencia de enfermedades
  pob_disease_prev = ps[, .N, by = CCS]
  pob_disease_prev$prev = pob_disease_prev$N / n_pacients
  
  # Añadimos descriptores 
  pob_disease_prev <- pob_disease_prev %>%
    arrange(-N) %>%
    inner_join(.,labels, by=c('CCS' = 'CCS')) %>%
    rename(CodiCCS = CCS, CCS = label) %>% 
    mutate(id = 1:n())
  
  # Top 20
  disease_prev_top20 <- pob_disease_prev %>%
    mutate(rank = rank(-prev)) %>%
    filter(rank <= 20)
  
  # Gráfico del Top 20 con categorías simplificadas
  ggplot(disease_prev_top20, aes(x = reorder(CCS, prev), y = prev, fill = category_short)) +
    geom_bar(stat = "identity", alpha = 0.8) +
    coord_flip() +
    scale_y_continuous(labels = scales::percent_format(scale = 100)) +
    scale_fill_brewer(palette = "Set3") +  # Paleta de colores más distintiva
    labs(x = NULL, 
         y = "Prevalence (%)", 
         fill = "CCS body systems") +
    theme_bw() +
    theme(legend.position = "right")
  #ggsave(filename = paste0(outputsDir, "/Disease network/DiseasePrevalence_top20.png"),  width = 9, height = 4, dpi = 300)
  
  
  # Gráfico de todas las CCS disease categories, con marca en el top20
  ggplot(pob_disease_prev, aes(x = reorder(CCS, -prev), y = prev)) +
    geom_point(size = 1, color = "skyblue") +
    geom_line(aes(group = 1), color = "gray", alpha = 0.7) +
    geom_hline(yintercept = slice(disease_prev_top20, 20) %>% pull(prev), color = "red", linetype = "dashed") +  # Línea horizontal
    #annotate("text", x = 140, y = slice(disease_prev_top20, 20) %>% pull(prev), label = "Top 20 threshold", color = "red", hjust = 0, vjust = -0.5, size = 4) +
    scale_y_continuous(labels = scales::percent_format(scale = 100)) +
    labs(x = NULL, 
         y = "Prevalence (%)") +
    theme_bw() +
    theme(axis.text.x = element_blank(),  # Ocultar texto en el eje X
          axis.ticks.x = element_blank(), 
          text = element_text(size = 12))
  #ggsave(filename = paste0(outputsDir, "/Disease network/DiseasePrevalence.png"),  width = 6, height = 4, dpi = 300)

  
  
# 5. Directed networks of the overall population ###########

  # Crear directorio de salida
  auxDir = paste0("./app/All Population/")
  if (!dir.exists(auxDir)) {
    dir.create(auxDir)
  }
  
  # Malalties de la població de l'estudi (majors 18 anys a 10 anys enrere)
  patients_aux <- dd[, unique(CIP)]
  
  # Filtrar por pacientes relacionados
  ps_aux <- ps[J(patients_aux)] 
  
  n_pacients = length(patients_aux)
  
  # Prevalencia de enfermedades
  disease_prev = ps_aux[, .N, by = CCS]
  disease_prev$prev = disease_prev$N / n_pacients
  
  # Añadimos descriptores
  disease_prev <- disease_prev %>%
    arrange(-N) %>%
    inner_join(.,labels, by=c('CCS' = 'CCS')) %>%
    mutate(id = 1:n())
  
  # Calcular prevalencia Pre y Post por enfermedad
  prevalence_by_strata <- ps_aux[, .(Pre = sum(Strata == "Pre"),
                                     Post = sum(Strata == "Post")),
                                 by = CCS]
  
  # Calcular el ratio Pre/Post
  prevalence_by_strata[, ratio_strata := Pre / (Pre + Post)]
  
  # Unir datasets
  disease_prev <- disease_prev %>%
    inner_join(.,prevalence_by_strata, by='CCS') %>%
    rename(CodiCCS = CCS, CCS = label) 
  
  # Generar pares dirigidos basados en Disease_number
  pairs_data <- ps_aux[ps_aux, on = .(CIP), allow.cartesian = TRUE]
  pairs_data <- pairs_data[Disease_number < i.Disease_number, 
                           .(ccs_from = CCS, ccs_to = i.CCS, strata_from = Strata, strata_to = i.Strata)]
  
  # Calcular relación entre estratos
  pairs_data[, strata_relation := fifelse(strata_from == strata_to, strata_from, "Mixed")]
  
  # Contar ocurrencias por relación dirigida
  pair_counts <- pairs_data[, .N, by = .(ccs_from, ccs_to, strata_relation)]
  
  # Transformar datos en formato dirigido
  disease_pairs <- dcast(pair_counts, ccs_from + ccs_to ~ strata_relation, value.var = "N", fill = 0)
  
  # Renombrar columnas
  setnames(disease_pairs, c("Pre", "Post", "Mixed"), c("N_pre", "N_post", "N_mixed"))
  
  # Calcular total de conexiones dirigidas
  disease_pairs[, N := N_pre + N_post + N_mixed]
  
  # Tipo de relación predominante
  disease_pairs[, strata_relation := fifelse(N_pre >= N_post & N_pre >= N_mixed, "Pre",
                                             fifelse(N_post >= N_pre & N_post >= N_mixed, "Post", "Mixed"))]
  
  # Asociar prevalencias para ccs_from y ccs_to
  disease_pairs <- merge(disease_pairs, 
                         disease_prev[, .(CodiCCS, N, prev, CCS)], 
                         by.x = "ccs_to", by.y = "CodiCCS", all.x = TRUE)
  setnames(disease_pairs, "N.x", "N")
  setnames(disease_pairs, "N.y", "patients_to")
  setnames(disease_pairs, "CCS", "CCS_to")
  setnames(disease_pairs, "prev", "prev_to")
  
  
  disease_pairs <- merge(disease_pairs, 
                         disease_prev[, .(CodiCCS, N, prev, CCS)], 
                         by.x = "ccs_from", by.y = "CodiCCS", all.x = TRUE)
  setnames(disease_pairs, "N.x", "N")
  setnames(disease_pairs, "CCS", "CCS_from")
  setnames(disease_pairs, "N.y", "patients_from")
  setnames(disease_pairs, "prev", "prev_from")
  
  
  # Calcular Risk Ratios
  disease_pairs[, patients_from := as.numeric(patients_from)]
  disease_pairs[, patients_to := as.numeric(patients_to)]
  disease_pairs[, N := as.numeric(N)]
  disease_pairs[, Risk_Ratio := (N * n_pacients) / (patients_from * patients_to)]
  
  # ADjusted risk ratio
  disease_pairs[, Risk_Ratio_Adjusted := N / sqrt(patients_from * patients_to)]
  # Scale to range [0, 1]
  disease_pairs$Risk_Ratio_Adjusted <- (disease_pairs$Risk_Ratio_Adjusted - min(disease_pairs$Risk_Ratio_Adjusted)) / (max(disease_pairs$Risk_Ratio_Adjusted) - min(disease_pairs$Risk_Ratio_Adjusted))
  
  
  # Filtrar pares por Bonferroni y N
  alpha <- 0.05
  total_pairs <- nrow(disease_pairs)
  bonferroni_threshold <- round(alpha / total_pairs, 8)
  
  #disease_pairs[, p_value := 2 * pnorm(-abs(N))]
  disease_pairs[, p_bonferroni := p.adjust(2 * pnorm(-abs(N)), method = "bonferroni")]
  disease_pairs <- disease_pairs[p_bonferroni < bonferroni_threshold & N >= 50]
  
  
  # Seleccionar relaciones relevantes
  disease_connections <- disease_pairs %>%
    #filter(ccs_from == disease_aux | ccs_to == disease_aux) %>%
    arrange(-N) %>%
    slice_head(n = 30)
  
  secondary_diseases <- unique(c(disease_connections$ccs_from, disease_connections$ccs_to))
  
  secondary_connections <- disease_pairs %>%
    filter(ccs_from %in% secondary_diseases | ccs_to %in% secondary_diseases) %>%
    arrange(-Risk_Ratio) %>%
    slice_head(n = 60)
  
  d_network_pob <- bind_rows(disease_connections, secondary_connections) %>%
    distinct(ccs_from, ccs_to, .keep_all = TRUE)
  
  d_network_pob$p_bonferroni <- ifelse(d_network_pob$p_bonferroni==0, paste0('<', bonferroni_threshold), as.character(d_network_pob$p_bonferroni))
  # write.csv2(
  #   x = d_network_pob,
  #   file = paste0(auxDir, "/directed_disease_network.csv"),
  #   row.names = F
  # )
  
  nodes <- unique(c(d_network_pob$CCS_from, d_network_pob$CCS_to))
  
  # Preparar nodos
  nodes <- data.table(
    id = nodes, 
    label = disease_prev[match(nodes, disease_prev$CCS), CCS],  # Usar descriptores
    value = disease_prev[match(nodes, disease_prev$CCS), prev] * 100,  # Tamaño según prevalencia
    group = disease_prev[match(nodes, disease_prev$CCS), ratio_strata],  # Agrupación opcional
    title = paste0("<b>", disease_prev[match(nodes, disease_prev$CCS), CCS], "</b><br>",
                   "<b>N: </b>",formatC(disease_prev[match(nodes, disease_prev$CCS), N], format = "d", big.mark = ","), 
                   " (", round(disease_prev[match(nodes, disease_prev$CCS), prev] * 100, 2), "%)", "<br>",
                   "<b>AMG risk strata (&lt;P80): </b>", round(disease_prev[match(nodes, disease_prev$CCS), ratio_strata] * 100, 2), "%")
  )
  # nodes$group = cut(nodes$group, breaks = c(-Inf, 0.33, 0.66, Inf),  # Mismos puntos de corte que antes
  #                   labels = c("Post", "Mixed", "Pre"),
  #                   include.lowest = TRUE
  # )
  color_gradient <- colorRampPalette(c("#E63946","#F4A261", "#2A9D8F"))  # Verde -> Amarillo -> Rojo
  nodes[, color := color_gradient(100)[as.numeric(cut(nodes$group, breaks = 100))]]  # Gradiente basado en ratio
  
  # Añadir descripciones basadas en strata_relation
  d_network_pob[, strata_description := fifelse(
    strata_relation == "Pre", "Pre-Transition",
    fifelse(strata_relation == "Post", "Post-Transition",
            "Mixed-Transition")
  )]
  
  strata_colors <- c("Pre" = "#2A9D8F",  # Verde
                     "Post" = "#E63946", # Rojo
                     "Mixed" = "#F4A261") # Amarillo
  
  # Preparar aristas
  edges <- data.table(
    from = d_network_pob$CCS_from,
    to = d_network_pob$CCS_to,
    value = d_network_pob$N,  # Grosor según peso
    #value = d_network_pob$Risk_Ratio_Adjusted,
    arrows = "to",  # Flechas dirigidas
    color = strata_colors[d_network_pob$strata_relation],  # Color según relación
    title = paste0("<b>From :</b> ",d_network_pob$CCS_from, "<br>", 
                   "<b>To :</b> ",  d_network_pob$CCS_to, "<br>",
                   "<b>Association type:</b> ", d_network_pob$strata_description, "<br>",
                   "<b>Number of connections:</b> ", formatC(d_network_pob$N, format = "d", big.mark = ","), "<br>",
                   "<b>Risk Ratio:</b> ", round(d_network_pob$Risk_Ratio, 2))
  )
  
  # Generar la network
  network <- visNetwork(nodes, edges, width = "100%", height = "800px") %>%
    visNodes(
      shape = "dot",
      shadow = TRUE,
      color = list(
        background = nodes$color,  # Usar colores continuos
        border = "#000000"
      )
    ) %>%
    visEdges(
      smooth = TRUE,
      arrows = list(to = list(enabled = TRUE)),    
      scaling = list(
        min = 1,  # Tamaño mínimo del grosor
        max = 10  # Tamaño máximo del grosor
      )
    ) %>%
    visOptions(
      highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE),
      nodesIdSelection = list(enabled = TRUE, main = "Selected by disease") 
    ) %>%
    visLayout(randomSeed = 27) %>%
    visInteraction(zoomView = TRUE) %>%
    visLegend(
      addNodes = list(
        list(label = "Pre Dominance (High)", shape = "dot", color = "#2A9D8F"),
        list(label = "Mixed", shape = "dot", color = "#F4A261"),
        list(label = "Post Dominance (High)", shape = "dot", color = "#E63946")
      ),
      useGroups = FALSE
    )
  
  # Añadir leyenda manualmente
  legend_nodes <- data.frame(
    label = c("Pre Transition", "Post Transition", "Mixed Transition"),
    color = c("#2A9D8F", "#E63946", "#F4A261"),
    shape = "box"
  )
  
  network <- network %>%
    
    visGroups(groupname = "Pre Transition", color = "#2A9D8F") %>%
    visGroups(groupname = "Post Transition", color = "#E63946") %>%
    visGroups(groupname = "Mixed Transition", color = "#F4A261") %>%
    visLegend(addNodes = legend_nodes, useGroups = FALSE)
  
  # Mostrar la red
  network
  network_name = paste0(auxDir, "directed_disease_network.html")
  # visSave(network,background = "white", file = network_name)
  
  # Guardado con el responsive design
  visSave(
    network,
    background = "white",
    file = network_name,
    selfcontained = TRUE # Ensures all dependencies are included
  )
  
  
  
  
# 6. Directed networks by most prevalent disease subpopulations #####
  
  ps 
  # Per fer-ho male vs female, canviar type
  
  i=1
  type = 'FEMALE'  # 'ALL', 'MALE', 'FEMALE'
  
  while(i<=nrow(disease_prev_top20)){
    # Seleccionar la enfermedad central
    disease_aux = as.character(disease_prev_top20[i, "CodiCCS"])
    disease_aux_desc = as.character(disease_prev_top20[i, "CCS"])
    print(disease_prev_top20[i,])
    
    # Crear directorio de salida
    # auxDir = paste0(outputsDir, "Disease network/",disease_aux_desc, "/")
    auxDir = paste0("C:/Projectes/008_MMP_Evolution/app/",disease_aux_desc, "/")
    if (!dir.exists(auxDir)) {
      dir.create(auxDir)
    }
    
    # Subset de población (tota, homes o dones)
    if(type == 'ALL'){
      patients_aux <- ps[CCS == disease_aux, unique(CIP)]
    } else if(type == 'MALE'){
      patients_aux <- ps[CCS == disease_aux & Sexe == 1, unique(CIP)]
    }else if(type == 'FEMALE'){
      patients_aux <- ps[CCS == disease_aux & Sexe == 2, unique(CIP)]
    }else{
      stop("El type utilitzat no és correcte. type 'ALL', 'MALE', 'FEMALE'")
    }
    
    
    ps_aux <- ps[J(patients_aux)]  # Filtrar por pacientes relacionados
    
    n_pacients = length(patients_aux)
    
    # Prevalencia de enfermedades
    disease_prev = ps_aux[, .N, by = CCS]
    disease_prev$prev = disease_prev$N / n_pacients
    
    # Añadimos descriptores
    disease_prev <- disease_prev %>%
      arrange(-N) %>%
      inner_join(.,labels, by=c('CCS' = 'CCS')) %>%
      mutate(id = 1:n())
    
    # Calcular prevalencia Pre y Post por enfermedad
    prevalence_by_strata <- ps_aux[, .(Pre = sum(Strata == "Pre"),
                                       Post = sum(Strata == "Post")),
                                   by = CCS]
    
    # Calcular el ratio Pre/Post
    prevalence_by_strata[, ratio_strata := Pre / (Pre + Post)]
    
    # Unir datasets
    disease_prev <- disease_prev %>%
      inner_join(.,prevalence_by_strata, by='CCS') %>%
      rename(CodiCCS = CCS, CCS = label) 
    
    # Generar pares dirigidos basados en Disease_number
    pairs_data <- ps_aux[ps_aux, on = .(CIP), allow.cartesian = TRUE]
    pairs_data <- pairs_data[Disease_number < i.Disease_number, 
                             .(ccs_from = CCS, ccs_to = i.CCS, strata_from = Strata, strata_to = i.Strata)]
    
    # Calcular relación entre estratos
    pairs_data[, strata_relation := fifelse(strata_from == strata_to, strata_from, "Mixed")]
    
    # Contar ocurrencias por relación dirigida
    pair_counts <- pairs_data[, .N, by = .(ccs_from, ccs_to, strata_relation)]
    
    # Transformar datos en formato dirigido
    disease_pairs <- dcast(pair_counts, ccs_from + ccs_to ~ strata_relation, value.var = "N", fill = 0)
    
    # Renombrar columnas
    setnames(disease_pairs, c("Pre", "Post", "Mixed"), c("N_pre", "N_post", "N_mixed"))
    
    # Calcular total de conexiones dirigidas
    disease_pairs[, N := N_pre + N_post + N_mixed]
    
    # Tipo de relación predominante
    disease_pairs[, strata_relation := fifelse(N_pre >= N_post & N_pre >= N_mixed, "Pre",
                                               fifelse(N_post >= N_pre & N_post >= N_mixed, "Post", "Mixed"))]
    
    # Asociar prevalencias para ccs_from y ccs_to
    disease_pairs <- merge(disease_pairs, 
                           disease_prev[, .(CodiCCS, N, prev, CCS)], 
                           by.x = "ccs_to", by.y = "CodiCCS", all.x = TRUE)
    setnames(disease_pairs, "N.x", "N")
    setnames(disease_pairs, "N.y", "patients_to")
    setnames(disease_pairs, "CCS", "CCS_to")
    setnames(disease_pairs, "prev", "prev_to")
    
    
    disease_pairs <- merge(disease_pairs, 
                           disease_prev[, .(CodiCCS, N, prev, CCS)], 
                           by.x = "ccs_from", by.y = "CodiCCS", all.x = TRUE)
    setnames(disease_pairs, "N.x", "N")
    setnames(disease_pairs, "CCS", "CCS_from")
    setnames(disease_pairs, "N.y", "patients_from")
    setnames(disease_pairs, "prev", "prev_from")
    
    
    # Calcular Risk Ratios
    disease_pairs[, patients_from := as.numeric(patients_from)]
    disease_pairs[, patients_to := as.numeric(patients_to)]
    disease_pairs[, N := as.numeric(N)]
    disease_pairs[, Risk_Ratio := (N * n_pacients) / (patients_from * patients_to)]
    
    # ADjusted risk ratio
    disease_pairs[, Risk_Ratio_Adjusted := N / sqrt(patients_from * patients_to)]
    # Scale to range [0, 1]
    disease_pairs$Risk_Ratio_Adjusted <- (disease_pairs$Risk_Ratio_Adjusted - min(disease_pairs$Risk_Ratio_Adjusted)) / (max(disease_pairs$Risk_Ratio_Adjusted) - min(disease_pairs$Risk_Ratio_Adjusted))
    
  # CALCULAR ELS RISK RATIOS AMB LA PREVALENCA POBLACIONAL?
    
    # Filtrar pares por Bonferroni y N
    alpha <- 0.05
    total_pairs <- nrow(disease_pairs)
    bonferroni_threshold <- round(alpha / total_pairs, 8)
    
    #disease_pairs[, p_value := 2 * pnorm(-abs(N))]
    disease_pairs[, p_bonferroni := p.adjust(2 * pnorm(-abs(N)), method = "bonferroni")]
    disease_pairs <- disease_pairs[p_bonferroni < bonferroni_threshold & N >= 50]
    
    
    # Seleccionar relaciones relevantes por prevalencia
    disease_connections <- disease_pairs %>%
      filter(ccs_from == disease_aux | ccs_to == disease_aux) %>%
      arrange(-N) %>%
      slice_head(n = 10)
    
    # De las secundarias, las escogemos por risk ratio
    secondary_diseases <- unique(c(disease_connections$ccs_from, disease_connections$ccs_to))
    secondary_diseases <- secondary_diseases[secondary_diseases != disease_aux]
    
    secondary_connections <- disease_pairs %>%
      filter((ccs_from %in% secondary_diseases | ccs_to %in% secondary_diseases) &
               !(ccs_from == disease_aux | ccs_to == disease_aux)) %>%
      arrange(-Risk_Ratio) %>%
      slice_head(n = 20)
    
    d_network_pob <- bind_rows(disease_connections, secondary_connections) %>%
      distinct(ccs_from, ccs_to, .keep_all = TRUE)
    
    d_network_pob$p_bonferroni <- ifelse(d_network_pob$p_bonferroni==0, paste0('<', bonferroni_threshold), as.character(d_network_pob$p_bonferroni))
    # write.csv2(
    #   x = d_network_pob,
    #   file = paste0(auxDir, "/directed_disease_network.csv"),
    #   row.names = F
    # )
    
    nodes <- unique(c(d_network_pob$CCS_from, d_network_pob$CCS_to))
    
    # Preparar nodos
    nodes <- data.table(
      id = nodes, 
      label = disease_prev[match(nodes, disease_prev$CCS), CCS],  # Usar descriptores
      value = disease_prev[match(nodes, disease_prev$CCS), prev] * 100,  # Tamaño según prevalencia
      group = disease_prev[match(nodes, disease_prev$CCS), ratio_strata],  # Agrupación opcional
      title = paste0("<b>", disease_prev[match(nodes, disease_prev$CCS), CCS], "</b><br>",
                     "<b>N: </b>",formatC(disease_prev[match(nodes, disease_prev$CCS), N], format = "d", big.mark = ","), 
                     " (", round(disease_prev[match(nodes, disease_prev$CCS), prev] * 100, 2), "%)", "<br>",
                     "<b>AMG risk strata (&lt;P80): </b>", round(disease_prev[match(nodes, disease_prev$CCS), ratio_strata] * 100, 2), "%")
    )
    # nodes$group = cut(nodes$group, breaks = c(-Inf, 0.33, 0.66, Inf),  # Mismos puntos de corte que antes
    #                   labels = c("Post", "Mixed", "Pre"),
    #                   include.lowest = TRUE
    # )
    color_gradient <- colorRampPalette(c("#E63946","#F4A261", "#2A9D8F"))  # Verde -> Amarillo -> Rojo
    nodes[, color := color_gradient(100)[as.numeric(cut(nodes$group, breaks = 100))]]  # Gradiente basado en ratio
    
    # Añadir descripciones basadas en strata_relation
    d_network_pob[, strata_description := fifelse(
      strata_relation == "Pre", "Pre-Transition",
      fifelse(strata_relation == "Post", "Post-Transition",
              "Mixed-Transition")
    )]
    
    strata_colors <- c("Pre" = "#2A9D8F",  # Verde
                       "Post" = "#E63946", # Rojo
                       "Mixed" = "#F4A261") # Amarillo
    
    # Preparar aristas
    edges <- data.table(
      from = d_network_pob$CCS_from,
      to = d_network_pob$CCS_to,
      value = d_network_pob$N,  # Grosor según peso
      #value = d_network_pob$Risk_Ratio_Adjusted,
      arrows = "to",  # Flechas dirigidas
      color = strata_colors[d_network_pob$strata_relation],  # Color según relación
      title = paste0("<b>From :</b> ",d_network_pob$CCS_from, "<br>", 
                     "<b>To :</b> ",  d_network_pob$CCS_to, "<br>",
                     "<b>Association type:</b> ", d_network_pob$strata_description, "<br>",
                     "<b>Number of connections:</b> ", formatC(d_network_pob$N, format = "d", big.mark = ","), "<br>",
                     "<b>Risk Ratio:</b> ", round(d_network_pob$Risk_Ratio, 2))
    )
    
    # Generar la network
    network <- visNetwork(nodes, edges, width = "100%", height = "800px") %>%
      visNodes(
        shape = "dot",
        shadow = TRUE,
        color = list(
          background = nodes$color,  # Usar colores continuos
          border = "#000000"
        )
      ) %>%
      visEdges(
        smooth = TRUE,
        arrows = list(to = list(enabled = TRUE)),    
        scaling = list(
          min = 1,  # Tamaño mínimo del grosor
          max = 10  # Tamaño máximo del grosor
        )
      ) %>%
      visOptions(
        highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE),
        nodesIdSelection = list(enabled = TRUE, main = "Selected by disease") 
      ) %>%
      visLayout(randomSeed = 27) %>%
      visInteraction(zoomView = TRUE) %>%
      visLegend(
        addNodes = list(
          list(label = "Pre Dominance (High)", shape = "dot", color = "#2A9D8F"),
          list(label = "Mixed", shape = "dot", color = "#F4A261"),
          list(label = "Post Dominance (High)", shape = "dot", color = "#E63946")
        ),
        useGroups = FALSE
      )
    
    # Añadir leyenda manualmente
    legend_nodes <- data.frame(
      label = c("Pre Transition", "Post Transition", "Mixed Transition"),
      color = c("#2A9D8F", "#E63946", "#F4A261"),
      shape = "box"
    )
    
    network <- network %>%
      visGroups(groupname = "Pre Transition", color = "#2A9D8F") %>%
      visGroups(groupname = "Post Transition", color = "#E63946") %>%
      visGroups(groupname = "Mixed Transition", color = "#F4A261") %>%
      visLegend(addNodes = legend_nodes, useGroups = FALSE)
    
    # Mostrar la red
    network
    network_name = paste0(auxDir, "directed_disease_network_", disease_aux, "_", type, ".html")
    # visSave(network,background = "white", file = network_name)
    
    # Guardado con el responsive design
    visSave(
      network,
      background = "white",
      file = network_name,
      selfcontained = TRUE # Ensures all dependencies are included
    )
    
    i <- i+1
  }


# Visualizacion con ggraph #######

  library(ggraph)  

# # Crear grafo dirigido
# edges <- d_network_pob[, .(CCS_from, CCS_to, weight = 100 * N / n_pacients, strata_relation)]
# nodes <- unique(c(d_network_pob$CCS_from, d_network_pob$CCS_to))
# 
# # Crear grafo dirigido con todos los atributos
# g <- graph_from_data_frame(d = edges, vertices = data.table(name = nodes), directed = TRUE)
# 
# # Añadir atributos de nodos
# node_info <- as.data.frame(disease_prev[match(V(g)$name, disease_prev$CCS), .(CCS, prev)])
# V(g)$prevalence <- 100 * node_info$prev
# V(g)$label <- node_info$CCS
# 
# # Añadir colores de nodos (gradiente según prevalencia)
# color_gradient <- scales::gradient_n_pal(c("#E63946", "#F4A261", "#2A9D8F"))  # Rojo -> Amarillo -> Verde
# V(g)$color <- color_gradient(V(g)$prevalence / max(V(g)$prevalence, na.rm = TRUE))  # Escalar prevalencia
# 
# # Añadir atributos de aristas
# E(g)$weight <- edges$weight
# E(g)$strata_relation <- edges$strata_relation
# 
# # Asignar colores a las aristas según el tipo de relación
# strata_colors <- c("Pre" = "#2A9D8F",  # Verde
#                    "Post" = "#E63946", # Rojo
#                    "Mixed" = "#F4A261") # Amarillo
# E(g)$color <- strata_colors[E(g)$strata_relation]
# 
# Visualización con ggraph
# ggraph(g, layout = "fr") +
#   geom_edge_link(aes(color = strata_relation, edge_width = weight),
#                  arrow = arrow(length = unit(0.3, "cm"), type = "closed"),
#                  alpha = 0.8, show.legend = TRUE) +
#   geom_node_point(aes(size = prevalence, fill = color),
#                   shape = 21, color = "black", stroke = 0.5) +
#   geom_node_text(aes(label = label), repel = TRUE, size = 3, max.overlaps = 50) +
#   scale_edge_color_manual(values = strata_colors, name = "Risk strata transition") +
#   scale_fill_identity() +  # Usar colores definidos para nodos
#   scale_size_continuous(range = c(4, 15), name = "Prevalence (%)") +  # Ajustar tamaño de nodos
#   guides(
#     size = guide_legend(title = "Prevalence (%)", nrow = 3, byrow = TRUE),
#     color = guide_legend(title = "Risk strata transition")
#   ) +
#   theme_void() +
#   labs(title = paste("Directed Disease Network:", disease_aux_desc)) +
#   theme(
#     legend.position = "right",
#     legend.box = "vertical",
#     legend.text = element_text(size = 10),
#     legend.title = element_text(size = 12),
#     panel.background = element_rect(fill = "white", color = NA),
#     plot.background = element_rect(fill = "white", color = NA)
#   )
# 
# # Guardar gráfico
# ggsave(filename = paste0(auxDir, "/directed_disease_network_", disease_aux_desc, ".png"),
#        dpi = 300, width = 10, height = 8)

