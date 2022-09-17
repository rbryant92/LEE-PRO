########################################################################
## FILENAME    : server.R
## DEVELOPER   : Richard Bryant (bryanri3)
## DATE        : 17-Jul-2020
## R           : R-3.4.3
## PLATFORM    : GNU/Linux
## PROTOCOL    : NA
## DESCRIPTION : App code for LEE-PRO (many studies)
########################################################################

function(input,output,session){
  
  ### Get variable labels from SAS datasets
  get_labels <- function (df, all=NULL) {
    x <- character(length(names(df)))
    names(x) <- names (df)
    
    for (var in (names(df))) {
      if (!is.null (attributes(df[[var]])$label)) {
        x[var] <-  attributes(df[[var]])$label
      } else {
        x[var] <- var
      }
    }
    
    if (!is.null (all))  { 
      x <- c('_ALL_' = all, x)
    }
    
    if (!is.null (x))  { 
      y<-as.data.frame(x)
      colnames(y)[1]<-c("label")
      y$variable=rownames(y)
      
      y<-as.data.table(y)
      y=y[,.(variable,label)]
    }
    
    return (y)
  }
  
  percent <- function(x, digits = 2, format = "f", ...) {
    paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
  }
  
  ### Custom function for risk table
  ggkm <- function(sfit,
                   table = TRUE,
                   xlabs = "Time-to-event",
                   ylabs = "Survival (%)",
                   xlims = c(0,max(sfit$time)),
                   ylims = c(0,1),
                   ystratalabs = names(sfit$strata),
                   ystrataname = "Strata",
                   timeby = signif(max(sfit$time)/7, 1),
                   main = "",
                   pval = FALSE,
                   marks = TRUE,
                   shape = 3,
                   legend = TRUE,
                   ci = FALSE,
                   linecols="Set1",
                   dashed= FALSE,
                   ...) {
    
    
    #################################
    # sorting the use of subsetting #
    #################################
    
    times <- seq(0, max(sfit$time), by = timeby)
    
    if(length(levels(summary(sfit)$strata)) == 0) {
      subs1 <- 1
      subs2 <- 1:length(summary(sfit,censored=T)$time)
      subs3 <- 1:length(summary(sfit,times = times,extend = TRUE)$time)
    } else {
      subs1 <- 1:length(levels(summary(sfit)$strata))
      subs2 <- 1:length(summary(sfit,censored=T)$strata)
      subs3 <- 1:length(summary(sfit,times = times,extend = TRUE)$strata)
    }
    
    
    ##################################
    # data manipulation pre-plotting #
    ##################################
    
    
    
    if(length(levels(summary(sfit)$strata)) == 0) {
      if(is.null(ystratalabs)) ystratalabs <- as.character(sub("group=*","","All"))
    } else {
      if(is.null(ystratalabs)) ystratalabs <- as.character(sub("group=*","",names(sfit$strata)))
    }
    
    if(is.null(ystrataname)) ystrataname <- "Strata"
    m <- max(nchar(ystratalabs))
    times <- seq(0, max(sfit$time), by = timeby)
    
    if(length(levels(summary(sfit)$strata)) == 0) {
      Factor <- factor(rep("All",length(subs2)))
    } else {
      Factor <- factor(summary(sfit, censored = T)$strata[subs2])
    }
    
    #Data to be used in the survival plot
    df <- data.frame(
      time = sfit$time[subs2],
      n.risk = sfit$n.risk[subs2],
      n.event = sfit$n.event[subs2],
      n.censor = sfit$n.censor[subs2],
      surv = sfit$surv[subs2],
      strata = Factor,
      upper = sfit$upper[subs2],
      lower = sfit$lower[subs2]
    )
    
    #Final changes to data for survival plot
    levels(df$strata) <- ystratalabs
    zeros <- data.frame(time = 0, surv = 1,
                        strata = factor(ystratalabs, levels=levels(df$strata)),
                        upper = 1, lower = 1)
    df <- plyr::rbind.fill(zeros, df)
    d <- length(levels(df$strata))
    
    ###################################
    # specifying axis parameteres etc #
    ###################################
    
    if(dashed == TRUE){
      linetype=c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash", "1F", "F1", "4C88C488", "12345678")
    } else {
      linetype=c("solid", "solid", "solid", "solid", "solid", "solid", "solid", "solid", "solid", "solid", "solid")
    }
    
    
    p <- ggplot( df, aes(x=time, y=surv, colour=strata, linetype=strata)) +
      ggtitle(main)
    
    #Set up theme elements
    p <- p + theme_bw() +
      theme(axis.title.x = element_text(vjust = 0.7),
            plot.title = element_text(face="bold"),
            panel.grid.minor = element_blank(),
            axis.line = element_line(size =0.5, colour = "black"),
            legend.position = "top",
            legend.background = element_rect(fill = NULL),
            legend.key = element_rect(colour = NA),
            legend.title = element_text(size = 12),
            legend.text = element_text(size = 11),
            panel.border = element_blank(),
            plot.margin = unit(c(0, 1, .5,ifelse(m < 10, 1.5, 2.5)),"lines"),
            panel.grid.major = element_blank(),
            axis.text.x = element_text(size = 10),
            axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
            axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")) +
      scale_x_continuous(xlabs, breaks = times, limits = xlims) +
      scale_y_continuous(ylabs, limits = ylims)
    
    
    #Add 95% CI to plot
    if(ci == TRUE)
      p <- p +  geom_ribbon(data=df, aes(ymin = lower, ymax = upper), fill = "grey", alpha=0.25, colour=NA)
    
    #Removes the legend:
    if(legend == FALSE)
      p <- p + theme(legend.position="none")
    
    #Add lines too plot
    p <- p + geom_step(size = 0.75) +
      scale_linetype_manual(name = ystrataname, values=linetype) +
      scale_colour_brewer(name = ystrataname, palette=linecols)
    
    #Add censoring marks to the line:
    if(marks == TRUE)
      p <- p + geom_point(data = subset(df, n.censor >= 1), aes(x = time, y = surv), shape = shape, colour = "black")
    
    
    
    ## Create a blank plot for place-holding
    blank.pic <- ggplot(df, aes(time, surv)) +
      geom_blank() + theme_bw() +
      theme(axis.text.x = element_blank(),axis.text.y = element_blank(),
            axis.title.x = element_blank(),axis.title.y = element_blank(),
            axis.ticks = element_blank(),
            panel.grid.major = element_blank(),panel.border = element_blank())
    
    #####################
    # p-value placement #
    #####################a
    
    if(length(levels(summary(sfit)$strata)) == 0) pval <- FALSE
    
    if(pval == TRUE) {
      sdiff <- survdiff(eval(sfit$call$formula), data = eval(sfit$call$data))
      pvalue <- pchisq(sdiff$chisq,length(sdiff$n) - 1,lower.tail = FALSE)
      pvaltxt <- ifelse(pvalue < 0.0001,"p < 0.0001",paste("p =", signif(pvalue, 3)))
      # MOVE P-VALUE LEGEND HERE BELOW [set x and y]
      p <- p + annotate("text",x = (as.integer(max(sfit$time)/5)), y = 0.1,label = pvaltxt)
    }
    
    ###################################################
    # Create table graphic to include at-risk numbers #
    ###################################################
    
    if(length(levels(summary(sfit)$strata)) == 0) {
      newFactor <- factor(rep("All",length(subs3)))
    } else {
      newFactor <- factor(summary(sfit,times = times,extend = TRUE)$strata[subs3])
    }
    
    if(table == TRUE) {
      risk.data <- data.frame(
        strata = newFactor,
        time = summary(sfit,times = times,extend = TRUE)$time[subs3],
        n.risk = summary(sfit,times = times,extend = TRUE)$n.risk[subs3]
      )
      
      risk.data$strata <- factor(risk.data$strata, levels=rev(levels(risk.data$strata)))
      
      data.table <- ggplot(risk.data,aes(x = time, y = strata, label = format(n.risk, nsmall = 0))) +
        geom_text(size = 3.5) + theme_bw() +
        scale_y_discrete(breaks = as.character(levels(risk.data$strata)),
                         labels = rev(ystratalabs)) +
        scale_x_continuous("Numbers at risk", limits = xlims) +
        theme(axis.title.x = element_text(size = 10, vjust = 1),
              panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.border = element_blank(),axis.text.x = element_blank(),
              axis.ticks = element_blank(),
              axis.text.y = element_text(face = "bold", hjust = 1))
      #axis.text.y = element_text(size = 12, face = "bold",hjust = 1))
      
      data.table <- data.table +
        theme(legend.position = "none") + xlab(NULL) + ylab(NULL)
      
      
      # ADJUST POSITION OF TABLE FOR AT RISK
      data.table <- data.table +
        theme(plot.margin = unit(c(-1.5, 1, 0.1, ifelse(m < 10, 2.5, 3.5) - 0.15 * m), "lines"))
      
      # data.table <- data.table +
      #   theme(plot.margin = unit(c(-1.25, 1.5, 0.2, ifelse(m < 10, 2.5, 3.5) - 0.15 * m), "lines"))
    }
    
    
    #######################
    # Plotting the graphs #
    #######################
    
    if(table == TRUE){
      grid.arrange(p, blank.pic, data.table, clip = FALSE, nrow = 3,
                   ncol = 1, heights = unit(c(2, .1, .75),c("null", "null", "null")))
    } else {
      p
    }
    
  }
  
  # Read data sets
  getData <- reactive({
    
    ### Progress Indicator
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    
    progress$set(message = "Loading GPS dataset, please wait ....", value = 0)
    prg.n <- 4
    
    if(input$study == "Study 1"){
      path <- file.path(paste0("/view/", system("whoami", intern = TRUE),
                              "_view/vob/study1/csr_9/analysis_data/"))
      adae <- read_sas(paste0(path, "adae.sas7bdat"))
      adsl <- read_sas(paste0(path, "adsl.sas7bdat"))
      adslsupp <- read_sas(paste0(path, "adslsupp.sas7bdat"))
      adqs <- read_sas(paste0(path, "adqs.sas7bdat")) %>%
        dplyr::filter(PARAMTYP == "DERIVED")
      adtteqs <- read_sas(paste0(path, "adtteqs.sas7bdat")) %>%
        dplyr::filter(str_detect(PARAM, "Deterioration") == TRUE) %>%
        dplyr::select(USUBJID, PARAM, PARAMCD, AVAL)
      adrecsl <- read_sas(paste0(path, "adrecsl.sas7bdat"))
      
      datasets <- list(adae = adae, adsl = adsl, adslsupp = adslsupp, adqs = adqs, adrecsl = adrecsl,
                       adtteqs = adtteqs)
    }
    
    if(input$study == "Study 2"){
      path <- file.path(paste0("/view/", system("whoami", intern = TRUE),
                              "_view/vob/study2/csr_5/analysis_data/"))
      adae <- read_sas(paste0("/view/", system("whoami", intern = TRUE),
                              "_view/vob/study2/csr_4/analysis_data/adae.sas7bdat"))
      adsl <- read_sas(paste0(path, "adsl.sas7bdat"))
      adslsupp <- read_sas(paste0(path, "adslsupp.sas7bdat"))
      adqs <- read_sas(paste0(path, "adqs.sas7bdat"))
      adtteqs <- read_sas(paste0(path, "adtteqs.sas7bdat")) %>%
        dplyr::filter(str_detect(PARAM, "Deterioration") == TRUE) %>%
        dplyr::select(USUBJID, PARAM, PARAMCD, AVAL)
      adrecsl <- read_sas(paste0(path, "adrecsl.sas7bdat"))
      
      datasets <- list(adae = adae, adsl = adsl, adqs = adqs, adslsupp = adslsupp, adrecsl = adrecsl,
                        adtteqs = adtteqs)
    }
    
    if(input$study == "Study 3"){
      path <- file.path(paste0("/view/", system("whoami", intern = TRUE),
                              "_view/vob/study3/csr_6/analysis_data/"))
      adae <- read_sas(paste0(path, "adae.sas7bdat"))
      adsl <- read_sas(paste0(path, "adsl.sas7bdat"))
      adslsupp <- read_sas(paste0(path, "adslsupp.sas7bdat"))
      adqs <- read_sas(paste0("/view/", system("whoami", intern = TRUE),
                              "_view/vob/study3/csr_4/analysis_data/adqs.sas7bdat"))
      adtteqs <- read_sas(paste0("/view/", system("whoami", intern = TRUE),
                                      "_view/vob/study3/csr_1/analysis_data/adtteqs.sas7bdat")) %>%
        dplyr::filter(str_detect(PARAM, "Deterioration") == TRUE) %>%
        dplyr::select(USUBJID, PARAM, PARAMCD, AVAL)
      adrecsl <- read_sas(paste0(path, "adrecsl.sas7bdat"))
      
      datasets <- list(adae = adae, adsl = adsl, adqs = adqs, adslsupp = adslsupp, adrecsl = adrecsl,
                       adtteqs = adtteqs)
    }
    
    if(input$study == "Study 4"){
      path <- file.path(paste0("/view/", system("whoami", intern = TRUE),
                              "_view/vob/study4/pub_5/analysis_data/"))
      adae <- read_sas(paste0(path, "adae.sas7bdat"))
      adsl <- read_sas(paste0(path, "adsl.sas7bdat"))
      adslsupp <- read_sas(paste0(path, "adslsupp.sas7bdat"))
      adqs <- read_sas(paste0(path, "adqs.sas7bdat"))
      adtteqs <- read_sas(paste0(path, "adtteqs_gerpro_cnf.sas7bdat")) %>%
        dplyr::filter(str_detect(PARAM, "Deterioration") == TRUE) %>%
        dplyr::select(USUBJID, PARAM, PARAMCD, AVAL) 
      adrecsl <- read_sas(paste0(path, "adrecsl.sas7bdat"))
      
      datasets <- list(adae = adae, adsl = adsl, adslsupp = adslsupp, adqs = adqs, adrecsl = adrecsl,
                       adtteqs = adtteqs)
    }
    
    if(input$study == "Study 5"){
      path <- file.path(paste0("/view/", system("whoami", intern = TRUE),
                               "_view/vob/study5/csr_4/analysis_data/"))
      adae <- read_sas(paste0(path, "adrisk.sas7bdat"))
      adsl <- read_sas(paste0(path, "adsl.sas7bdat"))
      adslsupp <- NULL
      adqs <- read_sas(paste0(path, "adpro.sas7bdat"))
      adtteqs <- read_sas(paste0("/view/", system("whoami", intern = TRUE),
                                      "_view/vob/study5/pub_6/analysis_data/adttepro.sas7bdat")) %>%
        dplyr::select(USUBJID, PARAM, PARAMCD, AVAL)
      adrecsl <- read_sas(paste0(path, "adrecsl.sas7bdat"))
      
      datasets <- list(adae = adae, adsl = adsl, adslsupp = adslsupp, adqs = adqs, adrecsl = adrecsl,
                       adtteqs = adtteqs)
    }
    
    if(input$study == "Study 6"){
      path <- file.path(paste0("/view/", system("whoami", intern = TRUE),
                              "_view/vob/study6/csr_1/analysis_data/"))
      adae <- read_sas(paste0(path, "adaerisk.sas7bdat"))
      adsl <- read_sas(paste0(path, "adsl.sas7bdat"))
      adslsupp <- read_sas(paste0(path, "adslsub.sas7bdat"))
      adqs <- read_sas(paste0(path, "adqs.sas7bdat"))
      adtteqs <- read_sas(paste0(path, "adtte.sas7bdat")) %>%
        dplyr::filter(str_detect(PARAM, "deterioration") == TRUE) %>%
        dplyr::select(USUBJID, PARAM, PARAMCD, AVAL)
      adrecsl <- read_sas(paste0(path, "adrecist.sas7bdat"))
      
      datasets <- list(adae = adae, adsl = adsl, adslsupp = adslsupp, adqs = adqs, adrecsl = adrecsl,
                       adtteqs = adtteqs)
    }
    
    return(datasets)
  })
  
  
  
  manipulateData <- reactive({
    adae <- data.table(getData()$adae)
    adsl <- data.table(getData()$adsl)
    adslsupp <- data.table(getData()$adslsupp)
    adqs <- data.table(getData()$adqs)
    adrecsl <- data.table(getData()$adrecsl)
    adtteqs <- data.table(getData()$adtteqs)
    
    # Endpoint and population variables from the ADSLSUPP dataset
    #adslVariables <- names(adslsupp)
    
    ### Create proData dataset for PRO feature ############################################
    
    if(!input$study %in% c("Study 5")){    
      proData <- adqs %>%
        dplyr::filter(PARCAT1 %in% c("QLQ-BR23",  "WPAI-GH",  "QLQ-C30", "BPI-SF", "EQ-5D-5L")) %>%
        dplyr::select(USUBJID, ADY, PARCAT1, PARAM, PARAMCD, AVAL, BASE, CHG, PCHG) %>%
        dplyr::filter(ADY > -10)
    }
    if(input$study %in% c("Study 5")){    
      proData <- adqs %>%
        dplyr::filter(PARAMCD == "OVERALL") %>%
        dplyr::select(USUBJID, ADY, PARAM, PARAMCD, AVAL, BASE, CHG) %>%
        mutate(PCHG = (CHG/BASE)*100) %>%
        dplyr::filter(ADY >- -10)
    }
    
    # Add the adsl variables
    if(!input$study %in% c("Study 5")){
      proData <- proData %>%
        left_join(adslsupp)
      proData <- data.table(proData)
    }
    
    if(input$study %in% c("Study 5")){
      proData <- proData %>%
        left_join(adsl)
      proData <- data.table(proData)
    }
    
    ### End creation of proData dataset ###################################################
    
    ### Create aeData dataset for Safety feature ##########################################
    
    if(input$study %in% c("Study 1", "Study 2", "Study 3")){
      aeData <- adae %>%
        dplyr::select(STUDYID, USUBJID, AEBODSYS, AEDECOD, ASTDY, AETOXGRN, CQ01NAM, CQ02NAM, CQ03NAM, CQ04NAM, CQ05NAM, 
                      CQ06NAM, CQ07NAM, CQ08NAM, CQ09NAM, CQ10NAM, CQ11NAM, CQ12NAM, CQ13NAM) %>%
        mutate(AESIFL = ifelse(CQ01NAM != "" | CQ02NAM != "" | CQ03NAM != "" | CQ04NAM != "" | CQ05NAM != "" |
                                 CQ06NAM != "" | CQ07NAM != "" | CQ08NAM != "" | CQ09NAM != "" | CQ10NAM != "" |
                                 CQ11NAM != "" | CQ12NAM != "" | CQ13NAM != "", "Y", "N")) %>%
        dplyr::select(-CQ01NAM, -CQ02NAM, -CQ03NAM, -CQ04NAM, -CQ05NAM, -CQ06NAM, -CQ07NAM, -CQ08NAM, -CQ09NAM,
                      -CQ10NAM, -CQ11NAM, -CQ12NAM, -CQ13NAM) %>%
        dplyr::filter(ASTDY >= 0)
    }
    
    if(input$study == "Study 4"){
      aeData <- adae  %>%
        dplyr::select(STUDYID, USUBJID, AEBODSYS, AEDECOD, AESTDY, AETOXGRN, CQ01NAM, CQ02NAM, CQ03NAM, CQ04NAM, CQ05NAM, 
                      CQ06NAM, CQ07NAM, CQ08NAM, CQ09NAM, CQ10NAM, CQ11NAM, CQ12NAM, CQ13NAM) %>%
        mutate(AESIFL = ifelse(CQ01NAM != "" | CQ02NAM != "" | CQ03NAM != "" | CQ04NAM != "" | CQ05NAM != "" |
                                 CQ06NAM != "" | CQ07NAM != "" | CQ08NAM != "" | CQ09NAM != "" | CQ10NAM != "" |
                                 CQ11NAM != "" | CQ12NAM != "" | CQ13NAM != "", "Y", "N")) %>%
        dplyr::select(-CQ01NAM, -CQ02NAM, -CQ03NAM, -CQ04NAM, -CQ05NAM, -CQ06NAM, -CQ07NAM, -CQ08NAM, -CQ09NAM,
                      -CQ10NAM, -CQ11NAM, -CQ12NAM, -CQ13NAM) %>%
        dplyr::filter(AESTDY >= 0) %>%
        dplyr::rename("ASTDY" = "AESTDY")
    }
    
    if(input$study == "Study 5"){
      aeData <- adae %>%
        dplyr::select(STUDYID, USUBJID, AEBODSYS, AEDECOD, AESTDY, ATOXGRN, AESIFL) %>%
        dplyr::filter(AESTDY >= 0) %>%
        dplyr::rename("ASTDY" = "AESTDY") %>%
        dplyr::rename("AETOXGRN" = "ATOXGRN")
    }
    
    if(input$study == "Study 6"){
      aeData <- adae %>%
        dplyr::select(STUDYID, USUBJID, AEBODSYS, AEDECOD, ASTDY, AETOXGRN, RISKNAME) %>%
        mutate(AESIFL = ifelse(RISKNAME %in% c("Infections", "QT prolongation", "Renal toxicity", "Hepatotoxicity",
                                               "Reproductive toxicity and genotoxicity (PSUR)"), 1, 0)) %>%
        dplyr::filter(ASTDY >= 0)
    }
    
    ### End creation of aeData dataset  ###################################################
    
    ### Create tteData dataset for efficacy feature #######################################
    if(!input$study %in% c("Study 5")){
      tteData <- adrecsl[,c("STUDYID", "USUBJID", "PARAM", "PARAMCD", "AVAL", "EVENT")] %>%
        dplyr::filter(!is.na(EVENT)) %>%
        left_join(adslsupp)
    }
    if(input$study %in% c("Study 5")){
      tteData <- adrecsl[,c("STUDYID", "USUBJID", "PARAM", "PARAMCD", "AVAL", "EVENT")] %>%
        dplyr::filter(!is.na(EVENT)) %>%
        left_join(adsl)
    }
      
    survData <- tteData %>%
      dplyr::filter(PARAMCD %in% c("PFS", "OS")) %>%
      mutate(PFS_3Month = ifelse(PARAMCD == "PFS" & !(EVENT == 1 & AVAL <= 365.25/4), 1, 0)) %>%
      mutate(OS_3Month = ifelse(PARAMCD == "OS" & !(EVENT == 1 & AVAL <= 365.25/4), 1, 0)) %>%
      mutate(PFS_6Month = ifelse(PARAMCD == "PFS" & !(EVENT == 1 & AVAL <= 365.25/2), 1, 0)) %>%
      mutate(OS_6Month = ifelse(PARAMCD == "OS" & !(EVENT == 1 & AVAL <= 365.25/2), 1, 0)) %>%
      mutate(PFS_9Month = ifelse(PARAMCD == "PFS" & !(EVENT == 1 & AVAL <= (365.25 * (3/4))), 1, 0)) %>%
      mutate(OS_9Month = ifelse(PARAMCD == "OS" & !(EVENT == 1 & AVAL <= (365.25 * (3/4))), 1, 0)) %>%
      mutate(PFS_12Month = ifelse(PARAMCD == "PFS" & !(EVENT == 1 & AVAL <= 365.25), 1, 0)) %>%
      mutate(OS_12Month = ifelse(PARAMCD == "OS" & !(EVENT == 1 & AVAL <= 365.25), 1, 0)) %>%
      mutate(PFS_18Month = ifelse(PARAMCD == "PFS" & !(EVENT == 1 & AVAL <= (365.25 * (3/2))), 1, 0)) %>%
      mutate(OS_18Month = ifelse(PARAMCD == "OS" & !(EVENT == 1 & AVAL <= (365.25 * (3/2))), 1, 0)) %>%
      mutate(PFS_24Month = ifelse(PARAMCD == "PFS" & !(EVENT == 1 & AVAL <= 365.25*2), 1, 0)) %>%
      mutate(OS_24Month = ifelse(PARAMCD == "OS" & !(EVENT == 1 & AVAL <= 365.25*2), 1, 0)) %>%
      mutate(PFS_30Month = ifelse(PARAMCD == "PFS" & !(EVENT == 1 & AVAL <= (365.25 * (5/2))), 1, 0)) %>%
      mutate(OS_30Month = ifelse(PARAMCD == "OS" & !(EVENT == 1 & AVAL <= (365.25 * (5/2))), 1, 0)) %>%
      mutate(PFS_36Month = ifelse(PARAMCD == "PFS" & !(EVENT == 1 & AVAL <= 365.25*3), 1, 0)) %>%
      mutate(OS_36Month = ifelse(PARAMCD == "OS" & !(EVENT == 1 & AVAL <= 365.25*3), 1, 0)) %>%
      group_by(USUBJID) %>%
      dplyr::summarize(PFS_3Month = max(PFS_3Month, na.rm=T), OS_3Month = max(OS_3Month, na.rm=T),
                       PFS_6Month = max(PFS_6Month, na.rm=T), OS_6Month = max(OS_6Month, na.rm=T),
                       PFS_9Month = max(PFS_9Month, na.rm=T), OS_9Month = max(OS_9Month, na.rm=T),
                       PFS_12Month = max(PFS_12Month, na.rm=T), OS_12Month = max(OS_12Month, na.rm=T),
                       PFS_18Month = max(PFS_18Month, na.rm=T), OS_18Month = max(OS_18Month, na.rm=T),
                       PFS_24Month = max(PFS_24Month, na.rm=T), OS_24Month = max(OS_24Month, na.rm=T),
                       PFS_30Month = max(PFS_30Month, na.rm=T), OS_30Month = max(OS_30Month, na.rm=T),
                       PFS_36Month = max(PFS_36Month, na.rm=T), OS_36Month = max(OS_36Month, na.rm=T)) %>%
      mutate_all(.funs = as.character)
    
    aesiData <- aeData %>%
      mutate(AESI_3Month = ifelse(AESIFL == "Y" & ASTDY <= 365.24/4, 1, 0)) %>%
      mutate(AESI_6Month = ifelse(AESIFL == "Y" & ASTDY <= 365.25/2, 1, 0)) %>%
      mutate(AESI_9Month = ifelse(AESIFL == "Y" & ASTDY <= (365.25 * (3/4)), 1, 0)) %>%
      mutate(AESI_12Month = ifelse(AESIFL == "Y" & ASTDY <= 365.25, 1, 0)) %>%
      mutate(AESI_18Month = ifelse(AESIFL == "Y" & ASTDY <= (365.25 * (3/2)), 1, 0)) %>%
      mutate(AESI_24Month = ifelse(AESIFL == "Y" & ASTDY <= 365.25*2, 1, 0)) %>%
      mutate(AESI_30Month = ifelse(AESIFL == "Y" & ASTDY <= (365.25 * (5/2)), 1, 0)) %>%
      mutate(AESI_36Month = ifelse(AESIFL == "Y" & ASTDY <= 365.25*3, 1, 0)) %>%
      group_by(USUBJID) %>%
      dplyr::summarize(AESI_3Month = max(AESI_3Month, na.rm=T), AESI_6Month = max(AESI_6Month, na.rm=T),
                       AESI_9Month = max(AESI_9Month, na.rm=T), AESI_12Month = max(AESI_12Month, na.rm=T),
                       AESI_18Month = max(AESI_18Month, na.rm=T), AESI_24Month = max(AESI_24Month, na.rm=T),
                       AESI_30Month = max(AESI_30Month, na.rm=T), AESI_36Month = max(AESI_36Month, na.rm=T)) %>%
      mutate_all(.funs = as.character)
    
    socData <- aeData %>%
      #dplyr::filter(AEBODSYS %in% socOption) %>%
      dplyr::select(USUBJID, AEBODSYS, ASTDY) %>%
      dplyr::rename("Category" = "AEBODSYS", "Time" = "ASTDY")
    
    tteData <- tteData %>%
      left_join(survData) %>%
      left_join(aesiData)
    tteData <- data.table(tteData)

    ### End creation of tteData dataset  ##################################################
    
    ### Begin creation of comparisonData dataset ##########################################
    comparisonData <- adrecsl %>%
      dplyr::filter(PARAM %in% c("Overall Survival", "Progression free Survival")) %>%
      mutate(Category = ifelse(EVENT == 1, paste0(PARAMCD, " (Event)"),
                               paste0(PARAMCD, " (Censor)"))) %>%
      dplyr::select(USUBJID, Category, AVAL) %>%
      dplyr::rename("Time" = "AVAL") %>%
      bind_rows(socData) %>%
      inner_join(adtteqs) %>%
      dplyr::rename("DeteriorationType" = "PARAM", "DeteriorationTime" = "AVAL")
    ### End creation of comparisonData dataset ############################################
    
    ### Begin creation of mapData dataset  ################################################
    comparisonData <- adrecsl %>%
      dplyr::filter(PARAM %in% c("Overall Survival", "Progression free Survival")) %>%
      mutate(Category = ifelse(EVENT == 1, paste0(PARAMCD, " (Event)"),
                               paste0(PARAMCD, " (Censor)"))) %>%
      dplyr::select(USUBJID, Category, AVAL) %>%
      dplyr::rename("Time" = "AVAL") %>%
      bind_rows(socData) %>%
      inner_join(adtteqs) %>%
      dplyr::rename("DeteriorationType" = "PARAM", "DeteriorationTime" = "AVAL")
    ### End creation of mapData dataset  ##################################################
    
    return(list(tteData = tteData, proData = proData, aeData = aeData, comparisonData = comparisonData))
  })
  
  
  basevar.lst<-reactive({
    tteData <- manipulateData()$tteData
    
    names.list <- names(tteData)[! names(tteData) %in% c("PARAM", "PARAMCD", "AVAL", "EVENT")]
  })
  
  # Create variable selection option
  output$varSel <- renderUI({
    
    label.list <- basevar.lst()
    
    selectInput("varSel", label = "Subgroup",
                choices = label.list ,
                selected = 1)
  })
  
  # Create value selection option for the above variable
  output$valueSel <- renderUI({
    
    stage <- manipulateData()$tteData
    
    x <- unique(stage[,get(input$varSel)])
    x <- x[!is.na(x)]
    x <- sort(x)
    
    if (is.null(input$singleSelection)) {
      varSelSingle <- FALSE
    } else {
      varSelSingle <- input$singleSelection
    }
    
    if (varSelSingle == TRUE) {
      selectInput("valueSel", label = "Value",
                  choices = x,
                  selected = 1
      )
    } else {
      selectInput("valueSel", label = "Value",
                  choices = x,
                  selected = x[1], multiple = TRUE
      )
    }
  })
  
  # Single selection feature
  output$singleSelection <- renderUI({
    var.data <- manipulateData()$tteData
    
    var.select <- var.data[,get(input$varSel)]
    if (input$varSel !="STUDYID") {
      if (!(class(var.select) == "numeric" | class(var.select) == "integer")) {
        checkboxInput("singleSelection", label = "Single selection", value = TRUE)
      }
    }
  })
  
  output$SliderRng <- renderUI({
    var.data <- manipulateData()$tteData
    
    var.select <- var.data[, get(input$varSel)]
    
    if (input$varSel != "STUDYID") {
      if (class(var.select)=="numeric" | class(var.select)=="integer" ) {
        sliderInput("SliderRng", "Range",
                    min = min(var.select, na.rm=T), max = max(var.select,na.rm=T), 
                    value = quantile(var.select,prob=c(0, 1),na.rm=T))
      }
    }
  })
  
  output$paraSelect <- renderUI({
    ##############################################################################################
    # Add parameters for Time to events
    ##############################################################################################
    tteData <- manipulateData()$tteData
    
    ttpar.lst <- unique(tteData$PARAM)
    
    selectInput("ttvar", label = "Time to event", 
                choices = ttpar.lst , 
                selected = 1)
  })
  
  
  output$compvarm <- renderUI({
    ##############################################################################################
    # Comparator
    ##############################################################################################
    names.lst<-basevar.lst()
    selectizeInput(
      inputId ='compvarm', label ='Comparison: Limited to 0 - 2 groups',
      choices = names.lst,
      multiple = TRUE, 
      options = list(maxItems = 2, onInitialize =I('function() { 
                                                   this.setValue("ARMCD"); }'))
      )
  })
  
  
  output$compvarm_yn1 <- renderUI({
    compvar <- input$compvarm
    
    tteData <- manipulateData()$tteData
    
    if (length(compvar)>=1) {
      var.data <- data.table(tteData)
      var.select1<-var.data[,get(compvar[1])]
      
      NumVar1_cut=FALSE
      if  ((class(var.select1)=="numeric" | class(var.select1)=="integer") & length(unique(var.select1))>=10 )  {NumVar1_cut=TRUE}
      
      checkboxInput("compvarm_yn1", label = "Redefine group 1", value = NumVar1_cut)
    } 
  })
  
  output$compvarm_sw1 <- renderUI({
    checkboxInput("compvarm_sw1", label = "Switch level for HR", value = FALSE)
  })
  
  output$compvarm_sd1 <- renderUI({
    
    compvar<-input$compvarm
    tteData <- manipulateData()$tteData
    
    if (length(compvar)>=1) {
      var.data <- data.table(tteData)
      var.select<-var.data[,get(compvar[1])]
      
      NumVar1_cut=FALSE
      if  ((class(var.select)=="numeric" | class(var.select)=="integer") & length(unique(var.select))>=10 )  {NumVar1_cut=TRUE}            
      
      if (  ( input$compvarm_yn1==T) | (NumVar1_cut==TRUE) ) { 
        
        if  (NumVar1_cut==TRUE ) {
          sd.items<- seq(1,5,1)
          len.sd.items<-max(sd.items)
        } else {  
          sd.items<- sort(unique(var.data[,get(compvar[1])]))
          len.sd.items<-length(sd.items)
        }
        
        sliderInput("compvarm_sd1", label = "", min = 1, 
                    max = len.sd.items, value = len.sd.items,step=1)
      } }
  })
  
  output$compvarm_gr1 <- renderUI({
    
    compvar<-input$compvarm
    tteData <- manipulateData()$tteData
    
    if (length(compvar)>=1 ) {
      var.data <- data.table(tteData)
      var.select<-var.data[,get(compvar[1])]
      
      NumVar1_cut=FALSE
      if  ((class(var.select)=="numeric" | class(var.select)=="integer") & length(unique(var.select))>=10 )  {NumVar1_cut=TRUE}            
      
      if (( input$compvarm_yn1==T) | (NumVar1_cut==TRUE) ) {
        sd1.value<-input$compvarm_sd1
        
        if ( NumVar1_cut==TRUE ) {
          sd.items<-levels(cut(var.select,  quantile(var.select ,seq(0, 1, 1/sd1.value) ,na.rm=T) ,include.lowest = T  ))
          len.sd.items<-length(sd.items)
          
        } else {   
          sd.items<- sort(unique(var.data[,get(compvar[1])]))
          len.sd.items<-length(sd.items)
          if ( nchar(sd.items[1])==0 ) {
            sd.items[1:(len.sd.items-1)]<-sd.items[2:len.sd.items]
            sd.items[len.sd.items]<-NA
          } ### Put the NA at the end 
        }
        
        
        lapply(1:sd1.value, function(i) {
          selectInput(inputId=paste0("compvarm_gr1",i),"",
                      choices = sd.items,
                      selected = sd.items[i],
                      multiple = TRUE)
        })
      } }
  })
  
  output$compvarm_tx1 <- renderUI({
    
    compvar <- input$compvarm
    tteData <- manipulateData()$tteData
    
    if (length(compvar)>=1 ) {
      # var.data<-getData()$adslsupp
      var.data<- data.table(tteData)
      var.select<-var.data[,get(compvar[1])]
      
      NumVar1_cut=FALSE
      if  ((class(var.select)=="numeric" | class(var.select)=="integer") & length(unique(var.select))>=10 )  {NumVar1_cut=TRUE}            
      
      if (( input$compvarm_yn1==T) | (NumVar1_cut==TRUE) ) {
        
        sd1.value<-input$compvarm_sd1
        
        if (NumVar1_cut==TRUE ) {
          sd.items<-levels(cut(var.select,  quantile(var.select ,seq(0, 1, 1/sd1.value) ,na.rm=T) ,include.lowest = T  ))
          len.sd.items<-length(sd.items)
        } else { 
          sd.items<- sort(unique(var.data[,get(compvar[1])]))
          len.sd.items<-length(sd.items)
          
          if ( nchar(sd.items[1])==0 ) {
            sd.items[1:(len.sd.items-1)]<-sd.items[2:len.sd.items]
            sd.items[len.sd.items]<-NA
          } ### Put the NA at the end
        }
        
        lapply(1:sd1.value, function(i) {
          textInput(inputId=paste0("compvarm_tx1",i), label="", 
                    value=ifelse(is.na(sd.items[i]),"Missing", sd.items[i] ))
        })
        
      } }
    
  })
  
  output$compvarm_keep1 <- renderUI({
    
    compvar<-input$compvarm
    tteData <- manipulateData()$tteData
    
    if (length(compvar)>=1 ) {
      # var.data<-getData()$adslsupp
      var.data<- data.table(tteData)
      var.select <-var.data[,get(compvar[1])]
      NumVar1_cut=FALSE
      if  ((class(var.select)=="numeric" | class(var.select)=="integer") & length(unique(var.select))>=10 )  {NumVar1_cut=TRUE}            
      
      if (( input$compvarm_yn1==T) | (NumVar1_cut==TRUE) ) {
        
        sd1.value<-input$compvarm_sd1
        
        lapply(1:sd1.value, function(i) {
          checkboxInput(paste0("compvarm_keep1",i), label = "", value =FALSE)
        })
        
      } }
    
  })
  
  
  output$compvarm_yn2 <- renderUI({
    compvar<-input$compvarm
    tteData <- manipulateData()$tteData
    
    if (length(compvar)>=2) {
      # var.data<-getData()$adslsupp
      var.data<- data.table(tteData)
      var.select<-var.data[,get(compvar[2])]
      
      NumVar2_cut=FALSE
      if  ((class(var.select)=="numeric" | class(var.select)=="integer") & length(unique(var.select))>=10 )  {NumVar2_cut=TRUE}
      
      checkboxInput("compvarm_yn2", label = "Redefine group 2", value = NumVar2_cut)
    } 
  })
  
  output$compvarm_sd2 <- renderUI({
    if (!is.null(input$compvarm_yn2)) {
      
      compvar<-input$compvarm
      tteData <- manipulateData()$tteData
      
      if  (length(compvar)>=2) {  
        # var.data<-getData()$adslsupp
        var.data<- data.table(tteData)
        var.select<-var.data[,get(compvar[2])]
        NumVar2_cut=FALSE
        if  ((class(var.select)=="numeric" | class(var.select)=="integer") & length(unique(var.select))>=10 )  {NumVar2_cut=TRUE}            
        
        if (  ( input$compvarm_yn2==T) | (NumVar2_cut==TRUE) ) {  
          
          if  (NumVar2_cut==TRUE ) {
            sd.items<- seq(1,5,1)
            len.sd.items<-max(sd.items)
          } else {
            sd.items<- sort(unique(var.select))
            len.sd.items<-length(sd.items)
          }
          
          sliderInput("compvarm_sd2", label = "", min = 1,
                      max = len.sd.items, value = len.sd.items,step=1)
          
        } }
    }
  })
  
  output$compvarm_gr2 <- renderUI({
    
    if (!is.null(input$compvarm_yn2)) {
      
      compvar<-input$compvarm
      tteData <- manipulateData()$tteData
      
      if ( (length(compvar)>=2) ) {
        # var.data<-getData()$adslsupp
        var.data<- data.table(tteData)
        
        var.select<-var.data[,get(compvar[2])]
        NumVar2_cut=FALSE
        if  ((class(var.select)=="numeric" | class(var.select)=="integer") & length(unique(var.select))>=10 )  {NumVar2_cut=TRUE}
        
        if ( (input$compvarm_yn2==T)| (NumVar2_cut==TRUE)  ) {
          sd2.value<-input$compvarm_sd2
          
          if ( NumVar2_cut==TRUE ) {
            sd.items<-levels(cut(var.select,  quantile(var.select ,seq(0, 1, 1/sd2.value) ,na.rm=T) ,include.lowest = T  ))
            len.sd.items<-length(sd.items)
            
          } else {
            sd.items<- sort(unique(var.select))
            len.sd.items<-length(sd.items)
            if ( nchar(sd.items[1])==0 ) {
              sd.items[1:(len.sd.items-1)]<-sd.items[2:len.sd.items]
              sd.items[len.sd.items]<-NA
            } ### Put the NA at the end
          }
          
          lapply(1:sd2.value, function(i) {
            selectInput(inputId=paste0("compvarm_gr2",i),"",
                        choices = sd.items,
                        selected = sd.items[i],
                        multiple = TRUE)
          })
        } }
    }
  })
  
  output$compvarm_tx2 <- renderUI({
    
    if (!is.null(input$compvarm_yn2)) {
      
      compvar<-input$compvarm
      tteData <- manipulateData()$tteData
      
      if (length(compvar)>=2 ) {
        # var.data<-getData()$adslsupp
        var.data<- data.table(tteData)
        
        var.select<-var.data[,get(compvar[2])]
        NumVar2_cut=FALSE
        if  ((class(var.select)=="numeric" | class(var.select)=="integer") & length(unique(var.select))>=10 )  {NumVar2_cut=TRUE} 
        
        if (input$compvarm_yn2==T  | NumVar2_cut==TRUE) {
          
          sd2.value<-input$compvarm_sd2
          
          if ( NumVar2_cut==TRUE  ) {
            sd.items<-levels(cut(var.select,  quantile(var.select ,seq(0, 1, 1/sd2.value) ,na.rm=T) ,include.lowest = T  ))
            len.sd.items<-length(sd.items)
            
          } else {    
            
            sd.items<- sort(unique(var.select))
            len.sd.items<-length(sd.items)
            
            if ( nchar(sd.items[1])==0 ) {
              sd.items[1:(len.sd.items-1)]<-sd.items[2:len.sd.items]
              sd.items[len.sd.items]<-NA
            } ### Put the NA at the end
          }
          
          
          lapply(1:sd2.value, function(i) {
            textInput(inputId=paste0("compvarm_tx2",i), label="",
                      value=ifelse(is.na(sd.items[i]),"Missing", sd.items[i] ))
          })
          
        } }
    }
  })
  
  
  pop.data<-reactive( {
    ##############################################################################################
    # Add the by variable based on 0 to 2 comparators 
    # Add the re-define comparator groups
    ##############################################################################################
    
    req(input$study)
    tteData <- manipulateData()$tteData
    data.in1 <- data.table(tteData)
    ttdata10 <- data.in1
    data.in2<-ttdata10
    compvar<-input$compvarm
    
    ### Comparator 1 - Re-define
    NumVar1_cut=FALSE
    if (length(compvar)>=1) {
      var.select<-data.in1[,get(compvar[1])]
      if  ((class(var.select)=="numeric" | class(var.select)=="integer") & length(unique(var.select))>=10 )  {NumVar1_cut=TRUE}
    }
    
    if  (NumVar1_cut==TRUE) {
      yn1<-TRUE
    } else if (!is.null(input$compvarm_yn1)) {
      yn1<-input$compvarm_yn1
    } else {
      yn1<-FALSE 
    }
    
    gr1.items<-rep(NA,1)
    gr1_keep.items<-rep(NA,1)
    if (yn1==T) {
      gr1.items0<-data.in2[,get(compvar[1])]
      if ( NumVar1_cut==TRUE  ) {
        sd1.value<-input$compvarm_sd1
        gr1.items0=as.character(cut(gr1.items0,  quantile(gr1.items0 ,seq(0, 1, 1/sd1.value) ,na.rm=T),include.lowest = T  ) )
      }      
      
      gr1.items<-rep(NA,length(gr1.items0))
      gr1_keep.items<-gr1.items
      
      for (i in 1:input$compvarm_sd1) {
        grptxt <-eval(parse(text=paste0("input$compvarm_tx1",i)))
        grpval <-eval(parse(text=paste0("input$compvarm_gr1",i)))
        grpkeep<-eval(parse(text=paste0("input$compvarm_keep1",i)))
        
        gr1.items<-     ifelse(gr1.items0 %in% grpval & is.na(gr1.items),grptxt,gr1.items)
        gr1_keep.items<-ifelse(gr1.items0 %in% grpval & is.na(gr1_keep.items) & grpkeep==TRUE,grptxt,gr1_keep.items)
      }
      
    } 
    
    ### Comparator 2  - Re-define
    NumVar2_cut=FALSE
    if (length(compvar)>=2) {
      var.select<-data.in1[,get(compvar[2])]
      if  ((class(var.select)=="numeric" | class(var.select)=="integer") & length(unique(var.select))>=10 )  {NumVar2_cut=TRUE}
    }
    
    if  (NumVar2_cut==TRUE) {
      yn2<-TRUE
    } else if (!is.null(input$compvarm_yn2)) {
      yn2<-input$compvarm_yn2
    } else { 
      yn2<-FALSE 
    }
    
    
    gr2.items<-rep(NA,1)
    if (yn2==T) {
      gr2.items0<-data.in2[,get(compvar[2])]
      
      if ( NumVar2_cut==TRUE  ) {
        sd2.value<-input$compvarm_sd2
        gr2.items0=as.character(cut( gr2.items0[is.na(gr1_keep.items)] ,  quantile(as.numeric(gr2.items0) ,seq(0, 1, 1/sd2.value) ,na.rm=T),include.lowest = T  ) )
      }
      
      gr2.items<-rep(NA,length(gr2.items0))
      
      for (i in 1:input$compvarm_sd2) {
        grptxt<-eval(parse(text=paste0("input$compvarm_tx2",i)))
        grpval<-eval(parse(text=paste0("input$compvarm_gr2",i)))
        
        gr2.items<-ifelse(gr2.items0 %in% grpval & is.na(gr2.items),grptxt,gr2.items)
      } 
    } 
    
    ### Check the number of comparators entered (0,1,2) and whether they have been re-defined   
    if (length(compvar)==1) {
      if (yn1==T  ) {
        ttdata10[, ":="(tt.byvar= gr1.items, tt.byvar1=gr1.items) ][!is.na(gr1.items)]
      } else {
        ttdata10[, ":="(tt.byvar = get(compvar), tt.byvar1= get(compvar)  ) ]
      }
    } else if (length(compvar)==2) {
      
      if (yn1==T & yn2==F) {
        ### Comparator 2  - Undo-redefine if comparator 1 choose so
        var2_txt=ifelse( !is.na(gr1_keep.items) ,gr1_keep.items , ttdata10[,get(compvar[2])] ) 
        
        ttdata10[,":="( tt.byvar= paste(gr1.items,var2_txt,sep=";"), 
                        tt.byvar1=gr1.items, tt.byvar2=var2_txt )][!is.na(gr1.items)]
      } else if (yn1==F & yn2==T) {
        ttdata10[,":="(tt.byvar= paste(get(compvar[1]),gr2.items,sep=";"), 
                       tt.byvar1=get(compvar[1]), tt.byvar2=gr2.items  )][!is.na(gr2.items)]
      } else if (yn1==T & yn2==T) { 
        ### Comparator 2  - Undo-redefine if comparator 1 choose so
        var2_txt=ifelse( !is.na(gr1_keep.items) ,gr1_keep.items , gr2.items ) 
        
        ttdata10[,":="(tt.byvar= paste(gr1.items,var2_txt,sep=";"), 
                       tt.byvar1=gr1.items, tt.byvar2=var2_txt  )  ][ !is.na(gr1.items) & !is.na(var2_txt) ]
      } else {
        ttdata10[, ":="( tt.byvar= paste(get(compvar[1]),";",compvar[2],"=",get(compvar[2]) ,sep=""),
                         tt.byvar1=get(compvar[1]), tt.byvar2=get(compvar[2]) )]
      }
    }  else if (length(compvar)==0) {
      ttdata10[,":="(tt.byvar ="All" , tt.byvar1="All") ]
    }
    
  })
  
  
  subpop.data <-reactive( {
    ##############################################################################################
    # Subset the dataset by subgroup
    ##############################################################################################
    dataStage <- pop.data()
    
    var.select <- dataStage[, get(input$varSel)]
    
    if (class(var.select) == "numeric" | class(var.select) == "integer" ) {
      dataStage <- dplyr::filter(dataStage, var.select <= input$SliderRng[2] &
                                   var.select >= input$SliderRng[1])
    } else {
      if (is.null(input$valueSel)) {
        dataStage <- dplyr::filter(dataStage, var.select == "")
      } else {
        dataStage <- dplyr::filter(dataStage, var.select %in% input$valueSel)
      }
    }
    
    return(dataStage)
  })
  
  paratte.data<-reactive({
    #param.in <- toupper(input$ttvar)
    param.in <- input$ttvar
    data.in1 <- pop.data()
    ttdata1 <- data.in1[data.in1$PARAM == input$ttvar,]
  })
  
  subvartte.data<-reactive( {
    ttdata2 <- paratte.data()
    subpop <- subpop.data()
    ttdata10 <- ttdata2 %>%
      inner_join(subpop["USUBJID"], by = c("USUBJID" = "USUBJID"))
    ttdata10 <- unique(ttdata10)
    
  })    
  
  ttevent<-function()
  {
    ##############################################################################################
    # KM and Coxph analysis
    ##############################################################################################
    
    # Sample size of # of subjects at risk of event add
    # X-axis multiples of 3
    
    timetodata <- copy(subvartte.data())
    
    timetodata <- dplyr::filter(timetodata, !is.na(EVENT))
    timetodata <- data.table(timetodata)
    km.formu <- stats::as.formula(paste("Surv(AVAL/(365.25/12), EVENT)~","tt.byvar",sep=""))
    fit<-  survfit(km.formu,data=timetodata, conf.type='log-log')
    
    ### Get the text for cox ph model formula
    compvar<-input$compvarm
    
    rev_level <- input$compvarm_sw1
    if (rev_level==T) {
      comp_rev <- as.factor( timetodata[,tt.byvar1] )
      comp_rev <- relevel(comp_rev,levels(comp_rev)[2])
      timetodata$comp_rev=comp_rev
    }
    
    if (length(compvar)==1) {
      compvar1_level<-length(timetodata[,unique(tt.byvar1)] )
      
      if (compvar1_level==1) {
        compvar_txt <- "1"
      } else if (compvar1_level>1) {
        if (rev_level==T) {
          compvar_txt <- "comp_rev"
        } else {
          compvar_txt <- "tt.byvar1"
        }
      }
      
    } else if (length(compvar)==2) {  
      compvar1_level<-length(timetodata[,unique(tt.byvar1)] )
      compvar2_level<-length(timetodata[,unique(tt.byvar2)] )
      
      if (compvar1_level>1 & compvar2_level>1) {
        
        if (rev_level==T) {
          compvar_txt=paste("comp_rev","tt.byvar2",sep="+")
        } else {
          compvar_txt=paste("tt.byvar1","tt.byvar2",sep="+")
        }
      } else if (compvar1_level>1 & compvar2_level==1 ) {
        
        if (rev_level==T) {
          compvar_txt="comp_rev"
        } else {
          compvar_txt="tt.byvar1"
        }
      } else if (compvar1_level==1 & compvar2_level>1 ) {
        compvar_txt="tt.byvar2"
      } else if (compvar1_level==1 & compvar2_level==1 ) {
        compvar_txt="1"
      }     
    } else if (length(compvar)==0) {
      compvar_txt="1"
    }
    
    cox.formu<-stats::as.formula(paste("Surv(AVAL/(365.25/12), EVENT)~",compvar_txt,sep=""))
    
    fit.cox <-  coxph(cox.formu,data=timetodata)
    
    list(fit = fit, fit.cox = fit.cox, km.formu = km.formu,
         cox.formu = cox.formu, timetodata = timetodata)
  }
  
  ###############################################################################
  
  output$kmPlot<-renderPlot({
    ##############################################################################################
    # KM plot
    ##############################################################################################
    ttdata.all<-ttevent()
    
    var.data <- pop.data()
    #km.data <- copy(subvartte.data())
    km.data <- data.table(ttdata.all$timetodata)
    km.data<-km.data[is.na(AVAL) ==FALSE,] ### Exclude the NAs
    
    km.fit<-ttdata.all[[1]]
    
    n.levels<-length(unique(km.data[,tt.byvar] ))
    
    color.list<-c("orange","purple","blue","red","green","black","pink","yellow")
    
    ### input the updated subgroup information on the plot title
    if (class(var.data[, get(input$varSel)])=="character") { 
      km.var.txt<-paste0(input$valueSel, collapse = " or ")
    } else {
      km.var.txt<-paste(input$SliderRng[1],input$SliderRng[2] ,sep = " to ")
    }
    
    ### km plot
    
    ggkm(km.fit, table = T, timeby = 3,
         main = paste(input$ttvar," of ", input$varSel, "=", km.var.txt, sep = ""))
    
  })
  
  
  coxph_ci<-function() {
    
    var.data <- copy(pop.data())
    
    compvar <- input$compvarm
    fix.cox.coef=NULL
    
    
    if (length(compvar)>=1) {
      compvar1_level<-  length(var.data[,unique(tt.byvar1)])
      # length(var.data[,unique(get(compvar[1]))] ) 
    } else {
      compvar1_level<-0
    }
    
    
    if (compvar1_level==2) {
      
      if (class(var.data[, get(input$varSel)])=="character") { 
        
        if (length(compvar)==1) {
          sub.cat<-as.data.table(unique(var.data[,get(input$varSel)]))
          sub.cat.list<-paste(input$varSel, ": ", sub.cat[,V1], sep="")  # RB MODIFICATION
        } else if (length(compvar)==2) {
          sub.cat<-as.data.table(unique(var.data[,.(V1=tt.byvar2,get(input$varSel))]))
          sub.cat.list<-paste(sub.cat[,V1],paste(input$varSel, ": ", sub.cat[,V2], sep=""),sep=";")   # RB MODIFICATION
        }
      } else {
        
        srng_low <-input$SliderRng1[1]
        srng_high<-input$SliderRng1[2]
        
        var.data<-var.data[ !is.na(get(input$varSel)) ,]
        srng_min<-min( var.data[,get(input$varSel)],na.rm=TRUE)
        srng_max<-max( var.data[,get(input$varSel)],na.rm=TRUE)
        
        var.data[,srng_var:=ifelse(get(input$varSel)<srng_low ,1, 
                                   ifelse(get(input$varSel)>srng_high,3, 2) )   ]
        srng_var_uni<-unique(var.data$srng_var)
        
        var.data[,srng_var_txt:=ifelse(srng_var==1 ,paste0(srng_min," - ",srng_low), 
                                       ifelse(srng_var==3,paste0(srng_high," - ", srng_max), 
                                              paste0(srng_low," - ", srng_high)))  ]
        
        if (length(compvar)==1) {
          sub.cat<-as.data.table(unique(var.data[, srng_var_txt ]))
          sub.cat.list<-paste(input$varSel, ": ", sub.cat[,V1], sep="")  # RB MODIFICATION
          
        } else if (length(compvar)==2) {
          sub.cat<-as.data.table(unique(var.data[,.( V1=tt.byvar2 ,srng_var_txt) ]))
          sub.cat.list<-paste(sub.cat[,V1],paste(input$varSel, ": ", sub.cat[,srng_var_txt], sep=""),sep=";")  # RB MODIFICATION
        }
      }
      
      rev_level <- input$compvarm_sw1
      if (rev_level==T) {
        comp_rev = as.factor( var.data[,tt.byvar1] )
        comp_rev = relevel(comp_rev,levels(comp_rev)[2])
        var.data$comp_rev=comp_rev
      }
      
      if (rev_level==T) {
        compvar_txt <- "comp_rev"
      } else {
        compvar_txt <- "tt.byvar1"
      }
      
      cox.formu <- stats::as.formula(paste("Surv(AVAL/(365.25/12), EVENT)~",compvar_txt,sep=""))

      fix.cox.coef<-matrix(1, nrow=nrow(sub.cat),ncol=5)
      colnames(fix.cox.coef)<-c("HR","ci_lower", "ci_upper","stderr","subcat")
      rownames(fix.cox.coef)<-sub.cat.list
      
      for (i in 1:nrow(sub.cat)) {
        
        if (class(var.data[, get(input$varSel)])=="character") { 
          if (length(compvar)==1) {
            cox_datain <- var.data[get(input$varSel)==sub.cat[i,V1]]
          } else {
            cox_datain <- var.data[tt.byvar2==sub.cat[i,V1] & get(input$varSel)==sub.cat[i,V2]]
          }
          
        } else {  
          if (length(compvar)==1) {
            cox_datain <- var.data[srng_var==srng_var_uni[i]]
          } else {
            cox_datain <- var.data[tt.byvar2==sub.cat[i,V1] & srng_var_txt==sub.cat[i,srng_var_txt] ]
          }
        }
        
        
        if ( length( cox_datain[, unique(tt.byvar1) ])>=2   )  {
          fit.cox <-  coxph(cox.formu,data=cox_datain)
          
          fix.cox.coef[i,1:5]<-cbind(exp(coef(fit.cox)),exp(confint(fit.cox)),sqrt(fit.cox$var),sub.cat.list[i] )
        }
        
        
      }
      
      
      fix.cox.coef<-as.data.table(fix.cox.coef)
      fix.cox.coef[,names(fix.cox.coef)[1:4]:=lapply(.SD, as.numeric)]
      sapply(fix.cox.coef, class)
      fix.cox.coef[, names(fix.cox.coef)[1:4]:=lapply(.SD, function(i)  round(i,3) ),.SDcols=names(fix.cox.coef)[1:4]]
      
      # print("#########fix.cox.coef")
      # print(fix.cox.coef)
      setcolorder(fix.cox.coef,c(names(fix.cox.coef)[5],names(fix.cox.coef)[1:4]))
      
    }
    
    return(list(fix.cox.coef=fix.cox.coef))
  }
  
  
  output$Forest <- renderPlot({
    ##############################################################################################
    # Add Forest plot
    ##############################################################################################
    fix.cox.coef <- coxph_ci()$fix.cox.coef
    
    # Filters rows in fix.cox.coef based on the selected subgroup filter in Subgroup 1
    pop.data_df <- pop.data()
    
    var.select<- pop.data_df[, get(input$varSel)]
    variable <- var.select
    
    if (class(var.select) == "numeric" | class(var.select) == "integer" ) {
      slider_char <- paste0(variable, ": ", input$SliderRng[1], " - ", input$SliderRng[2], sep = "")
      fix.cox.coef2 <- fix.cox.coef[grepl(slider_char, fix.cox.coef$subcat, fixed = TRUE) == TRUE]
    } else {
      if (is.null(variable)) {
        fix.cox.coef2 <- fix.cox.coef
      } else {
        appended_valueSel <- sapply(input$valueSel, function(x){paste(variable, ": ", x, sep='')})
        appended_valueSel <- as.character(appended_valueSel)
        
        inclusion <- vector()
        for(i in 1:length(appended_valueSel)){
          
          matches <- fix.cox.coef$subcat[grepl(appended_valueSel[i], fix.cox.coef$subcat, fixed = TRUE) == TRUE]
          inclusion <- append(inclusion, matches)
        }
        inclusion <- unique(inclusion)
        fix.cox.coef2 <- fix.cox.coef[fix.cox.coef$subcat %in% inclusion]
      }
    }
    # END RB ADDITION
    
    if (!is.null(fix.cox.coef2)) {   # RB MODIFICATION
      forest <- ggplot(fix.cox.coef2, aes(x = subcat, y = HR, ymin = ci_lower, ymax = ci_upper)) +
        geom_hline(aes(yintercept = 1), colour = '#4400FF', linetype = 2) +
        coord_flip() +
        geom_pointrange(size = 0.7, shape = 15) +
        scale_y_log10(limits = c(0.03, 10), breaks = c(0.1, 1, 10)) +
        scale_x_discrete(limits = rev(levels(fix.cox.coef2$subcat))) +
        labs(x="") +
        theme(axis.text.y = element_text(face="bold", size=11, angle=45))
      
      forest
    }
  })
  
  output$kmText<-renderPrint({
    ##############################################################################################
    # KM and Coxph analysis - R output
    ##############################################################################################
    ttdata.all<-ttevent()
    
    temp <- ttdata.all
    
    print(ttdata.all[[1]])
    
    #print("***")
    #print(paste("cox formula ",ttdata.all[[4]][3]))
    print(ttdata.all[[2]])
    
    #print("***")
    print("* Cox HR CI")
    print(coxph_ci()$fix.cox.coef)
    
    print("Note that the Cox model p-value(s) are two-sided and the Likelihood Ratio Test p-value is one-sided.")
  }
  )
  
  output$varPlot<-renderPlot({
    ##############################################################################################
    # Bar chart
    ##############################################################################################
    
    var.data <- pop.data()
    # var.data <- var.data %>%
    #   dplyr::select(-`PARAM (Parameter)`, -`PARAMCD (Parameter Code)`, -`AVAL (Analysis Value)`, 
    #                 -`EVENT (EVENT)`)
    var.data <- unique(var.data)
    
    variable <- input$varSel
    
    ### input the updated subgroup information on the plot title
    if (class(var.data[, get(variable)])=="character") { 
      var.txt<-paste0(input$valueSel, collapse = " or ")
    } else {
      var.txt<-paste(input$SliderRng[1], input$SliderRng[2], sep = " to ")
    }
    
    compvar<-input$compvarm
    
    if (length(compvar)>=1) {
      compvar1_level<-length(var.data[,unique(tt.byvar1)] )
    } else {
      compvar1_level<-0
    }
    
    varSel_level<-var.data[!is.na(get(variable)),unique(get(variable))]
    
    if (length(varSel_level)==1 & length(compvar)==1 ) {
      g <- ggplot(var.data, aes(x=tt.byvar1,fill=tt.byvar1))
      g + geom_bar() +
        theme(legend.position = "bottom",legend.title=element_blank(),
              title = element_text(size=9, face='bold'), axis.title.x=element_blank()) +
        geom_text(stat='count',aes(label=..count..),vjust=1.25,position = "stack") +
        ggtitle(paste(variable,"=",var.txt,sep=""))
    } else {
      if (length(compvar) %in% c(0,1) ) {
        
        var.data[,FAS:="FAS patients"]  
        
        if (class(var.data[, get(variable)])=="character") {
          if (length(compvar)==0 ) {
            g <- ggplot(var.data, aes(x=get(variable),fill= FAS  ))  
          } else {
            g <- ggplot(var.data, aes(x=get(variable),fill= tt.byvar1  ))
          }
          g + geom_bar() +
            theme(legend.position = "bottom",legend.title=element_blank(),
                  title = element_text(size=9, face='bold'), axis.title.x=element_blank()) +
            geom_text(stat='count',aes(label=..count..),vjust=1.25,position = "stack") +
            ggtitle(paste(variable,"=",var.txt,sep="")) 
          
          
        } else if (class(var.data[, get(variable)])=="numeric" | class(var.data[, get(variable)])=="integer") {
          
          d=data.frame(cut=as.numeric(c(input$SliderRng[1], input$SliderRng[2])), 
                       label=c("Low", "High"))
          
          if (length(compvar)==0 ) {
            g <- ggplot(var.data, aes(x=FAS, y=get(variable) )  )
          } else {
            g <- ggplot(var.data, aes(x=tt.byvar1,y=get(variable) )  )
          }
          
          g + geom_boxplot() +
            geom_hline(data=d, mapping=aes(yintercept=cut),color="red")+
            ylab(variable)+
            theme(legend.position = "bottom",legend.title=element_blank(),
                  title = element_text(size=9, face='bold'), axis.title.x=element_blank()) +
            ggtitle(paste(variable,"=",var.txt,sep="")) 
          
        }
      } else if (length(compvar)==2) {
        if (class(var.data[, get(variable)])=="character") {
          g <- ggplot(var.data, aes(x=get(variable),fill=tt.byvar1))
          g + geom_bar() +
            theme(legend.position = "bottom",legend.title=element_blank(),
                  title = element_text(size=9, face='bold'), axis.title.x=element_blank()) +
            theme(axis.text.x = element_text(face="bold", size=12, angle=90))+theme(panel.spacing = unit(0, "lines"))+
            geom_text(stat='count',aes(label=..count..),vjust=1.25,position = "stack") +
            facet_wrap(~tt.byvar2, nrow = 1)  +
            ggtitle(paste(variable,"=",var.txt,sep=""))
          
          
        } else if (class(var.data[, get(variable)])=="numeric" | class(var.data[, get(variable)])=="integer" ) {
          
          d=data.frame(cut=as.numeric(c(input$SliderRng[1], input$SliderRng[2])), 
                       label=c("Low", "High"))
          
          # var.data<-var.data[,fc:=tt.byvar2]
          g <- ggplot(var.data, aes(x=tt.byvar1,y=get(variable)))
          g + geom_boxplot() +
            geom_hline(data=d, mapping=aes(yintercept=cut),color="red")+
            theme(legend.position = "bottom",legend.title=element_blank(),
                  title = element_text(size=9, face='bold'), axis.title.x=element_blank())+
            theme(axis.text.x = element_text(face="bold", size=12, angle=90))+ theme(panel.spacing = unit(0, "lines"))+
            ylab(variable)+
            facet_grid(~tt.byvar2) +
            ggtitle(paste(variable,"=",var.txt,sep=""))
        }
        
      }
    }
  })
  
  ##############################################################################################
  # PRO module items
  ##############################################################################################
  
  # Parameter selector for PRO visualizations
  output$proParam1 <- renderUI({
    proData <- manipulateData()$proData
    
    selectInput(inputId = "proParam1",
                label = "Parameter",
                choices = unique(proData$PARAM),
                selected = proData$PARAM[1])
  })
  
  output$proSelector <- renderUI({
    proData <- manipulateData()$proData
    
    conditionalPanel(
      condition = "input.proLevel == 'Individual'",
      
      selectInput(inputId = "proSelector",
                  label = "Subject",
                  choices = unique(proData$USUBJID),
                  multiple = TRUE,
                  selected = proData$USUBJID[1])
    )
  })
  
  # PRO parameter visualization
  output$plotPro <- renderPlot({
    
    # Subgroup data
    sge <- subpop.data()
    uniquePts <- unique(sge$USUBJID)
    
    data <- manipulateData()$proData
    ggdata <- data %>%
      dplyr::filter(PARAM == input$proParam1) %>%
      dplyr::filter(USUBJID %in% uniquePts)
    
    if(input$proLevel == "Overall"){
      
      p <- ggplot(ggdata, aes(x = round(ADY/7), y = AVAL)) +
        geom_point() +
        geom_smooth(method = "loess") +
        labs(x = "Study Week", y = input$proParam1)
    }
    
    if(input$proLevel == "Individual"){
      subjData <- ggdata %>%
        dplyr::filter(USUBJID == input$proSelector)
      
      p <- ggplot(subjData, aes(x = round(ADY/7), y = AVAL)) +
        geom_point() +
        geom_smooth(method = "loess") +
        labs(x = "Study Week", y = input$proParam1)
    }
    
    p
  })
  
  # PRO parameter LINE visualization
  output$plotProLine <- renderPlot({
    
    # Subgroup data
    sge <- subpop.data()
    uniquePts <- unique(sge$USUBJID)
    
    data <- manipulateData()$proData
    ggdata <- data %>%
      dplyr::filter(PARAM == input$proParam1) %>%
      dplyr::filter(USUBJID %in% uniquePts)
    
    if(input$proLevel == "Overall"){
      # d <- ggplot(ggdata, aes(x = as.factor(round(ADY/7)), y = AVAL)) +
      #   geom_boxplot() +
      #   labs(x = "Study Week", y = input$proParam1)
      
      d <- NULL
    }
    
    if(input$proLevel == "Individual"){
      subjData <- ggdata %>%
        dplyr::filter(USUBJID == input$proSelector)
      
      d <- ggplot(subjData, aes(x = round(ADY/7), y = AVAL)) +
        geom_line(aes(color = USUBJID)) +
        labs(x = "Study Week", y = input$proParam1)
      
    }
    
    d    
  })
  
  # PRO parameter CHANGE visualization
  output$plotProChg <- renderPlot({
    
    # Subgroup data
    sge <- subpop.data()
    uniquePts <- unique(sge$USUBJID)
    
    data <- manipulateData()$proData
    ggdata <- data %>%
      dplyr::filter(PARAM == input$proParam1 & !is.na(CHG)) %>%
      dplyr::filter(USUBJID %in% uniquePts)
    
    if(input$proLevel == "Overall"){
      c <- ggplot(ggdata, aes(x = round(ADY/7), y = CHG)) +
        geom_point() +
        geom_smooth(method = "loess") +
        labs(x = "Study Week", 
             y = paste0("Change from Baseline (", toupper(input$proParam1), ")", sep = ""))
    }
    
    if(input$proLevel == "Individual"){
      subjData <- ggdata %>%
        dplyr::filter(USUBJID == input$proSelector)
      
      c <- ggplot(subjData, aes(x = round(ADY/7), y = CHG)) +
        geom_point() +
        geom_smooth(method = "loess") +
        labs(x = "Study Week", 
             y = paste0("Change from Baseline (", toupper(input$proParam1), ")", sep = ""))
    }
    
    c
    
  })
  
  # PRO parameter CHANGE visualization
  output$plotProPctChg <- renderPlot({
    
    # Subgroup data
    sge <- subpop.data()
    uniquePts <- unique(sge$USUBJID)
    
    data <- manipulateData()$proData
    ggdata <- data %>%
      dplyr::filter(PARAM == input$proParam1 & !is.na(PCHG)) %>%
      dplyr::filter(USUBJID %in% uniquePts)
    
    if(input$proLevel == "Overall"){
      pc <- ggplot(ggdata, aes(x = round(ADY/7), y = PCHG)) +
        geom_point() +
        geom_smooth(method = "loess") +
        labs(x = "Study Week", 
             y = paste0("% Change from Baseline (", toupper(input$proParam1), ")", sep = ""))
    }
    
    if(input$proLevel == "Individual"){
      subjData <- ggdata %>%
        dplyr::filter(USUBJID == input$proSelector)
      
      pc <- ggplot(subjData, aes(x = round(ADY/7), y = PCHG)) +
        geom_point() +
        geom_smooth(method = "loess") +
        labs(x = "Study Week", 
             y = paste0("% Change from Baseline (", toupper(input$proParam1), ")", sep = ""))
    }
    
    pc
    
  })
  
  # Safety plot
  output$aesiPlot <- renderPlot({
    
    # Subgroup data
    sge <- subpop.data()
    uniquePts <- unique(sge$USUBJID)
    
    aeData <- manipulateData()$aeData
    aeDataSge <- aeData %>%
      dplyr::filter(USUBJID %in% uniquePts) 
    
    if(input$aesiOnly == TRUE){
      aeDataSge <- aeDataSge %>%
        dplyr::filter(AESIFL == "Y")
    }
    
    ggplot(aeDataSge, aes(x = STUDYID, y = ASTDY)) +
      geom_boxplot(aes(fill = STUDYID))
    
  })
  
  # Safety table
  # output$aesiTab <- DT::renderDataTable({
  #   manipulateData()$aeData, filter = 'top', rownames = FALSE,
  #                                       options = list(pageLength = 20)
  # })
  
  output$aesiTab <- DT::renderDataTable({
    
    # Subgroup data
    sge <- subpop.data()
    uniquePts <- unique(sge$USUBJID)
    
    aeData <- manipulateData()$aeData
    aeDataSge <- aeData %>%
      dplyr::filter(USUBJID %in% uniquePts) 
    
    if(input$aesiOnly == TRUE){
      aeDataSge <- aeDataSge %>%
        dplyr::filter(AESIFL == "Y")
    }
    
    aeDataSge
  })
  
  # Parameter selector for PRO deterioration - comparison plot
  output$proDeter <- renderUI({
    comparisonData <- manipulateData()$comparisonData
    
    selectInput(inputId = "proDeter",
                label = "Parameter",
                choices = unique(comparisonData$DeteriorationType),
                selected = comparisonData$DeteriorationType[1])
  })
  
  # Parameter selector for PRO deterioration - comparison plot
  output$socOption <- renderUI({
    aeData <- manipulateData()$aeData
    
    selectInput(inputId = "socOption",
                label = "AE SOC",
                multiple = TRUE,
                choices = unique(aeData$AEBODSYS),
                selected = aeData$AEBODSYS[1])
  })
  
  # Safety plot
  output$bubblePlot <- renderPlot({
    
    # Subgroup data
    sge <- subpop.data()
    uniquePts <- unique(sge$USUBJID)
    
    comparisonData <- manipulateData()$comparisonData
    comparisonDataSge <- comparisonData %>%
      dplyr::filter(USUBJID %in% uniquePts) %>%
      dplyr::filter(DeteriorationType == input$proDeter) %>%
      dplyr::filter(Category %in% input$socOption | Category %in% c("PFS (Censor)", "PFS (Event)",
                                                                    "OS (Censor)", "OS (Event)"))
    
    ggplot(comparisonDataSge) +
      geom_point(aes(x = DeteriorationTime, y = Time, color = Category)) +
      labs(x = "PRO deterioration start day", y = "AESI/PFS/OS start day",
           title = input$proDeter)
    
  })
  
  # Parameter selector for PRO visualizations
  output$proParam2 <- renderUI({
    proData <- manipulateData()$proData
    
    selectInput(inputId = "proParam2",
                label = "Parameter",
                choices = unique(proData$PARAM),
                selected = proData$PARAM[1])
  })
  
  # Heatmap
  output$heatmap <- renderPlot({
    proData <<- manipulateData()$proData
    aeData <<- manipulateData()$aeData
    tteData <<- manipulateData()$tteData
    
    cnsrData <- tteData %>%
      dplyr::filter(PARAMCD %in% c("PFS", "OS")) %>%
      dplyr::rename("eventTime" = "AVAL") %>%
      dplyr::select(USUBJID, PARAMCD, EVENT, eventTime)
    
    # Subgroup data
    sge <- subpop.data()
    uniquePts <- unique(sge$USUBJID)
    
    mapData <<- proData %>%
      dplyr::filter(PARAM == input$proParam2) %>%
      dplyr::select(USUBJID, ADY, PARAM, AVAL) %>%
      dplyr::filter(USUBJID %in% uniquePts) %>%
      inner_join(aeData) %>%
      inner_join(cnsrData) %>%
      dplyr::filter(!is.na(AVAL)) %>%
      mutate(AMONTH = floor(ADY/30.43)+1)
    
    if(input$sortingOption %in% c("PFS", "OS")){
      mapData <- mapData %>%
        dplyr::filter(PARAMCD == input$sortingOption) %>%
        arrange(desc(eventTime)) %>%
        dplyr::select(USUBJID, AMONTH, AVAL, PARAMCD, EVENT)
      mapData <- unique(mapData)
    }
    
    # Define theme
    theme <- theme_bw(base_size=16) +
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.grid = element_blank(),
            strip.background = element_rect(fill="white"),
            strip.text.y = element_text(angle=0, hjust=0, size=rel(1.05), face="bold"))
    
    h <- ggplot(data = mapData, aes(x = AMONTH, y = USUBJID)) +
      geom_tile(aes(fill = as.factor(AVAL))) +
      scale_y_discrete(labels = NULL) +
      #facet_grid(AEBODSYS ~., scales = "free_y") +
      labs(x = "Month", y = "Subjects", fill = "PRO") +
      theme +
      theme(panel.grid.minor=element_blank(),
            panel.grid.major=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank()
            #axis.text.x = element_text(angle=60,hjust=1,vjust=1,size=15),
            #axis.title.x = element_text(size=18),
      )
    
    if(input$sortingOption %in% c("PFS", "OS")){
      h <- h + facet_grid(EVENT ~ ., scales = "free_y")
    }
    
    h
  })
  
  
}