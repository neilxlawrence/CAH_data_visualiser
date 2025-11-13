library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(tibble)

# --- Load centiles from CSV and convert to long format ---
loadCentiles <- function(file){
  df <- read.csv(file, stringsAsFactors = FALSE, check.names = TRUE)
  if(!"age_to_use" %in% names(df)) stop("'age_to_use' column not found in CSV")
  df$age <- df$age_to_use
  
  centile_cols <- c("2","10","25","50","75","90","98")
  centile_cols_safe <- paste0("X", gsub("\\.", "", centile_cols))
  centile_cols_present <- centile_cols_safe[centile_cols_safe %in% names(df)]
  
  if(length(centile_cols_present) == 0) stop("No matching centile columns found in CSV")
  
  df_subset <- df[, c("age", centile_cols_present)]
  df_long <- tidyr::pivot_longer(
    df_subset, cols=-age,
    names_to="centile", values_to="value"
  )
  df_long$centile <- as.numeric(gsub("X", "", df_long$centile))
  return(df_long)
}

#helper function to add centile labels to plots
add_centile_labels <- function(cent_data, color){
  cent_labels <- cent_data %>% 
    dplyr::group_by(centile) %>% 
    dplyr::slice_max(age) %>% 
    dplyr::ungroup()
  
  # Custom suffix function
  suffix <- function(x) {
    ifelse(x %% 100 %in% c(11, 12, 13), "th",
           ifelse(x %% 10 == 1, "st",
                  ifelse(x %% 10 == 2, "nd",
                         ifelse(x %% 10 == 3, "rd", "th"))))
  }
  
  cent_labels$label <- paste0(cent_labels$centile, suffix(cent_labels$centile))
  
  list(
    geom_line(data = cent_data,
              aes(x = age, y = value, group = factor(centile)), color = color),
    geom_text(data = cent_labels,
              aes(x = age, y = value, label = label),
              hjust = -0.1, size = 3, color = color)
  )
}

plot_themes <-
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) 

# --- Render a single visit tab ---
renderVisitUI <- function(visit_id){
  ns <- function(x) paste0(x, visit_id)
  
  tabPanel(paste("Visit", visit_id), value = paste0("visit", visit_id),
           tabsetPanel(
             tabPanel("Blood Pressure",
                      dateInput(ns("bp_date"), "Date of reading:"),
                      numericInput(ns("sbp"), "Systolic BP (mmHg):", value = NA),
                      numericInput(ns("dbp"), "Diastolic BP (mmHg):", value = NA),
                      plotOutput(ns("bp_plot"), height = "350px")
             ),
             tabPanel("Growth",
                      dateInput(ns("growth_date"), "Date of reading:"),
                      numericInput(ns("height"), "Height (cm):", value = NA),
                      numericInput(ns("weight"), "Weight (kg):", value = NA),
                      plotOutput(ns("height_plot"), height = "350px"),
                      plotOutput(ns("weight_plot"), height = "350px"),
                      plotOutput(ns("bmi_plot"), height = "350px")
             ),
             tabPanel("Medication",
                      fluidRow(
                        column(3,
                               selectInput(ns("med1_name"), "Medication 1:",
                                           choices = c("", "Hydrocortisone", "Prednisolone", "Dexamethasone"))
                        ),
                        column(3,
                               numericInput(ns("med1_dose"), "Dose 1 (mg):", value = NA)
                        ),
                        column(3,
                               textInput(ns("med1_time"), "Time 1 (HH:MM):", value = "")
                        )
                      ),
                      fluidRow(
                        column(3,
                               selectInput(ns("med2_name"), "Medication 2:",
                                           choices = c("", "Hydrocortisone", "Prednisolone", "Dexamethasone"))
                        ),
                        column(3,
                               numericInput(ns("med2_dose"), "Dose 2 (mg):", value = NA)
                        ),
                        column(3,
                               textInput(ns("med2_time"), "Time 2 (HH:MM):", value = "")
                        )
                      ),
                      fluidRow(
                        column(3,
                               selectInput(ns("med3_name"), "Medication 3:",
                                           choices = c("", "Hydrocortisone", "Prednisolone", "Dexamethasone"))
                        ),
                        column(3,
                               numericInput(ns("med3_dose"), "Dose 3 (mg):", value = NA)
                        ),
                        column(3,
                               textInput(ns("med3_time"), "Time 3 (HH:MM):", value = "")
                        )
                      ),
                      fluidRow(
                        column(3,
                               selectInput(ns("med4_name"), "Medication 4:",
                                           choices = c("", "Hydrocortisone", "Prednisolone", "Dexamethasone"))
                        ),
                        column(3,
                               numericInput(ns("med4_dose"), "Dose 4 (mg):", value = NA)
                        ),
                        column(3,
                               textInput(ns("med4_time"), "Time 4 (HH:MM):", value = "")
                        )
                      ),
                      hr(),
                      uiOutput(ns("total_dose"))   # <-- add this so the totals show up
             )
             
           )
  )
}

server <- function(input, output, session){
  
  # --- Load centiles ---
  centiles <- list(
    male_systolic  = loadCentiles("data/icah_male_systolic_centiles.csv"),
    male_diastolic = loadCentiles("data/icah_male_diastolic_centiles.csv"),
    female_systolic  = loadCentiles("data/icah_female_systolic_centiles.csv"),
    female_diastolic = loadCentiles("data/icah_female_diastolic_centiles.csv"),
    male_height  = loadCentiles("data/icah_male_height_centiles.csv"),
    female_height = loadCentiles("data/icah_female_height_centiles.csv"),
    male_weight  = loadCentiles("data/icah_male_weight_centiles.csv"),
    female_weight = loadCentiles("data/icah_female_weight_centiles.csv"),
    male_bmi = loadCentiles("data/icah_male_bmi_centiles.csv"),
    female_bmi = loadCentiles("data/icah_female_bmi_centiles.csv")
  )
  
  # --- Track visits ---
  visits <- reactiveVal(1)
  allData <- reactiveValues()
  
  # --- Initial visit tab ---
  output$visit_tabs <- renderUI({
    tabsetPanel(id="visit_set",
                renderVisitUI(1))
  })
  
  # --- Add visit dynamically ---
  observeEvent(input$addVisit, {
    new_id <- visits() + 1
    visits(new_id)
    insertTab("visit_set",
              tab = renderVisitUI(new_id),
              target = paste0("visit", new_id - 1),
              position = "after",
              select = TRUE)
  })
  
  # --- Store inputs and render total dose for each visit ---
  observe({
    n <- visits()
    
    for(i in 1:n){
      local({
        visit_id <- i
        ns <- function(x) paste0(x, visit_id)
        
        # --- Store inputs ---
        observeEvent({
          list(input[[ns("sbp")]], input[[ns("dbp")]], input[[ns("bp_date")]],
               input[[ns("height")]], input[[ns("weight")]], input[[ns("growth_date")]],
               input[[ns("med1_name")]], input[[ns("med1_dose")]], input[[ns("med1_time")]],
               input[[ns("med2_name")]], input[[ns("med2_dose")]], input[[ns("med2_time")]],
               input[[ns("med3_name")]], input[[ns("med3_dose")]], input[[ns("med3_time")]],
               input[[ns("med4_name")]], input[[ns("med4_dose")]], input[[ns("med4_time")]]
          )
        }, {
          allData[[paste0("visit", visit_id)]] <- list(
            sbp = input[[ns("sbp")]],
            dbp = input[[ns("dbp")]],
            bp_date = input[[ns("bp_date")]],
            height = input[[ns("height")]],
            weight = input[[ns("weight")]],
            growth_date = input[[ns("growth_date")]],
            med1_name = input[[ns("med1_name")]],
            med1_dose = input[[ns("med1_dose")]],
            med1_time = input[[ns("med1_time")]],
            med2_name = input[[ns("med2_name")]],
            med2_dose = input[[ns("med2_dose")]],
            med2_time = input[[ns("med2_time")]],
            med3_name = input[[ns("med3_name")]],
            med3_dose = input[[ns("med3_dose")]],
            med3_time = input[[ns("med3_time")]],
            med4_name = input[[ns("med4_name")]],
            med4_dose = input[[ns("med4_dose")]],
            med4_time = input[[ns("med4_time")]]
          )
        }, ignoreInit = TRUE)
        
        # --- Render total dose ---
        output[[ns("total_dose")]] <- renderUI({
          d <- allData[[paste0("visit", visit_id)]]
          if(is.null(d)) return(NULL)
          
          # Collect doses and meds
          doses <- c(d$med1_dose, d$med2_dose, d$med3_dose, d$med4_dose)
          meds  <- c(d$med1_name, d$med2_name, d$med3_name, d$med4_name)
          
          # Filter out non-zero doses
          used_idx <- which(!is.na(doses) & doses > 0)
          if(length(used_idx) > 0){
            used_entries <- paste0(meds[used_idx], " (", doses[used_idx], " mg)", collapse=", ")
            total_dose <- sum(doses[used_idx])
          } else {
            used_entries <- "-"
            total_dose <- 0
          }
          
          # Hydrocortisone equivalent
          hc_values <- mapply(function(med, dose){
            if(is.na(dose)) return(0)
            switch(med,
                   "Hydrocortisone" = dose,
                   "Prednisolone"  = dose * 4,
                   "Dexamethasone" = dose * 26.667,
                   0)
          }, meds, doses)
          hc_eq <- sum(hc_values)
          hc_used_idx <- which(hc_values > 0)
          hc_entries <- if(length(hc_used_idx)>0) paste0(meds[hc_used_idx], " (equivalent to ", round(hc_values[hc_used_idx],2), " mg)", collapse=", ") else "-"
          
          # BSA
          if(!is.null(d$weight) && !is.null(d$height)){
            bsa <- sqrt(d$weight * d$height / 3600)
            hc_eq_per_bsa <- round(hc_eq / bsa, 2)
          } else {
            hc_eq_per_bsa <- NA
          }
          
          HTML(paste0(
            "<b>Total Daily Dose: ", total_dose, " mg</b><br/>",
            "<i>Contributing entries: ", used_entries, "</i><br/>",
            "<br>",
            "<b>Total Daily Hydrocortisone Equivalent: ", round(hc_eq,2), " mg</b><br/>",
            "<i>Contributing entries: ", hc_entries, "</i><br/>",
            "<br>",
            "<b>HC Equivalent per BSA: </b>", ifelse(is.na(hc_eq_per_bsa), "-", paste0(hc_eq_per_bsa, " mg/m²")),
            "<br>"
          ))
        })
        
      }) # end local
    } # end for
  }) # end observe
  
  
  
  # --- Render plots for each visit ---
  observe({
    n <- visits()
    for(i in 1:n){
      local({
        visit_id <- i
        ns <- function(x) paste0(x, visit_id)
        sex_color <- reactive(ifelse(input$sex=="Male","darkblue","darkred"))
        
        # Blood pressure
        output[[ns("bp_plot")]] <- renderPlot({
          d <- allData[[paste0("visit", visit_id)]]
          req(d, input$dob, input$sex)
          if(is.null(d$bp_date) || is.na(d$bp_date) || is.na(d$sbp) || is.na(d$dbp)) return(NULL)
          
          age <- as.numeric(difftime(lubridate::ymd(d$bp_date), input$dob, units="days"))/365.25
          
          cent <- if(input$sex=="Male") {
            list(systolic = centiles$male_systolic, diastolic = centiles$male_diastolic)
          } else {
            list(systolic = centiles$female_systolic, diastolic = centiles$female_diastolic)
          }
          
          ggplot() +
            # Systolic centile curves + labels
            add_centile_labels(cent$systolic, sex_color()) +
            
            # Systolic patient point (solid)
            geom_point(aes(x = age, y = d$sbp), color = "black", size = 3) +
            
            # Diastolic centile curves + labels
            add_centile_labels(cent$diastolic, sex_color()) +
            
            # Diastolic patient points (hollow double circle, keeps your original look)
            geom_point(aes(x = age, y = d$dbp), color = "black", size = 3, shape = 1) +
            geom_point(aes(x = age, y = d$dbp), color = "black", size = 2, shape = 1) +
            
            labs(
              title = paste("Visit", visit_id, " - Blood Pressure"),
              x = "Age (years)",
              y = "BP (mmHg)"
            ) + 
            coord_cartesian(clip = "off") +
            plot_themes
        })
        
        
        # Height plot
        output[[ns("height_plot")]] <- renderPlot({
          d <- allData[[paste0("visit", visit_id)]]
          req(d, input$dob, input$sex)
          if(is.null(d$growth_date) || is.na(d$growth_date) || is.na(d$height)) return(NULL)
          age <- as.numeric(difftime(lubridate::ymd(d$growth_date), input$dob, units="days"))/365.25
          cent <- if(input$sex=="Male") centiles$male_height else centiles$female_height
          
          ggplot() +
            add_centile_labels(cent, sex_color()) +
            geom_point(aes(x = age, y = d$height), color="black", size=3) +
            labs(title=paste("Visit", visit_id, " - Height"), x="Age (years)", y="Height (cm)") +
            coord_cartesian(clip = "off") +
            plot_themes
        })
        
        # Weight plot
        output[[ns("weight_plot")]] <- renderPlot({
          d <- allData[[paste0("visit", visit_id)]]
          req(d, input$dob, input$sex)
          if(is.null(d$growth_date) || is.na(d$growth_date) || is.na(d$weight)) return(NULL)
          age <- as.numeric(difftime(lubridate::ymd(d$growth_date), input$dob, units="days"))/365.25
          cent <- if(input$sex=="Male") centiles$male_weight else centiles$female_weight
          
          ggplot() +
            add_centile_labels(cent, sex_color()) +
            geom_point(aes(x=age, y=d$weight), color="black", size=3) +
            labs(title=paste("Visit", visit_id, " - Weight"), x="Age (years)", y="Weight (kg)") +
            coord_cartesian(clip = "off") +
            plot_themes
        })
        
        # BMI plot
        output[[ns("bmi_plot")]] <- renderPlot({
          d <- allData[[paste0("visit", visit_id)]]
          req(d, input$dob, input$sex)
          if(is.null(d$growth_date) || is.na(d$growth_date) || is.na(d$weight) || is.na(d$height)) return(NULL)
          age <- as.numeric(difftime(lubridate::ymd(d$growth_date), input$dob, units="days"))/365.25
          bmi <- d$weight / (d$height/100)^2
          cent <- if(input$sex=="Male") centiles$male_bmi else centiles$female_bmi
          
          ggplot() +
            add_centile_labels(cent, sex_color()) +
            geom_point(aes(x=age, y=bmi), color="black", size=3) +
            labs(title=paste("Visit", visit_id, " - BMI"), x="Age (years)", y="BMI (kg/m²)") +
            coord_cartesian(clip = "off") +
            plot_themes
        })
        
      })
    }
  })
  
  # --- Combine all visits ---
  all_combined <- reactive({
    req(input$dob, input$sex)
    n <- visits()
    out <- vector("list", n)
    
    for(i in seq_len(n)){
      d <- allData[[paste0("visit", i)]]
      if(is.null(d)) next
      bmi <- if(!is.null(d$weight) && !is.null(d$height)) d$weight / (d$height/100)^2 else NA_real_
      out[[i]] <- tibble(
        patient_id = input$patient_id,  
        visit  = i,
        age_bp = if(!is.null(d$bp_date) && !is.na(d$bp_date)) as.numeric(difftime(lubridate::ymd(d$bp_date), input$dob, units="days"))/365.25 else NA_real_,
        sbp    = as.numeric(d$sbp),
        dbp    = as.numeric(d$dbp),
        age_g  = if(!is.null(d$growth_date) && !is.na(d$growth_date)) as.numeric(difftime(lubridate::ymd(d$growth_date), input$dob, units="days"))/365.25 else NA_real_,
        height = as.numeric(d$height),
        weight = as.numeric(d$weight),
        bmi    = bmi,
        med1_name = d$med1_name, med1_dose = d$med1_dose, med1_time = d$med1_time,
        med2_name = d$med2_name, med2_dose = d$med2_dose, med2_time = d$med2_time,
        med3_name = d$med3_name, med3_dose = d$med3_dose, med3_time = d$med3_time,
        med4_name = d$med4_name, med4_dose = d$med4_dose, med4_time = d$med4_time
      )
    }
    
    bind_rows(out)
  })
  
  # --- All Visits plots (optional: can also color by sex) ---

  
  plot_all <- function(df, centile_data, yvar, ylab){
    line_color <- if(input$sex=="Male") "darkblue" else "darkred"
    ggplot(df, aes()) +
      add_centile_labels(centile_data, line_color) +
      geom_point(
        data = df[!is.na(df[[yvar]]) & !is.na(df$age_g), ],
        aes(x=age_g, y=.data[[yvar]], group=factor(visit)),
        colour="black", size=3
      ) +
      labs(title=paste0(input$patient_id, " - ", ylab), x="Age (years)", y=ylab) +
      coord_cartesian(clip = "off") +
      plot_themes
  }
  
  output$height_all_plot <- renderPlot({ plot_all(all_combined(), if(input$sex=="Male") centiles$male_height else centiles$female_height, "height", "Height (cm)") })
  
  output$weight_all_plot <- renderPlot({ plot_all(all_combined(), if(input$sex=="Male") centiles$male_weight else centiles$female_weight, "weight", "Weight (kg)") })
  
  output$sbp_all_plot <- renderPlot({
    df <- all_combined()
    cent <- if(input$sex=="Male") centiles$male_systolic else centiles$female_systolic
    line_color <- if(input$sex=="Male") "darkblue" else "darkred"
    
    ggplot() +
      add_centile_labels(cent, line_color) +  # <--- use helper for labels
      geom_point(data=df[!is.na(df$sbp) & !is.na(df$age_bp), ],
                 aes(x=age_bp, y=sbp, group=factor(visit)),
                 colour="black", size=3) +
      labs(title=paste0(input$patient_id, " - Systolic Blood Pressure"), x="Age (years)", y="Systolic blood pressure (mmHg)") +
      coord_cartesian(clip = "off") +
      plot_themes
  })
  
  output$dbp_all_plot <- renderPlot({
    df <- all_combined()
    cent <- if(input$sex=="Male") centiles$male_diastolic else centiles$female_diastolic
    line_color <- if(input$sex=="Male") "darkblue" else "darkred"
    
    ggplot() +
      add_centile_labels(cent, line_color) +  # <--- use helper for labels
      geom_point(data=df[!is.na(df$dbp) & !is.na(df$age_bp), ],
                 aes(x=age_bp, y=dbp, group=factor(visit)),
                 colour="black", size=3) +
      labs(title=paste0(input$patient_id, " - Diastolic Blood Pressure"), x="Age (years)", y="Diastolic blood pressure (mmHg)") +
      coord_cartesian(clip = "off") +
      plot_themes
  })
  
  output$bmi_all_plot <- renderPlot({
    plot_all(
      all_combined(),
      if(input$sex=="Male") centiles$male_bmi else centiles$female_bmi,
      "bmi",
      "BMI (kg/m²)"
    )
  })
  
  observeEvent(input$uploadData, {
    file <- input$uploadData
    req(file)
    
    df <- read.csv(file$datapath, stringsAsFactors = FALSE)
    n_visits <- max(df$visit, na.rm = TRUE)
    
    # Convert dates
    df$bp_date <- lubridate::ymd(df$bp_date)
    df$growth_date <- lubridate::ymd(df$growth_date)
    
    # Update patient-level info
    updateTextInput(session, "patient_id", value = df$patient_id[1])
    updateRadioButtons(session, "sex", selected = df$sex[1])
    updateDateInput(session, "dob", value = as.Date(df$dob[1]))
    
    # --- Ensure tabs exist ---
    current_n <- visits()
    if(n_visits > current_n){
      for(i in (current_n + 1):n_visits){
        insertTab("visit_set",
                  tab = renderVisitUI(i),
                  target = paste0("visit", i-1),
                  position = "after",
                  select = TRUE)
      }
      visits(n_visits)
    }
    
    # --- Populate allData ---
    for(i in 1:n_visits){
      d <- df[df$visit == i, ]
      allData[[paste0("visit", i)]] <- list(
        sbp = d$sbp[1],
        dbp = d$dbp[1],
        bp_date = d$bp_date[1],
        height = d$height[1],
        weight = d$weight[1],
        growth_date = d$growth_date[1],
        med1_name = d$med1_name[1], med1_dose = d$med1_dose[1], med1_time = d$med1_time[1],
        med2_name = d$med2_name[1], med2_dose = d$med2_dose[1], med2_time = d$med2_time[1],
        med3_name = d$med3_name[1], med3_dose = d$med3_dose[1], med3_time = d$med3_time[1],
        med4_name = d$med4_name[1], med4_dose = d$med4_dose[1], med4_time = d$med4_time[1]
      )
    }
    
    # --- Update inputs AFTER tabs exist ---
    for(i in 1:n_visits){
      ns <- function(x) paste0(x, i)
      d <- allData[[paste0("visit", i)]]
      
      # Update BP
      updateNumericInput(session, ns("sbp"), value = d$sbp)
      updateNumericInput(session, ns("dbp"), value = d$dbp)
      updateDateInput(session, ns("bp_date"), value = d$bp_date)
      
      # Update Growth
      updateNumericInput(session, ns("height"), value = d$height)
      updateNumericInput(session, ns("weight"), value = d$weight)
      updateDateInput(session, ns("growth_date"), value = d$growth_date)
      
      # Update Medications
      updateSelectInput(session, ns("med1_name"), selected = d$med1_name)
      updateNumericInput(session, ns("med1_dose"), value = d$med1_dose)
      updateTextInput(session, ns("med1_time"), value = d$med1_time)
      
      updateSelectInput(session, ns("med2_name"), selected = d$med2_name)
      updateNumericInput(session, ns("med2_dose"), value = d$med2_dose)
      updateTextInput(session, ns("med2_time"), value = d$med2_time)
      
      updateSelectInput(session, ns("med3_name"), selected = d$med3_name)
      updateNumericInput(session, ns("med3_dose"), value = d$med3_dose)
      updateTextInput(session, ns("med3_time"), value = d$med3_time)
      
      updateSelectInput(session, ns("med4_name"), selected = d$med4_name)
      updateNumericInput(session, ns("med4_dose"), value = d$med4_dose)
      updateTextInput(session, ns("med4_time"), value = d$med4_time)
    }
    
    showNotification(paste("Successfully loaded", n_visits, "visits from CSV"), type="message")
  })
  
  
  
  # --- Download CSV ---
  output$downloadData <- downloadHandler(
    filename = function() paste0(input$patient_id, "_all_visits_data_", gsub("[: ]", "-", Sys.Date()), ".csv"),
    content = function(file){
      
      n <- visits()
      out <- vector("list", n)
      
      for(i in seq_len(n)){
        d <- allData[[paste0("visit", i)]]
        if(is.null(d)) next
        
        out[[i]] <- tibble(
          patient_id = input$patient_id,          # <-- added
          sex        = input$sex,                 # <-- added
          dob        = as.Date(input$dob),   # <-- added as string for CSV
          visit      = i,
          sbp        = d$sbp,
          dbp        = d$dbp,
          bp_date    = as.Date(d$bp_date),          # <-- ensure Date
          height     = d$height,
          weight      = d$weight,
          growth_date = as.Date(d$growth_date),  # <-- ensure Date
          med1_name = d$med1_name, med1_dose = d$med1_dose, med1_time = d$med1_time,
          med2_name = d$med2_name, med2_dose = d$med2_dose, med2_time = d$med2_time,
          med3_name = d$med3_name, med3_dose = d$med3_dose, med3_time = d$med3_time,
          med4_name = d$med4_name, med4_dose = d$med4_dose, med4_time = d$med4_time
        )
      }
      
      df_out <- bind_rows(out)
      write.csv(df_out, file, row.names = FALSE, na = "")
    }
  )
  
  # --- Download PDF report ---

  output$downloadPDF <- downloadHandler(
    filename = function() paste0(input$patient_id, "_report_", gsub("[: ]", "-", Sys.Date()), ".pdf"),
    content = function(file) {
      
      # Temporary PDF to render plots
      temp_pdf <- tempfile(fileext = ".pdf")
      
      # Get data and color
      df <- all_combined()
      sex <- input$sex
      line_color <- ifelse(sex == "Male", "darkblue", "darkred")
      
      plots <- list(
        # Systolic BP
        ggplot() +
          add_centile_labels(
            if(sex=="Male") centiles$male_systolic else centiles$female_systolic,
            line_color
          ) +
          geom_point(
            data=df[!is.na(df$sbp) & !is.na(df$age_bp), ],
            aes(x=age_bp, y=sbp, group=factor(visit)),
            colour="black", size=3
          ) +
          labs(title=paste0(input$patient_id, " - Systolic Blood Pressure"), x="Age (years)", y="Systolic blood pressure (mmHg)") +
          coord_cartesian(clip = "off") +
          plot_themes,
        
        # Diastolic BP
        ggplot() +
          add_centile_labels(
            if(sex=="Male") centiles$male_diastolic else centiles$female_diastolic,
            line_color
          ) +
          geom_point(
            data=df[!is.na(df$dbp) & !is.na(df$age_bp), ],
            aes(x=age_bp, y=dbp, group=factor(visit)),
            colour="black", size=3
          ) +
          labs(title=paste0(input$patient_id, " - Diastolic Blood Pressure"), x="Age (years)", y="Diastolic blood pressure (mmHg)") +
          coord_cartesian(clip = "off") +
          plot_themes,
        
        # Height
        plot_all(df, if(sex=="Male") centiles$male_height else centiles$female_height, "height", "Height (cm)"),
        
        # Weight
        plot_all(df, if(sex=="Male") centiles$male_weight else centiles$female_weight, "weight", "Weight (kg)"),
        
        # BMI
        plot_all(df, if(sex=="Male") centiles$male_bmi else centiles$female_bmi, "bmi", "BMI (kg/m²)")
      )
      
      # Save to PDF
      pdf(temp_pdf, width=11, height=8.5)
      
      # --- Cover page ---
      grid::grid.newpage()
      
      # First four lines in larger text
      grid::grid.text(
        paste0(
          "Patient Identifier: ", input$patient_id, "\n",
          "Sex: ", input$sex, "\n",
          "Date of Birth: ", input$dob, "\n",
          "Date of Report Generation: ", Sys.Date()
        ),
        x = 0.5, y = 0.7, just = "center",
        gp = grid::gpar(fontsize = 18, fontface = "bold")
      )
      
      # Disclaimer in smaller text below
      grid::grid.text(
        paste0(
          "Disclaimer: This report is generated from the prototype CAH Longitudinal Data Plotting Tool.\n",
          "This application is for research use only, and not to be used in clinical practice.\n",
          "Please direct queries to Neil Lawrence (n.r.lawrence@sheffield.ac.uk)"
        ),
        x = 0.5, y = 0.3, just = "center",
        gp = grid::gpar(fontsize = 12)
      )
      
      # --- Plots ---
      for(p in plots){
        print(p)
      }
      
      dev.off()
      
      # Copy temp PDF to output file
      file.copy(temp_pdf, file, overwrite = TRUE)
    }
  )
  
  
  
}

ui <- fluidPage(
  h1("Congenital Adrenal Hyperplasia", align = "center"),
  h2("Longitudinal Data Plotting Tool", align = "center"),
  h5("This is a prototype clinical decision support tool, use to be restricted for research purposes only", align = "center"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("patient_id", "Patient identifier:", value = ""),
      radioButtons("sex", "Sex:", choices = c("Male","Female")),
      dateInput("dob","Date of Birth:"),
      br(),
      actionButton("addVisit","Add Another Visit"),
      br(),br(),
      fileInput("uploadData","Upload Previously Saved Data (CSV)"),
      br(),
      downloadButton("downloadData","Download All Data (CSV)"),
      br(),br(),
      downloadButton("downloadPDF","Download PDF Report"),
      br(),br(),
      p("Centiles created from I-CAH registry data (further details in: https://github.com/neilxlawrence/I-CAH_Blood_Pressure)", style="text-align:center; color:black;")
    ),
    
    mainPanel(
      uiOutput("visit_tabs"),
      tabsetPanel(id="all_visits_tabs",
                  tabPanel("All Visits",
                           plotOutput("sbp_all_plot", height="350px"),
                           plotOutput("dbp_all_plot", height="350px"),
                           plotOutput("height_all_plot", height="350px"),
                           plotOutput("weight_all_plot", height="350px"),
                           plotOutput("bmi_all_plot", height="350px")
                  ),
                  tabPanel("Instructions",
                           h3("How to Use the Congenital Adrenal Hyperplasia Longitudinal Data Plotting Tool"),
                           p("1. Enter the patient identifier, sex, and date of birth."),
                           p("2. Add as many visits as are available using the 'Add Another Visit' button."),
                           p("3. Enter blood pressure, growth, and medication data for each visit."),
                           p("4. Use 'download data' to save progress as a csv file."),
                           p("5. Reload previously saved CSV data to carry on where you left off."),
                           p("5. Use 'download PDF' report to generate a report for viewing."),
                           p("6. Use the 'All Visits' tab to view the data displayed for all entered visits."),
                           br(),
                           p("For more information, contact n.r.lawrence@sheffield.ac.uk")
                  )
      )
    )
  )
)

shinyApp(ui, server)
