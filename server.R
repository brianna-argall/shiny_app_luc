## Shiny Server component for dashboard

# Type of low urgency care: "in"-hrs vs "after"-hrs vs "all"-hrs
# Seem to need to initialise with a value before the reactive chunk
luc_type <- c("all")

# Same for state
which_state <- c("NSW")

# AIHW demographics - this is constant and represents total LUC
# episodes for each SA3 region
aihw_dem_type <- c("All persons")

function(input, output, session){
  
  # Data table Output for AIHW
  output$dataT <- renderDataTable(aihw)
  # data frame structure for AIHW
  output$structure <- renderPrint({
    aihw %>% 
      str()
  })
  
  # Data table Output for ABS
  output$dataT2 <- renderDataTable(G19)
  # data frame structure for ABS
  output$structure2 <- renderPrint({
    G19 %>% 
      str()
  })
  
  # For Summary Output
  output$summary <- renderPrint({
    my_data %>% 
      summary()
  })

  observeEvent(input$dataSrc, {
    # Define new choices based on some condition or logic
    print(input$dataSrc)
    print(input$cond)
    new_choices <- NULL
    if (input$dataSrc == "G19") {
      new_choices <- G19_conds
    } else {
      new_choices <- G20_conds
    }
    
    # Update the choices of the selectInput
    updateSelectInput(session = shiny::getDefaultReactiveDomain(), "cond", choices = new_choices)
  })
  
  num_conditions_state <- reactive({
    # If G19
    
    if(input$state == "New South Wales"){
      state_df <- census_geo %>%
        filter(STE_NAME21 %in% c("New South Wales", 
                                 "Australian Capital Territory"))
    } else {
      state_df <- census_geo %>%
        filter(STE_NAME21 %in% input$state)
    }
    
  })
  
  var_choice <- reactive({
    paste(input$gender, input$cond, input$ageGroup, "prop", sep = "_")
  })
  
  census_var_label <- reactive({
    #req(var_choice())
    #col_labeller(var_choice())
    gender <- case_when(
      input$gender == "M" ~ "Males",
      input$gender == "F" ~ "Females",
      input$gender == "P" ~ "People"
    )
    
    age <- gsub("_", "-", input$ageGroup)
    
    cond <- gsub("_", " ", input$cond)
    
    paste(gender, age, "w/", cond, 
          br(ifelse(input$dataSrc == "G19", "Per 1,000", "% of cohort")))
  })
  
  output$censusMap <- renderTmap({
    tm_shape(num_conditions_state()) +
      tm_polygons("P_Arthritis_0_14_prop", id = "SA3_NAME21",
      # tm_polygons(var_choice(), id = "SA3_NAME21",
                  # alpha = 0.4, title = "People 0-14 w/o conditions (%)", zindex = 401)
                  alpha = 0.4, title = "People 0-14 w/ Arthritis (per thousand)")
  })
  
  observe({
    req(input$sidebar == "geoDist") # might fix the issue?
    req(census_var_label)
    
    tmapProxy("censusMap", session = shiny::getDefaultReactiveDomain(), {
      #  tm_remove_layer(401) +
      tm_shape(num_conditions_state()) +
        # tm_polygons(var_choice(), id = "SA3_NAME21", zindex = 401,title = "")
        tm_polygons(var_choice(), alpha = 0.4, id = "SA3_NAME21", 
                    title = census_var_label())
    })
  })
  
  aihw_state <- reactive({
    req(input$state)
    
    if(input$state == "New South Wales"){
      aihw %>%
        filter(!is.na(SA3_code)) %>%
        filter(aihw_dem == "All persons") %>%
        mutate(SA3_CODE21 = as.character(SA3_code)) %>%
        inner_join(SA3, ., by = "SA3_CODE21") %>%
        filter(STE_NAME21 %in% c("New South Wales", 
                                 "Australian Capital Territory"))
    } else {
      aihw %>%
        filter(!is.na(SA3_code)) %>%
        filter(aihw_dem == "All persons") %>%
        mutate(SA3_CODE21 = as.character(SA3_code)) %>%
        inner_join(SA3, ., by = "SA3_CODE21") %>%
        filter(STE_NAME21 %in% input$state)
    }
    
  })
  
  output$aihwPlot <- renderTmap({
      req(aihw_state())
      tm_shape(aihw_state()) +
      tm_polygons(luc_name(), alpha = 0.4, id = "SA3_NAME21", 
                  title = paste("Low urgency care:", input$luc_type, "hours",
                                br("Per thousand")))
  })
  cap_city <- reactive({
    case_when(
      input$state == "New South Wales" ~ "Greater Sydney",
      input$state == "Victoria" ~ "Greater Melbourne",
      input$state == "Queensland" ~ "Greater Brisbane",
      input$state == "South Australia" ~ "Greater Adelaide",
      input$state == "Western Australia" ~ "Greater Perth",
      input$state == "Tasmania" ~ "Greater Hobart",
      input$state == "Northern Territory" ~ "Greater Darwin"
    )
  })
  
  output$aihwSubset <- renderTmap({
    req(aihw_state())
    req(cap_city())
    aihw_state() %>%
      filter(GCC_NAME21 == cap_city()) %>%
      tm_shape() +
      tm_polygons(luc_name(), alpha = 0.4, id = "SA3_NAME21", 
                  title = paste("Low urgency care:", input$luc_type, "hours",
                                br("Per thousand")))
  })
  
  output$censusSubset <- renderTmap({
    req(num_conditions_state())
    req(cap_city())
    num_conditions_state() %>%
      filter(GCC_NAME21 == cap_city()) %>%
      tm_shape() +
      tm_polygons("P_Arthritis_0_14_prop", id = "SA3_NAME21",
                  alpha = 0.4, title = "People 0-14 w/ Arthritis (per thousand)")
  })
  
  # Dynamic variables
  var_name <- reactive({paste("P", input$cond_name, input$age, sep = "_")})
  luc_name <- reactive({paste("luc", input$luc_type, "prop", sep = "_")})
  
  plot_data <- reactive({
    
    G19_updated <- census_ages_conv(sex = "P", cond = input$cond_name, df = G19) %>%
      select(SA3_CODE_2021, matches("14|24|25_44|45_64|65_up|Tot"))
    
    # Subset the ABS (G19_updated) df
    condition <- G19_updated %>% 
      select(SA3_CODE_2021, !!var_name())
    
    # Subset the low urgency care df
    
    luc <- aihw %>%
      select(SA3_code, State, SA3_name, SA3_group, SA3_popn, aihw_dem, !!luc_name()) %>%
      filter(State == input$which_state, aihw_dem == aihw_dem_type)
    
    # Join the two dfs
    combined <- left_join(luc, condition, join_by(SA3_code == SA3_CODE_2021))
    
    # Disease prevalence per 1000 ("disease_prev")
    
    combined <- combined %>%
      mutate(cond_prev = get(var_name())*1000/ SA3_popn)
    
  })
  
  # Assess df structure 
  isolate({
    data <- plot_data()
    str(data)
  })
  
  # Create scatter plot
  
  output$scatterplot <- renderPlot({
    
    data <- plot_data()
    
    ggplot(data, aes(x = cond_prev, y = get(luc_name()))) +
      
      geom_point() +
      
      labs(x = paste0("Prevalence of ", gsub("cond", "condition", 
                                             gsub("_", " ", input$cond_name))), 
           y = "Episodes of Care",
           title = "Emergency Department Low Urgency Care",
           subtitle = paste0("[per 1,000 population]")
      ) +
      
      geom_smooth(method = "lm", formula = y ~x, alpha = 0.2) +
      
      theme_light() +
      
      # Make axis titles etc look a bit nicer
      theme(
        plot.title = element_text(size = 16, face = "bold",
                                  margin = margin(t = 0, r = 0, b = 10, l = 0),
                                  hjust = 0.5),
        plot.subtitle = element_text(size = 13, face = "bold",
                                     margin = margin(t = 0, r = 0, b = 15, l = 0),
                                     hjust = 0.5),
        
        axis.title.x = element_text(size = 14,
                                    margin = margin(t = 12, r = 0, b = 0, l = 0)),
        axis.text.x = element_text(size = 12),
        
        axis.title.y = element_text(size = 14,
                                    margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.text.y = element_text(size = 12),
      )
  })
  
  # Reactive expression to get the filtered data based on the selected state
  reactive_data <- reactive({
    
    which_state <- input$which_state2
    aihw_dem_type <- "All persons"
    cond_num <- c("0_cond","1_cond","2_cond", "3_or_mo_cond")
    var_name <- paste("P", cond_num, "Tot", sep = "_")
    
    # Subset the ABS (G20_updated) df for the current condition
    cond <- G20 %>%
      select(SA3_CODE_2021, !!var_name)
    
    # Subset the low urgency care df
    luc <- aihw %>%
      select(SA3_code, State, SA3_name, SA3_group, SA3_popn, aihw_dem, luc_all_prop, luc_in_prop, luc_after_prop) %>%
      filter(State == which_state, aihw_dem == aihw_dem_type)
    
    # Join the two dfs
    combined <- left_join(luc, cond, by = c("SA3_code" = "SA3_CODE_2021"))
    
    # Remove rows with any missing values
    combined_clean <- combined %>%
      drop_na() %>%
      subset(select = -c(SA3_code, SA3_popn))
    
    # Subset the LUC proportions and chronic condition columns
    luc_props <- combined_clean %>%
      select(luc_all_prop, luc_in_prop, luc_after_prop)
    
    num_cond <- combined_clean %>%
      select(contains("_Tot"))
    
    common_rows <- intersect(rownames(num_cond), rownames(luc_props))
    num_cond_aligned <- num_cond[common_rows, ]
    luc_props_aligned <- luc_props[common_rows, ]
    
    cor_matrix <- round(cor(num_cond_aligned, luc_props_aligned, use = "pairwise.complete.obs"),3)
    
    # Define the mapping from original names to new names
    rename_row <- c(
      "P_0_cond_Tot" = "None",
      "P_1_cond_Tot" = "One",
      "P_2_cond_Tot" = "Two",
      "P_3_or_mo_cond_Tot" = "Three\n or more"
    )
    
    # Rename luc_prop (columns) for better readability
    rename_luc <- c( "luc_in_prop" = "In hours", 
                     "luc_after_prop" = "After hours",
                     "luc_all_prop" = "All hours")
    
    
    # Update row and column names in the correlation matrix
    rownames(cor_matrix) <- str_replace_all(rownames(cor_matrix), rename_row)
    colnames(cor_matrix) <- str_replace_all(colnames(cor_matrix), rename_luc)
    
    # Melt the correlation matrix for use with Plotly
    melted_cor_matrix <- melt(cor_matrix)
    colnames(melted_cor_matrix) <- c("rowname", "name", "value")
    
    melted_cor_matrix$rowname <- as.factor(melted_cor_matrix$rowname)
    
    # Define the desired order for the number of chronic conditions
    # order <- c( "None", "One", "Two", "Three or more")
    
    # Convert the 'rowname' column to a factor with the desired order on y-axis
    # melted_cor_matrix$rowname <- factor(melted_cor_matrix$rowname, levels = order)
    
    # Update 'rowname' values in the melted data
    # melted_cor_matrix$rowname <- str_replace_all(melted_cor_matrix$rowname, rename_row)
    
    return(melted_cor_matrix)
  })
  
  # Render the heatmap for G20
  output$heatmapPlot_g20 <- renderPlotly({
    data <- reactive_data()
    
    # Create the Plotly heatmap with updated labels
    plotly_heatmap <- plot_ly(
      data = data,
      x = ~name,
      y = ~rowname,
      z = ~value,
      text = ~value,  # Add text annotations
      texttemplate = "%{text:.3f}",  # Format the text to show 3 decimal places
      type = "heatmap",
      colors = colorRamp(c("blue3", "white", "red3")),  # Color gradient
      colorbar = list(title = "Correlation"),  # Title for color bar
      zmin = -1,  # Minimum value for color scale
      zmax = 1    # Maximum value for color scale
    ) %>%
      layout(
        title = "Low Urgency Care Proportions vs Number of Chronic Conditions",
        xaxis = list(title = "Low Urgency Care"),
        #yaxis = list(title = "Number of Chronic Medical Conditions",categoryorder = "array", categoryarray = order)
        yaxis = list(title = "Number of Chronic Medical Conditions")
      )
    
    plotly_heatmap
  })
  
  # Reactive expression to filter data based on selected state
  filtered_data <- reactive({
    # Load necessary datasets (this should be updated with your actual data loading steps)
    # For demonstration purposes, assuming `combined_clean` and other datasets are available in the app environment
    
    which_state <- input$which_state3
    # Filter data based on selected state
    luc <- aihw %>%
      select(SA3_code, State, SA3_name, SA3_group, SA3_popn, aihw_dem, luc_all_prop, luc_in_prop, luc_after_prop) %>%
      filter(State == which_state, aihw_dem == aihw_dem_type)
    
    combined_all_cond <- NULL
    for (cond_name in cond_names) {
      var_name <- paste("P", cond_name, "Tot", sep = "_")
      
      G19_updated <- census_ages_conv(sex = "M", cond = "0_cond", df = G19) %>%
        select(SA3_CODE_2021, matches("14|24|25_44|45_64|65_up|Tot"))
      
      cond <- G19 %>%
        select(SA3_CODE_2021, !!var_name)
      
      combined <- left_join(luc, cond, by = c("SA3_code" = "SA3_CODE_2021"))
      combined <- combined %>%
        mutate(cond_prev = get(var_name) * 1000 / SA3_popn)
      
      var_prop_name <- paste(var_name, "prop", sep = "_")
      names(combined)[names(combined) == "cond_prev"] <- var_prop_name
      
      if (is.null(combined_all_cond)) {
        combined_all_cond <- combined
      } else {
        combined_all_cond <- left_join(combined_all_cond, combined, 
                                       by = c("SA3_code", "State", "SA3_name", 
                                              "SA3_group", "SA3_popn", "aihw_dem", 
                                              "luc_all_prop", "luc_in_prop", "luc_after_prop"))
      }
    }
    
    combined_clean <- na.omit(combined_all_cond) %>%
      select(-SA3_code, -SA3_popn)
    
    # Subset the LUC proportions and chronic condition columns
    luc_props <- combined_clean %>%
      select(luc_all_prop, luc_in_prop, luc_after_prop)
    
    chronic_conditions <- combined_clean %>%
      select(contains("_prop"))
    
    # Calculate the correlation matrix
    cor_matrix <- round(cor(chronic_conditions, luc_props, use = "pairwise.complete.obs"), 3)
    
    # Specify the rows and columns to select
    rows_to_select <- c(4:13)
    cols_to_select <- c(1:3)
    
    # Extract the subset of the correlation matrix
    sub_cor_matrix <- cor_matrix[rows_to_select,cols_to_select]
    
    # Rename rows for better readability
    rename_row <- c("P_Arthritis_Tot_prop" = "Arthritis", 
                    "P_Asthma_Tot_prop" = "Asthma", 
                    "P_Cancer_Tot_prop" = "Cancer",
                    "P_Dementia_Tot_prop" = "Dementia", 
                    "P_Diabetes_Tot_prop" = "Diabetes", 
                    "P_Heart_disease_Tot_prop" = "Heart Disease", 
                    "P_Kidney_disease_Tot_prop" = "Kidney Disease",
                    "P_Lung_cond_Tot_prop" = "Lung Condition", 
                    "P_Mental_health_cond_Tot_prop" = "Mental Health\n Condition",
                    "P_Stroke_Tot_prop" = "Stroke"
                   )
    
    # Rename LUC proportions(columns) for better readability
    rename_luc <- c( "luc_in_prop" = "In hours", 
                    "luc_after_prop" = "After hours",
                   "luc_all_prop" = "All hours"
                   )
    
    # Apply the renaming to rows and columns
    rownames(sub_cor_matrix) <- str_replace_all(rownames(sub_cor_matrix), rename_row)
    colnames(sub_cor_matrix) <- str_replace_all(colnames(sub_cor_matrix), rename_luc)
    
    # Convert sub_cor_matrix to a data frame
    sub_cor_matrix_df <- as.data.frame(sub_cor_matrix)
    
    # Add row names as a column
    sub_cor_matrix_df <- rownames_to_column(sub_cor_matrix_df, var = "rowname")
    
    # Reshape the data to long format for ggplot2
    melted_sub_cor_matrix <- pivot_longer(sub_cor_matrix_df, -rowname, names_to = "name", values_to = "value")
    
    # Update 'rowname' values in the melted data
    melted_sub_cor_matrix$rowname <- str_replace_all(melted_sub_cor_matrix$rowname, rename_row)
    
    # Define the desired order for the LUC proportions on the x-axis
    # x_order <- c("All_hours", "In_hours", "After_hours")
    
    # Convert the 'name' column to a factor with the desired order
    # melted_sub_cor_matrix$name <- factor(melted_sub_cor_matrix$name, levels = x_order)
    
    melted_sub_cor_matrix
  })
  
  # Render Plotly heatmap for G19
  output$heatmapPlot_g19 <- renderPlotly({
    
    data <- filtered_data()
    
    plot_ly(
      data = data,
      x = ~name,
      y = ~rowname,
      z = ~value,
      text = ~value,  # Add text annotations
      texttemplate = "%{text:.3f}",  # Format the text to show 3 decimal places
      type = "heatmap",
      colors = colorRamp(c("blue3", "white", "red3")),  
      colorbar = list(title = "Correlation"),
      zmin = -1,  # Minimum value for color scale
      zmax = 1    # Maximum value for color scale
      ) %>%
      layout(
        title = "Low Urgency Care Proportions vs Chronic Conditions",
        #xaxis = list(title = "Low Urgency Care", categoryorder = "array", categoryarray = x_order),
        xaxis = list(title = "Low Urgency Care"),
        yaxis = list(title = "Chronic Medical Conditions")
      )
  })
}

