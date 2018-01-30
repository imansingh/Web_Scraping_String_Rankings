shinyServer(function(input, output){
  #options(DT.extensions = list('ColReorder'), DT.options = list(scrollX=TRUE, scrollY=TRUE, colReorder = TRUE))
  # show strings data.table
  
  ## racquet_model_list
  # Produce updated list of racquet models based on user selection
  # for racquet manufacturer
  output$racquet_model_list = renderUI({
    filtered_model_list = sapply(input$racquet_manufacturer, 
                                 function(string) 
                                   unname(models_by_manufacturer[string]))
    selectizeInput(
      'racquet_model',
      'Tester Racquet Model(s)',
      choices = filtered_model_list,
      multiple = TRUE,
      options = list(placeholder = 
                       '(choose one or more)',
                     maxOptions = 2000))
  })
  
  ## get string_data_filtered()
  # reactive expression to store datatable filtered based on user criteria
  # to be used in criteria_table and selector_table output
  
  get_string_data_filtered = reactive({
    # Filter based on string criteria
    # create string_data_filtered and filter by string_minimum_reviews
    string_data_filtered = string_data1 %>%
      filter(num_ratings >= input$string_minimum_reviews)
    
    # string_price
    if(input$price_missing){
      string_data_filtered = string_data_filtered %>%
        filter(price_adjusted >= input$string_price[1] |
                 is.na(price_adjusted)) %>%
        filter(price_adjusted <= input$string_price[2] |
                 is.na(price_adjusted))    
    } else {
      string_data_filtered = string_data_filtered %>%
        filter(price_adjusted >= input$string_price[1])  %>%
        filter(price_adjusted <= input$string_price[2])
    }
    
    #string_material
    if(!(is.null(input$string_material))){
      matrix = sapply(input$string_material,
                      function(string) 
                        grepl(string, 
                              string_data_filtered$string_material))
      if('None Listed' %in% input$string_material){
        matrix = cbind(matrix, 
                       is.na(string_data_filtered$string_material))
      }
      string_data_filtered = string_data_filtered[rowSums(matrix) > 0,]
    }
    
    #string_construction
    if(!(is.null(input$string_construction))){
      matrix = sapply(input$string_construction,
                      function(string) 
                        grepl(string, 
                              string_data_filtered$string_construction))
      if('None Listed' %in% input$string_construction){
        matrix = cbind(matrix, 
                       is.na(string_data_filtered$string_construction))
      }
      string_data_filtered = string_data_filtered[rowSums(matrix) > 0,]
    }
    
    #string_features
    if(!(is.null(input$string_features))){
      matrix = sapply(input$string_features,
                      function(string) 
                        grepl(string, 
                              string_data_filtered$string_features))
      if('None Listed' %in% input$string_features){
        matrix = cbind(matrix, 
                       is.na(string_data_filtered$string_features))
      }
      string_data_filtered = string_data_filtered[rowSums(matrix) > 0,]
    }
    
    #string_gauge_metric
    if(input$gauge_metric_missing){
      string_data_filtered = string_data_filtered %>%
        filter(string_gauge_metric >= input$string_gauge_metric[1] |
                 is.na(string_gauge_metric)) %>%
        filter(string_gauge_metric <= input$string_gauge_metric[2] |
                 is.na(string_gauge_metric))    
    } else {
      string_data_filtered = string_data_filtered %>%
        filter(string_gauge_metric >= input$string_gauge_metric[1])  %>%
        filter(string_gauge_metric <= input$string_gauge_metric[2])
    }
    
    #string_gauge_us
    if(input$gauge_us_missing){
      string_data_filtered = string_data_filtered %>%
        filter(string_gauge_us >= input$string_gauge_us[1] |
                 is.na(string_gauge_us)) %>%
        filter(string_gauge_us <= input$string_gauge_us[2] |
                 is.na(string_gauge_us))    
    } else {
      string_data_filtered = string_data_filtered %>%
        filter(string_gauge_us >= input$string_gauge_us[1])  %>%
        filter(string_gauge_us <= input$string_gauge_us[2])
    }
    
    #string_adjectives_positive
    if(!(is.null(input$string_adjectives_positive))){
      matrix = sapply(input$string_adjectives_positive,
                      function(string) 
                        grepl(string, 
                              string_data_filtered$string_adjectives))
      if(input$adjectives_positive_missing){
        matrix = cbind(matrix, 
                       is.na(string_data_filtered$string_adjectives))
      }
      string_data_filtered = string_data_filtered[rowSums(matrix) > 0,]
    }
    
    #string_adjectives_negative
    if(!(is.null(input$string_adjectives_negative))){
      matrix = sapply(input$string_adjectives_negative,
                      function(string) 
                        grepl(string, 
                              string_data_filtered$string_adjectives))
      if(!input$adjectives_negative_missing){
        matrix = cbind(matrix, 
                       is.na(string_data_filtered$string_adjectives))
      }
      string_data_filtered = string_data_filtered[rowSums(matrix) < 1,]
    }
    
    # Filter based on tester criteria
    # tester_minimum_reviews
    string_data_filtered = string_data_filtered %>%
      filter(tester_reviews >= input$tester_minimum_reviews)
    
    # tester_gender
    if(!(is.null(input$tester_gender))){
      matrix = sapply(input$tester_gender,
                      function(string)
                        grepl(string,
                              string_data_filtered$tester_gender))
      if('None Listed' %in% input$tester_gender){
        matrix = cbind(matrix,
                       is.na(string_data_filtered$tester_gender))
      }
      string_data_filtered = string_data_filtered[rowSums(matrix) > 0,]
    } 
    
    # tester_age
    if(!(is.null(input$tester_age))){
      matrix = sapply(input$tester_age,
                      function(string)
                        grepl(string,
                              string_data_filtered$tester_age))
      if('None Listed' %in% input$tester_age){
        matrix = cbind(matrix,
                       is.na(string_data_filtered$tester_age))
      }
      string_data_filtered = string_data_filtered[rowSums(matrix) > 0,]
    }
    
    # tester_style
    if(!(is.null(input$tester_playstyle))){
      matrix = sapply(input$tester_playstyle,
                      function(string)
                        grepl(string,
                              string_data_filtered$tester_playstyle))
      if('None Listed' %in% input$tester_playstyle){
        matrix = cbind(matrix,
                       is.na(string_data_filtered$tester_playstyle))
      }
      string_data_filtered = string_data_filtered[rowSums(matrix) > 0,]
    }
    
    # tester_level
    if(!(is.null(input$tester_level))){
      matrix = sapply(input$tester_level,
                      function(string)
                        grepl(string,
                              string_data_filtered$tester_level))
      if('None Listed' %in% input$tester_level){
        matrix = cbind(matrix,
                       is.na(string_data_filtered$tester_level))
      }
      string_data_filtered = string_data_filtered[rowSums(matrix) > 0,]
    }
    
    # tester_swing_speed
    if(!(is.null(input$tester_strokes))){
      matrix = sapply(input$tester_strokes,
                      function(string)
                        grepl(string,
                              string_data_filtered$tester_strokes))
      if('None Listed' %in% input$tester_strokes){
        matrix = cbind(matrix,
                       is.na(string_data_filtered$tester_strokes))
      }
      string_data_filtered = string_data_filtered[rowSums(matrix) > 0,]
    }
    
    # tester_spin_level
    if(!(is.null(input$tester_spin))){
      matrix = sapply(input$tester_spin,
                      function(string)
                        grepl(string,
                              string_data_filtered$tester_spin))
      if('None Listed' %in% input$tester_spin){
        matrix = cbind(matrix,
                       is.na(string_data_filtered$tester_spin))
      }
      string_data_filtered = string_data_filtered[rowSums(matrix) > 0,]
    }
    
    # Filter based on tester racquet criteria
    # tester_racquet_manufacturer
    if(!(is.null(input$racquet_manufacturer))){
      matrix = sapply(input$racquet_manufacturer,
                      function(string)
                        grepl(string,
                              string_data_filtered$racquet_manufacturer))
      if(input$manufacturer_missing){
        matrix = cbind(matrix,
                       is.na(string_data_filtered$racquet_manufacturer))
      }
      string_data_filtered = string_data_filtered[rowSums(matrix) > 0,]
    }
    
    # if(input$manufacturer_missing){
    #   matrix = matrix(is.na(string_data_filtered$racquet_manufacturer))
    #   if(!(is.null(input$racquet_manufacturer))){
    #     matrix = cbind(matrix, 
    #                    sapply(input$racquet_manufacturer, function(string) 
    #                      grepl(string, 
    #                            string_data_filtered$racquet_manufacturer)))
    #   }
    #   string_data_filtered = string_data_filtered[rowSums(matrix) > 0 , ]
    # } else {
    #   if(!(is.null(input$racquet_manufacturer))){
    #     matrix = cbind(matrix, 
    #                    sapply(input$racquet_manufacturer, function(string) 
    #                      grepl(string, 
    #                            string_data_filtered$racquet_manufacturer)))
    #     string_data_filtered = string_data_filtered[rowSums(matrix) > 0 , ]
    #     }
    #   }
    
    # tester_racquet_model
    if(!(is.null(input$racquet_model))){
      matrix = sapply(input$racquet_model,
                      function(string)
                        grepl(string,
                              string_data_filtered$racquet_model))
      if(input$model_missing){
        matrix = cbind(matrix,
                       is.na(string_data_filtered$racquet_model))
      }
      string_data_filtered = string_data_filtered[rowSums(matrix) > 0,]
    }
    
    # tester_string_pattern
    if(!(is.null(input$string_pattern))){
      matrix = sapply(input$string_pattern,
                      function(string)
                        grepl(string,
                              string_data_filtered$string_pattern))
      if(input$pattern_missing){
        matrix = cbind(matrix,
                       is.na(string_data_filtered$string_pattern))
      }
      string_data_filtered = string_data_filtered[rowSums(matrix) > 0,]
    }
    
    # tester_frame_size
    size_vec = unname(sapply(input$frame_size, 
                             function(string) 
                               strsplit(string, split = ' ')[[1]][1]))
    
    if(!(is.null(size_vec))){
      matrix = sapply(size_vec,
                      function(string)
                        grepl(string,
                              string_data_filtered$frame_size))
      if('None' %in% size_vec){
        matrix = cbind(matrix,
                       is.na(string_data_filtered$frame_size))
      }
      string_data_filtered = string_data_filtered[rowSums(matrix) > 0,]
    }
    
    # tester_main_tension
    if(input$main_missing){
      string_data_filtered = string_data_filtered %>%
        filter(main_tension >= input$main_tension[1] |
                 main_tension < 35 | main_tension > 75 |
                 is.na(main_tension)) %>%
        filter(main_tension <= input$main_tension[2] |
                 main_tension < 35 | main_tension > 75 |
                 is.na(main_tension))    
    } else {
      string_data_filtered = string_data_filtered %>%
        filter(main_tension >= input$main_tension[1])  %>%
        filter(main_tension <= input$main_tension[2])
    }
    
    # tester_cross_tension
    if(input$cross_missing){
      string_data_filtered = string_data_filtered %>%
        filter(cross_tension >= input$cross_tension[1] |
                 cross_tension < 35 | cross_tension > 75 |
                 is.na(cross_tension)) %>%
        filter(cross_tension <= input$cross_tension[2] |
                 cross_tension < 35 | cross_tension > 75 |
                 is.na(cross_tension))    
    } else {
      string_data_filtered = string_data_filtered %>%
        filter(cross_tension >= input$cross_tension[1])  %>%
        filter(cross_tension <= input$cross_tension[2])
    }
    
    ## return filtered datatable
    return(string_data_filtered)
  })
  
  ## criteria_table
  # Produce data table using filtered data from string_data_filtered()
  output$criteria_table = DT::renderDataTable({
    # paste all the vectors of strings into single strings for display
    string_data_filtered = get_string_data_filtered()
    
    string_data_filtered$string_adjectives =
      sapply(string_data_filtered$string_adjectives,
             function(vec) paste(vec, collapse = ', '))
    
    string_data_filtered$string_material = 
      sapply(string_data_filtered$string_material, 
             function(vec) paste(vec, collapse = ', '))
    
    string_data_filtered$string_construction = 
      sapply(string_data_filtered$string_construction, 
             function(vec) paste(vec, collapse = ', '))
    
    string_data_filtered$string_features = 
      sapply(string_data_filtered$string_features, 
             function(vec) paste(vec, collapse = ', '))
    
    # create datatable
    datatable(string_data_filtered, rownames=TRUE, 
              extensions = list('ColReorder', 'FixedColumns', 'Responsive'),
              options = (list(scrollX = TRUE, scrollY=TRUE, colReorder = TRUE,
                              fixedColumns = TRUE, autoWidth = TRUE)))
  })
  
  ## selector_table
  # Produce sorted/ranked table based on 
  output$selector_table = DT::renderDataTable({
    
    selector_table_characteristics = get_string_data_filtered() %>%
      # group filtered data frame by string names
      group_by(string_name) %>%
      # get the mean scores for each string
      summarise(num_char_reviews = n(), 
                comfort = mean(comfort, na.rm=TRUE), 
                control = mean(control, na.rm=TRUE), 
                durability = mean(durability, na.rm=TRUE), 
                feel = mean(feel, na.rm=TRUE), 
                power = mean(power, na.rm=TRUE), 
                spin = mean(spin, na.rm=TRUE), 
                tension_stab = mean(tension_stability, na.rm=TRUE), 
                satisfaction = mean(tester_satisfaction, na.rm=TRUE)) %>%
      # multiply means by input to get weighted means, then sum to get c_score
      mutate(characteristics_score = 
               comfort * input$string_comfort +
               control * input$string_control +
               durability * input$string_durability +
               feel * input$string_feel +
               power * input$string_power +
               spin * input$string_spin +
               tension_stab * input$string_tension_stability +
               satisfaction * input$string_tester_satisfaction) %>%
      # arrange by c_score, descennding
      arrange(desc(characteristics_score))
    
    
    # convert c_score to percentile - scale for z-stat, then pnorm
    selector_table_characteristics = 
      selector_table_characteristics %>%
      mutate(characteristics_score = 
               pnorm(scale(selector_table_characteristics$
                             characteristics_score)) * 
               100)

     selector_table_adjectives = get_string_data_filtered() %>%
       # remove reviews with no adjectives listed
       filter(!(is.na(string_adjectives))) %>%
       # for each review get percentage of adjectives listed matching adjective
       mutate(soft = get_adjective_pct(
         get_string_data_filtered()$string_adjectives, 'soft')) %>%
       mutate(comfortable = get_adjective_pct(
         get_string_data_filtered()$string_adjectives, 'comfortable')) %>%
       mutate(flexible = get_adjective_pct(
         get_string_data_filtered()$string_adjectives, 'flexible')) %>%
       mutate(precise = get_adjective_pct(
         get_string_data_filtered()$string_adjectives, 'precise')) %>%
       mutate(resilient = get_adjective_pct(
         get_string_data_filtered()$string_adjectives, 'resilient')) %>%
       mutate(explosive = get_adjective_pct(
         get_string_data_filtered()$string_adjectives, 'explosive')) %>%
       mutate(innovative = get_adjective_pct(
         get_string_data_filtered()$string_adjectives, 'innovative')) %>%
       mutate(unique = get_adjective_pct(
         get_string_data_filtered()$string_adjectives, 'unique')) %>%
       mutate(spongy = get_adjective_pct(
         get_string_data_filtered()$string_adjectives, 'spongy')) %>%
       mutate(stiff = get_adjective_pct(
         get_string_data_filtered()$string_adjectives, 'stiff')) %>%
       mutate(dull = get_adjective_pct(
         get_string_data_filtered()$string_adjectives, 'dull')) %>%
       mutate(lively = get_adjective_pct(
         get_string_data_filtered()$string_adjectives, 'lively')) %>%
       mutate(stretchy = get_adjective_pct(
         get_string_data_filtered()$string_adjectives, 'stretchy')) %>%
       mutate(crispy = get_adjective_pct(
         get_string_data_filtered()$string_adjectives, 'crispy')) %>%
       mutate(boring = get_adjective_pct(
         get_string_data_filtered()$string_adjectives, 'boring')) %>%
       mutate(elastic = get_adjective_pct(
         get_string_data_filtered()$string_adjectives, 'elastic')) %>%
       mutate(solid = get_adjective_pct(
         get_string_data_filtered()$string_adjectives, 'solid')) %>%
       mutate(rough = get_adjective_pct(
         get_string_data_filtered()$string_adjectives, 'rough')) %>%
       mutate(wire_like = get_adjective_pct(
         get_string_data_filtered()$string_adjectives, 'wire-like')) %>%
       mutate(springy = get_adjective_pct(
         get_string_data_filtered()$string_adjectives, 'springy')) %>%
       mutate(sluggish = get_adjective_pct(
         get_string_data_filtered()$string_adjectives, 'sluggish')) %>%
       mutate(outdated = get_adjective_pct(
         get_string_data_filtered()$string_adjectives, 'outdated')) %>%
       # group reviews by string name
       group_by(string_name) %>%
       # for reviews grouped by string name get mean % for each adjective
       summarise(num_adjec_reviews = n(),
                 soft = mean(soft),
                 comfortable = mean(comfortable),
                 flexible = mean(flexible),
                 precise = mean (precise),
                 resilient = mean(resilient),
                 explosive = mean(explosive),
                 innovative = mean(innovative),
                 unique = mean(unique),
                 spongy = mean(spongy),
                 stiff = mean(stiff),
                 dull = mean(dull),
                 lively = mean(lively),
                 stretchy  = mean(stretchy),
                 crispy = mean(crispy),
                 boring = mean(boring),
                 elastic = mean(elastic),
                 solid = mean(solid),
                 rough = mean(rough),
                 wire_like = mean(wire_like),
                 springy = mean(springy),
                 sluggish = mean(sluggish),
                 outdated = mean(outdated)) %>%
       # multiply means by user input to get weighted means, then sum for score 
       mutate(adjectives_score = 
                soft * input$soft +
                comfortable * input$comfortable +
                flexible * input$flexible +
                precise * input$precise +
                resilient * input$resilient +
                explosive * input$explosive +
                innovative * input$innovative +
                unique * input$unique +
                spongy * input$spongy +
                stiff * input$stiff +
                dull * input$dull +
                lively * input$lively +
                stretchy  * input$stretchy +
                crispy * input$crispy +
                boring * input$boring +
                elastic * input$elastic +
                solid * input$solid +
                rough * input$rough +
                wire_like * input$wire_like +
                springy * input$springy +
                sluggish * input$sluggish +
                outdated * input$outdated) %>%
       # arrange by adjectives_score descending
       arrange(desc(adjectives_score))
     
     # convert adjectives_score to percentile - scale for z-stat, then pnorm
     selector_table_adjectives =
       selector_table_adjectives %>%
       mutate(adjectives_score = 
                pnorm(scale(selector_table_adjectives$adjectives_score)) * 
                100)
     
     selector_table_both = selector_table_characteristics %>%
       full_join(selector_table_adjectives, by = 'string_name') %>%
       mutate(combined_score = 
         (characteristics_score * input$c_weight +
            adjectives_score * input$a_weight) /
           sum(input$c_weight, input$a_weight)) %>%
       arrange(desc(combined_score[,]))
     
     if(input$table_choice == 'characteristics'){
       datatable(selector_table_characteristics, rownames=TRUE,
                 extensions = list('ColReorder', 'FixedColumns', 'Responsive'),
                 options = (list(scrollX = TRUE, scrollY=TRUE, colReorder = TRUE,
                                 fixedColumns = TRUE, autoWidth = TRUE))) %>%
         #columnDefs = list(list(width = '200px', targets= c(7,8)))) %>%
         formatRound(columns = c('comfort', 'control', 'durability', 'feel',
                                 'power', 'spin', 'tension_stab', 'satisfaction'),
                     digits = 4)
     } else if(input$table_choice == 'adjectives'){
       datatable(selector_table_adjectives, rownames=TRUE,
                 extensions = list('ColReorder', 'FixedColumns', 'Responsive'),
                 options = (list(scrollX = TRUE, scrollY=TRUE, colReorder = TRUE,
                                 fixedColumns = TRUE, autoWidth = TRUE)))
     } else if(input$table_choice == 'both'){
       datatable(selector_table_both, rownames=TRUE,
                 extensions = list('ColReorder', 'FixedColumns', 'Responsive'),
                 options = (list(scrollX = TRUE, scrollY=TRUE, colReorder = TRUE,
                                 fixedColumns = TRUE, autoWidth = TRUE)))
       }
    

    # 
    #%>%
      #columnDefs = list(list(width = '200px', targets= c(7,8)))) %>%
      # formatRound(columns = c('comfort', 'control', 'durability', 'feel',
      #                         'power', 'spin', 'tension_stab', 'satisfaction'),
      #            digits = 4)
    })
  
  # output$string_table <- DT::renderDataTable({
  #   string_means_selected <- string_means %>% 
  #     filter(reviews >= input$string_minimum_reviews)
  #   string_means_weighted <- string_means_selected %>% 
  #     arrange(desc(comfort * input$string_comfort + 
  #                    control * input$string_control + 
  #                    durability * input$string_durability + 
  #                    feel * input$string_feel + 
  #                    power * input$string_power + 
  #                    spin * input$string_spin + 
  #                    tension_stab * input$string_tension_stability + 
  #                    satisfaction * input$string_tester_satisfaction))
  #   datatable(string_means_weighted, rownames=TRUE, 
  #             extensions = list('ColReorder', 'FixedColumns', 'Responsive'),
  #             options = (list(scrollX = TRUE, scrollY=TRUE, colReorder = TRUE, 
  #                             fixedColumns = TRUE, autoWidth = TRUE))) %>%
  #   #columnDefs = list(list(width = '200px', targets= c(7,8)))) %>% 
  #     formatRound(columns = c('comfort', 'control', 'durability', 'feel', 
  #                             'power', 'spin', 'tension_stab', 'satisfaction'), 
  #                 digits = 4)
  # })
  # output$racquet_means_table <- DT::renderDataTable({
  #   #racquet_means_selected <- racquet_means_clean %>% filter(reviews >= input$racquet_minimum_reviews)
  #   racquet_selected_means <- racquet_means_clean %>% filter(tester_racquet == input$selected_racquet)
  #   datatable(racquet_selected_means, rownames=TRUE,
  #             extensions = list('ColReorder', 'FixedColumns', 'Responsive'),
  #             options = (list(scrollX = TRUE, scrollY=TRUE, colReorder = TRUE, 
  #                             fixedColumns = TRUE, autoWidth = TRUE))) %>%
  #     formatRound(columns = c('comfort', 'control', 'durability', 'feel', 
  #                           'power', 'spin', 'tension_stab', 'satisfaction'), 
  #               digits = 4)
  # })
  # 
  # output$racquet_details_table <- DT::renderDataTable({
  #   #racquet_means_selected <- racquet_means_clean %>% filter(reviews >= input$racquet_minimum_reviews)
  #   racquet_selected_details <- detailed_reviews %>% filter(tester_racquet == input$selected_racquet)
  #   datatable(racquet_selected_details, rownames = TRUE)
  # })
  # 
  # output$tester_means_table <- DT::renderDataTable({
  #   #racquet_means_selected <- racquet_means_clean %>% filter(reviews >= input$racquet_minimum_reviews)
  #   tester_selected_means <- tester_means_clean %>% filter(tester_name == input$selected_reviewer)
  #   datatable(tester_selected_means, rownames=TRUE) %>%
  #     formatRound(columns = c('comfort', 'control', 'durability', 'feel', 
  #                             'power', 'spin', 'tension_stab', 'satisfaction'), 
  #                 digits = 4)
  # })
  # 
  # output$tester_details_table <- DT::renderDataTable({
  #   #racquet_means_selected <- racquet_means_clean %>% filter(reviews >= input$racquet_minimum_reviews)
  #   tester_selected_details <- detailed_reviews %>% filter(tester_name == input$selected_reviewer)
  #   datatable(tester_selected_details, rownames = TRUE)
  # })
})
 
  
