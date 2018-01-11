shinyServer(function(input, output){
  #options(DT.extensions = list('ColReorder'), DT.options = list(scrollX=TRUE, scrollY=TRUE, colReorder = TRUE))
  # show strings data.table
  output$string_table <- DT::renderDataTable({
    string_means_selected <- string_means %>% 
      filter(reviews >= input$string_minimum_reviews)
    string_means_weighted <- string_means_selected %>% 
      arrange(desc(comfort * input$string_comfort + 
                     control * input$string_control + 
                     durability * input$string_durability + 
                     feel * input$string_feel + 
                     power * input$string_power + 
                     spin * input$string_spin + 
                     tension_stab * input$string_tension_stability + 
                     satisfaction * input$string_tester_satisfaction))
    datatable(string_means_weighted, rownames=TRUE, 
              extensions = list('ColReorder', 'FixedColumns', 'Responsive'),
              options = (list(scrollX = TRUE, scrollY=TRUE, colReorder = TRUE, 
                              fixedColumns = TRUE, autoWidth = TRUE))) %>%
    #columnDefs = list(list(width = '200px', targets= c(7,8)))) %>% 
      formatRound(columns = c('comfort', 'control', 'durability', 'feel', 
                              'power', 'spin', 'tension_stab', 'satisfaction'), 
                  digits = 4)
  })
  output$racquet_means_table <- DT::renderDataTable({
    #racquet_means_selected <- racquet_means_clean %>% filter(reviews >= input$racquet_minimum_reviews)
    racquet_selected_means <- racquet_means_clean %>% filter(tester_racquet == input$selected_racquet)
    datatable(racquet_selected_means, rownames=TRUE,
              extensions = list('ColReorder', 'FixedColumns', 'Responsive'),
              options = (list(scrollX = TRUE, scrollY=TRUE, colReorder = TRUE, 
                              fixedColumns = TRUE, autoWidth = TRUE))) %>%
      formatRound(columns = c('comfort', 'control', 'durability', 'feel', 
                            'power', 'spin', 'tension_stab', 'satisfaction'), 
                digits = 4)
  })
  
  output$racquet_details_table <- DT::renderDataTable({
    #racquet_means_selected <- racquet_means_clean %>% filter(reviews >= input$racquet_minimum_reviews)
    racquet_selected_details <- detailed_reviews %>% filter(tester_racquet == input$selected_racquet)
    datatable(racquet_selected_details, rownames = TRUE)
  })
  
  output$tester_means_table <- DT::renderDataTable({
    #racquet_means_selected <- racquet_means_clean %>% filter(reviews >= input$racquet_minimum_reviews)
    tester_selected_means <- tester_means_clean %>% filter(tester_name == input$selected_reviewer)
    datatable(tester_selected_means, rownames=TRUE) %>%
      formatRound(columns = c('comfort', 'control', 'durability', 'feel', 
                              'power', 'spin', 'tension_stab', 'satisfaction'), 
                  digits = 4)
  })
  
  output$tester_details_table <- DT::renderDataTable({
    #racquet_means_selected <- racquet_means_clean %>% filter(reviews >= input$racquet_minimum_reviews)
    tester_selected_details <- detailed_reviews %>% filter(tester_name == input$selected_reviewer)
    datatable(tester_selected_details, rownames = TRUE)
  })
})
 
  
