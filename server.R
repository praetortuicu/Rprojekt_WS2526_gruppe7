library(shiny)

server <- function(input, output, session){
  
  # ---------- DATENBANK ----------
  
  db <- reactiveVal(
    data.frame(
      Entry_ID = c("A","B","C"),
      
      stringsAsFactors = FALSE
    )
  )
  
  update_db <- function(new_db){
    db(new_db)
  }
  
  # ---------- HELPER ----------
  
  generate_alpha_ids <- function(n){
    ids <- character(n)
    
    for(i in 1:n){
      x <- i
      name <- ""
      
      while(x > 0){
        x <- x - 1
        name <- paste0(LETTERS[(x %% 26) + 1], name)
        x <- x %/% 26
      }
      
      ids[i] <- name
    }
    
    ids
  }
  
  # ---------- REACTIVE DB ----------
  
  db_data <- reactive({
    db()
  })
  
  # ---------- OUTPUTS ----------
  
  output$database_table <- renderTable({
    db_data()
  })
  
  output$selected_algo <- renderText({
    paste("Selected Algorithm:", input$choose_algo)
  })
  
  output$depth <- renderText({
    paste("Depth:", 4)
  })
  
  output$time <- renderText({
    paste("Time:", "0.01s")
  })
  
  output$leafs <- renderText({
    paste("Leafs:", 12)
  })
  
  # ---------- DRAW TREE ----------
  
  observeEvent(input$draw_tree,{
    
    if(nrow(db()) == 0 || any(is.na(db())) || input$choose_algo == "Choose Algorithm"){
      
      showModal(
        modalDialog(
          title = "Error",
          p("Die aktuellen Einstellungen für die Baumgenerierung sind ungültig."),
          footer = modalButton("Close")
        )
      )
      
      return()
    }
    
    showModal(
      modalDialog(
        title = "Baum generieren",
        p("Algorithmus:", input$choose_algo),
        p("Menge an Daten:", nrow(db())),
        br(),
        p("Möchten Sie fortfahren?"),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_draw_tree", "Confirm")
        )
      )
    )
    
  })
  
  # ---------- ADD ENTRY ----------
  
  observeEvent(input$add_entry,{
    
    showModal(
      modalDialog(
        title = "Eintrag hinzufügen",
        
        lapply(names(db()), function(col){
          textInput(paste0("new_", col), paste("Value for", col))
        }),
        
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_add_entry", "Confirm")
        )
      )
    )
    
  })
  
  
  observeEvent(input$confirm_add_entry,{
    
    current_db <- db()
    
    new_row <- lapply(names(current_db), function(col){
      input[[paste0("new_", col)]]
    })
    
    new_row <- as.data.frame(new_row, stringsAsFactors = FALSE)
    colnames(new_row) <- names(current_db)
    
    update_db(rbind(current_db, new_row))
    
    removeModal()
    
  })
  
  # ---------- ADD COLUMN ----------
  
  observeEvent(input$add_data,{
    
    showModal(
      modalDialog(
        title = "Datenkriterium hinzufügen",
        
        textInput("new_column_name", "Name of new column"),
        
        selectInput(
          "fill_option",
          "Choose how to fill the new column",
          choices = c("NA values","Random values","Alphabetical IDs")
        ),
        
        numericInput("min_random_value","Min Random",0),
        numericInput("max_random_value","Max Random",100),
        
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_add_data","Confirm")
        )
      )
    )
    
  })
  
  
  observeEvent(input$confirm_add_data,{
    
    current_db <- db()
    col_name <- input$new_column_name
    
    if(col_name == "" || col_name %in% names(current_db)){
      return()
    }
    
    n <- nrow(current_db)
    
    new_col <- switch(
      input$fill_option,
      
      "NA values" = rep(NA,n),
      
      "Random values" =
        sample(input$min_random_value:input$max_random_value, n, replace=TRUE),
      
      "Alphabetical IDs" =
        generate_alpha_ids(n)
    )
    
    current_db[[col_name]] <- new_col
    
    update_db(current_db)
    
    removeModal()
    
  })
  
  # ---------- GENERATE RANDOM DATA ----------
  
  observeEvent(input$generate_data,{
    
    showModal(
      modalDialog(
        title = "Generate Random Dataset",
        
        numericInput("num_entries","Number of entries", value=10, min=1),
        
        lapply(names(db()), function(col){
          
          selectInput(
            paste0("fill_option_",col),
            paste("Fill column", col),
            choices=c("NA values","Random values","Alphabetical IDs")
          )
          
        }),
        
        footer=tagList(
          modalButton("Cancel"),
          actionButton("confirm_generate_data","Confirm")
        )
      )
    )
    
  })
  
  
  observeEvent(input$confirm_generate_data,{
    
    n <- input$num_entries
    cols <- names(db())
    
    new_df <- data.frame(matrix(nrow=n, ncol=length(cols)))
    colnames(new_df) <- cols
    
    for(col in cols){
      
      option <- input[[paste0("fill_option_",col)]]
      
      new_df[[col]] <- switch(
        option,
        
        "NA values" = rep(NA,n),
        
        "Random values" = sample(1:100,n,replace=TRUE),
        
        "Alphabetical IDs" = generate_alpha_ids(n)
      )
      
    }
    
    update_db(new_df)
    
    removeModal()
    
  })
  
}