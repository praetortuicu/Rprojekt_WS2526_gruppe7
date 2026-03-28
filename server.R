library(shiny)
library(tictoc)
library(jsonlite)

server <- function(input, output, session){
  
  # ---------- DATENBANK ----------
  
  db <- reactiveVal(
    data.frame(
      Entry_ID = c("A","B","C"),
      target = c(TRUE, FALSE, TRUE),
      stringsAsFactors = FALSE
    )
  )
  
  update_db <- function(new_db){
    db(new_db)
  }
  
  reset_database <- function(db_reactive){
    db_reactive(data.frame())
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
  
  depth <- reactiveVal(0)
  output$depth <- renderText({
    paste("Depth:", depth())
  })
  timer <- reactiveVal(0)
  output$time <- renderText({
    paste("Time:", timer(), "seconds")
  })
  num_leafs <- reactiveVal(0)
  output$leafs <- renderText({
    paste("Leafs:", num_leafs())
  })
  
  # ---------- CONDITIONAL INPUTS ----------
  
  
  
  # ---------- DRAW TREE ----------
  
  observeEvent(input$draw_tree,{
    
    if(nrow(db()) == 0 || any(is.na(db())) || input$choose_algo == "Choose Algorithm"){
      
      showModal(
        modalDialog(
          title = "Error",
          p("Current Settings for Tree Generation are not valid."),
          footer = modalButton("Close")
        )
      )
      
      return()
    }
    
    showModal(
      modalDialog(
        title = "Generate Tree",
        p("Algorithm:", input$choose_algo),
        p("Amount of Data Entries:", nrow(db())),
        br(),
        p("Do you wish to proceed?"),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_draw_tree", "Confirm")
        )
      )
    )
    
  })
  
  observeEvent(input$confirm_draw_tree, {
    
    removeModal()
    
    tryCatch({
      
      # Timer starten
      tic()
      
      # Model generieren abhängig vom Algorithmus
      model <- switch(
        input$choose_algo,
        "Greedy-Algorithm" = generate_greedy_cart_tree(db(), input$max_depth, input$min_leaf_size),
        "Cost Complexity Pruning" = generate_pruned_cart_tree(db(), input$max_depth, input$min_leaf_size, input$prune_level),
        "Bagging" = generate_bagging_forest(db(), input$n_trees, input$max_depth, input$min_leaf_size),
        "Random Forest" = generate_random_forest(db(), input$n_trees, input$max_depth, input$min_leaf_size),
        "Test" = generate_test_tree()
      )
      
      tree <- model
      
      if(input$choose_algo %in% c("Bagging","Random Forest")){
        tree_index <- input$tree_index
        tree <- get_tree(model, tree_index)
      }
      
      # Timer stoppen
      t <- toc(quiet = TRUE)
      timer(t$toc - t$tic)
      
      # Tree-Metadaten setzen
      depth(tree@ref$depth)
      num_leafs(tree@ref$n_leaves)
      
      # Layout berechnen
      layout <- computate_node_layout(tree)
      
      nodes_df <- data.frame(
        id = layout$nodes$node_id,
        parent_id = layout$nodes$parent_id,
        x = layout$nodes$x,
        y = layout$nodes$y,
        s_feature = layout$nodes$s_feature,
        s_value = layout$nodes$s_value,
        depth = layout$nodes$depth,
        is_leaf = layout$nodes$is_leaf
      )
      
      # Tree an Canvas senden
      session$sendCustomMessage(
        "draw_tree",
        list(
          nodes = nodes_df,
          width = ifelse(is.null(layout$canvas_width), 800, layout$canvas_width),
          height = layout$canvas_height
        )
      )
      
    }, error = function(e) {
      # Fehler abfangen und Modal anzeigen
      showModal(modalDialog(
        title = "Fehler beim Erstellen des Baums",
        paste("Es ist ein Fehler aufgetreten:", e$message),
        easyClose = TRUE,
        footer = NULL
      ))
    })
    
  })
  
  # ---------- ADD ENTRY ----------
  
  observeEvent(input$add_entry,{
    
    showModal(
      modalDialog(
        title = "Add Entry",
        
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
        title = "Add Data Column",
        
        textInput("new_column_name", "Name of new column"),
        
        selectInput(
          "fill_option",
          "Choose how to fill the new column",
          choices = c("NA values","Random values","Alphabetical IDs", "Boolean values")
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
        generate_alpha_ids(n),
      
      "Boolean values" =
        sample(c(TRUE, FALSE), n, replace=TRUE)
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
          
          if(col %in% c("Entry_ID")) return(NULL)
          
          if(col == "target"){
            
            return(tagList(
              
              selectInput(
                "fill_target",
                "Choose Target Type",
                choices = c("Boolean values", "Random values")
              ),
              
              numericInput(
                "min_target",
                "In case of numeric target: minimum value",
                value = 1
              ),
              
              numericInput(
                "max_target",
                "In case of numeric target: maximum value",
                value = 5
              )
              
            ))
            
          }
          
          selectInput(
            paste0("fill_option_", col),
            paste("Fill column", col),
            choices = c("NA values", "Random values", "Alphabetical IDs", "Boolean values")
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
    
    if(col == "Entry_ID"){
      new_df[[col]] <- generate_alpha_ids(n)
      next
    }
    
    if(col == "target"){
      
      option <- input$fill_target
      min_target <- input$min_target
      max_target <- input$max_target
      
      new_df[[col]] <- switch(
        option, 
        "Random values" = sample(min_target:max_target, n, replace = TRUE),
        "Boolean values" = sample(c(TRUE, FALSE), n, replace = TRUE)
      )
      
      next
    }
    
    option <- input[[paste0("fill_option_", col)]]
    
    new_df[[col]] <- switch(
      option,
      "NA values" = rep(NA, n),
      "Random values" = sample(1:100, n, replace = TRUE),
      "Alphabetical IDs" = generate_alpha_ids(n),
      "Boolean values" = sample(c(TRUE, FALSE), n, replace = TRUE)
    )
  }
  
  update_db(new_df)
  removeModal()
  
})

observeEvent(input$clear_data,{
  showModal(
    modalDialog(
      title = "You are about to delete your database",
      p("Do you wish to proceed?"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_clear_data", "Confirm")
      )
    )
  )
})

observeEvent(input$confirm_clear_data, {
  reset_database(db)
  removeModal()
})

observeEvent(input$load_csv, {
  showModal(
    modalDialog(
      title = "Load CSV Dataset",
      fileInput("csv_file", "Choose CSV File", accept = ".csv"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_load_csv", "Confirm")
      )
    )
  )
  
})

observeEvent(input$confirm_load_csv, {
  req(input$csv_file)
  
  tryCatch({
    new_db <- read.csv(
      input$csv_file$datapath,
      header = FALSE,              # erste Zeile ist Daten!
      stringsAsFactors = FALSE
    )
    
    # Spalte finden mit nur 1 und -1
    is_target_col <- sapply(new_db, function(col) {
      all(na.omit(col) %in% c(1, -1))
    })
    
    # Prüfen ob genau eine solche Spalte existiert
    if (sum(is_target_col) != 1) {
      stop("Es muss genau eine Target-Spalte mit nur 1 und -1 geben.")
    }
    
    # Target extrahieren und in Boolean umwandeln
    target <- new_db[[which(is_target_col)]]
    target_bool <- target == 1
    
    # Restliche Daten (Features)
    features <- new_db[, !is_target_col, drop = FALSE]
    
    # Spalten durchnummerieren
    colnames(features) <- paste0("V", seq_len(ncol(features)))
    
    # Finaler DataFrame
    final_db <- cbind(features, target = target_bool)
    
    # In deine "DB" laden
    update_db(final_db)
    removeModal()
  }, error = function(e) {
    showModal(modalDialog(
      title = "Fehler beim Laden der CSV-Datei",
      paste("Es ist ein Fehler aufgetreten:", e$message),
      easyClose = TRUE,
      footer = NULL
    ))
  })
})

}