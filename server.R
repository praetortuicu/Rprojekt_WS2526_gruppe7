library(shiny)


server <- function(input, output, session){
  
  
  # ---------- DATENBANK ----------
  
  db <- reactiveVal(
    data.frame(
      Node=c("A","B","C"),
      Value=c(10,20,30)
      #leave empty at beginning - fill with add entry button or generate data button
    )
  )
  
  
  output$database_table <- renderTable({
    db()
  })
  
  
  # ---------- OUTPUTS ----------
  
  output$selected_algo <- renderText({
    paste("Algorithm: DummyAlgo") #open dropdown to choose algo and display selected algo as text on button
  })
  
  output$depth <- renderText({
    paste("Depth:", 4) # only render after Tree - get these values from tree object
  })
  
  output$time <- renderText({
    paste("Time:", "0.01s") # only render after Tree - get these values from tree object
  })
  
  output$leafs <- renderText({
    paste("Leafs:", 12) # only render after Tree - get these values from tree object
  })
  
  
  # ---------- BUTTONS----------
  
  observeEvent(input$draw_tree,{
    
    #do check for any NA in db and any data in db and input$algo != "" || Choose Algorithm - if any of these is not fulfilled, show error message and do not show confirm button - ERROR es fehlen die Werte in R
    if(nrow(db()) == 0 || any(is.na(db())) || input$algo == "Choose Algorithm"){
      showModal(
        modalDialog(
          title = "Error",
          p("Die aktuellen Einstellungen für die Baumgenerierung sind ungültig: "),
          p("Es fehlt entweder ein gültiger Algorithmus oder die Daten sind unvollständig"),
          footer = modalButton("Close")
        )
      )
      return()
    }else{
      showModal(
        modalDialog(
          title = "Baum generieren",
          
          
          p("Dies sind die aktuellen Einstellungen für die Baumgenerierung:"),
          
          p(("Algorithmus:"), input$algo), # input und nrow geben bisher 0 - fixxen ERROR
          p(("Menge an Daten:"), nrow(db)),
          
          br(),
          
          p("Möchten Sie fortfahren?"),
          
          footer = tagList(
            modalButton("Cancel"),
            actionButton("confirm", "Confirm")
          )
        )
      )
    }
  })
  
  output$selected_algo <- renderText({
    paste("Selected:", input$algo)
  })
  
  observeEvent(input$add_entry,{
    
    #open window and allow text input for all columns of db, then add new row to db with input values
    showModal(
      modalDialog(
        title = "Eintrag hinzufügen",
        
        #pro column in db a text input for the value of the new entry
        
        
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm", "Confirm")
        )
      )
    )
    
  })
  
  observeEvent(input$add_data,{
    
    #open window and allow text input for column name, then add new column to db with input name and NA values
    showModal(
      modalDialog(
        title = "Datenkriterium hinzufügen",
        
        #show db
        output$database_table <- renderTable({
          db()
        }),
        
        # below text input for column name, then add new column to db with input name
        
        
        # give option to fill new column with random values or NA values + ein numerical input for pivots, o. ä.
        
        
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm", "Confirm")
        )
      )
    )
    
  })
  
  
  observeEvent(input$generate_data,{
    
    #open window and aks to choose settings then generate based on number of elems asked
    showModal(
      modalDialog(
        title = "Einstellungen für Datengenerierung",
        
        # pro column fragen wie zufallswerte generiert werden sollen per dropdown + ein numerical input für pivots o. ä.
        
        
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm", "Confirm")
        )
      )
    )
    
  })
  
}