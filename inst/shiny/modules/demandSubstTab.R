
## Demand !----
output$demandBox <- renderValueBox({

  # validate(
  #   need(all(sapply(seq_along(Imp_ordered()$LevCount),
  #                   function(x) {
  #                     IDselected <- paste0("decMat", x, "_columns_selected")
  #                     !is.null(input[[IDselected]])
  #                   })),
  #        "")
  # )
  #
  # demand <- sum(sapply(seq_along(Imp_ordered()$LevCount),
  #                      function(x) {
  #                        IDselected <- paste0("decMat", x, "_columns_selected")
  #                        Imp_ordered()$LevCount[[x]][input[[IDselected]] + 1]
  #                      }))
  #
  # valueBox(value = round(demand, 2),
  #          subtitle = "Demand",
  #          color = "green",
  #          icon = icon("heart-o"))

  valueBox(value = 4.2,
           subtitle = "Demand of selected product",
           color = "purple",
           icon = icon("heart-o"))
})

## Number of competitor products !----
output$compBox <- renderValueBox({

  # validate(TRUE, "")

  nComp <- 42

  valueBox(value = nComp,
           subtitle = "Number of competitor products",
           color = "aqua",
           icon = icon("bolt"))
})


## Supply !----
output$supplyBox <- renderValueBox({

  validate(
    need(all(sapply(seq_along(Imp_ordered()$LevCount),
                    function(x) {
                      IDselected <- paste0("decMat", x, "_columns_selected")
                      !is.null(input[[IDselected]])
                    })),
         "Please select a combination across all attributes.")
  )

  # supply <- sum(sapply(seq_along(Imp_ordered()$LevCount),
  #                      function(x) {
  #                        IDselected <- paste0("decMat", x, "_columns_selected")
  #                        Supply()$supply[[x]][input[[IDselected]] + 1]
  #                      }))

  valueBox(value = round(42.42, 2),
           subtitle = "Supply",
           color = "lime",
           icon = icon("shopping-basket"))
})


## Incrementality !----
output$incrementBox <- renderValueBox({

  validate(
    need(all(sapply(seq_along(Imp_ordered()$LevCount),
                    function(x) {
                      IDselected <- paste0("decMat", x, "_columns_selected")
                      !is.null(input[[IDselected]])
                    })),
         "")
  )

  increment <- sum(sapply(seq_along(Imp_ordered()$LevCount),
                          function(x) {
                            IDselected <- paste0("decMat", x, "_columns_selected")
                            Imp_ordered()$LevCount[[x]][input[[IDselected]] + 1]
                          }
  )) - 42.42

  valueBox(value = round(increment, 2),
           subtitle = "Incrementality",
           color = "purple",
           icon = icon("star-o"))
})
