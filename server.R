

function(input, output, session) {
  
  donnees <- reactive({
    Delai_rdv_agreg[specialiste == input$choix_practicien]
  })

  carte_practiciens <- renderPlotly({
    p = ggplot() +
      geom_polygon(
        data = carte_france,
        aes(x = long,
            y = lat,
            group = group),
        fill = "grey70",
        alpha = 0.7
      ) +
      geom_point(
        data = donnees(),
        aes(
          x = long,
          y = lat,
          color = delai_moy_fct,
          size = depassement_moy_pct,
          text = mytext
        )
      ) +
      scale_radius(name = "Depassement moy.en en %",
                   range = c(2, 14)) +
      scale_color_brewer(palette = "RdPu")+
      # scale_color_continuous(
      #   name = "Délai moy. en jours",
      #   # breaks = NULL,
      #   limits = c(0, 365),
      #   low = "maroon",
      #   high = "black",
      #   space = "Lab",
      #   guide = "colourbar"
      # ) +
      guides(size = guide_legend(override.aes = list(colour = "grey70")),
             shape = guide_legend(override.aes = list(colour = "grey70"))) +
      theme_void() +  
      coord_map() +
      theme(legend.position = "none")
    
    ggplotly(p, tooltip="text")}
  )
  
}

