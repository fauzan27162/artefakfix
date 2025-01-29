function(input, output, session) {
  session$allowReconnect(TRUE)
  # Output untuk Data Table
  output$dataT <- renderDataTable({
    datatable(my_data, options = list(scrollX = TRUE))
  })
  
  # Rendering Header Tertinggi dan Terendah
  output$head1 <- renderText({
    paste("5 Kabupaten/Kota dengan", input$var2, "Tertinggi")
  })
  
  output$head2 <- renderText({
    paste("5 Kabupaten/Kota dengan", input$var2, "Terendah")
  })
  
  # Rendering tabel 5 kabupaten/kota dengan nilai tertinggi
  output$top5 <- renderTable({
    my_data %>% 
      select(Wilayah, input$var2) %>% 
      arrange(desc(get(input$var2))) %>% 
      head(5)
  })
  
  # Rendering tabel 5 kabupaten/kota dengan nilai terendah
  output$low5 <- renderTable({
    my_data %>% 
      select(Wilayah, input$var2) %>% 
      arrange(get(input$var2)) %>% 
      head(5)
  })
  
  # Output untuk Struktur Data
  output$structure <- renderPrint({
    str(my_data)
  })
  
  # Output untuk Ringkasan Data
  output$summary <- renderPrint({
    summary(my_data)
  })
  
  # Output untuk Histogram
  output$histplot <- renderPlotly({
    p1 <- my_data %>% 
      plot_ly() %>% 
      add_histogram(x = ~get(input$var1)) %>% 
      layout(xaxis = list(title = input$var1))
    
    p2 <- my_data %>%
      plot_ly() %>%
      add_boxplot(x = ~get(input$var1)) %>% 
      layout(yaxis = list(showticklabels = FALSE))
    
    # Gabungkan histogram dan boxplot
    subplot(p2, p1, nrows = 2, shareX = TRUE) %>%
      hide_legend() %>% 
      layout(title = "Grafik Distribusi: Histogram dan Boxplot",
             yaxis = list(title = "Frekuensi"))
  })
  
  # Output untuk Bar Chart
  output$bar <- renderPlotly({
    my_data %>% 
      plot_ly() %>% 
      add_bars(x = ~Wilayah, y = ~get(input$var2)) %>% 
      layout(title = paste("Kabupaten/Kota dengan", input$var2),
             xaxis = list(title = "Kabupaten/Kota"),
             yaxis = list(title = input$var2))
  })
  
  # Output untuk Scatterplot
  output$scatter <- renderPlotly({
    p <- ggplot(my_data, aes(x = get(input$var3), y = get(input$var4))) +
      geom_point() +
      geom_smooth(method = input$fit) +
      labs(
        title = paste("Hubungan antara", input$var3, "dan", input$var4),
        x = input$var3,
        y = input$var4
      ) +
      theme(plot.title = element_textbox_simple(size = 10, halign = 0.5))
    
    ggplotly(p)
  })
  
  # Output untuk Correlation Plot
  output$cor <- renderPlotly({
    my_df <- my_data %>% 
      select(-Wilayah, -Provinsi, -Klaster, -CO, -HCHO, -AOD, -AKB, -AKB2020, -Jumlah_Kematian, -Jumlah_Pneumonia, -Pneumonia_Rate)  # Exclude non-numeric columns sesuai kebutuhan
    
    corr <- cor(my_df, use = "complete.obs")
    corr.plot <- ggcorrplot(corr, lab = TRUE, outline.col = "white")
    
    ggplotly(corr.plot)
  })
  
  
  # Output untuk Klaster Map Plot
  output$map_plot <- renderLeaflet({
    # Membuat custom popup
    popupLabel <- paste0(
      "<b>", final_data$Wilayah, "</b><br/>", 
      "AKB: ", final_data$AKB, "<br/>", 
      "Pneumonia Rate: ", final_data$Pneumonia_Rate, "<br/>", 
      "Jumlah Kematian Bayi: ", final_data$Jumlah_Kematian, "<br/>", 
      "Jumlah Balita Pneumonia: ", final_data$Jumlah_Pneumonia, "<br/>"
    ) %>%
      lapply(htmltools::HTML)
    
    # Membuat peta leaflet
    leaflet(final_data) %>% 
      addProviderTiles(providers$CartoDB.PositronNoLabels, group = "Light Mode") %>%
      addProviderTiles(providers$CartoDB.DarkMatterNoLabels, group = "Dark Mode") %>%
      
      addPolygons(weight = 0.8,
                  color = "black",  # Warna garis batas antar wilayah
                  opacity = 1, 
                  fillOpacity = 0.8,
                  label = popupLabel,
                  fillColor = ~colorPalette(Klaster),  # Warna sesuai urutan kategori
                  highlightOptions = highlightOptions(color = "white", 
                                                      weight = 3, 
                                                      bringToFront = TRUE)) %>%
      addLegend(position = "bottomright", 
                pal = colorPalette, 
                values = ~Klaster,  # Menggunakan variabel kategori
                title = "Klaster",
                opacity = 1) %>%
      
      addLayersControl(position = 'topright',
                       baseGroups = c("Light Mode", "Dark Mode"),
                       options = layersControlOptions(collapsed = FALSE))
  })

  
  output$downloadMap <- downloadHandler(
    filename = function() {
      paste("map_klaster", ".png", sep = "")
    },
    content = function(file) {
      # Membuat peta menggunakan ggplot
      gg_map <- ggplot() +
        geom_sf(data = final_data, aes(fill = Klaster), color = "black", size = 0.2) +
        scale_fill_manual(values = c("Sangat Rendah" = "#f7fbff", 
                                     "Cukup Rendah" = "#c6dbef", 
                                     "Cukup Tinggi" = "#6baed6", 
                                     "Sangat Tinggi" = "#3182bd")) +
        labs(fill = "Klaster", 
             title = "Peta Klaster berdasarkan Wilayah") +
        theme_minimal() +
        theme(
          legend.position = "bottom",  # Letakkan legend di bawah
          panel.grid = element_blank(),  # Menghilangkan grid
          axis.text = element_blank(),   # Menghilangkan teks axis
          axis.title = element_blank()   # Menghilangkan judul axis
        )
      
      # Simpan peta sebagai PNG
      ggsave(file, plot = gg_map, width = 10, height = 8, dpi = 300)
    }
  )
  
  
  
  
  
  
  
  
  
  # Choropleth map
  output$mapvar <- renderPlot({
       # Definisikan palet warna kontinu menggunakan warna dari RColorBrewer
    colorPalette <- RColorBrewer::brewer.pal(4, "YlGnBu") # Palet dengan 4 warna
    
    ggplot(final_data) +
      geom_sf(aes(fill = get(input$airpollution)), color = "black", size = 0.4) + # Gunakan geom_sf()
      scale_fill_gradientn(
        colors = colorPalette, # Palet warna kontinu
        name = paste(input$airpollution) # Nama legenda
      )  +
      theme_void()  +
      theme(
        plot.title = element_textbox_simple(
          face = "bold",
          size = 18,
          halign = 0.5
        ),
        legend.position.inside = c(0.2, 0.1),
        legend.direction = "horizontal"
      )
  })
  
  output$download_map <- downloadHandler(
    filename = function() {
      paste("choropleth_map_", input$airpollution, ".png", sep = "")
    },
    content = function(file) {
      
      # Palet warna kontinu
      colorPalette <- RColorBrewer::brewer.pal(4, "YlGnBu")
      
      # Membuat ggplot
      p <- ggplot(final_data) +
        geom_sf(aes(fill = get(input$airpollution)), color = "black", size = 0.4) +
        scale_fill_gradientn(
          colors = colorPalette,
          name = paste(input$airpollution)
        ) +
        theme_void() +
        theme(
          plot.title = element_textbox_simple(
            face = "bold",
            size = 18,
            halign = 0.5
          ),
          legend.position = "bottom"
        )
      
      # Simpan plot ke file
      ggsave(file, plot = p, width = 10, height = 7, dpi = 300)
    }
  )
  
  
  # Fungsi unduh data
  output$downloadData <- downloadHandler(
    filename = function() { 
      paste("data.csv") 
    },
    content = function(file) {
      write.csv(my_data, file, row.names = FALSE)
    }
  )
  
  # Fungsi unduh struktur
  output$downloadStructure <- downloadHandler(
    filename = function() { 
      paste("struktur.txt") 
    },
    content = function(file) {
      writeLines(capture.output(str(my_data)), file)
    }
  )
  
  # Fungsi unduh ringkasan
  output$downloadSummary <- downloadHandler(
    filename = function() { 
      paste("ringkasan.txt") 
    },
    content = function(file) {
      writeLines(capture.output(summary(my_data)), file)
    }
  )
}
  
