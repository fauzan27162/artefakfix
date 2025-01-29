## Shiny UI component for the Dashboard

dashboardPage(
  
  dashboardHeader(
    titleWidth = 750, 
    # Directly apply the font style to the title using style argument
    title = "Dashboard Asosiasi Polusi Udara dengan Kematian Bayi & Pneumonia Balita",
    # Dropdown items
    tags$li(class = "dropdown", tags$a(href = "https://mail.google.com/mail/?view=cm&to=222112212@gmail.com", icon("envelope"), "My Email", target = "_blank"))
    ),
  dashboardSidebar(
    sidebarMenu(id = "sidebar",
                menuItem("Dataset", tabName = "data", icon = icon("database")),
                menuItem("Visualisasi", tabName = "viz", icon=icon("chart-line")),
                # Conditional Panel for conditional widget appearance
                # Filter should appear only for the visualization menu and selected tabs within it
                conditionalPanel("input.sidebar == 'viz' && input.t2 == 'distro'", selectInput(inputId = "var1" , label ="Pilih Variabel Polusi Udara" , choices = c1)),
                conditionalPanel("input.sidebar == 'viz' && input.t2 == 'trends' ", selectInput(inputId = "var2" , label ="Pilih Variabel Polusi Udara" , choices = c1)),
                conditionalPanel("input.sidebar == 'viz' && input.t2 == 'relation' ", selectInput(inputId = "var3" , label ="Pilih Variabel X" , choices = c1, selected = "CO")),
                conditionalPanel("input.sidebar == 'viz' && input.t2 == 'relation' ", selectInput(inputId = "var4" , label ="Pilih Variabel Y" , choices = c1, selected = "HCHO")),
                menuItem("Choropleth Map", tabName = "map", icon=icon("map")),
                menuItem("Peta Klaster", tabName = "mapklaster", icon=icon("project-diagram"))
                
    )
  ),
  
  
  dashboardBody(
    # Tambahkan CSS di sini
    tags$head(
      tags$style(HTML("
        /* Ubah warna sidebar */
    .main-sidebar {
      background-color: #002B5B !important; /* Warna hijau */
      color: white !important;
    }

    /* Ubah warna text pada sidebar */
    .main-sidebar .sidebar a {
      color: white !important;
    }

    /* Ubah warna hover text pada sidebar */
    .main-sidebar .sidebar a:hover {
      color: #FFC107 !important; /* Warna kuning */
    }

    /* Ubah warna headbar */
    .main-header .navbar {
      background-color: #2E3B55 !important; /* Warna biru gelap */
    }

    /* Ubah warna teks headbar */
    .main-header .logo,
    .main-header .navbar .navbar-nav > li > a {
      color: white !important;
    }

    /* Ubah warna hover teks headbar */
    .main-header .navbar .navbar-nav > li > a:hover {
      color: #FFD700 !important; /* Warna emas */
    }
        .content {
          height: 800px; /* Set a fixed height */
          overflow-y: scroll; /* Enable vertical scrolling */
        }
      "))
    ),
   
    tabItems(
      ## First tab item
      tabItem(
        tabName = "data",
        tabBox(
          id = "t1", width = 12,
          tabPanel(
            "Tentang Data", icon = icon("address-card"),
            fluidRow(
              # Gambar: Tambahkan margin bawah dan responsif
              column(
                width = 12, 
                class = "text-center",
                tags$img(
                  src = "airpollution2.jpg",
                  style = "max-width: 100%; height: auto; margin-bottom: 10;margin-top: 30px;" # Jarak ekstra di bawah gambar
                )
              ),
              # Deskripsi teks: Margin disesuaikan dengan lebar gambar
              column(
                width = 8, offset = 2, # Tengah dengan offset, lebar teks 8 kolom
                tags$p(
                  "Dataset penilaian kerentanan polusi udara ini diperoleh dari Google Earth Engine dan Data BPS. 
            Dengan rincian data SO2, NO2, O3, PM25, dan CH4 sebagai polutan paparan. Data curah hujan, LST, 
            kelembapan relatif, dan kecepatan angin sebagai data paparan faktor meteorologi. Terakhir, data rasio jumlah industri manufaktur dan kepadatan penduduk sebagai komponen sensitivitas. 
            Pembentukan klaster kerentanan dilakukan melalui perbandingan spatial fuzzy dan geoclust. 
            Kemudian hasil klaster terbaik diuji apakah memiliki hubungan dengan kematian bayi dan pneumonia balita. 
            Hasil menunjukkan variabel-variabel tersebut berhubungan secara signifikan. 
            Yang berarti, perlu ada tindakan dan perhatian khusus pada kesehatan bayi dan balita 
            di wilayah yang tergolong klaster dengan kerentanan polusi udara tinggi.",
                  style = "text-align: justify; margin-top: 20px;" # Tambahkan jarak atas
                )
              )
            )
          ),
          
          tabPanel(
            "Data", 
            dataTableOutput("dataT"), 
            icon = icon("table"),
            # Tombol unduh untuk data
            downloadButton("downloadData", "Unduh Data")
          ),
          tabPanel(
            "Struktur", 
            verbatimTextOutput("structure"), 
            icon = icon("uncharted"),
            # Tombol unduh untuk struktur
            downloadButton("downloadStructure", "Unduh Struktur")
          ),
          tabPanel(
            "Ringkasan Statistik", 
            verbatimTextOutput("summary"), 
            icon = icon("chart-pie"),
            # Tombol unduh untuk ringkasan statistik
            downloadButton("downloadSummary", "Unduh Ringkasan")
          )
        )
      ),  
      
      # Second Tab Item
      tabItem(tabName = "viz", 
              tabBox(id="t2",  width=12, 
                     tabPanel("Trend Variabel Polusi Wilayah", value="trends",
                              fluidRow(tags$div(align="center", box(tableOutput("top5"), title = textOutput("head1") , collapsible = TRUE, status = "primary",  collapsed = TRUE, solidHeader = TRUE)),
                                       tags$div(align="center", box(tableOutput("low5"), title = textOutput("head2") , collapsible = TRUE, status = "primary",  collapsed = TRUE, solidHeader = TRUE))
                                       
                              ),
                              withSpinner(plotlyOutput("bar"))
                     ),
                     tabPanel("Distribusi", value="distro",
                              # selectInput("var", "Select the variable", choices=c("CO", "HCHO")),
                              withSpinner(plotlyOutput("histplot", height = "350px"))),
                     tabPanel("Matriks Korelasi", id="corr", 
                              withSpinner(plotlyOutput("cor", height = "600px", width = "100%"))
                     ),
                     tabPanel("Scatter Plor 2 Variabel", 
                              radioButtons(inputId ="fit" , label = "Select smooth method" , choices = c("loess", "lm"), selected = "lm" , inline = TRUE), 
                              withSpinner(plotlyOutput("scatter")), value="relation"),
                     side = "left"
              ),
              
      ),
      
     
      # Fourth Tab Item
      tabItem(
        tabName = "mapklaster",
        fluidRow(
          box(
            width = 12,
            tags$p(
              "Pemetaan Klaster Kerentanan Polusi Udara di Pulau Jawa Tahun 2023",
              style = "font-size: 20px; color: #555; text-align: center; margin-top: 10px;"
            ),
            tags$p(
              "Gunakan peta interaktif ini untuk menjelajahi klaster kerentanan polusi udara. Klik pada wilayah untuk informasi lebih lanjut.",
              style = "font-size: 14px; color: #555; text-align: center; margin-top: 10px;"
            ),
            leafletOutput("map_plot", height = "600px"),  # Output untuk peta Leaflet
            
            # Tombol untuk unduh peta klaster dalam format PNG
            downloadButton(outputId = "downloadMap", label = "Unduh Peta", style = "color: white; background-color: #2E3B55; margin-top: 10px;")
          )
        )
      ),
      
      # Third Tab Item
      tabItem(
        tabName = "map",
        box(
          tags$p(
            "Sebaran Variabel Penyusun Polusi Udara di Pulau Jawa Tahun 2023",
            style = "font-size: 20px; color: #555; text-align: center; margin-top: 10px;"
          ),
          selectInput("airpollution", "Pilih Variabel", choices = c1, selected = "CO", width = 250),
          withSpinner(plotOutput("mapvar")),
          downloadButton("download_map", "Unduh Gambar", class = "btn-primary", style = "color: white;"), # Tombol unduh
          width = 12
        )
      )
      
      
      
    )
  )
)

