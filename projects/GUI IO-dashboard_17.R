library(shiny) #untuk membuat Gui
library(readxl) #untuk membaca file excel
library(leontief) #untuk melakukan analisis input output
library(shinydashboard) #tamplate tampilan ui gui dashboard

ui <- dashboardPage( #membuat halaman utama dari aplikasi dashboard
  dashboardHeader(title = "Analisis Input-Output Dalam Menentukan Sektor Unggulan", #membuat header dari dashboard
  titleWidth = 300,  # Menentukan lebar judul
  tags$li(class = "dropdown", style = "padding-top: 20px;", "By Dila Kiranti")  # Menambahkan informasi "By: Dila Kiranti" di bawah judul
),
  dashboardSidebar( #membuat sidebar dari dashboard
    sidebarMenu( 
      menuItem("Home", tabName = "home"),
      menuItem("Import Data", tabName = "import_data"),
      menuItem("Sektor Unggulan", tabName = "sektor_unggulan"),
      menuItem("Angka Pengganda", tabName = "angka_pengganda")
    )
  ),
  dashboardBody(
    tabItems(
      # Home Tab
      tabItem(tabName = "home",
              fluidPage(
                titlePanel("Selamat datang di Aplikasi Analisis Input-Output"),
                fluidRow(
                  column(12, h3("Petunjuk Penggunaan GUI R")),
                  column(12, p("Aplikasi ini menyediakan antarmuka grafis pengguna (GUI) untuk melakukan analisis input-output.")), #12 bahwa konten akan memenuhi lebar penuh baris, h3(menambah elemen teks), p(paragraf)
                  column(12, p("Petunjuk Penggunaan GUI Analisisi Input-Output:")),
                  column(12, p("1. klik tab 'Import Data' untuk mengunggah data transaksi antar sektor dan input total")),
                  column(12, p("2. tab 'Sektor Unggulan' menyajikan informasi tentang  analisis sektor unggulan")),
                  column(12, p("3. Tab 'Angka Pengganda' menyajikan informasi tentang angka pengganda output.")),
                  column(12, p("Selamat menggunakan aplikasi"))
                )
              )
      ),
      # Import Data Tab
      tabItem(tabName = "import_data",
              fluidPage(
                titlePanel("Import Data"),
                sidebarLayout(
                  sidebarPanel(
                    fileInput("file1", "Transaksi Antar Sektor", accept = ".xlsx"),
                    fileInput("file2", "Input Total Sektor", accept = ".xlsx")
                  ),
                  mainPanel(
                    tabsetPanel(
                      tabPanel("Data 1", dataTableOutput("table1")),
                      tabPanel("Data 2", dataTableOutput("table2"))
                    )
                  )
                )
              )
      ),
      
      # Sektor Unggulan Tab
      tabItem(tabName = "sektor_unggulan",
              fluidPage(
                h6("Matriks Koefisien Input"),
                verbatimTextOutput("Matriks_Koefisien_Input"),
                h6 ("Matriks Kebalikan Leontif"),
                verbatimTextOutput("Matriks_Kebalikan_Leontif"),
                h6("Angka Keterkaitan"),
                verbatimTextOutput("Angka_Keterkaitan"),
                h6("Direct Backward Linkage"),
                verbatimTextOutput("DB"),
                h6("Indirect Forward Linkage"),
                verbatimTextOutput("IB"),
                h6("Total Backward Linkage"),
                verbatimTextOutput("TB"),
                h6("Direct Forward Linkage"),
                verbatimTextOutput("DF"),
                h6("Indirect Forward Linkage"),
                verbatimTextOutput("IF"),
                h6("Total Forward Linkage"),
                verbatimTextOutput("TF"),
                h6("Angka Penyebaran"),
                verbatimTextOutput("Angka_Penyebaran"),
                h6("Sektor Unggulan"),
                verbatimTextOutput("Sektor_unggulan")
              )
      ),
      
      # Angka Penggandaan Tab
      tabItem(tabName = "angka_pengganda",
              fluidPage(
                h6("Angka Pengganda Output"),
                verbatimTextOutput("Angka_Penggandaan_Output")
              )
      )
    )
  )
)

# Mendefinisikan server
#Fungsi ini akan menerima input dari elemen-elemen UI yang telah didefinisikan sebelumnya dan menghasilkan output yang akan ditampilkan kepada pengguna.
server <- function(input, output) {
  #membaca data dari file yang diunggah
  read_data <- function(file) {
    if (is.null(file)) #memastikan data memiliki null atau tidak
      return(NULL)
    
    readxl::read_xlsx(file$datapath) #membaca file xlsx dan path(melihat lokasi data)
  }
  
  # Reaktif data akan dijalankan ulang secara otomatis setiap kali input yang digunakan di dalamnya berubah.
  data1 <- reactive({ 
    read_data(input$file1)
  })
  
  data2 <- reactive({
    read_data(input$file2)
  })
  
  # Render tabel untuk menampilkan data yang telah terupload
  output$table1 <- renderDataTable({
    data1()
  })
  
  output$table2 <- renderDataTable({
    data2()
  })

  
  observe({
    req(data1(), data2())  
    
    # Mengubah data yang terupload menjadi matriks
    mat_transaksi_antara <- as.matrix(data1())
    mat_input_total <- as.matrix(data2())
    
    mat_koef_input <- matrix(NA, nrow = nrow(mat_transaksi_antara), ncol = ncol(mat_transaksi_antara))
    
    for (j in 1:ncol(mat_transaksi_antara)) {
      mat_koef_input[, j] <- mat_transaksi_antara[, j] / mat_input_total[1, j]
    }
    rownames(mat_koef_input) <- colnames(mat_transaksi_antara)
    colnames(mat_koef_input) <- colnames(mat_transaksi_antara)
    
    #Matriks Kebalikan Leontif
    L_input <- leontief_inverse(mat_koef_input); 
    colnames(L_input)<-colnames(mat_transaksi_antara)
    rownames(L_input)<-colnames(mat_transaksi_antara)
    
    #Analisis Keterkaitan
    DBL <- backward_linkage(mat_koef_input)
    # Mendapatkan urutan hasil perhitungan secara menurun
    indeks_urutan_DB <- order(DBL, decreasing = TRUE) 
    # Mengurutkan hasil perhitungan berdasarkan urutan
    DBL_terurut <- DBL[indeks_urutan_DB,, drop = FALSE ] #drop FALSE berfungsi untuk membuat hasil pengurutan tetap matriks
    # Menyusun nama baris sesuai dengan hasil pengurutan
    nama_baris_DB <- rownames(L_input)[indeks_urutan_DB]
    rownames(DBL_terurut) <- nama_baris_DB
    colnames(DBL_terurut) <- c("Direct Backward Linkage")
    
    TBL <- backward_linkage(L_input)
    # Mendapatkan urutan hasil perhitungan secara menurun
    indeks_urutan_TB <- order(TBL, decreasing = TRUE)
    # Mengurutkan hasil perhitungan berdasarkan urutan
    TBL_terurut <- TBL[indeks_urutan_TB,, drop = FALSE ]
    # Menyusun nama baris sesuai dengan hasil pengurutan
    nama_baris_TB <- rownames(L_input)[indeks_urutan_TB]
    rownames(TBL_terurut) <- nama_baris_TB
    colnames(TBL_terurut) <- c("Total Backward Linkage") 
    
    IBL <- TBL-DBL
    # Mendapatkan urutan hasil perhitungan secara menurun
    indeks_urutan_IB <- order(IBL, decreasing = TRUE)
    # Mengurutkan hasil perhitungan berdasarkan urutan
    IBL_terurut <- IBL[indeks_urutan_IB,, drop = FALSE ]
    # Menyusun nama baris sesuai dengan hasil pengurutan
    nama_baris_IB <- rownames(L_input)[indeks_urutan_DB]
    rownames(IBL_terurut) <- nama_baris_IB
    colnames(IBL_terurut) <- c("Indirect Backward Linkage") 
    
    DFL <- forward_linkage(mat_koef_input)
    # Mendapatkan urutan hasil perhitungan secara menurun
    indeks_urutan_DF <- order(DFL, decreasing = TRUE)
    # Mengurutkan hasil perhitungan berdasarkan urutan
    DFL_terurut <- DFL[indeks_urutan_DF,, drop = FALSE ]
    # Menyusun nama baris sesuai dengan hasil pengurutan
    nama_baris_DF <- rownames(L_input)[indeks_urutan_DF]
    rownames(DFL_terurut) <- nama_baris_DF
    colnames(DFL_terurut) <- c("Direct Forward Linkage") 
    
    TFL<- forward_linkage(L_input)
    # Mendapatkan urutan hasil perhitungan secara menurun
    indeks_urutan_TF <- order(TFL, decreasing = TRUE)
    # Mengurutkan hasil perhitungan berdasarkan urutan
    TFL_terurut <- TFL[indeks_urutan_TF,, drop = FALSE ]
    # Menyusun nama baris sesuai dengan hasil pengurutan
    nama_baris_TF <- rownames(L_input)[indeks_urutan_TF]
    rownames(TFL_terurut) <- nama_baris_TF
    colnames(TFL_terurut) <- c("Total Forward Linkage") 
    
    IFL<-TFL-DFL
    # Mendapatkan urutan hasil perhitungan secara menurun
    indeks_urutan_IF <- order(IFL, decreasing = TRUE)
    # Mengurutkan hasil perhitungan berdasarkan urutan
    IFL_terurut <- IFL[indeks_urutan_IF,, drop = FALSE ]
    # Menyusun nama baris sesuai dengan hasil pengurutan
    nama_baris_IF <- rownames(L_input)[indeks_urutan_IF]
    rownames(IFL_terurut) <- nama_baris_IF
    colnames(IFL_terurut) <- c("Indirect Forward Linkage") 
    
    BFL <- cbind(DBL,IBL, TBL, DFL, IFL, TFL)
    rownames(BFL) <- rownames(L_input)
    colnames(BFL) <- c("Direct Backward Linkage", "Indirect Backward Linkage", "Total Backward Linkage", "Direct Forward Linkage", "Indirect Forward Linkage", "Total Forward Linkage")
    
    #Analisis Penggandaan
    out <- output_multiplier(L_input)
    rownames(out) <- rownames(L_input)
    colnames(out) <- c("Angka Penggandaan Output")
    
    #Indeks Penyebaran
    IDP<- power_dispersion(L_input)
    rownames(IDP) <- rownames(mat_transaksi_antara)
    colnames(IDP) <- c("Indeks Daya Penyebaran")
    
    #Indeks Derajat Kepekaan
    IDK <- sensitivity_dispersion(L_input)
    rownames(IDK) <- rownames(mat_transaksi_antara)
    colnames(IDK) <- c("Indeks Derajat Kepekaan")
    
    Analisis_Penyebaran <- round(cbind(IDK,IDP),2) #round untuk membulatkan angka
    rownames(Analisis_Penyebaran) <- rownames(L_input)
    colnames(Analisis_Penyebaran) <- c("Indeks Derajat Kepekaan", "Indeks Daya Penyebaran")
    
    sektor_unggulan <- rownames(L_input)[IDP > 1 & IDK > 1]
    
    # Display the results
    output$Matriks_Koefisien_Input <- renderPrint({
      mat_koef_input
    })
    
    output$Matriks_Kebalikan_Leontif <- renderPrint({
      L_input
    })
    
    output$Angka_Keterkaitan <- renderPrint({
      BFL
    })
    
    output$DF <- renderPrint({
      DFL_terurut
    })
    
    output$TF <- renderPrint({
      TFL_terurut
    })
    
    output$IF <- renderPrint({
      IFL_terurut
    })
    
    output$DB <- renderPrint({
      DBL_terurut
    })
    
    output$TB <- renderPrint({
      TBL_terurut
    })
    
    output$IB <- renderPrint({
      IBL_terurut
    })
    
    output$Angka_Penyebaran <- renderPrint({
      Analisis_Penyebaran
    })
    
    output$Angka_Penggandaan_Output <- renderPrint({
      out
    })

    output$Sektor_unggulan<- renderPrint({
      sektor_unggulan
    })
  })
}
shinyApp(ui, server)

