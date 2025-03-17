library(shiny)

# Library

# 0. Packages -------------------------------------------------------------
library(tidyverse)
library(readxl)
library(scales)
library(dplyr)
library(ggtext)
library(ggiraph)
library(viridis)
library(gt)
library(DT)
library(ggplot2)
library(ggtext)
library(patchwork)
library(tibble)
library(grid)
library(plotly)
library(wordcloud2)
library(forcats)
library(paletteer)

# 1. Import data & manipulate-------------------------------------------------------------

data <- read_excel("C:\\Users\\asusr\\OneDrive\\Documents\\R-Projects\\EDA_Pembayaran-Digital\\Kuesioner-Pembayaran-Digital-2.xlsx")
worddata <- read_excel("C:\\Users\\asusr\\OneDrive\\Documents\\R-Projects\\EDA_Pembayaran-Digital\\Wordfreq.xlsx")
datacomment <- read_excel("C:\\Users\\asusr\\OneDrive\\Documents\\R-Projects\\EDA_Pembayaran-Digital\\datacomment.xlsx")

data2 <- data |>
  mutate(Kelompok_Usia = case_when(
    Kelompok_Usia == "Generasi Z (13 - 28 tahun, lahir 1997 - 2012)" ~ "Generasi_Z",
    Kelompok_Usia == "Generasi Milenial (Gen Y) (29 - 44 tahun, lahir 1981 - 1996)" ~ "Generasi_y",
    Kelompok_Usia == "Generasi X (45 - 60 tahun, lahir 1965 - 1980)" ~ "Generasi_X",
    Kelompok_Usia == "Baby Boomers (61 - 79 tahun, lahir 1946 - 1964)" ~ "Baby_Boomers",
    TRUE ~ Kelompok_Usia  # Sisanya tetap pakai nilai aslinya
  ), Jenis_Pekerjaan = case_when(
    Jenis_Pekerjaan == "Pegawai Negeri Sipil (PNS/TNI/Polri)" ~ "PNS",
    Jenis_Pekerjaan == "Pegawai Swasta" ~ "Pegawai-Swasta",
    Jenis_Pekerjaan == "Wirausaha/UMKM" ~ "Wirausaha/UMKM",
    Jenis_Pekerjaan == "Mahasiswa" ~ "Mahasiswa",
    Jenis_Pekerjaan == "Siswa (pelajar SD/SMP/SMA)" ~ "Siswa",
    Jenis_Pekerjaan == "Ibu rumah tangga" ~ "IRT",
    Jenis_Pekerjaan == "Pekerja informal (ojek online, pedagang kaki lima, dll.)" ~ "Informal",
    TRUE ~ "Other" # sisanya other
  ), Alasan_Menggunakan = case_when(
    Alasan_Menggunakan == "Praktis & cepat" ~ "Praktis-Cepat",
    Alasan_Menggunakan == "Banyak merchant yang menerima" ~ "Merchant-Menerima",
    Alasan_Menggunakan == "Keamanan transaksi" ~ "Keamanan",
    Alasan_Menggunakan == "Tidak memiliki e-wallet atau mobile banking" ~ "Tidak-ada-e-wallet",
    Alasan_Menggunakan == "Masalah teknis (misalnya: sinyal, error)" ~ "Masalah-teknis",
    TRUE ~ "Other"
  ), Metode_pembayaran = case_when(
    Metode_pembayaran == "QRIS" ~ "QRIS",
    Metode_pembayaran == "Transfer bank (mobile/internet banking)" ~ "Transfer-bank",
    Metode_pembayaran == "Kartu debit/kredit" ~ "Kartu-debit/kredit",
    Metode_pembayaran == "Tunai" ~ "Tunai",
    TRUE ~ "Other"
  ), Kendala_Qris = case_when(
    Kendala_Qris == "Kurangnya edukasi atau informasi" ~ "Kurang-edukasi",
    Kendala_Qris == "Jaringan internet tidak stabil" ~ "jaringan-kurang-stabil",
    Kendala_Qris == "Tidak semua tempat menerima QRIS" ~ "Tidak-semua-terima-QRIS",
    Kendala_Qris == "Kekhawatiran terhadap keamanan data" ~ "Khawatir-keamanan",
    TRUE ~ "Other"
  ), Metode_Bayar_PBB = str_replace_all(Metode_Bayar_PBB, " ", "-"),
  Tingkat_Efisiensi_QRIS = factor(Tingkat_Efisiensi_QRIS, 
                                  levels = c("Tidak membantu sama sekali", 
                                             "Tidak terlalu berpengaruh", 
                                             "Ya, tetapi masih ada kendala", 
                                             "Ya, sangat membantu"),
                                  labels = c(1, 2, 3, 4), 
                                  ordered = TRUE))  # <- ORDINAL buat nanti visualisasi kategorikal))


# 2. count group aka count() value------------------------------------------------
# cari Count biar liat ga ada observasi aneh

data2 |>
  count(Kelompok_Usia, sort = TRUE)

data2 |>
  count(Jenis_Pekerjaan, sort = TRUE)

data2 |>
  count(Alasan_Menggunakan, sort = TRUE)

data2 %>% 
  count(Metode_pembayaran, sort = TRUE)

data2 %>% 
  count(Tingkat_Efisiensi_QRIS, sort = TRUE)

data2 %>% 
  count(Kendala_Qris, sort = TRUE)

data2 %>% 
  count(Metode_Bayar_PBB, sort = TRUE)

data2 %>% 
  count(Pernah_Pakai_QRIS, sort = TRUE)

# udah clear semua ya

# 3. check tibble and summarize -------------------------------------------

data2
summary(data2)

# 4. Rename ------------------------
nama_usia <- c(
  "Generasi_Z" = "Gen Z",
  "Generasi_X" = "Gen X",
  "Generasi_y" = "Gen Y",
  "Baby_Boomers" = "Baby Boomers"
)

nama_metode <- c(
  "QRIS" = "QRIS",
  "Tunai" = "Cash",
  "Transfer-bank" = "Bank Transfer",
  "Kartu-debit/kredit" = "Credit Card",
  "Other" = "Other"
)

nama_efektif <- c(
  "4" = "Highly efficient",
  "3" = "Some challenges",
  "2" = "Less impact",
  "1" = "Not effective"
)

nama_pernah <- c(
  "Ya" = "Have used QRIS",
  "Tidak" = "Have never used QRIS"
)

nama_kendala <- c(
  "Tidak-semua-terima-QRIS" = "Not all seller use QRIS",
  "jaringan-kurang-stabil" = "signal error",
  "Khawatir-keamanan" = "security issue",
  "Kurang-edukasi" = "uneducated",
  "Other" = "Other"
)

nama_alasan <- c(
  "Praktis-Cepat" = "Convenient and Fast",
  "Masalah-teknis" = "Technical Problems",
  "Tidak-ada-e-wallet" = "Doesn't have E-Wallet",
  "Other" = "Other",
  "Merchant-Menerima" = "Merchants available",
  "Keamanan" = "Security"
)

# UI ----------------------------------------------------------------------

ui <- fluidPage(
  navbarPage("QRIS Adoption Dashboard",
             
             # Overview Tab
             tabPanel("Data Overview",
                      fluidRow(
                        column(width = 12, h2("QRIS adoption across different demographics in Indonesia", align = "left")),  # Judul besar
                        column(width = 12, h5("The data was collected from Google Forms respondents and last verified on March 14, 2025, with a total of 425 participants.", align = "left")),  # Subjudul
                        
                        column(width = 2,  
                               wellPanel(
                                 h4(strong('Select Age Groups'), align = "left"),
                                 checkboxGroupInput("age_filter", "Filter by:", 
                                                    choices = unique(data2$Kelompok_Usia),
                                                    selected = unique(data2$Kelompok_Usia)),
                                 hr(),
                                 h5(strong('Specification:')),
                                 p("Generasi Z (Born 1997 - 2012)"),
                                 p("Generasi Milenial/ Y (Born 1981 - 1996)"),
                                 p("Generasi X (Born 1965 - 1980)"),
                                 p("Generasi Z (Born 1997 - 2012)")
                               ),
                               
                               wellPanel(
                                 h4(strong('Conclusion'), align = "left"),
                                 p("By reading the data shown, we can suggest that:", align = "left"),
                                 hr(),
                                 h4(strong('QRIS is A Popular Payment Method Among Gen Z')),
                                 p("85.3% of QRIS users belong to the Gen Z age group"),
                                 hr(),
                                 h4(strong('QRIS is the preferred choice for its speed and convenience')),
                                 p("75.8% of respondents agree that convenience and speed are the primary reasons for using QRIS in daily transactions"),
                                 hr(),
                                 h4(strong('Expanding the number of merchants can enhance user comfort')),
                                 p("56.3% of QRIS users who have used the service believe that limited merchant acceptance poses a significant challenge"),
                                 hr(),
                                 h4(strong('Strengthening security measures may encourage more people to adopt QRIS')),
                                 p("42% of those who have never used QRIS perceive security concerns as a key barrier")
                               )
                        ),
                        column(width = 5,
                               h3("Distribution of age groups based on Payment Method", align = "left"),
                               h5(strong('85.3% of QRIS users belong to Generation Z. (hover to see details)')),
                               girafeOutput("interactive_plot1"),
                               hr(),
                               h3("Percentage of Qris Users and Reasons to Use"),
                               h5(strong('56.3% of QRIS users who have used the service believe that limited merchant acceptance poses a significant challenge. (hover to see details)')),
                               girafeOutput("interactive_plot4")
                               
                        ),
                        column(width = 5,
                               h3("QRIS Efficiency Rate by Age Groups", align = "left"),
                               h5(strong('Generation Z perceives QRIS as the most effective payment method. (hover to see details)')),
                               girafeOutput("interactive_plot3"),
                               hr(),
                               h3("Main Factors Influencing Payment Method Choices"),
                               h5(strong('75.8% of respondents agree that convenience and speed are the primary reasons for using QRIS in daily transactions. (hover to see details)')),
                               girafeOutput("interactive_plot2")
                        )
                      )
             ),
             
             # Sentiment Analysis Tab
             tabPanel("Thematic and Sentiment Analysis",
                      fluidRow(
                        column(width = 12, 
                               h2("Thematic Analysis", align = "left")),  # Judul besar
                        column(width = 12, 
                               h5("Understanding public perception towards QRIS", align = "left")),  # Subjudul
                        
                        column(width = 6, 
                               h3("Positive vs Negative Sentiment", align = "left"),
                               wordcloud2Output("wordcloud"),
                               p("the most frequent words found from participant's comments"),
                               hr(),
                               h4(strong('Findings on Key Issues in Digital Payment Usage')),
                               p("Based on the thematic analysis, the primary barriers to adopting QRIS and other digital payment methods can be categorized into four main issues:"),
                               h5(strong('Merchant Acceptance (240 mentions)')),
                               p("The most frequently mentioned issue is the limited acceptance of digital payment methods by merchants. Many respondents expressed frustration over the inconsistency of QRIS adoption across different vendors."),
                               h5(strong('Internet Connectivity Issues (138 mentions)')),
                               p("A significant number of users face challenges due to unstable internet connections, which hinder the seamless use of digital payment services."),
                               h5(strong('System Errors (1 mention)')),
                               p("While relatively uncommon, some respondents reported experiencing system failures and technical glitches when making transactions."),
                               h5(strong('User Experience (1 mention)')),
                               p("A small portion of users find digital payments inconvenient or difficult to use, indicating potential areas for interface or process improvements."),
                               h5(strong('Other (55 mentions)')),
                               p("Responses that did not fit into the above categories were grouped under 'Other' These include varied concerns such as transaction delays and security issues."),
                               p("This thematic analysis provides insight into the most pressing challenges, highlighting the need for broader merchant adoption and improvements in internet infrastructure to ensure a smoother digital payment experience.")
                        ),
                        column(width = 6, 
                               h3("Sentiment Distribution", align = "left"),
                               div(style = "overflow-y: auto; height: 300px;", 
                                   DTOutput("table")),
                               p("collection of comments regarding QRIS usage in Indonesia"),
                               hr(),
                               h4(strong('Sentiment Analysis on Digital Payment Usage')),
                               p("A sentiment analysis was conducted to evaluate users' overall perception of digital payment methods, particularly QRIS. The analysis classified comments into three categories: positive, neutral, and negative, allowing us to understand general user sentiment toward digital transactions."),
                               h5(strong('Sentiment Distribution')),
                               tags$ol(
                                 tags$li("Positive Sentiment: Represents users who expressed satisfaction with QRIS, highlighting its convenience, speed, and efficiency in transactions."),
                                 tags$li("Neutral Sentiment: Includes comments that provide factual statements without strong emotions—such as observations about QRIS adoption or technical issues."),
                                 tags$li("Negative Sentiment: Captures complaints and frustrations, particularly concerning merchant acceptance, system reliability, and internet connectivity problems."),
                               ),
                               h5(strong('Key Insights from Sentiment Analysis')),
                               tags$ul(
                                 tags$li("Majority Neutral or Negative: A significant portion of responses indicate either a neutral or negative perception, suggesting mixed experiences with QRIS. While some users appreciate the convenience, others struggle with real-world implementation."),
                                 tags$li("Frustration Over Merchant Acceptance: Many negative comments revolve around the lack of widespread merchant adoption, signaling a key barrier to broader digital payment usage."),
                                 tags$li("Technical and Connectivity Challenges: Users also report issues related to internet instability and system errors, negatively impacting their trust in QRIS."),
                                 tags$li("Positive Remarks Highlight Convenience: Despite the concerns, several users praise QRIS for making transactions easier and reducing dependency on cash.")
                               ),
                               h5("The sentiment analysis reveals that while QRIS is valued for its efficiency, challenges related to merchant acceptance and technical issues hinder full adoption. Addressing these pain points could significantly enhance user experience and trust in digital payments"),

                        )
                      )
             )
  )
)

# Server ------------------------------------------------------------------

server <- function(input, output) {
  
  # Plot1 ----
  output$interactive_plot1 <- renderGirafe({
    # Filter data sesuai pilihan user
    data_filtered <- data2 %>% filter(Kelompok_Usia %in% input$age_filter)
    
    # bikin dulu data persen buat nunjukkin jumlah dan persen kategori di tiap variabel
    data_percentplot1 <- data_filtered %>% # data_percentplot1 isinya ada kolom 
      count(Metode_pembayaran, Kelompok_Usia) %>% # hitung count/ jumlah metode pembayaran & kelompok usia
      group_by(Metode_pembayaran) %>% # group per kelompok usia biar nanti di kasih persen dan angka value responden
      mutate(Respondent = sum(n), # nambahin kolom baru isinya respondent sama persen dan ini jumlah respondennya di paling bawah (bukan irisan)
             Percent = round(n / Respondent * 100, 1), # persen kategori responden per by_group : metode bayar as 100%
             Value = n, # jumlah responden (irisan x dan y)
             nama_usia = recode(Kelompok_Usia, # recode biar di legend dan toottip nanti sesuai
                                "Generasi_Z" = "Gen Z",
                                "Generasi_X" = "Gen X",
                                "Generasi_y" = "Gen Y",
                                "Baby_Boomers" = "Baby Boomers"
             )) %>% # itung persen, bulatkan ke satu desimal
      ungroup()
    # mapping warna berdasarkan bar chart interaktif dan custom (refer ke view data_percentplot1 yachh)
    color_mappingplot1 <- c("Generasi_Z" = "#fca50a", "Generasi_y" = "#EB3678", "Generasi_X" = "#4F1787", "Baby_Boomers" = "#180161")
    
    # bikin stacked bar stacked interaktif
    plot1 <- ggplot(data_percentplot1,
                    aes(
                      x = Value,
                      y = reorder(Metode_pembayaran, n),
                      fill = Kelompok_Usia,
                      tooltip = paste(nama_usia, "\n", n, " people", "(", Percent, "% )"), 
                      data_id = Kelompok_Usia # data id buat biar bisa di highlight
                    )) +
      geom_bar_interactive(
        stat = "identity", 
        position = "stack") +
      theme_minimal() +
      labs(
        x = "Number of Respondents",
        y = "Payment Method",
        fill = "Age Groups"
      ) +
      theme(
        legend.title = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        axis.title.x = element_text(face = "bold")
      ) +
      scale_fill_manual(
        values = color_mappingplot1,
        labels = nama_usia
      )
    # sekarang balikin ggiraph ke girafe biar bisa interaktif
    interactive_plot1 <- girafe(ggobj = plot1) %>% 
      girafe_options(
        opts_hover(css = "fill-opacity:1; stroke:black; stroke-width:1.5px;"),
        opts_hover_inv(css = "opacity: 0.3;")
      )
    return(interactive_plot1)
    
  })
  
  # Plot2 ----
  output$interactive_plot2 <- renderGirafe({
    # bikin dulu data persen buat nunjukkin jumlah dan persen kategori di tiap variabel
    data_percentplot2 <- data2 %>% # data_percentplot2 isinya ada kolom khusus slice variabel yang dipilih kyk mutate di bawah
      count(Alasan_Menggunakan, Metode_pembayaran) %>% # hitung count/ jumlah metode pembayaran & kelompok usia
      group_by(Alasan_Menggunakan) %>% # group per alasan menggunakan biar nanti di kasih persennya per alasan menggunakan dihitung 100%
      mutate(Respondent = sum(n), # nambahin kolom baru isinya respondent sama persen. baris ini ngitung irisan alasan menggunakan dan metode pembayaran
             Percent = round(n / Respondent * 100, 1), # ini ngitung persen, bulatkan ke satu desimal
             Value = n, # jumlah orangnya raw bukan persen
             nama_metode = recode(Metode_pembayaran, # recode biar pas output tooltip, nama yang ditunjukkin ga salah. ga cuman random milih ngasal
                                  "QRIS" = "QRIS",
                                  "Tunai" = "Cash",
                                  "Transfer-bank" = "Bank Transfer",
                                  "Kartu-debit/kredit" = "Credit Card",
                                  "Other" = "Other"
             )) %>% 
      ungroup()
    
    # mapping warna berdasarkan bar chart interaktif dan custom (refer ke view data_percentplot1 yachh)
    color_mappingplot2 <- c("QRIS" = "#fcce0a", "Tunai" = "#FB773C", "Transfer-bank" = "#EB3678", "Kartu-debit/kredit" = "#4F1787", "Other" = "#180161")
    
    # bikin stacked bar stacked interaktif
    plot2 <- ggplot(data_percentplot2,
                    aes(
                      x = Value,
                      y = reorder(Alasan_Menggunakan, n),
                      fill = Metode_pembayaran,
                      tooltip = paste(nama_metode, "\n", n, " people", "(", Percent, "% )"), 
                      data_id = Metode_pembayaran
                    )) +
      geom_bar_interactive(
        stat = "identity", 
        position = "stack") +
      theme_minimal() +
      labs(
        x = "Number of Respondents",
        y = "Reason to Use",
        fill = "Payment Method",  # Rename judul legend
      ) + # nambahin beberapa styling
      theme( # nambahin styling (terbatas selain nama isi y, x, dan legend)
        legend.title = element_text(face = "bold"), # bold title legend
        axis.title.y = element_text(face = "bold"),
        axis.title.x = element_text(face = "bold")
      ) +
      scale_fill_manual( # rename text legend yg samping warna
        values = color_mappingplot2, 
        labels = nama_metode
      ) +
      scale_y_discrete( # rename text di dalem alasan menggunakan
        labels = nama_alasan
      ) 
    
    # sekarang balikin ggiraph ke girafe biar bisa interaktif
    interactive_plot2 <- girafe(ggobj = plot2) %>% 
      girafe_options(
        opts_hover(css = "fill-opacity:1; stroke:black; stroke-width:1.5px;"),
        opts_hover_inv(css = "opacity: 0.3;")
      )
    
    return(interactive_plot2)
  })
  
  # Plot3 -----
  output$interactive_plot3 <- renderGirafe({
    # data count: karena ga ada data persen, jadi lebih simpel pake data count aja
    data_countplot3 <- data2 %>% 
      count(Kelompok_Usia, Tingkat_Efisiensi_QRIS) %>% # hitung irisan n kelompok usia dan tingkat efisiensi
      mutate(
        nama_efektif = recode(Tingkat_Efisiensi_QRIS,
                              "4" = "Highly efficient",
                              "3" = "Some challenges",
                              "2" = "Less impact",
                              "1" = "Not effective"
        ),
        nama_usia = recode(Kelompok_Usia,
                           "Generasi_Z" = "Gen Z",
                           "Generasi_X" = "Gen X",
                           "Generasi_y" = "Gen Y",
                           "Baby_Boomers" = "Baby Boomers")
      )
    
    # gaperlu mapping warna (lu ngeribetin diri sendiri)
    plot3 <- ggplot(data_countplot3,
                    aes(
                      x = Kelompok_Usia,
                      y = Tingkat_Efisiensi_QRIS,
                      fill = n, 
                      tooltip = paste0(
                        "Age Group: ", nama_usia, "\n",
                        "Efficiency Rating: ", nama_efektif, "\n",
                        "Number of Respondent: ", n, "\n"
                      ),
                      data_id = paste(Kelompok_Usia, Tingkat_Efisiensi_QRIS, sep = "_") # Bikin unik ID jadi ga ada yang ke highlight dobel
                    )) +
      geom_tile_interactive( # ngebalikin data unik, useful buat interaktif
        stat = "identity"
      ) +
      scale_fill_viridis_c( # ini auto perkiraan n ya
        option = "inferno",
        name = "Respondents", 
        begin = 0.1, 
        end = 0.8
      ) +
      theme_minimal() +
      labs( # rename judul title di x sama y
        x = "Age Groups",
        y = "QRIS Efficiency Rating (value = 0 not shown)",
      ) +
      theme( # cuman biar bold
        legend.title = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        axis.title.x = element_text(face = "bold")
      ) +
      scale_y_discrete(
        labels = nama_efektif
      ) +
      scale_x_discrete(
        labels = nama_usia
      )
    
    print(plot3) # kalo heatmap pakenya print
    
    # balikin ggiraph ke girafe biar interaktif
    interactive_plot3 <- girafe(ggobj = plot3) %>% 
      girafe_options(
        opts_hover(css = "fill-opacity:1; stroke:black; stroke-width:1.5px"),
        opts_hover_inv(css = "opacity: 0.3;")
      )
    
    return(interactive_plot3)
  })
  
  # Plot4 ----
  output$interactive_plot4 <- renderGirafe({
    ## Prepare data
    
    # plot4a pie chart prep data: hitung kategori tiap menjawab yes and no
    data_countplot4a <- data2 %>% 
      count(Pernah_Pakai_QRIS, name = "count") %>% 
      rename(category = Pernah_Pakai_QRIS) %>% 
      mutate(
        fraction = count / sum(count),
        ymax = cumsum(fraction),
        ymin = c(0, head(ymax, n = -1)),
        nama_pernah = recode(category,
                             "Ya" = "Have used QRIS",
                             "Tidak" = "Have never used QRIS"),
        labelpositionp= (ymax + ymin) / 2,
        label = paste0(nama_pernah, "\n", ": ", count),
      )
    
    # plot4b plot4c bar chart data
    data_countyes <- data2 %>% 
      filter(Pernah_Pakai_QRIS == "Ya") %>% 
      mutate(
        nama_kendala = recode(Kendala_Qris,
                              "Tidak-semua-terima-QRIS" = "Not all seller use QRIS",
                              "jaringan-kurang-stabil" = "signal error",
                              "Khawatir-keamanan" = "security issue",
                              "Kurang-edukasi" = "uneducated",
                              "Other" = "Other"),
        nama_pernah = recode(Pernah_Pakai_QRIS,
                             "Ya" = "Have used QRIS",
                             "Tidak" = "Have never used QRIS")
      )
    
    data_countno <- data2 %>% 
      filter(Pernah_Pakai_QRIS == "Tidak") %>% 
      complete(Kendala_Qris, fill = list(n = 0)) %>%
      mutate(
        nama_kendala = recode(Kendala_Qris,
                              "Tidak-semua-terima-QRIS" = "Not all seller use QRIS",
                              "jaringan-kurang-stabil" = "signal error",
                              "Khawatir-keamanan" = "security issue",
                              "Kurang-edukasi" = "uneducated"),
        nama_pernah = recode(Pernah_Pakai_QRIS,
                             "Ya" = "Have used QRIS",
                             "Tidak" = "Have never used QRIS")
      )

    
    ## bikin color mapping manual
    color_mappingplot4 <- c("Have used QRIS" = "#fca50a", "Have never used QRIS" = "#4F1787")
    
    ## Testing basic interactive plot 4a ----
    plot4a <- ggplot(data_countplot4a,
                     aes(ymax = ymax,
                         ymin = ymin,
                         xmax = 5,
                         xmin = 2.5,
                         fill = nama_pernah)
    ) +
      geom_rect_interactive(
        aes(tooltip = label, data_id = category)
      ) +
      coord_polar(theta = "y") +
      xlim(c(1.5, 5)) +
      theme_void() +
      theme(legend.position = "none") +
      scale_fill_manual(values = color_mappingplot4)

    
    ## Testing basic interactive plot 4b ----
    plot4b <- ggplot(data_countyes %>% 
                       count(Kendala_Qris) %>% 
                       arrange(desc(n)),
                     aes(y = reorder(Kendala_Qris, n), x = n, fill = "Have used QRIS", tooltip = paste0(n, " people"), data_id = "Ya")) +
      geom_col_interactive(width = 0.6) +
      labs(
        title = "Those who did use QRIS",
        y = "challenges",
        x = "Number of Respondents"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 7, face = "bold"),
        axis.text.x = element_text(size = 7),
        axis.text.y = element_text(size = 7),
        legend.title = element_text(size = 7, face = "bold"),  # Ukuran judul legend (Fill)
        legend.text = element_text(size = 5),
        legend.key.size = unit(0.3, "cm"),
        axis.title.x = element_text(size = 7, face = "bold"),  # Ukuran label X-axis
        axis.title.y = element_text(size = 7, face = "bold")  # Smaller title
      ) +
      scale_fill_manual(values = color_mappingplot4) +
      scale_y_discrete(labels = nama_kendala)

    
    ## Testing basic interactive plot 4c ----
    plot4c <- ggplot(data_countno %>% 
                       count(Kendala_Qris) %>% 
                       arrange(desc(n)),
                     aes(y = reorder(Kendala_Qris, n), x = n, fill = "Have never used QRIS", tooltip = paste0(n, " people"), data_id = "Tidak")) +
      geom_col_interactive(width = 0.6) +
      labs(
        title = "Those who did not use QRIS",
        y = "challenges",
        x = "Number of Respondents"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 7, face = "bold"),
        axis.text.x = element_text(size = 7),
        axis.text.y = element_text(size = 7),
        legend.title = element_text(size = 7),  # Ukuran judul legend (Fill)
        legend.text = element_text(size = 5),
        legend.key.size = unit(0.3, "cm"),
        axis.title.x = element_text(size = 7, face = "bold"),  # Ukuran label X-axis
        axis.title.y = element_text(size = 7, face = "bold")  # Smaller title
      ) +
      scale_fill_manual(values = color_mappingplot4) +
      scale_y_discrete(labels = nama_kendala)
    
    ## Combine plots with new layout
    plot4 <- (plot4a | (plot4b / plot4c)) +
      plot_layout(widths = c(2, 1.3), heights = c(0.1, 0.1)) +  
      plot_annotation(theme = theme(plot.margin = margin(t = 0, r = 4, b = 0, l = 4)))
    
    plot4
    ## create interactive plot
    interactive_plot4 <- girafe(ggobj = plot4)
    interactive_plot4 <- girafe_options(
      interactive_plot4,
      opts_hover(css = "fill-opacity:1; stroke:black; stroke-width:1.5px;"),
      opts_hover_inv(css = "opacity: 0.3;")
    )
    
    # view interactive plot
    return(interactive_plot4)
  
  })
  
  # wordcloud ----
  output$wordcloud <- renderWordcloud2({
    req(worddata)  # Pastikan dataset ada
    
    # Hapus NA kalau ada
    clean_data <- worddata %>% drop_na()
    clean_data_sorted <- clean_data %>% arrange(desc(freq))  # Urutin dari yang paling sering
    
    
    wordcloud2(clean_data_sorted, 
               color = ifelse(input$color == "random-light", "random-light", input$color),
               shuffle = FALSE)  # Supaya gak keacak
    wordcloud2(data=worddata, size=1.6)
  })
  
  # table ----
  output$table <- renderDT({
    req(datacomment)  # Cegah error kalau data kosong
    datatable(datacomment, 
              options = list(
                scrollY = "300px",  
                paging = FALSE    
              ))
    })
}

shinyApp(ui, server)
