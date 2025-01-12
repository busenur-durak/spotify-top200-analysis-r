# Gerekli kC<tC<phaneleri kontrol etmek 
required_packages <- c("ggplot2", "dplyr", "tidyr", "cluster", "caret", "openxlsx", 
                       "factoextra", "ggpubr", "ggcorrplot")
new_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if (length(new_packages)) install.packages(new_packages)

# KC<tC<phaneleri yC<klemek
lapply(required_packages, library, character.only = TRUE)

# Excel dosyasD1nD1 yC<klemek
data <- read.xlsx("data/R_Programming_Proje_.xlsx", sheet = "Sayfa1")

# Veri setinin yapD1sD1nD1 kontrol etmek
print("Dataset Overview")
print(str(data))

# VirgC<lleri kaldD1rmak ve 'Streams' sutununu sayD1sala cevirmek
data <- data %>%
  mutate(
    Streams = as.numeric(gsub(",", "", Streams))
  )

# Artist suutunlarD1nD1 birlestirmek
data <- data %>%
  mutate(Artist = paste(Artist1, Artist2, Artist3, Artist4, Artist5, sep = ", ")) %>%
  select(-Artist1, -Artist2, -Artist3, -Artist4, -Artist5)

# Language sutunlarD1nD1 birlestirmek
data <- data %>%
  mutate(Language = paste(Language1, Language2, sep = ", ")) %>%
  select(-Language1, -Language2)

# Eksik degerler
data <- data %>%
  mutate(across(everything(), ~ ifelse(is.na(.), "Unknown", .)))
sum(is.na(data))

# SayD1sal verileri normalize etmek
normalize <- function(x) {
  return ((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}
data <- data %>%
  mutate(across(where(is.numeric), normalize))

# Step 1: GC6rselleEtirme
# Streams DagD1lD1mD1 (Histogram ve Yogunluk Grafikleri)
ggplot(data, aes(x = Streams)) +
  geom_histogram(binwidth = 0.05, fill = "blue", alpha = 0.7) +
  labs(title = "Distribution of Streams", x = "Streams", y = "Frequency")

ggplot(data, aes(x = Streams)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Density of Streams", x = "Streams", y = "Density")
#peak dagD1lD1mD1(histogram grafigi)
ggplot(data, aes(x = Peak)) +
  geom_histogram(binwidth = 0.05, fill = "blue", alpha = 0.7) +
  labs(title = "Distribution of Peak", x = "Peak", y = "Frequency")
#streak dagD1lD1mD1(hD1stogram grafigi)
ggplot(data, aes(x = Streak)) +
  geom_histogram(binwidth = 0.05, fill = "blue", alpha = 0.7) +
  labs(title = "Distribution of Streak", x = "Streak", y = "Frequency")


# SayD1sal Degerlerin dagD1lD1mD1 (Histogram ve Yogunluk Grafikleri)
data %>%
  select(where(is.numeric)) %>%
  gather(key = "Variable", value = "Value") %>%
  ggplot(aes(x = Value, fill = Variable)) +
  geom_histogram(alpha = 0.7, bins = 20) +
  facet_wrap(~Variable, scales = "free") +
  labs(title = "Histograms of Numeric Variables", x = "Value", y = "Frequency")

data %>%
  select(where(is.numeric)) %>%
  gather(key = "Variable", value = "Value") %>%
  ggplot(aes(x = Value, fill = Variable)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~Variable, scales = "free") +
  labs(title = "Density Plots of Numeric Variables", x = "Value", y = "Density")

# kategorD1k degerlerin dagD1lD1mD1(pie chart)
#language
# Verileri dC<zenle
top_languages <- data %>%
  filter(!is.na(Language) & Language != "") %>%  # NA ve boE deDerleri kaldD1r
  separate_rows(Language, sep = ",\\s*") %>%    # Dillerin virgC<lle ayrD1lmD1E olduDu yerleri ayD1r
  group_by(Language) %>%                        # Dile gC6re grupla
  summarise(TotalStreams = sum(Streams, na.rm = TRUE)) %>% # Her dil iC'in toplam Stream deDerini hesapla
  filter(TotalStreams<20)%>%
  arrange(desc(TotalStreams))     
# Pasta grafiDi oluEtur
ggplot(top_languages, aes(x = "", y = TotalStreams, fill = Language)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  labs(title = "Language Distribution", x = "", y = "") +
  theme_void()  # Grafik gC6rC<nC<mC<nC< sadeleEtir

#genre
# Verileri dC<zenle
genre_dist <- data %>%
  count(Genre) %>%                         # Her tC<rC<n frekansD1nD1 hesapla
  arrange(desc(n))                         # Frekansa gC6re azalan sD1rada sD1rala

# Pasta grafiDi oluEtur
ggplot(genre_dist, aes(x = "", y = n, fill = Genre)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  labs(title = "Genre Distribution", x = "", y = "") +
  theme_void()  # Grafik gC6rC<nC<mC<nC< sadeleEtir

# Streams ve Genre DagD1lD1mD1 (Boxplot, Violin Plot, Jitter)
#boxplot
ggplot(data, aes(x = Genre, y = Streams, fill = Genre)) +
  geom_boxplot() +
  labs(title = "Streams by Genre (Boxplot)", x = "Genre", y = "Streams") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#violin
ggplot(data, aes(x = Genre, y = Streams, fill = Genre)) +
  geom_violin() +
  labs(title = "Streams by Genre (Violin Plot)", x = "Genre", y = "Streams")
#jitter
ggplot(data, aes(x = Genre, y = Streams, color = Genre)) +
  geom_jitter(alpha = 0.7) +
  labs(title = "Streams by Genre (Jitter)", x = "Genre", y = "Streams")


 
# Stream ve Peak Analizi (Scatter Plot + Line Fit)
ggplot(data, aes(x = Streams, y = Peak)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Streams vs Peak", x = "Streams", y = "Peak")

# Top 10 Artist, Album, Genre, Language, Song
#artist
# Verileri filtrele ve iEle
top_artist <- data %>%
  filter(!is.na(Artist) & Artist != "") %>%  # NA ve boE deDerleri kaldD1r
  separate_rows(Artist, sep = ",\\s*") %>%  # SanatC'D1 isimlerini ayD1r
  group_by(Artist) %>%
  summarise(TotalStreams = sum(Streams, na.rm = TRUE)) %>%  # Her sanatC'D1nD1n toplamD1nD1 hesapla
  arrange(desc(TotalStreams))  # Toplamlara gC6re azalan sD1rada sD1rala

# Bruno Mars'tan itibaren veriyi seC'
top_artist_filtered <- top_artist %>%
  filter(Artist == "Bruno Mars" | row_number() > which(Artist == "Bruno Mars")) %>%
  slice_head(n = 10)  # D0lk 10 sanatC'D1yD1 seC'

# GrafiDi oluEtur
ggplot(top_artist_filtered, aes(x = reorder(Artist, TotalStreams), y = TotalStreams, fill = Artist)) +
  geom_col() +
  coord_flip() +
  labs(title = "Top 10 Artists", x = "Artist", y = "Total Streams") +
  theme_minimal()


#language
# Verileri filtrele ve iEle
top_languages <- data %>%
  filter(!is.na(Language) & Language != "") %>%  # NA ve boE deDerleri kaldD1r
  separate_rows(Language, sep = ",\\s*") %>%  # Dilleri ayD1r
  group_by(Language) %>%
  summarise(TotalStreams = sum(Streams, na.rm = TRUE)) %>%  # Her dilin toplamD1nD1 hesapla
  filter(TotalStreams <20) %>%  
  arrange(desc(TotalStreams))  # Toplamlara gC6re azalan sD1rada sD1rala

# GrafiDi oluEtur
ggplot(top_languages, aes(x = reorder(Language, TotalStreams), y = TotalStreams, fill = Language)) +
  geom_col() +
  coord_flip() +
  labs(title = "Languages with Significant Total Streams", x = "Language", y = "Total Streams") +
  theme_minimal()

#album
top10_album.name <- data %>%
  group_by(Album.Name) %>%
  summarise(TotalStreams = sum(Streams, na.rm = TRUE)) %>%
  arrange(desc(TotalStreams)) %>%
  slice_head(n = 10)

ggplot(top10_album.name, aes(x = reorder(Album.Name, TotalStreams), y = TotalStreams, fill = Album.Name)) +
  geom_col() +
  coord_flip() +
  labs(title = "Top 10 Album Name by Total Streams", x = "Album Name", y = "Total Streams")
#genre
top10_genre <- data %>%
  group_by(Genre) %>%
  summarise(TotalStreams = sum(Streams, na.rm = TRUE)) %>%
  arrange(desc(TotalStreams)) %>%
  slice_head(n = 10)

ggplot(top10_genre, aes(x = reorder(Genre, TotalStreams), y = TotalStreams, fill = Genre)) +
  geom_col() +
  coord_flip() +
  labs(title = "Top 10 Genre by Total Streams", x = "Genre", y = "Total Streams")


# sayD1sal degerlerin Scatter Plot Matrisi 
numeric_data <- data %>%
  select(where(is.numeric))
pairs(numeric_data, main = "Scatter Plot Matrix of Numeric Variables")
#kC<tC<phane yuklemek
install.packages("GGally")

library(GGally)
#streams streak peak arasD1nda korelasyon
# Verilerden Streams, Streak ve Peak sC<tunlarD1nD1 seC'ip korelasyon grafiDi oluEturma
streams_correlation <- data %>%
  select(Streams, Streak, Peak) %>%
  filter(!is.na(Streams) & !is.na(Streak) & !is.na(Peak))  # NA deDerlerini kaldD1r

# Korelasyon grafiDi
ggpairs(streams_correlation, 
        title = "Correlation Between Streams, Streak, and Peak",
        lower = list(continuous = wrap("smooth", method = "lm", se = FALSE)), # Alt C<C'gende doDrusal regresyon eDrisi
        diag = list(continuous = wrap("densityDiag")), # Diyagonal histogram/density
        upper = list(continuous = wrap("cor", size = 4)))  # Cst C<C'gende korelasyon katsayD1sD1

#tC<rlerin ortalama streamslarD1na gore korelasyon
# Average Stream by Genre hesaplama
average_stream_by_genre <- data %>%
  group_by(Genre) %>%  # Her bir tC<r iC'in grupla
  summarise(AverageStream = mean(Streams, na.rm = TRUE)) %>%  # Ortalama stream hesapla
  filter(!is.na(Genre) & Genre != "")  # NA ve boE tC<r deDerlerini kaldD1r

# Korelasyon grafiDi
ggplot(average_stream_by_genre, aes(x = reorder(Genre, AverageStream), y = AverageStream)) +
  geom_point(color = "blue", size = 3) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +  # DoDrusal regresyon C'izgisi
  labs(title = "Average Stream by Genre", x = "Genre", y = "Average Stream") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # X ekseni yazD1larD1nD1 eD

# KC<meleme Analizleri
set.seed(123)
clustering_data <- data %>%
  select(Streams, Peak, Streak)

# Elbow YC6ntemi
fviz_nbclust(clustering_data, kmeans, method = "wss") +
  labs(title = "Elbow Method for Optimal Clusters")

# K-Means ile KC<meleme
kmeans_result <- kmeans(clustering_data, centers = 3)
data$Cluster <- as.factor(kmeans_result$cluster)

# KC<meleme sonuclarD1nD1n gorsellestirilmesi
ggplot(data, aes(x = Streams, y = Peak, color = Cluster)) +
  geom_point() +
  labs(title = "Clustering of Songs", x = "Streams", y = "Peak", color = "Cluster")

# Dendogram
d <- dist(clustering_data)
hc <- hclust(d)
plot(hc, main = "Dendrogram of Songs", xlab = "", sub = "", cex = 0.9)
rect.hclust(hc, k = 3, border = "red")
