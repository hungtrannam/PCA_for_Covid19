pks <- c('psych', 'tidyverse', 'factoextra', 'ggplot2',
         'gridExtra', 'FactoMineR', 'igraph', 'corrplot')
install.packages(pks, dependencies = TRUE)

# Thiết lập một số gói lệnh liên quan
library(psych)
library(tidyverse)
library(factoextra, FactoMineR)
library(ggplot2, gridExtra)
library(igraph)

# Truy cập đường dẫn dữ liệu
setwd("D:/PCA_for_Covid19-main/Data")

covid_cul <- read.csv("covid_cul.csv",
                      header = TRUE,
                      sep = ",",
                      stringsAsFactors = FALSE)

# Thiết lập định dạng
covid_cul$Day <- as.Date(covid_cul$Day, format = "%d/%m/%Y")

# Trích lọc dữ liệu
cul_data <- covid_cul %>% select(-Day)

# Số chiều dữ liệu
covid_cul %>% dim()

# Định danh các biến
covid_cul %>% names()

# Thống kê mô tả
covid_cul %>% sample_n(., 6)
covid_cul %>% tail()
covid_cul %>% View()


# Tương quan đồ
corrplot::corrplot.mixed(cor_data <- cul_data %>% cor(),
                         tl.cex = 0.5,
                         tl.col = "black",
                         order = "hclust",
                         addCoefasPercent = TRUE,
                         upper = "ellipse")

# Mạng tương quan
Graph_pcor <- cul_data %>% cor() %>% 
     qgraph::qgraph(.,
                    graph = "pcor",
                    layout = "spring",
                    threshold = "bonferroni",
                    sampleSize = nrow(cul_data),
                    alpha = 0.05)

# Sơ đồ sàng lọc (scree plot)
cul_data %>% psych::fa.parallel(., main = "Scree plot with parallel analysis")

# Phân tích thành phần chính
prcomp(cul_data, center = TRUE, scale = TRUE) %>% summary()

princomp(cul_data, scores = TRUE) %>% loadings()

pca_cul <- FactoMineR::PCA(cul_data, graph = FALSE)

# Giá trị riêng và phần trăm phương sai giải thích
left <- pca_cul %>% factoextra::fviz_eig(.,
                                          choice = 'eigenvalue',
                                          geom = 'line',
                                          addlabels = TRUE,
                                          repel = TRUE)
right <- pca_cul %>% factoextra::fviz_screeplot(.,
                                                 addlabels = TRUE,
                                                 repel = TRUE)
gridExtra::grid.arrange(left, right, ncol = 2)

# Biplot
pca_cul %>% factoextra::fviz_pca_biplot(.,
                                         repel = TRUE,
                                         col.var = "#2E9FDF",
                                         col.ind = "#696969")
                                        
# Biplot cá thể
pca_cul %>% fviz_pca_ind(.,
                          col.ind = "cos2",
                          pointsize = "cos2",
                          gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                          repel = TRUE)

# Dim2 và cos2
pca_cul %>% FactoMineR::plot.PCA(.,
                                  choix = 'var',
                                  select = 'contrib 5')
pca_cul %>% factoextra::fviz_pca_var(.,
                                      col.var = "cos2",
                                      gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                                      repel = TRUE)

# Trị riêng
factoextra::get_eig(pca_cul)
summary(pca_cul)

# Tương quan cos2 với Dim
var <- pca_cul %>% factoextra::get_pca_var()
                    corrplot(var$cos2, is.corr = FALSE)

# Kiểm định Barlett - KMO
cul_data %>% psych::cortest.bartlett()
cul_data %>% psych::KMO()

# Phân tích nhân tố
Nfacs <- 2
fa_cul <- cul_data %>% select(-Long.An) %>%
         factanal(.,
                  Nfacs,
                  rotation = "varimax")

# Ma trận xoay
principal(cul_data %>% select(-Long.An),
          nfactors = Nfacs,
          rotate = "varimax") %>%
  print.psych(.,
              cut = 0.55,
              sort = TRUE)

# Biểu đồ nhân tố
fa.diagram(fa_cul$loadings)

# Nhân tố với Factor Loadings = 0.55
fa_cul %>% factor.plot(., cut = 0.55)

# vass và omega
cul_data %>% psych::vss()
cul_data %>% psych::omega(pca_data)

# k-means với k = 4
km_cul <- cul_data %>% kmeans(, centers = 4)
fviz_cluster(object = km_cul, 
             data = cul_data, 
             labelsize = 0) + 
  theme_minimal()
