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
setwd("D:/PCA_for_Covid-main/Data")

covid_cul <- read.csv("covid_case.csv",
                      header = TRUE,
                      sep = ",",
                      stringsAsFactors = FALSE)
covid_case <- read.csv("covid_cul.csv",
                       header = TRUE,
                       stringsAsFactors = FALSE)

# Thiết lập định dạng
covid_case$Day <- as.Date(covid_case$Day, format = "%d/%m/%Y")
covid_cul$Day <- as.Date(covid_cul$Day, format = "%d/%m/%Y")

# Trích lọc dữ liệu
case_data <- covid_case %>% select(-Day)
cul_data <- covid_cul %>% select(-Day)

# Số chiều dữ liệu
covid_case %>% dim()
covid_cul %>% dim()

# Định danh các biến
case_data %>% names()
cul_data %>% names()

$ Thống kê mô tả
covid_case %>% sample_n(., 6)
covid_cul %>% tail()


# Tương quan đồ
corrplot::corrplot.mixed(cor_data <- case_data %>% cor(),
                         tl.cex = 0.5,
                         tl.col = "black",
                         order = "hclust",
                         addCoefasPercent = TRUE,
                         upper = "ellipse")

corrplot::corrplot.mixed(cor_data <- cul_data %>% cor(),
                         tl.cex = 0.5,
                         tl.col = "black",
                         order = "hclust",
                         addCoefasPercent = TRUE,
                         upper = "ellipse")

# Mạng tương quan
Graph_pcor <- case_data %>% cor() %>% 
     qgraph::qgraph(.,
                    graph = "pcor",
                    layout = "spring",
                    threshold = "bonferroni",
                    sampleSize = nrow(case_data),
                    alpha = 0.05)

Graph_pcor <- cul_data %>% cor() %>% 
     qgraph::qgraph(.,
                    graph = "pcor",
                    layout = "spring",
                    threshold = "bonferroni",
                    sampleSize = nrow(cul_data),
                    alpha = 0.05)

# Sơ đồ sàng lọc (scree plot)
case_data %>% psych::fa.parallel(., main = "Scree plot with parallel analysis")
cul_data %>% psych::fa.parallel(., main = "Scree plot with parallel analysis")

# Phân tích thành phần chính
prcomp(case_data, center = TRUE, scale = TRUE) %>% summary()
prcomp(cul_data, center = TRUE, scale = TRUE) %>% summary()

princomp(case_data, scores = TRUE) %>% loadings()
princomp(cul_data, scores = TRUE) %>% loadings()

# Giá trị riêng và phần trăm phương sai giải thích
pca_case <- FactoMineR::PCA(case_data, graph = FALSE)
left <- pca_case %>% factoextra::fviz_eig(.,
                                          choice = 'eigenvalue',
                                          geom = 'line',
                                          addlabels = TRUE,
                                          repel = TRUE)
right <- pca_case %>% factoextra::fviz_screeplot(.,
                                                 addlabels = TRUE,
                                                 repel = TRUE)
gridExtra::grid.arrange(left, right, ncol = 2)


# Biplot
pca_case %>% factoextra::fviz_pca_biplot(.,
                                         repel = TRUE,
                                         col.var = "#2E9FDF",
                                         col.ind = "#696969")
                                        
# Biplot cá thể
pca_case %>% fviz_pca_ind(.,
                          col.ind = "cos2",
                          pointsize = "cos2",
                          gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                          repel = TRUE)

# Dim2 và cos2
pca_case %>% FactoMineR::plot.PCA(.,
                                  choix = 'var',
                                  select = 'contrib 5')
pca_case %>% factoextra::fviz_pca_var(.,
                                      col.var = "cos2",
                                      gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                                      repel = TRUE)

# Trị riêng
factoextra::get_eig(pca_case)
summary(pca_case)

# Tương quan cos2 với Dim
var <- pca_case %>% factoextra::get_pca_var()
                    corrplot(var$cos2, is.corr = FALSE)
var <- pca_cul %>% factoextra::get_pca_var()
                    corrplot(var$cos2, is.corr = FALSE)

# Kiểm định Barlett - KMO
case_data %>% psych::cortest.bartlett()
case_data %>% psych::KMO()

cul_data %>% psych::cortest.bartlett()
cul_data %>% psych::KMO()


# Ma trận xoay
Nfacs <- 3
principal(case_data %>% select(-Long.An),
          nfactors = Nfacs,
          rotate = "varimax") %>%
  print.psych(.,
              cut = 0.55,
              sort = TRUE)

# Biểu đồ nhân tố
fa.diagram(fa_case$loadings)
fa.diagram(fa_cul$loadings)

# Nhân tố với Factor Loadings = 5555
fa_case %>% factor.plot(., cut = 0.55)
fa_cul %>% factor.plot(., cut = 0.55)



