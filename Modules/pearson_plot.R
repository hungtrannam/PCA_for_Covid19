# Thiết lập một số gói lệnh liên quan
library(ggplot2)
library(tidyverse)

# Truy cập đường dẫn dữ liệu
setwd("D:/PCA_for_Covid19-main/Data")

covid_case <- read.csv("covid_case.csv",
                      header = TRUE,
                      sep = ",",
                      stringsAsFactors = FALSE)

# Thiết lập định dạng
case_data <- covid_case %>% select(-Day)

# Dữ liệu ma trận tương quan
cor_data <- case_data %>% cor() %>% data.frame()

# Chuẩn hóa các trục x, y
Pearson_cor <- 1 - cor_data$TP.Ho.Chi.Minh
Variables <- cor_data %>% names()

# Tương quan theo biến chính
cor_HCMC <- cor_data %>% ggplot(., 
                    aes(x = Variables, 
                        y = Pearson_cor, 
                        size = TP.Ho.Chi.Minh)) + 
  geom_point(aes(colour = TP.Ho.Chi.Minh)) + 
  coord_polar() + 
  theme_bw() + 
  scale_colour_gradient2(low = "#132B43", high = "#56B1F7")

cor_HCMC
