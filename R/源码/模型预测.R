library(dplyr)
library(forecast)
library(tseries)
library(ggplot2)

# 选择要处理的城市
cities <- unique(df_clean$city)

# 创建一个空的数据框来存储所有预测结果
forecast_results <- data.frame(city = character(),
                               datetime = character(),
                               pollutant = character(),
                               forecast_value = numeric(),
                               stringsAsFactors = FALSE)

# 循环处理每个城市
for(city_sel in cities) {
  # 只处理 AQI 指数
  pollutant_sel <- "AQI"
  
  # 提取当前城市和 AQI 的数据
  ts_data <- df_clean %>%
    filter(city == city_sel, pollutant == pollutant_sel) %>%
    arrange(datetime) %>%
    pull(value_filled)
  
  # 将数据集划分为训练集和测试集
  train_size <- floor(0.7 * length(ts_data))  # 使用 70% 数据作为训练集
  train_data <- ts_data[1:train_size]
  test_data <- ts_data[(train_size+1):length(ts_data)]
  
  # 转换为时间序列对象
  ts_series <- ts(train_data, frequency = 24)
  
  # -----------------------------
  # 自动 ARIMA 拟合（含 Box-Cox 变换）
  # -----------------------------
  fit <- auto.arima(
    ts_series,
    seasonal = TRUE,         # 考虑季节性
    lambda = "auto"          # Box-Cox 自动变换
  )
  
  # -----------------------------
  # 在测试集上进行预测
  # -----------------------------
  forecast_length <- length(test_data)
  fc <- forecast(fit, h = forecast_length)
  
  # -----------------------------
  # 计算预测误差
  # -----------------------------
  accuracy_metrics <- accuracy(fc, test_data)
  print(paste("Accuracy metrics for city:", city_sel))
  print(accuracy_metrics)
  
  # -----------------------------
  #  模型诊断
  # -----------------------------
  checkresiduals(fit)  # 绘制残差图、ACF/PACF、Ljung-Box 检验
  
  # 将预测结果添加到数据框中
  forecast_data <- data.frame(
    city = rep(city_sel, forecast_length),
    datetime = as.character(time(fc$mean)),
    pollutant = rep(pollutant_sel, forecast_length),
    forecast_value = as.numeric(fc$mean)
  )
  
  # 合并到所有预测结果中
  forecast_results <- bind_rows(forecast_results, forecast_data)
  
  # -----------------------------
  #  可视化预测
  # -----------------------------
  p <- autoplot(fc) +
    ggtitle(paste("未来24小时预测 - 城市:", city_sel, "污染物:", pollutant_sel)) +
    xlab("时间") +
    ylab("污染值")
  
  # 输出图形
  print(p)
}

# -----------------------------
# 保存结果到 CSV 文件
# -----------------------------
write.csv(forecast_results, "forecast_results_AQI.csv", row.names = FALSE)
