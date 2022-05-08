read_data <- function(patien1,patient2,patient3,patient4,patient5,patient6) {
  data_diff <- NULL
  for (i in 1:length(patient1)){
    filename1 = patient1[i]
    filename2 = patient2[i]
    filename3 = patient3[i]
    filename4 = patient4[i]
    filename5 = patient5[i]
    filename6 = patient6[i]
    data1 = read.csv(filename1,header = FALSE, fill = TRUE, sep = " ")
    data2 = read.csv(filename2,header = FALSE, fill = TRUE, sep = " ")
    data3 = read.csv(filename3,header = FALSE, fill = TRUE, sep = " ")
    data4 = read.csv(filename4,header = FALSE, fill = TRUE, sep = " ")
    data5 = read.csv(filename5,header = FALSE, fill = TRUE, sep = " ")
    data6 = read.csv(filename6,header = FALSE, fill = TRUE, sep = " ")
    if (ncol(data1)>5){
      colnames(data1) <- c("input", "test", "input_id", "test_id", "n_matches", "nkey_in", "nkey_test")
      colnames(data2) <- c("input", "test", "input_id", "test_id", "n_matches", "nkey_in", "nkey_test")
      colnames(data3) <- c("input", "test", "input_id", "test_id", "n_matches", "nkey_in", "nkey_test")
      colnames(data4) <- c("input", "test", "input_id", "test_id", "n_matches", "nkey_in", "nkey_test")
      colnames(data5) <- c("input", "test", "input_id", "test_id", "n_matches", "nkey_in", "nkey_test")
      colnames(data6) <- c("input", "test", "input_id", "test_id", "n_matches", "nkey_in", "nkey_test")
    } else {
      colnames(data1) <- c("input", "test", "input_id", "test_id", "n_matches")
      colnames(data2) <- c("input", "test", "input_id", "test_id", "n_matches")
      colnames(data3) <- c("input", "test", "input_id", "test_id", "n_matches")
      colnames(data4) <- c("input", "test", "input_id", "test_id", "n_matches")
      colnames(data5) <- c("input", "test", "input_id", "test_id", "n_matches")
      colnames(data6) <- c("input", "test", "input_id", "test_id", "n_matches")
    }
    
    data1$snr  <- (data1$n_matches  - mean(data1$n_matches)) /sd(data1$n_matches)
    data1$type <- 1
    data2$snr  <- (data2$n_matches  - mean(data2$n_matches)) /sd(data2$n_matches)
    data2$type <- 2
    data3$snr  <- (data3$n_matches  - mean(data3$n_matches)) /sd(data3$n_matches)
    data3$type <- 3
    data4$snr  <- (data4$n_matches  - mean(data4$n_matches)) /sd(data4$n_matches)
    data4$type <- 4
    data5$snr  <- (data5$n_matches  - mean(data5$n_matches)) /sd(data5$n_matches)
    data5$type <- 5
    data6$snr  <- (data6$n_matches  - mean(data6$n_matches)) /sd(data6$n_matches)
    data6$type <- 6
    
    data_diff <- rbind(data_diff,data1,data2,data3,data4,data5,data6)
  }
  return(data_diff)
}

read_data_orig <- function(patien1,patient2,patient3,patient4,patient5,patient6,patient7) {
  data_diff <- NULL
  for (i in 1:length(patient1)){
    filename1 = patient1[i]
    filename2 = patient2[i]
    filename3 = patient3[i]
    filename4 = patient4[i]
    filename5 = patient5[i]
    filename6 = patient6[i]
    filename7 = patient7[i]
    data1 = read.csv(filename1,header = FALSE, fill = TRUE, sep = " ")
    data2 = read.csv(filename2,header = FALSE, fill = TRUE, sep = " ")
    data3 = read.csv(filename3,header = FALSE, fill = TRUE, sep = " ")
    data4 = read.csv(filename4,header = FALSE, fill = TRUE, sep = " ")
    data5 = read.csv(filename5,header = FALSE, fill = TRUE, sep = " ")
    data6 = read.csv(filename6,header = FALSE, fill = TRUE, sep = " ")
    data7 = read.csv(filename7,header = FALSE, fill = TRUE, sep = " ")
    if (ncol(data1)>5){
      colnames(data1) <- c("input", "test", "input_id", "test_id", "n_matches", "nkey_in", "nkey_test")
      colnames(data2) <- c("input", "test", "input_id", "test_id", "n_matches", "nkey_in", "nkey_test")
      colnames(data3) <- c("input", "test", "input_id", "test_id", "n_matches", "nkey_in", "nkey_test")
      colnames(data4) <- c("input", "test", "input_id", "test_id", "n_matches", "nkey_in", "nkey_test")
      colnames(data5) <- c("input", "test", "input_id", "test_id", "n_matches", "nkey_in", "nkey_test")
      colnames(data6) <- c("input", "test", "input_id", "test_id", "n_matches", "nkey_in", "nkey_test")
      colnames(data7) <- c("input", "test", "input_id", "test_id", "n_matches", "nkey_in", "nkey_test")
    } else {
      colnames(data1) <- c("input", "test", "input_id", "test_id", "n_matches")
      colnames(data2) <- c("input", "test", "input_id", "test_id", "n_matches")
      colnames(data3) <- c("input", "test", "input_id", "test_id", "n_matches")
      colnames(data4) <- c("input", "test", "input_id", "test_id", "n_matches")
      colnames(data5) <- c("input", "test", "input_id", "test_id", "n_matches")
      colnames(data6) <- c("input", "test", "input_id", "test_id", "n_matches")
      colnames(data7) <- c("input", "test", "input_id", "test_id", "n_matches")
    }

    data1$snr  <- (data1$n_matches  - mean(data1$n_matches)) /sd(data1$n_matches)
    data1$type <- "1-1"
    data2$snr  <- (data2$n_matches  - mean(data2$n_matches)) /sd(data2$n_matches)
    data2$type <- "2-2"
    data3$snr  <- (data3$n_matches  - mean(data3$n_matches)) /sd(data3$n_matches)
    data3$type <- "3-3"
    data4$snr  <- (data4$n_matches  - mean(data4$n_matches)) /sd(data4$n_matches)
    data4$type <- "4-4"
    data5$snr  <- (data5$n_matches  - mean(data5$n_matches)) /sd(data5$n_matches)
    data5$type <- "5-5"
    data6$snr  <- (data6$n_matches  - mean(data6$n_matches)) /sd(data6$n_matches)
    data6$type <- "6-6"
    data7$snr  <- (data6$n_matches  - mean(data6$n_matches)) /sd(data6$n_matches)
    data7$type <- "7-7"
    
    data_diff <- rbind(data_diff,data1,data2,data3,data4,data5,data6,data7)
  }
  return(data_diff)
}

read_data_one <- function(patient1,id) {
  data_diff <- NULL
  for (i in 1:length(patient1)){
    filename1 = patient1[i]
    data1 = read.csv(filename1,header = FALSE, fill = TRUE, sep = " ")
    if (ncol(data1)>5){
      colnames(data1) <- c("input", "test", "input_id", "test_id", "n_matches", "nkey_in", "nkey_test")
    } else {
      colnames(data1) <- c("input", "test", "input_id", "test_id", "n_matches")
    }
    
    data1$snr  <- (data1$n_matches  - mean(data1$n_matches)) /sd(data1$n_matches)
    data1$type <- paste0(id,"-",id)
    
    data_diff <- rbind(data_diff,data1)
  }
  return(data_diff)
}