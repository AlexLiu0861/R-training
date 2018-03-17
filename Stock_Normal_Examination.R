norm_exam <- function(code = "APLE", source = "yahoo", method = "ks") {
#need package "quantmod"
    Origina <- getSymbols(Symbols =  code, src = source, auto.assign = FALSE)
    oridata <- as.data.frame(Origina)
    # Get data of stocks from Yahoo, and change data type(from "zoo" to "data.frame"//

    STOCK.close <- as.vector(oridata[, 4])
    #Subset the data frame, and take the fourth column, or its close price.

    log_rate <- rep(0, length(STOCK.close) - 1)
    for (i in (1:(length(STOCK.close) - 1)))
        log_rate[i] <- log(STOCK.close[i + 1] / STOCK.close[i])
    #Log Yield has been cauculated and saved in the vector "log_rate"

    a <- ks.test(log_rate, "pnorm")
    #Kolmogorov-Smirnov Test
    b <- shapiro.test(log_rate)
    #Shapiro-Wilk Test (W Test)
    if (method == "ks") {
        print(a)
    }
    else if (method == "sw") {
        print(b)
    }
    else {
        qqnorm(log_rate)
        qqline(log_rate)
        #Draw QQ plot to exam.
    }
    #There are 3 main method to examine whether a serie is a normal distribution(KS,SW,QQ)
    #Then we need to print those descritive statistics.
    DS_name <- c("mean", "max", "min", "median", "variance", "std.Dev")
    DS_value <- c(mean(log_rate), max(log_rate), min(log_rate), median(log_rate), var(log_rate), sd(log_rate))
    result2 <- cbind(DS_name, DS_value)

    hist(log_rate,breaks =100)

    return(result2)
}
