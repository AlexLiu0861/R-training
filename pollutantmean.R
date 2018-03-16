pollutantmean <- function(directory = "specdata", pollutant = "sulfate", id = 1:332) {
    if (grep("specdata", directory) == 1) {
        directory <- c("./specdata/")
    }
    #比对输入的directory变量是否与实际的数据目录相同（毕竟输入的是向量），如是，则帮其转换为目录格式

    original_dataframe <- c()
    #初始化一个空向量。

    all_files <- as.character(list.files(directory))
    file_paths <- paste(directory, all_files, sep = "")
    #寻找specdata文件夹中的所有文件，file_paths将工作区中所有的文件存入同一个向量，以备接下来的循环使用

    for (i in id) {
        current_file <- read.csv(file_paths[i], header = TRUE, sep = ",")
        na_removed <- current_file[!is.na(current_file[, pollutant]), pollutant]
        original_dataframe <-c(original_dataframe,na_removed)
    }
    #主要的代码块由如上的for循环形成。
    #首先，read.csv函数（保留行名称，以逗号隔开）将第i个文件（实质名称为i.csv）存入名为current_file的当前data frame中，其他的操作就可以通过这个data frame展开（这个data frame只存在于函数的环境下，不会出现在全局变量中。）
    #第二步，用is.na()函数判断数据表中pollutant列的观测值是否缺失（pollutant是代号，由用户自行输入实际列名）。而中括号整体上是给current_file的索引，形成一个由非缺失值的行与pollutant列组成的向量，赋值给na_removed向量。
    #第三步，将原来的original_dataframe向量进一步延展，加入新挑选出来的值（python中可以用append实现的东西，R里实现得这么蠢）
    #end sub

    result <- mean(original_dataframe)
    #根据要求，函数返回算术平均值。

    return(round(result, 3))
    #round四舍五入，保留三位小数
}

complete <- function(directory = "specdata", id = 1:332) {
    if (grep("specdata", directory) == 1) {
        directory <- c("./specdata/")
    }

    id_lens <- length(id)
    #这个函数最后输出一个表格，因此需要一个参与for循环赋值的空变量，需要提前计算id的元素数量（向量长度）
    main_frame <- rep(0,id_lens)
    #它是这个函数的输出向量之一，负责nobs部分，长度应为执行者规定的行数（id的元素数）
    all_files <- as.character(list.files(directory))
    file_paths <- paste(directory, all_files, sep = "")

    j <- 1 #用来给main_frame赋值的flag变量，与其他语言不同，R的flag应当从1开始
    for (i in id) {
        current_file <- read.csv(file_paths[i], header=T, sep=",")
        main_frame[j] <- sum(complete.cases(current_file))
        #由于函数要求返回的是完整的案例数，因此使用complete.cases而非is.na来去除缺失值，sum()用于计数。
        j <- j+1
    }
    result <- data.frame(id = id, nobs = main_frame)
    #输出格式为id+nobs，因此由id和main_frame两个向量，分别命名后组成一个data frame。
    return(result)
}

corr <- function(directory, threshold = 0) {
    if (grep("specdata", directory) == 1) {
        directory <- ("./specdata/")
    }

    complete_table <- complete("specdata", 1:332)
    #借用上文中的complete函数，得到所有332个监测站数据的完整案件数表格

    nobs <- complete_table$nobs
    ids <- complete_table$id[nobs > threshold]
    #读取这个表格中的两行，不过id的表格不完整，只读取nob超过阈值时对应的的id

    id_len <- length(ids)
    corr_vector <- rep(0, id_len)

    all_files <- as.character(list.files(directory))
    file_paths <- paste(directory, all_files, sep = "")

    j <- 1
    for (i in ids) {
        current_file <- read.csv(file_paths[i], header = T, sep = ",")
        corr_vector[j] <- cor(current_file$sulfate, current_file$nitrate, use = "complete.obs")
        #cor用于计算两个向量序列的相关系数，use项表示只录入完整的观测值（不接受缺失值）。通过for循环计算了每个表格（监测站数据）的相关关系。
        j <- j + 1
    }
    result <- corr_vector
    return(result)
}
