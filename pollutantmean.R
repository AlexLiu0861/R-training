pollutantmean <- function(directory = "specdata", pollutant = "sulfate", id = 1:332) {
    if (grep("specdata", directory) == 1) {
        directory <- c("./specdata/")
    }
    #�ȶ������directory�����Ƿ���ʵ�ʵ�����Ŀ¼��ͬ���Ͼ�������������������ǣ������ת��ΪĿ¼��ʽ

    original_dataframe <- c()
    #��ʼ��һ����������

    all_files <- as.character(list.files(directory))
    file_paths <- paste(directory, all_files, sep = "")
    #Ѱ��specdata�ļ����е������ļ���file_paths�������������е��ļ�����ͬһ���������Ա���������ѭ��ʹ��

    for (i in id) {
        current_file <- read.csv(file_paths[i], header = TRUE, sep = ",")
        na_removed <- current_file[!is.na(current_file[, pollutant]), pollutant]
        original_dataframe <-c(original_dataframe,na_removed)
    }
    #��Ҫ�Ĵ���������ϵ�forѭ���γɡ�
    #���ȣ�read.csv���������������ƣ��Զ��Ÿ���������i���ļ���ʵ������Ϊi.csv��������Ϊcurrent_file�ĵ�ǰdata frame�У������Ĳ����Ϳ���ͨ�����data frameչ�������data frameֻ�����ں����Ļ����£����������ȫ�ֱ����С���
    #�ڶ�������is.na()�����ж����ݱ���pollutant�еĹ۲�ֵ�Ƿ�ȱʧ��pollutant�Ǵ��ţ����û���������ʵ�������������������������Ǹ�current_file���������γ�һ���ɷ�ȱʧֵ������pollutant����ɵ���������ֵ��na_removed������
    #����������ԭ����original_dataframe������һ����չ����������ѡ������ֵ��python�п�����appendʵ�ֵĶ�����R��ʵ�ֵ���ô����
    #end sub

    result <- mean(original_dataframe)
    #����Ҫ�󣬺�����������ƽ��ֵ��

    return(round(result, 3))
    #round�������룬������λС��
}

complete <- function(directory = "specdata", id = 1:332) {
    if (grep("specdata", directory) == 1) {
        directory <- c("./specdata/")
    }

    id_lens <- length(id)
    #�������������һ�����������Ҫһ������forѭ����ֵ�Ŀձ�������Ҫ��ǰ����id��Ԫ���������������ȣ�
    main_frame <- rep(0,id_lens)
    #��������������������֮һ������nobs���֣�����ӦΪִ���߹涨��������id��Ԫ������
    all_files <- as.character(list.files(directory))
    file_paths <- paste(directory, all_files, sep = "")

    j <- 1 #������main_frame��ֵ��flag���������������Բ�ͬ��R��flagӦ����1��ʼ
    for (i in id) {
        current_file <- read.csv(file_paths[i], header=T, sep=",")
        main_frame[j] <- sum(complete.cases(current_file))
        #���ں���Ҫ�󷵻ص��������İ����������ʹ��complete.cases����is.na��ȥ��ȱʧֵ��sum()���ڼ�����
        j <- j+1
    }
    result <- data.frame(id = id, nobs = main_frame)
    #�����ʽΪid+nobs�������id��main_frame�����������ֱ����������һ��data frame��
    return(result)
}

corr <- function(directory, threshold = 0) {
    if (grep("specdata", directory) == 1) {
        directory <- ("./specdata/")
    }

    complete_table <- complete("specdata", 1:332)
    #���������е�complete�������õ�����332�����վ���ݵ���������������

    nobs <- complete_table$nobs
    ids <- complete_table$id[nobs > threshold]
    #��ȡ��������е����У�����id�ı���������ֻ��ȡnob������ֵʱ��Ӧ�ĵ�id

    id_len <- length(ids)
    corr_vector <- rep(0, id_len)

    all_files <- as.character(list.files(directory))
    file_paths <- paste(directory, all_files, sep = "")

    j <- 1
    for (i in ids) {
        current_file <- read.csv(file_paths[i], header = T, sep = ",")
        corr_vector[j] <- cor(current_file$sulfate, current_file$nitrate, use = "complete.obs")
        #cor���ڼ��������������е����ϵ����use���ʾֻ¼�������Ĺ۲�ֵ��������ȱʧֵ����ͨ��forѭ��������ÿ�����񣨼��վ���ݣ�����ع�ϵ��
        j <- j + 1
    }
    result <- corr_vector
    return(result)
}