
yao.view.data <- function(data, n = 5, intro = FALSE, silent = FALSE) {
    
    if (silent) {
        sink(tempfile())
    }
    
    if (intro) {
        cat("\033[31myao.view.data 函数的功能介绍:\n\033[97m")
        cat("查看数据，可以是向量、数据框或矩阵。\n")
        cat("\n\033[31myao.view.data 函数的基本用法:\n\033[97m")
        cat("yao.view.data(data, n, intro, silent)\n")
        cat("\n\033[31myao.view.data 函数的参数说明:\n\033[97m")
        cat("1. data: 要查看的数据。\n")
        cat("2. n: 数据的行或列的最大数量。\n")
        cat("3. intro: 显示或隐藏基本用法。\n")
        cat("4. silent: 静默模式。\n")
        cat("\n\033[31myao.view.data 函数的使用示例:\n\033[97m")
        cat("yao.view.data(data = your_data, n = 10)\n\n")
    }
    
    if (is.vector(data)) {
        cat("\033[31m向量的长度:\n\033[97m", length(data), "\n")
        cat("\033[31m向量的前", min(length(data), n), "个元素:\n\033[97m")
        cat(data[1:min(length(data), n)], "\n")
    } else if (is.data.frame(data)) {
        cat("\033[31m数据框 '", deparse(substitute(data)), "' 的维度:\n\033[97m", dim(data)[1], "行 x", dim(data)[2], "列\n")
        num_rows <- min(nrow(data), n)
        num_cols <- min(ncol(data), n)
        cat("\033[31m数据框 '", deparse(substitute(data)), "' 的前", num_rows, "行", num_cols, "列的数据:\n\033[97m")
        print(data[1:num_rows, 1:num_cols])
    } else if (is.matrix(data)) {
        cat("\033[31m矩阵 '", deparse(substitute(data)), "' 的维度:\n\033[97m", dim(data)[1], "行 x", dim(data)[2], "列\n")
        num_rows <- min(nrow(data), n)
        num_cols <- min(ncol(data), n)
        cat("\033[31m矩阵 '", deparse(substitute(data)), "' 的前", num_rows, "行", num_cols, "列的数据:\n\033[97m")
        print(data[1:num_rows, 1:num_cols])
    } else {
        cat("\033[31m不支持的数据类型\n\033[97m")
    }
    
    if (silent) {
        sink()
    }
}
yao.show.data <- function(data, intro = FALSE, silent = FALSE) {
    
    if (silent) {
        sink(tempfile())
    }
    
    if (intro) {
        cat("\033[31myao.show.data 函数的功能介绍:\n\033[97m")
        cat("打开窗口查看数据。\n")
        
        cat("\n\033[31myao.show.data 函数的基本用法:\n\033[97m")
        cat("yao.show.data(data, intro, silent)\n")
        
        cat("\n\033[31myao.show.data 函数的参数说明:\n\033[97m")
        cat("1. data: 要查看的数据。\n")
        cat("2. intro: 显示或隐藏基本用法。\n")
        cat("3. silent: 静默模式。\n")
        
        cat("\n\033[31myao.show.data 函数的使用示例:\n\033[97m")
        cat("yao.show.data(data = your_data)\n\n")
    }
    
    data_name <- deparse(substitute(data))
    View(data, title = data_name)
    
    if (silent) {
        sink()
    }
}
yao.show.vect <- function(data_vect, num, intro = FALSE, silent = FALSE) {
    
    if (!is.vector(data_vect)) {
        stop("注意：该函数只支持向量类型的数据。")
    }
    
    if (silent) {
        sink(tempfile())
    }
    
    if (intro) {
        cat("\033[31myao.show.vect 函数的功能介绍:\n\033[97m")
        cat("查看数据，可以是向量。\n")
        
        cat("\n\033[31myao.show.vect 函数的基本用法:\n\033[97m")
        cat("yao.show.vect(data_vect, num, intro, silent)\n")
        
        cat("\n\033[31myao.show.vect 函数的参数说明:\n\033[97m")
        cat("1. data_vect: 要查看的向量数据。\n")
        cat("2. num: 要查看的数据元素的最大数量或者'max'。\n")
        cat("3. intro: 控制是否开启函数的说明模式。\n")
        cat("4. silent: 静默模式。\n")
        
        cat("\n\033[31myao.show.vect 函数的使用示例:\n\033[97m")
        cat("yao.show.vect(data_vect = your_data_vect, num = 10)\n\n")
    }
    
    if (num == "max") {
        display_length = length(data_vect)
        cat(sprintf("向量 xx 全部元素如下：\n"))
    } else {
        display_length = min(length(data_vect), num)
        cat(sprintf("向量 xx 前 %d 个元素如下：\n", display_length))
    }
    
    for (i in 1:display_length) {
        if (is.numeric(data_vect[i])) {
            cat(sprintf("[%03d] %f\n", i, data_vect[i]))
        } else {
            cat(sprintf("[%03d] %s\n", i, as.character(data_vect[i])))
        }
    }
    
    if (silent) {
        sink()
    }
}
yao.showTXT.vect <- function(data_vect, intro = FALSE, silent = FALSE) {
    
    if (silent) {
        sink(tempfile())
    }
    
    if (intro) {
        cat("\033[31myao.showTXT.vect 函数的功能介绍:\n\033[97m")
        cat("该函数允许用户输入任意向量数据，将向量元素写进以向量名称命名的txt文件，每个元素占一行，写完把相关txt文件存储在 01_Data 文件中，存储完成自动打开txt文件。\n")
        
        cat("\n\033[31myao.showTXT.vect 函数的基本用法:\n\033[97m")
        cat("yao.showTXT.vect(data_vect, intro, silent)\n")
        
        cat("\n\033[31myao.showTXT.vect 函数的参数说明:\n\033[97m")
        cat("1. data_vect: 待查看的向量。\n")
        cat("2. intro: 是否显示函数的介绍。\n")
        cat("3. silent: 静默模式。\n")
        
        cat("\n\033[31myao.showTXT.vect 函数的使用示例:\n\033[97m")
        cat("yao.showTXT.vect(data_vect = c('apple', 'banana'), intro = TRUE)\n\n")
    }
    
    if(!dir.exists("01_Data")) {
        dir.create("01_Data")
    }
    
    file_name <- deparse(substitute(data_vect))
    file_path <- paste0(getwd(), "/01_Data/", file_name, ".txt")
    
    formatted_data <- sprintf("[%04d] \"%s\"", seq_along(data_vect), data_vect)
    writeLines(formatted_data, con = file_path)
    
    if (!file.exists(file_path)) {
        stop(paste("File not written correctly at:", file_path))
    }
    
    shell.exec(file_path)
    
    if (silent) {
        sink()
    }
    
    return(invisible(NULL))
}

yao.check.common <- function(..., intro = FALSE, silent = FALSE) {
    
    if (silent) {
        sink(tempfile())
    }
    
    if (intro) {
        cat("\033[33myao.check.common 函数的功能介绍:\n\033[97m")
        cat("检查输入数据的类型和内容。\n")
        cat("\n\033[33myao.check.common 函数的基本用法:\n\033[97m")
        cat("yao.check.common(..., intro, silent)\n")
        cat("\n\033[33myao.check.common 函数的参数说明:\n\033[97m")
        cat("1. ...: 要检查的数据。\n")
        cat("2. intro: 显示或隐藏基本用法。\n")
        cat("3. silent: 静默模式。\n")
        cat("\n\033[33myao.check.common 函数的使用示例:\n\033[97m")
        cat("yao.check.common(data1, data2)\n\n")
        return(NULL)
    }
    
    args <- list(...)
    for(i in seq_along(args)) {
        data <- args[[i]]
        data_class <- class(data)[1]
        cat("\033[34m第 ", i, "个数据\033[97m\n")
        cat("1. 类型：\033[33m", data_class, "\033[97m\n")
        
        if(is.vector(data)) {
            cat("2. 长度：\033[33m", length(data), "\033[97m\n")
            cat("3. 向量的前 6 个元素：\033[33m", paste(head(data), collapse=" "), "\033[97m\n")
        } else if(is.matrix(data)) {
            cat("2. 维度：\033[33m", paste(dim(data), collapse=" x "), "\033[97m\n")
            cat("\033[33m3. 矩阵的前 6 行 6 列：\033[97m\n")
            print(head(data[,1:6]))
        } else if(is.data.frame(data)) {
            cat("2. 维度：\033[33m", paste(dim(data), collapse=" x "), "\033[97m\n")
            cat("\033[33m3. 数据框的前 6 行 6 列：\033[97m\n")
            print(head(data[,1:6]))
        } else if(is.list(data)) {
            cat("2. 长度：\033[33m", length(data), "\033[97m\n")
            cat("\033[33m3. 一级结构：\033[97m\n")
            print(names(data))
        }
    }
    
    if (silent) {
        sink()
    }
}
yao.check.na <- function(data, intro = FALSE, silent = FALSE) {
    
    if (silent) {
        sink(tempfile())
    }
    
    if (intro) {
        cat("\033[31myao.check.na 函数的功能介绍:\n\033[97m")
        cat("这个函数检查输入的数据是否包含NA值，并返回NA值的位置信息。\n")
        
        cat("\n\033[31myao.check.na 函数的基本用法:\n\033[97m")
        cat("yao.check.na(data, intro, silent)\n")
        
        cat("\n\033[31myao.check.na 函数的参数说明:\n\033[97m")
        cat("1. data: 要检查的数据，可以是向量、数据框或矩阵。\n")
        cat("2. intro: 可选参数，默认为 FALSE。如果为 TRUE，则显示函数的基本用法；如果为 FALSE，则不显示。\n")
        cat("3. silent: 可选参数，默认为 FALSE。如果为 TRUE，则不在控制台打印任何信息；如果为 FALSE，则在控制台输出相关信息。\n")
        
        cat("\n\033[31myao.check.na 函数的使用示例:\n\033[97m")
        cat("yao.check.na(data = your_data)\n\n")
    }
    
    result_list <- list()
    
    if (is.matrix(data) || is.data.frame(data)) {
        if (is.data.frame(data)) {
            data_matrix <- as.matrix(data)
            col_names <- colnames(data)
        } else {
            data_matrix <- data
            col_names <- NULL
        }
        
        na_count <- sum(is.na(data_matrix))
        na_indices_per_column <- vector("list", length = ncol(data_matrix))
        
        if (na_count > 0) {
            result_list$columns_with_na <- which(colSums(is.na(data_matrix)) > 0)
            result_list$na_count_per_column <- colSums(is.na(data_matrix))
            
            for (i in 1:length(result_list$columns_with_na)) {
                col_idx <- result_list$columns_with_na[i]
                row_names_with_na <- row.names(data_matrix)[which(is.na(data_matrix[, col_idx]))]
                na_indices_per_column[[col_idx]] <- list(col_names[col_idx], row_names_with_na)
            }
            
            cat("该数据框 NA 值总个数：", na_count, "\n")
            cat("含有 NA 值的列序号：", result_list$columns_with_na, "\n")
            cat("每列 NA 的数量：", result_list$na_count_per_column, "\n")
            cat("每列 NA 值对应的行名称：\n")
            for (i in 1:length(result_list$columns_with_na)) {
                cat("列", result_list$columns_with_na[i], " (", ifelse(length(col_names) > 0, col_names[result_list$columns_with_na[i]], "N/A"), "): ", na_indices_per_column[[result_list$columns_with_na[i]]][[2]], "\n")
            }
        } else {
            cat("该数据框没有 NA 值\n")
            for (i in 1:ncol(data_matrix)) {
                na_indices_per_column[[i]] <- list(col_names[i], "NA")
            }
        }
        
        result_list$na_indices_per_column <- na_indices_per_column
        
    } else if (is.numeric(data) || is.integer(data) || is.logical(data) || is.factor(data) || is.character(data)) {
        if (anyNA(data)) {
            result_list$na_indices <- which(is.na(data))
            cat("向量中的NA值位置：", result_list$na_indices, "\n")
        } else {
            cat("该向量没有NA值\n")
        }
    } else {
        cat("不支持的数据类型\n")
    }
    
    if (silent) {
        sink()
    }
    
    return(result_list)
}
yao.check.dup <- function(data_vector, show_indices = FALSE, intro = FALSE, silent = FALSE) {
    
    if (silent) {
        sink(tempfile())
    }
    
    if (intro) {
        cat("\033[31myao.check.dup 函数的功能介绍:\n\033[97m")
        cat("这个函数检查一个向量中是否存在重复的值，并根据用户的选择返回重复值的索引或仅返回一个表示是否存在重复值的布尔值。\n")
        
        cat("\n\033[31myao.check.dup 函数的基本用法:\n\033[97m")
        cat("yao.check.dup(data_vector, show_indices, intro, silent)\n")
        
        cat("\n\033[31myao.check.dup 函数的参数说明:\n\033[97m")
        cat("1. data_vector: 要检查的数据向量。\n")
        cat("2. show_indices: 逻辑值，决定是否返回重复值的索引。默认为 FALSE，即不返回索引。\n")
        cat("3. intro: 可选参数，默认为 FALSE。如果为 TRUE，则显示函数的基本用法；如果为 FALSE，则不显示。\n")
        cat("4. silent: 控制是否在控制台显示输出，默认为 FALSE。\n")
        
        cat("\n\033[31myao.check.dup 函数的使用示例:\n\033[97m")
        cat("yao.check.dup(data_vector = c(1, 2, 3, 2, 5), show_indices = TRUE)\n\n")
    }
    
    duplicate_indices <- which(duplicated(data_vector) | duplicated(data_vector, fromLast = TRUE))
    
    if (show_indices) {
        result = duplicate_indices
    } else {
        result = length(duplicate_indices) > 0
    }
    
    if (silent) {
        sink()
    }
    
    return(result)
}
yao.check.class <- function(dataframe, num_cols = ncol(dataframe), intro = FALSE, silent = FALSE) {
    
    if (silent) {
        sink(tempfile())
    }
    
    if (intro) {
        cat("\033[31myao.check.class 函数的功能介绍:\n\033[97m")
        cat("这个函数允许用户查看数据框中每列的数据类型及其前6个值。\n")
        
        cat("\n\033[31myao.check.class 函数的基本用法:\n\033[97m")
        cat("yao.check.class(dataframe, num_cols, intro, silent)\n")
        
        cat("\n\033[31myao.check.class 函数的参数说明:\n\033[97m")
        cat("1. dataframe: 输入的数据框。\n")
        cat("2. num_cols: 要查看的列的数量，默认为数据框的全部列。\n")
        cat("3. intro: 可选参数，默认为 FALSE。如果为 TRUE，则显示函数的基本用法；如果为 FALSE，则不显示。\n")
        cat("4. silent: 控制是否在控制台显示输出，默认为 FALSE。\n")
        
        cat("\n\033[31myao.check.class 函数的使用示例:\n\033[97m")
        cat("yao.check.class(dataframe = your_data, num_cols = 5)\n\n")
    }
    
    cat("\033[34m数据框列数据类型及其值:\n\033[97m")
    data_name <- deparse(substitute(dataframe))
    cat(paste("数据框'", data_name, "'中的列数据类型如下:\n", sep = ""))
    
    for (i in 1:num_cols) {
        col_name <- colnames(dataframe)[i]
        col_data <- dataframe[[i]]
        col_type <- class(col_data)
        col_values <- head(col_data, 6)
        
        cat("\033[31m", paste(i, ". ", col_name, "\033[31m(", col_type, ")\033[97m: ", toString(col_values), "\n", sep = ""))
    }
    
    if (silent) {
        sink()
    }
}
yao.check.obj <- function(obj_data, intro = FALSE, silent = FALSE) {
    
    if (silent) {
        sink(tempfile())
    }
    
    if (intro) {
        cat("\033[31myao.check.obj 函数的功能介绍:\n\033[97m")
        cat("这个函数允许用户查看对象的类、属性、摘要和结构。\n")
        
        cat("\n\033[31myao.check.obj 函数的基本用法:\n\033[97m")
        cat("yao.check.obj(obj_data, intro, silent)\n")
        
        cat("\n\033[31myao.check.obj 函数的参数说明:\n\033[97m")
        cat("1. obj_data: 要查看的对象，例如数据、向量、矩阵等。\n")
        cat("2. intro: 可选参数，默认为 FALSE。如果为 TRUE，则显示函数的基本用法和其他信息；如果为 FALSE，则不显示。\n")
        cat("3. silent: 可选参数，默认为 FALSE。如果为 TRUE，则不输出信息到控制台；如果为 FALSE，则输出。\n")
        
        cat("\n\033[31myao.check.obj 函数的使用示例:\n\033[97m")
        cat("yao.check.obj(obj_data = your_data_object)\n\n")
    }
    
    cat("\033[35m> class(", deparse(substitute(obj_data)), ")\n\033[97m")
    print(class(obj_data))
    
    cat("\033[35m> attributes(", deparse(substitute(obj_data)), ")\n\033[97m")
    print(attributes(obj_data))
    
    cat("\033[35m> summary(", deparse(substitute(obj_data)), ")\n\033[97m")
    print(summary(obj_data))
    
    cat("\033[35m> str(", deparse(substitute(obj_data)), ")\n\033[97m")
    str(obj_data)
    
    if (silent) {
        sink()
    }
}
yao.check.vect <- function(data_vect, intro = FALSE, silent = FALSE) {
    
    if (silent) {
        sink(tempfile())
    }
    
    if (intro) {
        cat("\033[31myao.check.vect 函数的功能介绍:\033[97m\n")
        cat("这个函数检查输入的向量数据的三个信息：向量的类型、向量是否含有NA值、向量是否含有重复值。\n")
        
        cat("\n\033[31myao.check.vect 函数的基本用法:\033[97m\n")
        cat("yao.check.vect(data_vect, intro, silent)\n")
        
        cat("\n\033[31myao.check.vect 函数的参数说明:\033[97m\n")
        cat("1. data_vect: 要检查的向量。\n")
        cat("2. intro: 可选参数，默认为 FALSE。如果为 TRUE，则显示函数的基本用法；如果为 FALSE，则不显示。\n")
        cat("3. silent: 可选参数，默认为 FALSE。如果为 TRUE，则不在控制台打印任何信息；如果为 FALSE，则在控制台输出相关信息。\n")
        
        cat("\n\033[31myao.check.vect 函数的使用示例:\033[97m\n")
        cat("yao.check.vect(data_vect = your_vector)\n\n")
    }
    
    vect_type <- class(data_vect)
    cat("向量类型： \033[33m", vect_type, "\033[97m\n")
    
    has_na <- anyNA(data_vect)
    if (has_na) {
        cat("向量含有 NA 值: \033[33m是\033[97m\n")
    } else {
        cat("向量含有 NA 值: \033[33m否\033[97m\n")
    }
    
    has_duplicate <- any(duplicated(data_vect))
    if (has_duplicate) {
        cat("向量含有重复值: \033[33m是\033[97m\n")
    } else {
        cat("向量含有重复值: \033[33m否\033[97m\n")
    }
    
    if (silent) {
        sink()
    }
}
y.cn <- function(data_frame, num = 5, intro = FALSE, silent = FALSE) {
    if (silent) {
        sink(tempfile())
    }
    
    if (intro) {
        cat("\033[33my.cn 函数的功能介绍:\n\033[97m")
        cat("这个函数允许用户查看数据框的列名。\n")
        
        cat("\n\033[33my.cn 函数的基本用法:\n\033[97m")
        cat("y.cn(data_frame, num, intro, silent)\n")
        
        cat("\n\033[33my.cn 函数的参数说明:\n\033[97m")
        cat("1. data_frame: 输入的数据框。\n")
        cat("2. num: 指定显示多少列名，可以是数字或字符“max”，默认为 5。\n")
        cat("3. intro: 可选参数，默认为 FALSE。如果为 TRUE，则显示函数的基本用法；如果为 FALSE，则不显示。\n")
        cat("4. silent: 可选参数，默认为 FALSE。如果为 TRUE，则不在控制台显示任何输出。\n")
        
        cat("\n\033[33my.cn 函数的使用示例:\n\033[97m")
        cat("y.cn(data_frame = your_data, num = 'max') 或 y.cn(data_frame = your_data, num = 3)\n\n")
    }
    
    if (num == "max") {
        colname = colnames(data_frame)
        displayed_num = length(colname)
    } else {
        colname = head(colnames(data_frame), num)
        displayed_num = min(num, length(colname))
    }
    
    yao.view.data(colname)
    
    cat("\n\033[33m前", displayed_num, "个列名分别是：\n\n\033[97m")
    for(i in seq_along(colname)) {
        cat(paste0(i, ".", colname[i], "\n"))
    }
    cat("\n")
    
    if (silent) {
        sink()
    }
}
y.rn <- function(data_frame, num = 5, intro = FALSE, silent = FALSE) {
    if (silent) {
        sink(tempfile())
    }
    
    if (intro) {
        cat("\033[33my.rn 函数的功能介绍:\n\033[97m")
        cat("这个函数允许用户查看数据框的行名。\n")
        
        cat("\n\033[33my.rn 函数的基本用法:\n\033[97m")
        cat("y.rn(data_frame, num, intro, silent)\n")
        
        cat("\n\033[33my.rn 函数的参数说明:\n\033[97m")
        cat("1. data_frame: 输入的数据框。\n")
        cat("2. num: 指定显示多少行名，可以是数字或字符“max”，默认为 5。\n")
        cat("3. intro: 可选参数，默认为 FALSE。如果为 TRUE，则显示函数的基本用法；如果为 FALSE，则不显示。\n")
        cat("4. silent: 可选参数，默认为 FALSE。如果为 TRUE，则不在控制台显示任何输出。\n")
        
        cat("\n\033[33my.rn 函数的使用示例:\n\033[97m")
        cat("y.rn(data_frame = your_data, num = 'max') 或 y.rn(data_frame = your_data, num = 3)\n\n")
    }
    
    if (num == "max") {
        rowname = rownames(data_frame)
        displayed_num = length(rowname)
    } else {
        rowname = head(rownames(data_frame), num)
        displayed_num = min(num, length(rowname))
    }
    
    yao.view.data(rowname)
    
    cat("\n\033[33m前", displayed_num, "个行名分别是：\n\n\033[97m")
    for(i in seq_along(rowname)) {
        cat(paste0(i, ".", rowname[i], "\n"))
    }
    cat("\n")
    
    if (silent) {
        sink()
    }
}
yao.print.dup <- function(data_vector, intro = FALSE, silent = FALSE) {
    
    if (silent) {
        sink(tempfile())
    }
    
    if (intro) {
        cat("\033[33myao.data.dup 函数的功能介绍:\n\033[97m")
        cat("这个函数检测数据向量中的重复值并返回相关的信息。\n")
        
        cat("\n\033[33myao.data.dup 函数的基本用法:\n\033[97m")
        cat("yao.data.dup(data_vector, intro)\n")
        
        cat("\n\033[33myao.data.dup 函数的参数说明:\n\033[97m")
        cat("1. data_vector: 输入的数据向量。\n")
        cat("2. intro: 可选参数，默认为 FALSE。如果为 TRUE，则显示函数的基本用法；如果为 FALSE，则不显示。\n")
        
        cat("\n\033[33myao.data.dup 函数的使用示例:\n\033[97m")
        cat("yao.data.dup(data_vector = your_data)\n\n")
    }
    
    tmp <- list()
    tmp$FFTT <- duplicated(data_vector)
    tmp$FTTT <- data_vector %in% unique(data_vector[duplicated(data_vector)])
    tmp$idx34 <- which(duplicated(data_vector))
    tmp$idx234 <- which(data_vector %in% unique(data_vector[duplicated(data_vector)]))
    tmp$B <- unique(data_vector[duplicated(data_vector)])
    tmp$BB <- data_vector[duplicated(data_vector)]
    tmp$BBB <- data_vector[which(data_vector %in% unique(data_vector[duplicated(data_vector)]))]

    
    yao.check.obj(tmp)
    
    if (silent) {
        sink()
    }
    
    return(tmp)
}
yao.table <- function(data, ..., intro = FALSE, silent = FALSE) {
    
    if (silent) {
        sink(tempfile())
    }
    
    if (intro) {
        cat("\033[33myao.table 函数的功能介绍:\n\033[97m")
        cat("这个函数允许用户计算数据框或向量的频率表，并可以显示 NA 值的数量。\n")
        
        cat("\n\033[33myao.table 函数的基本用法:\n\033[97m")
        cat("yao.table(data, column_name, intro)\n")
        
        cat("\n\033[33myao.table 函数的参数说明:\n\033[97m")
        cat("1. data: 输入的数据框或向量。\n")
        cat("2. column_name: 当数据为数据框时，需要计算其频率表的列名。\n")
        cat("3. intro: 可选参数，默认为 FALSE。如果为 TRUE，则显示函数的基本用法；如果为 FALSE，则不显示。\n")
        
        cat("\n\033[33myao.table 函数的使用示例:\n\033[97m")
        cat("yao.table(data = your_data, column_name = 'your_column_name')\n")
        cat("或\n")
        cat("yao.table(data = your_vector)\n\n")
    }
    
    column_name <- substitute(...)
    
    if(is.data.frame(data)) {
        result <- table(data[[as.character(column_name)]], useNA = "always")
    } else if (is.vector(data)) {
        result <- table(data, useNA = "always")
    } else {
        stop("data should be either a data.frame or a vector.")
    }
    
    cat("\033[31mtable 的结果是:\n\033[0m")
    print(result)
    
    if (silent) {
        sink()
    }
    
    return(result)
}

yao.convert.vect <- function(data_vect, type, intro = FALSE, silent = FALSE) {
    
    if (silent) {
        sink(tempfile())
    }
    
    if (intro) {
        cat("\033[31myao.convert.vect 函数的功能介绍:\n\033[97m")
        cat("允许用户输入任意向量数据，按照选择的目标向量类型对源向量进行数据转换。\n")
        
        cat("\n\033[31myao.convert.vect 函数的基本用法:\n\033[97m")
        cat("yao.convert.vect(data_vect, type, intro, silent)\n")
        
        cat("\n\033[31myao.convert.vect 函数的参数说明:\n\033[97m")
        cat("1. data_vect: 接受用户待转换的向量。\n")
        cat("2. type: 接受两种选择，分别是“Num”,“Char”，分别代表将源向量数据转换成数值型向量，将源向量数据转换成字符型向量。\n")
        cat("3. intro: 可选参数，默认为 FALSE。如果为 TRUE，则显示函数的基本用法；如果为 FALSE，则不显示。\n")
        cat("4. silent: 可选参数，默认为 FALSE。如果为 TRUE，则所有的输出将被重定向到一个临时文件，不会在控制台上显示；如果为 FALSE，则正常显示。\n")
        
        cat("\n\033[31myao.convert.vect 函数的使用示例:\n\033[97m")
        cat("yao.convert.vect(data_vect = your_vector, type = 'Num')\n\n")
    }
    
    original_type <- class(data_vect)
    
    if (tolower(type) == "num") {
        data_vect <- as.numeric(data_vect)
    } else if (tolower(type) == "char") {
        data_vect <- as.character(data_vect)
    }
    
    cat("数据转换成功，已由", original_type, "类型的数据转换成", class(data_vect), "类型的数据。\n")
    
    if (silent) {
        sink()
    }
    return(data_vect)
}

yao.intersect.all <- function(..., intro = FALSE, silent = FALSE) {
    
    if (silent) {
        sink(tempfile())
    }
    
    if (intro) {
        cat("\033[31myao.intersect.all 函数的功能介绍:\n\033[97m")
        cat("这个函数用于查找所有输入向量的交集。\n")
        
        cat("\n\033[31myao.intersect.all 函数的基本用法:\n\033[97m")
        cat("yao.intersect.all(..., intro, silent)\n")
        
        cat("\n\033[31myao.intersect.all 函数的参数说明:\n\033[97m")
        cat("1. ...: 两个或更多的输入向量。\n")
        cat("2. intro: 可选参数，默认为 FALSE。如果为 TRUE，则显示函数的基本用法；如果为 FALSE，则不显示。\n")
        cat("3. silent: 可选参数，默认为 FALSE。如果为 TRUE，则不在控制台上输出信息；如果为 FALSE，则输出信息。\n")
        
        cat("\n\033[31myao.intersect.all 函数的使用示例:\n\033[97m")
        cat("yao.intersect.all(c(1, 2, 3), c(2, 3, 4), c(3, 4, 5), intro = TRUE, silent = FALSE)\n\n")
    }
    
    args <- list(...)
    if(length(args) < 2) stop("\033[91m错误：至少需要两个向量\033[97m")
    
    res <- args[[1]]
    for(i in 2:length(args)){
        res <- intersect(res, args[[i]])
    }
    
    yao.view.data(res)
    
    if (silent) {
        sink()
    }
    
    return(res)
}
yao.pinp <- function(vector_1, vector_2, ele = FALSE, rev = TRUE, intro = FALSE, silent = TRUE) {
    
    if (silent) {
        sink(tempfile())
    }
    
    if (intro) {
        cat("\033[33myao.pinp 函数的功能介绍:\n\033[97m")
        cat("这个函数用于查找 vector_1 中的元素是否存在于 vector_2 中，并基于 ele 参数决定返回的数据类型。\n")
        
        cat("\n\033[33myao.pinp 函数的基本用法:\n\033[97m")
        cat("yao.pinp(vector_1, vector_2, ele, intro, silent, rev)\n")
        
        cat("\n\033[33myao.pinp 函数的参数说明:\n\033[97m")
        cat("1. vector_1: 第一个输入向量。\n")
        cat("2. vector_2: 第二个输入向量。\n")
        cat("3. ele: 逻辑值，确定返回数据的类型。如果为 TRUE，返回 vector_1 中存在于 vector_2 的元素；如果为 FALSE，返回一个逻辑向量。\n")
        cat("4. intro: 可选参数，默认为 FALSE。如果为 TRUE，则显示函数的基本用法；如果为 FALSE，则不显示。\n")
        cat("5. silent: 可选参数，默认为 FALSE。如果为 TRUE，则不显示任何输出；如果为 FALSE，则正常显示。\n")
        cat("6. rev: 可选参数，默认为 TRUE。如果为 TRUE，则使用 vector_1 %in% vector_2；如果为 FALSE，则使用 vector_2 %in% vector_1。\n")
        
        cat("\n\033[33myao.pinp 函数的使用示例:\n\033[97m")
        cat("yao.pinp(vector_1 = your_data1, vector_2 = your_data2, ele = TRUE, rev = TRUE)\n\n")
    }
    
    if (rev) {
        chose_logi = vector_1 %in% vector_2
    } else {
        chose_logi = vector_2 %in% vector_1
    }
    
    if (ele) {
        chose_ele = vector_1[chose_logi]
        yao.view.data(chose_ele)
        if (silent) {
            sink()
        }
        return(chose_ele)
    } else {
        yao.view.data(chose_logi)
        if (silent) {
            sink()
        }
        return(chose_logi)
    }
}
yao.NA.table <- function(data, col_name = NULL, intro = FALSE, silent = FALSE) {
    
    if (silent) {
        sink(tempfile())
    }
    
    if (intro) {
        cat("\033[33myao.NA.table 函数的功能介绍:\n\033[97m")
        cat("这个函数用于统计输入数据中的 NA 值数量。\n")
        
        cat("\n\033[33myao.NA.table 函数的基本用法:\n\033[97m")
        cat("yao.NA.table(data, col_name, intro, silent)\n")
        
        cat("\n\033[33myao.NA.table 函数的参数说明:\n\033[97m")
        cat("1. data: 输入的数据或向量。\n")
        cat("2. col_name: 如果data是数据框，则需要指定列名。\n")
        cat("3. intro: 可选参数，默认为 FALSE。如果为 TRUE，则显示函数的基本用法；如果为 FALSE，则不显示。\n")
        cat("4. silent: 可选参数，默认为 FALSE。如果为 TRUE，则不显示任何输出；如果为 FALSE，则正常显示。\n")
        
        cat("\n\033[33myao.NA.table 函数的使用示例:\n\033[97m")
        cat("yao.NA.table(data = your_data)\n\n")
    }
    
    if (is.vector(data)) {
        cat(sprintf("\033[31m向量 %s table 的结果:\033[97m", deparse(substitute(data))))
        print(table(is.na(data)))
    } else if (is.data.frame(data)) {
        if (!is.null(col_name)) {
            cat(sprintf("\033[31m数据框 %s，%s 列 table 的结果:\033[97m", deparse(substitute(data)), col_name))
            print(table(is.na(data[[col_name]])))
        } else {
            stop("请为数据框输入提供 'col_name'。")
        }
    }
    
    if (silent) {
        sink()
    }
} 

yao.col2row <- function(data_frame, col_name = NULL, print = FALSE, delcol = TRUE, intro = FALSE, silent = FALSE) {
    
    if (silent) {
        sink(tempfile())
    }
    
    if (intro) {
        cat("\033[33myao.col2row 函数的功能介绍:\n\033[97m")
        cat("这个函数允许用户从数据框中选择一列作为行名，并选择是否删除那一列。\n")
        
        cat("\n\033[33myao.col2row 函数的基本用法:\n\033[97m")
        cat("yao.col2row(data_frame, col_name, print, delcol, intro)\n")
        
        cat("\n\033[33myao.col2row 函数的参数说明:\n\033[97m")
        cat("1. data_frame: 输入的数据框。\n")
        cat("2. col_name: 选择用作行名的列名或列序号。\n")
        cat("3. print: 是否显示列名和对应序号。\n")
        cat("4. delcol: 是否在设置行名后删除选定的列。\n")
        cat("5. intro: 是否显示函数的基本用法和参数说明。\n")
        
        cat("\n\033[33myao.col2row 函数的使用示例:\n\033[97m")
        cat("yao.col2row(data_frame = your_data, col_name = 'your_column_name', print = TRUE, delcol = TRUE)\n\n")
    }
    
    if (print) {
        cat("\033[34m列名和对应的序号:\n\033[97m")
        for (i in 1:length(names(data_frame))) {
            cat(i, ":", names(data_frame)[i], "\n")
        }
        
        selected_index <- as.numeric(readline("请选择要用作行名的列序号: "))
    } else {
        if (is.numeric(col_name)) {
            selected_index <- col_name
        } else {
            selected_index <- which(names(data_frame) == col_name)
            if (length(selected_index) == 0) {
                cat("\033[31m错误：列名不存在。\n\033[97m")
                return(NULL)
            }
        }
    }
    
    if (selected_index < 1 || selected_index > length(names(data_frame))) {
        cat("\033[31m错误：无效的列序号或列名。\n\033[97m")
        return(NULL)
    }
    
    rownames(data_frame) <- data_frame[, selected_index]
    
    if (delcol) {
        data_frame <- data_frame[, -selected_index, drop = FALSE]
    }
    
    yao.view.data(data_frame)
    
    if (silent) {
        sink()
    }
    
    return(data_frame)
}
yao.row2col <- function(data_frame, col_name, intro = FALSE, silent = FALSE) {
    
    if (silent) {
        sink(tempfile())
    }
    
    if (intro) {
        cat("\033[33myao.row2col 函数的功能介绍:\n\033[97m")
        cat("这个函数允许用户将数据框的行名转化为一列，并按指定的列名保存。\n")
        
        cat("\n\033[33myao.row2col 函数的基本用法:\n\033[97m")
        cat("yao.row2col(data_frame, col_name, intro)\n")
        
        cat("\n\033[33myao.row2col 函数的参数说明:\n\033[97m")
        cat("1. data_frame: 输入的数据框。\n")
        cat("2. col_name: 指定的列名，用于保存行名。\n")
        cat("3. intro: 可选参数，默认为 FALSE。如果为 TRUE，则显示函数的基本用法；如果为 FALSE，则不显示。\n")
        
        cat("\n\033[33myao.row2col 函数的使用示例:\n\033[97m")
        cat("yao.row2col(data_frame = your_data, col_name = 'your_column_name')\n\n")
    }
    
    new_data_frame <- data_frame
    new_data_frame[[col_name]] <- rownames(data_frame)
    new_data_frame = yao.Col.arrange(new_data_frame, target_colname = col_name, silent = T)
    
    cat("\033[34m原始数据框内容:\n\033[97m")
    yao.view.data(data_frame)
    
    cat("\033[34m添加行名为列后的数据框内容:\n\033[97m")
    yao.view.data(new_data_frame)
    
    if (silent) {
        sink()
    }
    
    return(new_data_frame)
}
yao.Col.arrange <- function(data_frame, target_colname, intro = FALSE, silent = FALSE) {
    
    if (silent) {
        sink(tempfile())
    }
    
    if (intro) {
        cat("\033[33myao.rearangeCol 函数的功能介绍:\n\033[97m")
        cat("这个函数允许用户重新排列数据框中的列。\n")
        
        cat("\n\033[33myao.rearangeCol 函数的基本用法:\n\033[97m")
        cat("yao.rearangeCol(data_frame, target_colname, intro)\n")
        
        cat("\n\033[33myao.rearangeCol 函数的参数说明:\n\033[97m")
        cat("1. data_frame: 输入的数据框。\n")
        cat("2. target_colname: 需要被重新排列的列名。\n")
        cat("3. intro: 可选参数，默认为 FALSE。如果为 TRUE，则显示函数的基本用法；如果为 FALSE，则不显示。\n")
        
        cat("\n\033[33myao.rearangeCol 函数的使用示例:\n\033[97m")
        cat("yao.rearangeCol(data_frame = your_data, target_colname = 'your_column_name')\n\n")
    }
    
    other_colname = colnames(data_frame)[!(colnames(data_frame) %in% target_colname)]
    rearange_colname = c(target_colname, other_colname)
    data_frame_rearange = data_frame[, rearange_colname]
    yao.view.data(data_frame_rearange)
    
    if (silent) {
        sink()
    }
    
    return(data_frame_rearange)
}
yao.Row.arrange <- function(data_frame, target_rowname, intro = FALSE, silent = FALSE) {
    
    if (silent) {
        sink(tempfile())
    }
    
    if (intro) {
        cat("\033[33myao.rearangeRow 函数的功能介绍:\n\033[97m")
        cat("这个函数允许用户重新排列数据框中的行。\n")
        
        cat("\n\033[33myao.rearangeRow 函数的基本用法:\n\033[97m")
        cat("yao.rearangeRow(data_frame, target_rowname, intro)\n")
        
        cat("\n\033[33myao.rearangeRow 函数的参数说明:\n\033[97m")
        cat("1. data_frame: 输入的数据框。\n")
        cat("2. target_rowname: 需要被重新排列的行名。\n")
        cat("3. intro: 可选参数，默认为 FALSE。如果为 TRUE，则显示函数的基本用法；如果为 FALSE，则不显示。\n")
        
        cat("\n\033[33myao.rearangeRow 函数的使用示例:\n\033[97m")
        cat("yao.rearangeRow(data_frame = your_data, target_rowname = 'your_row_name')\n\n")
    }
    
    other_rowname = rownames(data_frame)[!(rownames(data_frame) %in% target_rowname)]
    rearange_rowname = c(target_rowname, other_rowname)
    data_frame_rearange = data_frame[rearange_rowname, ]
    yao.view.data(data_frame_rearange)
    
    if (silent) {
        sink()
    }
    
    return(data_frame_rearange)
}
yao.cbind <- function(data_frame_1, data_frame_2, intro = FALSE, silent = FALSE) {
    
    if (silent) {
        sink(tempfile())
    }
    
    if (intro) {
        cat("\033[33myao.cbind 函数的功能介绍:\n\033[97m")
        cat("这个函数允许用户将两个数据框按列合并。\n")
        
        cat("\n\033[33myao.cbind 函数的基本用法:\n\033[97m")
        cat("yao.cbind(data_frame_1, data_frame_2, intro)\n")
        
        cat("\n\033[33myao.cbind 函数的参数说明:\n\033[97m")
        cat("1. data_frame_1: 输入的第一个数据框。\n")
        cat("2. data_frame_2: 输入的第二个数据框。\n")
        cat("3. intro: 可选参数，默认为 FALSE。如果为 TRUE，则显示函数的基本用法；如果为 FALSE，则不显示。\n")
        
        cat("\n\033[33myao.cbind 函数的使用示例:\n\033[97m")
        cat("yao.cbind(data_frame_1 = your_first_data, data_frame_2 = your_second_data)\n\n")
    }
    
    new_data_frame = cbind(data_frame_1, data_frame_2)
    
    yao.view.data(new_data_frame)
    
    if (silent) {
        sink()
    }
    
    return(new_data_frame)
}
yao.rbind <- function(data_frame_1, data_frame_2, intro = FALSE, silent = FALSE) {
    
    if (silent) {
        sink(tempfile())
    }
    
    if (intro) {
        cat("\033[33myao.rbind 函数的功能介绍:\n\033[97m")
        cat("这个函数允许用户将两个数据框按行合并。\n")
        
        cat("\n\033[33myao.rbind 函数的基本用法:\n\033[97m")
        cat("yao.rbind(data_frame_1, data_frame_2, intro)\n")
        
        cat("\n\033[33myao.rbind 函数的参数说明:\n\033[97m")
        cat("1. data_frame_1: 输入的第一个数据框。\n")
        cat("2. data_frame_2: 输入的第二个数据框。\n")
        cat("3. intro: 可选参数，默认为 FALSE。如果为 TRUE，则显示函数的基本用法；如果为 FALSE，则不显示。\n")
        
        cat("\n\033[33myao.rbind 函数的使用示例:\n\033[97m")
        cat("yao.rbind(data_frame_1 = your_first_data, data_frame_2 = your_second_data)\n\n")
    }
    
    new_data_frame = rbind(data_frame_1, data_frame_2)
    
    yao.view.data(new_data_frame)
    
    if (silent) {
        sink()
    }
    
    return(new_data_frame)
}
yao.DFCol.reName <- function(data_frame, source_name, targer_name, intro = FALSE, silent = FALSE) {
    
    if (silent) {
        sink(tempfile())
    }
    
    if (intro) {
        cat("\033[33myao.DFCol.reName 函数的功能介绍:\n\033[97m")
        cat("这个函数允许用户根据列名或列索引重命名数据框中的某列。\n")
        cat("\n\033[33myao.DFCol.reName 函数的基本用法:\n\033[97m")
        cat("yao.DFCol.reName(data_frame, source_name, targer_name, intro, silent)\n")
        cat("\n\033[33myao.DFCol.reName 函数的参数说明:\n\033[97m")
        cat("1. data_frame: 输入的数据框。\n")
        cat("2. source_name: 要重命名的列的原名称或索引。\n")
        cat("3. targer_name: 要将 source_name 更改为的新名称。\n")
        cat("4. intro: 可选参数，默认为 FALSE。如果为 TRUE，则显示函数的基本用法；如果为 FALSE，则不显示。\n")
        cat("5. silent: 可选参数，默认为 FALSE。如果为 TRUE，则不会显示任何输出；如果为 FALSE，则会显示输出。\n")
        cat("\n\033[33myao.DFCol.reName 函数的使用示例:\n\033[97m")
        cat("yao.DFCol.reName(data_frame = your_data, source_name = 'old_name', targer_name = 'new_name')\n\n")
    }
    
    if (is.numeric(source_name)) {
        index <- source_name
    } else {
        index <- match(source_name, colnames(data_frame))
    }
    
    cat("\033[34m原数据框的列名:\n\033[97m")
    print(colnames(data_frame))
    
    reName_data_frame <- data_frame
    colnames(reName_data_frame)[index] <- targer_name
    cat("\033[34m新数据框的列名:\n\033[97m")
    print(colnames(reName_data_frame))
    
    yao.view.data(reName_data_frame)
    
    if (silent) {
        sink()
    }
    
    return(reName_data_frame)
}
yao.creNewCol <- function(data_frame, value_list, intro = FALSE, silent = FALSE) {
    
    if (silent) {
        sink(tempfile())
    }
    
    if (intro) {
        cat("\033[33myao.creNewCol 函数的功能介绍:\n\033[97m")
        cat("这个函数允许用户为数据框添加新列。\n")
        
        cat("\n\033[33myao.creNewCol 函数的基本用法:\n\033[97m")
        cat("yao.creNewCol(data_frame, value_list, intro, silent)\n")
        
        cat("\n\033[33myao.creNewCol 函数的参数说明:\n\033[97m")
        cat("1. data_frame: 输入的数据框。\n")
        cat("2. value_list: 包含新列数据的列表，列表的名字为新列的名字。\n")
        cat("3. intro: 可选参数，默认为 FALSE。如果为 TRUE，则显示函数的基本用法；如果为 FALSE，则不显示。\n")
        cat("4. silent: 可选参数，默认为 FALSE。如果为 TRUE，则不显示任何输出；如果为 FALSE，则显示所有输出。\n")
        
        cat("\n\033[33myao.creNewCol 函数的使用示例:\n\033[97m")
        cat("yao.creNewCol(data_frame = your_data, value_list = your_value_list, intro = TRUE)\n\n")
    }
    
    cat("\033[31m数据信息：\n\033[97m")
    cat(sprintf("源数据框有 %d 行 %d 列\n", nrow(data_frame), ncol(data_frame)))
    
    new_data_frame <- data_frame
    for (i in seq_along(value_list)) {
        col_name <- names(value_list)[i]
        new_data_frame[[col_name]] <- value_list[[i]]
    }
    
    cat("\033[31m数据更新：\n\033[97m")
    cat(sprintf("新数据框有 %d 行 %d 列\n", nrow(new_data_frame), ncol(new_data_frame)))
    
    yao.view.data(new_data_frame)
    
    if (silent) {
        sink()
    }
    
    return(new_data_frame)
}
yao.OrderDF.byCol <- function(data_frame, col_name, decreasing = T, intro = FALSE, silent = FALSE) {
    
    if (silent) {
        sink(tempfile())
    }
    
    if (intro) {
        cat("\033[33myao.OrderDFr.byCol 函数的功能介绍:\n\033[97m")
        cat("这个函数允许用户根据指定的列来对数据框进行排序。\n")
        
        cat("\n\033[33myao.OrderDFr.byCol 函数的基本用法:\n\033[97m")
        cat("yao.OrderDFr.byCol(data_frame, col_name, decreasing, intro)\n")
        
        cat("\n\033[33myao.OrderDFr.byCol 函数的参数说明:\n\033[97m")
        cat("1. data_frame: 输入的数据框。\n")
        cat("2. col_name: 需要进行排序的列名。\n")
        cat("3. decreasing: 排序的方式，默认为 TRUE，表示降序。\n")
        cat("4. intro: 可选参数，默认为 FALSE。如果为 TRUE，则显示函数的基本用法；如果为 FALSE，则不显示。\n")
        
        cat("\n\033[33myao.OrderDFr.byCol 函数的使用示例:\n\033[97m")
        cat("yao.OrderDFr.byCol(data_frame = your_data, col_name = 'your_column_name', decreasing = T/F)\n\n")
    }
    
    new_data_frame <- data_frame[order(data_frame[[col_name]], decreasing = decreasing), ]
    
    cat("\033[34m原始数据框:\n\033[97m")
    yao.view.data(data_frame)
    cat("\033[34m新数据框:\n\033[97m")
    yao.view.data(new_data_frame)
    
    if (silent) {
        sink()
    }
    
    return(new_data_frame)
}
yao.DFRow.rmDup <- function(data_frame, col_name, intro = FALSE, silent = FALSE) {
    
    if (silent) {
        sink(tempfile())
    }
    
    if (intro) {
        cat("\033[33myao.DFRow.rmDup 函数的功能介绍:\n\033[97m")
        cat("这个函数允许用户删除指定列中的重复行。\n")
        
        cat("\n\033[33myao.DFRow.rmDup 函数的基本用法:\n\033[97m")
        cat("yao.DFRow.rmDup(data_frame, col_name, intro, silent)\n")
        
        cat("\n\033[33myao.DFRow.rmDup 函数的参数说明:\n\033[97m")
        cat("1. data_frame: 输入的数据框。\n")
        cat("2. col_name: 需要删除重复行的列的名称。\n")
        cat("3. intro: 可选参数，默认为 FALSE。如果为 TRUE，则显示函数的基本用法；如果为 FALSE，则不显示。\n")
        cat("4. silent: 可选参数，默认为 FALSE。如果为 TRUE，则不会显示任何输出；如果为 FALSE，则会显示输出。\n")
        
        cat("\n\033[33myao.DFRow.rmDup 函数的使用示例:\n\033[97m")
        cat("yao.DFRow.rmDup(data_frame = your_data, col_name = 'gene_SYMBOL')\n\n")
    }
    
    cat("\033[34m原数据框的行数:\n\033[97m")
    print(nrow(data_frame))
    
    new_data_frame <- data_frame[!duplicated(data_frame[, col_name]), ]
    
    cat("\033[34m移除重复行后的数据框行数:\n\033[97m")
    print(nrow(new_data_frame))
    
    yao.view.data(new_data_frame)
    
    if (silent) {
        sink()
    }
    
    return(new_data_frame)
}

yao.del.DFCol.byCN <- function(data_frame, target_colname, intro = FALSE, silent = FALSE) {
    
    if (silent) {
        sink(tempfile())
    }
    
    if (intro) {
        cat("\033[33myao.del.DFCol.byCN 函数的功能介绍:\n\033[97m")
        cat("这个函数允许用户从数据框中删除指定的列。\n")
        
        cat("\n\033[33myao.del.DFCol.byCN 函数的基本用法:\n\033[97m")
        cat("yao.del.DFCol.byCN(data_frame, target_colname, intro, silent)\n")
        
        cat("\n\033[33myao.del.DFCol.byCN 函数的参数说明:\n\033[97m")
        cat("1. data_frame: 输入的数据框。\n")
        cat("2. target_colname: 指定要删除的列名或列名的向量。\n")
        cat("3. intro: 可选参数，默认为 FALSE。如果为 TRUE，则显示函数的基本用法；如果为 FALSE，则不显示。\n")
        cat("4. silent: 可选参数，默认为 FALSE。如果为 TRUE，则不会显示任何输出；如果为 FALSE，则会显示输出。\n")
        
        cat("\n\033[33myao.del.DFCol.byCN 函数的使用示例:\n\033[97m")
        cat("yao.del.DFCol.byCN(data_frame = your_data, target_colname = c('column1', 'column2'))\n\n")
    }
    
    data_frame = data_frame[, !(colnames(data_frame) %in% target_colname)]
    yao.view.data(data_frame)
    
    if (silent) {
        sink()
    }
    
    return(data_frame)
}
yao.del.DFRow.byRN <- function(data_frame, target_rowname, intro = FALSE, silent = FALSE) {
    if (silent) {
        sink(tempfile())
    }
    
    if (intro) {
        cat("\033[33myao.delRN 函数的功能介绍:\n\033[97m")
        cat("这个函数允许用户从数据框中删除指定的行。\n")
        
        cat("\n\033[33myao.delRN 函数的基本用法:\n\033[97m")
        cat("yao.delRN(data_frame, target_rowname, intro)\n")
        
        cat("\n\033[33myao.delRN 函数的参数说明:\n\033[97m")
        cat("1. data_frame: 输入的数据框。\n")
        cat("2. target_rowname: 指定要删除的行名或行名的向量。\n")
        cat("3. intro: 可选参数，默认为 FALSE。如果为 TRUE，则显示函数的基本用法；如果为 FALSE，则不显示。\n")
        
        cat("\n\033[33myao.delRN 函数的使用示例:\n\033[97m")
        cat("yao.delRN(data_frame = your_data, target_rowname = c('row1', 'row2'))\n\n")
    }
    
    data_frame = data_frame[!(rownames(data_frame) %in% target_rowname), ]
    yao.view.data(data_frame)
    
    if (silent) {
        sink()
    }
    
    return(data_frame)
}
yao.kep.DFCol.byCN <- function(data_frame, target_colname, intro = FALSE, silent = FALSE) {
    
    if (silent) {
        sink(tempfile())
    }
    
    if (intro) {
        cat("\033[33myao.kep.DFCol.byCN 函数的功能介绍:\n\033[97m")
        cat("这个函数允许用户从数据框中保留指定的列。\n")
        
        cat("\n\033[33myao.kep.DFCol.byCN 函数的基本用法:\n\033[97m")
        cat("yao.kep.DFCol.byCN(data_frame, target_colname, intro, silent)\n")
        
        cat("\n\033[33myao.kep.DFCol.byCN 函数的参数说明:\n\033[97m")
        cat("1. data_frame: 输入的数据框。\n")
        cat("2. target_colname: 指定要保留的列名或列名的向量。\n")
        cat("3. intro: 可选参数，默认为 FALSE。如果为 TRUE，则显示函数的基本用法；如果为 FALSE，则不显示。\n")
        cat("4. silent: 可选参数，默认为 FALSE。如果为 TRUE，则不会显示任何输出；如果为 FALSE，则会显示输出。\n")
        
        cat("\n\033[33myao.kep.DFCol.byCN 函数的使用示例:\n\033[97m")
        cat("yao.kep.DFCol.byCN(data_frame = your_data, target_colname = c('column1', 'column2'))\n\n")
    }
    
    data_frame = data_frame[, target_colname]
    yao.view.data(data_frame)
    
    if (silent) {
        sink()
    }
    
    return(data_frame)
}
yao.kep.DFRow.byRN <- function(data_frame, target_rowname, intro = FALSE, silent = FALSE) {
    
    if (silent) {
        sink(tempfile())
    }
    
    if (intro) {
        cat("\033[33myao.del.DFRow.byRN 函数的功能介绍:\n\033[97m")
        cat("这个函数允许用户从数据框中删除指定的行。\n")
        
        cat("\n\033[33myao.del.DFRow.byRN 函数的基本用法:\n\033[97m")
        cat("yao.del.DFRow.byRN(data_frame, target_rowname, intro, silent)\n")
        
        cat("\n\033[33myao.del.DFRow.byRN 函数的参数说明:\n\033[97m")
        cat("1. data_frame: 输入的数据框。\n")
        cat("2. target_rowname: 指定要删除的行名或行名的向量。\n")
        cat("3. intro: 可选参数，默认为 FALSE。如果为 TRUE，则显示函数的基本用法；如果为 FALSE，则不显示。\n")
        cat("4. silent: 可选参数，默认为 FALSE。如果为 TRUE，则不会显示任何输出；如果为 FALSE，则会显示输出。\n")
        
        cat("\n\033[33myao.del.DFRow.byRN 函数的使用示例:\n\033[97m")
        cat("yao.del.DFRow.byRN(data_frame = your_data, target_rowname = c('row1', 'row2'))\n\n")
    }
    
    data_frame = data_frame[(rownames(data_frame) %in% target_rowname), ]
    yao.view.data(data_frame)
    
    if (silent) {
        sink()
    }
    
    return(data_frame)
}

yao.subset.row <- function(data_frame, ..., intro = FALSE, silent = FALSE) {
    
    if (silent) {
        sink(tempfile())
    }
    
    if (intro) {
        cat("\033[33myao.kepDFrow.byCol 函数的功能介绍:\n\033[97m")
        cat("这个函数用于根据多个条件过滤数据框的行。\n")
        
        cat("\n\033[33myao.kepDFrow.byCol 函数的基本用法:\n\033[97m")
        cat("yao.kepDFrow.byCol(data_frame, ...)\n")
        
        cat("\n\033[33myao.kepDFrow.byCol 函数的参数说明:\n\033[97m")
        cat("1. data_frame: 输入的数据框。\n")
        cat("2. ...: 任意数量的过滤条件。\n")
        cat("3. intro: 可选参数，默认为 FALSE。如果为 TRUE，则显示函数的基本用法；如果为 FALSE，则不显示。\n")
        cat("4. silent: 可选参数，默认为 FALSE。如果为 TRUE，则不显示任何输出；如果为 FALSE，则正常显示。\n")
        
        cat("\n\033[33myao.kepDFrow.byCol 函数的使用示例:\n\033[97m")
        cat("yao.kepDFrow.byCol(data_frame = your_data, your_conditions...)\n\n")
    }
    
    conditions <- lapply(as.list(substitute(list(...)))[-1L], function(expr) {
        eval(expr, envir = data_frame)
    })
    
    first_condition <- eval(conditions[[1]], envir = data_frame)
    final_conditions <- first_condition
    
    if (length(conditions) > 1) {
        for (condition in conditions[-1]) {
            current_condition <- eval(condition, envir = data_frame)
            final_conditions <- final_conditions & current_condition
        }
    }
    
    new_data_frame <- data_frame[final_conditions, ]
    
    cat("\033[34m原始数据框：\n\033[0m")
    yao.view.data(data_frame)
    
    cat("\033[34m新数据框：\n\033[0m")
    yao.view.data(new_data_frame)
    
    if (silent) {
        sink()
    }
    
    return(new_data_frame)
}
yao.subset.col <- function(data_frame, ..., intro = FALSE, silent = FALSE) {
    
    if (silent) {
        sink(tempfile())
    }
    
    if (intro) {
        cat("\033[33myao.subset.col 函数的功能介绍:\n\033[97m")
        cat("这个函数用于根据列名从数据框中提取指定的列。\n")
        
        cat("\n\033[33myao.subset.col 函数的基本用法:\n\033[97m")
        cat("yao.subset.col(data_frame, ...)\n")
        
        cat("\n\033[33myao.subset.col 函数的参数说明:\n\033[97m")
        cat("1. data_frame: 输入的数据框。\n")
        cat("2. ...: 需要提取的列的列名。\n")
        cat("3. intro: 可选参数，默认为 FALSE。如果为 TRUE，则显示函数的基本用法；如果为 FALSE，则不显示。\n")
        cat("4. silent: 可选参数，默认为 FALSE。如果为 TRUE，则不显示任何输出；如果为 FALSE，则正常显示。\n")
        
        cat("\n\033[33myao.subset.col 函数的使用示例:\n\033[97m")
        cat("yao.subset.col(data_frame = your_data, column_names...)\n\n")
    }
    
    column_names <- as.character(substitute(list(...))[-1L])
    
    new_data_frame <- data_frame[, column_names, drop = FALSE]
    
    cat("\033[34m原始数据框：\n\033[0m")
    yao.view.data(data_frame)
    
    cat("\033[34m新数据框：\n\033[0m")
    yao.view.data(new_data_frame)
    
    if (silent) {
        sink()
    }
    
    return(new_data_frame)
}
yao.subset.NumVect <- function(data_vector, ..., result = "value", intro = FALSE, silent = FALSE) {
    
    # 检查 `stringdist` 包是否安装
    if (!requireNamespace("stringdist", quietly = TRUE)) {
        stop("需要 'stringdist' 包来进行模糊匹配。请先安装该包。")
    }
    
    # 模糊匹配 result 参数
    result_compare <- tolower(result)  
    valid_options <- c("value", "index", "logic")
    distances <- stringdist::stringdistmatrix(result_compare, valid_options)
    min_distance_index <- which.min(distances)
    chosen_option <- valid_options[min_distance_index]
    
    if (distances[min_distance_index] > 2) {
        stop("无效的 'result' 参数值。请选择 'value', 'index', 或 'logic' 之一（不区分大小写）或检查您的拼写。")
    }
    
    if (silent) {
        sink(tempfile())
    }
    
    if (intro) {
        cat("\033[33myao.subset.NumVect 函数的功能介绍:\n\033[97m")
        cat("这个函数允许用户根据提供的条件从数值型向量中筛选元素。\n")
        
        cat("\n\033[33myao.subset.NumVect 函数的基本用法:\n\033[97m")
        cat("yao.subset.NumVect(data_vector, ... , result, intro, silent)\n")
        
        cat("\n\033[33myao.subset.NumVect 函数的参数说明:\n\033[97m")
        cat("1. data_vector: 输入的数值型向量。\n")
        cat("2. ... : 一个或多个筛选条件。\n")
        cat("3. result: 输出结果的类型('value', 'index', 或 'logic')。\n")
        cat("4. intro: 是否展示函数的介绍，默认为 FALSE。\n")
        cat("5. silent: 是否在屏幕上显示结果，默认为 FALSE。\n")
        
        cat("\n\033[33myao.subset.NumVect 函数的使用示例:\n\033[97m")
        cat("yao.subset.NumVect(data_vector = c(1, 2, 3, 4, 5), tmp > 3, result = 'value')\n\n")
    }
    
    if(!is.numeric(data_vector)) {
        stop("提供的向量不是数值型。请提供一个数值型向量。")
    }
    
    conditions <- as.list(substitute(list(...)))[-1L]
    result_index <- rep(TRUE, length(data_vector))
    
    for(condition in conditions){
        if (is.call(condition) && condition[[1]] == as.name("=")) {
            condition[[1]] <- as.name("==")
        }
        
        current_index <- eval(condition, envir = list(tmp = data_vector))
        result_index <- result_index & current_index
    }
    
    switch(chosen_option,
           value = {
               result_output <- data_vector[result_index]
           },
           index = {
               result_output <- which(result_index)
           },
           logic = {
               result_output <- result_index
           },
           stop("无效的 'result' 参数值。请选择 'value', 'index', 或 'logic' 之一。")
    )
    
    if (!silent && length(result_output) == 0) {
        cat("\033[31m警告:\033[0m 根据提供的条件，没有找到满足的子集。\n")
    }
    
    if (!silent) {
        cat("\033[34m按照条件筛选的结果显示如下：\n\033[0m")
        cat("\033[31mResult：\n\033[0m")
        print(result_output)
    }
    
    if (silent) {
        sink()
    }
    
    return(result_output)
}
yao.subset.list <- function(data_list, chose, intro = FALSE, silent = FALSE) {
    
    if (silent) {
        sink(tempfile())
    }
    
    if (intro) {
        cat("\033[33myao.subset.list 函数的功能介绍:\n\033[97m")
        cat("这个函数允许用户根据所选的条件从列表中选择子集。\n")
        
        cat("\n\033[33myao.subset.list 函数的基本用法:\n\033[97m")
        cat("yao.subset.list(data_list, chose, intro)\n")
        
        cat("\n\033[33myao.subset.list 函数的参数说明:\n\033[97m")
        cat("1. data_list: 输入的列表。\n")
        cat("2. chose: 选择的条件，可以是数值、逻辑或字符。\n")
        cat("3. intro: 可选参数，默认为 FALSE。如果为 TRUE，则显示函数的基本用法；如果为 FALSE，则不显示。\n")
        
        cat("\n\033[33myao.subset.list 函数的使用示例:\n\033[97m")
        cat("yao.subset.list(data_list = your_list, chose = your_choice)\n\n")
    }
    
    if (is.numeric(chose)) {
        new_data_list = data_list[chose]
    } else if (is.logical(chose)) {
        if(length(chose) != length(data_list)) {
            stop("When 'chose' is logical, it should have the same length as 'data_list'.")
        }
        new_data_list = data_list[chose]
    } else if (is.character(chose)) {
        new_data_list = data_list[chose]
    } else {
        stop("'chose' must be numeric, logical, or character.")
    }
    
    cat("\033[31m原始列表长度为：\033[0m", length(data_list), "\n")
    cat("\033[31m新生列表长度为：\033[0m", length(new_data_list), "\n")
    
    if (silent) {
        sink()
    }
    
    return(new_data_list)
}
yao.subset.CharVect <- function(data_vector, pattern, Aa2aa = TRUE, NO.reg = FALSE, 
                                result = "logic", intro = FALSE, silent = FALSE) {
    
    if (!is.character(data_vector)) {
        stop("data_vector 必须是字符向量")
    }
    
    if (!requireNamespace("stringdist", quietly = TRUE)) {
        stop("需要 'stringdist' 包来进行模糊匹配。请先安装该包。")
    }
    
    result_compare <- tolower(result)  
    valid_options <- c("value", "index", "logic")
    distances <- stringdist::stringdistmatrix(result_compare, valid_options)
    min_distance_index <- which.min(distances)
    chosen_option <- valid_options[min_distance_index]
    
    if (distances[min_distance_index] > 2) {
        stop("无效的 'result' 参数值。请选择 'value', 'index', 或 'logic' 之一（不区分大小写）或检查您的拼写。")
    }
    
    if (silent) {
        sink(tempfile())
    }
    
    if (intro) {
        cat("\033[33myao.subset.CharVect 函数的功能介绍:\n\033[97m")
        cat("这个函数允许用户根据指定模式从字符向量中筛选元素。\n")
        
        cat("\n\033[33myao.subset.CharVect 函数的基本用法:\n\033[97m")
        cat("yao.subset.CharVect(data_vector, pattern, Aa2aa, NO.reg, result, intro, silent)\n")
        
        cat("\n\033[33myao.subset.CharVect 函数的参数说明:\n\033[97m")
        cat("1. data_vector: 输入的字符向量。\n")
        cat("2. pattern: 用于筛选的模式。\n")
        cat("3. Aa2aa: 是否忽略大小写，默认为 TRUE。\n")
        cat("4. NO.reg: 是否使用固定匹配，默认为 FALSE。\n")
        cat("5. result: 筛选的结果类型('value', 'index', 或 'logic')。\n")
        cat("6. intro: 是否展示函数的介绍，默认为 FALSE。\n")
        cat("7. silent: 是否在屏幕上显示结果，默认为 FALSE。\n")
        
        cat("\n\033[33myao.subset.CharVect 函数的使用示例:\n\033[97m")
        cat("yao.subset.CharVect(data_vector = c('apple', 'banana', 'cherry'), pattern = 'app', result = 'value')\n\n")
    }
    
    if (chosen_option == "value") {
        result_output <- grep(pattern, data_vector, ignore.case = Aa2aa, fixed = NO.reg, value = TRUE)
    } else if (chosen_option == "index") {
        result_output <- grep(pattern, data_vector, ignore.case = Aa2aa, fixed = NO.reg)
    } else if (chosen_option == "logic") {
        result_output <- grepl(pattern, data_vector, ignore.case = Aa2aa, fixed = NO.reg)
    }
    
    if (!silent) {
        cat("\033[34m按照条件筛选的结果显示如下：\n\033[0m")
        cat("\033[31mResult：\n\033[0m")
        print(result_output)
    }
    
    if (silent) {
        sink()
    }
    
    return(result_output)
}

yao.findColCont.byCha <- function(data_frame, char_str, intro = FALSE) {
    if (intro) {
        cat("\033[33myao.findColCont.byCha 函数的功能介绍:\n\033[97m")
        cat("这个函数会找到数据框中包含指定字符的列名。\n")
        
        cat("\n\033[33myao.findColCont.byCha 函数的基本用法:\n\033[97m")
        cat("yao.findColCont.byCha(data_frame, char_str, intro)\n")
        
        cat("\n\033[33myao.findColCont.byCha 函数的参数说明:\n\033[97m")
        cat("1. data_frame: 输入的数据框。\n")
        cat("2. char_str: 要在数据框中搜索的字符。\n")
        cat("3. intro: 可选参数，默认为 FALSE。如果为 TRUE，则显示函数的基本用法；如果为 FALSE，则不显示。\n")
        
        cat("\n\033[33myao.findColCont.byCha 函数的使用示例:\n\033[97m")
        cat("yao.findColCont.byCha(data_frame = your_data, char_str = \"your_character\")\n\n")
    }
    
    result.logi <- apply(data_frame, 2, function(x) any(grepl(char_str, x, ignore.case = TRUE)))
    result.ele <- colnames(data_frame)[result.logi]
    result.DF <- data_frame[, result.logi]
    result.list <- list()
    result.list[["result.logi"]] <- result.logi
    result.list[["result.ele"]] <- result.ele
    result.list[["result.DF"]] <- result.DF
    
    return(result.list)
}
yao.MultiGrep <- function(char_data, match_list, result = "index", Aa2aa = TRUE, No.reg = FALSE, intro = FALSE, silent = FALSE) {
    
    if (silent) {
        sink(tempfile())
    }
    
    if (intro) {
        cat("\033[33myao.grep 函数的功能介绍:\n\033[97m")
        cat("这个函数允许用户匹配字符串。\n")
        
        cat("\n\033[33myao.grep 函数的基本用法:\n\033[97m")
        cat("yao.grep(char_data, match_list, result, Aa2aa, No.reg)\n")
        
        cat("\n\033[33myao.grep 函数的参数说明:\n\033[97m")
        cat("1. char_data: 输入的字符数据。\n")
        cat("2. match_list: 匹配列表。\n")
        cat("3. result: 结果的类型，可以是 'index'、'logic' 或 'value'。\n")
        cat("4. Aa2aa: 是否忽略大小写，可选，默认为 FALSE。\n")
        cat("5. No.reg: 是否不使用正则表达式匹配，可选，默认为 FALSE。\n")
        cat("6. intro: 是否显示函数介绍，可选，默认为 FALSE。\n")
        
        cat("\n\033[33myao.grep 函数的使用示例:\n\033[97m")
        cat("yao.grep(char_data = c('apple', 'banana', 'cherry'), match_list = c('a$', 'b'))\n\n")
    }
    
    ignore_case <- if (Aa2aa) "ignore" else "match"
    
    if (No.reg) {
        matches <- sapply(match_list, function(pattern) grep(pattern, char_data, fixed = TRUE, ignore.case = (ignore_case == "ignore")))
    } else {
        matches <- sapply(match_list, function(pattern) grep(pattern, char_data, ignore.case = (ignore_case == "ignore")))
    }
    
    if (tolower(result) == "index") {
        res <- unlist(matches, use.names = FALSE)
        names(res) <- char_data[res]
    } else if (tolower(result) == "logic") {
        res <- sapply(char_data, function(data) any(sapply(match_list, function(pattern) grepl(pattern, data, ignore.case = (ignore_case == "ignore"), fixed = No.reg))))
    } else if (tolower(result) == "value") {
        res <- unlist(lapply(matches, function(m) char_data[m]), use.names = FALSE)
        names(res) <- char_data[unlist(matches, use.names = FALSE)]
    } else {
        stop("Invalid 'result' parameter. Choose between 'index', 'logic', and 'value'.")
    }
    
    if (silent) {
        sink()
    }
    
    return(res)
}




yao.libNins.pkg <- function(file_path, intro = FALSE) {
    if (intro) {
        cat("\033[31myao.libNins.pkg 函数的功能介绍:\n\033[97m")
        cat("该函数允许用户从一个包含R包名的文件中批量加载和安装R包。\n")
        
        cat("\n\033[31myao.libNins.pkg 函数的基本用法:\n\033[97m")
        cat("yao.libNins.pkg(file_path, intro)\n")
        
        cat("\n\033[31myao.libNins.pkg 函数的参数说明:\n\033[97m")
        cat("1. file_path: 包含R包名的文件的路径。\n")
        cat("2. intro: 可选参数，默认为 FALSE。如果为 TRUE，则显示函数的基本用法；如果为 FALSE，则不显示。\n")
        
        cat("\n\033[31myao.libNins.pkg 函数的使用示例:\n\033[97m")
        cat("yao.libNins.pkg(file_path = 'your_file_path.txt')\n\n")
    }
    
    package_list <- readLines(file_path)
    for (package_line in package_list) {
        package_line <- trimws(package_line)
        package_name <- gsub("library\\((.*?)\\)", "\\1", package_line)
        cat("\033[34m包名:", package_name, "\n\033[97m")
        if (!require(package_name, character.only = TRUE)) {
            cat("\033[31m无法加载或找到包:", package_name, "\n\033[97m")
            cat(paste("尝试安装包:", package_name, "\n"))
            install.packages(package_name, dependencies = TRUE)
            if (!require(package_name, character.only = TRUE)) {
                cat("\033[31m无法安装或加载包:", package_name, "\n\033[97m")
            } else {
                cat("\033[32m成功加载包:", package_name, "\n\033[97m")
                assign(package_name, asNamespace(package_name), envir = .GlobalEnv)
            }
        } else {
            cat("\033[32m成功加载包:", package_name, "\n\033[97m")
            assign(package_name, asNamespace(package_name), envir = .GlobalEnv)
        }
    }
}
yao.detach.pkg <- function(intro = FALSE) {
    if (intro) {
        cat("\033[31myao.detach.pkg 函数的功能介绍:\n\033[97m")
        cat("这个函数允许用户卸载所有非基础的R包。\n")
        
        cat("\n\033[31myao.detach.pkg 函数的基本用法:\n\033[97m")
        cat("yao.detach.pkg(intro)\n")
        
        cat("\n\033[31myao.detach.pkg 函数的参数说明:\n\033[97m")
        cat("1. intro: 可选参数，默认为 FALSE。如果为 TRUE，则显示函数的基本用法；如果为 FALSE，则不显示。\n")
        
        cat("\n\033[31myao.detach.pkg 函数的使用示例:\n\033[97m")
        cat("yao.detach.pkg()\n\n")
    }
    
    loaded_packages <- search()
    base_packages <- c("package:base", "package:utils", "package:stats", 
                       "package:graphics", "package:grDevices", 
                       "package:datasets", "package:methods", 
                       "tools:rstudio", "Autoloads")
    packages_to_detach <- setdiff(loaded_packages, base_packages)
    for (package in packages_to_detach) {
        if (package != ".GlobalEnv") {
            detach(package, unload = TRUE, character.only = TRUE, force = TRUE)
        }
    }
    cat("\033[32mAll non-base packages detached.\n\033[97m")
}



yao.save.Rdata <- function(var_list, file_name, dir_name = "02_Rdata", intro = FALSE) {
    if (intro) {
        cat("\033[31myao.save.Rdata 函数的功能介绍:\n\033[97m")
        cat("此函数允许用户在指定的目录中保存一个或多个R变量到.Rdata文件。\n")
        
        cat("\n\033[31myao.save.Rdata 函数的基本用法:\n\033[97m")
        cat("yao.save.Rdata(var_list, file_name, dir_name, intro)\n")
        
        cat("\n\033[31myao.save.Rdata 函数的参数说明:\n\033[97m")
        cat("1. var_list: 要保存的变量名称的字符向量。\n")
        cat("2. file_name: 要保存的文件的基本名称，可以包括或不包括.Rdata扩展名。\n")
        cat("3. dir_name: 可选参数，保存.Rdata文件的目录名称，默认为 '02_Rdata'。\n")
        cat("4. intro: 可选参数，默认为 FALSE。如果为 TRUE，则显示函数的基本用法；如果为 FALSE，则不显示。\n")
        
        cat("\n\033[31myao.save.Rdata 函数的使用示例:\n\033[97m")
        cat("yao.save.Rdata(var_list = c('data1', 'data2'), file_name = 'my_data', dir_name = 'my_directory')\n\n")
    }
    
    work_dir <- getwd()
    target_dir <- file.path(work_dir, dir_name)
    if (!dir.exists(target_dir)) dir.create(target_dir)
    save_path <- ifelse(grepl("\\.Rdata$", file_name), 
                        file.path(target_dir, file_name), 
                        file.path(target_dir, paste0(file_name, ".Rdata")))
    save(list = var_list, file = save_path)
    cat("\033[34m变量已保存在文件：", save_path, "\n\033[97m")
}
yao.load.Rdata <- function(file_name, dir_name = "02_Rdata", intro = FALSE) {
    if (intro) {
        cat("\033[31myao.load.Rdata 函数的功能介绍:\n\033[97m")
        cat("此函数从指定的目录中加载.Rdata文件到当前R环境中。\n")
        
        cat("\n\033[31myao.load.Rdata 函数的基本用法:\n\033[97m")
        cat("yao.load.Rdata(file_name, dir_name, intro)\n")
        
        cat("\n\033[31myao.load.Rdata 函数的参数说明:\n\033[97m")
        cat("1. file_name: 要加载的.Rdata文件的基本名称，可以包括或不包括.Rdata扩展名。\n")
        cat("2. dir_name: 可选参数，.Rdata文件所在的目录名称，默认为 '02_Rdata'。\n")
        cat("3. intro: 可选参数，默认为 FALSE。如果为 TRUE，则显示函数的基本用法；如果为 FALSE，则不显示。\n")
        
        cat("\n\033[31myao.load.Rdata 函数的使用示例:\n\033[97m")
        cat("yao.load.Rdata(file_name = 'my_data', dir_name = 'my_directory')\n\n")
    }
    
    work_dir <- getwd()
    target_dir <- file.path(work_dir, dir_name)
    Rdata_file <- ifelse(grepl("\\.Rdata$", file_name),
                         file.path(target_dir, file_name),
                         file.path(target_dir, paste0(file_name, ".Rdata")))
    load(file = Rdata_file, envir = .GlobalEnv)
    cat("\033[34m变量已从文件", Rdata_file, "中加载。\n\033[97m")
}
yao.write.intro.Rdata <- function(Rdata.name, intro = FALSE, dir_name = "01_DATA") {
    if (intro) {
        cat("\033[31myao.write.intro.Rdata 函数的功能介绍:\n\033[97m")
        cat("此函数允许用户为给定的.Rdata文件输入附带文本，并将其保存为.txt文件。\n")
        
        cat("\n\033[31myao.write.intro.Rdata 函数的基本用法:\n\033[97m")
        cat("yao.write.intro.Rdata(Rdata.name, intro, dir_name)\n")
        
        cat("\n\033[31myao.write.intro.Rdata 函数的参数说明:\n\033[97m")
        cat("1. Rdata.name: 输入的.Rdata文件的基本名称。\n")
        cat("2. intro: 可选参数，默认为 FALSE。如果为 TRUE，则显示函数的基本用法；如果为 FALSE，则不显示。\n")
        cat("3. dir_name: txt文档存储的目标文件夹，默认是工作目录下的 '01_DATA' 文件夹。\n")
        
        cat("\n\033[31myao.write.intro.Rdata 函数的使用示例:\n\033[97m")
        cat("yao.write.intro.Rdata(Rdata.name = 'my_data.Rdata', dir_name = 'specific_directory')\n\n")
    }
    
    tmp <- gsub("\\.Rdata$", "", Rdata.name)
    
    # Ensure the directory exists
    if(!dir.exists(dir_name)){
        dir.create(dir_name, recursive = TRUE)
    }
    
    txt_file <- file.path(dir_name, paste0(tmp, ".txt"))
    
    cat("\033[34m请输入文本内容，输入完毕后，请按Enter键换行输入 'ENDLINE' ，并再按Enter键:\n\033[97m")
    user_input <- character(0)
    while (TRUE) {
        line <- readLines(con = stdin(), n = 1)
        line <- trimws(line)
        if (line == "ENDLINE") {
            break
        }
        user_input <- c(user_input, line)
    }
    file_connection <- file(txt_file, "w")
    cat(user_input, file = file_connection, sep = "\n")
    close(file_connection)
    cat("\033[34m输入的内容已保存到", txt_file, "文件中。\n\033[97m")
}
yao.intro.Rdata <- function(Rdata.name, dir_name = "01_Data", intro = FALSE) {
    if (intro) {
        cat("\033[31myao.intro.Rdata 函数的功能介绍:\n\033[97m")
        cat("这个函数根据指定的.Rdata文件名读取相应的.txt文件内容，优先在指定的文件夹中查找，未找到则在工作目录中查找。\n")
        cat("\n\033[31myao.intro.Rdata 函数的基本用法:\n\033[97m")
        cat("yao.intro.Rdata(Rdata.name, dir_name, intro)\n")
        cat("\n\033[31myao.intro.Rdata 函数的参数说明:\n\033[97m")
        cat("1. Rdata.name: 输入的.Rdata文件的基本名称。\n")
        cat("2. dir_name: 指定.txt文件读取的文件夹，默认是工作目录下的01_DATA文件夹。\n")
        cat("3. intro: 可选参数，默认为 FALSE。如果为 TRUE，则显示函数的基本用法；如果为 FALSE，则不显示。\n")
        cat("\n\033[31myao.intro.Rdata 函数的使用示例:\n\033[97m")
        cat("yao.intro.Rdata(Rdata.name = 'your_data.Rdata', dir_name = '01_DATA')\n\n")
    }
    
    tmp <- gsub("\\.Rdata$", "", Rdata.name)
    txt_file_in_dir <- file.path(dir_name, paste0(tmp, ".txt"))
    txt_file_in_wd <- paste0(tmp, ".txt")
    
    if (file.exists(txt_file_in_dir)) {
        txt_file <- txt_file_in_dir
    } else if (file.exists(txt_file_in_wd)) {
        txt_file <- txt_file_in_wd
    } else {
        cat("\033[91m错误：在指定的文件夹和工作目录中均未找到", txt_file_in_wd, "文件", "\n\033[97m")
        return(NULL)
    }
    
    file_connection <- file(txt_file, "a")
    cat("\n", file = file_connection)
    close(file_connection)
    
    txt_contents <- readLines(txt_file)
    
    for (line in txt_contents) {
        if (grepl("^>", line)) {
            cat("\033[31m", line, "\033[97m\n", sep = "")
        } else {
            cat(line, "\n")
        }
    }
}
yao.save.worksp <- function(file_name, dir_name = NULL, intro = FALSE, silent = FALSE) {
    
    if (silent) {
        sink(tempfile())
    }
    
    if (is.null(dir_name)) {
        dir_name <- "02_Rdata"
        pkg_dir_name <- "01_Data"
    } else {
        pkg_dir_name <- dir_name
    }
    
    if (intro) {
        cat("\033[33myao.save.worksp 函数的功能介绍:\n\033[97m")
        cat("这个函数允许用户保存工作空间及其包信息。\n")
        
        cat("\n\033[33myao.save.worksp 函数的基本用法:\n\033[97m")
        cat("yao.save.worksp(file_name, dir_name, intro, silent)\n")
        
        cat("\n\033[33myao.save.worksp 函数的参数说明:\n\033[97m")
        cat("1. file_name: 要保存的文件名。\n")
        cat("2. dir_name: 保存工作空间的目录名，默认为'02_Rdata'。\n")
        cat("3. intro: 可选参数，默认为 FALSE。如果为 TRUE，则显示函数的基本用法；如果为 FALSE，则不显示。\n")
        cat("4. silent: 可选参数，默认为 FALSE。如果为 TRUE，则不会输出任何信息到控制台。\n")
        
        cat("\n\033[33myao.save.worksp 函数的使用示例:\n\033[97m")
        cat("yao.save.worksp(file_name = 'your_filename', dir_name = 'your_directory_name')\n\n")
    }
    
    if (!grepl("^WS_", file_name))
        file_name <- paste0("WS_", file_name)
    if (!grepl("\\.RData$", file_name, ignore.case = TRUE))
        file_name <- paste0(file_name, ".RData")
    
    file_path <- file.path(dir_name, file_name)
    save.image(file = file_path)
    
    pkg_file_name <- sub("\\.RData$", ".txt", file_name)
    pkg_file_name <- paste0("Pkg_info_", pkg_file_name)
    pkg_info <- sessionInfo()
    pkg_dir_path <- file.path(pkg_dir_name, pkg_file_name)
    writeLines(capture.output(print(pkg_info)), pkg_dir_path)
    cat("\033[33m您当前工作相关信息", "\033[34m", file_name, "\033[33m", "和", "\033[34m", pkg_file_name, "\033[33m", "已保存。\n")
    
    if (silent) {
        sink()
    }
    
    return(invisible(NULL))
}
yao.load.worksp <- function(file_name, intro = FALSE, silent = FALSE) {
    
    dir_name <- "02_Rdata"
    pkg_dir_name <- "01_Data"
    
    if (silent) {
        sink(tempfile())
    }
    
    if (intro) {
        cat("\033[33myao.load.worksp 函数的功能介绍:\n\033[97m")
        cat("这个函数允许用户加载工作空间及其包信息。\n")
        
        cat("\n\033[33myao.load.worksp 函数的基本用法:\n\033[97m")
        cat("yao.load.worksp(file_name, intro, silent)\n")
        
        cat("\n\033[33myao.load.worksp 函数的参数说明:\n\033[97m")
        cat("1. file_name: 要加载的文件名。\n")
        cat("2. intro: 可选参数，默认为 FALSE。如果为 TRUE，则显示函数的基本用法；如果为 FALSE，则不显示。\n")
        cat("3. silent: 可选参数，默认为 FALSE。如果为 TRUE，则不会输出任何信息到控制台。\n")
        
        cat("\n\033[33myao.load.worksp 函数的使用示例:\n\033[97m")
        cat("yao.load.worksp(file_name = 'your_filename')\n\n")
    }
    
    if (!grepl("^WS_", file_name))
        file_name <- paste0("WS_", file_name)
    if (!grepl("\\.RData$", file_name, ignore.case = TRUE))
        file_name <- paste0(file_name, ".RData")
    
    file_path <- file.path(dir_name, file_name)
    
    pkg_file_name <- sub("\\.RData$", ".txt", file_name)
    pkg_file_name <- paste0("Pkg_info_", pkg_file_name)
    pkg_file_path <- file.path(pkg_dir_name, pkg_file_name)
    
    if (file.exists(file_path)) {
        load(file_path)
        cat("\033[33m您的工作空间", "\033[34m", file_name, "\033[33m", "已经成功加载。\n")
        
        if (file.exists(pkg_file_path)) {
            cat("\033[33m以下是您的包信息:\n\033[97m")
            cat(readLines(pkg_file_path), sep = "\n")
        } else {
            cat("\033[31m错误:", "\033[34m", pkg_file_name, "\033[31m", "不存在。\n")
        }
        
    } else {
        cat("\033[31m错误:", "\033[34m", file_name, "\033[31m", "不存在。\n")
    }
    
    if (silent) {
        sink()
    }
    
    return(invisible(NULL))
}
yao.slit.print <- function(file_name = NULL, intro = FALSE) {
    
    if (intro) {
        cat("\033[33myao.slit 函数的功能介绍:\n\033[97m")
        cat("该函数生成一系列与文件名相关的命令，并在屏幕上输出这些命令。\n")
        
        cat("\n\033[33myao.slit 函数的基本用法:\n\033[97m")
        cat("yao.slit(file_name, intro)\n")
        
        cat("\n\033[33myao.slit 函数的参数说明:\n\033[97m")
        cat("1. file_name: 指定的文件名。\n")
        cat("2. intro: 可选参数，默认为 FALSE。如果为 TRUE，则显示函数的基本用法；如果为 FALSE，则不显示。\n")
        
        cat("\n\033[33myao.slit 函数的使用示例:\n\033[97m")
        cat("yao.slit(file_name = 'your_filename.Rdata')\n\n")
    }
    
    cat("\033[34m生成的命令如下:\n\033[97m")
    
    var_name <- if(grepl("\\.Rdata$", file_name)) sub("\\.Rdata$", "", file_name) else file_name
    cat(paste0("var_list <- c(\"", var_name, "\")\n"))
    
    if (!is.null(file_name) && !grepl("\\.Rdata$", file_name)) {
        file_name <- paste0(file_name, ".Rdata")
    }
    
    cat(paste0("yao.save.Rdata(var_list = var_list, file_name = \"", file_name, "\")\n"))
    cat(paste0("yao.load.Rdata(file_name = \"", file_name, "\")\n"))
    cat(paste0("yao.write.intro.Rdata(Rdata.name = \"", file_name, "\")\n"))
    cat(paste0("yao.intro.Rdata(Rdata.name = \"", file_name, "\")\n"))
}
yao.set.print <- function() {
    cat("rm(list = ls())\n")
    cat("source(\"R02_Yao_Basic_Function.R\")\n")
    cat("yao.source.function()\n")
    cat("source(\"04_Rscript/R02_Yao_Basic_Function.R\")\n")
    cat("yao.load.Rdata('')\n")
    cat("yao.intro.Rdata('')")
}
yao.print.cat <- function(intro = FALSE, silent = FALSE) {
    if (silent) sink(tempfile())
    
    if (intro) {
        cat("\033[31myao.print.cat 函数的功能介绍:\n\033[97m")
        cat("允许用户获得让控制台输出内容代码模板。\n")
        cat("\n\033[31myao.print.cat 函数的基本用法:\n\033[97m")
        cat("yao.print.cat(intro, silent)\n")
        cat("\n\033[31myao.print.cat 函数的参数说明:\n\033[97m")
        cat("1. intro: 显示或隐藏基本用法。\n")
        cat("2. silent: 静默模式。\n")
        cat("\n\033[31myao.print.cat 函数的使用示例:\n\033[97m")
        cat("yao.print.cat(intro = TRUE)\n\n")
    } else {
        cat('cat("\\033[34m第", i, "个数据\\033[97m\\n")\n')
    }
    
    if (silent) sink()
}



yao.open.pdf <- function(file_name, dir_name = "03_Graph", intro = FALSE) {
    if (intro) {
        cat("\033[31myao.open.pdf 函数的功能介绍:\n\033[97m")
        cat("该函数允许用户打开工作目录下的 03_Graph 文件夹中的 pdf 文档，\n")
        cat("如果文件夹内找不到，则在工作目录中查找。\n")
        
        cat("\n\033[31myao.open.pdf 函数的基本用法:\n\033[97m")
        cat("yao.open.pdf(filename, intro)\n")
        
        cat("\n\033[31myao.open.pdf 函数的参数说明:\n\033[97m")
        cat("1. filename: 文件名，可以包含或不包含文件名后缀“.pdf”。\n")
        cat("2. intro: 可选参数，默认为 FALSE。如果为 TRUE，则显示函数的基本用法；如果为 FALSE，则不显示。\n")
        
        cat("\n\033[31myao.open.pdf 函数的使用示例:\n\033[97m")
        cat("yao.open.pdf(filename = 'test')\n\n")
    }
    
    if (!grepl("\\.pdf$", file_name)) {
        file_name <- paste0(file_name, ".pdf")
    }
    
    pdf_path <- file.path(dir_name, file_name)
    
    if (!file.exists(pdf_path)) {
        pdf_path <- file_name
    }
    
    if (file.exists(pdf_path)) {
        system(paste("open", pdf_path), wait = FALSE)
    } else {
        cat("\033[34m文件未找到，请检查相关文件夹是否存在目标文件或检查目标文件名是否正确。\n\033[97m")
    }
}
yao.open.image <- function(file_name, dir_name = "03_Graph", intro = FALSE) {
    library(imager)
    
    if (intro) {
        cat("\033[31myao.load.image 函数的功能介绍:\n\033[97m")
        cat("此函数从指定的目录中加载图像文件，并在R中进行展示。\n")
        
        cat("\n\033[31myao.load.image 函数的基本用法:\n\033[97m")
        cat("yao.load.image(file_name, dir_name, intro)\n")
        
        cat("\n\033[31myao.load.image 函数的参数说明:\n\033[97m")
        cat("1. file_name: 要加载的图像文件名，可以包含或不包含文件扩展名。\n")
        cat("   可支持的扩展名 .png/.bmp/.tiff/.jpg/.jpeg 格式。\n")
        cat("2. dir_name: 可选参数，图像文件所在的目录名称，默认为 '03_Graph'。\n")
        cat("3. intro: 可选参数，默认为 FALSE。如果为 TRUE，则显示函数的基本用法；如果为 FALSE，则不显示。\n")
        
        cat("\n\033[31myao.load.image 函数的使用示例:\n\033[97m")
        cat("yao.load.image(file_name = 'sample_image', dir_name = 'my_images')\n\n")
    }
    
    work_dir <- getwd()
    target_dir <- file.path(work_dir, dir_name)
    
    # 检查文件扩展名
    image_file <- ifelse(grepl("\\.(png|jpg|jpeg|bmp|tiff)$", file_name),
                         file.path(target_dir, file_name),
                         paste0(file.path(target_dir, file_name), ".png")) # 默认为png格式
    
    if (!file.exists(image_file)) {
        image_file <- ifelse(grepl("\\.(png|jpg|jpeg|bmp|tiff)$", file_name),
                             file_name,
                             paste0(file_name, ".png")) # 默认为png格式
    }
    
    if (file.exists(image_file)) {
        image_data <- load.image(file = image_file)
        plot(image_data)
        cat("\033[34m图像", image_file, "已加载并展示。\n\033[97m")
    } else {
        cat("\033[34m文件未找到，请检查相关文件夹是否存在目标文件或检查目标文件名是否正确。\n\033[97m")
    }
}
yao.GFormat.convert <- function(file_list, target_format, dir_name = "03_Graph", intro = FALSE) {
    
    supported_formats <- c("png", "jpeg", "jpg", "gif", "bmp", "tiff", "pdf")
    
    target_format <- tolower(gsub("\\s|\\.", "", target_format))
    
    if (!file.exists(dir_name)) {
        dir.create(dir_name)
    }
    
    if (intro) {
        cat("\033[31myao.GFormat.convert 函数的基本用法:\n\033[97m")
        cat("yao.GFormat.convert(file_list, target_format, dir_name, intro)\n")
        cat("\n\033[31myao.GFormat.convert 函数的参数说明:\n\033[97m")
        cat("1. file_list: 接受文件名向量，函数根据文件名的向量在目标文件夹中检索相关的文件，然后逐个进行转换。\n")
        cat("2. target_format: 目标文件格式，不区分大小写。支持的格式包括：", paste(supported_formats, collapse = ", "), "\n")
        cat("3. dir_name: 目标文件夹，默认为工作目录下的 '03_Graph'。\n")
        cat("4. intro: 可选参数，默认为 FALSE。如果为 TRUE，则显示函数的基本用法和支持的格式列表；如果为 FALSE，则不显示。\n")
        cat("\n\033[31myao.GFormat.convert 函数的使用示例:\n\033[97m")
        cat("yao.GFormat.convert(file_list = c('test1.png', 'test2.png'), target_format = 'pdf')\n\n")
    }
    
    if (!(target_format %in% supported_formats)) {
        cat("\033[35m错误\033[97m：不支持的目标文件格式。支持的格式包括：", paste(supported_formats, collapse = ", "), "\n")
        return()
    }
    
    for (filename in file_list) {
        input_path <- file.path(dir_name, filename)
        output_path <- file.path(dir_name, gsub("\\.\\w+$", paste0(".", target_format), filename))
        
        if (file.exists(input_path)) {
            image <- magick::image_read(input_path)
            magick::image_write(image, path = output_path, format = target_format)
            cat(paste("\033[34m转换完成\033[97m：", filename, "->", basename(output_path), "\n"))
        } else {
            cat(paste("\033[35m文件不存在\033[97m：", filename, "\n"))
        }
    }
}



yao.initial.set <- function(intro = FALSE) {
    
    if (intro) {
        cat("\033[31myao.initial.set 函数的功能介绍:\n\033[97m")
        cat("这个函数用于初始化或重置R的环境，包括清理环境、设置语言、加载并行包等。\n")
        cat("\n\033[31myao.initial.set 函数的基本用法:\n\033[97m")
        cat("yao.initial.set(intro)\n")
        cat("\n\033[31myao.initial.set 函数的参数说明:\n\033[97m")
        cat("1. intro: 可选参数，默认为 FALSE。如果为 TRUE，则显示函数的基本用法；如果为 FALSE，则不显示。\n")
        cat("\n\033[31myao.initial.set 函数的使用示例:\n\033[97m")
        cat("yao.initial.set(intro = TRUE)\n\n")
    }
    
    Sys.setenv(LANGUAGE = "en")
    set.seed(123)
    # library(doParallel)
    # registerDoParallel(cores = 4)
    
    options(nwarnings = 50, max.print = 100, scipen = 999, stringsAsFactors = FALSE, 
            showWarnCalls = TRUE, error = function() { 
                cat("\033[31mOops, an error occurred!\n\033[97m"); traceback() 
            })
}
yao.initial.dir <- function(intro = FALSE) {
    if (intro) {
        cat("\033[31myao.initial.dir 函数的功能介绍:\n\033[97m")
        cat("这个函数在当前工作目录下创建特定的子目录（如'02_Rdata', '01_Data'等）。\n")
        
        cat("\n\033[31myao.initial.dir 函数的基本用法:\n\033[97m")
        cat("yao.initial.dir(intro)\n")
        
        cat("\n\033[31myao.initial.dir 函数的参数说明:\n\033[97m")
        cat("1. intro: 可选参数，默认为 FALSE。如果为 TRUE，则显示函数的基本用法；如果为 FALSE，则不显示。\n")
        
        cat("\n\033[31myao.initial.dir 函数的使用示例:\n\033[97m")
        cat("yao.initial.dir()\n\n")
    }
    
    current_dir <- getwd()
    
    create_dir <- function(dir_name) {
        dir_path <- file.path(current_dir, dir_name)
        if (!dir.exists(dir_path)) {
            dir.create(dir_path)
            cat("\033[32mCreated", dir_name, "folder.\n\033[97m")
        } else {
            cat("\033[31m", dir_name, "folder already exists.\n\033[97m")
        }
    }
    
    dirs <- c("01_Data", "02_Rdata", "03_Graph", "04_Rscript", "05_Tmp")
    
    for(dir in dirs) {
        create_dir(dir)
    }
}
yao.source.function <- function(..., dir_name = "04_Rscript", intro = FALSE) {
    if (intro) {
        cat("\033[31myao.source.function 函数的功能介绍:\n\033[97m")
        cat("该函数允许用户从指定的文件夹中源（source）一个或多个R文件，执行其中的命令。\n")
        cat("如果文件夹内找不到，则在工作目录中查找。\n")
        
        cat("\n\033[31myao.source.function 函数的基本用法:\n\033[97m")
        cat("yao.source.function(..., dir_name, intro)\n")
        
        cat("\n\033[31myao.source.function 函数的参数说明:\n\033[97m")
        cat("1. ...: 一个或多个文件名，可以包含或不包含文件名后缀“.R”。\n")
        cat("2. dir_name: 可选参数，R文件所在的目录名称，默认为 '04_Rscript'。\n")
        cat("3. intro: 可选参数，默认为 FALSE。如果为 TRUE，则显示函数的基本用法；如果为 FALSE，则不显示。\n")
        
        cat("\n\033[31myao.source.function 函数的使用示例:\n\033[97m")
        cat("yao.source.function('my_script1', 'my_script2', dir_name = 'my_directory')\n\n")
    }
    
    file_names <- list(...)
    
    if (length(file_names) == 0) {
        all_files <- list.files(path = dir_name, pattern = "\\.R$", ignore.case = TRUE, full.names = TRUE)
        file_names <- all_files[grepl("function", basename(all_files), ignore.case = TRUE)]
        
        if(length(file_names) == 0) {
            all_files_wd <- list.files(pattern = "\\.R$", ignore.case = TRUE, full.names = TRUE)
            file_names <- all_files_wd[grepl("function", basename(all_files_wd), ignore.case = TRUE)]
        }
    } else {
        for(i in 1:length(file_names)) {
            if (!grepl("\\.R$", file_names[[i]])) {
                file_names[[i]] <- paste0(file_names[[i]], ".R")
            }
        }
    }
    
    for(file_name in file_names) {
        R_file_path <- ifelse(file.exists(file.path(dir_name, file_name)), file.path(dir_name, file_name), file_name)
        
        if (file.exists(R_file_path)) {
            source(R_file_path)
            cat("\033[34m文件", R_file_path, "已被成功源（source）。\n\033[97m")
        } else {
            cat("\033[34m文件", R_file_path, "未找到，请检查相关文件夹是否存在目标文件或检查目标文件名是否正确。\n\033[97m")
        }
    }
}
yao.sink <- function(intro = FALSE, silent = FALSE) {
    
    if (silent) {
        sink(tempfile())
    }
    
    if (intro) {
        cat("\033[33myao.sink 函数的功能介绍:\n\033[97m")
        cat("这个函数用于清除所有的 sink。即使存在多个 sink，该函数会一一移除。\n")
        
        cat("\n\033[33myao.sink 函数的基本用法:\n\033[97m")
        cat("yao.sink(intro)\n")
        
        cat("\n\033[33myao.sink 函数的参数说明:\n\033[97m")
        cat("1. intro: 可选参数，默认为 FALSE。如果为 TRUE，则显示函数的基本用法；如果为 FALSE，则不显示。\n")
        
        cat("\n\033[33myao.sink 函数的使用示例:\n\033[97m")
        cat("yao.sink()\n\n")
    }
    
    while(!is.null(sink.number())) {
        sink(NULL)
    }
    
    if (silent) {
        sink()
    }
}



yao.transfer.file <- function(intro = FALSE) {
    if (intro) {
        cat("\033[31myao.transfer.file 函数的功能介绍:\n\033[97m")
        cat("这个函数用于将当前工作目录下的不同格式的文件转移至指定文件夹。如果目标文件夹的文件与要转移的文件同名，新文件会覆盖原文件。\n")
        cat("\n\033[31myao.transfer.file 函数的基本用法:\n\033[97m")
        cat("yao.transfer.file(intro)\n")
        cat("\n\033[31myao.transfer.file 函数的参数说明:\n\033[97m")
        cat("1. intro: 可选参数，默认为 FALSE。如果为 TRUE，则显示函数的基本用法；如果为 FALSE，则不显示。\n")
        cat("\n\033[31myao.transfer.file 函数的使用示例:\n\033[97m")
        cat("yao.transfer.file()\n\n")
        return()  # 结束函数，不执行后续的文件转移操作
    }
    
    work_dir <- getwd()
    files <- list.files(work_dir, full.names = TRUE, recursive = FALSE)
    files <- files[sapply(files, function(x) !file.info(x)$isdir)]  # 仅选择文件，不选择文件夹
    
    for (file_path in files) {
        file_ext <- tools::file_ext(file_path)
        
        if (file_ext %in% c("txt", "gz", "zip", "tar", "rar")) {
            target_dir <- file.path(work_dir, "01_Data")
        } else if (file_ext == "Rdata") {
            target_dir <- file.path(work_dir, "02_Rdata")
        } else if (file_ext %in% c("pdf", "jpg", "png", "tiff", "jpeg")) {
            target_dir <- file.path(work_dir, "03_Graph")
        } else if (file_ext == "R") {
            target_dir <- file.path(work_dir, "04_Rscript")
        } else {
            cat(paste("\033[31mUnsupported file extension:", file_ext, "for file:", file_path, "\033[97m\n"))
            next
        }
        
        if (!dir.exists(target_dir)) {
            dir.create(target_dir)
        }
        
        file_name <- basename(file_path)
        target_path <- file.path(target_dir, file_name)
        
        file.copy(from = file_path, to = target_path, overwrite = TRUE)
        file.remove(file_path)  # 如果你想保留原始文件，请删除这一行
        
        cat(paste("\033[34mFile", file_path, "has been transferred to", target_path, "\033[97m\n"))
    }
}
yao.renew.basicFun <- function(intro = FALSE) {
    if (intro) {
        cat("\033[33myao.renew.basicFun 函数的功能介绍:\n\033[97m")
        cat("此函数用于比较两个脚本文件中的函数列表，并提供有关是否覆盖源文件的选项。\n")
        
        cat("\n\033[33myao.renew.basicFun 函数的基本用法:\n\033[97m")
        cat("yao.renew.basicFun(intro)\n")
        
        cat("\n\033[33myao.renew.basicFun 函数的参数说明:\n\033[97m")
        cat("1. intro: 可选参数，默认为 FALSE。如果为 TRUE，则显示函数的基本用法；如果为 FALSE，则不显示。\n")
        
        cat("\n\033[33myao.renew.basicFun 函数的使用示例:\n\033[97m")
        cat("yao.renew.basicFun(intro = TRUE)\n\n")
    }
    
    pattern <- "yao\\.[A-Za-z0-9.]+ <- function\\("
    current_file_path <- ifelse(file.exists("04_Rscript/R02_Yao_Basic_Function.R"), "04_Rscript/R02_Yao_Basic_Function.R", "R02_Yao_Basic_Function.R")
    
    current_yaoFun <- list()
    if (file.exists(current_file_path)) {
        script <- readLines(current_file_path)
        for (line in script) {
            if (grepl(pattern, line)) {
                function_name <- gsub(" <- function\\(", "", regmatches(line, regexpr(pattern, line)))
                current_yaoFun <- c(current_yaoFun, function_name)
            }
        }
        current_yaoFun <- unlist(current_yaoFun)
    }
    
    old_yaoFun <- list()
    script <- readLines("C:/Users/10981/Desktop/Hello_R/R02_Yao_Basic_Function.R")
    for (line in script) {
        if (grepl(pattern, line)) {
            function_name <- gsub(" <- function\\(", "", regmatches(line, regexpr(pattern, line)))
            old_yaoFun <- c(old_yaoFun, function_name)
        }
    }
    old_yaoFun <- unlist(old_yaoFun)
    
    missing_in_old <- setdiff(current_yaoFun, old_yaoFun)
    missing_in_current <- setdiff(old_yaoFun, current_yaoFun)
    
    if (length(missing_in_old) > 0) {
        cat("\033[34m以下元素在当前文件中存在，但在源文件中不存在：\n\033[97m")
        cat(missing_in_old, sep = "\n")
    }
    if (length(missing_in_current) > 0) {
        cat("\033[34m以下元素在源文件中存在，但在当前文件中不存在：\n\033[97m")
        cat(missing_in_current, sep = "\n")
    }
    
    if (length(missing_in_old) > 0 || length(missing_in_current) > 0) {
        overwrite <- readline(prompt = "\033[34m是否要覆盖源文件？(yes/no):\033[97m ")
        if (tolower(overwrite) == "yes") {
            file.copy(current_file_path, "C:/Users/10981/Desktop/Hello_R/R02_Yao_Basic_Function.R", overwrite = TRUE)
            cat("\033[32m文件已成功覆盖。\n\033[97m")
        } else {
            cat("\033[31m用户选择不覆盖，操作终止。\n\033[97m")
        }
    } 
    
    if (length(missing_in_old) == 0 & length(missing_in_current) == 0) {
        overwrite <- readline(prompt = "\033[34m两个文件函数名一致，是否要覆盖源文件？(yes/no):\033[97m ")
        if (tolower(overwrite) == "yes") {
            file.copy(current_file_path, "C:/Users/10981/Desktop/Hello_R/R02_Yao_Basic_Function.R", overwrite = TRUE)
            cat("\033[32m文件已成功覆盖。\n\033[97m")
        } else {
            cat("\033[31m用户选择不覆盖，操作终止。\n\033[97m")
        }
    }
}
yao.printColor <- function() {
    colors <- list(
        black = "0;30",
        red = "0;31",
        green = "0;32",
        yellow = "0;33",
        blue = "0;34",
        purple = "0;35",
        cyan = "0;36",
        white = "0;37",
        bold_black = "1;30",
        bold_red = "1;31",
        bold_green = "1;32",
        bold_yellow = "1;33",
        bold_blue = "1;34",
        bold_purple = "1;35",
        bold_cyan = "1;36",
        bold_white = "1;37",
        bright_black = "0;90",
        bright_red = "0;91",
        bright_green = "0;92",
        bright_yellow = "0;93",
        bright_blue = "0;94",
        bright_purple = "0;95",
        bright_cyan = "0;96",
        bright_white = "0;97",
        bg_black = "0;100",
        bg_red = "0;101",
        bg_green = "0;102",
        bg_yellow = "0;103",
        bg_blue = "0;104",
        bg_purple = "0;105",
        bg_cyan = "0;106",
        bg_white = "0;107"
    )
    
    sections <- list("普通颜色", "加粗颜色", "亮色", "背景色")
    separators <- c(1, 9, 17, 25, 33)
    section_idx <- 1
    
    for (i in seq_along(colors)) {
        if (i %in% separators) {
            cat("\033[1;31m------------------------------------------------------------------\033[0m \n")
            cat(sprintf("\033[0;34m%-6s\033[0m \n", sections[[section_idx]]))
            section_idx <- section_idx + 1
        }
        
        name <- names(colors)[i]
        code <- colors[[name]]
        
        cat(paste0(name, " (", code, "): ", "\033[", code, "m", name, "\033[0m", " ", "\\033[", code, "m", name, "\\033[0m"), "\n")
    }
    
    cat("\033[1;31m------------------------------------------------------------------\033[0m \n")
    cat("\033[0m")
}