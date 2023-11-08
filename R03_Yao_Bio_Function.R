yao.constr.GTFtidy <- function(gtf_path, intro = FALSE) {
    library(rtracklayer)
    
    if (intro) {
        cat("\033[33myao.import.gtf 函数的基本用法:\n\033[97m")
        cat("yao.import.gtf(gtf_path, intro)\n")
        cat("\n\033[33m参数说明:\n\033[97m")
        cat("1. gtf_path: 要加载的GTF文件的路径。\n")
        cat("2. intro: 可选参数，默认为 FALSE。如果为 TRUE，则显示函数的基本用法；如果为 FALSE，则不显示。\n")
        cat("\n\033[33m示例:\n\033[97m")
        cat("yao.constr.GTFtidy(gtf_path = '01_Data/Homo_sapiens.GRCh38.110.chr.gtf')\n\n")
    }
    
    gtf_data <- import(gtf_path)
    
    attribute_df <- as.data.frame(gtf_data@elementMetadata@listData)
    
    seqnames <- rep(gtf_data@seqnames@values, gtf_data@seqnames@lengths)
    strand <- rep(gtf_data@strand@values, gtf_data@strand@lengths)
    
    add_df <- data.frame(
        seqnames = seqnames,
        start = gtf_data@ranges@start,
        width = gtf_data@ranges@width,
        strand = strand
    )
    
    gtf_tidy <- cbind(add_df, attribute_df)
    
    metaInfo_gtf <- readLines(gtf_path, n = 5)
    
    cat("\033[34mgtf_tidy 已构建成功，该数据源 gtf 文件信息如下：\n\033[97m")
    print(metaInfo_gtf)
    
    cat("\033[33mgtf_tidy 的维度:\n\033[97m")
    cat(dim(gtf_tidy), "\n")
    
    return(gtf_tidy)
}

yao.load.GTFtidy <- function(data_dir = "C:/Users/10981/Desktop/Hello_R/05_BioData") {
    file_path <- file.path(data_dir, "gtf_tidy.Rdata")
    
    if (file.exists(file_path)) {
        loaded_data <- load(file_path)
        cat("\033[34mgtf_tidy.Rdata 文件已成功加载！\n\033[97m")
        return(get(loaded_data))
    } else {
        cat("\033[31m错误：gtf_tidy.Rdata 文件不存在于指定目录。\n\033[97m")
        return(NULL)
    }
}

yao.RawCounts2TPM <- function(data_rna, 
                              data_dir = "C:/Users/10981/Desktop/Hello_R/05_BioData", 
                              intro = FALSE) {
    
    if (intro) {
        cat("\033[33myao.RawCounts2TPM 函数的基本用法:\n\033[97m")
        cat("yao.RawCounts2TPM(data_rna, data_dir, intro)\n")
        cat("\n\033[33m参数说明:\n\033[97m")
        cat("1. data_rna: 转录组测序数据，raw_counts 数据，行为SYMBOL，列为样本。\n")
        cat("2. data_dir: ENSEMBL_length.Rdata文件所在的文件夹，默认为 'C:/Users/10981/Desktop/Hello_R/05_BioData'。\n")
        cat("3. intro: 可选参数，默认为 FALSE。如果为 TRUE，则显示函数的基本用法；如果为 FALSE，则不显示。\n")
        cat("\n\033[33m示例:\n\033[97m")
        cat("yao.RawCounts2TPM(data_rna)\n\n")
    }
    
    source("R03_Yao_Bio_Function.R")
    library(clusterProfiler)
    library(org.Hs.eg.db)
    # Load required packages
    # library(GenomicFeatures)
    
    # txdb <- makeTxDbFromGFF("01_Data/Homo_sapiens.GRCh38.110.chr.gtf", format = "auto")
    # exons_gene <- exonsBy(txdb, by = "gene")
    # class(exons_gene)
    # exons_gene_lens <- lapply(exons_gene, function(x){sum(width(reduce(x)))})
    # ENSEMBL_length = t(as.data.frame(exons_gene_lens))
    # ENSEMBL_length <- data.frame(ENSEMBL = row.names(ENSEMBL_length), length = ENSEMBL_length)
    
    file_path <- file.path(data_dir, "ENSEMBL_length.Rdata")
    load(file = file_path)
    
    gtf_tidy <- yao.load.GTFtidy()
    gtf_pro <- gtf_tidy[gtf_tidy$gene_biotype == "protein_coding", ]
    
    ENSEMBL_length_pro <- ENSEMBL_length[ENSEMBL_length$ENSEMBL %in% gtf_pro$gene_id, ]
    
    ebl2sym <- bitr(ENSEMBL_length_pro$ENSEMBL, 
                    fromType = "ENSEMBL", 
                    toType = "SYMBOL", 
                    OrgDb = org.Hs.eg.db)
    
    ebl_sym_len_pro <- merge(ENSEMBL_length_pro, ebl2sym, by.x = "ENSEMBL", by.y = "ENSEMBL")
    
    ebl_sym_len_pro <- ebl_sym_len_pro[!duplicated(ebl_sym_len_pro$SYMBOL), ]
    rownames(ebl_sym_len_pro) <- ebl_sym_len_pro$SYMBOL
    ebl_sym_len_pro <- ebl_sym_len_pro[ebl_sym_len_pro$SYMBOL %in% rownames(data_rna), ]
    data_rna <- data_rna[rownames(data_rna) %in% ebl_sym_len_pro$SYMBOL, ]
    ebl_sym_len_pro <- ebl_sym_len_pro[rownames(data_rna), ]
    
    kb <- ebl_sym_len_pro$length / 1000
    rpk <- data_rna / kb
    tpm <- t(t(rpk)/colSums(rpk) * 1000000)
    tpm <- as.data.frame(tpm)
    
    cat("\033[34m您的转录组数据 data_rna 已经成功转换成 TPM 数据\n\033[97m")
    
    return(tpm)
}

yao.RawCounts2FPKM <- function(data_rna, 
                              data_dir = "C:/Users/10981/Desktop/Hello_R/05_BioData", 
                              intro = FALSE) {
    
    if (intro) {
        cat("\033[33myao.RawCounts2FPKM 函数的基本用法:\n\033[97m")
        cat("yao.RawCounts2FPKM(data_rna, data_dir, intro)\n")
        cat("\n\033[33m参数说明:\n\033[97m")
        cat("1. data_rna: 转录组测序数据，raw_counts 数据，行为SYMBOL，列为样本。\n")
        cat("2. data_dir: ENSEMBL_length.Rdata文件所在的文件夹，默认为 'C:/Users/10981/Desktop/Hello_R/05_BioData'。\n")
        cat("3. intro: 可选参数，默认为 FALSE。如果为 TRUE，则显示函数的基本用法；如果为 FALSE，则不显示。\n")
        cat("\n\033[33m示例:\n\033[97m")
        cat("yao.RawCounts2FPKM(data_rna)\n\n")
    }
    
    source("R03_Yao_Bio_Function.R")
    library(clusterProfiler)
    library(org.Hs.eg.db)
    # Load required packages
    # library(GenomicFeatures)
    
    # txdb <- makeTxDbFromGFF("01_Data/Homo_sapiens.GRCh38.110.chr.gtf", format = "auto")
    # exons_gene <- exonsBy(txdb, by = "gene")
    # class(exons_gene)
    # exons_gene_lens <- lapply(exons_gene, function(x){sum(width(reduce(x)))})
    # ENSEMBL_length = t(as.data.frame(exons_gene_lens))
    # ENSEMBL_length <- data.frame(ENSEMBL = row.names(ENSEMBL_length), length = ENSEMBL_length)
    
    file_path <- file.path(data_dir, "ENSEMBL_length.Rdata")
    load(file = file_path)
    
    gtf_tidy <- yao.load.GTFtidy()
    gtf_pro <- gtf_tidy[gtf_tidy$gene_biotype == "protein_coding", ]
    
    ENSEMBL_length_pro <- ENSEMBL_length[ENSEMBL_length$ENSEMBL %in% gtf_pro$gene_id, ]
    
    ebl2sym <- bitr(ENSEMBL_length_pro$ENSEMBL, 
                    fromType = "ENSEMBL", 
                    toType = "SYMBOL", 
                    OrgDb = org.Hs.eg.db)
    
    ebl_sym_len_pro <- merge(ENSEMBL_length_pro, ebl2sym, by.x = "ENSEMBL", by.y = "ENSEMBL")
    
    ebl_sym_len_pro <- ebl_sym_len_pro[!duplicated(ebl_sym_len_pro$SYMBOL), ]
    rownames(ebl_sym_len_pro) <- ebl_sym_len_pro$SYMBOL
    ebl_sym_len_pro <- ebl_sym_len_pro[ebl_sym_len_pro$SYMBOL %in% rownames(data_rna), ]
    data_rna <- data_rna[rownames(data_rna) %in% ebl_sym_len_pro$SYMBOL, ]
    ebl_sym_len_pro <- ebl_sym_len_pro[rownames(data_rna), ]
    
    kb <- ebl_sym_len_pro$length / 1000
    rpk <- data_rna / kb
    fpkm <- t(t(rpk)/colSums(data_rna) * 10^6)
    fpkm <- as.data.frame(fpkm)
    
    cat("\033[34m您的转录组数据 data_rna 已经成功转换成 FPKM 数据\n\033[97m")
    
    return(fpkm)
}

yao.RawCounts2FPKM_TPM.ENSEMBL <- function(data_rna, 
                                           type = c("FPKM", "TPM"),
                                           data_dir = "C:/Users/10981/Desktop/Hello_R/05_BioData", 
                                           intro = FALSE) {
    
    type <- match.arg(type)
    
    if (intro) {
        cat("\033[33myao.RawCounts2FPKM_TPM.ENSEMBL 函数的基本用法:\n\033[97m")
        cat("yao.RawCounts2FPKM_TPM.ENSEMBL(data_rna, type, data_dir, intro)\n")
        cat("\n\033[33m参数说明:\n\033[97m")
        cat("1. data_rna: 转录组测序数据，raw_counts 数据，行为 ENSEMBL，列为样本。\n")
        cat("2. type: 转换的类型，可以是 'FPKM' 或 'TPM'。\n")
        cat("3. data_dir: ENSEMBL_length.Rdata文件所在的文件夹，默认为 'C:/Users/10981/Desktop/Hello_R/05_BioData'。\n")
        cat("4. intro: 可选参数，默认为 FALSE。如果为 TRUE，则显示函数的基本用法；如果为 FALSE，则不显示。\n")
        cat("\n\033[33m示例:\n\033[97m")
        cat("yao.RawCounts2FPKM_TPM.ENSEMBL(data_rna, type = 'FPKM')\n\n")
    }
    
    file_path <- file.path(data_dir, "ENSEMBL_length.Rdata")
    load(file = file_path)
    
    data_rna$ENSEMBL <- sub("\\..*", "", rownames(data_rna))
    data_rna$length <- ENSEMBL_length$length[match(data_rna$ENSEMBL, ENSEMBL_length$ENSEMBL)]
    
    library(dplyr)
    data_rna <- data_rna %>% 
        select(ENSEMBL,length, everything())
    
    data_rna <- na.omit(data_rna)
    
    kb <- data_rna$length / 1000
    raw_count_data <- data_rna[, 3:ncol(data_rna)]
    
    if (type == "FPKM") {
        rpk <- raw_count_data / kb
        fpkm <- t(t(rpk)/colSums(raw_count_data) * 10^6)
        fpkm <- as.data.frame(fpkm)
        cat("\033[34m您的转录组数据 data_rna 已经成功转换成 FPKM 数据\n\033[97m")
        return(fpkm)
    } else {
        rpk <- raw_count_data / kb
        tpm <- t(t(rpk)/colSums(rpk) * 1000000)
        tpm <- as.data.frame(tpm)
        cat("\033[34m您的转录组数据 data_rna 已经成功转换成 TPM 数据\n\033[97m")
        return(tpm)
    }
}

yao.fpkm2tpm <- function(data_fpkm, intro = FALSE) {
    if(intro) {
        cat("\033[33myao.fpkm2tpm 函数的基本用法:\n\033[97m")
        cat("yao.fpkm2tpm(data_fpkm)\n")
        
        cat("\n\033[33m参数说明:\n\033[97m")
        cat("1. data_fpkm: FPKM 格式的转录组数据，其中行为基因，列为样本。\n")
        cat("2. intro: 可选参数，默认为 FALSE。如果为 TRUE，则显示函数的基本用法和参数说明；如果为 FALSE，则不显示。\n")
        
        cat("\n\033[33m示例:\n\033[97m")
        cat("yao.fpkm2tpm(your_fpkm_data)\n\n")
    }
    
    tpm <- t(t(data_fpkm) / colSums(data_fpkm)) * 10^6
    
    cat("\033[34mdata_fpkm 已转化成 TPM。\n\033[97m")
    
    return(tpm)
}



yao.chose.col.none.na <- function(dataframe) {
    PRS_type_sample_id <- rownames(dataframe)[!is.na(dataframe$PRS_type)]
    sample_id_list <- list()
    for (col_name in colnames(dataframe)) {
        sample_id <- rownames(dataframe)[!is.na(dataframe[[col_name]])]
        sample_id_list[[paste0(col_name, "_sample_id")]] <- sample_id
    }
    save(sample_id_list, file = "sample_id_by_clin_colname.Rdata")
    return(sample_id_list)
}

yao.clin.subset.by.col.none.na <- function(dataframe) {
    env <- new.env()
    for (i in 1:ncol(dataframe)) {
        eval(parse(text = paste0("clin_data_", names(dataframe)[i], "_325 <- clin_data_325[clin_col_none_na[[", i, "]], ]")), envir = env)
    }
    save(list = ls(envir = env), file = "clin_data_group_by_col_none_na.Rdata", envir = env)
}

yao.rna.subset.by.col.none.na <- function(dataframe) {
    env <- new.env()
    for (i in 1:ncol(dataframe)) {
        eval(parse(text = paste0("rna_data_", names(dataframe)[i], "_325 <- rna_data_325[, clin_col_none_na[[", i, "]]]")), envir = env)
        cat("第", i, "个数据已经处理完毕\n")
    }
    cat("当前正在存储数据\n")
    save(list = ls(envir = env), file = "rna_data_group_by_col_none_na.Rdata", envir = env)
}

yao.extract.strings <- function(input_vector) {
    result_vector <- character(length(input_vector))
    
    for (i in seq_along(input_vector)) {
        match <- regmatches(input_vector[i], regexec("none_na_(.*?)_oscensor_big30_protein", input_vector[i]))
        
        if (length(match[[1]]) > 1) {
            result_vector[i] <- match[[1]][2]
        } else {
            result_vector[i] <- NA_character_
        }
    }
    
    return(result_vector)
}


yao.option <- function(matching_variables) {
    # options <- c(
    #     "Codel", "Non_codel", "bigger_42", "less_42", "dead", "live", "TMZ_treat", "TMZ_untreat",
    #     "Female", "Male", "WHO_II", "WHO_III", "WHO_IV", "A", "AA", "AO", "GBM", "O", "rA", "rAA",
    #     "rGBM", "rO", "sGBM", "Mutant", "Wildtype", "methylated", "unmethylated", "bigger_30",
    #     "less_30", "Primary", "Recurrent", "Secondary", "treat", "untreat"
    # )
    if (!exists("clin_data_693", envir = .GlobalEnv)) {
        message("未找到 'clin_data_693' 变量，正在加载数据...")
        load(file = "clin_rna_data_693.Rdata")
        message("'clin_data_693' 变量加载完毕，继续执行后续任务。")
    } else {
        message("已加载 'clin_data_693' 变量，正在执行后续任务。")
    }
    
    options <- yao.extract.strings(matching_variables)
    selected_options <- character(0)
    options_per_line <- 5
    cat("示例: 1, 3, 5\n")
    while (TRUE) {
        cat("请选择一个或多个选项的序号，用逗号分隔 (或输入 'done' 结束选择):\n")
        
        for (i in seq_along(options)) {
            cat(sprintf("[%2d] %-15s", i, options[i]))
            if (i %% options_per_line == 0 || i == length(options)) {
                cat("\n")
            }
        }
        user_input <- readline(prompt = "> ")
        if (user_input == "done") {
            break
        }
        selected_indices <- as.integer(unlist(strsplit(user_input, ",")))
        valid_indices <- !is.na(selected_indices) & selected_indices >= 1 & selected_indices <= length(options)
        
        if (all(valid_indices)) {
            selected_options <- c(selected_options, options[selected_indices])
        } else {
            cat("无效的输入，请选择一个或多个合法的序号，用逗号分隔，或输入 'done' 结束选择。\n")
        }
    }
    if (length(selected_options) == 0) {
        cat("未选择任何选项。\n")
        return(NULL)
    }
    selected_options <- paste0("_", selected_options, "_")
    regex_pattern <- paste0(".*", selected_options, ".*")
    chose <- c()
    for (i in 1:length(regex_pattern)) {
        chose[i] <- grep(regex_pattern[i], matching_variables, value = TRUE)
    }
    if (length(chose) > 0) {
        tmp <- list()
        for (i in 1:length(chose)) {
            tmp[[i]] <- get(chose[i])
        }
        merged_rna <- do.call(cbind, tmp)
        merged_rna <- merged_rna[, unique(colnames(merged_rna))]
        merged_clin <- clin_data_693[unique(colnames(merged_rna)), ]
        data <- list()
        data[["rnaset"]] <- merged_rna
        data[["clinset"]] <- merged_clin
        return(data)
    } else {
        cat("未找到匹配的变量。\n")
        return(NULL)
    }
}

yao.runUinvCoxs <- function(data_rna, data_clin, target_gene) {
    
    library(DESeq2)
    library(survival)
    
    # target_gene <- c("ABCG2", "ABCA7", "ABCB4", "ABCA1", 
    #                  "ABCD3", "ABCA12", "ABCA13", "ABCB10", 
    #                  "ABCB6", "ABCC4", "ABCF2", "ABCF3", 
    #                  "ABCB11", "ABCC12", "ABCG8", "ABCB9", "ABCG4")
    
    kep_high_gene <- rowSums(data_rna) >= ceiling(dim(data_rna)[2] * 0.25)
    data_rna <- data_rna[kep_high_gene, ]
    data_clin <- dplyr::select(data_clin, OS, Censor)
    colnames(data_clin) <- c("survive_time", "survive_state")
    
    con_num <- floor(ncol(data_rna)/2)
    trt_num <- ceiling(ncol(data_rna)/2)
    
    group_con_trt <- c(rep("con", con_num), rep("trt", trt_num))
    group_con_trt <- factor(group_con_trt)
    
    colData <- data.frame(row.names = colnames(data_rna), condition = group_con_trt)
    dds <- DESeqDataSetFromMatrix(countData = data_rna, colData = colData, design = ~condition)
    vsd <- assay(vst(dds), blind = FALSE)
    rna_data_target.gene <- vsd[target_gene, ]
    
    rna_data_target.gene <- t(rna_data_target.gene)
    
    coxdata <- cbind(data_clin[, c(1,2)], rna_data_target.gene)
    univ_formulas <- sapply(colnames(rna_data_target.gene), function(x) as.formula(paste('Surv(survive_time, survive_state)~', x)))
    univ_models <- lapply(univ_formulas, function(x) {coxph(x, data = coxdata)})
    univ_results <- lapply(univ_models, function(x) {
        x <- summary(x)
        p.value <- signif(x$wald["pvalue"], digits = 2)
        HR <- signif(x$coef[2], digits = 2)  # exp(beta)
        HR.confint.lower <- signif(x$conf.int[, "lower .95"], 2)
        HR.confint.upper <- signif(x$conf.int[, "upper .95"], 2)
        CI <- paste0(" (", HR.confint.lower, "-", HR.confint.upper, ")")
        res <- c(HR, CI, p.value)
        names(res) <- c("HR", "95% CI", "p.value")
        return(res)
    })
    
    univ_cox_result_df <- as.data.frame(do.call(rbind, univ_results))
    univ_cox_result_df$p.value <- as.numeric(univ_cox_result_df$p.value)
    univ_cox_sig_gene <- rownames(univ_cox_result_df[univ_cox_result_df$p.value < 0.05, ])
    
    result <- list(univ_cox_result_df = univ_cox_result_df, univ_cox_sig_gene = univ_cox_sig_gene)
    return(result)
}

yao.cre.coxdata <- function(data_rna, data_clin, target_gene){
    
    library(DESeq2)
    
    kep_high_gene <- rowSums(data_rna) >= ceiling(dim(data_rna)[2] * 0.25)
    data_rna <- data_rna[kep_high_gene, ]
    data_clin <- dplyr::select(data_clin, OS, Censor)
    colnames(data_clin) <- c("survive_time", "survive_state")
    
    con_num <- floor(ncol(data_rna)/2)
    trt_num <- ceiling(ncol(data_rna)/2)
    
    group_con_trt <- c(rep("con", con_num), rep("trt", trt_num))
    group_con_trt <- factor(group_con_trt)
    
    colData <- data.frame(row.names = colnames(data_rna), condition = group_con_trt)
    dds <- DESeqDataSetFromMatrix(countData = data_rna, colData = colData, design = ~condition)
    vsd <- assay(vst(dds), blind = FALSE)
    rna_data_target.gene <- vsd[target_gene, ]
    
    rna_data_target.gene <- t(rna_data_target.gene)
    
    coxdata <- cbind(data_clin[, c(1,2)], rna_data_target.gene)
    return(coxdata)
}

yao.pic.unicox <- function(rna, clin, target_gene, main = "Hazard Ratio",
                           breaks = c(0.5, 1, 2), cpositions = c(0.02, 0.18, 0.4, 0.22), 
                           fontsize = 0.7, noDigits = 2, refLabel = 'reference',
                           confirm = 1, filename = "PIC_UNICON_TABLE"){
    library(ggplot2)
    
    if (confirm == 1) {
        cat("Function Parameters:\n")
        cat("1.rna: RNA data\n")
        cat("Example:\n")
        cat("         CGGA_1001 CGGA_1006 CGGA_1007 CGGA_1011 CGGA_1015\n")
        cat("A1BG            2         5         9         9         1\n")
        cat("A1CF            0        61         0         0         2\n")
        cat("A2M         30059      8399     13184     54130     52877\n")
        cat("A2ML1           4       290       249       100         3\n")
        cat("A3GALT2         0        47         3         6        16\n")
        cat("2.clin: Clinical data\n")
        cat("Example:\n")
        cat("          PRS_type Histology   Grade Gender Age   OS Censor Radio_status Chemo_status IDH_mutation_status\n")
        cat("CGGA_1001  Primary       GBM  WHO IV   Male  11 3817      0            0            1            Wildtype\n")
        cat("CGGA_1006  Primary        AA WHO III   Male  42  254      1            1            1            Wildtype\n")
        cat("CGGA_1007  Primary       GBM  WHO IV Female  57  345      1            1            1            Wildtype\n")
        cat("CGGA_1011  Primary       GBM  WHO IV Female  46  109      1            1            0            Wildtype\n")
        cat("CGGA_1015  Primary       GBM  WHO IV   Male  62  164      1            1            0            Wildtype\n")
        
        cat("3.target_gene: List of target genes\n")
        cat("4.main: Title for the plot\n")
        cat("5.breaks: Breaks for the y-axis\n")
        cat("6.cpositions: Positions for various elements\n")
        cat("   - The first value corresponds to the positioning of Gene in the plot\n")
        cat("   - The second value corresponds to the positioning of Number in the plot\n")
        cat("   - The third value corresponds to the positioning of Estimate(CI) in the plot\n")
        cat("   - The fourth value corresponds to the positioning of P value in the plot\n")
        cat("7.fontsize: Font size for annotations\n")
        cat("8.noDigits: Number of digits for formatting\n")
        cat("9.refLabel: Label for reference\n")
        cat("10,confirm: 1 to run the function, 0 to exit\n")
        
        if (!interactive()) {
            stop("Non-interactive session. Set 'confirm' to 1 or 0.")
        }
        
        confirm <- as.numeric(readline("Enter 1 to run the function, 0 to exit: "))
        
        if (confirm == 0) {
            cat("Function execution aborted.\n")
            return(NULL)
        }
    } else if (confirm == 0) {
        cat("Function execution aborted.\n")
        return(NULL)
    }
    
    target_gene <- gtools::mixedsort(target_gene)
    
    Coxdata <- yao.cre.coxdata(data_rna = rna, 
                               data_clin = clin, 
                               target_gene)
    
    univ_formulas <- sapply(target_gene, function(x) as.formula(paste('survival::Surv(survive_time, survive_state)~', x)))
    univ_models <- lapply(univ_formulas, function(x) {survival::coxph(x, data = Coxdata)})
    
    coef <- list()
    for (i in seq_along(univ_models)) {
        coef[[i]] <- as.data.frame(broom::tidy(univ_models[[i]], conf.int = TRUE))
    }
    coef <- do.call(rbind, coef)
    
    terms <- c()
    for (i in 1:length(univ_models)) {
        attr_value <- attr(univ_models[[i]]$terms, "dataClasses")[-1]
        function_name <- names(attr_value)
        terms <- c(terms, setNames(attr_value, function_name))
    }
    
    allTerms <- lapply(seq_along(terms), function(i) {
        var <- names(terms)[i]
        if (terms[i] %in% c("factor", "character")) {
            adf <- as.data.frame(table(Coxdata[, var]))
            cbind(var = var, adf, pos = 1:nrow(adf))
        }
        else if (terms[i] == "numeric") {
            data.frame(var = var, Var1 = "", Freq = nrow(Coxdata), 
                       pos = 1)
        }
        else {
            vars <- grep(paste0("^", var, "*."), coef$term, value = TRUE)
            data.frame(var = vars, Var1 = "", Freq = nrow(Coxdata), 
                       pos = seq_along(vars))
        }
    })
    
    allTermsDF <- do.call(rbind, allTerms)
    colnames(allTermsDF) <- c("var", "level", "N", "pos")
    inds <- apply(allTermsDF[, 1:2], 1, paste0, collapse = "")
    rownames(coef) <- gsub(coef$term, pattern = "`", replacement = "")
    toShow <- cbind(allTermsDF, coef[inds, ])[, c("var", "level", "N", "p.value", "estimate", "conf.low", "conf.high", "pos")]
    toShowExp <- toShow[, 5:7]
    toShowExp[is.na(toShowExp)] <- 0
    toShowExp <- format(exp(toShowExp), digits = noDigits)
    toShowExpClean <- data.frame(toShow, pvalue = signif(toShow[, 4], noDigits + 1), toShowExp)
    toShowExpClean$stars <- paste0(round(toShowExpClean$p.value, 
                                         noDigits + 1), " ", ifelse(toShowExpClean$p.value < 
                                                                        0.05, "*", ""), ifelse(toShowExpClean$p.value < 0.01, 
                                                                                               "*", ""), ifelse(toShowExpClean$p.value < 0.001, "*", 
                                                                                                                ""))
    toShowExpClean$ci <- paste0("(", toShowExpClean[, "conf.low.1"], 
                                " - ", toShowExpClean[, "conf.high.1"], ")")
    toShowExpClean$estimate.1[is.na(toShowExpClean$estimate)] = refLabel
    toShowExpClean$stars[which(toShowExpClean$p.value < 0.001)] = "<0.001 ***"
    toShowExpClean$stars[is.na(toShowExpClean$estimate)] = ""
    toShowExpClean$ci[is.na(toShowExpClean$estimate)] = ""
    toShowExpClean$estimate[is.na(toShowExpClean$estimate)] = 0
    toShowExpClean$var = as.character(toShowExpClean$var)
    toShowExpClean$var[duplicated(toShowExpClean$var)] = ""
    toShowExpClean$N <- paste0("(N=", toShowExpClean$N, ")")
    toShowExpClean <- toShowExpClean[nrow(toShowExpClean):1, ]
    
    rangeb <- range(toShowExpClean$conf.low, toShowExpClean$conf.high, na.rm = TRUE)
    rangeplot <- rangeb
    rangeplot[1] <- rangeplot[1] - diff(rangeb)
    rangeplot[2] <- rangeplot[2] + cpositions[4] * diff(rangeb)
    width <- diff(rangeplot)
    y_variable <- rangeplot[1] + cpositions[1] * width
    y_nlevel <- rangeplot[1] + cpositions[2] * width
    y_cistring <- rangeplot[1] + cpositions[3] * width
    y_stars <- rangeb[2] + 0.1
    x_annotate <- seq_len(nrow(toShowExpClean))
    annot_size_mm <- fontsize * as.numeric(grid::convertX(grid::unit(theme_get()$text$size, "pt"), "mm"))
    
    lines_data <- data.frame(
        y = breaks,
        yend = breaks
    )
    
    p <- ggplot2::ggplot(toShowExpClean, aes(seq_along(var), exp(estimate))) + 
        geom_rect(
            aes(
                xmin = seq_along(var) - 0.5, 
                xmax = seq_along(var) + 0.5, 
                ymin = exp(rangeplot[1]), 
                ymax = exp(rangeplot[2]), 
                fill = ordered((seq_along(var)) %% 2 + 1)
            ),
            color = "black",
            linewidth = 0.7
        ) +
        geom_rect(
            aes(
                xmin = nrow(toShowExpClean) + 1 - 0.5, 
                xmax = nrow(toShowExpClean) + 1 + 0.5, 
                ymin = exp(rangeplot[1]), 
                ymax = exp(rangeplot[2]), 
                fill = ordered((nrow(toShowExpClean) + 1) %% 2 + 1)
            ),
            color = "black",
            linewidth = 0.6
        ) +
        scale_fill_manual(values = c("#EEEEEE", "#E9B824"), guide = "none") + 
        geom_segment(data = lines_data, aes(x = 0.5, xend = nrow(toShowExpClean) + 0.5, y = breaks, yend = breaks),
                     linetype = "dashed", color = "#004225", linewidth = rep(0.7, length(breaks))) +
        geom_point(pch = 15, size = 2.5, col = "#361500") + 
        geom_errorbar(
            aes(ymin = exp(conf.low), ymax = exp(conf.high)), 
            width = 0.15, 
            col = "#361500"
        ) + 
        # geom_hline(yintercept = 1, linetype = 3, col = "#361500") + 
        coord_flip(ylim = exp(rangeplot)) + 
        ggtitle(main) + 
        scale_y_log10(
            name = "", 
            labels = NULL, 
            expand = c(0.02, 0.02),
            breaks = NULL
        ) + 
        theme_light() + 
        theme(
            panel.grid.minor.y = element_blank(), 
            panel.grid.minor.x = element_blank(), 
            panel.grid.major.y = element_blank(), 
            legend.position = "none", 
            panel.border = element_blank(), 
            axis.title.y = element_blank(), 
            axis.text.y = element_blank(), 
            axis.ticks.y = element_blank(), 
            plot.title = element_text(hjust = 0.5, vjust = -3, face = "bold", size = 25)
        ) + 
        xlab("") + 
        annotate(
            geom = "text", 
            x = x_annotate, 
            y = exp(y_variable), 
            label = toShowExpClean$var, 
            fontface = "bold", 
            hjust = 0, 
            size = annot_size_mm
        ) + 
        annotate(
            geom = "text", 
            x = x_annotate, 
            y = exp(y_nlevel), 
            hjust = 0, 
            label = toShowExpClean$level, 
            vjust = -0.1, 
            size = annot_size_mm
        ) + 
        annotate(
            geom = "text", 
            x = x_annotate, 
            y = exp(y_nlevel), 
            label = toShowExpClean$N, 
            fontface = "italic", 
            hjust = 0, 
            vjust = ifelse(toShowExpClean$level == "", 0.5, 1.1), 
            size = annot_size_mm
        ) + 
        annotate(
            geom = "text", 
            x = x_annotate, 
            y = exp(y_cistring), 
            label = toShowExpClean$estimate.1, 
            size = annot_size_mm, 
            vjust = ifelse(toShowExpClean$estimate.1 == "reference", 0.5, -0.1)
        ) + 
        annotate(
            geom = "text", 
            x = x_annotate, 
            y = exp(y_cistring), 
            label = toShowExpClean$ci, 
            size = annot_size_mm, 
            vjust = 1.1, 
            fontface = "italic"
        ) + 
        annotate(
            geom = "text", 
            x = x_annotate, 
            y = exp(y_stars), 
            label = toShowExpClean$stars, 
            size = annot_size_mm, 
            hjust = -0.2, 
            fontface = "italic"
        ) + 
        # annotate(
        #     geom = "text", 
        #     x = 0.5, 
        #     y = exp(y_variable), 
        #     label = paste0("# Events: ", gmodel$nevent, "; Global p-value (Log-Rank): ", 
        #                    format.pval(gmodel$p.value.log, eps = ".001"), " \nAIC: ", round(gmodel$AIC, 2), 
        #                    "; Concordance Index: ", round(gmodel$concordance, 2)), 
        #     size = annot_size_mm, 
        #     hjust = 0, 
        #     vjust = 1.2, 
        #     fontface = "italic"
    # ) +
    annotate(
        geom = "text",
        x = 18,  # 调整x位置以居中
        y = c(exp(y_variable) + 0.002, #0.0115, 
              exp(y_nlevel) + 0.0046, #018, 
              exp(y_cistring), 
              1, 
              exp(y_stars) + 1.7),  # 调整y位置以放置在第一个条形图上方
        label = c("Gene", "Number", "Estimate(CI)", "Graph", "P value"),  # 添加表头文本
        size = 4,  # 调整字体大小
        fontface = "bold"
    ) + 
        annotate(
            geom = "text",
            x = 0.5,
            y = breaks,
            label = breaks,
            size = 3.5,
            hjust = 0.4,
            vjust = 2.5
        )
    save(p, file = paste0(filename, ".Rdata"))
    ggsave(filename = paste0(filename, ".pdf"), height = 7.09, width = 9.58)
    print(p)
}


yao.lasso.none.coef <- function(data_rna, data_clin, target_gene){
    
    unicox_PRI_RECUR_SECON <- yao.runUinvCoxs(data_rna = data_rna, 
                                              data_clin = data_clin,
                                              target_gene = target_gene)
    target_gene <- unicox_PRI_RECUR_SECON$univ_cox_sig_gene
    Coxdata <- yao.cre.coxdata(data_rna, data_clin, target_gene)
    x <- data.matrix(Coxdata[, 3:length(colnames(Coxdata))])
    y <- data.matrix(Surv(Coxdata[, 1], Coxdata[, 2]))
    set.seed(100)
    fit <-  glmnet::glmnet(x, y, family = "cox", maxit = 1000)
    cv.fit <- glmnet::cv.glmnet(x, y, family = "cox", maxit = 10000)
    Coefficients <- coef(fit, s = cv.fit$lambda.min)
    Active.Index <- which(Coefficients != 0)
    Active.Coefficients  <- Coefficients[Active.Index]                         
    lasso_unicox_gene <- row.names(Coefficients)[Active.Index]
    lass_none_coef_df <- data.frame(lasso_unicox_gene = lasso_unicox_gene, Coefficients = Active.Coefficients)
    return(lass_none_coef_df)
}

yao.pic.multicox <- function(data_rna = data_rna, 
                             data_clin = data_clin, 
                             target_gene = target_gene, 
                             main = "Hazard Ratio",
                             breaks = c(0.5, 1, 2), 
                             cpositions = c(0.02, 0.14, 0.35, 0.17), 
                             fontsize = 0.7, 
                             noDigits = 2, 
                             refLabel = 'reference',
                             confirm = 1,
                             filename = "PIC_MULTICOX_TABLE"){
    library(ggplot2)
    
    if (confirm == 1) {
        cat("Function Parameters:\n")
        cat("1.data_rna: RNA data\n")
        cat("Example:\n")
        cat("         CGGA_1001 CGGA_1006 CGGA_1007 CGGA_1011 CGGA_1015\n")
        cat("A1BG            2         5         9         9         1\n")
        cat("A1CF            0        61         0         0         2\n")
        cat("A2M         30059      8399     13184     54130     52877\n")
        cat("A2ML1           4       290       249       100         3\n")
        cat("A3GALT2         0        47         3         6        16\n")
        cat("2.data_clin: Clinical data\n")
        cat("Example:\n")
        cat("          PRS_type Histology   Grade Gender Age   OS Censor Radio_status Chemo_status IDH_mutation_status\n")
        cat("CGGA_1001  Primary       GBM  WHO IV   Male  11 3817      0            0            1            Wildtype\n")
        cat("CGGA_1006  Primary        AA WHO III   Male  42  254      1            1            1            Wildtype\n")
        cat("CGGA_1007  Primary       GBM  WHO IV Female  57  345      1            1            1            Wildtype\n")
        cat("CGGA_1011  Primary       GBM  WHO IV Female  46  109      1            1            0            Wildtype\n")
        cat("CGGA_1015  Primary       GBM  WHO IV   Male  62  164      1            1            0            Wildtype\n")
        
        cat("3.target_gene: List of target genes\n")
        cat("4.main: Title for the plot\n")
        cat("5.breaks: Breaks for the y-axis\n")
        cat("6.cpositions: Positions for various elements\n")
        cat("   - The first value corresponds to the positioning of Gene in the plot\n")
        cat("   - The second value corresponds to the positioning of Number in the plot\n")
        cat("   - The third value corresponds to the positioning of Estimate(CI) in the plot\n")
        cat("   - The fourth value corresponds to the positioning of P value in the plot\n")
        cat("7.fontsize: Font size for annotations\n")
        cat("8.noDigits: Number of digits for formatting\n")
        cat("9.refLabel: Label for reference\n")
        cat("10,confirm: 1 to run the function, 0 to exit\n")
        
        if (!interactive()) {
            stop("Non-interactive session. Set 'confirm' to 1 or 0.")
        }
        
        confirm <- as.numeric(readline("Enter 1 to run the function, 0 to exit: "))
        
        if (confirm == 0) {
            cat("Function execution aborted.\n")
            return(NULL)
        }
    } else if (confirm == 0) {
        cat("Function execution aborted.\n")
        return(NULL)
    }
    
    target_gene <- target_gene
    # > target_gene
    # [1] "ABCA7"  "ABCA13" "ABCB4"  "ABCB6"  "ABCC4"  "ABCD3"  "ABCG2"  "ABCG4"  "ABCG8" 
    
    Coxdata <- yao.cre.coxdata(data_rna = data_rna,
                               data_clin = data_clin,
                               target_gene = target_gene)
    
    model <- coxph(Surv(survive_time, survive_state) ~ ., data = Coxdata)
    data <- Coxdata
    
    stopifnot(inherits(model, "coxph"))
    terms <- attr(model$terms, "dataClasses")[-1]
    
    coef <- as.data.frame(broom::tidy(model, conf.int = TRUE))
    gmodel <- broom::glance(model)
    
    allTerms <- lapply(seq_along(terms), function(i) {
        var <- names(terms)[i]
        if (terms[i] %in% c("factor", "character")) {
            adf <- as.data.frame(table(data[, var]))
            cbind(var = var, adf, pos = 1:nrow(adf))
        }
        else if (terms[i] == "numeric") {
            data.frame(var = var, Var1 = "", Freq = nrow(data), 
                       pos = 1)
        }
        else {
            vars = grep(paste0("^", var, "*."), coef$term, value = TRUE)
            data.frame(var = vars, Var1 = "", Freq = nrow(data), 
                       pos = seq_along(vars))
        }
    })
    
    allTermsDF <- do.call(rbind, allTerms)
    
    colnames(allTermsDF) <- c("var", "level", "N", "pos")
    inds <- apply(allTermsDF[, 1:2], 1, paste0, collapse = "")
    rownames(coef) <- gsub(coef$term, pattern = "`", replacement = "")
    toShow <- cbind(allTermsDF, coef[inds, ])[, c("var", "level", "N", "p.value", "estimate", "conf.low", "conf.high", "pos")]
    toShowExp <- toShow[, 5:7]
    toShowExp[is.na(toShowExp)] <- 0
    toShowExp <- format(exp(toShowExp), digits = 2)
    toShowExpClean <- data.frame(toShow, pvalue = signif(toShow[, 4], noDigits + 1), toShowExp)
    toShowExpClean$stars <- paste0(round(toShowExpClean$p.value, noDigits + 1), " ", 
                                   ifelse(toShowExpClean$p.value < 0.05, "*", ""), 
                                   ifelse(toShowExpClean$p.value < 0.01, "*", ""), 
                                   ifelse(toShowExpClean$p.value < 0.001, "*", ""))
    
    toShowExpClean$ci <- paste0("(", toShowExpClean[, "conf.low.1"], " - ", toShowExpClean[, "conf.high.1"], ")")
    
    toShowExpClean$estimate.1[is.na(toShowExpClean$estimate)] = refLabel
    toShowExpClean$stars[which(toShowExpClean$p.value < 0.001)] = "<0.001 ***"
    toShowExpClean$stars[is.na(toShowExpClean$estimate)] = ""
    toShowExpClean$ci[is.na(toShowExpClean$estimate)] = ""
    toShowExpClean$estimate[is.na(toShowExpClean$estimate)] = 0
    toShowExpClean$var = as.character(toShowExpClean$var)
    toShowExpClean$var[duplicated(toShowExpClean$var)] = ""
    toShowExpClean$N <- paste0("(N = ", toShowExpClean$N, ")")
    toShowExpClean <- toShowExpClean[nrow(toShowExpClean):1, ]
    
    rangeb <- range(toShowExpClean$conf.low, toShowExpClean$conf.high, na.rm = TRUE)
    rangeplot <- rangeb
    rangeplot[1] <- rangeplot[1] - diff(rangeb)
    rangeplot[2] <- rangeplot[2] + cpositions[4] * diff(rangeb) + 0.17
    width <- diff(rangeplot)
    y_variable <- rangeplot[1] + cpositions[1] * width
    y_nlevel <- rangeplot[1] + cpositions[2] * width
    y_cistring <- rangeplot[1] + cpositions[3] * width
    y_stars <- rangeb[2] + 0.1
    x_annotate <- seq_len(nrow(toShowExpClean))
    annot_size_mm <- fontsize * as.numeric(grid::convertX(grid::unit(theme_get()$text$size, "pt"), "mm"))
    
    lines_data <- data.frame(
        y = breaks,
        yend = breaks
    )
    
    p <- ggplot2::ggplot(toShowExpClean, aes(seq_along(var), exp(estimate))) + 
        geom_rect(
            aes(
                xmin = seq_along(var) - 0.5, 
                xmax = seq_along(var) + 0.5, 
                ymin = exp(rangeplot[1]), 
                ymax = exp(rangeplot[2]), 
                fill = ordered((seq_along(var)) %% 2 + 1)
            ),
            color = "black",
            linewidth = 0.7
        ) +
        geom_rect(
            aes(
                xmin = nrow(toShowExpClean) + 0.5, 
                xmax = nrow(toShowExpClean) + 1.5, 
                ymin = exp(rangeplot[1]), 
                ymax = exp(rangeplot[2]), 
                fill = ordered((nrow(toShowExpClean) + 1) %% 2 + 1)
            ),
            color = "black",
            linewidth = 0.6
        ) +
        scale_fill_manual(values = c("#EEEEEE", "#35A29F"), guide = "none") +
        geom_segment(data = lines_data, aes(x = 0.5, xend = nrow(toShowExpClean) + 0.5, y = breaks, yend = breaks), 
                     linetype = "dashed", color = "#004225", linewidth = rep(0.7, length(breaks))) +
        geom_point(pch = 15, size = 2.5, col = "#004225") + 
        geom_errorbar(
            aes(ymin = exp(conf.low), ymax = exp(conf.high)), 
            width = 0.15, 
            col = "#004225"
        ) + 
        coord_flip(ylim = exp(rangeplot)) + 
        ggtitle(main) + 
        scale_y_log10(
            name = "", 
            labels = NULL, 
            expand = c(0.02, 0.02),
            breaks = NULL
        ) + 
        theme_light() + 
        theme(
            panel.grid.minor.y = element_blank(), 
            panel.grid.minor.x = element_blank(), 
            panel.grid.major.y = element_blank(), 
            legend.position = "none", 
            panel.border = element_blank(), 
            axis.title.y = element_blank(), 
            axis.text.y = element_blank(), 
            axis.ticks.y = element_blank(), 
            plot.title = element_text(hjust = 0.5, vjust = -1, face = "bold", size = 25)
        ) + 
        xlab("") + 
        annotate(
            geom = "text", 
            x = x_annotate, 
            y = exp(y_variable), 
            label = toShowExpClean$var, 
            fontface = "bold", 
            hjust = 0, 
            size = annot_size_mm
        ) + 
        annotate(
            geom = "text", 
            x = x_annotate, 
            y = exp(y_nlevel), 
            hjust = 0, 
            label = toShowExpClean$level, 
            vjust = -0.1, 
            size = annot_size_mm
        ) + 
        annotate(
            geom = "text", 
            x = x_annotate, 
            y = exp(y_nlevel), 
            label = toShowExpClean$N, 
            fontface = "italic", 
            hjust = 0, 
            vjust = ifelse(toShowExpClean$level == "", 0.5, 1.1), 
            size = annot_size_mm
        ) + 
        annotate(
            geom = "text", 
            x = x_annotate, 
            y = exp(y_cistring), 
            label = toShowExpClean$estimate.1, 
            size = annot_size_mm, 
            vjust = ifelse(toShowExpClean$estimate.1 == "reference", 0.5, -0.1)
        ) + 
        annotate(
            geom = "text", 
            x = x_annotate, 
            y = exp(y_cistring), 
            label = toShowExpClean$ci, 
            size = annot_size_mm, 
            vjust = 1.1, 
            fontface = "italic"
        ) + 
        annotate(
            geom = "text", 
            x = x_annotate, 
            y = exp(y_stars), 
            label = toShowExpClean$stars, 
            size = annot_size_mm, 
            hjust = -0.2, 
            fontface = "italic"
        ) +
        annotate(
            geom = "text",
            x = 0.5,
            y = exp(y_variable),
            label = paste0("# Events: ", gmodel$nevent, "; Global p-value (Log-Rank): ",
                           format.pval(gmodel$p.value.log, eps = ".001"), " \nAIC: ", round(gmodel$AIC, 2),
                           "; Concordance Index: ", round(gmodel$concordance, 2)),
            size = annot_size_mm,
            hjust = 0,
            vjust = 1.2,
            fontface = "italic"
        ) +
        annotate(
            geom = "text",
            x = nrow(toShowExpClean) + 1,
            y = c(exp(y_variable) + 0.0039, 
                  exp(y_nlevel) + 0.01, 
                  exp(y_cistring), 
                  1, 
                  exp(y_stars) + 0.6),
            label = c("Gene", "Number", "Estimate(CI)", "Graph", "P value"),  # 添加表头文本
            size = 4,  # 调整字体大小
            fontface = "bold"
        ) + 
        annotate(
            geom = "text",
            x = 0,
            y = breaks,
            label = breaks,
            size = 3.5,
            hjust = 0.4,
            vjust = 0
        )
    save(p, file = paste0(filename, ".Rdata"))
    ggsave(filename = paste0(filename, ".pdf"), height = 18.2/2.54, width = 20.96/2.54)
    print(p)
}

yao.runMulticox <- function(data_rna = data_rna,
                            data_clin = data_clin,
                            target_gene = target_gene){
    Coxdata <- yao.cre.coxdata(data_rna = data_rna,
                               data_clin = data_clin,
                               target_gene = target_gene)
    model <- coxph(Surv(survive_time, survive_state) ~ ., data = Coxdata)
    coef <- as.data.frame(broom::tidy(model, conf.int = TRUE))
    multicox_sig_gene <- coef$term[coef$p.value < 0.05]
    coefficient <- coef(model)[coef$p.value < 0.05]
    tmp <- data.frame(multicox_sig_gene = multicox_sig_gene,
                      coefficient = coefficient)
    tmp <- tmp[order(tmp$coefficient), ]
    tmp$multicox_sig_gene <- factor(tmp$multicox_sig_gene, levels = tmp$multicox_sig_gene)
    return(tmp)
}

yao.create.riskScore <- function(data_rna = data_rna,
                                 data_clin = data_clin,
                                 target_gene = target_gene){
    res_Multicox <- yao.runMulticox(data_rna = data_rna,
                                    data_clin = data_clin,
                                    target_gene = target_gene)
    res_Multicox$multicox_sig_gene <- as.character(res_Multicox$multicox_sig_gene)
    
    Coxdata <- yao.cre.coxdata(data_rna = data_rna,
                               data_clin = data_clin,
                               target_gene = res_Multicox$multicox_sig_gene)
    
    data_exp <- Coxdata[, res_Multicox$multicox_sig_gene]
    data_coeffient <- res_Multicox
    
    column_names <- colnames(data_exp)
    exp.coeffient <- sapply(column_names, function(col_name) {
        col_index <- which(colnames(data_exp) == col_name)
        row_index <- which(rownames(data_coeffient) == col_name)
        data_exp[, col_index] * data_coeffient$coefficient[row_index]
    })
    riskScore <- apply(exp.coeffient, 1, sum)
    riskScore <- as.data.frame(riskScore)
    riskScore$sample <- rownames(data_exp)
    data2pic <- cbind(Coxdata[, c(1,2)], riskScore)
    data2pic$risk.grade <- ifelse(data2pic$riskScore > median(data2pic$riskScore), "High","Low")
    
    data2pic <- data2pic %>% dplyr::arrange(riskScore)
    data2pic$num <- seq(1, length(rownames(data2pic)))
    data2pic$event <- ifelse(data2pic$survive_state == "1", "Death", "Alive")
    data2pic$event <- factor(data2pic$event)
    
    test <- list()
    test[["riskScore"]] <- riskScore
    test[["data2pic"]] <- data2pic
    
    return(test)
}
