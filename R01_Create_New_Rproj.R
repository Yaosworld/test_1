# 清空R环境中的所有对象
rm(list = ls())

source("R02_Yao_Basic_Function.R")

# 获取当前工作目录
work_dir <- getwd()

# 设置文件名和父目录路径
file_name <- "Code_Style_Check"
parent_dir <- "E:\\Yaos_Rproj\\"
target_dir <- paste0(parent_dir, file_name)

# 创建目标文件夹（如果不存在）
if (!file.exists(target_dir)) {
    dir.create(target_dir)
}

# 指定源文件和目标文件夹路径
source_file <- c("R01_Create_New_Rproj.R", "R02_Yao_Basic_Function.R", "R03_Yao_Bio_Function.R")

# 使用file.copy()函数复制文件，包括子目录
file.copy(source_file, target_dir, recursive = TRUE)

# 设置源工作目录（如果需要的话，请将其替换为你的实际工作目录）
setwd(target_dir)

yao.initial.dir()

# 使用shell.exec函数打开目标文件夹
shell.exec(target_dir)

开始手动创建新 .Rproj 文件




