if(!dir.exists("L:/")) message("You need to have L drive mapped")
.libPaths(c("L:/newlib", .libPaths()))
source(file = "cong_setting_part1.R")