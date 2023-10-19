

args = (commandArgs(TRUE))
directory = args[1]
out_tab_path = args[2]


list_table = list.files(directory,full.names=T)
information_table = data.frame()
for (file in list_table){print(file)
    information_table = rbind(information_table,read.delim(file))
}


write.table(information_table,out_tab_path, row.names=F, col.names=T, sep="\t", quote=F)
