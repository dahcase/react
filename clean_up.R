#clean up directory
out_dir = "/media/dan/react_data/post_proc//output_MOD11A2"
dirs= list.dirs(out_dir)
dirs = dirs[nchar(basename(dirs)) == 6]

fff = unlist(lapply(dirs, function(d) list.files(d, full.names = T)))

library('data.table')

files = data.table(fff = fff)
files[, directs := basename(dirname(fff))]
files[, direct_implied:= stringr::str_extract(basename(fff), pattern = 'h[0-9][0-9]v[0-9][0-9]')]
files[,keep_me:= directs == direct_implied]

file.remove(files[keep_me == F, fff])
