set1 = paste0('https://ngdc.noaa.gov/eog/data/web_data/v4composites/F16',2004:2009,'.v4.tar')
set2 = paste0('https://ngdc.noaa.gov/eog/data/web_data/v4composites/F18',2010:2013,'.v4.tar')

all_links = c(set1, set2)

outdir = '/media/dan/react_data/post_proc/output_dmspntl'
dir.create(outdir)

for(lll in all_links){
  print(lll)
  download.file(lll, paste0(outdir,'/', basename(lll)))
  
}