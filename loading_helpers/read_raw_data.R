trunc <- function(x, ..., prec = 4) base::trunc(x * 10^prec, ...) / 10^prec


read_data <- function(x) {
  ##read the header from the file to paste back into the new file
  header <- scan(paste0(ET_DATA_DIR, x),
                 what = character(), nlines = MAX_HEADER_ROWS, 
                 sep="\n", quiet=TRUE)
  
  #find last header row
  header_rows <- sum(sapply(1:length(header), 
                            function(x) substring(header[x], 1, 1) == "#"))
  
  
  # Deal with idiosyncacies caused by different number of calib points
  result <- fread(paste0(ET_DATA_DIR, x), skip = header_rows,
                  integer64 = "numeric") %>%
    mutate(subj = rename_subj(x))
  
  # Deal with Timestamps greater than R's max 32bit int
  if(max(result$Time) > .Machine$integer.max){
    result %<>% 
      mutate(Time = Time - min(Time) + 1,
             Time = as.integer(Time))
  }
  
  return(result)
}


# strip filenames down to subject ids
rename_asd_subj <- function(subj) {
  subj <- sub("-eye_data Samples_fixed.txt","",subj)
  subj <- paste0(strsplit(subj,"_")[[1]][1:2],collapse="_")
}
