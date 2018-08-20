prefix <- "https://scholar.google.com/scholar?hl=en&as_sdt=0%2C39&q="
refs$link <- paste0(paste0(paste0(paste0(paste0("<a href='", prefix), refs$reference), "' target='_blank'>"), refs$code), "</a>")
write(refs$link, file="reference_links.txt", sep="/n")
