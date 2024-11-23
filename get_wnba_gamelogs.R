library(dplyr)

files <- list.files("~/pareto-app/data/wnba")[-1]

for(file in files) {

  type <- stringr::str_sub(tools::file_path_sans_ext(file), -3)
  
    frame <- arrow::read_parquet(paste0("~/pareto-app/data/wnba/", file)) 

  if(type == "REG") {

    frame <- frame |> 
      mutate(type_season = "Regular Season")

  } else {

    frame <- frame |> 
      mutate(type_season = "Playoffs")

  }
  
  arrow::write_parquet(frame, paste0("~/pareto-app/data/wnba/", file))

}


files <- list.files("~/pareto-app/data/wnba")[-1]

file_list <- vector("list")

for(file in files) {

  logs <- arrow::read_parquet(paste0("~/pareto-app/data/wnba/", file)) 

  file_list[[which(files == file)]] <- logs

}

full_frame <- bind_rows(file_list)
arrow::write_parquet(full_frame, paste0("~/pareto-app/pareto/wnba_logs.parquet"))
