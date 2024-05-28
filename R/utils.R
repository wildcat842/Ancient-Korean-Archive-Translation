library(tidyverse)
library(sacRebleu)
library(sentencepiece)
library(RColorBrewer)

## Load sentence piece Model


fn_sp <- list.files("Corpus",pattern = "sentencepiece.model", full.names = TRUE)[1]
model <- sentencepiece::sentencepiece_load_model(file = fn_sp)

get_token <- function(x){
  sentencepiece_encode(model, x)[[1]] %>% 
    data.frame(x = .) %>% left_join(model$vocabulary, by = c("x" = "subword")) %>% 
    select(id) %>% unname() %>% as.list()
}

get_bleu <- function(input, output){
  #input = c(1,2,3); output = c(1,2,3); get_bleu(input, output)
  #input = list(c(1,2,3)); output = list(c(1,2,3)); get_bleu(input, output)
  ref_corpus <- list(); ref_corpus[[1]] <- list()
  cand_corpus <- list()
  
  if (is.list(input) == TRUE && is.list(output) == TRUE){
    ref_corpus[[1]][[1]] <- input[[1]]
    cand_corpus[[1]] <- output[[1]]
    
  }
  else if (is.numeric(input) == TRUE && is.numeric(input)) {
    ref_corpus[[1]][[1]] <- input
    cand_corpus[[1]] <- output
  }
  else return(0)
  
  score1 <- bleu_corpus_ids(ref_corpus, cand_corpus)
  score2 <- bleu_corpus_ids(ref_corpus, cand_corpus,smoothing="floor", epsilon=0.0 )
  score3 <- bleu_corpus_ids(ref_corpus, cand_corpus, smoothing="add-k", k=1)
  return(c(standard = score1, floor = score2, add_k =  score3))
}


# colnames should be predictions
get_df_with_bleu <- function(test, talk = 50){
  scores = list()
  
  for (i in 1:nrow(test)){
    input_sentence = test$predictions[i] %>% str_remove_all(" ")
    output_sentence = test$modern_ko[i] %>% str_remove_all(" ")
    
    input = get_token(input_sentence)
    output = get_token(output_sentence)
    
    score = get_bleu(input, output)
    scores[[i]] = score
    
    if (i%%talk == 0){
      cat(i, ":", input_sentence, " ==> ", output_sentence, "\n==", score, "\n")
    }
    
  }
  
  standard <- lapply(scores, function(x) x[1]  ) %>% unlist()
  floor <- lapply(scores, function(x) x[2]  ) %>% unlist()
  add_k <- lapply(scores, function(x) x[3]  ) %>% unlist()
  
  ## make data frame
  test$standard <- standard
  test$floor <- floor
  test$add_k <- add_k
  
  test = test %>% mutate(length = str_length(ancient_ch)) 
  
  test = test %>% mutate(str_len = cut_number(test$length, 5)) 
  
}


get_df_with_score <- function(test, scores){
  standard <- lapply(scores, function(x) x[1]  ) %>% unlist()
  floor <- lapply(scores, function(x) x[2]  ) %>% unlist()
  add_k <- lapply(scores, function(x) x[3]  ) %>% unlist()
  
  ## make data frame
  test$standard <- standard
  test$floor <- floor
  test$add_k <- add_k
  
  test = test %>% mutate(length = str_length(ancient_ch)) 
  test = test %>% mutate(str_len = cut_number(test$length, 5)) 
  
}

rm(fn_sp)

## GGPlot #####################################################

plot_bleu <- function(x, df){
  colors = palette.colors(8, "Accent")
  test <- df
  chart_nm1 <- x[1]
  chart_nm2 <- x[2]
  
  test %>% pivot_longer(cols = standard:add_k, names_to = "type", 
                        values_to = "score") %>% 
    ggplot(aes(x = score, fill = type))  + geom_density(alpha = 0.2) + 
    xlab(label = "BLEU Score") + ylab(label = "Density") + 
    theme_bw() + 
    scale_fill_manual(name = "", values = colors) + 
    scale_y_sqrt() 
  
  ggsave(filename = chart_nm1,
         width = 4.12*2, height = 2.67*2, units = "in",
         dpi = 600)
  
  
  test %>% mutate(str_len = cut_number(test$length, 5) ) %>% 
    pivot_longer(cols = standard:add_k, names_to = "type", 
                 values_to = "score") %>% 
    ggplot(aes(x = str_len, y = score, fill = type)) +
    geom_boxplot() + 
    theme_bw() + 
    xlab(label = "length of ancient string") + 
    ylab(label = "BLEU Score") + 
    scale_fill_manual(name = "", values = colors) + 
    scale_y_sqrt() 
  
  ggsave(filename = chart_nm2,
         width = 4.12*2, height = 2.67*2, units = "in",
         dpi = 600)
  
  cat("[Notice]", chart_nm1, chart_nm2, "is created")
  
}

