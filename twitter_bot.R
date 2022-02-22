pacman::p_load(tidyverse,ggplot2,ggrepel,bigrquery,googlesheets4,googledrive,jsonlite,janitor,tidyRSS,lubridate,anytime,rtweet)
my_secrets = read_json("/home/cujo253/mines_of_moria/Essential_Referential_CSVS/personal_data.json")
'%!in%' <- function(x,y)!('%in%'(x,y))

# Anything with "cyber" in the names are my visuals, with colours based on cyberpunk themes.
# I will not provide commentation inside of them, but they are tailored to the data for each daily release.
# as the month went on I stretched more and more for content, and thus needed more and more visual functions,
# Though I'd hoped to keep them as few as possible bc I personally don't find them particularly enjoyable. 
cujos_cyber_lines <- function(df, 
                              y_col,
                              y_format = NULL,
                              main = NULL,
                              ylab = NULL,
                              xlab = NULL,
                              cap = "Powered By MTGBAN.com",
                              force_glow = T){
  area = FALSE
  main.size = 20
  bg.col = "#222035"
  grid.col = "#242d4d"
  text.col = "Green"
  xlim = NULL
  ylim = NULL
  
  #y_format = "dollars"
  glow = TRUE
  n_lines <- 10
  diff_linewidth <- 0.65
  alpha_value <- 0.05
  #df = logic
  y_m <- df
  
  if(is.na(unique(y_m$number))){
    y_m = y_m %>% mutate(hasFoil = ifelse(hasFoil == 1, "Foil","Non Foil"),
                         identifier = paste(card,hasFoil))
  }else{
    y_m = y_m %>% mutate(hasFoil = ifelse(hasFoil == 1, "Foil","Non Foil"),
               identifier = paste(card,set,number,hasFoil))
  }
  
  if(unique(y_m$rarity)=="S"){
    y_m = y_m %>% mutate(identifier = paste(card))
  }
  
  # https://blog.depositphotos.com/15-cyberpunk-color-palettes-for-dystopian-designs.html
  choices = y_m%>%select(card)%>%distinct()%>% nrow()
  
  if(choices <= 5){
    lwd = 1.75
  }else if( (choices > 5)&(choices <= 10) ){
    lwd = 1.25
  }else if( (choices > 10)&(choices <= 15) ){
    lwd = .75
  }else{
    lwd = .50
  }
  
  
  col <- colorRampPalette(c("#ff184c", "#ff577d", "#ffccdc", "#0a9cf5", "#003062",
                            "#ff124f", "#ff00a0", "#fe75fe", "#7a04eb", "#ff6e27",
                            "#7700a6", "#fe00fe", "#defe47","#00b3f3","#0016ee"))(choices)
  
  
  
  p <- ggplot(data = y_m, aes(x = Date, y = {{y_col}}, group = identifier, colour = identifier, fill = identifier)) +
    geom_line(size = lwd) +
    scale_color_manual(values = col) + 
    geom_line(size = 0.75 + (diff_linewidth), alpha = alpha_value) 
  
  
  if(is.null(y_format)){
    p = p + scale_y_continuous(limits = ylim)
    
    p <- p  +
      labs(x = xlab, y = ylab, title = main, caption = cap) +
      theme(plot.background = element_rect(fill = bg.col, colour = bg.col),
            panel.grid = element_line(colour = grid.col, size = 1),
            axis.text = element_text(colour = '#05d9e8', size = 10),
            axis.title.y = element_text(angle=0,vjust = 0.5),
            axis.title = element_text(colour = '#05d9e8',size=10, face = 'bold'),
            legend.background = element_blank(),
            legend.key = element_blank(),
            legend.position = "top",
            plot.title = element_text(colour = "#05d9e8", size = 13,hjust = .5, face = 'bold'),
            plot.caption = element_text(colour = "#05d9e8", size = 6, face = 'bold'),
            legend.title = element_blank(),
            legend.text = element_text(colour = '#05d9e8', size = 11, face = 'bold'),
            plot.margin = margin(1.5, 2, 1.5, 1.5, "cm"),
            panel.background = element_rect(fill = bg.col, colour = bg.col)) +
      geom_label_repel(aes(Date, {{y_col}},  label = ({{y_col}})),
                       data = rbind(y_m[1,],y_m[7,],y_m[14,],y_m[21,],y_m[28,]), 
                       fontface = "bold",
                       box.padding = unit(0.45, "lines"),
                       point.padding = unit(0.45, "lines"),
                       nudge_y = 1,
                       colour = "#ff184c",
                       fill = "black",
                       show.legend = FALSE)
      
  }else if(tolower(y_format) == "dollars"){
    p = p +
      scale_y_continuous(labels=scales::dollar_format(),limits = ylim)
    
    p <- p  +
      labs(x = xlab, y = ylab, title = main, caption = cap) +
      theme(plot.background = element_rect(fill = bg.col, colour = bg.col),
            panel.grid = element_line(colour = grid.col, size = 1),
            axis.text = element_text(colour = '#05d9e8', size = 10),
            axis.title.y = element_text(angle=0,vjust = 0.5),
            axis.title = element_text(colour = '#05d9e8',size=10, face = 'bold'),
            legend.background = element_blank(),
            legend.key = element_blank(),
            legend.position = "top",
            plot.title = element_text(colour = "#05d9e8", size = 13,hjust = .5, face = 'bold'),
            plot.caption = element_text(colour = "#05d9e8", size = 6, face = 'bold'),
            legend.title = element_blank(),
            legend.text = element_text(colour = '#05d9e8', size = 11, face = 'bold'),
            plot.margin = margin(1.5, 2, 1.5, 1.5, "cm"),
            panel.background = element_rect(fill = bg.col, colour = bg.col)) +
      geom_label_repel(aes(Date, {{y_col}},  label = scales::dollar({{y_col}})),
                       data = rbind(y_m[1,],y_m[7,],y_m[14,],y_m[21,],y_m[28,]), 
                       fontface = "bold",
                       box.padding = unit(0.45, "lines"),
                       point.padding = unit(0.45, "lines"),
                       nudge_y = 1,
                       colour = "#ff184c",
                       fill = "black",
                       show.legend = FALSE)
  }else if(tolower(y_format)=="percent"){
    p = p +
      scale_y_continuous(labels=scales:::percent_format(),limits = ylim)
    
    p <- p  +
      labs(x = xlab, y = ylab, title = main, caption = cap) +
      theme(plot.background = element_rect(fill = bg.col, colour = bg.col),
            panel.grid = element_line(colour = grid.col, size = 1),
            axis.text = element_text(colour = '#05d9e8', size = 10),
            axis.title.y = element_text(angle=0,vjust = 0.5),
            axis.title = element_text(colour = '#05d9e8',size=10, face = 'bold'),
            legend.background = element_blank(),
            legend.key = element_blank(),
            legend.position = "top",
            plot.title = element_text(colour = "#05d9e8", size = 13,hjust = .5, face = 'bold'),
            plot.caption = element_text(colour = "#05d9e8", size = 6, face = 'bold'),
            legend.title = element_blank(),
            legend.text = element_text(colour = '#05d9e8', size = 11, face = 'bold'),
            plot.margin = margin(1.5, 2, 1.5, 1.5, "cm"),
            panel.background = element_rect(fill = bg.col, colour = bg.col)) +
      geom_label_repel(aes(Date, {{y_col}},  label = scales::percent({{y_col}})),
                       data = rbind(y_m[1,],y_m[7,],y_m[14,],y_m[21,],y_m[28,]), 
                       fontface = "bold",
                       box.padding = unit(0.45, "lines"),
                       point.padding = unit(0.45, "lines"),
                       nudge_y = 1,
                       colour = "#ff184c",
                       fill = "black",
                       show.legend = FALSE)
  }

  
  return(p)
  
}

cujos_cyber_bars <- function(df, 
                             y_col,
                             y_format = NULL,
                             palette = 1, 
                             area = TRUE, 
                             main = NULL, 
                             main.size = 20, 
                             cap = "Powered By MTGBAN.com", 
                             xlab = NULL, 
                             ylab = NULL 
                            ){
  #df = logic
  bg.col = "#212946"
  grid.col = "#242d4d"
  text.col = "#05d9e8" 
  lwd = 1.75
  xlim = NULL
  ylim = NULL
  df = df %>% head(10) 
  # https://blog.depositphotos.com/15-cyberpunk-color-palettes-for-dystopian-designs.html
  
  if(palette == 1) {
    col <- colorRampPalette(c("#00ff9f", "#00b8ff", "#001eff", "#bd00ff", "#d600ff",
                              "#ff124f", "#ff00a0", "#fe75fe", "#7a04eb", "#ff6e27"))(nrow(df))
  }
  
  p <- ggplot(df, aes(x = reorder(card,desc({{y_col}})), y = {{y_col}}, label = round({{y_col}},0) ))
  
  if(area == FALSE) {
    p <- p + geom_bar(stat = "identity", fill = NA, color = col, size = 2)
  } else {
    p <- p + geom_bar(stat = "identity", fill = col)
  }
  
  if(is.null(y_format)){
    p = p + scale_y_continuous(limits = ylim)
    
  }else if(tolower(y_format) == "dollars"){
    p = p +
      scale_y_continuous(labels=scales::dollar_format(),limits = ylim)
  }else if(tolower(y_format)=="percent"){
    p = p +
      scale_y_continuous(labels=scales:::percent_format(),limits = ylim)
  }
  
  p <- p + labs(x = xlab, y = ylab, title = main, caption = cap) +
    theme(plot.background = element_rect(fill = bg.col, colour = bg.col),
          panel.grid = element_line(colour = grid.col, size = 1),
          axis.text = element_text(colour = text.col),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
          axis.title.y = element_text(angle=0,vjust = 0.5),
          axis.title = element_text(colour = text.col),
          legend.background = element_blank(),
          legend.key = element_blank(),
          legend.position = "none",
          plot.title = element_text(colour = text.col, size = main.size,hjust=.5),
          plot.caption = element_text(colour = text.col, size = 6, face = 'bold'),
          legend.title = element_text(colour = text.col),
          legend.text = element_text(colour = text.col, size = 12, face = "bold"),
          plot.margin = margin(1.5, 2, 1.5, 1.5, "cm"),
          panel.background = element_rect(fill = bg.col, colour = bg.col)) +
    geom_label_repel(aes(card, {{y_col}},  label = scales::percent({{y_col}})),
                     data = df, 
                     fontface = "bold",
                     box.padding = unit(0.25, "lines"),
                     point.padding = unit(0.25, "lines"),
                     label.size = 0.005,
                     nudge_y = 0.03,
                     #nudge_x = 1,
                     colour = "#ff184c",
                     fill = "black",
                     show.legend = FALSE)
  
  p
  
  return(p)
  
}
cujos_extra_cyber_bars <- function(df, 
                             y_col,
                             y_format = NULL,
                             palette = 1, 
                             area = TRUE, 
                             main = NULL, 
                             main.size = 20, 
                             cap = "Powered By MTGBAN.com", 
                             xlab = NULL, 
                             ylab = NULL 
){
  #df = logic
  bg.col = "#212946"
  grid.col = "#242d4d"
  text.col = "#05d9e8" 
  lwd = 1.75
  xlim = NULL
  ylim = NULL
  df = df %>% head(10) 
  # https://blog.depositphotos.com/15-cyberpunk-color-palettes-for-dystopian-designs.html
  
  if(palette == 1) {
    col <- colorRampPalette(c("#00ff9f", "#00b8ff", "#001eff", "#bd00ff", "#d600ff",
                              "#ff124f", "#ff00a0", "#fe75fe", "#7a04eb", "#ff6e27"))(nrow(df))
  }
  
  p <- ggplot(df, aes(x = reorder(card,({{y_col}})), y = {{y_col}}, label = round({{y_col}},0) ))
  
  if(area == FALSE) {
    p <- p + geom_bar(stat = "identity", fill = NA, color = col, size = 2)
  } else {
    p <- p + geom_bar(stat = "identity", fill = col)
  }
  
  if(is.null(y_format)){
    p = p + scale_y_continuous(limits = ylim)
    
    p <- p + labs(x = xlab, y = ylab, title = main, caption = cap) +
      theme(plot.background = element_rect(fill = bg.col, colour = bg.col),
            panel.grid = element_line(colour = grid.col, size = 1),
            axis.text = element_text(colour = text.col),
            axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
            axis.title.y = element_text(angle=0,vjust = 0.5),
            axis.title = element_text(colour = text.col),
            legend.background = element_blank(),
            legend.key = element_blank(),
            legend.position = "none",
            plot.title = element_text(colour = text.col, size = main.size,hjust=.5),
            plot.caption = element_text(colour = text.col, size = 6, face = 'bold'),
            legend.title = element_text(colour = text.col),
            legend.text = element_text(colour = text.col, size = 12, face = "bold"),
            plot.margin = margin(1.5, 2, 1.5, 1.5, "cm"),
            panel.background = element_rect(fill = bg.col, colour = bg.col)) +
      geom_label_repel(aes(card, {{y_col}},  label = {{y_col}}),
                       data = df, 
                       fontface = "bold",
                       box.padding = unit(0.25, "lines"),
                       point.padding = unit(0.25, "lines"),
                       label.size = 0.005,
                       nudge_y = 0.03,
                       #nudge_x = 1,
                       colour = "#ff184c",
                       fill = "black",
                       show.legend = FALSE)
    
  }else if(tolower(y_format) == "dollars"){
    p = p +
      scale_y_continuous(labels=scales::dollar_format(),limits = ylim)
    
    p <- p + labs(x = xlab, y = ylab, title = main, caption = cap) +
      theme(plot.background = element_rect(fill = bg.col, colour = bg.col),
            panel.grid = element_line(colour = grid.col, size = 1),
            axis.text = element_text(colour = text.col),
            axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
            axis.title.y = element_text(angle=0,vjust = 0.5),
            axis.title = element_text(colour = text.col),
            legend.background = element_blank(),
            legend.key = element_blank(),
            legend.position = "none",
            plot.title = element_text(colour = text.col, size = main.size,hjust=.5),
            plot.caption = element_text(colour = text.col, size = 6, face = 'bold'),
            legend.title = element_text(colour = text.col),
            legend.text = element_text(colour = text.col, size = 12, face = "bold"),
            plot.margin = margin(1.5, 2, 1.5, 1.5, "cm"),
            panel.background = element_rect(fill = bg.col, colour = bg.col)) +
      geom_label_repel(aes(card, {{y_col}},  label = scales::dollar_format({{y_col}})),
                       data = df, 
                       fontface = "bold",
                       box.padding = unit(0.25, "lines"),
                       point.padding = unit(0.25, "lines"),
                       label.size = 0.005,
                       nudge_y = 0.03,
                       #nudge_x = 1,
                       colour = "#ff184c",
                       fill = "black",
                       show.legend = FALSE)
  }else if(tolower(y_format)=="percent"){
    p = p +
      scale_y_continuous(labels=scales:::percent_format(),limits = ylim)
    
    p <- p + labs(x = xlab, y = ylab, title = main, caption = cap) +
      theme(plot.background = element_rect(fill = bg.col, colour = bg.col),
            panel.grid = element_line(colour = grid.col, size = 1),
            axis.text = element_text(colour = text.col),
            axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
            axis.title.y = element_text(angle=0,vjust = 0.5),
            axis.title = element_text(colour = text.col),
            legend.background = element_blank(),
            legend.key = element_blank(),
            legend.position = "none",
            plot.title = element_text(colour = text.col, size = main.size,hjust=.5),
            plot.caption = element_text(colour = text.col, size = 6, face = 'bold'),
            legend.title = element_text(colour = text.col),
            legend.text = element_text(colour = text.col, size = 12, face = "bold"),
            plot.margin = margin(1.5, 2, 1.5, 1.5, "cm"),
            panel.background = element_rect(fill = bg.col, colour = bg.col)) +
      geom_label_repel(aes(card, {{y_col}},  label = scales::percent_format({{y_col}})),
                       data = df, 
                       fontface = "bold",
                       box.padding = unit(0.25, "lines"),
                       point.padding = unit(0.25, "lines"),
                       label.size = 0.005,
                       nudge_y = 0.03,
                       #nudge_x = 1,
                       colour = "#ff184c",
                       fill = "black",
                       show.legend = FALSE)
  }
  
  
  return(p)
  
}
cujos_secret_cyber_bars <- function(df, 
                                   y_col,
                                   y_format = NULL,
                                   palette = 1, 
                                   area = TRUE, 
                                   main = NULL, 
                                   main.size = 20, 
                                   cap = "Powered By MTGBAN.com", 
                                   xlab = NULL, 
                                   ylab = NULL 
){
  #df = logic
  bg.col = "#212946"
  grid.col = "#242d4d"
  text.col = "#05d9e8" 
  lwd = 1.75
  xlim = NULL
  ylim = NULL
  #df = logic %>% head(20) 
  df = df %>% head(20)
  if(!is.na(df$condition)){
    df = df %>% select(-condition) %>% group_by(product_name,current_supply,current_months_sales,most_recent_sale,qty_last_4_months,average_sales_price,monthly_sell_through,quarterly_sell_through,daily_sell_through_current_month,daily_sell_through_current_quarter) %>% summarize(tcg_low = min(tcg_low)) %>% ungroup() 
  }
  # https://blog.depositphotos.com/15-cyberpunk-color-palettes-for-dystopian-designs.html
  
  if(palette == 1) {
    col <- colorRampPalette(c("#00ff9f", "#00b8ff", "#001eff", "#bd00ff", "#d600ff",
                              "#ff124f", "#ff00a0", "#fe75fe", "#7a04eb", "#ff6e27"))(nrow(df))
  }
  
  p <- ggplot(df, aes(x = reorder(product_name,desc({{y_col}})), y = {{y_col}}, label = round({{y_col}},0) ))
  
  if(area == FALSE) {
    p <- p + geom_bar(stat = "identity", fill = NA, color = col, size = 2)
  } else {
    p <- p + geom_bar(stat = "identity", fill = col)
  }
  
  if(is.null(y_format)){
    p = p + scale_y_continuous(limits = ylim)
    
    p <- p + labs(x = xlab, y = ylab, title = main, caption = cap) +
      theme(plot.background = element_rect(fill = bg.col, colour = bg.col),
            panel.grid = element_line(colour = grid.col, size = 1),
            axis.text = element_text(colour = text.col),
            axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
            axis.title.y = element_text(angle=0,vjust = 0.5),
            axis.title = element_text(colour = text.col),
            legend.background = element_blank(),
            legend.key = element_blank(),
            legend.position = "none",
            plot.title = element_text(colour = text.col, size = main.size,hjust=.5),
            plot.caption = element_text(colour = text.col, size = 6, face = 'bold'),
            legend.title = element_text(colour = text.col),
            legend.text = element_text(colour = text.col, size = 12, face = "bold"),
            plot.margin = margin(1.5, 2, 1.5, 1.5, "cm"),
            panel.background = element_rect(fill = bg.col, colour = bg.col)) +
      geom_label_repel(aes(product_name, {{y_col}},  label = ({{y_col}})),
                       data = df, 
                       fontface = "bold",
                       box.padding = unit(0.25, "lines"),
                       point.padding = unit(0.25, "lines"),
                       label.size = 0.005,
                       nudge_y = 0.03,
                       #nudge_x = 1,
                       colour = "#ff184c",
                       fill = "black",
                       show.legend = FALSE)
    
  }else if(tolower(y_format) == "dollars"){
    p = p +
      scale_y_continuous(labels=scales::dollar_format(),limits = ylim)
    
    p <- p + labs(x = xlab, y = ylab, title = main, caption = cap) +
      theme(plot.background = element_rect(fill = bg.col, colour = bg.col),
            panel.grid = element_line(colour = grid.col, size = 1),
            axis.text = element_text(colour = text.col),
            axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
            axis.title.y = element_text(angle=0,vjust = 0.5),
            axis.title = element_text(colour = text.col),
            legend.background = element_blank(),
            legend.key = element_blank(),
            legend.position = "none",
            plot.title = element_text(colour = text.col, size = main.size,hjust=.5),
            plot.caption = element_text(colour = text.col, size = 6, face = 'bold'),
            legend.title = element_text(colour = text.col),
            legend.text = element_text(colour = text.col, size = 12, face = "bold"),
            plot.margin = margin(1.5, 2, 1.5, 1.5, "cm"),
            panel.background = element_rect(fill = bg.col, colour = bg.col)) +
      geom_label_repel(aes(product_name, {{y_col}},  label = scales::dollar({{y_col}})),
                       data = df, 
                       fontface = "bold",
                       box.padding = unit(0.25, "lines"),
                       point.padding = unit(0.25, "lines"),
                       label.size = 0.005,
                       nudge_y = 0.03,
                       #nudge_x = 1,
                       colour = "#ff184c",
                       fill = "black",
                       show.legend = FALSE)
  }else if(tolower(y_format)=="percent"){
    p = p +
      scale_y_continuous(labels=scales:::percent_format(),limits = ylim)
    
    p <- p + labs(x = xlab, y = ylab, title = main, caption = cap) +
      theme(plot.background = element_rect(fill = bg.col, colour = bg.col),
            panel.grid = element_line(colour = grid.col, size = 1),
            axis.text = element_text(colour = text.col),
            axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
            axis.title.y = element_text(angle=0,vjust = 0.5),
            axis.title = element_text(colour = text.col),
            legend.background = element_blank(),
            legend.key = element_blank(),
            legend.position = "none",
            plot.title = element_text(colour = text.col, size = main.size,hjust=.5),
            plot.caption = element_text(colour = text.col, size = 6, face = 'bold'),
            legend.title = element_text(colour = text.col),
            legend.text = element_text(colour = text.col, size = 12, face = "bold"),
            plot.margin = margin(1.5, 2, 1.5, 1.5, "cm"),
            panel.background = element_rect(fill = bg.col, colour = bg.col)) +
      geom_label_repel(aes(product_name, {{y_col}},  label = scales::percent({{y_col}})),
                       data = df, 
                       fontface = "bold",
                       box.padding = unit(0.25, "lines"),
                       point.padding = unit(0.25, "lines"),
                       label.size = 0.005,
                       nudge_y = 0.03,
                       #nudge_x = 1,
                       colour = "#ff184c",
                       fill = "black",
                       show.legend = FALSE)
  }
  
  
  return(p)
  
}

#Guess what? My data base is named Gaea's Cradle! I have so much imaginative prowess. Let's connect to it with this function.
gaeas_cradle <- function(email){
  con <- dbConnect(
    bigrquery::bigquery(),
    project = my_secrets$project,
    dataset = my_secrets$dataset,
    billing = my_secrets$billing
  )
  bq_auth(email = email, use_oob = TRUE)
  options(scipen = 20)
  con
} 
# Incredibly important function. It is the powerhouse of the cell. This basic math will separate me, from my better & more comprehensive data (thank you Koda), to this simple filtering to provide better insights.
# To be fair, this should likely be dyanmic to the data source being looked at to opt between median and mean, but, for now, it opts to highly skewed data, bc mtg is really skewed data.
detect_outliers <- function(x) {
  
  if (missing(x)) stop("The argument x needs a vector.")
  
  if (!is.numeric(x)) stop("The argument x must be numeric.")
  
  data_tbl <- tibble(data = x)
  
  limits_tbl <- data_tbl %>%
    summarise(
      quantile_lo = quantile(data, probs = 0.25, na.rm = TRUE),
      quantile_hi = quantile(data, probs = 0.75, na.rm = TRUE),
      iqr         = IQR(data, na.rm = TRUE),
      limit_lo    = quantile_lo - 1.5 * iqr,
      limit_hi    = quantile_hi + 1.5 * iqr
    )
  
  output_tbl <- data_tbl %>%
    mutate(outlier = case_when(
      data < limits_tbl$limit_lo ~ TRUE,
      data > limits_tbl$limit_hi ~ TRUE,
      TRUE ~ FALSE
    ))
  
  return(as.numeric(output_tbl$outlier))
  
}

# Whether we're looking at NA or JPN data, let's prep the data from teh BAN api in the same fashion for consistency
bl_pre_preparations = function(ban_bl_tbl){
  ban_bl_tbl %>% 
    select(-id) %>%
    distinct() %>%
    mutate(tcg_id = as.numeric(tcg_id)) %>%
    arrange(tcg_id,hasFoil,Date,card,set,rarity,number) %>%
    group_by(tcg_id,hasFoil,card,set,rarity,number,vendor) %>%
    summarize(
      Date = Date,
      offer = offer,
      l_offer = lag(offer,1,order_by = Date),
      avg_offer = mean(offer,na.rm=T),
      change = offer - l_offer,
      change_sum = sum(change,na.rm=T),
      boolean_offer = ifelse(offer > l_offer,1, ifelse(offer == l_offer,0,-1)),
      boolean_score = sum(boolean_offer,na.rm=T),
      ct = round(n(),0)
    ) %>% 
    ungroup() %>%
    drop_na() %>%
    distinct()
}
# Likewise for markets, lets's be consistent. The main benefit of this approach is allows me to have very few
# visualizations, which I just love.
mkt_pre_preparations = function(ban_retail_tbl){
  ban_retail_tbl %>%
    select(-id) %>%
    distinct() %>%
    mutate(tcg_id = as.numeric(tcg_id)) %>%
    mutate(mkt_value_outlier_flag = as.numeric(detect_outliers(mkt_value)),
           mkt_value_outliers = sum(mkt_value_outlier_flag),
           mkt_value_cleansed = ifelse(mkt_value_outlier_flag == 1,NA,mkt_value),
           mkt_value_cleansed = ifelse(is.na(mkt_value_cleansed),round(mean(mkt_value,na.rm=T),0),mkt_value )) %>%
    filter() %>%
    arrange(tcg_id,hasFoil,Date,card,set,rarity,number) %>%
    group_by(tcg_id,hasFoil,card,set,rarity,number,vendor) %>%
    summarize(
      Date = Date,
      offer = mkt_value_cleansed,
      l_offer = lag(mkt_value_cleansed,1,order_by = Date),
      avg_offer = mean(mkt_value_cleansed,na.rm=T),
      change = offer - l_offer,
      change_sum = sum(change,na.rm=T),
      boolean_offer = ifelse(offer > l_offer,1, ifelse(offer == l_offer,0,-1)),
      boolean_absolute = sum(abs(boolean_offer),na.rm=T),
      boolean_score = sum(boolean_offer,na.rm=T),
      ct = round(n(),0)
    ) %>% 
    ungroup() %>%
    drop_na() %>%
    distinct()
}
#Likewise for all data in regards to copies sold, for singles and sealed, we'll format everything so my post_logic function knows what it is grabbing every time.
basket_copies_pre_preparations = function(mtg_basket_all_tbl){
  mtg_basket_all_tbl %>% 
    mutate(number = as.character(number)) %>%
    filter(language == "English") %>%
    group_by(date,tcg_id,hasFoil,card,set,rarity,number) %>%
    summarize(sold_quantity = sum(sold_quantity),
              sell_price = round(mean(sell_price,na.rm=T),1)) %>%
    ungroup() %>%
    mutate(sold_quant_outlier_flag = as.numeric(detect_outliers(sold_quantity)),
           sold_quant_outliers = sum(sold_quant_outlier_flag),
           sold_quantity_cleansed = ifelse(sold_quant_outlier_flag == 1,NA,sold_quantity),
           sold_quantity_cleansed = ifelse(is.na(sold_quantity_cleansed),round(mean(sold_quantity,na.rm=T),0),sold_quantity )) %>%
    distinct() %>%
    arrange(tcg_id,hasFoil,date,card,set,rarity,number) %>%
    group_by(tcg_id,hasFoil,card,set,rarity,number) %>%
    summarize(
      Date = date,
      offer = sold_quantity_cleansed,
      avg_offer = mean(sold_quantity_cleansed),
      sell_price,
      avg_sell_price = mean(sell_price),
      l_offer = lag(sold_quantity_cleansed,1,order_by = Date),
      change = offer - l_offer,
      change_sum = sum(change,na.rm=T),
      boolean_offer = ifelse(offer > l_offer,1, ifelse(offer == l_offer,0,-1)),
      boolean_score = sum(boolean_offer,na.rm=T),
      ct = round(n(),0),
      outliers_detected = sum(sold_quant_outlier_flag)
    ) %>% 
    ungroup() %>%
    distinct() 
}
basket_sale_pre_preparations = function(mtg_basket_all_tbl){
  mtg_basket_all_tbl %>% 
    mutate(number = as.character(number)) %>%
    filter(language == "English") %>%
    group_by(date,tcg_id,hasFoil,card,set,rarity,number) %>%
    summarize(sold_quantity = sum(sold_quantity),
              sell_price = round(mean(sell_price,na.rm=T),1)) %>%
    ungroup() %>%
    mutate(sell_price_outlier_flag = as.numeric(detect_outliers(sell_price)),
           sell_price_outliers = sum(sell_price_outlier_flag),
           sell_price_cleansed = ifelse(sell_price_outlier_flag == 1,NA,sell_price),
           sell_price_cleansed = ifelse(is.na(sell_price_cleansed),round(mean(sell_price,na.rm=T),0),sell_price )) %>%
    distinct() %>%
    arrange(tcg_id,hasFoil,date,card,set,rarity,number) %>%
    group_by(tcg_id,hasFoil,card,set,rarity,number) %>%
    summarize(
      Date = date,
      offer = sell_price_cleansed,
      avg_offer = mean(offer),
      l_offer = lag(sell_price_cleansed,1,order_by = Date),
      change = offer - l_offer,
      change_sum = sum(change,na.rm=T),
      boolean_offer = ifelse(offer > l_offer,1, ifelse(offer == l_offer,0,-1)),
      boolean_score = sum(boolean_offer,na.rm=T),
      ct = round(n(),0),
      outliers_detected = sum(sell_price_outlier_flag)
    ) %>% 
    ungroup() %>%
    distinct()
}
basket_sealed_copies_pre_preparations = function(mtg_basket_all_tbl){
  mtg_basket_all_tbl %>% 
    mutate(number = as.character(number)) %>%
    filter(rarity == "S") %>%
    group_by(date,tcg_id,hasFoil,card,set,rarity,number) %>%
    summarize(sold_quantity = sum(sold_quantity),
              sell_price = round(mean(sell_price,na.rm=T),1)) %>%
    ungroup() %>%
    mutate(sold_quant_outlier_flag = as.numeric(detect_outliers(sold_quantity)),
           sold_quant_outliers = sum(sold_quant_outlier_flag),
           sold_quantity_cleansed = ifelse(sold_quant_outlier_flag == 1,NA,sold_quantity),
           sold_quantity_cleansed = ifelse(is.na(sold_quantity_cleansed),round(mean(sold_quantity,na.rm=T),0),sold_quantity )) %>%
    distinct() %>%
    arrange(tcg_id,hasFoil,date,card,set,rarity,number) %>%
    group_by(tcg_id,hasFoil,card,set,rarity,number) %>%
    summarize(
      Date = date,
      offer = sold_quantity_cleansed,
      avg_offer = mean(offer),
      l_offer = lag(sold_quantity_cleansed,1,order_by = Date),
      change = offer - l_offer,
      change_sum = sum(change,na.rm=T),
      boolean_offer = ifelse(offer > l_offer,1, ifelse(offer == l_offer,0,-1)),
      boolean_score = sum(boolean_offer,na.rm=T),
      ct = round(n(),0),
      outliers_detected = sum(sold_quant_outlier_flag)
    ) %>% 
    ungroup() %>%
    distinct() 
}
basket_sealed_sale_pre_preparations = function(mtg_basket_all_tbl){
  mtg_basket_all_tbl %>% 
    mutate(number = as.character(number)) %>%
    filter(rarity == "S") %>%
    group_by(date,tcg_id,hasFoil,card,set,rarity,number) %>%
    summarize(sold_quantity = sum(sold_quantity),
              sell_price = round(mean(sell_price,na.rm=T),1)) %>%
    ungroup() %>%
    filter(sell_price >= 40) %>%
    mutate(sell_price_outlier_flag = as.numeric(detect_outliers(sell_price)),
           sell_price_outliers = sum(sell_price_outlier_flag),
           sell_price_cleansed = ifelse(sell_price_outlier_flag == 1,NA,sell_price),
           sell_price_cleansed = ifelse(is.na(sell_price_cleansed),round(mean(sell_price,na.rm=T),0),sell_price )) %>%
    distinct() %>%
    arrange(tcg_id,hasFoil,date,card,set,rarity,number) %>%
    group_by(tcg_id,hasFoil,card,set,rarity,number) %>%
    summarize(
      Date = date,
      offer = sell_price_cleansed,
      avg_offer = mean(offer),
      l_offer = lag(sell_price_cleansed,1,order_by = Date),
      change = offer - l_offer,
      change_sum = sum(change,na.rm=T),
      boolean_offer = ifelse(offer > l_offer,1, ifelse(offer == l_offer,0,-1)),
      boolean_score = sum(boolean_offer,na.rm=T),
      ct = round(n(),0),
      outliers_detected = sum(sell_price_outlier_flag)
    ) %>% 
    ungroup() %>%
    distinct()
}

#I've had this logic in my scripts for 3+ years, and just felt it was such an easy to communicate finding that it deserved it's own day.
card_kingdom_buylist_review = function(){
  options(httr_oob_default=TRUE) 
  options(gargle_oauth_email = my_secrets$og_patches)
  drive_auth(email = my_secrets$og_patches,use_oob=TRUE)
  gs4_auth(email = my_secrets$og_patches,use_oob=TRUE)
  gc()
  #drive_create("TCG_Review")
  ss <- drive_get("Sets")
  
  Sets <- read_sheet(ss,"Sets") %>% mutate_if(is.character,as.factor)
  #View(Sets)
  ck_conversion <- read_sheet(ss,"mtgjson_ck_sets")
  
  Exclusion <- Sets %>% select(contains("Excl"))
  
  tryCatch({Updated_Tracking_Keys <- read_csv("/home/cujo253/mines_of_moria/Essential_Referential_CSVS/C20_Addition.csv", col_types = cols(hasFoil = col_character())) %>%
    #rename(c("scryfall_id" = "scryfall","tcg_ID"="param","card" = "name", "set" = "Set", "rarity" = "Rarity","hasFoil" = "Foil")) %>%
    rename(c("scryfall" = "scryfall_id","param"="tcg_ID","name" = "card", "Set" = "set", "Rarity" = "rarity","Foil" = "hasFoil")) %>%
    mutate(Semi = paste(name, Set,sep=""))},error = function(e){Updated_Tracking_Keys <- read_csv("/home/cujo253/mines_of_moria/Essential_Referential_CSVS/C20_Addition.csv", col_types = cols(hasFoil = col_character())) %>%
      rename(c("scryfall_id" = "scryfall","tcg_ID"="param","card" = "name", "set" = "Set", "rarity" = "Rarity","hasFoil" = "Foil")) %>%
      #rename(c("scryfall" = "scryfall_id","param"="tcg_ID","name" = "card", "Set" = "set", "Rarity" = "rarity","Foil" = "hasFoil")) %>%
      mutate(Semi = paste(name, Set,sep=""))})
  
  Updated_Tracking_Keys = Updated_Tracking_Keys %>% replace_na(list(Foil = "")) %>%mutate(name = gsub("\\s\\/\\/.*","",name),
                                                                                          Key = trimws(paste(name,Set,Rarity," ",Foil,sep="")),
                                                                                          Semi = paste(name,Set,sep=""))
  
  CK_Buylist = fromJSON("https://api.cardkingdom.com/api/v2/pricelist") %>% as.data.frame() %>% 
    mutate(data.edition = as.factor(data.edition), 
           data.price_buy = as.numeric(as.character(data.price_buy)),
           data.price_retail = as.numeric(as.character(data.price_retail)))
  
  No_Foils =  CK_Buylist                                     %>% 
    filter(data.variation == "" & data.is_foil == "false") %>% 
    group_by(data.edition)                                 %>% 
    count(data.edition)                                    %>%
    rename("edition"="data.edition","count"="n")
  
  
  Foils =  CK_Buylist                                        %>% 
    filter(data.variation == "" & data.is_foil == "true")  %>% 
    group_by(data.edition)                                 %>% 
    count(data.edition)                                    %>%
    rename("edition"="data.edition","count"="n")      
  
  NF_Total_Offers <- sum(No_Foils$count)
  F_Total_Offers <- sum(Foils$count)
  
  No_Foils = No_Foils %>% mutate(composition = (round(count/NF_Total_Offers,4)))
  Foils = Foils %>% mutate(composition = (round(count/F_Total_Offers,4)))
  
  
  # Jenny Craig Buy List ----------------------------------------------------
  
  Slim_CK_Buylist = CK_Buylist                                %>%
    mutate(data.number = as.numeric(gsub(".*-","",data.sku)) ) %>%
    select(meta.created_at     ,
           data.name           ,
           data.edition        ,
           data.is_foil        ,
           data.number         ,
           data.price_retail   ,
           data.qty_retail     ,
           data.price_buy      ,
           data.qty_buying)                                 %>%
    left_join(.,
              Sets %>% select(CK_BL_Scrape_Sets,mtgjson), 
              by = c("data.edition" = "CK_BL_Scrape_Sets")) %>%
    left_join(.,Exclusion, by = c("mtgjson" = "Set_Excl"))  %>% 
    #mutate(data.edition = mtgjson)                         %>% 
    replace_na(list(Excl_Excl = "Unclear"))                 %>%
    filter(Excl_Excl != "Exclude"   & 
             data.qty_buying != 0 &
             data.price_buy > 1.50)                       %>%
    select(-mtgjson, - Excl_Excl)                           %>%
    mutate(data.is_foil = ifelse(data.is_foil == "false",
                                 0,
                                 1),
           #
           data.qty_retail = ifelse(data.qty_retail == 0,
                                    1, 
                                    data.qty_retail),
           #
           QTY_Diff = round((data.qty_buying
                             -
                               data.qty_retail)
                            /
                              data.qty_buying,2),
           #
           Price_Diff = round(data.price_buy
                              /
                                data.price_retail,2),
           
           Tier_QTY_Diff = ntile(QTY_Diff,10),
           Tier_Price_Diff = ntile(Price_Diff,10),
           Tier_data.qty_buying = ntile(data.qty_buying,10))%>%
    mutate(
      Tier = round(rowMeans(
        (select(.,Tier_QTY_Diff,
                Tier_Price_Diff,
                Tier_data.qty_buying))),2))          %>%
    
    arrange(desc(Tier))                                     %>%
    select(-Tier_QTY_Diff,
           -Tier_Price_Diff,
           -Tier_data.qty_buying)                           %>%
    
    `row.names<-` (seq(nrow(.)))                            %>%
    mutate(meta.created_at = paste(data.name,
                                   data.edition,
                                   data.is_foil,
                                   sep="")) %>%
    distinct()
  
  Eternal_Growers = Slim_CK_Buylist                           %>% 
    filter(Tier >= 9.5 & data.qty_retail >= 8)              %>%
    left_join(.,
              Sets %>% select(CK_BL_Scrape_Sets,mtgjson), 
              by = c("data.edition" = "CK_BL_Scrape_Sets")) %>%
    left_join(.,Updated_Tracking_Keys      %>% 
                select(rdate,Set)        %>% 
                distinct(), 
              by = c("mtgjson" = "Set"))                    %>% 
    mutate(data.edition = mtgjson)                          %>% 
    distinct()                                              %>%
    filter(is.na(rdate)==F                                   & 
             rdate <= (Sys.Date() %m-% months(13))) %>%
    arrange(desc(Tier),desc(Price_Diff)) %>%
    mutate(Tier = seq(nrow(.)))
  
  return(Eternal_Growers)
  
}

# Thank you hidden agenda, mod over at mtgban.com, for allowing me to utilize his google sheet to provide content for half the month XD
secret_lair_data = function(){
  options(httr_oob_default=TRUE) 
  options(gargle_oauth_email = my_secrets$og_patches)
  drive_auth(email = my_secrets$og_patches,use_oob=TRUE)
  gs4_auth(email = my_secrets$og_patches,use_oob=TRUE)
  gc()
  #drive_create("TCG_Review")
  ss <- drive_get("TCGPlayer Pricing Sheet 2020")
  
  sl_sealed_data <- read_sheet(ss,"Sealed") 
  
  sl_singles_data = read_sheet(ss,"Singles")
  
  return(list(sl_sealed_data,sl_singles_data))
}

# If you want to dissect my work, this is the place, the database_pull function is my using object oriented programming to establish content off the above functions
# for every day of the month. Mostly, it pulls on my personal database, but occasionally other resources as well to create content.
database_pull = function(){
  # This entire thing ticks off day_in_month, it decides the data base pulls as well as the content to send twitters ways.
  day_in_month = day(Sys.Date())
  #day_in_month = 1
  post_logic = function(data){
    
    day_in_month = day(Sys.Date())
    #day_in_month = 1
    
    #Buy List Analysis Dates 1:8
    
    #test on my end to ensure functionality
    #data = bl_review_tbl
    if(day_in_month == 1){
      #Create a cutoff to ensure that we stick with data that has represented itself a great deal in the prior month
      cutoff = data %>% select(ct) %>% summarize(cutoff = max(ct)-1)
      #Filter for the worst change
      logic = data %>% filter(ct >= cutoff$cutoff) %>% filter(change_sum == min(change_sum))
      #Name our logic and also create our chart title
      logic_chosen = "Worst US Buylist Performance By Value"
      #Provide key details for graph axis and formatting. It was really quite the hassle to account for dollars, percents, and integers all at once, let me tell you...
      media_component = logic %>% cujos_cyber_lines(.,offer,y_format="dollars",ylab="CK Offer",main=logic_chosen)
      
      if(unique(logic$hasFoil)==1){
        nf_f = "(F)"
      }else{
        nf_f = "(NF)"
      }
      
      #Ensure that if my desired message is too large, for some reason, the content will post without it, to ensure something is generate.
      # Likely will alter this to ensure it retain the hashtags for BAN.
      tweet_content = paste0(unique(logic$card),
                            " from ",
                            gsub("Cards","",unique(logic$set)),
                            "-",
                            nf_f,
                            " has been declining this month on Card Kingdoms Buy List, down ",
                            min(logic$change),
                            "(",
                            scales::percent(min(logic$change)/logic$offer[1]),
                            "). Spotlight on higher value item to gauge broader movement, 
                            may be an opportunity or heads up it's going down atm.",
                            "#mtgban #mtgfinance")
      
      if(nchar(tweet_content)> 280){
        tweet_content = ""
      }
      
      #Return all 4 elements for the day for ease of twitter post
      return(list(logic,logic_chosen,media_component,tweet_content))
    }
    if(day_in_month == 2){
      #Create a cutoff to ensure that we stick with data that has represented itself a great deal in the prior month
      cutoff = data %>% select(ct) %>% summarize(cutoff = max(ct)-1)
      #Filter for the best change
      logic = data %>% filter(ct >= cutoff$cutoff) %>% filter(change_sum == max(change_sum))
      #Name our logic and also create our chart title
      logic_chosen = "Best US Buylist Performance By Value"
      #Provide key details for graph axis and formatting. It was really quite the hassle to account for dollars, percents, and integers all at once, let me tell you..
      media_component = logic %>% cujos_cyber_lines(.,offer,y_format="dollars",ylab="CK Offer",main=logic_chosen)
      
      #Ensure that if my desired message is too large, for some reason, the content will post without it, to ensure something is generate.
      # Likely will alter this to ensure it retain the hashtags for BAN.
      tweet_content = paste0(unique(logic$card),
                             " from ",
                             gsub("Cards","",unique(logic$set)),
                             "-",
                             nf_f,
                             " has been increasing this month on Card Kingdoms Buy List, up ",
                             max(logic$change),
                             "(",
                             scales::percent(max(logic$change)/logic$offer[1]),
                             "). Spotlight on higher value item to gauge broader movement, 
                            may be an opportunity for arb or time to buy.",
                             "#mtgban #mtgfinance")
      
      if(nchar(tweet_content)> 280){
        tweet_content = ""
      }
      
      return(list(logic,logic_chosen,media_component,tweet_content))
    }
    if(day_in_month == 3){
      #Create a cutoff to ensure that we stick with data that has represented itself a great deal in the prior month
      cutoff = data %>% select(ct) %>% summarize(cutoff = max(ct)-1)
      #Filter for the worst change
      logic = data %>% filter(ct >= cutoff$cutoff) %>% filter(avg_offer > 20) %>% filter(boolean_score == min(boolean_score))
      #Name our logic and also create our chart title
      logic_chosen = "Worst US Buylist Performance By Rate of Decline"
      #Provide key details for graph axis and formatting. It was really quite the hassle to account for dollars, percents, and integers all at once, let me tell you..
      media_component = logic %>% cujos_cyber_lines(.,offer,y_format="dollars",ylab="CK Offer",main=logic_chosen)
      
      if(unique(logic$hasFoil)==1){
        nf_f = "(F)"
      }else{
        nf_f = "(NF)"
      }
      
      #Ensure that if my desired message is too large, for some reason, the content will post without it, to ensure something is generate.
      # Likely will alter this to ensure it retain the hashtags for BAN.
      tweet_content = paste0(unique(logic$card),
                             " from ",
                             gsub("Cards","",unique(logic$set)),
                             "-",
                             nf_f,
                             " has been declining this past month on Card Kingdoms Buy List, down ",
                             scales::dollar(min(logic$change_sum)),
                             "(",
                             scales::percent(max(logic$change_sum)/logic$offer[1]),
                             "). Spotlight on rate of change, rapid change often ties with related items. Usually a reprint, or some other cause..?",
                             "#mtgban #mtgfinance")
      
      if(nchar(tweet_content)> 280){
        tweet_content = ""
      }
      
      return(list(logic,logic_chosen,media_component,tweet_content))
    }
    if(day_in_month == 4){
      #Create a cutoff to ensure that we stick with data that has represented itself a great deal in the prior month
      cutoff = data %>% select(ct) %>% summarize(cutoff = max(ct)-1)
      logic = data %>% filter(ct >= cutoff$cutoff)  %>% filter(avg_offer > 15) %>% filter(boolean_score == max(boolean_score))
      logic_chosen = "Best US Buylist Performance By Rate of Increase"
      media_component = logic %>% cujos_cyber_lines(.,offer,y_format="dollars",ylab="CK Offer",main=logic_chosen)
      
      if(unique(logic$hasFoil)==1){
        nf_f = "(F)"
      }else{
        nf_f = "(NF)"
      }
      
      
      tweet_content = paste0(unique(logic$card),
                             " from ",
                             gsub("Cards","",unique(logic$set)),
                             "-",
                             nf_f,
                             " has been increasing this past month on Card Kingdoms Buy List, up ",
                             scales::dollar(max(logic$change_sum)),
                             "(",
                             scales::percent(max(logic$change_sum)/logic$offer[1]),
                             "). Spotlight on rate of change, a rapid increase tends to show increased demand, but limited/throttled supply.",
                             "#mtgban #mtgfinance")
      
      if(nchar(tweet_content)> 280){
        tweet_content = ""
      }
      
      return(list(logic,logic_chosen,media_component,tweet_content))
    }
    
    #test on my end to ensure functionality
    #data = jpn_bl_review_tbl
    if(day_in_month == 5){
      #Create a cutoff to ensure that we stick with data that has represented itself a great deal in the prior month
      cutoff = data %>% select(ct) %>% summarize(cutoff = max(ct)-1)
      logic = data %>% filter(ct >= cutoff$cutoff) %>% filter(change_sum == min(change_sum))
      logic_chosen = "Worst JPN Buylist Performance By Value"
      media_component = logic %>% cujos_cyber_lines(.,offer,y_format="dollars",ylab="JPN BL Offer",main=logic_chosen)
      
      if(unique(logic$hasFoil)==1){
        nf_f = "(F)"
      }else{
        nf_f = "(NF)"
      }
      
      
      tweet_content = paste0(unique(logic$card),
                             " from ",
                             gsub("Cards","",unique(logic$set)),
                             "-",
                             nf_f,
                             " has declined this past month on Hareruya's BL, down ",
                             scales::dollar(min(logic$change_sum)),
                             "(",
                             scales::percent(min(logic$change_sum)/logic$offer[1]),
                             ").Demand from around the globe causes copies to ebb and flow! 
                             JPN is usually slower to move, but when they do...",
                             "#mtgban #mtgfinance")
      
      if(nchar(tweet_content)> 280){
        tweet_content = ""
      }
      
      return(list(logic,logic_chosen,media_component,tweet_content))
      
      return(list(logic,logic_chosen,media_component))
    }
    if(day_in_month == 6){
      #Create a cutoff to ensure that we stick with data that has represented itself a great deal in the prior month
      cutoff = data %>% select(ct) %>% summarize(cutoff = max(ct)-1)
      logic = data %>% filter(ct >= cutoff$cutoff) %>% filter(change_sum == max(change_sum))
      logic_chosen = "Best JPN Buylist Performance by Value"
      media_component = logic %>% cujos_cyber_lines(.,offer,ylab="JPN BL Offer",main=logic_chosen)
      
      if(unique(logic$hasFoil)==1){
        nf_f = "(F)"
      }else{
        nf_f = "(NF)"
      }
      
      
      tweet_content = paste0(unique(logic$card),
                             " from ",
                             gsub("Cards","",unique(logic$set)),
                             "-",
                             nf_f,
                             " has been increased this past month on Hareruya's BL, up ",
                             scales::dollar(min(logic$change_sum)),
                             "(",
                             scales::percent(max(logic$change_sum)/logic$offer[1]),
                             ").Always worth comparing these items to 'anomalies' in the NA Market. 
                             Just saying, can explain a lot.",
                             "#mtgban #mtgfinance")
      
      if(nchar(tweet_content)> 280){
        tweet_content = ""
      }
      
      return(list(logic,logic_chosen,media_component,tweet_content))
    }
    if(day_in_month == 7){
      #Create a cutoff to ensure that we stick with data that has represented itself a great deal in the prior month
      cutoff = data %>% select(ct) %>% summarize(cutoff = max(ct)-1)
      logic = data %>% filter(ct >= cutoff$cutoff) %>% filter(avg_offer > 15) %>% filter(boolean_score == min(boolean_score))
      logic_chosen = "Worst JPN Buylist Performance By Rate of Decline"
      media_component = logic %>% cujos_cyber_lines(.,offer,y_format="dollars",ylab="JPN BL Offer",main=logic_chosen)
      
      if(unique(logic$hasFoil)==1){
        nf_f = "(F)"
      }else{
        nf_f = "(NF)"
      }
      
      
      tweet_content = paste0(unique(logic$card),
                             " from ",
                             gsub("Cards","",unique(logic$set)),
                             "-",
                             nf_f,
                             " is declining this past month on Hareruya's BL, down ",
                             scales::dollar(min(logic$change_sum)),
                             "(",
                             scales::percent(min(logic$change_sum)/logic$offer[1]),
                             ").JPN buylist tends to be a lot more calm, than NA, but if something is moving, always worth noting.",
                             "#mtgban #mtgfinance")
      
      if(nchar(tweet_content)> 280){
        tweet_content = ""
      }
      
      return(list(logic,logic_chosen,media_component,tweet_content))
      
    }
    if(day_in_month == 8){
      #Create a cutoff to ensure that we stick with data that has represented itself a great deal in the prior month
      cutoff = data %>% select(ct) %>% summarize(cutoff = max(ct)-1)
      logic = data %>% filter(ct >= cutoff$cutoff)  %>% filter(avg_offer > 15) %>% filter(boolean_score == max(boolean_score))
      logic_chosen = "Best JPN Buylist Performance By Rate of Increase"
      media_component = logic %>% cujos_cyber_lines(.,offer,y_format="dollars",ylab="JPN BL Offer",main=logic_chosen)
      
      if(unique(logic$hasFoil)==1){
        nf_f = "(F)"
      }else{
        nf_f = "(NF)"
      }
      
      
      tweet_content = paste0(unique(logic$card),
                             " from ",
                             gsub("Cards","",unique(logic$set)),
                             "-",
                             nf_f,
                             " is increasing this past month on Hareruya's BL, up ",
                             scales::dollar(max(logic$change_sum)),
                             "(",
                             scales::percent(max(logic$change_sum)/logic$offer[1]),
                             ").JPN buylist increasing can lead a lot of bigger sellers to ship overseas, not uncommon to move our market prices.",
                             "#mtgban #mtgfinance")
      
      if(nchar(tweet_content)> 280){
        tweet_content = ""
      }
      
      return(list(logic,logic_chosen,media_component,tweet_content))
    }
    #Basket Analysis Dates 9:17
    #test on my end to ensure functionality
    #data = basket_copies_sold_tbl
    if(day_in_month == 9){
      #Create a cutoff to ensure that we stick with data that has represented itself a great deal in the prior month
      cutoff = data %>% select(ct) %>% summarize(cutoff = max(ct)-1)
      logic = data %>% filter(ct >= cutoff$cutoff) %>% filter(rarity != 'S')  %>% filter(avg_sell_price >= 15) %>%filter(change_sum == min(change_sum))
      logic_chosen = "Worst Card By Actual Copies Sold"
      media_component = logic %>% cujos_cyber_lines(.,offer,ylab="Copies Sold",main=logic_chosen)
      
      if(unique(logic$hasFoil)==1){
        nf_f = "(F)"
      }else{
        nf_f = "(NF)"
      }
      
      
      tweet_content = paste0(unique(logic$card),
                             " from ",
                             gsub("Cards","",unique(logic$set)),
                             "-",
                             nf_f,
                             " sales rate declined the prior month, w/ mkt value starting at ",
                             scales::dollar((logic$sell_price[1])),
                             " ending at ",
                             scales::dollar(logic$sell_price[nrow(logic)]),
                             ".Market value tends to declines with low demand, but not always. In general, Caveat Emptor on these ones.The 'why' is crucial.",
                             "#mtgban #mtgfinance")
      
      if(nchar(tweet_content)> 280){
        tweet_content = ""
      }
      
      return(list(logic,logic_chosen,media_component,tweet_content))
      
    }
    if(day_in_month == 10){
      #Create a cutoff to ensure that we stick with data that has represented itself a great deal in the prior month
      cutoff = data %>% select(ct) %>% summarize(cutoff = max(ct)-1)
      logic = data %>% filter(ct >= cutoff$cutoff) %>% filter(rarity != 'S') %>% filter(avg_sell_price >= 15) %>% filter(change_sum == max(change_sum))
      logic_chosen = "Best Card Performance By Actual Copies Increase"
      media_component = logic %>% cujos_cyber_lines(.,offer,ylab="Copies Sold",main=logic_chosen)
      
      
      if(unique(logic$hasFoil)==1){
        nf_f = "(F)"
      }else{
        nf_f = "(NF)"
      }
      
      
      tweet_content = paste0(unique(logic$card),
                             " from ",
                             gsub("Cards","",unique(logic$set)),
                             "-",
                             nf_f,
                             " sales rate increased the prior month, w/ mkt value starting at ",
                             scales::dollar((logic$sell_price[1])),
                             " ending at ",
                             scales::dollar(logic$sell_price[nrow(logic)]),
                             ".Market value tends to increase with increasing demand. In my experience these are the cards folks 'don't notice' go up.",
                             "#mtgban #mtgfinance")
      
      if(nchar(tweet_content)> 280){
        tweet_content = ""
      }
      
      return(list(logic,logic_chosen,media_component,tweet_content))
    }
    if(day_in_month == 11){
      #Create a cutoff to ensure that we stick with data that has represented itself a great deal in the prior month
      cutoff = data %>% select(ct) %>% summarize(cutoff = max(ct)-1)
      logic = data %>% filter(ct >= cutoff$cutoff)  %>% filter(rarity != 'S') %>% filter(avg_sell_price > 5) %>% filter(boolean_score == min(boolean_score))
      if( length(unique(logic$card)) > 1 ){
        logic_id = logic %>% filter(offer == min(offer,na.rm=T)) %>% select(tcg_id) %>% unique()
        logic = logic %>% filter(tcg_id %in% logic_id$tcg_id)
      }
      logic_chosen = "Worst Card By Declining Sales Rate"
      media_component = logic %>% cujos_cyber_lines(.,offer,ylab="Copies Sold",main=logic_chosen)
      
      
      if(unique(logic$hasFoil)==1){
        nf_f = "(F)"
      }else{
        nf_f = "(NF)"
      }
      
      
      tweet_content = paste0(unique(logic$card),
                             " from ",
                             gsub("Cards","",unique(logic$set)),
                             "-",
                             nf_f,
                             " sales rate decreased the prior month, w/ mkt value starting at ",
                             scales::dollar((logic$sell_price[1])),
                             " ending at ",
                             scales::dollar(logic$sell_price[nrow(logic)]),
                             ".This card is experiencing a lot of gaps in demand, bringing into question what might be occuring here recently.",
                             "#mtgban #mtgfinance")
      
      if(nchar(tweet_content)> 280){
        tweet_content = ""
      }
      
      return(list(logic,logic_chosen,media_component,tweet_content))
    }
    if(day_in_month == 12){
      #Create a cutoff to ensure that we stick with data that has represented itself a great deal in the prior month
      cutoff = data %>% select(ct) %>% summarize(cutoff = max(ct)-1)
      logic = data %>% filter(ct >= cutoff$cutoff)  %>% filter(rarity != 'S')  %>% filter(avg_sell_price > 15) %>% filter(boolean_score == max(boolean_score))
      if( length(unique(logic$card)) > 1 ){
        logic_id = logic %>% filter(offer == max(offer,na.rm=T)) %>% select(tcg_id) %>% unique()
        logic = logic %>% filter(tcg_id %in% logic_id$tcg_id)
      }
      logic_chosen = "Best Card By Increasing Sales Rate"
      media_component = logic %>% cujos_cyber_lines(.,offer,ylab="Copies Sold",main=logic_chosen)
      
      if(unique(logic$hasFoil)==1){
        nf_f = "(F)"
      }else{
        nf_f = "(NF)"
      }
      
      
      tweet_content = paste0(unique(logic$card),
                             " from ",
                             gsub("Cards","",unique(logic$set)),
                             "-",
                             nf_f,
                             " sales rate increased the prior month, w/ mkt value starting at ",
                             scales::dollar((logic$sell_price[1])),
                             " ending at ",
                             scales::dollar(logic$sell_price[nrow(logic)]),
                             ".This card is experiencing a lot of increasing demand, interesting to watch how the overall value moves here.",
                             "#mtgban #mtgfinance")
      
      if(nchar(tweet_content)> 280){
        tweet_content = ""
      }
      
      return(list(logic,logic_chosen,media_component,tweet_content))
    }
    #DAY 13 NEEDS REWORK //// NO - We made it PODCAST DAY!
    if(day_in_month == 13){
      #Create a cutoff to ensure that we stick with data that has represented itself a great deal in the prior month
      logic_chosen = "Podcast Day!"
      
      rss_feed = tidyfeed(
        feed = "https://media.rss.com/bandwidth/feed.xml",
        config = list(),
        clean_tags = TRUE,
        list = FALSE,
        parse_dates = TRUE
      )
      
      latest_rss = rss_feed%>% filter(item_pub_date == max(item_pub_date))
      
      tweet_content = paste0("Check out the latest Podcast from ",
                             latest_rss$feed_title,
                             " where we discuss ",
                             latest_rss$item_title,
                             " over on https://open.spotify.com/show/755ha9x9YrO5zsBHdBmQjK?si=b1c0699d9f7747cc ",
                             "#mtgban #mtgfinance")
      
      if(nchar(tweet_content)> 280){
        tweet_content = ""
      }
      
      media_component = ""
      
      return(list(logic,logic_chosen,media_component,tweet_content))
    }
    #test on my end to ensure functionality
    #data = basket_sale_price_tbl
    if(day_in_month == 14){
      #Create a cutoff to ensure that we stick with data that has represented itself a great deal in the prior month
      cutoff = data %>% select(ct) %>% summarize(cutoff = max(ct)-1)
      logic = data %>% filter(ct >= cutoff$cutoff) %>% filter(!grepl("(Ice Age|Alpha|Beta|Arabian)",set)) %>% filter(change_sum == min(change_sum)) %>% filter(outliers_detected == max(outliers_detected))
      if( length(unique(logic$card)) > 1 ){
        logic_id = logic %>% filter(offer == min(offer,na.rm=T)) %>% select(tcg_id) %>% unique()
        logic = logic %>% filter(tcg_id %in% logic_id$tcg_id)
      }
      logic_chosen = "Voltile Card By Value Fluctuation"
      media_component = logic %>% cujos_cyber_lines(.,offer,y_format="dollars",ylab="Sold For",main=logic_chosen)
      
      if(unique(logic$hasFoil)==1){
        nf_f = "(F)"
      }else{
        nf_f = "(NF)"
      }
      
      tweet_content = paste0(unique(logic$card),
                             " from ",
                             gsub("Cards","",unique(logic$set)),
                             "-",
                             nf_f,
                             " sale price is fluctating, w/ a mkt value base of ",
                             scales::dollar(min(logic$offer,na.rm=T)),
                             " and up to ",
                             scales::dollar(max(logic$offer,na.rm=T)),
                             ".This card is experiencing a greater trend downwards, but it raises the question, Why is it fighting against greater value? ",
                             "#mtgban #mtgfinance")
      
      if(nchar(tweet_content)> 280){
        tweet_content = ""
      }
      
      return(list(logic,logic_chosen,media_component,tweet_content))
      
    }
    if(day_in_month == 15){
      #Create a cutoff to ensure that we stick with data that has represented itself a great deal in the prior month
      cutoff = data %>% select(ct) %>% summarize(cutoff = max(ct)-1)
      logic = data %>% filter(ct >= cutoff$cutoff) %>% filter(avg_offer > 5) %>%filter(!grepl("(Ice Age|Alpha|Beta|Arabian)",set))  %>% filter(change_sum == max(change_sum))
      if( length(unique(logic$card)) > 1 ){
        logic_id = logic %>% filter(offer == max(offer,na.rm=T)) %>% select(tcg_id) %>% unique()
        logic = logic %>% filter(tcg_id %in% logic_id$tcg_id)
      }
      logic_chosen = "Best Card Performance By Value"
      media_component = logic %>% cujos_cyber_lines(.,offer,y_format="dollars",ylab="Sold For",main=logic_chosen)
      
      if(unique(logic$hasFoil)==1){
        nf_f = "(F)"
      }else{
        nf_f = "(NF)"
      }
      
      tweet_content = paste0(unique(logic$card),
                             " from ",
                             gsub("Cards","",unique(logic$set)),
                             "-",
                             nf_f,
                             " value is fluctating, w/ a mkt base of ",
                             scales::dollar(min(logic$offer,na.rm=T)),
                             " and up to ",
                             scales::dollar(max(logic$offer,na.rm=T)),
                             ".This card trending upwards. Keep in mind, if the uptick is around $5, it may show shipping costs affecting price points, before others notice",
                             " #mtgban #mtgfinance")
      
      if(nchar(tweet_content)> 280){
        tweet_content = ""
      }
      
      return(list(logic,logic_chosen,media_component,tweet_content))
    }
    if(day_in_month == 16){
      #Create a cutoff to ensure that we stick with data that has represented itself a great deal in the prior month
      cutoff = data %>% select(ct) %>% summarize(cutoff = max(ct)-1)
      logic = data %>% filter(ct >= cutoff$cutoff) %>% filter(avg_offer > 5) %>% filter(boolean_score == min(boolean_score))
      if( length(unique(logic$card)) > 1 ){
        logic_id = logic %>% filter(offer == min(offer,na.rm=T)) %>% select(tcg_id) %>% unique()
        logic = logic %>% filter(tcg_id %in% logic_id$tcg_id)
      }
      logic_chosen = "Worst Card By Rate of Decline"
      media_component = logic %>% cujos_cyber_lines(.,offer,y_format="dollars",ylab="Sold For",main=logic_chosen)
      
      if(unique(logic$hasFoil)==1){
        nf_f = "(F)"
      }else{
        nf_f = "(NF)"
      }
      
      tweet_content = paste0(unique(logic$card),
                             " from ",
                             gsub("(\\s*Cards|\\s*Promos)","",unique(logic$set)),
                             "-",
                             nf_f,
                             " value is declining, starting the month w/ a mkt value of ",
                             scales::dollar(min(logic$offer,na.rm=T)),
                             " and down to ",
                             scales::dollar(max(logic$offer,na.rm=T)),
                             ".The buyer spotlight, or phase, may be moving away. This is creating either a warning sign or an opportunity",
                             " #mtgban #mtgfinance")
      
      if(nchar(tweet_content)> 280){
        tweet_content = ""
      }
      
      return(list(logic,logic_chosen,media_component,tweet_content))
    }
    if(day_in_month == 17){
      #Create a cutoff to ensure that we stick with data that has represented itself a great deal in the prior month
      cutoff = data %>% select(ct) %>% summarize(cutoff = max(ct)-1)
      logic = data %>% filter(ct >= cutoff$cutoff)  %>% filter(avg_offer > 5) %>% filter(boolean_score == max(boolean_score))
      if( length(unique(logic$card)) > 1 ){
        logic_id = logic %>% filter(offer == max(offer,na.rm=T)) %>% select(tcg_id) %>% unique()
        logic = logic %>% filter(tcg_id %in% logic_id$tcg_id)
      }
      logic_chosen = "Best Card By Rate of Increase"
      media_component = logic %>% cujos_cyber_lines(.,offer,y_format="dollars",ylab="Sold For",main=logic_chosen)
      
      if(unique(logic$hasFoil)==1){
        nf_f = "(F)"
      }else{
        nf_f = "(NF)"
      }
      
      tweet_content = paste0(unique(logic$card),
                             " from ",
                             gsub("(\\s*Cards|\\s*Promos)","",unique(logic$set)),
                             "-",
                             nf_f,
                             " value is increasing, starting the month w/ a mkt value of ",
                             scales::dollar(min(logic$offer,na.rm=T)),
                             " and up to ",
                             scales::dollar(max(logic$offer,na.rm=T)),
                             ".Buyers, for whatever reason, have focused on it. Will it last, and grow, or will this be just another phase?",
                             " #mtgban #mtgfinance")
      
      if(nchar(tweet_content)> 280){
        tweet_content = ""
      }
      
      return(list(logic,logic_chosen,media_component,tweet_content))
    }
    
    #Play Dates 18:21
    #test on my end to ensure functionality
    #data = decklist_data
    if(day_in_month == 18){
      #Create a cutoff to ensure that we stick with data that has represented itself a great deal in the prior month
      cutoff = data %>% 
        filter(grepl("pioneer",format)) %>% 
        select(date,format,style) %>%
        distinct() %>%
        group_by(format) %>%
        summarize(cutoff = n()) %>%
        ungroup()
      
      logic = data %>% 
        filter(grepl("pioneer",format)) %>% 
        filter(!grepl("(Forest|Mountain|Plains|Swamp|Island)$",card)) %>%
        arrange(card,desc(date)) %>%
        group_by(card,rarity) %>%
        summarize(
          meta_rank = round(mean(meta_rank),1),
          percent_of_meta = round(sum(potential_decks)/sum(decks_in_tournament),3),
          copies_entered = sum(qty),
          avg_copies_in_deck = round(mean(avg_copies),0),
          tournaments_entered = n()
        ) %>%
        ungroup() %>%
        filter(tournaments_entered >= cutoff$cutoff/2 ) %>%
        arrange(meta_rank) %>%
        mutate(meta_rank = seq(nrow(.)))
      
      logic_chosen = "Pioneer Meta Data"
      
      media_component = logic %>% cujos_cyber_bars(.,percent_of_meta,y_format = "percent",main=logic_chosen,cap="Powered By MTGBAN.com")
      
      tweet_content = paste0(logic$card[1],", ",
                             logic$card[2],", ",
                             logic$card[3],", ",
                             logic$card[4]," & ",
                             logic$card[5],
                             " are the defining cards for the Pioneer format this prior month via mtgo. Of all decks submitted, X% contain these cards, and define the current meta",
                             " #mtgban #mtgfinance")
      
      if(nchar(tweet_content)> 280){
        tweet_content = ""
      }
      
      
      
      return(list(logic,logic_chosen,media_component,tweet_content))
    }
    if(day_in_month == 19){
      #Create a cutoff to ensure that we stick with data that has represented itself a great deal in the prior month
      cutoff = data %>% 
        filter(grepl("modern",format)) %>% 
        select(date,format,style) %>%
        distinct() %>%
        group_by(format) %>%
        summarize(cutoff = n()) %>%
        ungroup()
      
      logic = data %>% 
        filter(grepl("modern",format)) %>% 
        filter(!grepl("(Forest|Mountain|Plains|Swamp|Island)$",card)) %>%
        arrange(card,desc(date)) %>%
        group_by(card,rarity) %>%
        summarize(
          meta_rank = round(mean(meta_rank),1),
          percent_of_meta = round(sum(potential_decks)/sum(decks_in_tournament),3),
          copies_entered = sum(qty),
          avg_copies_in_deck = round(mean(avg_copies),0),
          tournaments_entered = n()
        ) %>%
        ungroup() %>%
        filter(tournaments_entered >= cutoff$cutoff/2) %>%
        arrange(meta_rank) %>%
        mutate(meta_rank = seq(nrow(.)))
      
      logic_chosen = "Modern Meta Data"
      
      
      media_component = logic %>% cujos_cyber_bars(.,percent_of_meta,y_format = "percent",main=logic_chosen)
      
      tweet_content = paste0(logic$card[1],", ",
                             logic$card[2],", ",
                             logic$card[3],", ",
                             logic$card[4]," & ",
                             logic$card[5],
                             " are the defining cards for the Modern format this prior month via mtgo. Of all decks submitted, X% contain these cards, and define the current meta",
                             " #mtgban #mtgfinance")
      
      if(nchar(tweet_content)> 280){
        tweet_content = ""
      }
      
      
      
      return(list(logic,logic_chosen,media_component,tweet_content))
    }
    if(day_in_month == 20){
      #Create a cutoff to ensure that we stick with data that has represented itself a great deal in the prior month
      cutoff = data %>% 
        filter(grepl("legacy",format)) %>% 
        select(date,format,style) %>%
        distinct() %>%
        group_by(format) %>%
        summarize(cutoff = n()) %>%
        ungroup()
      
      logic = data %>% 
        filter(grepl("legacy",format)) %>% 
        filter(!grepl("(Forest|Mountain|Plains|Swamp|Island)$",card)) %>%
        arrange(card,desc(date)) %>%
        group_by(card,rarity) %>%
        summarize(
          meta_rank = round(mean(meta_rank),1),
          percent_of_meta = round(sum(potential_decks)/sum(decks_in_tournament),3),
          copies_entered = sum(qty),
          avg_copies_in_deck = round(mean(avg_copies),0),
          tournaments_entered = n()
        ) %>%
        ungroup() %>%
        filter(tournaments_entered >= cutoff$cutoff/2) %>%
        arrange(meta_rank) %>%
        mutate(meta_rank = seq(nrow(.)))
      
      logic_chosen = "Legacy Meta Data"
      
      media_component = logic %>% cujos_cyber_bars(.,percent_of_meta,y_format = "percent",main=logic_chosen)
      
      tweet_content = paste0(logic$card[1],", ",
                             logic$card[2],", ",
                             logic$card[3],", ",
                             logic$card[4]," & ",
                             logic$card[5],
                             " are the defining cards for the Legacy format this prior month via mtgo. Of all decks submitted, X% contain these cards, and define the current meta",
                             " #mtgban #mtgfinance")
      
      if(nchar(tweet_content)> 280){
        tweet_content = ""
      }
      
      
      
      return(list(logic,logic_chosen,media_component,tweet_content))
    }
    if(day_in_month == 21){
      #Create a cutoff to ensure that we stick with data that has represented itself a great deal in the prior month
      cutoff = data %>% 
        filter(grepl("pauper",format)) %>% 
        select(date,format,style) %>%
        distinct() %>%
        group_by(format) %>%
        summarize(cutoff = n()) %>%
        ungroup()
      
      logic = data %>% 
        filter(grepl("pauper",format)) %>% 
        filter(!grepl("(Forest|Mountain|Plains|Swamp|Island)$",card)) %>%
        arrange(card,desc(date)) %>%
        group_by(card,rarity) %>%
        summarize(
          meta_rank = round(mean(meta_rank),1),
          percent_of_meta = round(sum(potential_decks)/sum(decks_in_tournament),3),
          copies_entered = sum(qty),
          avg_copies_in_deck = round(mean(avg_copies),0),
          tournaments_entered = n()
        ) %>%
        ungroup() %>%
        filter(tournaments_entered >= cutoff$cutoff/2) %>%
        arrange(meta_rank) %>%
        mutate(meta_rank = seq(nrow(.)))
      
      logic_chosen = "Pauper Meta Data"
      
      media_component = logic %>% cujos_cyber_bars(.,percent_of_meta,y_format = "percent",main=logic_chosen)
      
      tweet_content = paste0(logic$card[1],", ",
                             logic$card[2],", ",
                             logic$card[3],", ",
                             logic$card[4]," & ",
                             logic$card[5],
                             " are the defining cards for the Pauper format this prior month via mtgo. Of all decks submitted, X% contain these cards, and define the current meta",
                             " #mtgban #mtgfinance")
      
      if(nchar(tweet_content)> 280){
        tweet_content = ""
      }
      
      
      
      return(list(logic,logic_chosen,media_component,tweet_content))
    }
    #Sealed Only 22:25
    #test on my end to ensure functionality
    #data = basket_sealed_sale_price_tbl
    if(day_in_month == 22){
      #Create a cutoff to ensure that we stick with data that has represented itself a great deal in the prior month
      cutoff = data %>% select(ct) %>% summarize(cutoff = max(ct)/2)
      logic = data %>% filter(ct >= cutoff$cutoff) %>% filter(change_sum == min(change_sum))
      
      logic_chosen = "Worst Sealed by Value Lost this Month"
      
      media_component = logic %>% cujos_cyber_lines(.,offer,y_format="dollars",ylab="Sold For",main=logic_chosen)
      
      tweet_content = paste0("The ",
                             unique(logic$card),
                             " has experienced a decline this prior month.",
                             "Sealed product tends to move low and slow, so why this item has declined should raise some eyebrows... ",
                             "#mtgban #mtgfinance")
      
      if(nchar(tweet_content)> 280){
        tweet_content = ""
      }
      
      return(list(logic,logic_chosen,media_component,tweet_content))
    }
    if(day_in_month == 23){
      #Create a cutoff to ensure that we stick with data that has represented itself a great deal in the prior month
      cutoff = data %>% select(ct) %>% summarize(cutoff = max(ct)/2)
      logic = data %>% filter(ct >= cutoff$cutoff) %>% filter(change_sum == max(change_sum))
      logic_chosen = "Best Sealed Performer by Value Lost this Month"
      
      media_component = logic %>% cujos_cyber_lines(.,offer,y_format="dollars",ylab="Sold For",main=logic_chosen)
      
      tweet_content = paste0("The ",
                             unique(logic$card),
                             " has experienced an increase this prior month.",
                             "Sealed product tends to move low and slow, so why this item has increase recently is likely of interest... ",
                             "#mtgban #mtgfinance")
      
      if(nchar(tweet_content)> 280){
        tweet_content = ""
      }
      
      return(list(logic,logic_chosen,media_component,tweet_content))
    }
    #test on my end to ensure functionality
    #data = basket_sealed_copies_sold
    if(day_in_month == 24){
      #Create a cutoff to ensure that we stick with data that has represented itself a great deal in the prior month
      cutoff = data %>% select(ct) %>% summarize(cutoff = max(ct)/2)
      logic = data %>% filter(!grepl("Secret Lair",card)) %>%filter(ct >= cutoff$cutoff) %>% filter(boolean_score == min(boolean_score))
      logic_chosen = "Worst Sealed by Rate of Decline"
      
      media_component = logic %>% cujos_cyber_lines(.,offer,ylab="Sold For",main=logic_chosen)
      
      
      tweet_content = paste0("The ",
                             unique(logic$card),
                             " is experiencing a high rate of loss this prior month.",
                             "These rapid losers tend to be standard products falling out of fashion...buying opp or indications of things to come for the set? ",
                             "#mtgban #mtgfinance")
      
      if(nchar(tweet_content)> 280){
        tweet_content = ""
      }
      
      return(list(logic,logic_chosen,media_component,tweet_content))
    }
    if(day_in_month == 25){
      #Create a cutoff to ensure that we stick with data that has represented itself a great deal in the prior month
      cutoff = data %>% select(ct) %>% summarize(cutoff = max(ct)/2)
      logic = data %>% filter(!grepl("Secret Lair",card)) %>%filter(ct >= cutoff$cutoff) %>% filter(boolean_score == max(boolean_score))
      logic_chosen = "Best Sealed Performer by Rate of Increase"
      
      
      media_component = logic %>% cujos_cyber_lines(.,offer,ylab="Sold For",main=logic_chosen)
      
      
      tweet_content = paste0("The ",
                             unique(logic$card),
                             " is experiencing a high rate of growth this prior month.",
                             "Sealed growth is rare, and if the item is rising, are the cards inside of greater value than the sealed itself? Or is the reverse happening. ",
                             "#mtgban #mtgfinance")
      
      if(nchar(tweet_content)> 280){
        tweet_content = ""
      }
      
      return(list(logic,logic_chosen,media_component,tweet_content))
    }
    #Parlour Trick With CK Buy List 
    #test on my end to ensure functionality
    #data = card_kingdom_buylist_review()
    if(day_in_month == 26){
      logic = data
      logic_chosen = "Card Kingdom's REAL Hotlist"
      
      media_component = logic %>%rename(card=data.name)%>% cujos_extra_cyber_bars(.,Tier,main=logic_chosen)
      
      tweet_content = paste0("CK's hot buylist has always been a curiosity to me. The data they release on their wants has never been in line with what their data is asking for. I've had this in my scripts for years as a parlour trick... This is what they really* want. ",
                             "#mtgban #mtgfinance")
      
      if(nchar(tweet_content)> 280){
        tweet_content = ""
      }
      
      return(list(logic,logic_chosen,media_component,tweet_content))
    }
    #Secret Lair Data 27:30
    #test on my end to ensure functionality
    #data_base = secret_lair_data()
    #data = data_base[[1]]
    if(day_in_month == 27){
      logic = data %>% clean_names() %>% filter(!is.na(tc_gplayer_id)) %>% arrange(desc(monthly_sell_through)) %>% head(10)
      logic_chosen = "Best Secret Lair by Monthly Sales Rate"
      
      media_component = logic %>% cujos_secret_cyber_bars(.,monthly_sell_through,y_format="percent",ylab="% Inventory Sold",main=logic_chosen)
      
      tweet_content = paste0("Secret Lair's are the future aye? Alrighty then. Let's looks at the best sealed releases are performing. We're looking at the amount of copies sold vs current inventory to discover which are the best products ",
                             "#mtgban #mtgfinance")
      
      if(nchar(tweet_content)> 280){
        tweet_content = ""
      }
      
      return(list(logic,logic_chosen,media_component,tweet_content))
      
    }
    if(day_in_month == 28){
      logic = data %>% clean_names() %>% filter(!is.na(tc_gplayer_id)) %>% arrange(desc(last_4_months_qty)) %>% head(10)
      logic_chosen = "Best Secret Lair by Quarterly Editions Sold"
      
      
      media_component = logic %>% arrange(last_4_months_qty) %>% cujos_secret_cyber_bars(.,last_4_months_qty,y_format = NULL,ylab="Products Sold",main=logic_chosen)
      
      tweet_content = paste0("Okay, I'm sassy, I also think Secret Lairs are the future. As such, let's throw that window wider into the past and look at all sealed copies sold in the last 3 months.
                              Remember, % only matter with solid sales amts beneath them.",
                             "#mtgban #mtgfinance")
      
      if(nchar(tweet_content)> 280){
        tweet_content = ""
      }
      
      return(list(logic,logic_chosen,media_component,tweet_content))
      
      
    }
    #test on my end to ensure functionality
    #data = data_base[[2]]
    if(day_in_month == 29){
      logic = data %>% clean_names() %>% select(-condition_2) %>% filter(!is.na(tc_gplayer_id)) %>% filter(current_supply >= 15) %>% arrange(desc(monthly_sell_through)) %>% head(20)
      logic_chosen = "Best Secret Lair Singles by Monthly Sales Rate"
      
      media_component = logic %>% cujos_secret_cyber_bars(.,monthly_sell_through,y_format="percent",ylab="% Inventory Sold",main=logic_chosen)
      
      
      tweet_content = paste0("Remember, percentages aren't everything... That said... when a cards overall sell through rate for the month is greater than 20%, that's normally a great win. Meanwhile secret lair cards... Not Hard to why vendors love these products",
                             "#mtgban #mtgfinance")
      
      if(nchar(tweet_content)> 280){
        tweet_content = ""
      }
      
      return(list(logic,logic_chosen,media_component,tweet_content))
    }
    if(day_in_month == 30){
      logic = data %>% clean_names() %>% filter(!is.na(tc_gplayer_id)) %>% filter(current_supply >= 15)%>% arrange(desc(qty_last_4_months)) %>% head(20)
      logic_chosen = "Best Secret Lair Singles by Quarterly Copies Sold"
      
      media_component = logic %>% arrange(qty_last_4_months) %>% cujos_secret_cyber_bars(.,qty_last_4_months,y_format = NULL,ylab="Copies Sold",main=logic_chosen)
      
      tweet_content = paste0("Please be mindful, cards that are cheaper will sell more copies. Cards of higher value being compared to lower value cards irg to copies sold is very important elemtn to keep in mind. A 20$ card is moving as many copies as a $1 card? That is of interest.",
                             "#mtgban #mtgfinance")
      
      if(nchar(tweet_content)> 280){
        tweet_content = ""
      }
      
      
      return(list(logic,logic_chosen,media_component,tweet_content))
    }
    #test on my end to ensure functionality - Shilling still needs testing now
    #SHILL BABY SHILL
    if(day_in_month == 31){
      logic = "I don't need logic to shill. MTGBAN IS THE BEST. I like money, please support me."
      logic_chosen  = "I don't need logic to shill. MTGBAN IS THE BEST. I like money, please support me."
      media_component = ""
      
      tweet_content = paste0("Please support me over at: https://www.patreon.com/ban_community . We're focused on open source contributions, both monetarily and code, we really want to move mtg finance out of the 90's from a tech, data, and community understanding as fast as we can.",
                             "#mtgban #mtgfinance")
      
      if(nchar(tweet_content)> 280){
        tweet_content = ""
      }
      
      
      return(list(logic,logic_chosen,media_component,tweet_content))
    }
    
  }
  
  #Pull appropriate data from BQ, GS, or my own general scripts depending on dates for content generation
  if(day_in_month <=4){
  con <- gaeas_cradle(my_secrets$patches)
  statement <- paste('SELECT most_recent_table, DATE_DIFF( CURRENT_DATE() , most_recent_table ,DAY) days_behind_today
                    FROM (
                        SELECT max(CAST(regexp_replace(regexp_extract(table_name,"\\\\d{4}_\\\\d{2}_\\\\d{2}"),"_","-") as Date)) most_recent_table
                        FROM `gaeas-cradle`.mtg_basket.INFORMATION_SCHEMA.TABLES
                    )
 ',sep = "")
  table_limitations <- dbSendQuery(con, statement = statement) %>% dbFetch( n = -1) %>% as_tibble()
  
  
  con <- gaeas_cradle(my_secrets$patches)
  statement <- paste("SELECT * ","FROM `gaeas-cradle.ban_buylist.*` a 
  LEFT JOIN (SELECT tcg_id,card,c.set,c.rarity,c.number FROM `gaeas-cradle.roster.mtgjson_ban` c) b on a.tcg_id = b.tcg_id 
  WHERE _TABLE_SUFFIX BETWEEN
  FORMAT_DATE('%Y_%m_%d', DATE_SUB(CURRENT_DATE(), INTERVAL ",30 + table_limitations$days_behind_today ," DAY)) AND
  FORMAT_DATE('%Y_%m_%d', DATE_SUB(CURRENT_DATE(), INTERVAL ",table_limitations$days_behind_today ," DAY)) AND
                   regexp_contains(vendor,'Card Kingdom') ",sep = "")
  ban_bl_tbl <- dbSendQuery(con, statement = statement) %>% dbFetch( n = -1) %>% select(-tcg_id_1) %>% distinct()
  
 
  con <- gaeas_cradle(my_secrets$patches)
  statement <- paste("SELECT * ","FROM `gaeas-cradle.ban_retail.*` a 
  LEFT JOIN (SELECT uuid,tcg_id,card,c.set,c.rarity,c.number FROM `gaeas-cradle.roster.mtgjson_ban` c) b on a.tcg_id = b.uuid  
  WHERE _TABLE_SUFFIX BETWEEN
  FORMAT_DATE('%Y_%m_%d', DATE_SUB(CURRENT_DATE(), INTERVAL ",30 + table_limitations$days_behind_today ," DAY)) AND
  FORMAT_DATE('%Y_%m_%d', DATE_SUB(CURRENT_DATE(), INTERVAL ",table_limitations$days_behind_today ," DAY)) AND
                   regexp_contains(vendor, 'TCG Low') ",sep = "")
  ban_retail_tbl <- dbSendQuery(con, statement = statement) %>% dbFetch( n = -1) %>% select(-tcg_id,-uuid) %>% rename(tcg_id=tcg_id_1) %>% distinct()
  
  

  pacman::p_load(janitor)
  
  
  bl_review_tbl = bl_pre_preparations(ban_bl_tbl)
  
  performant = "Unknown"
  while(performant == "Unknown"){
    
    all_content = post_logic(bl_review_tbl)
    
    performant = all_content[[1]]
    
    if(length(unique(performant$tcg_id)) > 1){
      performant_prio = performant %>% 
        filter(Date == min(Date)) %>% 
        arrange(desc(offer)) %>%
        .[1,];
      performant = performant %>% filter( (tcg_id %in% performant_prio$tcg_id) & (hasFoil %in% performant_prio$hasFoil) )
    }
    
    ggplot_data = performant %>%
      select(-l_offer,-contains("change"),-contains("boolean")) %>%
      full_join(
        ban_retail_tbl %>% 
          mutate(tcg_id = as.numeric(tcg_id)) %>%
          filter(tcg_id == unique(performant$tcg_id) & hasFoil == unique(performant$hasFoil)) %>% 
          arrange(Date) %>%
          rename(mkt_vendor = vendor) %>%
          select(-id)
      )
    
    con <- gaeas_cradle(my_secrets$patches)
    statement <- paste('SELECT * FROM `gaeas-cradle.ban_buylist.*` a 
  LEFT JOIN (SELECT tcg_id,card,c.set,c.rarity,c.number FROM `gaeas-cradle.roster.mtgjson_ban` c) b on a.tcg_id = b.tcg_id
  WHERE _TABLE_SUFFIX BETWEEN
  FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL ',30 + table_limitations$days_behind_today ,' DAY)) AND
  FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL ',table_limitations$days_behind_today ,' DAY)) AND
                  not regexp_contains(vendor,"',unique(ggplot_data %>% filter(!is.na(vendor)) %>% select(vendor) ),'") AND card like "',unique(ggplot_data$card),'"  
                   AND b.set like "',unique(ggplot_data$set),'"  
                   AND b.number like "',unique(ggplot_data$number),'" 
                   AND a.hasFoil = ',unique(ggplot_data$hasFoil),' 
  ORDER BY id, Date',sep = "")
    performant_alt_bl_tbl <- dbSendQuery(con, statement = statement) %>% dbFetch( n = -1) %>% select(-tcg_id_1)
    
    ggplot_aux_bl_data = performant_alt_bl_tbl %>% 
      mutate(tcg_id = as.numeric(tcg_id)) %>%
      group_by(Date,tcg_id,card,set,rarity,number,hasFoil) %>%
      summarize(all_others_offer = round(mean(offer,na.rm=T),-1)) %>%
      ungroup() %>%
      arrange(Date)
    
    
    
    con <- gaeas_cradle(my_secrets$patches)
    statement <- paste('SELECT * FROM `gaeas-cradle.ban_retail.*` a 
  LEFT JOIN (SELECT uuid,tcg_id,card,c.set,c.rarity,c.number FROM `gaeas-cradle.roster.mtgjson_ban` c) b on a.tcg_id = b.uuid
  WHERE _TABLE_SUFFIX BETWEEN
  FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL ',30 + table_limitations$days_behind_today ,' DAY)) AND
  FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL ',table_limitations$days_behind_today ,' DAY)) AND
                  not regexp_contains(vendor,"(',unique(ggplot_data %>% filter(!is.na(mkt_vendor)) %>% select(mkt_vendor) ),'|Trend)") AND card like "',unique(ggplot_data$card),'"  
                   AND b.set like "',unique(ggplot_data$set),'"  
                   AND b.number like "',unique(ggplot_data$number),'" 
                   AND a.hasFoil = ',unique(ggplot_data$hasFoil),' 
  ORDER BY id, Date',sep = "")
    performant_alt_mkt_tbl <- dbSendQuery(con, statement = statement) %>% dbFetch( n = -1) %>% select(-tcg_id,-uuid) %>% rename(tcg_id = tcg_id_1)
    
    if(nrow(performant_alt_mkt_tbl) == 0){bl_review_tbl = bl_review_tbl %>% filter( (tcg_id != unique(performant$tcg_id)) & (hasFoil != unique(performant$hasFoil)));performant = "Unknown"}
    
    if(length(performant)>1){print("All Error Handling Completed");break}
    
  }
  
  ggplot_aux_mkt_data = performant_alt_mkt_tbl %>% 
    mutate(tcg_id = as.numeric(tcg_id)) %>%
    group_by(Date,tcg_id,card,set,rarity,number,hasFoil) %>%
    summarize(all_others_mkt = round(mean(mkt_value,na.rm=T),-1)) %>%
    ungroup() %>%
    arrange(Date)
  
  con <- gaeas_cradle(my_secrets$patches)
  
  statement <- paste('SELECT date,tcg_id,version,condition,language,listing_type,sold_quantity,sell_price FROM `gaeas-cradle.mtg_basket.*` a 
  WHERE _TABLE_SUFFIX BETWEEN
  FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL ',30 + table_limitations$days_behind_today ,' DAY)) AND
  FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL ',table_limitations$days_behind_today ,' DAY)) AND
                  card_name like "',unique(ggplot_data$card),'"  
                   AND a.set like "',unique(ggplot_data$set),'"  
                   AND a.number = ',unique(ggplot_data$number),' 
                   AND a.version = ',unique(ggplot_data$hasFoil),' 
  ORDER BY Date',sep = "")
  mtg_basket_tbl <- dbSendQuery(con, statement = statement) %>% dbFetch( n = -1)
  
  if(nrow(mtg_basket_tbl)==0){
    statement <- paste('SELECT date,tcg_id,version,condition,language,listing_type,sold_quantity,sell_price FROM `gaeas-cradle.mtg_basket.*` a 
  WHERE _TABLE_SUFFIX BETWEEN
  FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL ',(30 + table_limitations$days_behind_today ) ,' DAY)) AND
  FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL ',table_limitations$days_behind_today ,' DAY)) AND
                  card_name like "',unique(ggplot_data$card),'"  
                   AND a.set like "',unique(ggplot_data$set),'" 
                   AND a.version = ',unique(ggplot_data$hasFoil),' 
  ORDER BY Date',sep = "")
    mtg_basket_tbl <- dbSendQuery(con, statement = statement) %>% dbFetch( n = -1)
  }
  
  cleansed_basket_tbl = mtg_basket_tbl %>% 
    mutate(listing_type = ifelse(listing_type == 1, "Photo","")) %>%
    group_by(date,tcg_id,version) %>%
    summarize(conditions = paste(condition, collapse = ", "),
              languages = paste(language, collapse = ", "),
              listings_sold = paste(listing_type, collapse = ", "),
              sold_quantity = sum(sold_quantity),
              sell_price = round(mean(sell_price),0)) %>%
    ungroup()
  
  
  
  gg_data = ggplot_data %>%
    full_join(ggplot_aux_bl_data) %>%
    full_join(ggplot_aux_mkt_data) %>%
    full_join(cleansed_basket_tbl %>% select(tcg_id,version,date,sold_quantity,sell_price) %>% rename(hasFoil=version), by=c("tcg_id"="tcg_id","Date"="date","hasFoil"="hasFoil")) %>%
    rename(ck_offer = offer, tcg_low = mkt_value) %>%
    clean_names() %>%
    select(-ct) %>%
    filter(!is.na(vendor)) %>%
    arrange(date) %>%
    distinct() %>%
    replace(is.na(.),0)
  
  return(list(gg_data,all_content[[2]],all_content[[3]]))
  }else 
    if( (day_in_month > 4)&(day_in_month<=8) ){
      con <- gaeas_cradle(my_secrets$patches)
      statement <- paste('SELECT most_recent_table, DATE_DIFF( CURRENT_DATE() , most_recent_table ,DAY) days_behind_today
                    FROM (
                        SELECT max(CAST(regexp_replace(regexp_extract(table_name,"\\\\d{4}_\\\\d{2}_\\\\d{2}"),"_","-") as Date)) most_recent_table
                        FROM `gaeas-cradle`.mtg_basket.INFORMATION_SCHEMA.TABLES
                    )
 ',sep = "")
      table_limitations <- dbSendQuery(con, statement = statement) %>% dbFetch( n = -1) %>% as_tibble()
      
      con <- gaeas_cradle(my_secrets$patches)
      statement <- paste('SELECT most_recent_table, DATE_DIFF( CURRENT_DATE() , most_recent_table ,DAY) days_behind_today
                    FROM (
                        SELECT max(CAST(regexp_replace(regexp_extract(table_name,"\\\\d{4}_\\\\d{2}_\\\\d{2}"),"_","-") as Date)) most_recent_table
                        FROM `gaeas-cradle`.mtg_basket.INFORMATION_SCHEMA.TABLES
                    )
 ',sep = "")
      table_limitations <- dbSendQuery(con, statement = statement) %>% dbFetch( n = -1) %>% as_tibble()
      
      con <- gaeas_cradle(my_secrets$patches)
      statement <- paste("SELECT * ","FROM `gaeas-cradle.ban_buylist.*` a 
  LEFT JOIN (SELECT tcg_id,card,c.set,c.rarity,c.number FROM `gaeas-cradle.roster.mtgjson_ban` c) b on a.tcg_id = b.tcg_id 
  WHERE _TABLE_SUFFIX BETWEEN
  FORMAT_DATE('%Y_%m_%d', DATE_SUB(CURRENT_DATE(), INTERVAL ",30 + table_limitations$days_behind_today ," DAY)) AND
  FORMAT_DATE('%Y_%m_%d', DATE_SUB(CURRENT_DATE(), INTERVAL ",table_limitations$days_behind_today ," DAY)) AND
                   regexp_contains(vendor,'Hare') ",sep = "")
      jpn_ban_bl_tbl <- dbSendQuery(con, statement = statement) %>% dbFetch( n = -1) %>% select(-tcg_id_1) %>% distinct()
      
      jpn_bl_review_tbl = bl_pre_preparations(jpn_ban_bl_tbl)
      
      performant = "Unknown"
      while(performant == "Unknown"){
        
        all_content = post_logic(jpn_bl_review_tbl)
        
        performant = all_content[[1]]
        
        if(length(unique(performant$tcg_id)) > 1){
          performant_prio = performant %>% 
            filter(Date == min(Date)) %>% 
            arrange(desc(offer)) %>%
            .[1,];
          performant = performant %>% filter( (tcg_id %in% performant_prio$tcg_id) & (hasFoil %in% performant_prio$hasFoil) )
        }
        
        ggplot_data = performant %>%
          select(-l_offer,-contains("change"),-contains("boolean")) %>%
          full_join(
            ban_retail_tbl %>% 
              mutate(tcg_id = as.numeric(tcg_id)) %>%
              filter(tcg_id == unique(performant$tcg_id) & hasFoil == unique(performant$hasFoil)) %>% 
              arrange(Date) %>%
              rename(mkt_vendor = vendor) %>%
              select(-id)
          )
        
        con <- gaeas_cradle(my_secrets$patches)
        statement <- paste('SELECT * FROM `gaeas-cradle.ban_buylist.*` a 
  LEFT JOIN (SELECT tcg_id,card,c.set,c.rarity,c.number FROM `gaeas-cradle.roster.mtgjson_ban` c) b on a.tcg_id = b.tcg_id
  WHERE _TABLE_SUFFIX BETWEEN
  FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL ',30 + table_limitations$days_behind_today ,' DAY)) AND
  FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL ',table_limitations$days_behind_today ,' DAY)) AND
                  not regexp_contains(vendor,"',unique(ggplot_data %>% filter(!is.na(vendor)) %>% select(vendor) ),'") AND card like "',unique(ggplot_data$card),'"  
                   AND b.set like "',unique(ggplot_data$set),'"  
                   AND b.number like "',unique(ggplot_data$number),'" 
                   AND a.hasFoil = ',unique(ggplot_data$hasFoil),' 
  ORDER BY id, Date',sep = "")
        performant_alt_bl_tbl <- dbSendQuery(con, statement = statement) %>% dbFetch( n = -1) %>% select(-tcg_id_1)
        
        ggplot_aux_bl_data = performant_alt_bl_tbl %>% 
          mutate(tcg_id = as.numeric(tcg_id)) %>%
          group_by(Date,tcg_id,card,set,rarity,number,hasFoil) %>%
          summarize(all_others_offer = round(mean(offer,na.rm=T),-1)) %>%
          ungroup() %>%
          arrange(Date)
        
        
        
        con <- gaeas_cradle(my_secrets$patches)
        statement <- paste('SELECT * FROM `gaeas-cradle.ban_retail.*` a 
  LEFT JOIN (SELECT uuid,tcg_id,card,c.set,c.rarity,c.number FROM `gaeas-cradle.roster.mtgjson_ban` c) b on a.tcg_id = b.uuid
  WHERE _TABLE_SUFFIX BETWEEN
  FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL ',30 + table_limitations$days_behind_today ,' DAY)) AND
  FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL ',table_limitations$days_behind_today ,' DAY)) AND
                  not regexp_contains(vendor,"(',unique(ggplot_data %>% filter(!is.na(mkt_vendor)) %>% select(mkt_vendor) ),'|Trend)") AND card like "',unique(ggplot_data$card),'"  
                   AND b.set like "',unique(ggplot_data$set),'"  
                   AND b.number like "',unique(ggplot_data$number),'" 
                   AND a.hasFoil = ',unique(ggplot_data$hasFoil),' 
  ORDER BY id, Date',sep = "")
        performant_alt_mkt_tbl <- dbSendQuery(con, statement = statement) %>% dbFetch( n = -1) %>% select(-tcg_id,-uuid) %>% rename(tcg_id = tcg_id_1)
        
        if(nrow(performant_alt_mkt_tbl) == 0){bl_review_tbl = bl_review_tbl %>% filter( (tcg_id != unique(performant$tcg_id)) & (hasFoil != unique(performant$hasFoil)));performant = "Unknown"}
        
        if(length(performant)>1){print("All Error Handling Completed");break}
        
      }
      
      ggplot_aux_mkt_data = performant_alt_mkt_tbl %>% 
        mutate(tcg_id = as.numeric(tcg_id)) %>%
        group_by(Date,tcg_id,card,set,rarity,number,hasFoil) %>%
        summarize(all_others_mkt = round(mean(mkt_value,na.rm=T),-1)) %>%
        ungroup() %>%
        arrange(Date)
      
      con <- gaeas_cradle(my_secrets$patches)
      
      statement <- paste('SELECT date,tcg_id,version,condition,language,listing_type,sold_quantity,sell_price FROM `gaeas-cradle.mtg_basket.*` a 
  WHERE _TABLE_SUFFIX BETWEEN
  FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL ',30 + table_limitations$days_behind_today ,' DAY)) AND
  FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL ',table_limitations$days_behind_today ,' DAY)) AND
                  card_name like "',unique(ggplot_data$card),'"  
                   AND a.set like "',unique(ggplot_data$set),'"  
                   AND a.number = ',unique(ggplot_data$number),' 
                   AND a.version = ',unique(ggplot_data$hasFoil),' 
  ORDER BY Date',sep = "")
      mtg_basket_tbl <- dbSendQuery(con, statement = statement) %>% dbFetch( n = -1)
      
      if(nrow(mtg_basket_tbl)==0){
        statement <- paste('SELECT date,tcg_id,version,condition,language,listing_type,sold_quantity,sell_price FROM `gaeas-cradle.mtg_basket.*` a 
  WHERE _TABLE_SUFFIX BETWEEN
  FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL ',(30 + table_limitations$days_behind_today ) ,' DAY)) AND
  FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL ',table_limitations$days_behind_today ,' DAY)) AND
                  card_name like "',unique(ggplot_data$card),'"  
                   AND a.set like "',unique(ggplot_data$set),'" 
                   AND a.version = ',unique(ggplot_data$hasFoil),' 
  ORDER BY Date',sep = "")
        mtg_basket_tbl <- dbSendQuery(con, statement = statement) %>% dbFetch( n = -1)
      }
      
      cleansed_basket_tbl = mtg_basket_tbl %>% 
        mutate(listing_type = ifelse(listing_type == 1, "Photo","")) %>%
        group_by(date,tcg_id,version) %>%
        summarize(conditions = paste(condition, collapse = ", "),
                  languages = paste(language, collapse = ", "),
                  listings_sold = paste(listing_type, collapse = ", "),
                  sold_quantity = sum(sold_quantity),
                  sell_price = round(mean(sell_price),0)) %>%
        ungroup()
      
      
      
      gg_data = ggplot_data %>%
        full_join(ggplot_aux_bl_data) %>%
        full_join(ggplot_aux_mkt_data) %>%
        full_join(cleansed_basket_tbl %>% select(tcg_id,version,date,sold_quantity,sell_price) %>% rename(hasFoil=version), by=c("tcg_id"="tcg_id","Date"="date","hasFoil"="hasFoil")) %>%
        rename(ck_offer = offer, tcg_low = mkt_value) %>%
        clean_names() %>%
        select(-ct) %>%
        filter(!is.na(vendor)) %>%
        arrange(date) %>%
        distinct() %>%
        replace(is.na(.),0)
      
      return(list(gg_data,all_content[[2]],all_content[[3]]))
      
    }else
      if((day_in_month > 8)&(day_in_month<=13)){
        check = 1
        con <- gaeas_cradle(my_secrets$patches)
        statement <- paste('SELECT most_recent_table, DATE_DIFF( CURRENT_DATE() , most_recent_table ,DAY) days_behind_today
                    FROM (
                        SELECT max(CAST(regexp_replace(regexp_extract(table_name,"\\\\d{4}_\\\\d{2}_\\\\d{2}"),"_","-") as Date)) most_recent_table
                        FROM `gaeas-cradle`.mtg_basket.INFORMATION_SCHEMA.TABLES
                    )
                    ',sep = "")
        table_limitations <- dbSendQuery(con, statement = statement) %>% dbFetch( n = -1) %>% as_tibble()
        
        
        con <- gaeas_cradle(my_secrets$patches)
        statement <- paste('SELECT date, tcg_id, Card_name card,a.set,rarity,number,version hasFoil,condition,a.language,listing_type, sold_quantity,dop,sell_price
                            FROM `gaeas-cradle.mtg_basket.*` a 
                            WHERE _TABLE_SUFFIX BETWEEN
                            FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL ',30 + table_limitations$days_behind_today ,' DAY)) AND
                            FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL ',table_limitations$days_behind_today ,' DAY))  
                            ORDER BY Date',sep = "")
        mtg_basket_all_tbl <- dbSendQuery(con, statement = statement) %>% dbFetch( n = -1) %>% distinct()
        
        basket_copies_sold_tbl = basket_copies_pre_preparations(mtg_basket_all_tbl)
        
        gg_data = post_logic(basket_copies_sold_tbl)
        
        while( (max(gg_data[[1]]$change,na.rm=T)/unique(gg_data[[1]]$change_sum,na.rm=T) >= .65) | (unique(gg_data[[1]]$avg_offer)<=5) ){
          if(check == 1){
            refined_basket_sale_price = basket_sale_price_tbl %>% 
              filter( tcg_id != unique(gg_data[[1]]$tcg_id)  )
            
            gg_data = post_logic(refined_basket_sale_price)
            
            check = check + 1
          }else{
            refined_basket_sale_price = refined_basket_sale_price %>% filter( (tcg_id !=  unique(gg_data[[1]]$tcg_id)) )
            
            gg_data = post_logic(refined_basket_sale_price)
            
            check = check + 1
          }
        }
        
        if(unique(gg_data[[1]]$card)>1){
          
          card_selection = gg_data[[1]] %>% filter(avg_sell_price == max(avg_sell_price))
          
          gg_data[[1]] = gg_data[[1]] %>% filter(card %in% card_selection$card)
        }
        
        return(gg_data)
        
      }else
        if((day_in_month > 13)&(day_in_month<=17)){
          check = 1
          con <- gaeas_cradle(my_secrets$patches)
          statement <- paste('SELECT most_recent_table, DATE_DIFF( CURRENT_DATE() , most_recent_table ,DAY) days_behind_today
                    FROM (
                        SELECT max(CAST(regexp_replace(regexp_extract(table_name,"\\\\d{4}_\\\\d{2}_\\\\d{2}"),"_","-") as Date)) most_recent_table
                        FROM `gaeas-cradle`.mtg_basket.INFORMATION_SCHEMA.TABLES
                    )
          ',sep = "")
          table_limitations <- dbSendQuery(con, statement = statement) %>% dbFetch( n = -1) %>% as_tibble()
          
          
          con <- gaeas_cradle(my_secrets$patches)
          statement <- paste('SELECT date, tcg_id, Card_name card,a.set,rarity,number,version hasFoil,condition,a.language,listing_type, sold_quantity,dop,sell_price
                            FROM `gaeas-cradle.mtg_basket.*` a 
                            WHERE _TABLE_SUFFIX BETWEEN
                            FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL ',30 + table_limitations$days_behind_today ,' DAY)) AND
                            FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL ',table_limitations$days_behind_today ,' DAY))  
                            ORDER BY Date',sep = "")
          mtg_basket_all_tbl <- dbSendQuery(con, statement = statement) %>% dbFetch( n = -1) %>% distinct()
          
          basket_sale_price_tbl = basket_sale_pre_preparations(mtg_basket_all_tbl)
          #data = basket_sale_price_tbl
          gg_data = post_logic(basket_sale_price_tbl)
          
          
          if(unique(gg_data[[1]]$card)>1){
            
            card_selection = gg_data[[1]] %>% filter(avg_offer == max(avg_offer))
            
            gg_data[[1]] = gg_data[[1]] %>% filter(card %in% card_selection$card)
          }
          
          
          return(gg_data)
        }else
          if((day_in_month > 17)&(day_in_month<=21)){
            gs4_auth(email=my_secrets$og_patches, use_oob = T)
            drive_auth(email=my_secrets$og_patches, use_oob = T)
            ss <- drive_get("Decklists For Ban")
            decklist_data = read_sheet(ss =ss,
                                       sheet = 'all')
            
            gg_data = post_logic(decklist_data)
            
          }else
            if((day_in_month > 21)&(day_in_month<=23)){
              con <- gaeas_cradle(my_secrets$patches)
              statement <- paste('SELECT most_recent_table, DATE_DIFF( CURRENT_DATE() , most_recent_table ,DAY) days_behind_today
                    FROM (
                        SELECT max(CAST(regexp_replace(regexp_extract(table_name,"\\\\d{4}_\\\\d{2}_\\\\d{2}"),"_","-") as Date)) most_recent_table
                        FROM `gaeas-cradle`.mtg_basket.INFORMATION_SCHEMA.TABLES
                    )
 ',sep = "")
              table_limitations <- dbSendQuery(con, statement = statement) %>% dbFetch( n = -1) %>% as_tibble()
              
              
              con <- gaeas_cradle(my_secrets$patches)
              statement <- paste('SELECT date, tcg_id, Card_name card,a.set,rarity,number,version hasFoil,condition,a.language,listing_type, sold_quantity,dop,sell_price
                            FROM `gaeas-cradle.mtg_basket.*` a 
                            WHERE _TABLE_SUFFIX BETWEEN
                            FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL ',30 + table_limitations$days_behind_today ,' DAY)) AND
                            FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL ',table_limitations$days_behind_today ,' DAY))  
                            ORDER BY Date',sep = "")
              mtg_basket_all_tbl <- dbSendQuery(con, statement = statement) %>% dbFetch( n = -1) %>% distinct()
              
              basket_sealed_sale_price_tbl = basket_sealed_sale_pre_preparations(mtg_basket_all_tbl)
              
              gg_data = post_logic(basket_sealed_sale_price_tbl)
              
              return(gg_data)
            }else
              if((day_in_month > 23)&(day_in_month<=25)){
                con <- gaeas_cradle(my_secrets$patches)
                statement <- paste('SELECT most_recent_table, DATE_DIFF( CURRENT_DATE() , most_recent_table ,DAY) days_behind_today
                    FROM (
                        SELECT max(CAST(regexp_replace(regexp_extract(table_name,"\\\\d{4}_\\\\d{2}_\\\\d{2}"),"_","-") as Date)) most_recent_table
                        FROM `gaeas-cradle`.mtg_basket.INFORMATION_SCHEMA.TABLES
                    )
 ',sep = "")
                table_limitations <- dbSendQuery(con, statement = statement) %>% dbFetch( n = -1) %>% as_tibble()
                
                con <- gaeas_cradle(my_secrets$patches)
                statement <- paste('SELECT date, tcg_id, Card_name card,a.set,rarity,number,version hasFoil,condition,a.language,listing_type, sold_quantity,dop,sell_price
                            FROM `gaeas-cradle.mtg_basket.*` a 
                            WHERE _TABLE_SUFFIX BETWEEN
                            FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL ',30 + table_limitations$days_behind_today ,' DAY)) AND
                            FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL ',table_limitations$days_behind_today ,' DAY))  
                            ORDER BY Date',sep = "")
                mtg_basket_all_tbl <- dbSendQuery(con, statement = statement) %>% dbFetch( n = -1) %>% distinct()
                
                basket_sealed_copies_sold = basket_sealed_sale_pre_preparations(mtg_basket_all_tbl)
                
                gg_data = post_logic(basket_sealed_copies_sold)
                
                return(gg_data)
              }else
                if((day_in_month > 25)&(day_in_month<=26)){
                  
                  gg_data = post_logic(card_kingdom_buylist_review())
                  
                  return(gg_data)
                } else 
                  if((day_in_month > 26)&(day_in_month<=31)){
                  
                  secret_lair = secret_lair_data()
                  gg_data = post_logic(secret_lair)
                  
                  return(gg_data)
                }
  

}


todays_content = database_pull()


todays_content[[3]]

ggsave(filename = "/home/cujo253/mines_of_moria/Essential_Referential_CSVS/twitter_media.png", device = "png", plot = todays_content[[3]])



# Let's Make a Post (finally) after all that selection work ---------------

twitter_api_keys = read_json("/home/cujo253/mines_of_moria/Essential_Referential_CSVS/twitter_api_wolfoftinstreet.json")

# Create a token containing your Twitter keys
token_for_wolf= rtweet::create_token(
    app = "automatic_poster_app",  # the name of the Twitter app
    consumer_key = twitter_api_keys$consumer_key,
    consumer_secret = twitter_api_keys$consumer_key_secret,
    access_token = twitter_api_keys$access_token,
    access_secret = twitter_api_keys$access_token_secret
)

rtweet::post_tweet(
  status = todays_content[[4]],
  media = "/home/cujo253/mines_of_moria/Essential_Referential_CSVS/twitter_media.png",
  token = token_for_wolf
)





