moexplot <- function(df, line_color = c("#2ecc71", "#3498db", "#af7ac5", "#d98880")) {

  print("Checking libraries") #check libraries
  Sys.sleep(0.5)
  is_ggplot2 <- require("ggplot2", quietly = TRUE)
  if(is_ggplot2 == FALSE) {
    print("ERROR: install 'ggplot2' package")
    stop()
  }

  print("Checking arguments") #check conditions to run function
  Sys.sleep(0.5)
  if(!is.data.frame(df)){
    print("ERROR: wrong table format, convert to df")
    stop()
  }
  if(length(line_color) < 4){
    print("ERROR: Lack of colors, add more")
    stop()
  }

  colorp <- c("Стоимость в начале торгов" = line_color[1],
              "Стоимость в конце торгов" = line_color[2],
              "Наибольшая стоимость" = line_color[3],
              "Наименьшеая стоимость" = line_color[4]) #custom legend

  im <- ggplot(df) + geom_line(aes(TRADEDATE, OPEN,
                                   color = "Стоимость в начале торгов")) +
    geom_line(aes(TRADEDATE, CLOSE, color = "Стоимость в конце торгов")) +
    geom_line(aes(TRADEDATE, HIGH, color = "Наибольшая стоимость")) +
    geom_line(aes(TRADEDATE, LOW, color = "Наименьшеая стоимость")) +
    geom_smooth(aes(TRADEDATE, (HIGH + LOW)/2)) +
    labs(x = "Дата", y = "Стоимость", color = "Легенда") +
    scale_color_manual(values = colorp)

  return(im)
}
