# need to pass the condition for the filter argument

ShowWorkMeans <- function(condition){
  PData <- AllDataTextTib %>% filter(eval(parse(text=condition))) 
  assign("count", length(PData$Q2.2_1[!is.na(PData$Q2.2_1)]), envir = .GlobalEnv)
  #print(length(PData$Q2.2_1[!is.na(PData$Q2.2_1)]))
  PData <- PData %>% summarise(across(16:30, ~ mean(.x, na.rm = TRUE))) %>%
    pivot_longer(
      cols = starts_with("Q2."),
      names_to = c("Who","Activity"),
      names_prefix = "Q2.",
      names_sep = "_",
      values_to = "Distribution",
      values_drop_na = TRUE
    ) %>%
    mutate(Who=str_replace_all(Who,c("2"="Actual","3"="Department","4"="School","5"="Campus","6"="Preferred"))) %>%
    mutate(Activity=str_replace_all(Activity, c("1"="Research","2"="Teaching","3"="Service")))
  
  PData$Who <- factor(PData$Who,levels = c("Actual", "Preferred", "Department", "School", "Campus"))
  ggplot(PData, aes(x=Who, y=Distribution, fill=Activity, label=sprintf("%0.1f", round(Distribution, digits = 1)))) +
    geom_bar(stat = "identity")+geom_text(size = 6, position = position_stack(vjust = 0.5)) + coord_flip()
}

