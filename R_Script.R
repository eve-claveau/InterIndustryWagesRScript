install.packages("tidyverse")
library(tidyverse)
install.packages("dplyr")
install.packages("ggplot2")

# importing the data --------------------------------------------------------
# assumed file structure: - a column with Skilled/Unskilled work
                        # - Columns numbered 3-6 for 4 different years
      
wages <-read_csv("german_hourly_wages.csv") # change file input for different country

# extracting yearly wages -------------------------------------------------
# computing the coefficients of variation
cv <- function(col) { sd(col) / mean(col) }
year1 <- wages %>% pull(3) %>% cv() * 100 # possibly change the column number 
                                          # depending on file structure
year2 <- wages %>% pull(4) %>% cv() * 100
year3 <- wages %>% pull(5) %>% cv() * 100
year4 <- wages %>% pull(6) %>% cv() * 100

general_wages <- tibble(
  Year = as.integer(str_sub(colnames(wages)[3:6], -4)),
  Coefficient = c(year1, year2, year3, year4),
)

# skilled labour wages only
skilled_wages <- wages %>% filter(Skill == "Skilled")

s_year1 <- skilled_wages %>% pull(3) %>% cv() * 100
s_year2 <- skilled_wages %>% pull(4) %>% cv() * 100
s_year3 <- skilled_wages %>% pull(5) %>% cv() * 100
s_year4 <- skilled_wages %>% pull(6) %>% cv() * 100

skilled_wages <- tibble(
  Year = as.integer(str_sub(colnames(wages)[3:6], -4)),
  Coefficient = c(s_year1, s_year2, s_year3, s_year4)
)


# unskilled wages
unskilled_wages <- wages %>% filter(Skill == "Unskilled")

u_year1 <- unskilled_wages %>% pull(3) %>% cv() * 100
u_year2 <- unskilled_wages %>% pull(4) %>% cv() * 100
u_year3 <- unskilled_wages %>% pull(5) %>% cv() * 100
u_year4 <- unskilled_wages %>% pull(6) %>% cv() * 100

unskilled_wages <- tibble(
  Year = as.integer(str_sub(colnames(wages)[3:6], -4)),
  Coefficient = c(u_year1, u_year2, u_year3, u_year4)
)

skilled_wages$SkillLevel <- "Skilled Labour"
general_wages$SkillLevel <- "All"


unskilled_wages$SkillLevel <- "Unskilled Labour"
all_wages <- rbind(skilled_wages, general_wages, unskilled_wages)
# Plotting ----------------------------------------------------------------

graph <- (ggplot(all_wages, aes(x = Year, y = Coefficient,
                                group = SkillLevel,
                                shape = SkillLevel)) 
          +  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), 
                                limits = c(0, NA)) 
          + scale_x_continuous(breaks = c(1913, 1925, 1930, 1939))
          + geom_line(color = "black")
          + geom_point(color = "black", size = 2.5, fill = "white")
         + scale_shape_manual( # legend
           name = "Hourly Wages by Skill Level",
           values = c("Skilled Labour" = 22,   
                      "Unskilled Labour" = 23,
                      "All" = 15  
                      ), 
           breaks = c("Skilled Labour",  "Unskilled Labour", "All") 
         )
         + labs(
           title = "Inter-Industry Wage Differentials in Germany",
           x= "Year",
           y = "Coefficient of Variation Between Industries (%)"
           )
           +  theme_classic() +
           theme(
             plot.title = element_text(hjust = 0.5, face = "bold"),
             legend.position = c(0.8, 0.2), 
             legend.background = element_rect(fill = "white", color = "black"))
           )
graph
ggsave("Wage_Differentials_Germany.png", plot = graph, width = 6, height = 4, dpi = 300)

