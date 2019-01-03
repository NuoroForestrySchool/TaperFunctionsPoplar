library(officer)
# Package `magrittr` makes officer usage easier.
library(magrittr)

if( require("ggplot2") ){
  gg <- ggplot(data = iris, aes(Sepal.Length, Petal.Length)) + 
    geom_point()
  
  read_docx() %>% 
    body_add_par(value = "Table of content", style = "heading 1") %>% 
    body_add_toc(level = 2) %>% 
    body_add_break() %>% 
    
    body_add_par(value = "dataset iris", style = "heading 2") %>% 
    body_add_table(value = head(iris), style = "table_template" ) %>% 
    
    body_add_par(value = "plot examples", style = "heading 1") %>% 
    body_add_gg(value = gg, style = "centered" ) %>% 
    
    print(target = "assets/docx/body_add_demo.docx")
}