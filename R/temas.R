theme_icic = function(sentido="v"){
if(sentido=="h") add=ggthemes::theme_clean() + ggplot2::theme(plot.background = element_rect(colour = NA), 
                                                panel.background = element_rect(fill = "transparent", 
                                                                                color = NA), plot.title = element_text(hjust = 0.5), 
                                                plot.subtitle = element_text(hjust = 0.5), legend.background = element_rect(color = NA),
                                                panel.grid.major.y = element_line(linetype = 3, color = 'gray'),
                                                panel.grid.major.x = element_blank())
if(sentido=="v") add=ggthemes::theme_clean() + ggplot2::theme(plot.background = element_rect(colour = NA), 
                                                              panel.background = element_rect(fill = "transparent", 
                                                                                              color = NA), plot.title = element_text(hjust = 0.5), 
                                                              plot.subtitle = element_text(hjust = 0.5), legend.background = element_rect(color = NA),
                                                              panel.grid.major.x = element_line(linetype = 3, color = 'gray'),
                                                              panel.grid.major.y = element_blank())
return(add)}
