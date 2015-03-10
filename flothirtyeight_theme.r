flothirtyeight_theme = theme_bw() + 
        # set chart to light grey
        theme(panel.background = element_rect(fill = '#fafafa')) + 
        theme(plot.background = element_rect(fill = '#fafafa')) + 
        theme(panel.border = element_rect(color = '#fafafa')) + 
        # format the grid
        theme(panel.grid.major = element_line(color = '#D0D0D0', size = .75)) + 
        # no legend
        theme(legend.position = 'None') + 
        # set title
        theme(plot.title = element_text(face = 'bold', hjust = 2, vjust = 2, color = '#3C3C3C', size = 30)) +
        # formate axis and ticks
        theme(axis.text.x=element_text(size=11,colour="#535353",face="bold")) +
        theme(axis.text.y=element_text(size=11,colour="#535353",face="bold")) +
        theme(axis.title.y=element_text(size=20,colour="#535353",face="bold",vjust=1.5)) +
        theme(axis.title.x=element_text(size=20,colour="#535353",face="bold",vjust=-.5)) +
        # get rid of axis ticks
        theme(axis.ticks = element_blank())
