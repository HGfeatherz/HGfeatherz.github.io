library(ggplot2)


setwd("C:/Users/HGFea/Documents/Vs_code/phd")

ggplot(dataset, aes(x = POLY4vsStandard_perc_avg, y = P..ppm.)) +

        geom_segment(aes(x = 0, y = P..ppm., xend = POLY4vsStandard_perc_avg, yend = P..ppm.)) +

        #coord_flip()+

        geom_point(color="#7e5fc4", size =4) +

        theme_classic()+

        geom_vline(xintercept=0, linetype = "dashed")+

        labs(x = "Yield: POLY4 vs SP (%)")+

        labs(y = "P (ppm)")+

        facet_wrap(vars(drought))