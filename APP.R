library(readxl)
library(arules)
library(highcharter)
library(stringr)
library(shinycssloaders)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(ggrepel)
library(DT)
library(dplyr)
library(stats)

mat2itemlists_rare <- function(mat) {
    res <- NULL
    for (i in seq_len(nrow(mat))) {
        item <- mat[i, ]
        for (col in seq_len(ncol(mat))) {
            xi <- paste0(colnames(mat)[col], '-', item[col])
            if (col == 1) {
                si <- xi
            } else {
                si <- paste0(si, " ", xi)
            }
        }
        res <- c(res, si)
    }
    return(res)
}

mat2itemlists <- function(mat, dig = FALSE) {
    res <- NULL
    items <- colnames(mat)
    for (i in seq_len(nrow(mat))) {
        xi <- items[mat[i, ] == 1]
        
        if (length(xi) == 0) {
            next
        } else {
            for (j in seq_along(xi)) {
                if (j == 1) {
                    si <- xi[j]
                } else {
                    si <- paste(si, xi[j])
                }
            }
            
            if (is.data.frame(dig)) {
                # if (dig$Diagnose[i] <= 6){
                #     xi <- paste0('diagnosis', '-', 1)
                # } else {
                #     xi <- paste0('diagnosis', '-', 0)
                # }
                
                xi <- paste0('diagnosis', '-', dig$Diagnose[i])
                si <- paste(si, xi)
            }
        }
        
        res <- c(res, si)
    }
    
    return(res)
}

get_rules <-
    function(data,
             threshold,
             parameter_supp,
             parameter_conf,
             num_rules,
             max_len = 10,
             binarization = TRUE) {
        if (binarization) {
            bin.mat <- data >= threshold
            item.lists <- mat2itemlists(bin.mat)
        } else {
            item.lists <- data
        }
        
        retail.list <- strsplit(item.lists, " ")
        retail.trans <- as(retail.list, "transactions")
        retail.rules <-
            apriori(
                retail.trans,
                parameter = list(
                    supp = parameter_supp,
                    conf = parameter_conf,
                    target = "rules",
                    maxlen = max_len
                )
            )
        ### Select the best 5 rules w.r.t. lift
        
        if (length(retail.rules) == 0) {
            return(NULL)
        } else {
            retail.rules <-
                retail.rules[!is.redundant(
                    retail.rules,
                    measure = "confidence",
                    confint = TRUE,
                    level = 0.95
                )]
            
            if (length(retail.rules) >= 5) {
                num_rules <- 5
            } else {
                num_rules <- length(retail.rules)
            }
            
            retail.li5 <-
                head(sort(retail.rules, by = "lift"), num_rules)
            
            retail.li5 <- inspect(retail.li5, linebreak = FALSE)
            retail.li5["Threshold"] <- threshold
            
            return(retail.li5)
        }
    }

get_rules_rare <-
    function(data,
             threshold,
             parameter_supp,
             parameter_conf,
             num_rules,
             max_len = 5) {
        all_rules <- NULL
        
        data_sub <- data[which(data$Diagnose == threshold), ]
        
        for (i in 2:5) {
            bin.mat <- data_sub[, 1:(ncol(data_sub) - 1)] >= i
            item.lists <- mat2itemlists(bin.mat)
            retail.list <- strsplit(item.lists, " ")
            retail.trans <- as(retail.list, "transactions")
            retail.rules <-
                apriori(
                    retail.trans,
                    parameter = list(
                        supp = parameter_supp,
                        conf = parameter_conf,
                        target = "rules",
                        maxlen = max_len
                    )
                )
            # retail.sets <- apriori(retail.trans, parameter = list(support = parameter_supp, target = "frequent itemsets"))
            if (length(retail.rules) == 0) {
                next
            } else {
                retail.rules <-
                    retail.rules[!is.redundant(
                        retail.rules,
                        measure = "confidence",
                        confint = TRUE,
                        level = 0.95
                    )]
                if (length(retail.rules) >= num_rules) {
                    num_rules <- num_rules
                } else {
                    num_rules <- length(retail.rules)
                }
                
                retail.li5 <-
                    head(sort(retail.rules, by = "lift"), num_rules)
                
                retail.li5 <- inspect(retail.li5, linebreak = FALSE)
                retail.li5["Threshold"] <- i
                retail.li5["Diagnose"] <- threshold
                
                if (!is.null(retail.li5)) {
                    if (is.null(all_rules)) {
                        all_rules <- retail.li5
                    } else {
                        all_rules <- rbind(all_rules, retail.li5)
                    }
                } else {
                    next
                }
            }
        }
        colnames(all_rules) <-
            c(
                "lhs",
                "notation",
                "rhs",
                "support",
                "confidence",
                "coverage",
                "lift",
                "frequency",
                "threshold",
                "diagnose"
            )
        all_rules["rule"] <-
            paste0(all_rules$lhs, all_rules$notation, all_rules$rhs)
        all_rules$support <- round(all_rules$support, 2)
        all_rules$confidence <- round(all_rules$confidence, 2)
        all_rules$lift <- round(all_rules$lift, 2)
        all_rules$threshold <- paste('Threshold', all_rules$threshold)
        all_rules$diagnose <- paste('Diagnosis', all_rules$diagnose)
        all_rules$thre_diag <- paste(all_rules$threshold, all_rules$diagnose)
        
        return(all_rules)
    }

get_itemsets_amount <- function(data, threshold, parameter_supp) {
    bin.mat <- data >= threshold
    item.lists <- mat2itemlists(bin.mat)
    retail.list <- strsplit(item.lists, " ")
    retail.trans <- as(retail.list, "transactions")
    
    retail.sets <-
        apriori(retail.trans,
                parameter = list(support = parameter_supp, target = "frequent itemsets"))
    
    if (length(retail.sets) == 0) {
        return(NULL)
    } else {
        retail.sets <- retail.sets[is.maximal(retail.sets)]
        
        ### Select the best 5 rules w.r.t. lift
        
        retail.itemsets <- inspect(retail.sets, linebreak = FALSE)
        
        Amount <- nrow(retail.itemsets)
        Threshold <- threshold
        
        retail.item.summary <- data.frame(Threshold, Amount)
        
        return(retail.item.summary)
    }
}

get_itemsets <-
    function(data,
             threshold,
             parameter_supp,
             num_itemsets) {
        bin.mat <- data >= threshold
        item.lists <- mat2itemlists(bin.mat)
        retail.list <- strsplit(item.lists, " ")
        retail.trans <- as(retail.list, "transactions")
        
        retail.sets <-
            apriori(retail.trans,
                    parameter = list(support = parameter_supp, target = "frequent itemsets"))
        
        if (length(retail.sets) == 0) {
            return(NULL)
        } else {
            retail.itemsets <- inspect(retail.sets, linebreak = FALSE)
            
            Threshold <- threshold
            
            if (nrow(retail.itemsets) >= num_itemsets) {
                num_itemsets <- num_itemsets
            } else {
                num_itemsets <- nrow(retail.itemsets)
            }
            
            supp_list <-
                sort(unique(retail.itemsets$support), decreasing = TRUE)[1:num_itemsets]
            
            sets <- NULL
            for (supp in supp_list) {
                if (is.null(sets)) {
                    sets <- retail.itemsets[which(retail.itemsets$support == supp), ]
                } else {
                    sets <-
                        rbind(sets, retail.itemsets[which(retail.itemsets$support == supp), ])
                }
            }
            # retail.itemsets <- top_n(retail.itemsets, num_itemsets, support)
            
            retail.item.summary <- data.frame(Threshold, sets)
            
            return(retail.item.summary)
        }
    }

get_itemsets_amount_rare <-
    function(data, threshold, parameter_supp, max_len = 5) {
        all_sets <- NULL
        data_sub <- data[which(data$Diagnose == threshold), ]
        
        for (i in 2:5) {
            bin.mat <- data_sub[, 1:(ncol(data_sub) - 1)] >= i
            item.lists <- mat2itemlists(bin.mat)
            retail.list <- strsplit(item.lists, " ")
            retail.trans <- as(retail.list, "transactions")
            retail.sets <-
                apriori(
                    retail.trans,
                    parameter = list(
                        support = parameter_supp,
                        target = "frequent itemsets",
                        maxlen = max_len
                    )
                )
            
            if (length(retail.sets) == 0) {
                next
            } else {
                retail.sets <- retail.sets[is.maximal(retail.sets)]
                
                retail.itemsets <- inspect(retail.sets, linebreak = FALSE)
                
                Amount <- nrow(retail.itemsets)
                Diagnosis <- threshold
                Threshold <- i
                
                retail.item.summary <- data.frame(Diagnosis, Threshold, Amount)
                
                if (is.null(all_sets)) {
                    all_sets <- retail.item.summary
                } else {
                    all_sets <- rbind(all_sets, retail.item.summary)
                }
            }
        }
        
        return(all_sets)
        
    }

get_itemsets_rare <-
    function(data,
             threshold,
             parameter_supp,
             num_itemsets,
             max_len = 5) {
        all_sets <- NULL
        data_sub <- data[which(data$Diagnose == threshold), ]
        
        for (i in 2:5) {
            bin.mat <- data_sub[, 1:(ncol(data_sub) - 1)] >= i
            item.lists <- mat2itemlists(bin.mat)
            retail.list <- strsplit(item.lists, " ")
            retail.trans <- as(retail.list, "transactions")
            retail.sets <-
                apriori(
                    retail.trans,
                    parameter = list(
                        support = parameter_supp,
                        target = "frequent itemsets",
                        maxlen = max_len
                    )
                )
        
            if (length(retail.sets) == 0) {
                next
            } else {
                retail.itemsets <- inspect(retail.sets, linebreak = FALSE)
                
                Diagnosis <- threshold
                Threshold <- i
                
                if (nrow(retail.itemsets) >= num_itemsets) {
                    num_itemsets <- num_itemsets
                } else {
                    num_itemsets <- nrow(retail.itemsets)
                }
                
                supp_list <-
                    sort(unique(retail.itemsets$support), decreasing = TRUE)[1:num_itemsets]
                
                sets <- NULL
                for (supp in supp_list) {
                    if (is.null(sets)) {
                        sets <- retail.itemsets[which(retail.itemsets$support == supp), ]
                    } else {
                        sets <-
                            rbind(sets, retail.itemsets[which(retail.itemsets$support == supp), ])
                    }
                }
                
                retail.item.summary <- data.frame(Diagnosis, Threshold, sets)
                
                if (is.null(all_sets)) {
                    all_sets <- retail.item.summary
                } else {
                    all_sets <- rbind(all_sets, retail.item.summary)
                }
            }
        }
        
        return(all_sets)
    }

handle_data <- function(data, gender = FALSE) {
    data.finished <- data[!is.na(data$Pseudonym), ]
    
    # long covid
    data.longcovid <- data.finished[data.finished$Group_A == 1, ]
    
    #only symptoms data
    if (!gender) {
        dat.symp <- select(data.longcovid, starts_with("SY"))
    } else {
        dat.symp <- select(data.longcovid, starts_with(c("SY", "Gender")))
    }
    
    # dat.symp <- dat[0:126, c(10:45, 72)]
    
    # NA means not answered
    dat.symp[dat.symp == "11"] <- NA
    dat.symp <- na.omit(dat.symp)
    
    return(dat.symp)
}

extract_rules <- function(data, support, confidence, num_rules) {
    all_rules <- NULL
    
    for (i in 2:10) {
        rules <- get_rules(data, i, support, confidence, num_rules)
        
        if (!is.null(rules)) {
            if (is.null(all_rules)) {
                all_rules <- rules
            } else {
                all_rules <- rbind(all_rules, rules)
            }
        } else {
            next
        }
    }
    colnames(all_rules) <-
        c(
            "lhs",
            "notation",
            "rhs",
            "support",
            "confidence",
            "coverage",
            "lift",
            "frequency",
            "threshold"
        )
    all_rules["rule"] <-
        paste0(all_rules$lhs, all_rules$notation, all_rules$rhs)
    all_rules$support <- round(all_rules$support, 2)
    all_rules$confidence <- round(all_rules$confidence, 2)
    all_rules$lift <- round(all_rules$lift, 2)
    all_rules$threshold <- paste('Threshold', all_rules$threshold)
    
    return(all_rules)
}

extract_rules_rare <-
    function(data, support, confidence, num_rules, n) {
        all_rules <- NULL
        for (i in (1:6)) {
            rules <- get_rules_rare(data, i, support, confidence, num_rules, 5)
            if (is.null(all_rules)) {
                all_rules <- rules
            } else {
                all_rules <- rbind(all_rules, rules)
            }
        }
        
        return(all_rules)
    }

extract_freq_itemsets_amount <- function(data, support) {
    all_itemsets <- NULL
    
    tag_null <- 0
    
    for (i in 2:10) {
        itemsets <- get_itemsets_amount(data, i, support)
        
        if (!is.null(itemsets)) {
            tag_null <- tag_null + 1
            if (tag_null == 1) {
                all_itemsets <- itemsets
            } else {
                all_itemsets <- rbind(all_itemsets, itemsets)
            }
        } else {
            next
        }
    }
    
    return(all_itemsets)
}

extract_freq_itemsets_amount_rare <-
    function(data, support, label) {
        all_itemsets <- NULL
        
        for (i in 1:6) {
            itemsets <-
                get_itemsets_amount_rare(data, i, support)
            
            
            if (is.null(all_itemsets)) {
                all_itemsets <- itemsets
            } else {
                all_itemsets <- rbind(all_itemsets, itemsets)
            }
            
        }
        
        return(all_itemsets)
    }

extract_freq_itemsets <- function(data, support, num_itemsets) {
    all_itemsets <- NULL
    
    tag_null <- 0
    
    for (i in 2:10) {
        itemsets <- get_itemsets(data, i, support, num_itemsets)
        
        if (!is.null(itemsets)) {
            tag_null <- tag_null + 1
            if (tag_null == 1) {
                all_itemsets <- itemsets
            } else {
                all_itemsets <- rbind(all_itemsets, itemsets)
            }
        } else {
            next
        }
    }
    
    colnames(all_itemsets) <-
        c("threshold", "item", "support", "frequency")
    all_itemsets$support <- round(all_itemsets$support, 3)
    all_itemsets$threshold <-
        paste('Threshold', all_itemsets$threshold)
    
    return(all_itemsets)
}

extract_freq_itemsets_rare <-
    function(data, support, label, num_itemsets) {
        all_itemsets <- NULL
        
        for (i in 1:6) {

            itemsets <-
                get_itemsets_rare(data, i, support, num_itemsets, 5)
            
            if (is.null(all_itemsets)) {
                all_itemsets <- itemsets
            } else {
                all_itemsets <- rbind(all_itemsets, itemsets)
            }
            
        }
        
        colnames(all_itemsets) <-
            c("diagnosis", "threshold", "item", "support", "frequency")
        all_itemsets$support <- round(all_itemsets$support, 2)
        all_itemsets$diagnosis <-
            paste('Diagnosis', all_itemsets$diagnosis)
        all_itemsets$threshold <-
            paste('Threshold', all_itemsets$threshold)
        
        return(all_itemsets)
    }

extract_state <- function(df) {
    rules_by_id <- transform(df, ID = as.numeric(factor(df$rule)))
    id <- unique(rules_by_id$ID)
    segments <- NULL
    for (k in id) {
        df_sub <- rules_by_id[which(rules_by_id$ID == k), ]
        
        if (nrow(df_sub) == 1) {
            next
        } else {
            if (is.null(segments)) {
                segments <- df_sub
            } else {
                segments <- rbind(segments, df_sub)
            }
        }
    }
    
    
    print(segments)
    return(segments)
}

make_plot_all <- function(df, segments, bmc = FALSE) {
    if (!is.null(segments)) {
        if (bmc) {
            hc <- df %>%
                hchart(
                    'scatter',
                    hcaes(
                        x = support,
                        y = confidence,
                        size = lift,
                        group = diagnose
                    ),
                    maxSize = "10%"
                ) %>%
                hc_tooltip(
                    pointFormat = "{point.threshold} <br>
                    rule: {point.rule} <br>
                    support: {point.x} <br>
                    confdence: {point.y} <br>
                    lift: {point.lift}"
                )
        } else {
            hc <- df %>%
                hchart(
                    'scatter',
                    hcaes(
                        x = support,
                        y = confidence,
                        size = lift,
                        group = threshold
                    ),
                    maxSize = "10%"
                ) %>%
                hc_tooltip(pointFormat = "rule: {point.rule} <br> 
                           support: {point.x} <br> 
                           confdence: {point.y} <br> 
                           lift: {point.lift}")
        }
        hc <- hc  %>%
            hc_add_series(
                data = segments,
                type = "line",
                hcaes(
                    x = support,
                    y = confidence,
                    group = rule
                ),
                enableMouseTracking = FALSE,
                visible = FALSE
            ) %>%
            hc_legend(
                align = "right",
                verticalAlign = "top",
                layout = "vertical",
                x = 0,
                y = 100
            ) %>%
            hc_yAxis(title = list(text = "Confidence", style = list(fontSize = "15px")), 
                     labels = list(style = list(fontSize = "15px")
                                   )) %>%
            hc_xAxis(title = list(text = "Support", style = list(fontSize = "15px")), 
                     labels = list(style = list(fontSize = "15px")
                     ))
    } else {
        if (bmc) {
            format <- "rule: {point.rule} <br> support: {point.x} <br> confdence: {point.y} <br> lift: {point.lift}"
        } else {
            format <- "rule: {point.rule} <br> support: {point.x} <br> confdence: {point.y} <br> lift: {point.lift}"
        }
        
        
        hc <- df %>%
            hchart(
                'scatter',
                hcaes(
                    x = support,
                    y = confidence,
                    size = lift,
                    group = threshold
                ),
                maxSize = "10%"
            ) %>%
            hc_tooltip(pointFormat = format) %>%
            hc_legend(
                align = "right",
                verticalAlign = "top",
                layout = "vertical",
                x = 0,
                y = 100
            ) %>%
            hc_yAxis(title = list(text = "Confidence")) %>%
            hc_xAxis(title = list(text = "Support"))
    }
    
    
    return(hc)
}


frequent_single_itemsets <- function(all_sets, total_count) {
    prob_itemset <- NULL
    df <- data.frame(matrix(nrow = 0, ncol = 6))
    colnames(df) <- c(colnames(all_sets), 'probability', 'p_value')
    
    for (threshold in 2:10) {
        dataset <-
            all_sets[all_sets['threshold'] == paste('Threshold', threshold), ]
        for (i in seq_len(nrow(dataset))) {
            row <- dataset[i, ]
            itemset <-
                unlist(strsplit(str_replace_all(row$item, "[{}]", ""), ", "))
            len <- sapply(strsplit(row$item, ", "), length)
            
            if (len == 0) {
                next
            }
            
            if (len == 1) {
                prob_itemset[itemset] <- row$support
            } else {
                probability <- 1.0
                for (i in 1:len) {
                    probability <-
                        prob_itemset[itemset[i]][[1]] * probability
                }
                
                row$probability <- round(probability, digits = 3)
                row$pvalue <-
                    round(1 - pbinom(row$frequency, total_count, probability),
                          digits = 3)
                row$frequency <-
                    round(row$frequency / total_count, digits = 3)
                
                df <- rbind(df, row)
            }
            # browser()
        }
    }
    
    return(df)
}


make_plot_fis <- function(df) {
    hc <- df %>%
        hchart('scatter',
               hcaes(
                   x = threshold,
                   y = support,
                   size = 1 - pvalue,
                   group = item
               ),
               maxSize = "5%") %>%
        hc_tooltip(pointFormat = "support: {point.y} <br> frequency: {point.frequency} <br> p-value: {point.pvalue}") %>%
        hc_legend(
            align = "right",
            verticalAlign = "top",
            layout = "vertical",
            x = 0,
            y = 100
        )
    return(hc)
}

make_plot_fis_compr <- function(df1, df2) {
    df1['gender'] <- 1
    df2['gender'] <- 2
    
    
    
    df <- merge(df1, df2, by = c("threshold", "item"))
    
    hc <- ggplot(df)  +
        scale_fill_gradient(low = "green", high = "red") +
        scale_shape_manual(name = "Shape", values = c(24, 25)) +
        geom_point(aes(
            x = support.x,
            y = support.y + 0.002,
            fill = -log10(pvalue.x),
            shape = 'group 1'
        ),
        size = 5) +
        geom_point(aes(
            x = support.x,
            y = support.y - 0.002,
            fill = -log10(pvalue.y),
            shape = 'group 2'
        ),
        size = 5) +
        geom_abline(
            intercept = 0,
            slope = 1,
            color = 'blue',
            size = 1.5,
            shape = "diagonal line"
        ) +
        scale_x_continuous(name = "Support of FIS in group 1") +
        scale_y_continuous(name = "Support of FIS in group 2") +
        labs(fill = "-log10(p-value)") +
        geom_label_repel(
            aes(
                x = support.x,
                y = support.y,
                label = paste(item, '\n', threshold)
            ),
            box.padding   = 1,
            segment.size  = 1,
            segment.color = 'black'
        ) +
        theme(
            legend.title = element_text(size = 20),
            legend.text = element_text(size = 15),
            text = element_text(size = 20)
        )
    return(hc)
}

make_plot_state <- function(segments, bmc = FALSE) {
    if (!is.null(segments)) {
        if (bmc) {
            hc_xAxis <- 'Threshold_Diagnosis'
            hc <- segments %>%
                hchart(
                    'line',
                    hcaes(
                        x = thre_diag,
                        y = support,
                        group = rule
                    ),
                    maxSize = "10%",
                    legend = FALSE
                )
        } else {
            hc_xAxis <- 'Threshold'
            hc <- segments %>%
                hchart(
                    'line',
                    hcaes(
                        x = threshold,
                        y = support,
                        group = rule
                    ),
                    maxSize = "10%",
                    legend = FALSE
                )
        }
        hc <- hc %>%
            hc_tooltip(headerFormat = "", pointFormat = "support: {point.y} <br> confdence: {point.confidence} <br> lift: {point.lift}") %>%
            hc_legend(
                align = "right",
                verticalAlign = "top",
                layout = "vertical",
                x = 0,
                y = 100
            ) %>%
            hc_yAxis(title = list(text = "Support", style = list(fontSize = "15px")), 
        labels = list(style = list(fontSize = "12px"))) %>%
            hc_xAxis(title = list(text = hc_xAxis, style = list(fontSize = "15px")),  
                     labels = list(style = list(fontSize = "12px")))
        
        return(hc)
    } else {
        return(NULL)
    }
    
    
}

ui <- fluidPage(
    titlePanel(title = "", windowTitle = "Association Rule Mining"),
    navbarPage(
        "Association Rule Mining",
        tabPanel(
            "Data set",
            # side bar with support, confidence and number of rules.
            sidebarPanel(
                width = 3,
                sliderInput(
                    "slider_supp",
                    label = "Support",
                    min = 0,
                    max = 1,
                    value = 0.5
                ),
                sliderInput(
                    "slider_conf",
                    label = "Confidence",
                    min = 0,
                    max = 1,
                    value = 0.7
                ),
                numericInput(
                    "num_rules",
                    label = "Top X rules",
                    min = 0,
                    value = 5
                ),
                numericInput(
                    "num_itemsets",
                    label = "Top X itemsets",
                    min = 0,
                    value = 10
                ),
                fileInput(
                    "file_visual",
                    "Upload .xlsx file",
                    multiple = FALSE,
                    accept = ".xlsx"
                ),
                uiOutput("selected_input")
            ),
            
            # Main panel with scatter plot.
            mainPanel(
                tabsetPanel(
                    tabPanel(
                        "Resulting Rules in Plot View",
                        highchartOutput("scatter_plot_rules", height = "650px")
                        %>% withSpinner(type = 4, color = "SteelBlue")
                    ),
                    tabPanel("State Diagram", fluidRow(
                        box(
                            h4("Overall State Diagram"),
                            highchartOutput("state_plot")
                            %>% withSpinner(type = 4, color = "SteelBlue")
                        ),
                        box(
                            h4("State Diagram for selected Rule"),
                            plotOutput("rule_support_diagram", brush = 'rule_support_diagram_brush', width = "120%") %>% withSpinner(type = 4, color = "SteelBlue"),
                            tableOutput('test'),
                        )
                    )),
                    tabPanel(
                        "Resulting Rules in Table View",
                        DT::dataTableOutput("table_rule")
                    ),
                    tabPanel(
                        "Resulting Itemsets in Table View",
                        DT::dataTableOutput("table_itemsets")
                    ),
                    tabPanel(
                        "Resulting Amount of Maximum Frequent Itemsets in Table View",
                        DT::dataTableOutput("table_itemset_amount")
                    ),
                    tabPanel("Rule tracking", DT::dataTableOutput("tracking")),
                    tabPanel(
                        "Resulting FIS in Plot View",
                        highchartOutput("scatter_plot_fis", height = "650px")
                        %>% withSpinner(type = 4, color = "SteelBlue")
                    ),
                    tabPanel(
                        "Compared FIS in Plot View",
                        plotOutput("scatter_plot_fis_cmpr", height = "650px")
                        %>% withSpinner(type = 4, color = "SteelBlue")
                    ),
                )
            )
        ),
        tabPanel(
            "Rare disease I",
            sidebarPanel(
                sliderInput(
                    "slider_supp_bmc",
                    label = "Support",
                    min = 0,
                    max = 1,
                    value = 0.45
                ),
                sliderInput(
                    "slider_conf_bmc",
                    label = "Confidence",
                    min = 0,
                    max = 1,
                    value = 0.7
                ),
                numericInput(
                    "num_rules_bmc",
                    label = "Top X rules",
                    min = 0,
                    value = 10
                ),
                numericInput(
                    "num_itemsets_bmc",
                    label = "Top X itemsets",
                    min = 0,
                    value = 10
                ),
                fileInput(
                    "file_bmc",
                    "Upload .xlsx file",
                    multiple = FALSE,
                    accept = ".xlsx"
                ),
                uiOutput("selected_input_bmc")
            ),
            
            # Main panel with scatter plot.
            mainPanel(
                tabsetPanel(
                    tabPanel(
                        "Results in Plot View",
                        highchartOutput("scatter_plot_bmc", height = "650px")
                        %>% withSpinner(type = 4, color = "SteelBlue")
                    ),
                    tabPanel("State Diagram", fluidRow(
                        box(
                            h4("Overall State Diagram"),
                            highchartOutput("state_plot_bmc")
                            %>% withSpinner(type = 4, color = "SteelBlue")
                        ),
                        box(
                            h4("State Diagram for selected Rule"),
                            plotOutput(
                                "rule_support_diagram_bmc",
                                brush = 'rule_support_diagram_bmc_brush',
                                width = "120%"
                            ) %>% withSpinner(type = 4, color = "SteelBlue"),
                            tableOutput('test_bmc'),
                        )
                    )),
                    tabPanel(
                        "Resulting Rules in Table View",
                        DT::dataTableOutput("table_rule_bmc")
                    ),
                    tabPanel(
                        "Resulting Itemsets in Table View",
                        DT::dataTableOutput("table_itemsets_bmc")
                    ),
                    tabPanel(
                        "Resulting Amount of Maximum Frequent Itemsets in Table View",
                        DT::dataTableOutput("table_itemset_amount_bmc")
                    ),
                    tabPanel("Rule tracking", DT::dataTableOutput("tracking_bmc")),
                    tabPanel("Compared AR in table view", tableOutput("tracking_bmc_fis"))
                )
            )
        ),
        
        tabPanel(
            "Rare disease II",
            sidebarPanel(
                sliderInput(
                    "slider_supp_pm",
                    label = "Support",
                    min = 0,
                    max = 1,
                    value = 0.6
                ),
                sliderInput(
                    "slider_conf_pm",
                    label = "Confidence",
                    min = 0,
                    max = 1,
                    value = 0.7
                ),
                numericInput(
                    "num_rules_pm",
                    label = "Top X rules",
                    min = 0,
                    value = 10
                ),
                numericInput(
                    "num_itemsets_pm",
                    label = "Top X itemsets",
                    min = 0,
                    value = 10
                ),
                fileInput(
                    "file_pm",
                    "Upload .csv file",
                    multiple = FALSE,
                    accept = ".csv"
                ),
                uiOutput("selected_input_pm")
            ),
            
            # Main panel with scatter plot.
            mainPanel(
                tabsetPanel(
                    tabPanel(
                        "Results in Plot View",
                        highchartOutput("scatter_plot_pm", height = "650px")
                        %>% withSpinner(type = 4, color = "SteelBlue")
                    ),
                    tabPanel("State Diagram", fluidRow(
                        box(
                            h4("Overall State Diagram"),
                            highchartOutput("state_plot_pm")
                            %>% withSpinner(type = 4, color = "SteelBlue")
                        ),
                        box(
                            h4("State Diagram for selected Rule"),
                            plotOutput(
                                "rule_support_diagram_pm",
                                brush = 'rule_support_diagram_pm_brush',
                                width = "120%"
                            ) %>% withSpinner(type = 4, color = "SteelBlue"),
                            tableOutput('test_pm'),
                        )
                    )),
                    tabPanel(
                        "Resulting Rules in Table View",
                        DT::dataTableOutput("table_rule_pm")
                    ),
                    tabPanel(
                        "Resulting Itemsets in Table View",
                        DT::dataTableOutput("table_itemsets_pm")
                    ),
                    tabPanel(
                        "Resulting Amount of Maximum Frequent Itemsets in Table View",
                        DT::dataTableOutput("table_itemset_amount_pm")
                    ),
                    tabPanel("Rule tracking", DT::dataTableOutput("tracking_pm")),
                    tabPanel("Maximum FIS", tableOutput("tracking_pm_fis"))
                )
            )
        )
    )
)


# Define server logic to read selected file ----
server <- function(input, output) {
    output$scatter_plot_rules <- renderHighchart({
        req(input$file_visual)
        
        tryCatch({
            input_data <- read_excel(input$file_visual$datapath)
        }, error = function(e) {
            # return a safeError if a parsing error occurs
            stop(safeError(e))
        })
        
        support <- input$slider_supp
        confidence <- input$slider_conf
        num_rules <- input$num_rules
        
        data.symp <- handle_data(input_data)
        all_rules <-
            extract_rules(data.symp, support, confidence, num_rules)
        multiple_segments <- extract_state(all_rules)
        
        g <- make_plot_all(all_rules, multiple_segments)
        return(g)
    })
    
    output$scatter_plot_fis <- renderHighchart({
        req(input$file_visual)
        
        tryCatch({
            input_data <- read_excel(input$file_visual$datapath)
        }, error = function(e) {
            # return a safeError if a parsing error occurs
            stop(safeError(e))
        })
        
        support <- input$slider_supp
        num_itemsets <- input$num_itemsets
        
        data.symp <- handle_data(input_data)
        all_sets <-
            extract_freq_itemsets(data.symp, support, num_itemsets)
        
        pvalue <-
            frequent_single_itemsets(all_sets, nrow(data.symp))
        
        hc_fis <- make_plot_fis(pvalue)
        return(hc_fis)
    })
    
    output$scatter_plot_fis_cmpr <- renderPlot({
        req(input$file_visual)
        
        tryCatch({
            input_data <- read_excel(input$file_visual$datapath)
        }, error = function(e) {
            # return a safeError if a parsing error occurs
            stop(safeError(e))
        })
        
        support <- input$slider_supp
        num_itemsets <- input$num_itemsets
        
        data.symp <- handle_data(input_data, TRUE)
        
        dat.symp_1 <-
            data.symp[which(data.symp[, "Gender_A"] == 1), 1:27]
        all_sets_1 <-
            extract_freq_itemsets(dat.symp_1, support, num_itemsets)
        pvalue.1 <-
            frequent_single_itemsets(all_sets_1, nrow(dat.symp_1))
        dat.symp_2 <-
            data.symp[which(data.symp[, "Gender_A"] == 2), 1:27]
        all_sets_2 <-
            extract_freq_itemsets(dat.symp_2, support, num_itemsets)
        pvalue.2 <-
            frequent_single_itemsets(all_sets_2, nrow(dat.symp_2))
        
        hc_fis <- make_plot_fis_compr(pvalue.1, pvalue.2)
        return(hc_fis)
    })
    
    output$state_plot <- renderHighchart({
        req(input$file_visual)
        
        tryCatch({
            input_data <- read_excel(input$file_visual$datapath)
        }, error = function(e) {
            # return a safeError if a parsing error occurs
            stop(safeError(e))
        })
        
        support <- input$slider_supp
        confidence <- input$slider_conf
        num_rules <- input$num_rules
        
        data.symp <- handle_data(input_data)
        all_rules <-
            extract_rules(data.symp, support, confidence, num_rules)
        multiple_segments <- extract_state(all_rules)
        hc_state <- make_plot_state(multiple_segments)
        return(hc_state)
    })
    
    output$selected_input <- renderUI({
        req(input$file_visual)
        
        tryCatch({
            input_data <- read_excel(input$file_visual$datapath)
        }, error = function(e) {
            # return a safeError if a parsing error occurs
            stop(safeError(e))
        })
        
        support <- input$slider_supp
        confidence <- input$slider_conf
        num_rules <- input$num_rules
        
        data.symp <- handle_data(input_data)
        all_rules <-
            extract_rules(data.symp, support, confidence, num_rules)
        
        multiple_segments <- extract_state(all_rules)
        return(selectInput(
            'sel_rule',
            label = 'Observable Rule: ',
            choices = unique(multiple_segments$rule)
        ))
    })
    
    customer <- reactive({
        req(input$sel_rule)
    })
    
    observeEvent(customer(), {
        selected_rule <- input$sel_rule
        
        tryCatch({
            input_data <- read_excel(input$file_visual$datapath)
        }, error = function(e) {
            # return a safeError if a parsing error occurs
            stop(safeError(e))
        })
        dat.symp <- handle_data(input_data)
        
        df <-
            data.frame(
                fis = character(),
                support = double(),
                frequency = integer(),
                threshold = character(),
                stringsAsFactors = FALSE
            )
        selected_rule <-
            str_split(str_remove_all(selected_rule, "[{}]"), "=>")
        
        lhs <- selected_rule[[1]][1]
        rhs <- selected_rule[[1]][2]
        
        for (threshold in 2:10) {
            bin.df <- as.data.frame(dat.symp >= threshold)
            n <- nrow(bin.df)
            
            if (lhs == '') {
                sub.df <- bin.df[which(bin.df[rhs] == 1), ]
                fis <- paste0('{', rhs, '}')
            } else if (rhs == '') {
                sub.df <- bin.df[which(bin.df[lhs] == 1), ]
                fis <- paste0('{', lhs, '}')
            } else {
                sub.df <- bin.df[which(bin.df[lhs] == 1 & bin.df[rhs] == 1), ]
                fis <- paste0('{', lhs, ', ', rhs, '}')
            }
            
            count <- nrow(sub.df)
            support <- round(count / n, 2)
            threshold <- paste('Threshold', threshold)
            
            df <- rbind(df, c(fis, support, count, threshold))
        }
        
        colnames(df) <-
            c('FIS', 'Support', 'Frequency', 'Threshold')
        output$rule_support_diagram <- renderPlot({
            return(
                ggplot(df, aes(Threshold, Support)) +
                    geom_point() +
                    theme_minimal() +
                    theme(
                        axis.title = element_text(size = 15),
                        axis.text.x = element_text(angle = 45, hjust = 1),
                        axis.text = element_text(size = 12),
                    )
            )
        })
        output$test <- renderTable({
            brushedPoints(df, input$rule_support_diagram_brush)
        })
    })
    
    output$table_rule <- DT::renderDataTable({
        req(input$file_visual)
        
        tryCatch({
            input_data <- read_excel(input$file_visual$datapath)
        }, error = function(e) {
            # return a safeError if a parsing error occurs
            stop(safeError(e))
        })
        
        support <- input$slider_supp
        confidence <- input$slider_conf
        num_rules <- input$num_rules
        
        data.symp <- handle_data(input_data)
        all_rules <-
            extract_rules(data.symp, support, confidence, num_rules)
        
        my_vars <-
            c("threshold",
              "support",
              "confidence",
              "lift",
              "frequency")
        
        clean_rules <- all_rules[my_vars]
        
        clean_rules["rule"] <-
            paste0(all_rules$lhs, all_rules$notation, all_rules$rhs)
        
        return(datatable(
            clean_rules,
            colnames = c(
                'Threshold',
                'Rule',
                'Support',
                'Confidence',
                'Lift',
                'Frequency'
            ),
            rownames = FALSE
        ))
    })
    
    output$table_itemsets <- DT::renderDataTable({
        req(input$file_visual)
        
        tryCatch({
            input_data <- read_excel(input$file_visual$datapath)
        }, error = function(e) {
            # return a safeError if a parsing error occurs
            stop(safeError(e))
        })
        
        support <- input$slider_supp
        num_itemsets <- input$num_itemsets
        
        data.symp <- handle_data(input_data)
        all_sets <-
            extract_freq_itemsets(data.symp, support, num_itemsets)
        
        return(datatable(all_sets, rownames = FALSE))
        
    })
    
    output$table_itemset_amount <- DT::renderDataTable({
        req(input$file_visual)
        
        tryCatch({
            input_data <- read_excel(input$file_visual$datapath)
        }, error = function(e) {
            # return a safeError if a parsing error occurs
            stop(safeError(e))
        })
        
        support <- input$slider_supp
        
        data.symp <- handle_data(input_data)
        all_sets <- extract_freq_itemsets_amount(data.symp, support)
        
        return(datatable(all_sets, rownames = FALSE))
    })
    
    output$tracking <- DT::renderDataTable({
        req(input$file_visual)
        
        tryCatch({
            input_data <- read_excel(input$file_visual$datapath)
        }, error = function(e) {
            # return a safeError if a parsing error occurs
            stop(safeError(e))
        })
        
        support <- input$slider_supp
        confidence <- input$slider_conf
        num_rules <- input$num_rules
        
        data.symp <- handle_data(input_data)
        all_rules <-
            extract_rules(data.symp, support, confidence, num_rules)
        
        multiple_segments <- extract_state(all_rules)
        multiple_segments <-
            multiple_segments[, c('threshold',
                                  'rule',
                                  'support',
                                  'confidence',
                                  'lift',
                                  'frequency')]
        
        return(datatable(
            multiple_segments,
            colnames = c(
                'Threshold',
                'Rule',
                'Support',
                'Confidence',
                'Lift',
                'Frequency'
            ),
            rownames = FALSE
        ))
    })
    
    
    output$scatter_plot_bmc <- renderHighchart({
        req(input$file_bmc)
        
        tryCatch({
            input_data <- read_excel(input$file_bmc$datapath)
        }, error = function(e) {
            # return a safeError if a parsing error occurs
            stop(safeError(e))
        })
        
        support <- input$slider_supp_bmc
        confidence <- input$slider_conf_bmc
        num_rules <- input$num_rules_bmc
        
        data_factor <- input_data[grep("^q", names(input_data))]
        
        data_factor$Diagnose <- input_data$diagnosis
        
        all_rules <-
            extract_rules_rare(data_factor, support, confidence, num_rules, 3)
        
        multiple_segments <- extract_state(all_rules)
        
        g <- make_plot_all(all_rules, multiple_segments, bmc = TRUE)
        
        return(g)
    })
    
    output$state_plot_bmc <- renderHighchart({
        req(input$file_bmc)
        
        tryCatch({
            input_data <- read_excel(input$file_bmc$datapath)
        }, error = function(e) {
            # return a safeError if a parsing error occurs
            stop(safeError(e))
        })
        
        support <- input$slider_supp_bmc
        confidence <- input$slider_conf_bmc
        num_rules <- input$num_rules_bmc
        
        data_factor <- input_data[grep("^q", names(input_data))]
        
        data_factor$Diagnose <- input_data$diagnosis
        
        all_rules <-
            extract_rules_rare(data_factor, support, confidence, num_rules, 7)
        
        multiple_segments <- extract_state(all_rules)
        
        hc_state <- make_plot_state(multiple_segments, TRUE)
        
        return(hc_state)
    })
    
    output$selected_input_bmc <- renderUI({
        req(input$file_bmc)
        
        tryCatch({
            input_data <- read_excel(input$file_bmc$datapath)
        }, error = function(e) {
            # return a safeError if a parsing error occurs
            stop(safeError(e))
        })
        
        support <- input$slider_supp_bmc
        confidence <- input$slider_conf_bmc
        num_rules <- input$num_rules_bmc
        
        data_factor <- input_data[grep("^q", names(input_data))]
        
        data_factor$Diagnose <- input_data$diagnosis
        
        all_rules <-
            extract_rules_rare(data_factor, support, confidence, num_rules, 7)
        
        multiple_segments <- extract_state(all_rules)
        
        return(selectInput(
            'sel_rule_bmc',
            label = 'Observable Rule: ',
            choices = unique(multiple_segments$rule)
        ))
    })
    
    customer_2 <- reactive({
        req(input$sel_rule_bmc)
    })
    
    observeEvent(customer_2(), {
        selected_rule <- input$sel_rule_bmc
        
        tryCatch({
            input_data <- read_excel(input$file_bmc$datapath)
        }, error = function(e) {
            # return a safeError if a parsing error occurs
            stop(safeError(e))
        })
        data_factor <- input_data[grep("^q", names(input_data))]
        
        data_factor$Diagnose <- input_data$diagnosis
        
        df <-
            data.frame(
                fis = character(),
                support = double(),
                frequency = integer(),
                diagnosis = character(),
                stringsAsFactors = FALSE
            )
        selected_rule <-
            str_split(str_remove_all(selected_rule, "[{}]"), "=>")
        
        lhs <- selected_rule[[1]][1]
        rhs <- selected_rule[[1]][2]

        for (diagnosis in 1:6) {
            diag.df <- data_factor[which(data_factor$Diagnose == diagnosis), ]
            n <- nrow(diag.df)
            for (threshold in 2:5) {
                bin.df <- as.data.frame(diag.df >= threshold)
                
                if (lhs == '') {
                    sub.df <- bin.df[which(bin.df[rhs] == 1), ]
                    fis <- paste0('{', rhs, '}')
                } else if (rhs == '') {
                    sub.df <- bin.df[which(bin.df[lhs] == 1), ]
                    fis <- paste0('{', lhs, '}')
                } else {
                    sub.df <- bin.df[which(bin.df[lhs] == 1 & bin.df[rhs] == 1), ]
                    fis <- paste0('{', lhs, ', ', rhs, '}')
                }
                
                count <- nrow(sub.df)
                support <- round(count / n, 2)
                threshold <- paste('Threshold', threshold)
                diagnose <- paste('Diagnosis ', diagnosis)
                thre_diag <- paste0(threshold, ', Diagnosis ', diagnosis)
                
                df <- rbind(df,
                            c(
                                fis,
                                support,
                                count,
                                threshold,
                                diagnose,
                                thre_diag
                            ))
            }
        }
        
        colnames(df) <-
            c(
                'FIS',
                'Support',
                'Frequency',
                'Threshold',
                'Diagnosis',
                'Threshold_Diagnosis'
            )
        
        df <- df[which(df$Frequency != 0), ]
        
        output$rule_support_diagram_bmc <- renderPlot({
            return(
                ggplot(df, aes(
                    Threshold_Diagnosis, Support
                )) +
                    geom_point() +
                    theme_minimal() +
                    theme(
                        axis.title = element_text(size = 15),
                        axis.text.x = element_text(angle = 45, hjust = 1),
                        axis.text = element_text(size = 12),
                    ) +
                    xlab("Threshold & Diagnosis")
            )
        })
        
        output$test_bmc <- renderTable({
            brushedPoints(df[, c('FIS',
                                 'Support',
                                 'Frequency',
                                 'Threshold_Diagnosis')], input$rule_support_diagram_bmc_brush)
        })
    })
    
    output$table_rule_bmc <- DT::renderDataTable({
        req(input$file_bmc)
        
        tryCatch({
            input_data <- read_excel(input$file_bmc$datapath)
        }, error = function(e) {
            # return a safeError if a parsing error occurs
            stop(safeError(e))
        })
        
        support <- input$slider_supp_bmc
        confidence <- input$slider_conf_bmc
        num_rules <- input$num_rules_bmc
        
        data_factor <- input_data[grep("^q", names(input_data))]
        
        data_factor$Diagnose <- input_data$diagnosis
        
        all_rules <-
            extract_rules_rare(data_factor, support, confidence, num_rules, 7)
        
        my_vars <-
            c("threshold",
              "diagnose",
              "support",
              "confidence",
              "lift",
              "frequency")
        
        clean_rules <- all_rules[my_vars]
        
        clean_rules["rule"] <-
            paste0(all_rules$lhs, all_rules$notation, all_rules$rhs)
        
        return(datatable(
            clean_rules,
            colnames = c(
                "Threshold",
                "Diagnose",
                "Support",
                "Confidence",
                "Lift",
                "Frequency"
            ),
            rownames = FALSE
        ))
    })
    
    output$table_itemset_amount_bmc <- DT::renderDataTable({
        req(input$file_bmc)
        
        tryCatch({
            input_data <- read_excel(input$file_bmc$datapath)
        }, error = function(e) {
            # return a safeError if a parsing error occurs
            stop(safeError(e))
        })
        
        support <- input$slider_supp_bmc
        
        data_factor <- input_data[grep("^q", names(input_data))]
        data_factor$Diagnose <- input_data$diagnosis
        
        all_sets <-
            extract_freq_itemsets_amount_rare(data_factor, support, 7)
        
        return(datatable(all_sets, rownames = FALSE))
    })
    
    output$table_itemsets_bmc <- DT::renderDataTable({
        req(input$file_bmc)
        
        tryCatch({
            input_data <- read_excel(input$file_bmc$datapath)
        }, error = function(e) {
            # return a safeError if a parsing error occurs
            stop(safeError(e))
        })
        
        support <- input$slider_supp_bmc
        num_itemsets <- input$num_itemsets_bmc
        
        data_factor <- input_data[grep("^q", names(input_data))]
        data_factor$Diagnose <- input_data$diagnosis
        
        all_sets <-
            extract_freq_itemsets_rare(data_factor, support, 'bmc', num_itemsets)
        
        return(datatable(all_sets, rownames = FALSE))
    })
    
    output$tracking_bmc <- DT::renderDataTable({
        req(input$file_bmc)
        
        tryCatch({
            input_data <- read_excel(input$file_bmc$datapath)
        }, error = function(e) {
            # return a safeError if a parsing error occurs
            stop(safeError(e))
        })
        
        support <- input$slider_supp_bmc
        confidence <- input$slider_conf_bmc
        num_rules <- input$num_rules_bmc
        
        data_factor <- input_data[grep("^q", names(input_data))]
        
        data_factor$Diagnose <- input_data$diagnosis
        
        all_rules <-
            extract_rules_rare(data_factor, support, confidence, num_rules, 7)
        
        multiple_segments <- extract_state(all_rules)
        
        multiple_segments <-
            multiple_segments[, c('diagnose',
                                  'threshold',
                                  'rule',
                                  'support',
                                  'confidence',
                                  'lift',
                                  'frequency')]
        # names(multiple_segments)[names(multiple_segments) == "threshold"] <-
        #     "Diagnosis"
        
        return(datatable(
            multiple_segments,
            colnames = c('Diagnosis',
                         'Threshold',
                         'Rule',
                         'Support',
                         'Confidence',
                         'Lift',
                         'Frequency'), 
            rownames = FALSE
        ))
    })
    
    output$scatter_plot_pm <- renderHighchart({
        req(input$file_pm)
        
        tryCatch({
            input_data <- read.csv(input$file_pm$datapath, sep = ';')
        }, error = function(e) {
            # return a safeError if a parsing error occurs
            stop(safeError(e))
        })
        
        support <- input$slider_supp_pm
        confidence <- input$slider_conf_pm
        num_rules <- input$num_rules_pm
        
        data_factor <- input_data[grep("^Aus", names(input_data))]
        
        data_factor$Diagnose <- input_data$Diagnose
        
        all_rules <-
            extract_rules_rare(data_factor, support, confidence, num_rules, 6)
        
        multiple_segments <- extract_state(all_rules)
        
        g <- make_plot_all(all_rules, multiple_segments)
        return(g)
    })
    
    output$state_plot_pm <- renderHighchart({
        req(input$file_pm)
        
        tryCatch({
            input_data <- read.csv(input$file_pm$datapath, sep = ';')
        }, error = function(e) {
            # return a safeError if a parsing error occurs
            stop(safeError(e))
        })
        
        support <- input$slider_supp_pm
        confidence <- input$slider_conf_pm
        num_rules <- input$num_rules_pm
        
        data_factor <- input_data[grep("^Aus", names(input_data))]
        
        data_factor$Diagnose <- input_data$Diagnose
        
        all_rules <-
            extract_rules_rare(data_factor, support, confidence, num_rules, 6)
        
        multiple_segments <- extract_state(all_rules)
        
        hc_state <- make_plot_state(multiple_segments)
        
        return(hc_state)
    })
    
    output$selected_input_pm <- renderUI({
        req(input$file_pm)
        
        tryCatch({
            input_data <- read.csv(input$file_pm$datapath, sep = ';')
        }, error = function(e) {
            # return a safeError if a parsing error occurs
            stop(safeError(e))
        })
        
        support <- input$slider_supp_pm
        confidence <- input$slider_conf_pm
        num_rules <- input$num_rules_pm
        
        data_factor <- input_data[grep("^Aus", names(input_data))]
        
        data_factor$Diagnose <- input_data$Diagnose
        
        all_rules <-
            extract_rules_rare(data_factor, support, confidence, num_rules, 6)
        
        multiple_segments <- extract_state(all_rules)
        
        return(selectInput(
            'sel_rule_pm',
            label = 'Observable Rule: ',
            choices = unique(multiple_segments$rule)
        ))
    })
    
    customer_3 <- reactive({
        req(input$sel_rule_pm)
    })
    
    observeEvent(customer_3(), {
        selected_rule <- input$sel_rule_pm
        
        tryCatch({
            input_data <- read.csv(input$file_pm$datapath, sep = ';')
        }, error = function(e) {
            # return a safeError if a parsing error occurs
            stop(safeError(e))
        })
        data_factor <- input_data[grep("^Aus", names(input_data))]
        
        data_factor$Diagnose <- input_data$Diagnose
        
        df <-
            data.frame(
                fis = character(),
                support = double(),
                frequency = integer(),
                diagnosis = character(),
                stringsAsFactors = FALSE
            )
        selected_rule <-
            str_split(str_remove_all(selected_rule, "[{}]"), "=>")
        
        lhs <- selected_rule[[1]][1]
        if (lhs != '') {
            lhs_split <- str_split(lhs, "-")
        }
        
        rhs <- selected_rule[[1]][2]
        if (rhs != '') {
            rhs_split <- str_split(rhs, "-")
        }
        
        for (threshold in 1:7) {
            bin.df <- data_factor[which(data_factor$Diagnose == threshold), ]
            n <- nrow(bin.df)
            
            if (lhs == '') {
                sub.df <-
                    bin.df[which(bin.df[rhs_split[[1]][1]] == rhs_split[[1]][2]), ]
                fis <- paste0('{', rhs, '}')
            } else if (rhs == '') {
                sub.df <-
                    bin.df[which(bin.df[lhs_split[[1]][1]] == lhs_split[[1]][2]), ]
                fis <- paste0('{', lhs, '}')
            } else {
                sub.df <-
                    bin.df[which(bin.df[lhs_split[[1]][1]] == lhs_split[[1]][2] &
                                     bin.df[rhs_split[[1]][1]] == rhs_split[[1]][2]), ]
                fis <- paste0('{', lhs, ', ', rhs, '}')
            }
            
            count <- nrow(sub.df)
            support <- round(count / n, 2)
            threshold <- paste('Diagnosis', threshold)
            
            df <- rbind(df, c(fis, support, count, threshold))
        }
        
        colnames(df) <-
            c('FIS', 'Support', 'Frequency', 'Diagnosis')
        output$rule_support_diagram_pm <- renderPlot({
            return(
                ggplot(df, aes(Diagnosis, Support)) +
                    geom_point() +
                    theme_minimal() +
                    theme(
                        axis.text.x = element_text(angle = 45, hjust = 1),
                        axis.text = element_text(size = 12),
                    )
            )
        })
        output$test_pm <- renderTable({
            brushedPoints(df, input$rule_support_diagram_pm_brush)
        })
    })
    
    output$table_rule_pm <- DT::renderDataTable({
        req(input$file_pm)
        
        tryCatch({
            input_data <- read.csv(input$file_pm$datapath, sep = ';')
        }, error = function(e) {
            # return a safeError if a parsing error occurs
            stop(safeError(e))
        })
        
        support <- input$slider_supp_pm
        confidence <- input$slider_conf_pm
        num_rules <- input$num_rules_pm
        
        data_factor <- input_data[grep("^Aus", names(input_data))]
        
        data_factor$Diagnose <- input_data$Diagnose
        
        all_rules <-
            extract_rules_rare(data_factor, support, confidence, num_rules, 6)
        
        my_vars <-
            c("threshold",
              "support",
              "confidence",
              "lift",
              "frequency")
        
        clean_rules <- all_rules[my_vars]
        
        clean_rules["rule"] <-
            paste0(all_rules$lhs, all_rules$notation, all_rules$rhs)
        
        return(datatable(
            clean_rules,
            colnames =
                c(
                    "Threshold",
                    "Support",
                    "Confidence",
                    "Lift",
                    "Frequency"
                ),
            rownames = FALSE
        ))
    })
    
    output$table_itemsets_pm <- DT::renderDataTable({
        req(input$file_pm)
        
        tryCatch({
            input_data <- read.csv(input$file_pm$datapath, sep = ';')
        }, error = function(e) {
            # return a safeError if a parsing error occurs
            stop(safeError(e))
        })
        
        support <- input$slider_supp_pm
        num_itemsets <- input$num_itemsets_pm
        
        data_factor <- input_data[grep("^Aus", names(input_data))]
        data_factor$Diagnose <- input_data$Diagnose
        
        all_sets <-
            extract_freq_itemsets_rare(data_factor, support, 'pm', num_itemsets)
        
        return(datatable(all_sets, rownames = FALSE))
    })
    
    output$table_itemset_amount_pm <- DT::renderDataTable({
        req(input$file_pm)
        
        tryCatch({
            input_data <- read.csv(input$file_pm$datapath, sep = ';')
        }, error = function(e) {
            # return a safeError if a parsing error occurs
            stop(safeError(e))
        })
        
        support <- input$slider_supp_pm
        
        data_factor <- input_data[grep("^Aus", names(input_data))]
        data_factor$Diagnose <- input_data$Diagnose
        
        all_sets <-
            extract_freq_itemsets_amount_rare(data_factor, support, 'pm')
        
        return(datatable(all_sets, rownames = FALSE))
    })
    
    output$tracking_pm <- DT::renderDataTable({
        req(input$file_pm)
        
        tryCatch({
            input_data <- read.csv(input$file_pm$datapath, sep = ';')
        }, error = function(e) {
            # return a safeError if a parsing error occurs
            stop(safeError(e))
        })
        
        support <- input$slider_supp_pm
        confidence <- input$slider_conf_pm
        num_rules <- input$num_rules_pm
        
        data_factor <- input_data[grep("^Aus", names(input_data))]
        
        data_factor$Diagnose <- input_data$Diagnose
        
        all_rules <-
            extract_rules_rare(data_factor, support, confidence, num_rules, 6)
        
        multiple_segments <- extract_state(all_rules)
        
        multiple_segments <-
            multiple_segments[, c('threshold',
                                  'rule',
                                  'support',
                                  'confidence',
                                  'lift',
                                  'frequency')]
        names(multiple_segments)[names(multiple_segments) == "threshold"] <-
            "Diagnosis"
        
        return(datatable(multiple_segments, rownames = FALSE))
    })
    
    output$tracking_bmc_fis <- renderTable({
        req(input$file_bmc)
        
        tryCatch({
            input_data <- read_excel(input$file_bmc$datapath)
        }, error = function(e) {
            # return a safeError if a parsing error occurs
            stop(safeError(e))
        })
        
        support <- input$slider_supp_bmc
        confidence <- input$slider_conf_bmc
        num_rules <- input$num_rules_bmc
        num_itemsets <- input$num_itemsets_bmc
        
        data_factor <- input_data[grep("^q", names(input_data))]
        
        data_factor$Diagnose <- input_data$diagnosis
        
        all_rules <-
            extract_rules_rare(data_factor, support, confidence, num_rules, 7)
        
        multiple_segments <- extract_state(all_rules)
        
        unique_rules <- unique(multiple_segments$rule)
        
        
        tab <- data.frame(
            "Rule" = NULL,
            "Diagnosis I (Frequency, Support, Confidence, Lift)" = NULL,
            "Diagnosis II (Frequency, Support, Confidence, Lift)" = NULL,
            "P-value by Fisher's Test(confidence interval)" = NULL
        )
        
        for (rule in unique_rules) {
            temp_rules_set <- multiple_segments[multiple_segments$rule == rule, ]
            
            for (i in 1:(nrow(temp_rules_set) - 1)) {
                j <- i + 1
                
                diagnosis_1 <- temp_rules_set$diagnose[i]
                diagnosis_2 <- temp_rules_set$diagnose[j]
                threshold_1 <- str_replace_all(temp_rules_set$threshold[i], 'Threshold ', '')
                threshold_2 <- str_replace_all(temp_rules_set$threshold[j], 'Threshold ', '')
                
                data_sub_1 <- data_factor[which(paste('Diagnosis', data_factor$Diagnose) == diagnosis_1),]
                data_sub_2 <- data_factor[which(paste('Diagnosis', data_factor$Diagnose) == diagnosis_2),]
                
                bin.data_1 <- data.frame(data_sub_1 >= threshold_1)
                bin.data_2 <- data.frame(data_sub_2 >= threshold_2)
                
                lhs <- unlist(strsplit(str_replace_all(temp_rules_set$lhs[i], "[{}]", ""), ', '))
                rhs <- unlist(strsplit(str_replace_all(temp_rules_set$rhs[i], "[{}]", ""), ', '))
                
                print(lhs)
                print(rhs)
                print(bin.data_1)
                print(bin.data_2)
                
                
                bin.data_1['sum_lhs'] <- rowSums(bin.data_1[lhs]) == length(lhs)
                amount_1 <- sum(bin.data_1['sum_lhs'])
                bin.data_2['sum_lhs'] <- rowSums(bin.data_2[unlist(lhs)]) == length(lhs)
                amount_2 <- sum(bin.data_2['sum_lhs'])
                
                lrhs <- c(lhs, rhs)
                bin.data_1['sum_lrhs'] <- rowSums(bin.data_1[lrhs]) == length(lrhs)
                count_1 <- sum((bin.data_1['sum_lrhs']))
                bin.data_2['sum_lrhs'] <- rowSums(bin.data_2[lrhs]) == length(lrhs)
                count_2 <- sum((bin.data_2['sum_lrhs']))
                
                print(amount_1)
                print(count_1)
                print(amount_2)
                print(count_2)
            
                
                fisher_matrix <-
                    matrix(c(
                        count_1,
                        amount_1 - count_1,
                        count_2,
                        amount_2 - count_2
                    ),
                    nrow = 2)
                
                fisher_value <- fisher.test(fisher_matrix)
                
                tab <- rbind(tab,
                             c(
                                 rule,
                                 paste0(
                                     diagnosis_1,
                                     ' (',
                                     count_1,
                                     ', ',
                                     temp_rules_set$support[i],
                                     ', ',
                                     temp_rules_set$confidence[i],
                                     ', ',
                                     temp_rules_set$lift[i],
                                     ')'
                                 ),
                                 paste0(
                                     diagnosis_2,
                                     ' (',
                                     count_2,
                                     ', ',
                                     temp_rules_set$support[j],
                                     ', ',
                                     temp_rules_set$confidence[j],
                                     ', ',
                                     temp_rules_set$lift[j],
                                     ')'
                                 ),
                                 paste0(
                                     round(fisher_value$p.value, 3),
                                     ' (',
                                     round(fisher_value$conf.int[1], 3),
                                     ', ',
                                     round(fisher_value$conf.int[2], 3),
                                     ')'
                                 )
                             ))
            }
        }
        
        colnames(tab) <- c(
            "Rule",
            "Diagnosis I (Frequency, Support, Confidence, Lift)",
            "Diagnosis II (Frequency, Support, Confidence, Lift)",
            "P-value by Fisher's Test(confidence interval)"
        )
        
        return(tab)
    })
    
    output$tracking_pm_fis <- renderTable({
        req(input$file_pm)
        
        tryCatch({
            input_data <- read.csv(input$file_pm$datapath, sep = ';')
        }, error = function(e) {
            # return a safeError if a parsing error occurs
            stop(safeError(e))
        })
        
        support <- input$slider_supp_pm
        confidence <- input$slider_conf_pm
        num_rules <- input$num_rules_pm
        num_itemsets <- input$num_itemsets_pm
        
        data_factor <- input_data[grep("^Aus", names(input_data))]
        
        data_factor$Diagnose <- input_data$Diagnose
        
        all_sets <-
            extract_freq_itemsets_rare(data_factor, support, 'pm', num_itemsets)
        
        all_rules <-
            extract_rules_rare(data_factor, support, confidence, num_rules, 6)
        
        multiple_segments <- extract_state(all_rules)
        
        unique_rules <- unique(multiple_segments$rule)
        
        tab <- data.frame(
            "Rule" = NULL,
            "Diagnosis(support confidence)" = NULL,
            "FIS Diagnosis I" = NULL,
            "Diagnosis(support confidence)" = NULL,
            "FIS Diagnosis II" = NULL,
            "P-value by Fisher's Test(confidence interval)" = NULL
        )

        for (rule in unique_rules) {
            temp_rules_set <- multiple_segments[multiple_segments$rule == rule, ]
            
            for (i in 1:(nrow(temp_rules_set) - 1)) {
                j <- i + 1
                count_1 <- temp_rules_set$frequency[i]
                count_2 <- temp_rules_set$frequency[j]
                
                diagnosis_1 <- temp_rules_set$threshold[i]
                diagnosis_2 <- temp_rules_set$threshold[j]
                
                lhs_1 <- temp_rules_set$lhs[i]
                lhs_2 <- temp_rules_set$lhs[j]
                rhs_1 <- temp_rules_set$rhs[i]
                rhs_2 <- temp_rules_set$rhs[j]
                
                temp_set_1 <- all_sets[all_sets$diagnosis == diagnosis_1, ]
                temp_set_2 <- all_sets[all_sets$diagnosis == diagnosis_2, ]
                
                amount_1 <- 0
                amount_2 <- 0
                
                for (a in 1:(nrow(temp_set_1) - 1)) {
                    if (grepl(str_replace_all(lhs_1, "[{}]", ""),
                              temp_set_1$item[a])) {
                        amount_1 <- amount_1 + temp_set_1$frequency[a]
                    }
                }
                
                for (a in 1:(nrow(temp_set_2) - 1)) {
                    if (grepl(str_replace_all(lhs_2, "[{}]", ""),
                              temp_set_2$item[a])) {
                        amount_2 <- amount_2 + temp_set_2$frequency[a]
                    }
                }
                
                fisher_matrix <-
                    matrix(c(
                        count_1,
                        amount_1 - count_1,
                        count_2,
                        amount_2 - count_2
                    ),
                    nrow = 2)
                
                fisher_value <- fisher.test(fisher_matrix)
                
                tab <- rbind(tab,
                             c(
                                 rule,
                                 paste0(
                                     diagnosis_1,
                                     ' (',
                                     temp_rules_set$support[i],
                                     ', ',
                                     temp_rules_set$confidence[i],
                                     ', ',
                                     temp_rules_set$lift[i],
                                     ')'
                                 ),
                                 count_1,
                                 paste0(
                                     diagnosis_2,
                                     ' (',
                                     temp_rules_set$support[j],
                                     ', ',
                                     temp_rules_set$confidence[j],
                                     ', ',
                                     temp_rules_set$lift[j],
                                     ')'
                                 ),
                                 count_2,
                                 paste0(
                                     round(fisher_value$p.value, 3),
                                     ' (',
                                     round(fisher_value$conf.int[1], 3),
                                     ', ',
                                     round(fisher_value$conf.int[2], 3),
                                     ')'
                                 )
                             ))
            }
        }
        
        colnames(tab) <- c(
            "Rule",
            "Diagnosis I (Support, Confidence, Lift)",
            "Frequency Maximum FIS Diagnosis I",
            "Diagnosis II (Support, Confidence, Lift)",
            "Frequency Maximum FIS Diagnosis II",
            "P-value by Fisher's Test(confidence interval)"
        )
        return(tab)
    })
}

shinyApp(ui, server)
