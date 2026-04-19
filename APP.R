library(readxl)
library(arules)
library(arulesViz)
library(highcharter)
library(stringr)
library(shinycssloaders)
library(shiny)
library(shinydashboard)
library(stringr)
library(ggplot2)
library(DT)
library(dplyr)
library(stats)

mat2itemlists_rare <- function(mat) {
    res <- NULL
    for (i in seq_len(nrow(mat))) {
        item <- mat[i,]
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

mat2itemlists <- function(mat) {
    res <- c()
    items <- colnames(mat)
    for (i in seq_len(nrow(mat))) {
        xi <- items[mat[i,] == 1]

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
        }

        res <- c(res, si)
    }
    return(res)
}

get_rules <- function(data, threshold, parameter_supp, parameter_conf, num_rules, max_len = 10) {
    bin.mat <- data >= threshold
    item.lists <- mat2itemlists(bin.mat)
    retail.list <- strsplit(item.lists, " ")
    retail.trans <- as(retail.list, "transactions")
    retail.rules <- apriori(retail.trans, parameter = list(supp = parameter_supp, conf = parameter_conf, target = "rules", maxlen = max_len))
    ### Select the best 5 rules w.r.t. lift

    if (length(retail.rules) == 0) {
        return(NULL)
    } else {
        retail.rules <- retail.rules[!is.redundant(retail.rules, measure = "confidence", confint = TRUE, level = 0.95)]

        if (length(retail.rules) >= 5) {
            num_rules <- 5
        } else {
            num_rules <- length(retail.rules)
        }

        retail.li5 <- head(sort(retail.rules, by = "lift"), num_rules)

        retail.li5 <- inspect(retail.li5, linebreak = FALSE)
        retail.li5["threshold"] <- threshold

        return(retail.li5)
    }
}

get_rules_rare <- function(data, threshold, parameter_supp, parameter_conf, num_rules, max_len = 10) {
    data_sub <- data[which(data$Diagnose == threshold),]
    data_sub <- data_sub[, 1:(ncol(data_sub) - 1)]
    item.lists <- mat2itemlists_rare(data_sub)
    retail.list <- strsplit(item.lists, " ")
    retail.trans <- as(retail.list, "transactions")
    retail.rules <- apriori(retail.trans, parameter = list(supp = parameter_supp, conf = parameter_conf, target = "rules", maxlen = max_len))
    # retail.sets <- apriori(retail.trans, parameter = list(support = parameter_supp, target = "frequent itemsets"))
    if (length(retail.rules) == 0) {
        return(NULL)
    } else {
        retail.rules <- retail.rules[!is.redundant(retail.rules, measure = "confidence", confint = TRUE, level = 0.95)]
        if (length(retail.rules) >= num_rules) {
            num_rules <- num_rules
        } else {
            num_rules <- length(retail.rules)
        }

        retail.li5 <- head(sort(retail.rules, by = "lift"), num_rules)

        retail.li5 <- inspect(retail.li5, linebreak = FALSE)
        retail.li5["threshold"] <- threshold

        return(retail.li5)
    }
}

get_itemsets_amount <- function(data, threshold, parameter_supp) {
    bin.mat <- data >= threshold
    item.lists <- mat2itemlists(bin.mat)
    retail.list <- strsplit(item.lists, " ")
    retail.trans <- as(retail.list, "transactions")

    retail.sets <- apriori(retail.trans, parameter = list(support = parameter_supp, target = "frequent itemsets"))

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

get_itemsets <- function(data, threshold, parameter_supp, num_itemsets) {
    bin.mat <- data >= threshold
    item.lists <- mat2itemlists(bin.mat)
    retail.list <- strsplit(item.lists, " ")
    retail.trans <- as(retail.list, "transactions")

    retail.sets <- apriori(retail.trans, parameter = list(support = parameter_supp, target = "frequent itemsets"))

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

        supp_list <- sort(unique(retail.itemsets$support), decreasing = TRUE)[1:num_itemsets]

        sets <- NULL
        for (supp in supp_list) {
            if (is.null(sets)) {
                sets <- retail.itemsets[which(retail.itemsets$support == supp),]
            } else {
                sets <- rbind(sets, retail.itemsets[which(retail.itemsets$support == supp),])
            }
        }
        # retail.itemsets <- top_n(retail.itemsets, num_itemsets, support)

        retail.item.summary <- data.frame(Threshold, sets)

        return(retail.item.summary)
    }
}

get_itemsets_amount_rare <- function(data, threshold, parameter_supp, max_len = 10) {
    data_sub <- data[which(data$Diagnose == threshold),]
    data_sub <- data_sub[, 1:(ncol(data_sub) - 1)]
    item.lists <- mat2itemlists_rare(data_sub)
    retail.list <- strsplit(item.lists, " ")
    retail.trans <- as(retail.list, "transactions")

    retail.sets <- apriori(retail.trans, parameter = list(support = parameter_supp, target = "frequent itemsets", maxlen = max_len))

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

get_itemsets_rare <- function(data, threshold, parameter_supp, num_itemsets, max_len = 10) {
    data_sub <- data[which(data$Diagnose == threshold),]
    data_sub <- data_sub[, 1:(ncol(data_sub) - 1)]
    item.lists <- mat2itemlists_rare(data_sub)
    retail.list <- strsplit(item.lists, " ")
    retail.trans <- as(retail.list, "transactions")

    retail.sets <- apriori(retail.trans, parameter = list(support = parameter_supp, target = "frequent itemsets", maxlen = max_len))

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

        supp_list <- sort(unique(retail.itemsets$support), decreasing = TRUE)[1:num_itemsets]

        sets <- NULL
        for (supp in supp_list) {
            if (is.null(sets)) {
                sets <- retail.itemsets[which(retail.itemsets$support == supp),]
            } else {
                sets <- rbind(sets, retail.itemsets[which(retail.itemsets$support == supp),])
            }
        }

        retail.item.summary <- data.frame(Threshold, sets)

        return(retail.item.summary)
    }
}

handle_data <- function(data) {
    data <- data %>% rename(
        SY09_01 = LoCoDyspnoe_A,
        SY09_02 = LoCoHusten_A,
        SY09_03 = LoCoPalpi_A,
        SY09_04 = LoCoAngina_A,
        SY09_05 = LoCoChestpain_A,
        SY09_06 = LoCoFatigue_A,
        SY09_07 = LoCoFieber_A,
        SY09_08 = LoCoPain_A,
        SY09_09 = LoCoConcentrate_A,
        SY09_10 = LoCoHeadache_A,
        SY09_11 = LoCoSleep_A,
        SY09_12 = LoCoKribbel_A,
        SY09_13 = LoCoSchwindel_A,
        SY09_14 = LoCoDelir_A,
        SY09_15 = LoCoBauch_A,
        SY09_16 = LoCoUebel_A,
        SY09_17 = LoCoDurchfall_A,
        SY09_18 = LoCoAppetit_A,
        SY09_19 = LoCoJointpain_A,
        SY09_20 = LoCoMusclepain_A,
        SY09_21 = LoCoDepressiv_A,
        SY09_22 = LoCoAngst_A,
        SY09_23 = LoCoTinnitus_A,
        SY09_24 = LoCoEarpain_A,
        SY09_25 = LoCoHals_A,
        SY09_26 = LoCoSmell_A,
        SY09_27 = LoCoAusschlag_A
    )

    data.finished <- data[!is.na(data[, "Group_A"]),]

    #remove duplciate rows based on patcode
    # all participants
    data.all <- data.finished[!duplicated(data.finished$patcode),]

    #long covid
    data.longcovid <- data.all[data.all$Group_A == 1,]

    dat <- data.longcovid

    #only symptoms data
    if (ncol(dat) > 20) {
        dat.symp <- dat[, 28:54]
        # dat.symp <- dat[0:126, c(10:45, 72)]
    } else {
        dat.symp <- dat
    }

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
    colnames(all_rules) <- c("lhs", "notation", "rhs", "support", "confidence", "coverage", "lift", "frequency", "threshold")
    all_rules["rule"] <- paste0(all_rules$lhs, all_rules$notation, all_rules$rhs)
    all_rules$support <- round(all_rules$support, 2)
    all_rules$confidence <- round(all_rules$confidence, 2)
    all_rules$lift <- round(all_rules$lift, 2)
    all_rules$threshold <- paste('Threshold ', all_rules$threshold)

    return(all_rules)
}

extract_rules_rare <- function(data, support, confidence, num_rules, n) {
    all_rules <- NULL

    if (n == 6) {
        rules <- get_rules_rare(data, 7, 0.1, confidence, num_rules, 3)
        all_rules <- rules
    }

    for (i in 1:n) {
        if (n == 6) {
            max_len <- c(10, 10, 10, 10, 7, 7)
            rules <- get_rules_rare(data, i, support, confidence, num_rules, max_len[i])
        } else {
            rules <- get_rules_rare(data, i, support, confidence, num_rules)

        }


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
    colnames(all_rules) <- c("lhs", "notation", "rhs", "support", "confidence", "coverage", "lift", "frequency", "threshold")
    all_rules["rule"] <- paste0(all_rules$lhs, all_rules$notation, all_rules$rhs)
    all_rules$support <- round(all_rules$support, 2)
    all_rules$confidence <- round(all_rules$confidence, 2)
    all_rules$lift <- round(all_rules$lift, 2)
    all_rules$threshold <- paste('Diagnosis ', all_rules$threshold)

    print(all_rules)

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

extract_freq_itemsets_amount_rare <- function(data, support, label) {
    all_itemsets <- NULL

    for (i in 1:7) {
        if (label == 'pm') {
            max_len <- c(10, 10, 10, 4, 4, 10, 3)
        } else {
            max_len <- c(10, 10, 10, 10, 10, 10, 3)
        }

        itemsets <- get_itemsets_amount_rare(data, i, support, max_len[i])


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

    colnames(all_itemsets) <- c("threshold", "item", "support", "frequency")
    all_itemsets$support <- round(all_itemsets$support, 3)
    all_itemsets$threshold <- paste('Threshold ', all_itemsets$threshold)

    return(all_itemsets)
}

extract_freq_itemsets_rare <- function(data, support, label, num_itemsets) {
    all_itemsets <- NULL

    for (i in 1:7) {
        if (label == 'pm') {
            max_len <- c(10, 10, 10, 4, 4, 10, 3)
        } else {
            max_len <- c(10, 10, 10, 10, 10, 10, 3)
        }
        itemsets <- get_itemsets_rare(data, i, support, num_itemsets, max_len[i])


        if (is.null(all_itemsets)) {
            all_itemsets <- itemsets
        } else {
            all_itemsets <- rbind(all_itemsets, itemsets)
        }

    }

    colnames(all_itemsets) <- c("threshold", "item", "support", "frequeny")
    all_itemsets$support <- round(all_itemsets$support, 2)
    all_itemsets$threshold <- paste('Threshold ', all_itemsets$threshold)

    return(all_itemsets)
}

extract_state <- function(df) {
    rules_by_id <- transform(df, ID = as.numeric(factor(df$rule)))
    id <- unique(rules_by_id$ID)
    segments <- NULL
    for (k in id) {
        df_sub <- rules_by_id[which(rules_by_id$ID == k),]

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

    return(segments)
}

make_plot_all <- function(df, segments) {
    if (!is.null(segments)) {
        hc <- df %>%
            hchart('scatter', hcaes(x = support, y = confidence, size = lift, group = threshold), maxSize = "10%") %>%
            hc_tooltip(pointFormat = "support: {point.x} <br> confdence: {point.y} <br> lift: {point.lift}") %>%
            hc_add_series(data = segments, type = "line", hcaes(x = support, y = confidence, group = rule),
                          enableMouseTracking = FALSE, visible = FALSE) %>%
            hc_legend(align = "right", verticalAlign = "top",
                      layout = "vertical", x = 0, y = 100)
    } else {
        hc <- df %>%
            hchart('scatter', hcaes(x = support, y = confidence, size = lift, group = threshold), maxSize = "10%") %>%
            hc_tooltip(pointFormat = "support: {point.x} <br> confdence: {point.y} <br> lift: {point.lift}") %>%
            hc_legend(align = "right", verticalAlign = "top",
                      layout = "vertical", x = 0, y = 100)
    }


    return(hc)
}


frequent_single_itemsets <- function(all_sets, total_count) {
    prob_itemset <- NULL
    df <- data.frame(matrix(nrow = 0, ncol = 6))
    colnames(df) <- c(colnames(all_sets), 'probability', 'p_value')

    for (threshold in 2:10) {
        dataset <- all_sets[all_sets['threshold'] == paste('Threshold ', threshold),]
        for (i in seq_len(nrow(dataset))) {
            row <- dataset[i,]
            itemset <- unlist(strsplit(str_replace_all(row$item, "[{}]", ""), ", "))
            len <- sapply(strsplit(row$item, ", "), length)

            if (len == 0) {
                next
            }

            if (len == 1) {
                prob_itemset[itemset] <- row$support
            } else {
                probability <- 1.0
                for (i in 1:len) {
                    a <- prob_itemset[itemset[i]]
                    probability <- prob_itemset[itemset[i]][[1]] * probability
                }
                
                row$probability <- round(probability, digits = 3)
                row$pvalue <- round(1 - pbinom(row$frequency, total_count, probability), digits = 3)
                row$frequency <- round(row$frequency / total_count, digits = 3)

                df <- rbind(df, row)
            }
            # browser()
        }
    }

    return(df)
}


make_plot_fis <- function(df) {
    hc <- df %>%
        hchart('scatter', hcaes(x = threshold, y = support, size = 1 - pvalue, group=item), maxSize = "5%") %>%
        hc_tooltip(pointFormat = "support: {point.y} <br> frequency: {point.frequency} <br> p-value: {point.pvalue}") %>%
        hc_legend(align = "right", verticalAlign = "top",
                  layout = "vertical", x = 0, y = 100)
    return(hc)
}


make_plot_state <- function(segments) {

    if (!is.null(segments)) {
        hc <- segments %>%
            hchart('line', hcaes(x = threshold, y = support, group = rule), maxSize = "10%", legend = FALSE) %>%
            hc_tooltip(headerFormat = "", pointFormat = "support: {point.y} <br> confdence: {point.confidence} <br> lift: {point.lift}") %>%
            hc_legend(align = "right", verticalAlign = "top",
                      layout = "vertical", x = 0, y = 100)

        return(hc)
    } else {
        return(NULL)
    }


}

ui <- fluidPage(
    titlePanel(
        title = "",
        windowTitle = "Association Rule Mining"
    ),
    navbarPage(
        "Association Rule Mining",
        tabPanel(
            "Data set",
            # side bar with support, confidence and number of rules.
            sidebarPanel(width = 3,
                         sliderInput("slider_supp", label = "support", min = 0, max = 1, value = 0.5),
                         sliderInput("slider_conf", label = "confidence", min = 0, max = 1, value = 0.7),
                         numericInput("num_rules", label = "Top X rules", min = 0, value = 5),
                         numericInput("num_itemsets", label = "Top X itemsets", min = 0, value = 10),
                         fileInput("file_visual", "Upload .xlsx file",
                                   multiple = FALSE,
                                   accept = ".xlsx"),
                         uiOutput("selected_input")
            ),

            # Main panel with scatter plot.
            mainPanel(
                tabsetPanel(
                    tabPanel("Resulting Rules in Plot View", highchartOutput("scatter_plot_rules", height = "650px")
                        %>% withSpinner(type = 4, color = "SteelBlue")),
                    tabPanel("State Diagram",
                             fluidRow(
                                 box(h4("Overall State Diagram"),
                                     highchartOutput("state_plot")
                                         %>% withSpinner(type = 4, color = "SteelBlue")),
                                 box(h4("Stata Diagram for selected Rule"),
                                     plotOutput("rule_support_diagram", brush = 'rule_support_diagram_brush',
                                                width = "120%") %>% withSpinner(type = 4, color = "SteelBlue"),
                                     tableOutput('test'),
                                 )
                             )
                    ),
                    tabPanel("Resulting Rules in Table View", DT::dataTableOutput("table_rule")),
                    tabPanel("Resulting Itemsets in Table View", DT::dataTableOutput("table_itemsets")),
                    tabPanel("Resulting Amount of Maximum Frequent Itemsets in Table View",
                             DT::dataTableOutput("table_itemset_amount")),
                    tabPanel("Rule tracking", DT::dataTableOutput("tracking")),
                    tabPanel("Resulting FIS in Plot View", highchartOutput("scatter_plot_fis", height = "650px")
                        %>% withSpinner(type = 4, color = "SteelBlue")),
                )
            )
        ),
        tabPanel(
            "BMC Data",
            sidebarPanel(
                sliderInput("slider_supp_bmc", label = "support", min = 0, max = 1, value = 0.4),
                sliderInput("slider_conf_bmc", label = "confidence", min = 0, max = 1, value = 0.7),
                numericInput("num_rules_bmc", label = "Top X rules", min = 0, value = 10),
                numericInput("num_itemsets_bmc", label = "Top X itemsets", min = 0, value = 10),
                fileInput("file_bmc", "Upload .xlsx file",
                          multiple = FALSE,
                          accept = ".xlsx"),
                uiOutput("selected_input_bmc")
            ),

            # Main panel with scatter plot.
            mainPanel(
                tabsetPanel(
                    tabPanel("Results in Plot View", highchartOutput("scatter_plot_bmc", height = "650px")
                        %>% withSpinner(type = 4, color = "SteelBlue")),
                    tabPanel("State Diagram",
                             fluidRow(
                                 box(h4("Overall State Diagram"),
                                     highchartOutput("state_plot_bmc")
                                         %>% withSpinner(type = 4, color = "SteelBlue")),
                                 box(h4("Stata Diagram for selected Rule"),
                                     plotOutput("rule_support_diagram_bmc", brush = 'rule_support_diagram_bmc_brush',
                                                width = "120%") %>% withSpinner(type = 4, color = "SteelBlue"),
                                     tableOutput('test_bmc'),
                                 )
                             )
                    ),
                    tabPanel("Resulting Rules in Table View", DT::dataTableOutput("table_rule_bmc")),
                    tabPanel("Resulting Itemsets in Table View", DT::dataTableOutput("table_itemsets_bmc")),
                    tabPanel("Resulting Amount of Maximum Frequent Itemsets in Table View",
                             DT::dataTableOutput("table_itemset_amount_bmc")),
                    tabPanel("Rule tracking", DT::dataTableOutput("tracking_bmc")),
                    tabPanel("Maximum FIS", tableOutput("tracking_bmc_fis"))
                )
            )
        ),

        tabPanel(
            "PM Data",
            sidebarPanel(
                sliderInput("slider_supp_pm", label = "support", min = 0, max = 1, value = 0.6),
                sliderInput("slider_conf_pm", label = "confidence", min = 0, max = 1, value = 0.7),
                numericInput("num_rules_pm", label = "Top X rules", min = 0, value = 10),
                numericInput("num_itemsets_pm", label = "Top X itemsets", min = 0, value = 10),
                fileInput("file_pm", "Upload .csv file",
                          multiple = FALSE,
                          accept = ".csv"),
                uiOutput("selected_input_pm")
            ),

            # Main panel with scatter plot.
            mainPanel(
                tabsetPanel(
                    tabPanel("Results in Plot View", highchartOutput("scatter_plot_pm", height = "650px")
                        %>% withSpinner(type = 4, color = "SteelBlue")),
                    tabPanel("State Diagram",
                             fluidRow(
                                 box(h4("Overall State Diagram"),
                                     highchartOutput("state_plot_pm")
                                         %>% withSpinner(type = 4, color = "SteelBlue")),
                                 box(h4("Stata Diagram for selected Rule"),
                                     plotOutput("rule_support_diagram_pm", brush = 'rule_support_diagram_pm_brush',
                                                width = "120%") %>% withSpinner(type = 4, color = "SteelBlue"),
                                     tableOutput('test_pm'),
                                 )
                             )
                    ),
                    tabPanel("Resulting Rules in Table View", DT::dataTableOutput("table_rule_pm")),
                    tabPanel("Resulting Itemsets in Table View", DT::dataTableOutput("table_itemsets_pm")),
                    tabPanel("Resulting Amount of Maximum Frequent Itemsets in Table View",
                             DT::dataTableOutput("table_itemset_amount_pm")),
                    tabPanel("Rule tracking", DT::dataTableOutput("tracking_pm")),
                    tabPanel("Maximum FIS", tableOutput("tracking_pm_fis")
                    )
                )
            )
        )
    )
)


# Define server logic to read selected file ----
server <- function(input, output) {

    output$scatter_plot_rules <- renderHighchart({
        req(input$file_visual)

        tryCatch(
        {
            input_data <- read_excel(input$file_visual$datapath)
        },
            error = function(e) {
                # return a safeError if a parsing error occurs
                stop(safeError(e))
            }
        )

        support <- input$slider_supp
        confidence <- input$slider_conf
        num_rules <- input$num_rules

        data.symp <- handle_data(input_data)
        all_rules <- extract_rules(data.symp, support, confidence, num_rules)
        multiple_segments <- extract_state(all_rules)
        g <- make_plot_all(all_rules, multiple_segments)
        return(g)
    }
    )

    output$scatter_plot_fis <- renderHighchart({
        req(input$file_visual)

        tryCatch(
        {
            input_data <- read_excel(input$file_visual$datapath)
        },
            error = function(e) {
                # return a safeError if a parsing error occurs
                stop(safeError(e))
            }
        )

        support <- input$slider_supp
        num_itemsets <- input$num_itemsets

        data.symp <- handle_data(input_data)
        all_sets <- extract_freq_itemsets(data.symp, support, num_itemsets)

        pvalue <- frequent_single_itemsets(all_sets, nrow(data.symp))

        hc_fis <- make_plot_fis(pvalue)
        return(hc_fis)
    }
)

    output$state_plot <- renderHighchart({
        req(input$file_visual)

        tryCatch(
        {
            input_data <- read_excel(input$file_visual$datapath)
        },
            error = function(e) {
                # return a safeError if a parsing error occurs
                stop(safeError(e))
            }
        )

        support <- input$slider_supp
        confidence <- input$slider_conf
        num_rules <- input$num_rules

        data.symp <- handle_data(input_data)
        all_rules <- extract_rules(data.symp, support, confidence, num_rules)
        multiple_segments <- extract_state(all_rules)
        hc_state <- make_plot_state(multiple_segments)
        return(hc_state)
    })

    output$selected_input <- renderUI({
        req(input$file_visual)

        tryCatch(
        {
            input_data <- read_excel(input$file_visual$datapath)
        },
            error = function(e) {
                # return a safeError if a parsing error occurs
                stop(safeError(e))
            }
        )

        support <- input$slider_supp
        confidence <- input$slider_conf
        num_rules <- input$num_rules

        data.symp <- handle_data(input_data)
        all_rules <- extract_rules(data.symp, support, confidence, num_rules)

        multiple_segments <- extract_state(all_rules)
        return(selectInput('sel_rule', label = 'Observable Rule: ', choices = unique(multiple_segments$rule)))
    })

    customer <- reactive({
        req(input$sel_rule)
    })

    observeEvent(customer(), {
        selected_rule <- input$sel_rule

        tryCatch(
        {
            input_data <- read_excel(input$file_visual$datapath)
        },
            error = function(e) {
                # return a safeError if a parsing error occurs
                stop(safeError(e))
            }
        )
        dat.symp <- handle_data(input_data)

        df <- data.frame(fis = character(), support = double(), frequency = integer(), threshold = character(), stringsAsFactors = FALSE)
        selected_rule <- str_split(str_remove_all(selected_rule, "[{}]"), "=>")

        lhs <- selected_rule[[1]][1]
        # if (lhs != '') {
        #     if (nchar(lhs) == 1) {
        #         lhs <- paste0('SY09_0', lhs)
        #     } else {
        #         lhs <- paste0('SY09_', lhs)
        #     }
        # }

        rhs <- selected_rule[[1]][2]
        # if (rhs != '') {
        #     if (nchar(rhs) == 1) {
        #         rhs <- paste0('SY09_0', rhs)
        #     } else {
        #         rhs <- paste0('SY09_', rhs)
        #     }
        # }

        for (threshold in 2:10) {
            bin.df <- as.data.frame(dat.symp >= threshold)
            n <- nrow(bin.df)

            if (lhs == '') {
                sub.df <- bin.df[which(bin.df[rhs] == 1),]
                fis <- paste0('{', rhs, '}')
            } else if (rhs == '') {
                sub.df <- bin.df[which(bin.df[lhs] == 1),]
                fis <- paste0('{', lhs, '}')
            } else {
                sub.df <- bin.df[which(bin.df[lhs] == 1 & bin.df[rhs] == 1),]
                fis <- paste0('{', lhs, ', ', rhs, '}')
            }

            count <- nrow(sub.df)
            support <- round(count / n, 2)
            threshold <- paste0('threshold ', threshold)

            df <- rbind(df, c(fis, support, count, threshold))
        }

        colnames(df) <- c('FIS', 'support', 'frequency', 'threshold')
        output$rule_support_diagram <- renderPlot({
            return(ggplot(df, aes(threshold, support)) +
                       geom_point() +
                       theme_minimal() +
                       theme(axis.text.x = element_text(angle = 45, hjust = 1),
                             axis.text = element_text(size = 12),
                       ))
        })
        output$test <- renderTable({
            brushedPoints(df, input$rule_support_diagram_brush)
        })
    })

    output$table_rule <- DT::renderDataTable({
        req(input$file_visual)

        tryCatch(
        {
            input_data <- read_excel(input$file_visual$datapath)
        },
            error = function(e) {
                # return a safeError if a parsing error occurs
                stop(safeError(e))
            }
        )

        support <- input$slider_supp
        confidence <- input$slider_conf
        num_rules <- input$num_rules

        data.symp <- handle_data(input_data)
        all_rules <- extract_rules(data.symp, support, confidence, num_rules)

        my_vars <- c("threshold", "support", "confidence", "lift", "frequency")

        clean_rules <- all_rules[my_vars]

        clean_rules["rule"] <- paste0(all_rules$lhs, all_rules$notation, all_rules$rhs)

        return(datatable(clean_rules, rownames = FALSE))
    })

    output$table_itemsets <- DT::renderDataTable({
        req(input$file_visual)

        tryCatch(
        {
            input_data <- read_excel(input$file_visual$datapath)
        },
            error = function(e) {
                # return a safeError if a parsing error occurs
                stop(safeError(e))
            }
        )

        support <- input$slider_supp
        num_itemsets <- input$num_itemsets

        data.symp <- handle_data(input_data)
        all_sets <- extract_freq_itemsets(data.symp, support, num_itemsets)

        return(datatable(all_sets, rownames = FALSE))

    })

    output$table_itemset_amount <- DT::renderDataTable({
        req(input$file_visual)

        tryCatch(
        {
            input_data <- read_excel(input$file_visual$datapath)
        },
            error = function(e) {
                # return a safeError if a parsing error occurs
                stop(safeError(e))
            }
        )

        support <- input$slider_supp

        data.symp <- handle_data(input_data)
        all_sets <- extract_freq_itemsets_amount(data.symp, support)

        return(datatable(all_sets, rownames = FALSE))
    })

    output$tracking <- DT::renderDataTable({
        req(input$file_visual)

        tryCatch(
        {
            input_data <- read_excel(input$file_visual$datapath)
        },
            error = function(e) {
                # return a safeError if a parsing error occurs
                stop(safeError(e))
            }
        )

        support <- input$slider_supp
        confidence <- input$slider_conf
        num_rules <- input$num_rules

        data.symp <- handle_data(input_data)
        all_rules <- extract_rules(data.symp, support, confidence, num_rules)

        multiple_segments <- extract_state(all_rules)
        multiple_segments <- multiple_segments[, c('threshold', 'rule', 'support', 'confidence', 'lift', 'frequency')]

        return(datatable(multiple_segments, rownames = FALSE))
    })


    output$scatter_plot_bmc <- renderHighchart({
        req(input$file_bmc)

        tryCatch(
        {
            input_data <- read_excel(input$file_bmc$datapath)
        },
            error = function(e) {
                # return a safeError if a parsing error occurs
                stop(safeError(e))
            }
        )

        support <- input$slider_supp_bmc
        confidence <- input$slider_conf_bmc
        num_rules <- input$num_rules_bmc

        data_factor <- input_data[grep("^q", names(input_data))]

        data_factor$Diagnose <- input_data$diagnosis

        all_rules <- extract_rules_rare(data_factor, support, confidence, num_rules, 7)

        multiple_segments <- extract_state(all_rules)

        g <- make_plot_all(all_rules, multiple_segments)
        return(g)
    })

    output$state_plot_bmc <- renderHighchart({
        req(input$file_bmc)

        tryCatch(
        {
            input_data <- read_excel(input$file_bmc$datapath)
        },
            error = function(e) {
                # return a safeError if a parsing error occurs
                stop(safeError(e))
            }
        )

        support <- input$slider_supp_bmc
        confidence <- input$slider_conf_bmc
        num_rules <- input$num_rules_bmc

        data_factor <- input_data[grep("^q", names(input_data))]

        data_factor$Diagnose <- input_data$diagnosis

        all_rules <- extract_rules_rare(data_factor, support, confidence, num_rules, 7)

        multiple_segments <- extract_state(all_rules)

        hc_state <- make_plot_state(multiple_segments)

        return(hc_state)
    })

    output$selected_input_bmc <- renderUI({
        req(input$file_bmc)

        tryCatch(
        {
            input_data <- read_excel(input$file_bmc$datapath)
        },
            error = function(e) {
                # return a safeError if a parsing error occurs
                stop(safeError(e))
            }
        )

        support <- input$slider_supp_bmc
        confidence <- input$slider_conf_bmc
        num_rules <- input$num_rules_bmc

        data_factor <- input_data[grep("^q", names(input_data))]

        data_factor$Diagnose <- input_data$diagnosis

        all_rules <- extract_rules_rare(data_factor, support, confidence, num_rules, 7)

        multiple_segments <- extract_state(all_rules)

        return(selectInput('sel_rule_bmc', label = 'Observable Rule: ', choices = unique(multiple_segments$rule)))
    })

    customer_2 <- reactive({
        req(input$sel_rule_bmc)
    })

    observeEvent(customer_2(), {
        selected_rule <- input$sel_rule_bmc

        tryCatch(
        {
            input_data <- read_excel(input$file_bmc$datapath)
        },
            error = function(e) {
                # return a safeError if a parsing error occurs
                stop(safeError(e))
            }
        )
        data_factor <- input_data[grep("^q", names(input_data))]

        data_factor$Diagnose <- input_data$diagnosis

        df <- data.frame(fis = character(), support = double(), frequency = integer(), diagnosis = character(), stringsAsFactors = FALSE)
        selected_rule <- str_split(str_remove_all(selected_rule, "[{}]"), "=>")

        lhs <- selected_rule[[1]][1]
        if (lhs != '') {
            lhs_split <- str_split(lhs, "-")
        }

        rhs <- selected_rule[[1]][2]
        if (rhs != '') {
            rhs_split <- str_split(rhs, "-")
        }

        for (threshold in 1:7) {
            bin.df <- data_factor[which(data_factor$Diagnose == threshold),]
            n <- nrow(bin.df)

            if (lhs == '') {
                sub.df <- bin.df[which(bin.df[rhs_split[[1]][1]] == rhs_split[[1]][2]),]
                fis <- paste0('{', rhs, '}')
            } else if (rhs == '') {
                sub.df <- bin.df[which(bin.df[lhs_split[[1]][1]] == lhs_split[[1]][2]),]
                fis <- paste0('{', lhs, '}')
            } else {
                sub.df <- bin.df[which(bin.df[lhs_split[[1]][1]] == lhs_split[[1]][2] &
                                           bin.df[rhs_split[[1]][1]] == rhs_split[[1]][2]),]
                fis <- paste0('{', lhs, ', ', rhs, '}')
            }

            count <- nrow(sub.df)
            support <- round(count / n, 2)
            diagnosis <- paste0('Diagnose ', threshold)

            df <- rbind(df, c(fis, support, count, diagnosis))
        }

        colnames(df) <- c('FIS', 'support', 'frequency', 'diagnosis')
        output$rule_support_diagram_bmc <- renderPlot({
            return(ggplot(df, aes(diagnosis, support)) +
                       geom_point() +
                       theme_minimal() +
                       theme(axis.text.x = element_text(angle = 45, hjust = 1),
                             axis.text = element_text(size = 12),
                       ))
        })
        output$test_bmc <- renderTable({
            brushedPoints(df, input$rule_support_diagram_bmc_brush)
        })
    })

    output$table_rule_bmc <- DT::renderDataTable({
        req(input$file_bmc)

        tryCatch(
        {
            input_data <- read_excel(input$file_bmc$datapath)
        },
            error = function(e) {
                # return a safeError if a parsing error occurs
                stop(safeError(e))
            }
        )

        support <- input$slider_supp_bmc
        confidence <- input$slider_conf_bmc
        num_rules <- input$num_rules_bmc

        data_factor <- input_data[grep("^q", names(input_data))]

        data_factor$Diagnose <- input_data$diagnosis

        all_rules <- extract_rules_rare(data_factor, support, confidence, num_rules, 7)

        my_vars <- c("threshold", "support", "confidence", "lift", "frequency")

        clean_rules <- all_rules[my_vars]

        clean_rules["rule"] <- paste0(all_rules$lhs, all_rules$notation, all_rules$rhs)

        return(datatable(clean_rules, rownames = FALSE))
    })

    output$table_itemset_amount_bmc <- DT::renderDataTable({
        req(input$file_bmc)

        tryCatch(
        {
            input_data <- read_excel(input$file_bmc$datapath)
        },
            error = function(e) {
                # return a safeError if a parsing error occurs
                stop(safeError(e))
            }
        )

        support <- input$slider_supp_bmc

        data_factor <- input_data[grep("^q", names(input_data))]
        data_factor$Diagnose <- input_data$diagnosis

        all_sets <- extract_freq_itemsets_amount_rare(data_factor, support, 7)

        return(datatable(all_sets, rownames = FALSE))
    })

    output$table_itemsets_bmc <- DT::renderDataTable({
        req(input$file_bmc)

        tryCatch(
        {
            input_data <- read_excel(input$file_bmc$datapath)
        },
            error = function(e) {
                # return a safeError if a parsing error occurs
                stop(safeError(e))
            }
        )

        support <- input$slider_supp_bmc
        num_itemsets <- input$num_itemsets_bmc

        data_factor <- input_data[grep("^q", names(input_data))]
        data_factor$Diagnose <- input_data$diagnosis

        all_sets <- extract_freq_itemsets_rare(data_factor, support, 'bmc', num_itemsets)

        return(datatable(all_sets, rownames = FALSE))
    })

    output$tracking_bmc <- DT::renderDataTable({
        req(input$file_bmc)

        tryCatch(
        {
            input_data <- read_excel(input$file_bmc$datapath)
        },
            error = function(e) {
                # return a safeError if a parsing error occurs
                stop(safeError(e))
            }
        )

        support <- input$slider_supp_bmc
        confidence <- input$slider_conf_bmc
        num_rules <- input$num_rules_bmc

        data_factor <- input_data[grep("^q", names(input_data))]

        data_factor$Diagnose <- input_data$diagnosis

        all_rules <- extract_rules_rare(data_factor, support, confidence, num_rules, 7)

        multiple_segments <- extract_state(all_rules)

        multiple_segments <- multiple_segments[, c('threshold', 'rule', 'support', 'confidence', 'lift', 'frequency')]
        names(multiple_segments)[names(multiple_segments) == "threshold"] <- "Diagnosis"

        return(datatable(multiple_segments, rownames = FALSE))
    })

    output$scatter_plot_pm <- renderHighchart({
        req(input$file_pm)

        tryCatch(
        {
            input_data <- read.csv(input$file_pm$datapath, sep = ';')
        },
            error = function(e) {
                # return a safeError if a parsing error occurs
                stop(safeError(e))
            }
        )

        support <- input$slider_supp_pm
        confidence <- input$slider_conf_pm
        num_rules <- input$num_rules_pm

        data_factor <- input_data[grep("^Aus", names(input_data))]

        data_factor$Diagnose <- input_data$Diagnose

        all_rules <- extract_rules_rare(data_factor, support, confidence, num_rules, 6)

        multiple_segments <- extract_state(all_rules)

        g <- make_plot_all(all_rules, multiple_segments)
        return(g)
    })

    output$state_plot_pm <- renderHighchart({
        req(input$file_pm)

        tryCatch(
        {
            input_data <- read.csv(input$file_pm$datapath, sep = ';')
        },
            error = function(e) {
                # return a safeError if a parsing error occurs
                stop(safeError(e))
            }
        )

        support <- input$slider_supp_pm
        confidence <- input$slider_conf_pm
        num_rules <- input$num_rules_pm

        data_factor <- input_data[grep("^Aus", names(input_data))]

        data_factor$Diagnose <- input_data$Diagnose

        all_rules <- extract_rules_rare(data_factor, support, confidence, num_rules, 6)

        multiple_segments <- extract_state(all_rules)

        hc_state <- make_plot_state(multiple_segments)

        return(hc_state)
    })

    output$selected_input_pm <- renderUI({
        req(input$file_pm)

        tryCatch(
        {
            input_data <- read.csv(input$file_pm$datapath, sep = ';')
        },
            error = function(e) {
                # return a safeError if a parsing error occurs
                stop(safeError(e))
            }
        )

        support <- input$slider_supp_pm
        confidence <- input$slider_conf_pm
        num_rules <- input$num_rules_pm

        data_factor <- input_data[grep("^Aus", names(input_data))]

        data_factor$Diagnose <- input_data$Diagnose

        all_rules <- extract_rules_rare(data_factor, support, confidence, num_rules, 6)

        multiple_segments <- extract_state(all_rules)

        return(selectInput('sel_rule_pm', label = 'Observable Rule: ', choices = unique(multiple_segments$rule)))
    })

    customer_3 <- reactive({
        req(input$sel_rule_pm)
    })

    observeEvent(customer_3(), {
        selected_rule <- input$sel_rule_pm

        tryCatch(
        {
            input_data <- read.csv(input$file_pm$datapath, sep = ';')
        },
            error = function(e) {
                # return a safeError if a parsing error occurs
                stop(safeError(e))
            }
        )
        data_factor <- input_data[grep("^Aus", names(input_data))]

        data_factor$Diagnose <- input_data$Diagnose

        df <- data.frame(fis = character(), support = double(), frequency = integer(), diagnosis = character(), stringsAsFactors = FALSE)
        selected_rule <- str_split(str_remove_all(selected_rule, "[{}]"), "=>")

        lhs <- selected_rule[[1]][1]
        if (lhs != '') {
            lhs_split <- str_split(lhs, "-")
        }

        rhs <- selected_rule[[1]][2]
        if (rhs != '') {
            rhs_split <- str_split(rhs, "-")
        }

        for (threshold in 1:7) {
            bin.df <- data_factor[which(data_factor$Diagnose == threshold),]
            n <- nrow(bin.df)

            if (lhs == '') {
                sub.df <- bin.df[which(bin.df[rhs_split[[1]][1]] == rhs_split[[1]][2]),]
                fis <- paste0('{', rhs, '}')
            } else if (rhs == '') {
                sub.df <- bin.df[which(bin.df[lhs_split[[1]][1]] == lhs_split[[1]][2]),]
                fis <- paste0('{', lhs, '}')
            } else {
                sub.df <- bin.df[which(bin.df[lhs_split[[1]][1]] == lhs_split[[1]][2] &
                                           bin.df[rhs_split[[1]][1]] == rhs_split[[1]][2]),]
                fis <- paste0('{', lhs, ', ', rhs, '}')
            }

            count <- nrow(sub.df)
            support <- round(count / n, 2)
            threshold <- paste0('Diagnosis ', threshold)

            df <- rbind(df, c(fis, support, count, threshold))
        }

        colnames(df) <- c('FIS', 'support', 'frequency', 'diagnosis')
        output$rule_support_diagram_pm <- renderPlot({
            return(ggplot(df, aes(diagnosis, support)) +
                       geom_point() +
                       theme_minimal() +
                       theme(axis.text.x = element_text(angle = 45, hjust = 1),
                             axis.text = element_text(size = 12),
                       ))
        })
        output$test_pm <- renderTable({
            brushedPoints(df, input$rule_support_diagram_pm_brush)
        })
    })

    output$table_rule_pm <- DT::renderDataTable({
        req(input$file_pm)

        tryCatch(
        {
            input_data <- read.csv(input$file_pm$datapath, sep = ';')
        },
            error = function(e) {
                # return a safeError if a parsing error occurs
                stop(safeError(e))
            }
        )

        support <- input$slider_supp_pm
        confidence <- input$slider_conf_pm
        num_rules <- input$num_rules_pm

        data_factor <- input_data[grep("^Aus", names(input_data))]

        data_factor$Diagnose <- input_data$Diagnose

        all_rules <- extract_rules_rare(data_factor, support, confidence, num_rules, 6)

        my_vars <- c("threshold", "support", "confidence", "lift", "frequency")

        clean_rules <- all_rules[my_vars]

        clean_rules["rule"] <- paste0(all_rules$lhs, all_rules$notation, all_rules$rhs)

        return(datatable(clean_rules, rownames = FALSE))
    })

    output$table_itemsets_pm <- DT::renderDataTable({
        req(input$file_pm)

        tryCatch(
        {
            input_data <- read.csv(input$file_pm$datapath, sep = ';')
        },
            error = function(e) {
                # return a safeError if a parsing error occurs
                stop(safeError(e))
            }
        )

        support <- input$slider_supp_pm
        num_itemsets <- input$num_itemsets_pm

        data_factor <- input_data[grep("^Aus", names(input_data))]
        data_factor$Diagnose <- input_data$Diagnose

        all_sets <- extract_freq_itemsets_rare(data_factor, support, 'pm', num_itemsets)

        return(datatable(all_sets, rownames = FALSE))
    })

    output$table_itemset_amount_pm <- DT::renderDataTable({
        req(input$file_pm)

        tryCatch(
        {
            input_data <- read.csv(input$file_pm$datapath, sep = ';')
        },
            error = function(e) {
                # return a safeError if a parsing error occurs
                stop(safeError(e))
            }
        )

        support <- input$slider_supp_pm

        data_factor <- input_data[grep("^Aus", names(input_data))]
        data_factor$Diagnose <- input_data$Diagnose

        all_sets <- extract_freq_itemsets_amount_rare(data_factor, support, 'pm')

        return(datatable(all_sets, rownames = FALSE))
    })

    output$tracking_pm <- DT::renderDataTable({
        req(input$file_pm)

        tryCatch(
        {
            input_data <- read.csv(input$file_pm$datapath, sep = ';')
        },
            error = function(e) {
                # return a safeError if a parsing error occurs
                stop(safeError(e))
            }
        )

        support <- input$slider_supp_pm
        confidence <- input$slider_conf_pm
        num_rules <- input$num_rules_pm

        data_factor <- input_data[grep("^Aus", names(input_data))]

        data_factor$Diagnose <- input_data$Diagnose

        all_rules <- extract_rules_rare(data_factor, support, confidence, num_rules, 6)

        multiple_segments <- extract_state(all_rules)

        multiple_segments <- multiple_segments[, c('threshold', 'rule', 'support', 'confidence', 'lift', 'frequency')]
        names(multiple_segments)[names(multiple_segments) == "threshold"] <- "Diagnosis"

        return(datatable(multiple_segments, rownames = FALSE))
    })

    output$tracking_bmc_fis <- renderTable({
        req(input$file_bmc)

        tryCatch(
        {
            input_data <- read_excel(input$file_bmc$datapath)
        },
            error = function(e) {
                # return a safeError if a parsing error occurs
                stop(safeError(e))
            }
        )

        support <- input$slider_supp_bmc
        confidence <- input$slider_conf_bmc
        num_rules <- input$num_rules_bmc

        data_factor <- input_data[grep("^q", names(input_data))]

        data_factor$Diagnose <- input_data$diagnosis

        all_rules <- extract_rules_rare(data_factor, support, confidence, num_rules, 7)

        rules_count <- all_rules %>%
            group_by(threshold) %>%
            summarise(sum = sum(frequency),
                      .groups = 'drop')

        multiple_segments <- extract_state(all_rules)

        unique_rules <- unique(multiple_segments$rule)

        tab <- data.frame("Rule" = NULL,
                          "Diagnosis(support confidence)" = NULL,
                          "FIS Diagnose I" = NULL,
                          "P-value Diagnose I (confidence interval)" = NULL,
                          "Diagnosis(support confidence)" = NULL,
                          "FIS Diagnose II" = NULL,
                          "P-value Diagnose II (confidence interval)" = NULL)

        for (rule in unique_rules) {
            temp_rules_set <- multiple_segments[multiple_segments$rule == rule,]

            for (i in 1:(nrow(temp_rules_set) - 1)) {
                j <- i + 1
                count_1 <- temp_rules_set$frequency[i]
                count_2 <- temp_rules_set$frequency[j]

                threshold_1 <- temp_rules_set$threshold[i]
                threshold_2 <- temp_rules_set$threshold[j]

                summary_1 <- rules_count[rules_count$threshold == threshold_1,]
                summary_2 <- rules_count[rules_count$threshold == threshold_2,]

                fisher_matrix_1 <- matrix(c(count_1, summary_1$sum - count_1, count_2, summary_2$sum - count_2), nrow = 2)
                fisher_matrix_2 <- matrix(c(count_2, summary_2$sum - count_2, count_1, summary_1$sum - count_1), nrow = 2)

                fisher_value_1 <- fisher.test(fisher_matrix_1)
                fisher_value_2 <- fisher.test(fisher_matrix_2)

                tab <- rbind(tab,
                             c(rule,
                               paste0(threshold_1, ' (', temp_rules_set$support[i], ', ', temp_rules_set$confidence[i], ')'),
                               count_1,
                               paste0(round(fisher_value_1$p.value, 2), ' (', round(fisher_value_1$conf.int[1], 2), ', ', round(fisher_value_1$conf.int[2], 2), ')'),
                               paste0(threshold_2, ' (', temp_rules_set$support[j], ', ', temp_rules_set$confidence[j], ')'),
                               count_2,
                               paste0(round(fisher_value_2$p.value, 2), ' (', round(fisher_value_2$conf.int[1], 2), ', ', round(fisher_value_2$conf.int[2], 2), ')')
                             )
                )
            }
        }

        colnames(tab) <- c("Rule",
                           "Diagnosis I (support, confidence)", "Maximum FIS Diagnose I", "P-value Diagnose I (confidence interval)",
                           "Diagnosis II (support, confidence)", "Maximum FIS Diagnose II", "P-value Diagnose II (confidence interval)")
        return(tab)
    })

    output$tracking_pm_fis <- renderTable({
        req(input$file_pm)

        tryCatch(
        {
            input_data <- read.csv(input$file_pm$datapath, sep = ';')
        },
            error = function(e) {
                # return a safeError if a parsing error occurs
                stop(safeError(e))
            }
        )

        support <- input$slider_supp_pm
        confidence <- input$slider_conf_pm
        num_rules <- input$num_rules_pm

        data_factor <- input_data[grep("^Aus", names(input_data))]

        data_factor$Diagnose <- input_data$Diagnose

        all_rules <- extract_rules_rare(data_factor, support, confidence, num_rules, 6)

        rules_count <- all_rules %>%
            group_by(threshold) %>%
            summarise(sum = sum(frequency),
                      .groups = 'drop')

        multiple_segments <- extract_state(all_rules)

        unique_rules <- unique(multiple_segments$rule)

        tab <- data.frame("Rule" = NULL,
                          "Diagnosis(support confidence)" = NULL,
                          "FIS Diagnose I" = NULL,
                          "P-value Diagnose I (confidence interval)" = NULL,
                          "Diagnosis(support confidence)" = NULL,
                          "FIS Diagnose II" = NULL,
                          "P-value Diagnose II (confidence interval)" = NULL)

        for (rule in unique_rules) {
            temp_rules_set <- multiple_segments[multiple_segments$rule == rule,]

            for (i in 1:(nrow(temp_rules_set) - 1)) {
                j <- i + 1
                count_1 <- temp_rules_set$frequency[i]
                count_2 <- temp_rules_set$frequency[j]

                threshold_1 <- temp_rules_set$threshold[i]
                threshold_2 <- temp_rules_set$threshold[j]

                summary_1 <- rules_count[rules_count$threshold == threshold_1,]
                summary_2 <- rules_count[rules_count$threshold == threshold_2,]

                fisher_matrix_1 <- matrix(c(count_1, summary_1$sum - count_1, count_2, summary_2$sum - count_2), nrow = 2)
                fisher_matrix_2 <- matrix(c(count_2, summary_2$sum - count_2, count_1, summary_1$sum - count_1), nrow = 2)

                fisher_value_1 <- fisher.test(fisher_matrix_1)
                fisher_value_2 <- fisher.test(fisher_matrix_2)

                tab <- rbind(tab,
                             c(rule,
                               paste0(threshold_1, ' (', temp_rules_set$support[i], ', ', temp_rules_set$confidence[i], ')'),
                               count_1,
                               paste0(round(fisher_value_1$p.value, 2), ' (', round(fisher_value_1$conf.int[1], 2), ', ', round(fisher_value_1$conf.int[2], 2), ')'),
                               paste0(threshold_2, ' (', temp_rules_set$support[j], ', ', temp_rules_set$confidence[j], ')'),
                               count_2,
                               paste0(round(fisher_value_2$p.value, 2), ' (', round(fisher_value_2$conf.int[1], 2), ', ', round(fisher_value_2$conf.int[2], 2), ')')
                             )
                )
            }
        }

        colnames(tab) <- c("Rule",
                           "Diagnosis I (support, confidence)", "Maximum FIS Diagnose I", "P-value Diagnose I (confidence interval)",
                           "Diagnosis II (support, confidence)", "Maximum FIS Diagnose II", "P-value Diagnose II (confidence interval)")
        return(tab)
    })
}

shinyApp(ui, server)