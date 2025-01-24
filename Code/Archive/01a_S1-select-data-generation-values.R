




# Generate data 
data_list <- generate_data2.0c(
    J =  100,
    njrange = c(5, 20), #c(50, 100),
    Mfamily = "binomial",
    Yfamily = "binomial", # "gaussian", 
    seed = 8675309,
    num_x = 3,
    # include_overlapMsg = FALSE,
    
    m_on_a = 15,
    m_on_anj = 0.5,
    m_on_az = 0.2,
    y_on_a = 2,
    y_on_m = 15,
    y_on_am = 5,
    y_on_az = 0.2,
    y_on_mz = 0.2,
    y_on_anj = 5,
    int.XZ = FALSE 
)

data_list$overlap$ps_summary

data_list$effects$individual$tnde
data_list$effects$individual$pnie

data_list$effects$individual$pnde
data_list$effects$individual$tnie





m_on_a, m_on_anj, m_on_az, y_on_a, y_on_m, y_on_am, y_on_az, y_on_mz, y_on_anj






# check values ------------------------------------------------------------

# Define the range of values to loop over for each parameter
m_on_a_values <- c(10, 15, 20)
m_on_anj_values <- c(0.1, 0.5, 1.0)
# m_on_az_values <- c(0.1, 0.2, 0.3)
y_on_a_values <- c(1, 2, 3)
y_on_m_values <- c(10, 15, 20)
y_on_am_values <- c(3, 5, 7)
# y_on_az_values <- c(0.1, 0.2, 0.3)
# y_on_mz_values <- c(0.1, 0.2, 0.3)
y_on_anj_values <- c(2, 5, 8)

# Create an empty dataframe to store the results
results_df <- data.frame(
    m_on_a = numeric(),
    m_on_anj = numeric(),
    m_on_az = numeric(),
    y_on_a = numeric(),
    y_on_m = numeric(),
    y_on_am = numeric(),
    y_on_az = numeric(),
    y_on_mz = numeric(),
    y_on_anj = numeric(),
    tnde = numeric(),
    pnie = numeric(),
    pnde = numeric(),
    tnie = numeric()
)

# Loop through all combinations of values
for (m_on_a in m_on_a_values) {
    for (m_on_anj in m_on_anj_values) {
        for (m_on_az in m_on_az_values) {
            for (y_on_a in y_on_a_values) {
                for (y_on_m in y_on_m_values) {
                    for (y_on_am in y_on_am_values) {
                        for (y_on_az in y_on_az_values) {
                            for (y_on_mz in y_on_mz_values) {
                                for (y_on_anj in y_on_anj_values) {
                                    
                                    # Generate data with the current parameter values
                                    data_list <- generate_data2.0c(
                                        J = 100,
                                        njrange = c(5, 20),
                                        Mfamily = "binomial",
                                        Yfamily = "binomial",
                                        seed = 8675309,
                                        num_x = 3,
                                        m_on_a = m_on_a,
                                        m_on_anj = m_on_anj,
                                        m_on_az = m_on_az,
                                        y_on_a = y_on_a,
                                        y_on_m = y_on_m,
                                        y_on_am = y_on_am,
                                        y_on_az = y_on_az,
                                        y_on_mz = y_on_mz,
                                        y_on_anj = y_on_anj,
                                        int.XZ = FALSE
                                    )
                                    
                                    # Extract the effects
                                    tnde <- data_list$effects$individual$tnde
                                    pnie <- data_list$effects$individual$pnie
                                    pnde <- data_list$effects$individual$pnde
                                    tnie <- data_list$effects$individual$tnie
                                    
                                    # Append the results to the dataframe
                                    results_df <- rbind(
                                        results_df,
                                        data.frame(
                                            m_on_a = m_on_a,
                                            m_on_anj = m_on_anj,
                                            m_on_az = m_on_az,
                                            y_on_a = y_on_a,
                                            y_on_m = y_on_m,
                                            y_on_am = y_on_am,
                                            y_on_az = y_on_az,
                                            y_on_mz = y_on_mz,
                                            y_on_anj = y_on_anj,
                                            tnde = tnde,
                                            pnie = pnie,
                                            pnde = pnde,
                                            tnie = tnie
                                        )
                                    )
                                    
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

# Save the results as a CSV for later review
# write.csv(results_df, "results_summary.csv", row.names = FALSE)

# Display the first few rows of the dataframe
head(results_df)





# check data gen values ---------------------------------------------------


# Define the range of values to loop over for each parameter
m_on_a_values <- c(10, 15, 20)
m_on_anj_values <- c(0.1, 0.5, 1.0)
# m_on_az_values <- c(0.1, 0.2, 0.3)
y_on_a_values <- c(1, 2, 3)
y_on_m_values <- c(10, 15, 20)
y_on_am_values <- c(3, 5, 7)
# y_on_az_values <- c(0.1, 0.2, 0.3)
# y_on_mz_values <- c(0.1, 0.2, 0.3)
y_on_anj_values <- c(2, 5, 8)


expand.grid(m_on_a_values = m_on_a_values, 
            y_on_a_values = y_on_a_values, 
            y_on_m_values = y_on_m_values, 
            y_on_am_values = y_on_am_values) |> 
    nrow() # 81


# Create an empty dataframe to store the results
results_df <- data.frame(
    m_on_a = numeric(),
    m_on_anj = numeric(),
    m_on_az = numeric(),
    y_on_a = numeric(),
    y_on_m = numeric(),
    y_on_am = numeric(),
    y_on_az = numeric(),
    y_on_mz = numeric(),
    y_on_anj = numeric(),
    tnde = numeric(),
    pnie = numeric(),
    pnde = numeric(),
    tnie = numeric()
)


data_list <- generate_data2.0c(
    J =  100,
    njrange = c(5, 20), #c(50, 100),
    Mfamily = "binomial", # "gaussian", 
    Yfamily = "binomial", # "gaussian", 
    seed = 8675309,
    num_x = 3,
    # include_overlapMsg = FALSE,
    
    m_on_a = 10, #2.5, #3, #5, #10, #15,
    m_on_anj = 0.5,
    m_on_az = 0.2,
    y_on_a = 2, 
    y_on_m = 10, #2.5, #3, #5, #10, #15,
    y_on_am = 2, #3, #5,
    y_on_az = 0.2,
    y_on_mz = 0.2,
    y_on_anj = 3, #5,
    int.XZ = FALSE 
)
# Loop through all combinations of values
for (m_on_a in m_on_a_values) {
    # for (m_on_anj in m_on_anj_values) {
        # for (m_on_az in m_on_az_values) {
            for (y_on_a in y_on_a_values) {
                for (y_on_m in y_on_m_values) {
                    for (y_on_am in y_on_am_values) {
                        # for (y_on_az in y_on_az_values) {
                            # for (y_on_mz in y_on_mz_values) {
                                # for (y_on_anj in y_on_anj_values) {
                                    
                                    # Generate data with the current parameter values
                                    data_list <- generate_data2.0c(
                                        J = 100,
                                        njrange = c(5, 20),
                                        Mfamily = "binomial",
                                        Yfamily = "binomial",
                                        seed = 8675309,
                                        num_x = 3,
                                        m_on_a = m_on_a,
                                        m_on_anj = 0.5,
                                        m_on_az = 0.2,
                                        # m_on_anj = m_on_anj,
                                        # m_on_az = m_on_az,
                                        y_on_a = y_on_a,
                                        y_on_m = y_on_m,
                                        y_on_am = y_on_am,
                                        y_on_az = 0.2,
                                        y_on_mz = 0.2,
                                        # y_on_az = y_on_az,
                                        # y_on_mz = y_on_mz,
                                        y_on_anj = 3,
                                        # y_on_anj = y_on_anj,
                                        int.XZ = FALSE
                                    )
                                    
                                    # Extract the effects
                                    tnde <- data_list$effects$individual$tnde
                                    pnie <- data_list$effects$individual$pnie
                                    pnde <- data_list$effects$individual$pnde
                                    tnie <- data_list$effects$individual$tnie
                                    
                                    # Append the results to the dataframe
                                    results_df <- rbind(
                                        results_df,
                                        data.frame(
                                            m_on_a = m_on_a,
                                            m_on_anj = 0.5,
                                            m_on_az = 0.2,
                                            # m_on_anj = m_on_anj,
                                            # m_on_az = m_on_az,
                                            y_on_a = y_on_a,
                                            y_on_m = y_on_m,
                                            y_on_am = y_on_am,
                                            y_on_az = 0.2,
                                            y_on_mz = 0.2,
                                            # y_on_az = y_on_az,
                                            # y_on_mz = y_on_mz,
                                            y_on_anj = 3, 
                                            # y_on_anj = y_on_anj,
                                            tnde = tnde,
                                            pnie = pnie,
                                            pnde = pnde,
                                            tnie = tnie
                                        )
                                    )
                                    
                                }
                            }
                        }
                    }
#                 }
#             }
#         }
#     }
# }




# checking data gen -------------------------------------------------------

# Load the required library for the progress bar
library(progress)

# Calculate the total number of iterations
num_iterations <- length(m_on_a_values) * length(y_on_a_values) * length(y_on_m_values) * length(y_on_am_values)

# Initialize the progress bar
pb <- progress_bar$new(
    format = "  Progress [:bar] :percent in :elapsed, ETA: :eta",
    total = num_iterations,
    clear = FALSE,
    width = 60
)

# Create an empty dataframe to store the results
results_df <- data.frame(
    m_on_a = numeric(),
    m_on_anj = numeric(),
    m_on_az = numeric(),
    y_on_a = numeric(),
    y_on_m = numeric(),
    y_on_am = numeric(),
    y_on_az = numeric(),
    y_on_mz = numeric(),
    y_on_anj = numeric(),
    tnde = numeric(),
    pnie = numeric(),
    pnde = numeric(),
    tnie = numeric()
)

# Loop through all combinations of values
for (m_on_a in m_on_a_values) {
    for (y_on_a in y_on_a_values) {
        for (y_on_m in y_on_m_values) {
            for (y_on_am in y_on_am_values) {
                
                # Update the progress bar at each iteration
                pb$tick()
                
                # Generate data with the current parameter values
                data_list <- generate_data2.0c(
                    J = 100,
                    njrange = c(5, 20),
                    Mfamily = "binomial",
                    Yfamily = "binomial",
                    seed = 8675309,
                    num_x = 3,
                    m_on_a = m_on_a,
                    m_on_anj = 0.5,
                    m_on_az = 0.2,
                    y_on_a = y_on_a,
                    y_on_m = y_on_m,
                    y_on_am = y_on_am,
                    y_on_az = 0.2,
                    y_on_mz = 0.2,
                    y_on_anj = 3,
                    int.XZ = FALSE
                )
                
                # Extract the effects
                tnde <- data_list$effects$individual$tnde
                pnie <- data_list$effects$individual$pnie
                pnde <- data_list$effects$individual$pnde
                tnie <- data_list$effects$individual$tnie
                
                # Append the results to the dataframe
                results_df <- rbind(
                    results_df,
                    data.frame(
                        m_on_a = m_on_a,
                        m_on_anj = 0.5,
                        m_on_az = 0.2,
                        y_on_a = y_on_a,
                        y_on_m = y_on_m,
                        y_on_am = y_on_am,
                        y_on_az = 0.2,
                        y_on_mz = 0.2,
                        y_on_anj = 3, 
                        tnde = tnde,
                        pnie = pnie,
                        pnde = pnde,
                        tnie = tnie
                    )
                )
                
            }
        }
    }
}

# Save the results as a CSV for later review
# write.csv(results_df, "results_summary.csv", row.names = FALSE)

# Display the first few rows of the dataframe
head(results_df)











# checking data gen -------------------------------------------------------


# Define the range of values to loop over for each parameter
m_on_a_values <- 1:10 #m_on_a_values <- c(10, 15, 20)
# m_on_anj_values <- c(0.1, 0.5, 1.0)
# m_on_az_values <- c(0.1, 0.2, 0.3)
y_on_a_values <- 1:10 #y_on_a_values <- c(1, 2, 3)
y_on_m_values <- 1:10 #y_on_m_values <- c(10, 15, 20)
y_on_am_values <- 1:10 #y_on_am_values <- c(3, 5, 7)
# y_on_az_values <- c(0.1, 0.2, 0.3)
# y_on_mz_values <- c(0.1, 0.2, 0.3)
y_on_anj_values <- 1:10 #y_on_anj_values <- c(2, 5, 8)


expand.grid(m_on_a_values = m_on_a_values, 
            y_on_a_values = y_on_a_values, 
            y_on_m_values = y_on_m_values, 
            y_on_am_values = y_on_am_values) |> 
    nrow() # 81

# Load the required library for the progress bar
library(progress)

# Calculate the total number of iterations
num_iterations <- length(m_on_a_values) * length(y_on_a_values) * length(y_on_m_values) * length(y_on_am_values) * length(y_on_anj_values)
num_iterations

# Initialize the progress bar
pb <- progress_bar$new(
    format = "  Progress [:bar] :percent in :elapsed, ETA: :eta",
    total = num_iterations,
    clear = FALSE,
    width = 60
)

# Create an empty dataframe to store the results
results_df <- data.frame(
    m_on_a = numeric(),
    m_on_anj = numeric(),
    m_on_az = numeric(),
    y_on_a = numeric(),
    y_on_m = numeric(),
    y_on_am = numeric(),
    y_on_az = numeric(),
    y_on_mz = numeric(),
    y_on_anj = numeric(),
    tnde = numeric(),
    pnie = numeric(),
    pnde = numeric(),
    tnie = numeric()
)

# Loop through all combinations of values
for (m_on_a in m_on_a_values) {
    for (y_on_a in y_on_a_values) {
        for (y_on_m in y_on_m_values) {
            for (y_on_am in y_on_am_values) {
                for (y_on_anj in y_on_anj_values) {
                
                # Update the progress bar at each iteration
                pb$tick()
                
                # Generate data with the current parameter values
                data_list <- generate_data2.0c(
                    J = 100,
                    njrange = c(5, 20),
                    Mfamily = "binomial",
                    Yfamily = "binomial",
                    seed = 8675309,
                    num_x = 3,
                    m_on_a = m_on_a,
                    m_on_anj = 0.5,
                    m_on_az = 0.2,
                    y_on_a = y_on_a,
                    y_on_m = y_on_m,
                    y_on_am = y_on_am,
                    y_on_az = 0.2,
                    y_on_mz = 0.2,
                    y_on_anj = y_on_anj, #3,
                    int.XZ = FALSE, 
                    include_overlapMsg = FALSE
                )
                
                # Extract the effects
                tnde <- data_list$effects$individual$tnde
                pnie <- data_list$effects$individual$pnie
                pnde <- data_list$effects$individual$pnde
                tnie <- data_list$effects$individual$tnie
                
                # Append the results to the dataframe
                results_df <- rbind(
                    results_df,
                    data.frame(
                        m_on_a = m_on_a,
                        m_on_anj = 0.5,
                        m_on_az = 0.2,
                        y_on_a = y_on_a,
                        y_on_m = y_on_m,
                        y_on_am = y_on_am,
                        y_on_az = 0.2,
                        y_on_mz = 0.2,
                        y_on_anj = y_on_anj, #3, 
                        tnde = tnde,
                        pnie = pnie,
                        pnde = pnde,
                        tnie = tnie
                    )
                )
                
                }
            }
        }
    }
}

# Save the results as a CSV for later review
# write.csv(results_df, "results_summary.csv", row.names = FALSE)

# Display the first few rows of the dataframe
head(results_df)


# Create output directory & save possible data generation values 
path <- "Output/S1_Data-Generation-Values"
if (!dir.exists(path)) dir.create(path, recursive = TRUE)
saveRDS(results_df, file.path(path, "S1_possible-data-generation-values_M-binomial-Y-binomial.rds"))



# Drop duplicates 
gen_vals <- results_df |> 
    distinct(tnde, pnie, pnde, tnie, .keep_all = TRUE)

# non-scientific
options(scipen = 999)
summary(gen_vals)
gen_vals

# narrow gen_vals to those larger than tnde & pnie means (0.027 & 0.12)
gen_vals2 <- gen_vals |> 
    filter(gen_vals$tnde >= mean(gen_vals$tnde)) # & gen_vals$pnie >= mean(gen_vals$pnie))

summary(gen_vals2)

# library(dplyr)
# 
# max_row <- gen_vals2 %>%
#     mutate(
#         tnde_scaled = (tnde - min(tnde)) / (max(tnde) - min(tnde)),
#         pnie_scaled = (pnie - min(pnie)) / (max(pnie) - min(pnie)),
#         pnde_scaled = (pnde - min(pnde)) / (max(pnde) - min(pnde)),
#         tnie_scaled = (tnie - min(tnie)) / (max(tnie) - min(tnie))
#     ) %>%
#     slice_max(tnde_scaled + pnie_scaled + pnde_scaled + tnie_scaled, n = 1)


# maybe use this 

gen_vals2 |> 
    filter(tnde > 0.02 & pnie > 0.1 & 
               pnde > 0.02 & tnie > 0.02)
## this is what ill use for now 
#   m_on_a m_on_anj m_on_az y_on_a y_on_m y_on_am y_on_az y_on_mz y_on_anj       tnde      pnie       pnde       tnie
# 3      2      0.5     0.2      1      2       3     0.2     0.2        1 0.02728740 0.1123586 0.09483227 0.04481371




# checking starting values & ps overlap -----------------------------------
# looping potential data generation parameter values to check effect size & PS overlap for a binomial M & Y case 

# ══════════════════════════════
#    Load functions 
# ══════════════════════════════

# Load Data gen functions 
# Define the vector of function names
function_names <- c(
    "generate_data", 
    "generate_clusters", 
    "generate_confounders", 
    "generate_treatment", 
    "generate_mediator", 
    "generate_outcome", 
    "pm1", 
    "my", 
    "trueVals", 
    
    # As of 01/01/2025 the two funcs below are the updated versions 
    "generate_data2.0c", 
    "trueVals2.0c"
)

# Loop through the function names and source each file
for (func in function_names) {
    source(file.path("Functions", paste0(func, ".R")))
}

# ══════════════════════════════
#    Loop throuhg possible data gen values  
# ══════════════════════════════
library(tidyverse)
library(progress)

# Create an empty dataframe to store the results
results_df <- data.frame(
    m_on_a = numeric(),
    m_on_anj = numeric(),
    m_on_az = numeric(),
    y_on_a = numeric(),
    y_on_m = numeric(),
    y_on_am = numeric(),
    y_on_az = numeric(),
    y_on_mz = numeric(),
    y_on_anj = numeric(),
    tnde = numeric(),
    pnie = numeric(),
    pnde = numeric(),
    tnie = numeric(),
    pctPSbelow01 = numeric(),  
    pctPSabove99 = numeric()
)

# Define the range of values to loop over for each parameter
m_on_a_values <- 1:5
y_on_a_values <- 1:5
y_on_m_values <- 1:5
y_on_am_values <- 1:5
y_on_anj_values <- 1:5

num_iterations <- length(m_on_a_values) * length(y_on_a_values) * length(y_on_m_values) * length(y_on_am_values) * length(y_on_anj_values)

pb <- progress_bar$new(
    format = "  Progress [:bar] :percent in :elapsed, ETA: :eta",
    total = num_iterations,
    clear = FALSE,
    width = 60
)

# Loop through all combinations of values
for (m_on_a in m_on_a_values) {
    for (y_on_a in y_on_a_values) {
        for (y_on_m in y_on_m_values) {
            for (y_on_am in y_on_am_values) {
                for (y_on_anj in y_on_anj_values) {
                    
                    # Update the progress bar at each iteration
                    pb$tick()
                    
                    # Generate data with the current parameter values
                    data_list <- generate_data2.0c(
                        J = 100,
                        njrange = c(5, 20),
                        Mfamily = "binomial",
                        Yfamily = "binomial",
                        seed = 8675309,
                        num_x = 3,
                        m_on_a = m_on_a,
                        m_on_anj = 0.5,
                        m_on_az = 0.2,
                        y_on_a = y_on_a,
                        y_on_m = y_on_m,
                        y_on_am = y_on_am,
                        y_on_az = 0.2,
                        y_on_mz = 0.2,
                        y_on_anj = y_on_anj,
                        int.XZ = FALSE,
                        include_overlapMsg = FALSE
                    )
                    
                    # Extract the effects
                    tnde <- data_list$effects$individual$tnde
                    pnie <- data_list$effects$individual$pnie
                    pnde <- data_list$effects$individual$pnde
                    tnie <- data_list$effects$individual$tnie
                    
                    # Extract the percentages from ps_summary
                    ps_summary <- data_list$overlap$ps_summary
                    percentages <- str_match_all(ps_summary, "\\(([^)]+)%\\)")[[1]][, 2]
                    
                    pctPSbelow01 <- as.numeric(percentages[1])
                    pctPSabove99 <- as.numeric(percentages[2])
                    
                    # Append the results to the dataframe
                    results_df <- rbind(
                        results_df,
                        data.frame(
                            m_on_a = m_on_a,
                            m_on_anj = 0.5,
                            m_on_az = 0.2,
                            y_on_a = y_on_a,
                            y_on_m = y_on_m,
                            y_on_am = y_on_am,
                            y_on_az = 0.2,
                            y_on_mz = 0.2,
                            y_on_anj = y_on_anj,
                            tnde = tnde,
                            pnie = pnie,
                            pnde = pnde,
                            tnie = tnie,
                            pctPSbelow01 = pctPSbelow01,  # Add the percentages to the dataframe
                            pctPSabove99 = pctPSabove99
                        )
                    )
                    
                }
            }
        }
    }
}

# View the first few rows of the results
head(results_df)


# Create output directory & save possible data generation values 
path <- "Output/S1_Data-Generation-Values"
if (!dir.exists(path)) dir.create(path, recursive = TRUE)
saveRDS(results_df, file.path(paste0(path, "S1_possible-data-generation-values_M-binomial-Y-binomial_", Sys.Date(), ".rds")))



### review possible values --------------------------------------------------

gen_vals <- readRDS(file = file.path(paste0(path, "S1_possible-data-generation-values_M-binomial-Y-binomial_", Sys.Date(), ".rds")))




