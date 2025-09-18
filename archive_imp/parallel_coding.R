# Load necessary libraries
library(dplyr)    # For data manipulation
library(ggplot2)  # For data visualization
library(readr)    # For reading CSV files

# Read the dataset from a CSV file (replace 'your_data.csv' with your actual file path)
data <- read_csv("your_data.csv")

# Preview the structure of the dataset
glimpse(data)

# Clean and transform the data using dplyr
# Example: Let's say the dataset has columns 'Category', 'Value', and 'Date'

# Summarize average value per category
summary_data <- data %>%
  group_by(Category) %>%                 # Group data by 'Category'
  summarise(mean_value = mean(Value, na.rm = TRUE)) %>%  # Calculate mean while handling NAs
  arrange(desc(mean_value))             # Sort categories by average value descending

# Print summary to check
print(summary_data)

# Plot using ggplot2 - Bar chart of average values by category
ggplot(summary_data, aes(x = reorder(Category, -mean_value), y = mean_value)) +
  geom_bar(stat = "identity", fill = "skyblue") +  # Create bar plot
  labs(title = "Average Value by Category",        # Add title
       x = "Category",                             # Label x-axis
       y = "Average Value") +                      # Label y-axis
  theme_minimal()                                  # Use a clean theme

# Another example: Time series line plot
# Convert 'Date' to Date type (if needed)
data$Date <- as.Date(data$Date)

# Aggregate value by Date
time_data <- data %>%
  group_by(Date) %>%
  summarise(total_value = sum(Value, na.rm = TRUE))

# Plot the time series
ggplot(time_data, aes(x = Date, y = total_value)) +
  geom_line(color = "steelblue") +
  labs(title = "Total Value Over Time",
       x = "Date",
       y = "Total Value") +
  theme_minimal()



# check data type 
# Check data types of each column
str(df)  # Shows structure of the data frame

# Convert 'age' column from character to integer
df$age <- as.integer(df$age)  # Now 'age' is integer type

# Convert 'score' column from character to numeric (float)
df$score <- as.numeric(df$score)  # Now 'score' is numeric (float)

# Convert a character column to factor (for categorical data)
df$name <- as.factor(df$name)