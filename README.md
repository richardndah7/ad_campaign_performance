---
title: "Advert Campaign Performance Analysis with R"
author: "Richard Ndah"
date: "2025-02-26"
output: rmarkdown::github_document
---



### **Analyzing Advertising Campaign Performance across Multiple Platforms and Uncovering Insights into Advertising Campaign Effectiveness with R. **





**Note**: Setting up my R environment
```{r}
library(tidyverse)

library(janitor)

library(skimr)

library(here)

library(scales)

library(dplyr)

```

 **Loading csv file and getting to know its content for further analysis**
```{r}
advert_campaign_perfomance <- read.csv("ad_campaign_performance.csv")


head(advert_campaign_perfomance)

glimpse(advert_campaign_perfomance)

colnames(advert_campaign_perfomance)
```
#### **Summary:**
**Now lets have a look at the summary for potential insights**

```{r}
summary(advert_campaign_perfomance)
```

#### **Platform Success:** 
**From the summary above it is clear that there's a lot to uncover from this data set even though its small. First we will be looking at platforms with the highest success rates **

```{r}
advert_platforms_data <- advert_campaign_perfomance %>%
  group_by(Platform) %>%
  summarize(
    total_clicks = sum(Clicks),
    total_conversions = sum(Conversions),
    success_rate = (total_conversions / total_clicks) * 100
  ) %>% 
  arrange(desc(success_rate))

print(advert_platforms_data)
```

#### **Top 10 adverts:**
**LinkedIn seems to be the best performing platform due to success rate from clicks and conversions followed by Facebook and Google. Now lets take a look at the top 10 most successful adverts next**

```{r}
advert_ctr_data <- advert_campaign_perfomance %>%
  group_by(Platform) %>%
  reframe(
    campaign_id = (Campaign_ID), 
    ad_content = (Content_Type),
    duration = (Duration),      
    total_clicks = sum(Clicks),
    total_conversions = sum(Conversions),
    success_rate = (total_conversions / total_clicks) * 100,
    success = (Success)
  ) %>%
  arrange((duration),desc(success_rate)) %>% 
  head(10)

print(advert_ctr_data)

```


#### **Most successful advert campaigns and content type:**

**The table above gives information about the most successful adverts that recorded the best engagements in the shortest span period of 3 days now lets take look at the visualization that further explains more on this finding **


```{r}
ggplot(advert_ctr_data, aes(x = campaign_id, y = success_rate, fill = Platform)) +
  geom_col() +
  # Add labels for ad_content
  geom_text(
    aes(label = ad_content),
    position = position_stack(vjust = 0.5),  # Center labels in stacked bars
    size = 3,  # Adjust font size
    color = "white"  # Optional: Improve readability
  ) +
  labs(
    title = "Top-Performing Campaigns by Platform and Type:", subtitle = "Campaign Success Rates Across Platforms and Ad Formats (3-Day Period)",
    x = "Campaign Id",
    y = "Success Rate (%)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    legend.position = "bottom"
  )
```

#### **Content Type Distribution:** 
**To have a clearer knowledge we need to look deeper into the content type that drives more engagements. **

```{r}
advert_content_data <- advert_campaign_perfomance %>%
  group_by(content = Content_Type) %>%
  summarise(avg_total_clicks = mean(Clicks)) %>%
  mutate(
    sum_total_clicks = sum(avg_total_clicks),
    percentage = (avg_total_clicks / sum_total_clicks) * 100
  )

print(advert_content_data)
```


```{r}
ggplot(advert_content_data, aes(x = "", y = percentage, fill = content)) +
   geom_bar(stat = "identity", width = 1, color = "white") +
   coord_polar("y", start = 0) +
   geom_text(aes(label = paste0(round(percentage, 1), "%")), 
             position = position_stack(vjust = 0.5),
             size = 5, color = "black", fontface = "bold") +
   scale_fill_manual(values = c("#80ff00", "#00ffff", "#ffe6e6", "#ff4d4d", "orange")) +
   labs(title = "Content Type Distribution",
        subtitle = "Percentage breakdown of different content types") +
   theme_void() +
   theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
         plot.subtitle = element_text(hjust = 0.5, size = 14, margin = margin(b = 20)),
         legend.position = "right",
         legend.title = element_text(size = 12, face = "bold"),
         legend.text = element_text(size = 11),
         plot.margin = unit(c(1, 1, 1, 1), "cm")) +
   guides(fill = guide_legend(title = "Content Types"))
```

#### **Average click performance by age groups:**
**Now we also need to look at engagements by age groups **

```{r}
ad_cam_pef_viz <- advert_campaign_perfomance %>%
  group_by(age_group = Target_Age) %>%
  reframe(
    country = (Region),
    avg_clicks = mean(Clicks))


ggplot(data = ad_cam_pef_viz,  
       aes(x = reorder(age_group, avg_clicks), 
           y = avg_clicks, 
           fill = age_group)) +
  geom_col(width = 0.7, color = "white", linewidth = 0.5) +
  geom_text(aes(label = scales::comma(avg_clicks, accuracy = 1)),
            vjust = -0.5, 
            size = 3.5,
            color = "gray30") +
  scale_fill_manual(values = c("#264653", "#2A9D8F", "#E9C46A", "#F4A261", "#E76F51")) +
  scale_y_continuous(
    labels = scales::comma_format(accuracy = 1),  # Force whole numbers with commas
    expand = expansion(mult = c(0, 0.1)),
    breaks = scales::pretty_breaks(n = 6)  # Clean integer breaks
  ) +
  labs(
    title = "Average Click Performance by Age Group",
    subtitle = "Digital Engagement Analysis Across User Cohorts",
    x = "Age Group",
    y = "Average Clicks",
    caption = "Source: Marketing Analytics Data"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "gray50"),
    axis.text.y = element_text(color = "gray30"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.title.position = "plot"
  )
```



```{r}
advert_country_summary <- advert_campaign_perfomance %>%
            group_by(country = Region) %>%
            summarize(
              total_clicks = sum(Clicks),
              total_conversions = sum(Conversions),
              success_rate = (total_conversions / total_clicks) * 100,
              # Assuming success summary is based on a 50% success rate threshold
              success_summary = success_rate >= 10
            )      
print(advert_country_summary)
```
#### **Country Perfomance:**
**Next we are going to look into how countries have been engaged and their performance so far **

```{r}
ggplot(advert_country_summary, 
                 aes(x = reorder(country, total_clicks), 
                     y = total_clicks,
                     fill = success_summary)) +
            geom_bar(stat = "identity") +
            geom_line(aes(y = success_rate * max(total_clicks)/100, 
                          group = 1),
                      color = "navy",
                      linewidth = 1) +
            geom_text(aes(y = success_rate * max(total_clicks)/100, 
                          label = paste0(round(success_rate, 1), "%")),
                      vjust = -0.5,
                      color = "black") +
            scale_y_continuous(
              name = "Total Clicks",
              labels = scales::comma,  # Format with commas instead of scientific notation
              sec.axis = sec_axis(~. * 100 / max(advert_country_summary$total_clicks),
                                  name = "Success Rate (%)")
            ) +
            scale_fill_manual(values = c("FALSE" = "#FF6B6B", "TRUE" = "#4ECDC4")) +
            labs(
              title = "Country Performance: Total Clicks and Success Rate",
              x = "Country",
              fill = "Met Success Criteria"
            ) +
            theme_minimal() +
            theme(
              axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = "bottom"
            )
```

#### **Gender Engagements by Country:**

```{r}
ggplot(data = advert_campaign_perfomance) +
            geom_bar(mapping = aes(x = Target_Gender, fill = Target_Gender))+
            labs(title = "Gender Engagements by Country")+
            facet_wrap(~Region) +
  
  # Customize bar colors
  scale_fill_manual(
    name = "Gender",  # Legend title
    values = c("All" = "#c2a100", "Male" = "#1f77b4", "Female" = "#5d8c4e")  # Assign colors
    # Replace "Male"/"Female" with your actual categories (case-sensitive)
    # Replace hex codes with your desired colors
  )
```

#### **Clicks:** 

```{r}
advert_click_rate_data <- advert_campaign_perfomance %>%
  group_by(country = Region) %>% 
  reframe(
    avg_clicks  = mean(Clicks),
    avg_ctr = mean(CTR)
  )




ggplot(advert_click_rate_data, aes(x = avg_clicks, y = avg_ctr, color = country)) +
                                 geom_point(size = 3) +  # Scatter plot with points
                                 geom_smooth(method = "lm", se = FALSE) +  # Add a trend line
                                 labs(
                                   title = "Average Clicks vs. Average CTR by Region",
                                   x = "Average Clicks",
                                   y = "Average CTR",
                                   color = "Region"  # Legend title
                                 ) +
                                 theme_minimal() +  # Clean theme
                                 theme(
                                   plot.title = element_text(face = "bold", size = 16),  
                                   axis.title = element_text(face = "bold", size = 12),  
                                   legend.position = "bottom"  # Place legend at the bottom
                                 )
```


### **Campaign Performance by Country:**

```{r}
plot_data <- advert_click_rate_data %>%
  pivot_longer(cols = c(avg_clicks, avg_ctr), 
               names_to = "metric", 
               values_to = "value")

# Then create the plot
ggplot(plot_data, aes(x = reorder(country, -value), y = value, fill = metric)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_manual(
    values = c("avg_clicks" = "#1f77b4", "avg_ctr" = "#ff7f0e"),
    labels = c("Average Clicks", "Average CTR")
  ) +
  scale_y_continuous(
    labels = scales::comma_format(),
    sec.axis = sec_axis(~., 
                       labels = scales::percent_format(accuracy = 1),
                       name = "Click-Through Rate (CTR)")
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    panel.grid.major.x = element_blank(),
    legend.position = "top",
    plot.title = element_text(face = "bold", size = 16),
    axis.title = element_text(size = 12)
  ) +
  labs(
    title = "Campaign Performance by Country",
    subtitle = "Comparative Analysis of Click Metrics",
    x = "Country",
    y = "Average Clicks",
    fill = "Metric",
    caption = "Data Source: Campaign Analytics Database"
  ) +
  geom_text(
    aes(label = ifelse(metric == "avg_ctr",
                     scales::percent(value, accuracy = 1),
                     scales::comma(value, accuracy = 1))),
    position = position_dodge(width = 0.8),
    vjust = -0.5,
    size = 2.4
  )  
```


#### **Campaign Performance Distribution:**


```{r}
campaign_type_group <- advert_campaign_perfomance %>%
            mutate(
              campaign_type = factor(
                case_when(
                  CPC <= mean(CPC) & Clicks >= mean(Clicks) & Success == 1 & Budget < mean(Budget) & Duration <= 10 ~ "Good",
                  CPC > mean(CPC) & Clicks < mean(Clicks) & Success == 0 & Budget > mean(Budget) & Duration > 20 ~ "Bad",
                  TRUE ~ "Moderate"  # Catch-all for remaining cases
                ),
                levels = c("Good", "Bad", "Moderate")  
              )
            ) %>%
            select(Campaign_ID, Budget, Clicks, CPC, Success, Duration, campaign_type) %>%
            drop_na()

campaign_summary <- campaign_type_group %>%
  count(campaign_type) %>%
  mutate(pct = n / sum(n),
         label = paste0(round(pct * 100, 1), "%"))

# Create visualization
campaign_vis <- ggplot(campaign_summary, 
                       aes(x = campaign_type, y = pct, fill = campaign_type)) +
  geom_col(width = 0.7, show.legend = FALSE) +
  geom_text(aes(label = label), 
            vjust = -0.5, 
            size = 5,
            color = "navy",
            fontface = "bold") +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     limits = c(0, max(campaign_summary$pct) * 1.1),
                     expand = c(0, 0)) +
  scale_fill_manual(values = c("Good" = "#4E79A7", 
                              "Bad" = "#E15759", 
                              "Moderate" = "#F28E2B")) +
  labs(title = "Campaign Performance Distribution",
       subtitle = "Classification Based on CPC, Clicks, Budget, and Success Metrics",
       caption = "Data Source: Marketing Analytics Platform\nClassification Criteria: Relative to Platform Averages",
       x = NULL, y = "Percentage of Total Campaigns") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 20),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40", margin = margin(b = 20)),
    plot.caption = element_text(color = "gray50", hjust = 0),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(face = "bold", size = 12),
    axis.text.y = element_blank(),
    plot.margin = margin(20, 40, 20, 40)
  )

print(campaign_vis)
```


### **Findings:**

**Key Findings from Advertising Campaign Performance Analysis**

**T**his analysis explored advertising campaign performance across various platforms and content types, focusing on key metrics such as clicks, conversions, and success rates. Here are some notable findings:

1. LinkedIn's Effectiveness: LinkedIn demonstrated strong potential as an advertising platform, achieving high conversion rates within a short three-day period. This suggests that LinkedIn ads can be particularly effective for reaching and engaging target audiences.

2. Carousel Content's Rising Popularity: Surprisingly, carousel content emerged as a top performer, surpassing even video content in engagement. This indicates that carousels are an increasingly effective format for capturing audience attention and driving ad interactions.

3. Age and Engagement: The 35-44 age group exhibited the highest engagement levels, suggesting that this demographic may be more receptive to advertising messages, potentially due to increased responsibilities and life stage considerations.

4. Gender and Geographic Trends: Female audiences showed higher engagement across most regions, with particularly strong performance in Germany and Canada. This highlights the importance of considering gender when tailoring ad content and targeting strategies.

Note: This analysis was conducted on a specific dataset for training purposes. The findings should be interpreted within this context and may not directly reflect real-time advertising performance.

Feedback Welcome:

We appreciate your interest in this analysis. Please feel free to leave your feedback or suggestions in the comments section below. Your insights are valuable for improving future analyses.

Thank you for your time!