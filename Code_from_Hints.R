#start with guide for making chart




# Load necessary libraries
library(tidyverse)
library(janitor)

# Set seed for reproducibility
set.seed(123)

# Create the dataset
data <- data.frame(
  bps = rep(c("ecosystem A", "ecosystem B", "ecosystem C"), each = 10),
  label = rep(c("A", "B", "C", "D", "E", "Agriculture", "Developed", "Water", "UN", "UE"), times = 3),
  ref_cur = rep(c("refPercent", "currentPercent"), each = 15),
  amount = sample(10:200, 30, replace = TRUE)
)

# Plot
fake_plot <-
  ggplot(data, aes(fill=(ref_cur), y=amount, x=label)) +
  geom_col(width = 0.8, position = position_dodge()) +
  coord_flip() +
  facet_grid(. ~bps) +
  labs(
    title = "Succession Classes past and present",
    subtitle = "Fake chart with fake data",
    caption = "Data from landfire.gov",
    x = "",
    y = "Amount (units unknown")

fake_plot


### Time to review inputs

#FIrst Input: Bps aoi, useful data for my graph
bps_aoi_attributes <- read_csv("input_data/bps_aoi_attributes.csv")
head(bps_aoi_attributes)
tail(bps_aoi_attributes)

#Second Input: Bps Model, not useful data
bps_model_number_name <- read_csv("input_data/bps_model_number_name.csv")
head(bps_model_number_name)
tail(bps_model_number_name)

#Third Input: Combine Raw, useful, Var 1 and Value 1 are same and needed for calculating percentages
combine_raw <- read_csv("input_data/combine_raw.csv")
head(combine_raw)
tail(combine_raw)

####Pivot longer and data clean up needs to be run

ref_con <- ref_con_modified %>%
  pivot_longer(!Model_Code, names_to = "refLabel", values_to = "refPercent") %>%
  unite(model_label, c("Model_Code", "refLabel"), remove = FALSE) %>%
  left_join(bps_model_number_name)


# get list of aoi BpS model numbers

aoi_bps_models <- bps_aoi_attributes$BPS_MODEL

#subset ref_con to aoi, keeping all BpSs in the AOI for now
aoi_ref_con <- subset(ref_con, Model_Code %in% aoi_bps_models)

#bring in s-class labels
combine <- left_join(combine_raw, 
                     scls_aoi_attributes %>%
                       dplyr::select(2, 4),  
                     by = c("Var2" = "VALUE"))

#bring in bps labels
combine <- left_join(combine, 
                     LF16_BPS_200 %>%
                       dplyr::select(1:4),
                     by = c("Var1" = "VALUE"))

# calculate current sclass percents
combine <- combine %>%
  group_by(Var1, BPS_MODEL) %>%
  mutate(total_count = sum(Freq)) %>%
  mutate(currentPercent = as.integer((Freq/total_count)*100)) %>%
  unite(model_label, c("BPS_MODEL", "LABEL"))

## Note, in general percents for any one BpS should = 100, but may not due to water or 'noise'. They should be close. 

## Also note Jaxson's comment in Slack---there may be missing sclass lables here if a class is not mapped on the current landscape.  For example, old-growth classes are often missing on the current landscape, but were important historically

####Combine everything into one code string
aoi_ref_cur <- left_join(aoi_ref_con,
                         combine) %>%
  drop_na(refPercent) %>%
  mutate(currentPercent = as.numeric(currentPercent),
         currentPercent = ifelse(is.na(currentPercent), 0, currentPercent)) %>%
  mutate(total_count = as.numeric(total_count),
         total_count = ifelse(is.na(total_count), 0, total_count)) %>%
  select(-c(BPS_CODE, ZONE)) %>%
  select(c(Freq,
           Var1,
           Var2,
           BpS_Name,
           Model_Code,
           refLabel,
           model_label,
           refPercent,
           currentPercent,
           total_count)) %>%
  rename(count = Freq,
         bps_value = Var1,
         scl_value = Var2,
         bps_name = BpS_Name) %>%
  clean_names() 

####Craft the CHart

bps_scls_top <- aoi_ref_cur %>%
  group_by(model_code) %>%
  mutate(total_count = ifelse(total_count == 0, max(total_count), total_count)) %>%
  arrange(desc(total_count))  %>%
  ungroup() %>%
  dplyr::filter(dense_rank(desc(total_count)) < 4) %>%
  dplyr::select(c("bps_name", "ref_label",  "current_percent", "ref_percent")) %>%
  pivot_longer(
    cols = c(`ref_percent`, `current_percent`), 
    names_to = "ref_cur", 
    values_to = "percent"
  )


# order classes
bps_scls_top$ref_label <- factor(bps_scls_top$ref_label, 
                                 levels = c(
                                   "Developed",
                                   "Agriculture",
                                   "UE",
                                   "UN",
                                   "E",
                                   "D",
                                   "C",
                                   "B",
                                   "A"))

sclasplot <-
  ggplot(bps_scls_top, aes(fill = factor(ref_cur), y = percent, x = ref_label)) + 
  geom_col(width = 0.8, position = position_dodge()) +
  coord_flip() +
  facet_grid(. ~BpS) +
  scale_x_discrete(limits = (levels(bps_scls_top$ref_label))) +
  labs(
    title = "Succession Classes past and present",
    subtitle = "Top BpSs selected for illustration. Not all succession classes present in all BpSs",
    caption = "Data from landfire.gov.",
    x = "",
    y = "Percent") +
  theme_minimal(base_size = 12) +
  theme(plot.caption = element_text(hjust = 0, face = "italic"), #Default is hjust=1
        plot.title.position = "plot", #NEW parameter. Apply for subtitle too.
        plot.caption.position =  "plot") +
  scale_fill_manual(values = c("#3d4740", "#32a852" ), # present (grey), historical (green)
                    name = " ", 
                    labels = c("Present",
                               "Past")) +
  facet_wrap(~bps_name, nrow(3),labeller = labeller(bps_name = label_wrap_gen())) +
  theme(panel.spacing = unit(.05, "lines"),
        panel.border = element_rect(color = "black", fill = NA, size = 1), 
        strip.background = element_rect(color = "black", size = 1))

sclasplot