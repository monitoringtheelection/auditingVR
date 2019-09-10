source("setup.R")
library(brms)

## Check if a pre-run report exists. If not, run setup.R =======================
if (length(list.files(pattern = "report.*Rda", all.files = TRUE)) > 0) {
  report <- loadRData(
    max(list.files(pattern = "report.*Rda", all.files = TRUE))
  )
} else {
  source("match.R")
}

## Wrangle the data into long/wide format ======================================
report <- report %>%
  mutate(date = parse_date_time(date_origin, "mdy")) %>%
  arrange(date, labels)

long_df <- c("change_n", "change_prop") %>%
  set_names(., .) %>%
  map(
    ~ report %>%
      select(labels, date = date_origin, value = !!.x) %>%
      mutate(date = parse_date_time(date, "mdy"))
  )

wide_df <- long_df %>%
  map(
    ~ spread(.x, key = labels, value = value) %>%
      mutate(
        wday = wday(date, label = TRUE),
        date_label = format(date, "%b %d"),
        gap = ifelse(is.na(lag(date)), 1, date - lag(date)),
        month = month(date),
        year = year(date),
        month_24 = month + (year - 2018) * 12
      )
  )

## Visualization of changes ====================================================
### For proportions, use [[2]]. 
### Recommended for Rmd for best value out of echarts

### Records added, dropped, and changed
change_graph(wide_df[[1]])
### Field by field
field_graph(wide_df[[1]])

## First-stage anomaly detection: interquartile range method (IQR) =============
levels <- c(
  "Records Added" = "Added", "Records Dropped" = "Dropped",
  "Records Changed" = "Changed", "Party" = "szPartyName",
  "Address" = "szSitusAddress", "First Name" = "szNameFirst",
  "Last Name" = "szNameLast", "Birthday" = "dtBirthDate",
  "Voter ID" = "lVoterUniqueID"
)

### Set factor levels and regress on day of week / gap between snaphosts
### augment the residuals (there are some missing snapshots, w/ changes piling)
dl <- list(
  resid_df = report %>%
    mutate(labels = as.character(labels)) %>%
    filter(!(labels %in% c("Exact Matches"))) %>%
    mutate(
      labels = fct_recode(labels, !!!levels),
      date = as.Date(parse_date_time(date_origin, orders = "mdy")),
      change_prop = change_n / totalB * 100,
      count = format(change_prop, nsmall = 4),
      wday = as.factor(wday(date, label = TRUE))
    ) %>%
    select(date, labels, gap, change_prop, wday) %>%
    group_by(labels) %>%
    ungroup() %>%
    bind_cols(
      {.} %>% select(-change_prop, -gap, -wday),
      augment(lm(change_prop ~ wday + gap, data = {.}))
    )
)

### Seasonal decomposition. 
### Frequency and trend are set so that it's not automatically determined.
dl$anom_df <- dl$resid_df %>%
  group_by(labels) %>%
  time_decompose(
    ## not target = "change_prop"
    ## not method = "twitter"
    target = .std.resid, merge = TRUE, method = "stl", 
    frequency = "1 week", trend = "3 months"
  ) %>%
  anomalize(remainder, method = "iqr")

### Draw plot and see which dates are picked out by IQR
dl$anom_plot <- plot_anomalies(
  dl$anom_df %>% mutate(observed = change_prop), ncol = 3, alpha_dots = 0.25
) +
  ylab("Percentage of Changes Per Variable of Interest") +
  xlab("Date") +
  scale_y_continuous(
    labels = function(x) format(x, scientific = FALSE, nsmall = 4)
  ) +
  scale_x_date(breaks = "2 months", date_labels = "%b") +
  guides(color = guide_legend(title = "Observation Anomalous?"))

dl$anom_dates <- dl$anom_df %>%
  arrange(date) %>%
  select(labels, date, anomaly) %>%
  group_by(date) %>%
  filter(!all(anomaly == "No")) %>%
  ungroup() %>%
  spread(key = labels, value = anomaly)

pdf_default(dl$anom_plot) + theme(legend.position = "bottom")
print(as.data.frame(dl$anom_dates))
prop.table(table(factor(
  dl$anom_df$anomaly, levels = c("Yes", "No"), 
  labels = c("Anomaly by IQR", "Normal by IQR")
)))

## Second-stage anomaly detection: Bayesian modelling ==========================
### Multivariate (multiple response variables, intercept/sd correlated)
### Using proportion of changes and not the raw counts, zero-inflated Beta dist.
### Multilevel, group effects by month and day of week
fit <- brm(
  mvbind(Added, Dropped, Changed, dtBirthDate, lVoterUniqueID, 
         szNameFirst, szNameLast, szPartyName, szSitusAddress) ~ 
    gap + (1|p|wday) + (1|q|month_24) + s(gen2018),
  data = wide_df[[2]] %>%
    mutate(
      date = as.Date(date),
      gen2018 = as.numeric(date - as.Date("2018-11-06"))
    ), 
  control = list(adapt_delta = 0.99, max_treedepth = 12),
  chains = 4, cores = 4,
  family = zero_inflated_beta(),
  seed = 1234
)
save(fit, file = paste0("brms_vraudit_", format(Sys.Date(), "%y%m%d"), ".Rda"))

### Posterior predictive plots =================================================
dl$pp_plots <- list(
  Added = pp_check(fit, resp = "Added"),
  Dropped = pp_check(fit, resp = "Dropped"),
  Changed = pp_check(fit, resp = "Changed"),
  DOB = pp_check(fit, resp = "dtBirthDate"),
  ID = pp_check(fit, resp = "lVoterUniqueID"),
  FirstName = pp_check(fit, resp = "szNameFirst"),
  LastName = pp_check(fit, resp = "szNameLast"),
  Party = pp_check(fit, resp = "szPartyName"),
  Address = pp_check(fit, resp = "szSitusAddress")
)
dl$pp_plots <- names(dl$pp_plots) %>% 
  set_names(., .) %>%
  map(
    ~ plot_nolegend(pdf_default(dl$pp_plots[[.x]] + ggtitle(.x)))
  )

multiplot(
  dl$pp_plots[[1]], dl$pp_plots[[2]], dl$pp_plots[[3]], 
  dl$pp_plots[[4]], dl$pp_plots[[5]], dl$pp_plots[[6]], 
  dl$pp_plots[[7]], dl$pp_plots[[8]], dl$pp_plots[[9]],
  cols = 3
)

### Turn simulated predictions into summaries ==================================
pred <- posterior_predict(fit)
pred <- bind_cols(
  wide_df[[2]] %>% 
    select(
      date, Added, Dropped, Changed, dtBirthDate, lVoterUniqueID, 
      szNameFirst, szNameLast, szPartyName, szSitusAddress
    ) %>%
    set_names(c("date", names(dl$pp_plots))),
  seq(8) %>% 
    map(~ apply(pred[, , .x], 2, mean)) %>% 
    bind_cols() %>% 
    set_names(paste0(names(dl$pp_plots), "_mean")),
  seq(8) %>% 
    map(~ apply(pred[, , .x], 2, sd)) %>% 
    bind_cols() %>% 
    set_names(paste0(names(dl$pp_plots), "_sd"))
)

## Visualize which data points are out of the prediction confidence band =======
dl$conf_bands <- names(dl$pp_plots) %>%
  map(
    ~ ggplot(pred, aes(date, !!as.name(.x))) +
      geom_line(color = "darkblue", size = 0.7) +
      geom_line(
        data = pred, aes(date, !!as.name(paste0(.x, "_mean"))), 
        linetype = "dotdash"
      ) +
      geom_ribbon(
        aes(
          ymin = max(
            !!as.name(paste0(.x, "_mean")) - 
              1.96 * !!as.name(paste0(.x, "_sd")), 
            0
          ),
          ymax = !!as.name(paste0(.x, "_mean")) + 
            1.96 * !!as.name(paste0(.x, "_sd"))
        ),
        alpha = 0.3
      ) +
      ylab(paste0(.x, " Changes (%)")) +
      xlab("Date")
  ) %>%
  map(
    ~ pdf_default(.x) + 
      theme(
        axis.title.x = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_blank()
      )
  )
multiplot(
  dl$conf_bands[[1]], dl$conf_bands[[2]], dl$conf_bands[[3]], 
  dl$conf_bands[[4]], dl$conf_bands[[5]], dl$conf_bands[[6]],
  dl$conf_bands[[7]], dl$conf_bands[[8]], dl$conf_bands[[9]],
  cols = 3
)

## Show which dates are these for each variable ================================
pred <- bind_cols(
  pred, 
  names(dl$pp_plots) %>%
    set_names(., paste0(., "_exceed")) %>%
    map(
      ~ pred[[.x]] - 
        (pred[[paste0(.x, "_mean")]] + 1.96 * pred[[paste0(.x, "_sd")]])
    ) %>%
    bind_cols()
)
paste0(names(dl$pp_plots), "_exceed") %>%
  set_names(., .) %>%
  map(
    ~ (
      pred %>% 
        arrange(desc(!!as.name(.x))) %>% 
        filter(!!as.name(.x) > 0)
    )$date
  )

