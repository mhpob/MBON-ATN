library(data.table)
library(readxl)

meta <- read_excel(
  "./Data and Imports/telemetry/MAMBON_deployments_master.xlsx",
  sheet = 2,
  skip = 3
)
setDT(meta)
meta[, let(
  deploy = as.POSIXct(
    `DEPLOY_DATE_TIME   (yyyy-mm-ddThh:mm:ss)`,
    format = "%Y-%m-%dT%H:%M:%S",
    tz = 'UTC'
  ),
  recover = as.POSIXct(
    `RECOVER_DATE_TIME (yyyy-mm-ddThh:mm:ss)`,
    format = "%Y-%m-%dT%H:%M:%S",
    tz = 'UTC'
  ),
  receiver = paste(INS_MODEL_NO, INS_SERIAL_NO, sep = '-')
)]

setkey(meta, receiver, deploy, recover)

mbon <- list.files(
  './Data and Imports/telemetry/raw',
  pattern = '^VR2AR.*\\.csv',
  full.names = TRUE
) |>
  lapply(
    fread,
    fill = TRUE,
    col.names = \(x) tolower(gsub('[\\( \\)]', '', x))
  ) |>
  rbindlist()
# drop internal transmitter
mbon <- mbon[!grepl('A69-1601-609', transmitter)]
mbon[,
  station := factor(
    STATION_NO,
    levels = c(
      "MBON1",
      "T-1C",
      'MBON2',
      'MBON3',
      'A-5C',
      'MBON4',
      'MBON5',
      'T-2C',
      'T-3C'
    ),
    ordered = T
  )
]
mbon[, day := as.Date(dateandtimeutc)]


mbon[, dummy := dateandtimeutc]
setkey(mbon, receiver, dateandtimeutc, dummy)


mbon <- foverlaps(mbon, meta[!is.na(recover)], nomatch = 0)

mbon[, .N, by = "STATION_NO"]
unique(mbon, by = "transmitter")
mbon[, .N, by = transmitter][order(-N)] |>
  _[N >= 2, sum(N)]

unique(
  mbon,
  by = c('transmitter', "STATION_NO")
) |>
  _[, .N, by = 'STATION_NO']

mbon[STATION_NO == 'T-1C', .N, by = transmitter][order(-N)] |>
  _[1:50]
unique(mbon[grepl('A69-9001-500', transmitter)]$transmitter)


library(ggplot2)

date_key <- expand.grid(
  day = seq.Date(min(mbon$day), max(mbon$day)),
  station = levels(mbon$station)
)

plot_trans <- mbon[, .N, by = .(transmitter, station, day)][,
  .N,
  by = .(station, day)
] |>
  _[date_key, , on = .(day, station)] |>
  _[station != 'MBON5', .(N = fifelse(is.na(N), 0, N), station, day)]
plot_trans <- plot_trans |>
  ggplot() +
  geom_line(aes(x = day, y = N)) +
  labs(x = NULL, y = NULL, subtitle = 'Transmitters') +
  facet_wrap(~station, ncol = 1, scales = 'free_y') +
  theme_minimal()

plot_dets <- mbon[, .N, by = .(station, day)] |>
  _[date_key, , on = .(day, station)] |>
  _[station != 'MBON5', .(N = fifelse(is.na(N), 0, N), station, day)]
plot_dets <- plot_dets |>
  ggplot() +
  geom_line(aes(x = day, y = N)) +
  labs(x = NULL, y = NULL, subtitle = 'Detections') +
  facet_wrap(~station, ncol = 1, scales = 'free_y') +
  theme_minimal()

library(patchwork)
plot_trans + plot_dets
