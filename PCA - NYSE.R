#load dataset
data=read.csv("rates.csv")

data[[date_col_name]] <- as.Date(data[["Date"]], format = "%m/%d/%Y")
data_filtered <- data[data[[date_col_name]] <= as.Date("1996-12-31"), ]

data.pc = data_filtered[-1]
names(data.pc)

#perform PCA on scaled data
pc1 = prcomp(data.pc, scale=T)
summary(pc1)
plot(pc1, type="l")
pc1$rotation
pc1$rotation[,1]
pc1$rotation[,2]

#perform PCA on unscaled data
pc1.u = prcomp(data.pc)
summary(pc1.u)
plot(pc1.u, type="l")
pc1.u$rotation[,1]


# Option 2: Re-run PCA with yields in percentage points
yieldppoints <- data.pc * 100  # Convert 0.0783 → 7.83
pc1_pct <- prcomp(yieldppoints, scale = F)
summary(pc1_pct)
plot(pc1_pct, type="l")
pc1_pct$rotation[,1]