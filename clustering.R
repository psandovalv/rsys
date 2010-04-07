library(quantmod)

DATADIR <- "~/data/yahoo/"

FileName <- function(symbol) {
  ## Resolve a symbol to its actual path
  letter <- strsplit(symbol, split="")[[1]][1]
  filename <- paste(letter, paste(symbol, ".txt", sep=""), sep="/")
  return (filename)
}

FilePath <- function(symbol) {
  ## Get the absolute path of the symbol data
  return (paste(DATADIR, FileName(symbol), sep="/"))
}

Load <- function(symbol) {
  ## Load ohlcv data for the symbol and return an xts object
  z <- read.zoo(FilePath(symbol), sep=",")
  z <- as.xts(z)
  colnames(z) <- c("open", "high", "low", "close", "volume")
  return (z)
}

DailyReturns <- function(symbols, start.year=NULL, end.year=NULL, verbose=FALSE) {
  ## Compute daily log returns
  returns = list()
  for (i in symbols) {

    if (verbose)
      cat("Loading data for symbol ", i, "\n")

    z <- Load(i)
    #z <- try({Load(i)}, silent=T)

    returns[[i]]<- dailyReturn(z$close, type="log")
  }
  df <- as.data.frame(returns)
  colnames(df) <- symbols
  return (df)
}

DailyCorr <- function(symbols, start.year=NULL, end.year=NULL) {
  df <- DailyReturns(symbols, start.year, end.year)
  return (cor(df))
}

RollingReturns <- function(daily.returns, ndays=5) {
  return (rollapply(daily.returns, ndays, sum, na.pad=TRUE, align="right"))
}


### Symbols with no data have been removed
symbols <- c('MMM', 'MO', 'MTB', 'M', 'MRO', 'MAR', 'MMC', 'MI', 'MAS', 'MEE', 'MAT.O', 'MFE', 'MKC', 'MCD', 'MHP', 'MCK', 'MJN', 'MWV', 'MHS', 'MDT', 'MRK', 'MDP', 'MET', 'MU.O', 'MSFT', 'MIL', 'MOLX', 'MON', 'MWW', 'MCO', 'MS', 'MOT', 'MYL.O', 'AES', 'AFL', 'AKS', 'ABT', 'ANF', 'ADBE', 'AMD', 'AET', 'A', 'APD', 'ARG', 'AKAM', 'AA', 'AYE', 'ATI', 'AGN', 'ALL', 'ALTR', 'AMZN', 'AEE', 'AEP', 'AXP', 'AIG', 'AMT', 'AMP', 'ABC', 'AMGN', 'APH', 'APC', 'ADI', 'AON', 'APA', 'AIV', 'APOL', 'AAPL', 'AMAT', 'ADM', 'AIZ', 'AN', 'AZO', 'ADP.O', 'AVB', 'AVY', 'AVP', 'T', 'TEG', 'TAP', 'TROW', 'TE', 'TJX', 'TGT', 'TLAB', 'THC', 'TDC', 'TER', 'TSO', 'TXN', 'TXT', 'TMO', 'TIF', 'TWC', 'TWX', 'TIE', 'TMK', 'TSS', 'TRV', 'TSN', 'BBT', 'BMC.O', 'BHI', 'BLL', 'BAC', 'BCR', 'BAX', 'BDX', 'BBBY', 'BBY', 'BIG', 'BIIB', 'BA', 'BXP', 'BSX', 'BMY', 'BEN', 'BTU', 'BK', 'HRB', 'HCP', 'HAL', 'HOG', 'HAR', 'HRS', 'HIG', 'HAS', 'HCN', 'HNZ', 'HP', 'HSY', 'HES', 'HPQ', 'HD', 'HON', 'HRL', 'HSP', 'HST', 'HCBK', 'HUM', 'HBAN', 'HOT', 'CA.O', 'CBG', 'CBS', 'CF', 'CHRW', 'CI', 'CMS', 'CNX', 'CSX', 'CVS', 'COG', 'CAM', 'CPB', 'COF', 'CAH', 'CFN', 'CCL', 'CAT', 'CELG', 'CNP', 'CTL', 'CEPH', 'CHK', 'CME.O', 'CB', 'CINF', 'CTAS', 'CSCO', 'C', 'CTXS', 'CLF', 'CLX', 'COH', 'CCE', 'CTSH', 'CL', 'CMCSA', 'CMA', 'CSC', 'CPWR', 'CAG', 'COP', 'CEG', 'COST', 'CVH', 'COL', 'CRM', 'KO', 'KLAC', 'K', 'KEY', 'KMB', 'KIM', 'KG', 'KSS', 'KFT', 'KR', 'ED', 'ETFC', 'EMC', 'EOG', 'EQT', 'EMN', 'EK', 'ETN', 'ECL', 'EIX', 'EP', 'ERTS', 'EMR', 'ETR', 'EFX', 'EQR', 'EL', 'EXC', 'EXPE', 'EXPD', 'ESRX', 'STZ', 'STR', 'SAI', 'SCG', 'SLM', 'SWY', 'SNDK', 'SLE', 'SCHW.N', 'SNI', 'SEE', 'SHLD', 'SRE', 'SHW', 'SIAL', 'SPG', 'SII', 'SJM', 'SNA', 'SO', 'SWN', 'SE', 'S', 'STJ', 'SWK', 'SPLS', 'SBUX', 'STT', 'SRCL', 'SYK', 'STI', 'SUN', 'SVU', 'SYMC', 'SYY', 'GLW', 'GME', 'GCI', 'GPS', 'GD', 'GE', 'GIS', 'GPC', 'GNW', 'GENZ', 'GILD', 'GS', 'GR', 'GT', 'GOOG', 'GWW', 'GAS', 'DTV.O', 'DTE', 'DHR', 'DRI', 'DVA', 'DV', 'DF', 'DE', 'DELL', 'DNR', 'DVN', 'DO', 'DFS', 'DISCA', 'DOV', 'DOW', 'DPS', 'DD', 'DUK', 'DNB', 'DHI', 'DGX', 'DIS', 'XRAY', 'XOM', 'X', 'XL', 'XTO', 'XEL', 'XRX', 'XLNX', 'RRD.O', 'RL', 'RSH', 'RRC', 'RTN', 'RHT', 'RF', 'RSG', 'RAI', 'RHI', 'ROK', 'ROP', 'ROST', 'RDC', 'R', 'FLIR', 'FMC', 'FTI', 'FPL', 'FDO', 'FAST', 'FDX', 'FII', 'FIS', 'FITB', 'FHN', 'FSLR', 'FE', 'FISV', 'FLS', 'FLR', 'F', 'FRX', 'FO', 'FCX', 'FTR', 'ITT', 'ITW', 'INTC', 'ICE', 'IPG', 'IBM', 'IFF', 'IGT', 'IP', 'INTU', 'ISRG', 'IVZ', 'IRM', 'JDSU', 'JPM', 'JBL', 'JEC', 'JNS', 'JNJ', 'JNPR.N', 'JWN', 'JCP', 'LLL', 'LSI', 'LH', 'LM', 'LEG', 'LEN', 'LUK', 'LXK', 'LIFE', 'LLY', 'LTD', 'LNC', 'LLTC', 'LMT', 'L', 'LO', 'LOW', 'LUV', 'WFR', 'WMT', 'WAG', 'WPO', 'WM', 'WAT', 'WPI', 'WLP', 'WFC', 'WDC', 'WU', 'WY', 'WHR', 'WFMI', 'WIN.O', 'WEC', 'WYN', 'WYNN', 'PCS', 'PCAR', 'PCG', 'PNC', 'PPG', 'PPL', 'PTV', 'PLL', 'PH', 'PDCO', 'PAYX', 'PBCT', 'POM', 'PEP', 'PKI', 'PFE', 'PM', 'PNW', 'PXD', 'PBI', 'PCL', 'PX', 'PCP', 'PCLN', 'PFG', 'PG', 'PGN', 'PGR', 'PLD', 'PEG', 'PSA', 'PHM', 'PWR', 'NKE', 'NRG', 'NYX', 'NBR', 'NDAQ', 'NOV', 'NSM', 'NTAP', 'NYT', 'NWL', 'NEM', 'NWSA', 'NI', 'NBL', 'NSC', 'NU', 'NTRS', 'NOC', 'NOVL', 'NVLS', 'NUE', 'NVDA', 'ORLY', 'OKE', 'OXY', 'ODP', 'OMC', 'ORCL', 'OI', 'QLGC', 'QCOM', 'Q', 'USB', 'UNP', 'UPS', 'UTX', 'UNH', 'UNM', 'URBN', 'VFC', 'VLO', 'VAR', 'VTR', 'VRSN', 'VIA.B', 'V', 'VNO', 'VMC', 'YHOO', 'YUM', 'ZMH', 'ZION')

