library(dplyr)
library(data.table)
library(ggplot2)
library(reshape2)
library(ggrepel)
library(maps)
permittedevents <- c("ASTRONOMICAL LOW TIDE",
                     "AVALANCHE",
                     "BLIZZARD",
                     "COASTAL FLOOD",
                     "COLD/WIND CHILL",
                     "DEBRIS FLOW",
                     "DENSE FOG",
                     "DENSE SMOKE",
                     "DROUGHT",
                     "DUST DEVIL",
                     "DUST STORM",
                     "EXCESSIVE HEAT",
                     "EXTREME COLD/WIND CHILL",
                     "FLASH FLOOD",
                     "FLOOD",
                     "FROST/FREEZE",
                     "FUNNEL CLOUD",
                     "FREEZING FOG",
                     "HAIL",
                     "HEAT",
                     "HEAVY RAIN",
                     "HEAVY SNOW",
                     "HIGH SURF",
                     "HIGH WIND",
                     "HURRICANE (TYPHOON)",
                     "ICE STORM",
                     "LAKE-EFFECT",
                     "SNOW",
                     "LAKESHORE FLOOD",
                     "LIGHTNING", 
                     "MARINE HAIL",
                     "MARINE HIGH WIND",
                     "MARINE STRONG WIND",
                     "MARINE THUNDERSTORM WIND",
                     "RIP CURRENT",
                     "SEICHE",
                     "SLEET",
                     "STORM SURGE/TIDE",
                     "STRONG WIND",
                     "THUNDERSTORM WIND",
                     "TORNADO",
                     "TROPICAL DEPRESSION",
                     "TROPICAL STORM",
                     "TSUNAMI", 
                     "VOLCANIC ASH",
                     "WATERSPOUT",
                     "WILDFIRE",
                     "WINTER STORM",
                     "WINTER WEATHER")

# download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2",
#              "Storm_Data.bz2")
# 
# df <- read.csv("Storm_Data.bz2", stringsAsFactors = F)

dt <- data.table(df)

dt[, EVTYPE := toupper(EVTYPE)]

dt[which(EVTYPE %in% c("TSTM WIND", 
                       "TSTM WIND/HAIL",
                       "GUSTY THUNDERSTORM WIND",
                       "GUSTY THUNDERSTORM WINDS",
                       "SEVERE THUNDERSTORMS",
                       "THUNDERSTORMS",
                       "THUNDERSTORM",
                       "TSTM WIND (G45)",
                       "TSTM WIND (G40)",
                       " TSTM WIND",
                       "THUNDERSTORM W INDS",
                       "TSTM",
                       "TSTMW",
                       "TSTM WND",
                       "TSTM WINDS",
                       "TSTM WIND G45",
                       "TSTM WIND (G35)",
                       " TSTM WIND (G45)",
                       "TSTM WIND  (G45)",
                       "TSTM WIND (41)",
                       "TSTM WIND 45",
                       "TSTM WIND 52",
                       "TSTM WIND AND LIGHTNING",
                       "TSTM HEAVY RAIN",
                       "TSTM WIND 40",
                       "THUNDERSTORM WINS",
                       "THUNDERSTORM WINDS/HAIL",
                       "SEVERE THUNDERSTORM WINDS",
                       "MICROBURST",
                       "WET MICROBURST",
                       "WET MICOBURST",
                       "MICROBURST WINDS",
                       "DOWNBURST",
                       "DOWNBURST WINDS",
                       "THUNDERSTORMW WINDS",
                       "THUNDERSTORM WIND G50",
                       "THUNDERTORM WINDS",
                       "SEVERE THUNDERSTORM",
                       "THUNDERSTORM  WINDS",
                       "TSTM WIND 65)",
                       "TSTM WIND 55",
                       "THUNDERSTORMW 50",
                       "TSTM WIND 52",
                       "TSTM WIND G58",
                       "TUNDERSTORM WIND",
                       "THUNDERSTORMS WIND",
                       "TSTM WIND 51",
                       "TSTM WIND 52",
                       "TSTM WIND 55",
                       "TSTM WIND 50",
                       "THUNDERSTORM WIND 56",
                       "THUNDERSTORM WIND 69",
                       "THUNDERSTORM WINDS 53",
                       "THUNDERSTORM WIND 59",
                       "TSTM WIND DAMAGE",
                       "STORM FORCE WINDS",
                       "THUNDERSTORMWINDS",
                       "THUNDERSTORMW",
                       "THUDERSTORM WINDS",
                       "THUNDERTSORM WIND",
                       "THUNDERSTROM WIND",
                       "THUNDERSTORM DAMAGE",
                       "THUNDERSTORMS WINDS",
                       "THUNDESTORM WINDS",
                       "THUNERSTORM WINDS",
                       "THUNDERSTORM WIND 59 MPH",
                       "THUNDERSTORM WIND/ TREE",
                       "THUNDERSTORM WINDS 63 MPH",
                       "THUNDERSTORM WIND 65 MPH",
                       "THUNDERSTORM WIND 59 MPH.",
                       "THUNDERSTORM WINDSHAIL",
                       "THUNDERSTORM HAIL",
                       "THUNDERSTORM DAMAGE TO",
                       "THUNDERSTORM WIND.",
                       "THUNDERESTORM WINDS",
                       "THUNDEERSTORM WINDS",
                       "THUNDERSTROM WINDS",
                       "THUNDERSTORM WINDS",
                       "THUNDERSTORM WINDS.",
                       "THUNDERSTORM WINDS/HAIL",
                       "THUNDERSTORM WINDS/ HAIL",
                       "THUNDERSTORM WINDS      LE CEN",
                       "THUNDERSTORM WINDS G60",
                       "THUNDERSTORM WINDS 62",
                       "THUNDERSTORM WIND 52",
                       "THUNDERSTORM WINDS/HEAVY RAIN",
                       "THUNDERSTORM WINDS 52",
                       "THUNDERSTORM WINDS 50",
                       "THUNDERSTORM WINDS AND",
                       "THUNDERSTORM WINDS 2",
                       "THUNDERSTORM WINDS SMALL STREA",
                       "THUNDERSTORM WINDS HEAVY RAIN",
                       "THUNDERSTORM WINDS/FLOODING",
                       "THUNDERSTORM WINDS/FLASH FLOOD",
                       "THUNDERSTORM WINDS FUNNEL CLOU",  
                       "THUNDERSTORM WINDS HAIL",
                       "THUNDERSTORM WINDSS",
                       "THUNDERSTORM WINDS LIGHTNING",
                       "THUNDERSTORM WINDS/HAIL",
                       "THUNDERSTORM WINDS/ FLOOD",
                       "THUNDERSTORM WINDS/FUNNEL CLOU",
                       "THUNDERSTORM WINDS G",
                       "THUNDERSTORM WIND (G40)",
                       "THUNDERSTORM WIND 50",
                       "THUNDERSTORM WIND/HAIL",
                       "THUNDERSTORM WIND/LIGHTNING",
                       "THUNDERSTORM WIND TREES",
                       "THUNDERSTORM WIND 60 MPH",
                       "THUNDERSTORM WIND 65MPH",
                       "THUNDERSTORM WIND/AWNING",
                       "THUNDERSTORM WIND/ TREES",
                       "THUNDERSTORM WIND 98 MPH",
                       "THUNDERSTORM WIND G61",
                       "THUNDERSTORM WIND G51",
                       "THUNDERSTORM WIND G55",
                       "THUNDERSTORM WIND G60",
                       "THUNDERSTORM WIND G52",
                       "THUNDERSTORM WINDS53",
                       "THUNDERSTORM WINDS 60",
                       "THUNDERSTORM WINDS 61",
                       "THUNDERSTORM WINDS 13")), 
   EVTYPE := "THUNDERSTORM WIND"]

dt[which(EVTYPE %in% c("STRONG WINDS",
                       "GUSTY WINDS",
                       "GUSTY WIND",
                       "WHIRLWIND",
                       "WIND ADVISORY",
                       "NON TSTM WIND",
                       "NON-TSTM WIND",
                       "GUSTY LAKE WIND",
                       "WIND DAMAGE",
                       "WND",
                       "WIND GUSTS",
                       "NON-SEVERE WIND DAMAGE",
                       "GUSTY WIND/HAIL",
                       "HIGH WINDS",
                       "WINDS",
                       "HIGH  WINDS",
                       "WIND",
                       "WIND AND WAVE",
                       "HIGH WINDS 57",
                       "HIGH WINDS 58",
                       "HIGH WINDS 66",
                       "HIGH WINDS 76",
                       "HIGH WINDS 63",
                       "HIGH WINDS 67",
                       "HIGH WINDS 80",
                       "HIGH WINDS 73",
                       "HIGH WINDS 55",
                       "HIGH WIND DAMAGE",
                       "HIGH",
                       " WIND",
                       "HIGH WIND 63",
                       "HIGH WINDS 82",
                       "HIGH WIND (G40)",
                       "STRONG WIND GUST",
                       "GUSTY WIND/HVY RAIN",
                       "HIGH WIND AND HIGH TIDES",
                       "WIND CHILL/HIGH WIND",
                       "GUSTY WIND/RAIN",
                       "HIGH WINDS HEAVY RAINS",
                       "HIGH WIND 48",
                       "HIGH WINDS/FLOODING",
                       "HIGH WINDS AND WIND CHILL",
                       "HIGH WIND/LOW WIND CHILL",
                       "HIGH WIND/WIND CHILL/BLIZZARD",
                       "HIGH WINDS/COLD",
                       "SEVERE TURBULENCE",
                       "ICE/STRONG WINDS",
                       "HIGH WINDS/SNOW",
                       "HIGH WINDS/",
                       "WIND STORM",
                       "HIGH WIND 70")),
EVTYPE := "HIGH WIND"]

dt[which(EVTYPE %in% c("MONTHLY PRECIPITATION",
                       "MIXED PRECIPITATION",
                       "RAIN",
                       "COOL AND WET",
                       "NORMAL PRECIPITATION",
                       "ABNORMALLY WET",
                       "HVY RAIN",
                       "RAIN AND WIND",
                       "HEAVY RAIN EFFECTS",
                       "EXCESSIVE RAIN",
                       "RECORD/EXCESSIVE RAINFALL",
                       "HEAVY RAIN/SEVERE WEATHER",
                       "EXCESSIVE WETNESS",
                       "EXTREMELY WET",
                       "RECORD RAINFALL",
                       "UNSEASONABLY WET",
                       "LOCALLY HEAVY RAIN",
                       "HEAVY RAINFALL",
                       "PROLONGED RAIN",
                       "MONTHLY RAINFALL",
                       "EXCESSIVE RAINFALL",
                       "HEAVY RAINS",
                       "HEAVY RAINS/FLOODING",
                       "HEAVY RAIN/MUDSLIDES/FLOOD",
                       "HEAVY RAIN/WIND",
                       "UNSEASONAL RAIN",
                       "RECORD PRECIPITATION",
                       "RAIN (HEAVY)",
                       "HEAVY RAIN AND WIND",
                       "HEAVY PRECIPITATION",
                       "WET MONTH",
                       "WET YEAR",
                       "HEAVY RAIN/SMALL STREAM URBAN",
                       "HEAVY RAIN/URBAN FLOOD",
                       "HEAVY RAIN/FLOODING",
                       "HEAVY SHOWER",
                       "RAINSTORM",
                       "HIGH WINDS/HEAVY RAIN",
                       "HEAVY PRECIPATATION",
                       "HEAVY SHOWERS",
                       "HEAVY RAIN/SNOW",
                       "TORRENTIAL RAINFALL",
                       "TORRENTIAL RAIN",
                       "RAIN DAMAGE",
                       "MIXED PRECIP",
                       "EXCESSIVE PRECIPITATION",
                       "RAIN/WIND",
                       "WET WEATHER")),
   EVTYPE := "HEAVY RAIN"]

dt[which(EVTYPE %in% c("TORNADO F0",
                       "TORNADO F1",
                       "TORNADO F2",
                       "TORNADO F3",
                       "TORNDAO",
                       "TORNADOS",
                       "TORNADOES",
                       "WATERSPOUT/TORNADO",
                       "WATERSPOUT/ TORNADO",
                       "WATERSPOUT TORNADO",
                       "WATERSPOUT-TORNADO",
                       "TORNADOES, TSTM WIND, HAIL",
                       "TORNADO/WATERSPOUT",
                       "GUSTNADO",
                       "GUSTNADO AND",
                       "COLD AIR TORNADO")),
   EVTYPE := "TORNADO"]

dt[which(EVTYPE %in% c("LIGHTNING FIRE",
                       "LIGNTNING",
                       "LIGHTNING.",
                       "LIGHTNING AND WINDS",
                       "LIGHTNING  WAUSEON",
                       "LIGHTNING DAMAGE",
                       "LIGHTNING THUNDERSTORM WINDS",
                       "LIGHTNING INJURY",
                       "LIGHTNING AND THUNDERSTORM WIN",
                       "HEAVY RAIN/LIGHTNING",
                       "LIGHTNING/HEAVY RAIN",
                       "LIGHTING",
                       " LIGHTNING",
                       "LIGHTNING THUNDERSTORM WINDSS",
                       "LIGHTNING AND HEAVY RAIN")),
   EVTYPE := "LIGHTNING"]

dt[which(EVTYPE %in% c("WINTER STORM/HIGH WINDS",
                       "WINTER STORM/HIGH WIND",
                       "WINTER STORM HIGH WINDS",
                       "WINTER STORMS"  )),
   EVTYPE := "WINTER STORM"]

dt[which(EVTYPE %in% c("ICE STORM/FLASH FLOOD",
                       "ICE STORM AND SNOW",
                       "SNOW/ICE STORM" ,
                       "SNOW AND ICE STORM",
                       "GLAZE/ICE STORM")),
   EVTYPE := "ICE STORM"]

dt[which(EVTYPE %in% c("DUSTSTORM",
                       "HIGH WINDS DUST STORM",
                       "DUST STORM/HIGH WINDS")),
   EVTYPE := "DUST STORM"]

dt[which(EVTYPE %in% c("BLOWING DUST",
                       "DUST DEVEL",
                       "LANDSPOUT",
                       "SAHARAN DUST",
                       "DUST DEVIL WATERSPOUT")),
   EVTYPE := "DUST DEVIL"]



dt[which(EVTYPE %in% c(" WATERSPOUT",
                       "WATERSPOUT/",
                       "WAYTERSPOUT",
                       "WATERSPOUTS",
                       "WATERSPOUT-",
                       "WATER SPOUT",
                       "WATERSPOUT FUNNEL CLOUD")),
   EVTYPE := "WATERSPOUT"]

dt[which(EVTYPE %in% c("ABNORMALLY DRY",
                       "UNSEASONABLY DRY",
                       "DRY MICROBURST WINDS",
                       "DRY MIRCOBURST WINDS",
                       "DRY MICROBURST 58",
                       "DRY MICROBURST 84",
                       "DRY MICROBURST 61",
                       "DRY MICROBURST 50",
                       "DRY MICROBURST 53",
                       "DRY MICROBURST 84",
                       "DRY MICROBURST",
                       "RECORD LOW RAINFALL",
                       "DRY SPELL",
                       "DRY CONDITIONS",
                       "VERY DRY",
                       "RECORD DRYNESS",
                       "UNSEASONABLY WARM AND DRY",
                       "DRY WEATHER",
                       "EXCESSIVELY DRY",
                       "DRYNESS",
                       "DRIEST MONTH",
                       "HOT WEATHER",
                       "HOT AND DRY",
                       "DROUGHT/EXCESSIVE HEAT",
                       "RECORD DRY MONTH",
                       "HEAT/DROUGHT",
                       "DRY",
                       "BELOW NORMAL PRECIPITATION")),
   EVTYPE := "DROUGHT"]

dt[which(EVTYPE %in% c("RIP CURRENTS",
                       "RIP CURRENTS/HEAVY SURF",
                       "RIP CURRENTS HEAVY SURF")), 
   EVTYPE := "RIP CURRENT"]

dt[which(EVTYPE %in% c("AVALANCE",
                       "HEAVY SNOW/BLIZZARD/AVALANCHE")), 
   EVTYPE := "AVALANCHE"]

dt[which(EVTYPE %in% c("RECORD WARMTH",
                       "RECORD HEAT",
                       "EXCESSIVE HEAT/DROUGHT",
                       "HEAT WAVE DROUGHT",
                       "DRY HOT WEATHER",
                       "HEAT DROUGHT",
                       "HOT PATTERN",
                       "HOT/DRY PATTERN",
                       "DRY PATTERN",
                       "WARM DRY CONDITIONS",
                       "RECORD TEMPERATURE",
                       "RECORD TEMPERATURES",
                       "HYPERTHERMIA/EXPOSURE",
                       "ABNORMAL WARMTH",
                       "RECORD WARM",
                       "UNUSUAL/RECORD WARMTH",
                       "RECORD HIGH",
                       "RECORD HIGH TEMPERATURE",
                       "RECORD HIGH TEMPERATURES",
                       "HIGH TEMPERATURE RECORD",
                       "TEMPERATURE RECORD",
                       "RECORD WARM TEMPS.",
                       "EXTREME HEAT",
                       "RECORD HEAT WAVE",
                       "RECORD/EXCESSIVE HEAT" )), 
   EVTYPE := "EXCESSIVE HEAT"]

dt[which(EVTYPE %in% c("UNSEASONABLY WARM",
                       "PROLONG WARMTH",
                       "UNSEASONABLY HOT",
                       "VERY WARM",
                       "UNSEASONABLY WARM/WET",
                       "UNUSUALLY WARM",
                       "UNSEASONABLY WARM & WET",
                       "UNSEASONABLY WARM AND DRY",
                       "UNSEASONABLY WARM YEAR",
                       "UNUSUAL WARMTH",
                       "HEAT WAVE",
                       "HOT SPELL",
                       "HOT WEATHER",
                       "WARM WEATHER",
                       "HEAT WAVES" )), 
   EVTYPE := "HEAT"]


dt[which(EVTYPE %in% c("EXTREME COLD", 
                       "BLOWING SNOW & EXTREME WIND CH",
                       "SEVERE COLD",
                       "COLD WAVE",
                       "EXTREME WINDCHILL",
                       "EXTREME WIND CHILLS",
                       "RECORD COLD/FROST",
                       "BITTER WIND CHILL",
                       "EXTREME/RECORD COLD",
                       "UNUSUALLY COLD",
                       "WIND CHILL",
                       "UNSEASONABLY COLD",
                       "PROLONG COLD",
                       "EXTREME WINDCHILL TEMPERATURES",
                       "RECORD COLD",
                       "BITTER WIND CHILL TEMPERATURES",
                       "RECORD  COLD",
                       "EXTREME WIND CHILL",
                       "SNOW/ BITTER COLD",
                       "RECORD COOL",
                       "EXTENDED COLD",
                       "EXCESSIVE COLD",
                       "RECORD COLD AND HIGH WIND",
                       "EXTREME WIND CHILL/BLOWING SNO",
                       "PROLONG COLD/SNOW",
                       "RECORD LOW",
                       "LOW TEMPERATURE RECORD",
                       "HIGH WIND/WIND CHILL",
                       "BLOWING SNOW- EXTREME WIND CHI",
                       "SNOW- HIGH WIND- WIND CHILL",
                       "LOW WIND CHILL")), 
   EVTYPE := "EXTREME COLD/WIND CHILL"]

dt[which(EVTYPE %in% c("WINTRY MIX", 
                       "COLD WEATHER",
                       "LOW TEMPERATURE",
                       "COLD TEMPERATURE",
                       "EARLY FREEZE",
                       "COLD",
                       "WINTER WEATHER MIX",
                       "COLD WIND CHILL TEMPERATURES",
                       "UNSEASONABLY COOL",
                       "UNSEASONAL LOW TEMP",
                       "HYPOTHERMIA",
                       "HYPOTHERMIA/EXPOSURE",
                       "COLD/WINDS",
                       "COLD TEMPERATURES",
                       "UNSEASONABLY COOL & WET",
                       "WINTER MIX",
                       "WINTERY MIX",
                       "COOL SPELL",
                       "COLD AND FROST",
                       "FOG AND COLD TEMPERATURES",
                       "COLD AND WET CONDITIONS",
                       "UNSEASONABLE COLD")), 
   EVTYPE := "COLD/WIND CHILL"]

dt[which(EVTYPE %in% c("URBAN/SML STREAM FLD",
                       "URBAN/SMALL STREAM  FLOOD",
                       "URBAN/SML STREAM FLDG",
                       "SNOWMELT FLOODING",
                       "TIDAL FLOODING",
                       "TIDAL FLOOD",
                       "RIVER FLOODING",
                       "RAPIDLY RISING WATER",
                       "URBAN FLOOD",
                       "MAJOR FLOOD",
                       "ICE JAM FLOODING",
                       "RIVER FLOOD",
                       "URBAN/STREET FLOODING",
                       "STREET FLOODING",
                       "FLOOD/FLASH FLOOD",
                       "FLOODING",
                       "SMALL STREAM FLOODING",
                       "SMALL STREAM FLOOD",
                       "URBAN/SMALL STREAM FLOODING",
                       "URBAN AND SMALL STREAM FLOOD",
                       "URBAN AND SMALL STREAM",
                       "FLOOD/FLASH",
                       "FLOOD/RIVER FLOOD",
                       "MUD SLIDES URBAN FLOODING",
                       "HIGHWAY FLOODING",
                       "FLOOD/FLASH/FLOOD",
                       "FLOOD/STRONG WIND",
                       "URBAN/SMALL STRM FLDG",
                       "SML STREAM FLD",
                       "URBAN FLOODING",
                       "DROWNING",
                       "URBAN/SMALL STREAM FLOODING",
                       "URBAN FLOODS",
                       "URBAN/SMALL STREAM FLOOD",
                       "URBAN SMALL STREAM FLOOD",
                       "URBAN/SMALL STREAM",
                       "FLOOD/RAIN/WINDS",
                       "URBAN SMALL",
                       "URBAN/SMALL",
                       "SMALL STREAM",
                       "URBAN AND SMALL",
                       "SMALL STREAM/URBAN FLOOD",
                       "SMALL STREAM URBAN FLOOD",
                       "STREAM FLOODING",
                       "URBAN FLOOD LANDSLIDE",
                       "FLOOD & HEAVY RAIN",
                       "MINOR FLOODING",
                       "MINOR FLOOD",
                       "FLOOD WATCH/",
                       "URBAN/SMALL FLOODING",
                       "URBAN AND SMALL STREAM FLOODIN",
                       "THUNDERSTORM WINDS URBAN FLOOD",
                       "FLOODING/HEAVY RAIN",
                       "LOCAL FLOOD",
                       "BREAKUP FLOODING",
                       "HEAVY RAIN AND FLOOD",
                       "SMALL STREAM AND URBAN FLOODIN",
                       "SMALL STREAM AND URBAN FLOOD",
                       "RURAL FLOOD",
                       "SMALL STREAM AND",
                       "FLOODS",
                       "STREET FLOOD",
                       "LAKE FLOOD",
                       "BEACH FLOOD",
                       "ICE JAM FLOOD (MINOR",
                       "HEAVY RAIN; URBAN FLOOD WINDS;",
                       "URBAN AND SMALL STREAM FLOOD")), 
   EVTYPE := "FLOOD"]

dt[which(EVTYPE %in% c(" FLASH FLOOD",
                       "FLASH FLOODING",
                       "FLASH FLOOODING",
                       "FLASH FLOOD/FLOOD",
                       "FLASH FLOODING/THUNDERSTORM WI",
                       "FLASH FLOODS",
                       "DAM BREAK",
                       "FLASH FLOOD LANDSLIDES",
                       "FLASH FLOOD/LANDSLIDE",
                       "FLASH FLOODING/FLOOD",
                       "FLASH FLOOD/ FLOOD",
                       "FLASH FLOOD/",
                       "FLOOD/RAIN/WIND",
                       "FLASH FLOOD WINDS",
                       "LOCAL FLASH FLOOD",
                       "FLOOD/FLASH FLOODING",
                       "FLASH FLOOD FROM ICE JAMS",
                       "FLASH FLOOD - HEAVY RAIN",
                       "FLASH FLOOD/ STREET",
                       "DAM FAILURE",
                       "FLOOD FLOOD/FLASH",
                       "FLOOD/FLASHFLOOD",
                       "FLOOD FLASH",
                       "FLASH FLOOD/HEAVY RAIN",
                       "RIVER AND STREAM FLOOD")), 
   EVTYPE := "FLASH FLOOD"]

dt[which(EVTYPE %in% c("COASTAL FLOODING",
                       "CSTL FLOODING/EROSION",
                       "COASTAL FLOODING/EROSION",
                       "BEACH EROSION",
                       "COASTAL  FLOODING/EROSION",
                       "HEAVY SURF COASTAL FLOODING",
                       "COASTAL EROSION",
                       "EROSION/CSTL FLOOD",
                       "COASTALFLOOD",
                       "BEACH EROSIN",
                       "BEACH EROSION/COASTAL FLOOD",
                       " COASTAL FLOOD",
                       "COASTAL SURGE",
                       "HIGH WINDS/COASTAL FLOOD",
                       "COASTAL/TIDAL FLOOD")),
   EVTYPE := "COASTAL FLOOD"]

dt[which(EVTYPE %in% c("WILD/FOREST FIRE",
                       "BRUSH FIRE",
                       "WILD FIRES",
                       "WILDFIRES",
                       "FOREST FIRES",
                       "GRASS FIRES",
                       "RED FLAG FIRE WX",
                       "WILD/FOREST FIRES",
                       "BRUSH FIRES")), 
   EVTYPE := "WILDFIRE"]

dt[which(EVTYPE %in% c("GLAZE",
                       "ICE FOG",
                       "GLAZE ICE")), 
   EVTYPE := "FREEZING FOG"]

dt[which(EVTYPE %in% c("MARINE TSTM WIND")), 
   EVTYPE := "MARINE THUNDERSTORM WIND"]

dt[which(EVTYPE %in% c("LANDSLIDE",
                       "MUDSLIDE",
                       "MUD SLIDE",
                       "MUD SLIDES",
                       "TORNADO DEBRIS",
                       "ROCK SLIDE",
                       "LANDSLUMP",
                       "LANDSLIDES",
                       "MUDSLIDES",
                       "MUDSLIDE/LANDSLIDE",
                       "LANDSLIDE/URBAN FLOOD",
                       "MUD/ROCK SLIDE"
)), 
EVTYPE := "DEBRIS FLOW"]

dt[which(EVTYPE %in% c("COASTALSTORM",
                       "COASTAL STORM",
                       "TROPICAL STORM DEAN",
                       "TROPICAL STORM ALBERTO",
                       "TROPICAL STORM GORDON",
                       "TROPICAL STORM JERRY")), 
   EVTYPE := "TROPICAL STORM"]

dt[which(EVTYPE %in% c("FUNNEL CLOUDS",
                       "FUNNEL CLOUD.",
                       "FUNNELS",
                       "WALL CLOUD",
                       "ROTATING WALL CLOUD",
                       "FUNNEL",
                       "FUNNEL CLOUD/HAIL",
                       "WALL CLOUD/FUNNEL CLOUD",
                       "LARGE WALL CLOUD",
                       "COLD AIR FUNNEL",
                       "COLD AIR FUNNELS")), 
   EVTYPE := "FUNNEL CLOUD"]

dt[which(EVTYPE %in% c("WINTER WEATHER/MIX")), 
   EVTYPE := "WINTER WEATHER"]

dt[which(EVTYPE %in% c("STORM SURGE")),
   EVTYPE := "STORM SURGE/TIDE"]

dt[which(EVTYPE %in% c("HEAVY SURF/HIGH SURF", 
                       "HIGH WAVES",
                       "HIGH WIND AND SEAS",
                       "HURRICANE-GENERATED SWELLS",
                       "ASTRONOMICAL HIGH TIDE",
                       "HAZARDOUS SURF",
                       "HIGH SURF ADVISORIES",
                       "HIGH SURF ADVISORY",
                       "   HIGH SURF ADVISORY",
                       "HIGH SEAS",
                       "HIGH WINDS/SNOW",
                       "HIGH WATER",
                       "ROUGH SEAS",
                       "HEAVY SEAS",
                       "HIGH SWELLS",
                       "HIGH  SWELLS",
                       "HEAVY SURF AND WIND",
                       "HEAVY SURF",
                       "HIGH TIDES",
                       "HIGH WIND/SEAS",
                       "ROUGH SURF",
                       "HEAVY SWELLS",
                       "HEAVY RAIN/HIGH SURF")), 
   EVTYPE := "HIGH SURF"]

dt[which(EVTYPE %in% c("ROGUE WAVE")), 
   EVTYPE := "TSUNAMI"]

dt[which(EVTYPE %in% c("ICESTORM/BLIZZARD",
                       "BLIZZARD AND HEAVY SNOW" ,
                       "BLIZZARD SUMMARY",
                       "BLIZZARD/WINTER STORM",
                       "HEAVY SNOW/BLIZZARD",
                       "GROUND BLIZZARD",
                       "BLIZZARD WEATHER",
                       "HIGH WIND/BLIZZARD",
                       "BLIZZARD/HEAVY SNOW",
                       "HIGH WIND/BLIZZARD/FREEZING RA",
                       "BLIZZARD/FREEZING RAIN",
                       "BLIZZARD AND EXTREME WIND CHIL",
                       "BLIZZARD/HIGH WIND",
                       "HIGH WIND/ BLIZZARD")), 
   EVTYPE := "BLIZZARD"]

dt[which(EVTYPE %in% c("SLEET STORM",
                       "FREEZING SPRAY",
                       "SNOW/SLEET/FREEZING RAIN",
                       "FREEZING DRIZZLE AND FREEZING",
                       "FREEZING RAIN AND SNOW",
                       "FREEZING RAIN SLEET AND LIGHT",
                       "SLEET/ICE STORM",
                       "SLEET & FREEZING RAIN",
                       "SLEET/FREEZING RAIN",
                       "FREEZING RAIN SLEET AND",
                       "SLEET/SNOW",
                       "SNOW FREEZING RAIN",
                       "FREEZING RAIN/SNOW",
                       "SLEET/RAIN/SNOW",
                       "SNOW/RAIN/SLEET",
                       "FREEZING RAIN AND SLEET")), 
   EVTYPE := "SLEET"]

dt[which(EVTYPE %in% c("HURRICANE", 
                       "HURRICANE EMILY",
                       "HURRICANE GORDON",
                       "HURRICANE/TYPHOON",
                       "TYPHOON",
                       "HURRICANE OPAL/HIGH WINDS",
                       "HURRICANE OPAL",
                       "HURRICANE ERIN",
                       "HURRICANE EDOUARD",
                       "HURRICANE FELIX")), 
   EVTYPE := "HURRICANE (TYPHOON)"]
dt[which(EVTYPE %in% c("LAKE-EFFECT SNOW",
                       "LAKE EFFECT SNOW")), 
   EVTYPE := "LAKE-EFFECT"]

dt[which(EVTYPE %in% c("FOG",
                       "SMOKE",
                       "PATCHY DENSE FOG")), 
   EVTYPE := "DENSE FOG"]

dt[which(EVTYPE %in% c("VOLCANIC ASHFALL",
                       "VOLCANIC ERUPTION",
                       "VOLCANIC ASH PLUME")), 
   EVTYPE := "VOLCANIC ASH"]

dt[which(EVTYPE %in% c("ICY ROADS",
                       "BLACK ICE",
                       "FREEZING RAIN",
                       "FREEZE",
                       "HARD FREEZE",
                       "FROST",
                       "ICE",
                       "ICE ON ROAD",
                       "FREEZING DRIZZLE",
                       "LIGHT FREEZING RAIN",
                       "PATCHY ICE",
                       "FREEZING RAIN/SLEET",
                       "ICE ROADS",
                       "AGRICULTURAL FREEZE",
                       "FIRST FROST",
                       "LATE FREEZE",
                       "DAMAGING FREEZE",
                       "EARLY FROST",
                       "FROST\\FREEZE")), 
EVTYPE := "FROST/FREEZE"]


dt[which(EVTYPE %in% c("LIGHT SNOW",
                       "SNOW SQUALLS",
                       "SNOW SHOWERS",
                       "ICE/SNOW",
                       "FIRST SNOW",
                       "EARLY SNOWFALL",
                       "MODERATE SNOWFALL",
                       "SNOW/SLEET",
                       "SNOW SLEET",
                       "FALLING SNOW/ICE",
                       "ACCUMULATED SNOWFALL",
                       "SNOW/BLOWING SNOW",
                       "SNOW/FREEZING RAIN",
                       "LATE SEASON SNOW",
                       "SNOW AND SLEET",
                       "SNOW ADVISORY",
                       "UNUSUALLY LATE SNOW",
                       "SNOW AND ICE",
                       "LIGHT SNOW/FREEZING PRECIP",
                       "MONTHLY SNOWFALL",
                       "BLOWING SNOW",
                       "RAIN/SNOW",
                       "MOUNTAIN SNOWS",
                       "SNOW/ICE",
                       "THUNDERSNOW SHOWER",
                       "THUNDERSNOW",
                       "COLD AND SNOW",
                       "EARLY SNOW",
                       "SEASONAL SNOWFALL",
                       "LIGHT SNOW/FLURRIES",
                       "SNOW ACCUMULATION",
                       "DRIFTING SNOW",
                       "LIGHT SNOWFALL",
                       "LATE SEASON SNOWFALL",
                       "LATE SNOW",
                       "SNOW AND COLD",
                       "SNOW\\COLD",
                       "SNOW/SLEET/RAIN",
                       "SNOW/COLD",
                       "SNOWSTORM",
                       "SNOW AND WIND",
                       "SNOW/HIGH WINDS",
                       "SNOW/RAIN",
                       "ICE AND SNOW",
                       "LIGHT SNOW AND SLEET",
                       "SNOW- HIGH WIND- WIND CHILL",
                       "WET SNOW",
                       "SNOW/ ICE",
                       "BLOWING SNOW/EXTREME WIND CHIL",
                       "MODERATE SNOW")),
   EVTYPE := "SNOW"]

dt[which(EVTYPE %in% c("RECORD SNOW",
                       "HEAVY SNOW   FREEZING RAIN",
                       "HEAVY SNOW/WINTER STORM",
                       "HEAVY SNOW & ICE",
                       "HEAVY SNOW/SLEET",
                       "HEAVY SNOW/HIGH WIND",
                       "HEAVY SNOW AND ICE",
                       "HEAVY SNOW/BLOWING SNOW",
                       "HEAVY SNOW/ICE",
                       "HEAVY SNOW/HIGH WINDS & FLOOD",
                       "HEAVY SNOW AND HIGH WINDS",
                       "HEAVY SNOW AND STRONG WINDS",
                       "RECORD SNOW/COLD",
                       "HEAVY WET SNOW",
                       "SNOW AND HEAVY SNOW",
                       "HEAVY SNOW ANDBLOWING SNOW",
                       "SNOW/HEAVY SNOW",
                       "RECORD SNOWFALL",
                       "EXCESSIVE SNOW",
                       "NEAR RECORD SNOW",
                       "HEAVY SNOW SQUALLS",
                       "HEAVY SNOW/HIGH WINDS",
                       "HEAVY SNOW SHOWER",
                       "RECORD WINTER SNOW",
                       "HIGH WIND AND HEAVY SNOW",
                       "HEAVY SNOW/WIND",
                       "HEAVY SNOW/BLIZZARD",
                       "HEAVY SNOW AND ICE STORM",
                       "SNOW/HEAVY SNOW",
                       "HEAVY SNOW/SQUALLS",
                       "SNOW SQUALL",
                       "HEAVY LAKE SNOW",
                       "HEAVY SNOW/FREEZING RAIN",
                       "HEAVY SNOW AND",
                       "HEAVY SNOW/ICE STORM",
                       "SNOWFALL RECORD",
                       "HEAVY SNOW-SQUALLS",
                       "HEAVY SNOW/HIGH",
                       "HEAVY SNOWPACK",
                       "HEAVY SNOW/HIGH WINDS/FREEZING",
                       "HIGH WIND/HEAVY SNOW")), 
   EVTYPE := "HEAVY SNOW"]

dt[which(EVTYPE %in% c("SMALL HAIL",
                       "NON SEVERE HAIL",
                       "LATE SEASON HAIL",
                       "ICE PELLETS",
                       "HAIL 1.75)",
                       "HAIL(0.75)",
                       "HAIL/WIND",
                       "HAIL 225",
                       "HAIL 0.75",
                       "HAIL 1.00",
                       "HAIL 0.88",
                       "DEEP HAIL",
                       "HAIL 75",
                       "HAIL 175",
                       "HAIL 100",
                       "HAIL 450",
                       "HAIL 275",
                       "HAIL 150",
                       "HAIL ALOFT",
                       "HAIL DAMAGE",
                       "HAIL STORM",
                       "HAILSTORM",
                       "HAILSTORMS",
                       "HAIL/WINDS",
                       "HAIL 1.75",
                       "HAIL 80",
                       "HAIL 125",
                       "HAIL 88",
                       "HAIL 088",
                       "HAIL 075",
                       "HAIL 200",
                       "HAIL 125",
                       "WIND/HAIL",
                       "HAIL/ICY ROADS",
                       "HAIL FLOODING")),
   EVTYPE := "HAIL"]


print(unique(dt[!(EVTYPE %in% permittedevents), EVTYPE]))

# dt[, BGN_DATE := as.Date(BGN_DATE,
#                          format = "%m/%d/%Y %H:%M:%S",
#                          tz = TIME_ZONE)]
# healthkpi <- dt[EVTYPE %in% permittedevents] %>%
#     filter(STATE %in% state.abb) %>%
#     select(BGN_DATE, STATE, EVTYPE, FATALITIES, INJURIES) %>%
#     group_by(year(BGN_DATE)) %>%
#     summarise(TOTAL_EVENTS = n())
# 
# healthkpi.bystateandevent <- dt[EVTYPE %in% permittedevents] %>%
#     filter(STATE %in% state.abb & 
#                year(BGN_DATE) > 1999) %>%
#     select(BGN_DATE, STATE, EVTYPE, FATALITIES, INJURIES) %>%
#     group_by(STATE, EVTYPE) %>%
#     summarise(FATALITY = mean(FATALITIES),
#               INJURY = mean(INJURIES))
# 
# healthkpi.byimpact <- melt(healthkpi.bystateandevent, 
#                            id = c("STATE", "EVTYPE"), 
#                            variable.name = "IMPACT",
#                            value.name = "MEAN")
# 
# healthkpi.byfatality <- healthkpi.byimpact[healthkpi.byimpact[IMPACT == "FATALITY", .I[which.max(MEAN)], by = STATE]$V1]
# 
# 
# state <- sapply(healthkpi.byfatality$STATE, function(x) tolower(
#     state.name[grep(x, state.abb)]))
# 
# states_map <- map_data("state")
# snames <- data.table(aggregate(cbind(long, lat) ~ region, 
#                                data = states_map, 
#                                FUN=function(x) mean(range(x))))
# 
# healthkpi.byinjury <- healthkpi.byimpact[healthkpi.byimpact[IMPACT == "INJURY", .I[which.max(MEAN)], by = STATE]$V1]
# 
# healthkpi.bymaximumimpact <- union(healthkpi.byfatality, 
#                                    healthkpi.byinjury)
# healthkpi.bymaximumimpact[, region := 
#                               tolower(state.name[match(STATE, state.abb)])] 
# 
# healthkpi.bymaximumimpact <- 
#     merge(healthkpi.bymaximumimpact, snames, by = "region")
# 
# 
# p <- ggplot(healthkpi.bymaximumimpact) +
#     geom_map(aes(fill = EVTYPE,
#                  map_id = region),
#              map = states_map) +
#     expand_limits(x = states_map$long, y = states_map$lat) +
#     geom_text(aes(long, lat, label = region), 
#               size = 2) +
#     geom_text_repel(aes(long, lat, label = EVTYPE), 
#                     size = 2) +
#     facet_wrap(~ IMPACT) +
#     labs(title = 
#         "Statewise Events with Highest Health Impact/Year (since 2000)",
#          x = "Longitude", 
#          y = "Latitude") +
#     theme(plot.title = element_text(size = rel(1.2), 
#                                     colour = "black",
#                                     face = "bold")) +
#     scale_fill_discrete(name = "Event")
# 
# print(p)
# 
# dt[toupper(PROPDMGEXP) == "K", PROPDMG := PROPDMG * 1000]
# dt[toupper(PROPDMGEXP) == "M", PROPDMG := PROPDMG * 1000000]
# dt[toupper(PROPDMGEXP) == "B", PROPDMG := PROPDMG * 1000000000]
# dt[toupper(CROPDMGEXP) == "K", CROPDMG := CROPDMG * 1000]
# dt[toupper(CROPDMGEXP) == "M", CROPDMG := CROPDMG * 1000000]
# dt[toupper(CROPDMGEXP) == "B", CROPDMG := CROPDMG * 1000000000]
# 
# dmgkpi <- dt[EVTYPE %in% permittedevents] %>%
#     filter(STATE %in% state.abb & 
#                toupper(PROPDMGEXP) %in% c("", "K", "M", "B") &
#                toupper(CROPDMGEXP) %in% c("", "K", "M", "B")
#     ) %>%
#     select(BGN_DATE, STATE, EVTYPE, PROPDMG, CROPDMG) %>%
#     group_by(year(BGN_DATE)) %>%
#     summarise(TOTAL_EVENTS = n())
# 
# 
# dmgkpi.bystateandevent <- dt[EVTYPE %in% permittedevents] %>% 
#     filter(STATE %in% state.abb & 
#                toupper(PROPDMGEXP) %in% c("", "K", "M", "B") &
#                toupper(CROPDMGEXP) %in% c("", "K", "M", "B") &
#                year(BGN_DATE) > 1999) %>%
#     select(BGN_DATE, STATE, EVTYPE, PROPDMG, CROPDMG) %>%
#     group_by(STATE, EVTYPE) %>%
#     summarise(PROPERTY = mean(PROPDMG),
#               CROP = mean(CROPDMG))
# 
# dmgkpi.byimpact <- melt(dmgkpi.bystateandevent, 
#                         id = c("STATE", "EVTYPE"), 
#                         variable.name = "IMPACT",
#                         value.name = "MEAN")
# 
# dmgkpi.bypropdmg <- dmgkpi.byimpact[dmgkpi.byimpact[IMPACT == "PROPERTY", 
#                                                     .I[which.max(MEAN)], 
#                                                     by = STATE]$V1]
# dmgkpi.bycropdmg <- dmgkpi.byimpact[dmgkpi.byimpact[IMPACT == "CROP", 
#                                                     .I[which.max(MEAN)], 
#                                                     by = STATE]$V1]
# 
# dmgkpi.bymaximumimpact <- union(dmgkpi.bypropdmg, dmgkpi.bycropdmg)
# dmgkpi.bymaximumimpact[, region := 
#                            tolower(state.name[match(STATE, state.abb)])] 
# 
# dmgkpi.bymaximumimpact <- 
#     merge(dmgkpi.bymaximumimpact, cnames, by = "region")
# 
# p1 <- ggplot(dmgkpi.bymaximumimpact) +
#     geom_map(aes(fill = EVTYPE,
#                  alpha = MEAN,
#                  colour = EVTYPE,
#                  map_id = region),
#              map = states_map) +
#     geom_text(aes(long, lat, label = region), 
#               size = 2) +
#     geom_text_repel(aes(long, lat, label = EVTYPE), 
#                     size = 2) +
#     expand_limits(x = states_map$long, y = states_map$lat) +
#     facet_wrap(~ IMPACT) +
#     labs(title = "Statewise Events with Highest Mean Economic Loss/Year (since 2000)",
#          x = "Longitude", 
#          y = "Latitude") +
#     theme(plot.title = element_text(size = rel(1.2), 
#                                     colour = "black",
#                                     face = "bold")) +
#     scale_fill_discrete(guide = FALSE)
# 
# print(p1)
# 
# kpi.byyear <- dt[EVTYPE %in% permittedevents] %>%
#     filter(STATE %in% state.abb &
#                toupper(PROPDMGEXP) %in% c("", "K", "M", "B") &
#                toupper(CROPDMGEXP) %in% c("", "K", "M", "B") &
#                year(BGN_DATE) > 1999) %>%
#     select(BGN_DATE, EVTYPE, FATALITIES, INJURIES, PROPDMG, CROPDMG) %>%
#     group_by(year(BGN_DATE), EVTYPE) %>%
#     summarise(FATALITY = sum(FATALITIES),
#               INJURY = sum(INJURIES),
#               CROP = sum(CROPDMG),
#               PROPERTY = sum(PROPDMG)) %>%
#     setnames(1, "YEAR") %>%
#     melt(id = c("YEAR", "EVTYPE"),
#          variable.name = "IMPACT",
#          value.name = "TOTAL")
# p2 <- ggplot(kpi.byyear[, .SD, 
#                         by = .(YEAR, IMPACT)], 
#              aes(x = EVTYPE, 
#                  y = TOTAL,
#                  fill = IMPACT)) +
#     geom_bar(stat = "summary", fun.y = mean) +
#     facet_grid(IMPACT ~ ., scale = "free")  +
#     theme(axis.text.x = element_text(angle = 90),
#           plot.title = element_text(size = rel(2))) +
#     scale_fill_discrete(guide = FALSE) +
#     labs(title = "Mean Impact/Year per Event (since 2000)",
#          x = "Weather Event", 
#          y = "People Impacted (Fatality/Injury)/Damage in $ (Crop/Property)")
#     
# print(p2)