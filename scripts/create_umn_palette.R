# Creating UMN colors
umn_palettes <- list(
  Primary = c("#7A0019", "#FFCC33"),
  Secondary_Tier1 = c("#16BBE6", "#E98524", "#BCD530", "#CCCCCC", "#E2D3A4", "#87D5EB", "#F6C17E", "#DDe681"),
  Secondary_Tier2 = c("#0B3D4C", "#CA7A29", "#726F2D", "#616365", "#91785B", "#809FA6", "#E5BA7F", "#B9B790", "#B7B7B7", "#D3BF96"),
  Secondary_Tier3 = c("#A3DCE8", "#F0CC65", "#DFE670", "#D5D6D2", "#E9DEBB", "#D0EDF5", "#F7E7B2", "#EEF2B8", "#EBEBEB", "#F0E9D1"))


barley_palette <- c("#074973", "#BE9519", "#8F610E", "#BEA672", "#724E2E")

fall <- c("#98D3F2", "#EFD635", "#D98034", "#61540C", "#DA3534")

neyhart_palettes <- list(
  umn1 = c(umn_palettes$Primary, umn_palettes$Secondary_Tier1),
  umn2 = c(umn_palettes$Primary, umn_palettes$Secondary_Tier2),
  umn3 = c(umn_palettes$Primary, umn_palettes$Secondary_Tier3),
  barley = barley_palette,
  fall = fall
)
