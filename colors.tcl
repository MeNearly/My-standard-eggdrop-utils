variable color 

variable white "00"
variable black "01"
variable blue "02"
variable green "03"
variable red "04"
variable brown "05"
variable purple "06"
variable orange "07"
variable yellow "08"
variable lightgreen "09"
variable cyan "10"
variable lightcyan "11"
variable lightblue "12"
variable pink "13"
variable grey "14"
variable lightgrey "15"

variable colors {white black blue green red brown purple orange yellow lightgreen cyan lightcyan lightblue pink grey lightgrey}

foreach colorname $colors {
  variable ${colorname}_back ",[getvar $colorname]"
}
