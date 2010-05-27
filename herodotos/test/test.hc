/*
	This is a comment.
	/* This is a comment too */
*/

/* Another one */

//prefix="tst-config"
prefix="."
patterns="./cocci"
projects="./projects"
results="./results"
website="./website"
findcmd="spatch %f -sp_file %p -dir %d 2> %b.log > %o"
flags="-timeout 60 -use_glimpse"
//cpucore = 2 // Automatically determined from /proc/cpuinfo

// cpucore should be something like
// grep processor /proc/cpuinfo | sort -ru | cut -d ":" -f2 | head -1
// +1

project Linux {

 dir = linux
 color = 1 0 0
 linestyle = solid
 marktype = circle
 versions = {
//  ("linux-2.6.12", 06/17/2005, 4155826)
  ("linux-2.6.13", 08/28/2005, 4289406)
  ("linux-2.6.14", 10/27/2005, 4399620)
  ("linux-2.6.15", 01/02/2006, 4481795)
  ("linux-2.6.16", 03/19/2006, 4601957)
  ("linux-2.6.17", 06/17/2006, 4669412)
  ("linux-2.6.18", 09/19/2006, 4750426)
  ("linux-2.6.19", 11/29/2006, 4890004)
  ("linux-2.6.20", 02/04/2007, 4972172)
  ("linux-2.6.21", 04/25/2007, 5060948)
  ("linux-2.6.22", 07/08/2007, 5215458)
  ("linux-2.6.23", 10/09/2007, 5272632)
  ("linux-2.6.24", 01/24/2008, 5455345)
  ("linux-2.6.25", 04/16/2008, 5679912)
  ("linux-2.6.26", 07/13/2008, 5780304)
  ("linux-2.6.27", 10/09/2008, 5903948)
  ("linux-2.6.28", 12/24/2008, 6219333)
//  ("linux-2.6.29", 03/23/2009, 6724330)
//  ("linux-2.6.30", 06/09/2009, 7083842)
//  ("linux-2.6.31", 09/09/2009, 7338314)
 }
}

project Wine {
 dir = wine
 color = 0 0 1
 linestyle = solid
 marktype = box
 versions = {
  ("wine-20050830", 08/30/2005, 1037069)
  ("wine-0.9"     , 10/25/2005, 1051743)
  ("wine-0.9.5"   , 01/04/2006, 1085684)
  ("wine-0.9.10"  , 03/15/2006, 1107063)
  ("wine-0.9.16"  , 06/21/2006, 1141730)
  ("wine-0.9.21"  , 09/13/2006, 1181745)
  ("wine-0.9.26"  , 11/24/2006, 1203677)
  ("wine-0.9.30"  , 01/25/2007, 1227183)
  ("wine-0.9.36"  , 04/27/2007, 1260912)
  ("wine-0.9.41"  , 07/13/2007, 1298833)
  ("wine-0.9.47"  , 10/12/2007, 1355986)
  ("wine-0.9.54"  , 01/25/2008, 1393639)
  ("wine-0.9.60"  , 04/18/2008, 1446788)
  ("wine-1.1.1"   , 07/11/2008, 1482518)
  ("wine-1.1.6"   , 10/10/2008, 1556194)
  ("wine-1.1.11"  , 12/20/2008, 1586995)
 }
}

project VLC {
 dir = vlc
 color = 0 1 0
 linestyle = solid
 marktype = diamond
 versions = {
  ("0.8.2" , 06/25/2005, 213186)
  ("0.8.4" , 11/26/2005, 240215)
  ("0.8.4b", 12/14/2005, 239598)
  ("0.8.5" , 05/14/2006, 256197)
  ("0.8.6" , 12/10/2006, 263927)
  ("0.8.6a", 03/11/2007, 263927)
  ("0.8.6b", 04/18/2007, 265074)
  ("0.8.6c", 06/16/2007, 265460)
  ("0.8.6d", 11/30/2007, 264908)
  ("0.8.6e", 02/27/2008, 265337)
  ("0.8.6f", 03/31/2008, 266198)
  ("0.8.6g", 05/17/2008, 266211)
  ("0.8.6h", 06/03/2008, 266666)
  ("0.8.6i", 07/08/2008, 266686)
  ("0.9.0" , 08/24/2008, 326858)
  ("0.9.4" , 10/06/2008, 327318)
  ("0.9.8a", 12/02/2008, 327494)
 }
}

project OpenSSL {
 dir = openssl
 color = 0 0 0
 linestyle = solid
 marktype = triangle
 flags="-macro_file tst-config/cocci/openssl.h"
 versions = {
  ("openssl-0.9.8a", 10/11/2005, 200187)
  ("openssl-0.9.8b", 05/04/2006, 200479)
  ("openssl-0.9.8c", 09/05/2006, 203553)
  ("openssl-0.9.8d", 09/28/2006, 203684)
  ("openssl-0.9.8e", 02/23/2007, 205870)
  ("openssl-0.9.8g", 10/19/2007, 208715)
  ("openssl-0.9.8h", 05/28/2008, 216565)
  ("openssl-0.9.8i", 09/15/2008, 219305)
  ("openssl-0.9.8j", 01/07/2009, 231578)
 }
}

pattern open {
 file = "open.cocci"
 color = 1 0 .5
 linestyle = solid
 marktype = circle
}

pattern kmalloc7 {
 file = "kmalloc7.cocci"
 color = 0 1 1
 marktype = diamond
 linestyle = solid
}

pattern malloc {
 file = "malloc.cocci"
 color = 0 1 1
 marktype = diamond
 linestyle = solid
}

pattern isnull {
 file = "isnull.cocci"
 color = 0 0 1
 marktype = box
 linestyle = solid
}

pattern null_ref {
 file = "null_ref.cocci"
 color = .5 .5 .5
 marktype = cross
 linestyle = solid
}

pattern notnull {
 file = "notnull.cocci"
 color = 0 .5 .5
 marktype = x
 linestyle = solid
}

pattern unused {
 file = "unused.cocci"
 color = .5 .5 1
 marktype = box
 linestyle = solid
}

pattern notand {
 file = "notand.cocci"
 flags = "-macro_file_builtins tst-config/cocci/notand.h"
 color = 0 1 .5
 marktype = triangle
 linestyle = solid
}

pattern find_unsigned {
 file = "find_unsigned.cocci"
 flags = "-all_includes"
 color = 0 1 0
 marktype = circle
 linestyle = solid
}

pattern badzero {
 file = "badzero.cocci"
}

graph gr/hist/vlc-null_ref.jgr {
 xaxis = version
 xlegend = "Versions"
 yaxis = occurrences
 ylegend = "Defects"
 sort = true
 info = false
 filename = true
 ratio = true

 curve project VLC pattern null_ref {
   notexistcolor = 0 0 0
   nooccurcolor = 1 1 1
   occurcolor = 0 1 1
//   occurcolor = 1 0 0
 }
}

graph gr/hist/vlc-null_ref-by-date.jgr {
 xaxis = date
 xlegend = ""
 yaxis = occurrences
 ylegend = "Defects"
 sort = true
 filename = false

 curve project VLC pattern null_ref {
   notexistcolor = 0 0 0
   nooccurcolor = 1 1 1
   occurcolor = 0 1 1
//   occurcolor = 1 0 0
 }
}

graph gr/evol/linux.jgr {
 xaxis = date
 xlegend = "Linux"
 yaxis = count
 ylegend = "Number of defects"
 legend = "defaults fontsize 8 left"
 project = Linux

 curve pattern open
 curve pattern kmalloc7 {
  legend = "kmalloc/malloc"
 }
 curve pattern isnull
 curve pattern null_ref
 curve pattern notnull
 curve pattern unused
 curve pattern notand
 curve pattern find_unsigned
}

graph gr/evol/linux-birth.jgr {
 xaxis = date
 xlegend = "Linux - Defects introduced"
 yaxis = birth
 ylegend = "Number of defects"
 legend = "off"
 project = Linux

 curve pattern open
 curve pattern kmalloc7
 curve pattern isnull
 curve pattern null_ref
 curve pattern notnull
 curve pattern unused
 curve pattern notand
 curve pattern find_unsigned
}

graph gr/evol/linux-death.jgr {
 xaxis = date
 xlegend = "Linux - Defects removed"
 yaxis = death
 ylegend = "Number of defects"
 legend = "off"
 project = Linux

 curve pattern open
 curve pattern kmalloc7
 curve pattern isnull
 curve pattern null_ref
 curve pattern notnull
 curve pattern unused
 curve pattern notand
 curve pattern find_unsigned
}

graph gr/evol/eldest.jgr {
 xaxis = date
 xlegend = "Linux"
 yaxis = eldest
 ylegend = "Age of eldest"
 legend = "off"
 project = Linux

 curve pattern open
 curve pattern kmalloc7
 curve pattern isnull
 curve pattern null_ref
 curve pattern notnull
 curve pattern unused
 curve pattern notand
 curve pattern find_unsigned
 curve pattern badzero
}

graph gr/evol/eldest-death.jgr {
 xaxis = date
 xlegend = "Linux"
 yaxis = eldestdeath
 ylegend = "Age of eldest death"
 legend = "off"
 project = Linux

 curve pattern open
 curve pattern kmalloc7
 curve pattern isnull
 curve pattern null_ref
 curve pattern notnull
 curve pattern unused
 curve pattern notand
 curve pattern find_unsigned
 curve pattern badzero
}

graph gr/evol/linux-lifeexpect.jgr {
 xaxis = date
 xlegend = "Linux"
 yaxis = lifeexpect
 ylegend = "Life expectancy"
 legend = "off"
 project = Linux

 curve pattern open
 curve pattern kmalloc7
 curve pattern isnull
 curve pattern null_ref
 curve pattern notnull
 curve pattern unused
 curve pattern notand
 curve pattern find_unsigned
 curve pattern badzero
}

graph gr/evol/wine-lifeexpect.jgr {
 xaxis = date
 xlegend = "Wine"
 yaxis = lifeexpect
 ylegend = "Life expectancy"
 legend = "off"
 project = Wine

 curve pattern open
 curve pattern malloc
 curve pattern isnull
 curve pattern null_ref
 curve pattern notnull
 curve pattern unused
 curve pattern notand
 curve pattern find_unsigned
 curve pattern badzero
}

graph gr/evol/vlc-lifeexpect.jgr {
 xaxis = date
 xlegend = "VLC"
 yaxis = lifeexpect
 ylegend = "Life expectancy"
 legend = "off"
 project = VLC

 curve pattern open
 curve pattern malloc
 curve pattern isnull
 curve pattern null_ref
 curve pattern notnull
 curve pattern unused
 curve pattern notand
 curve pattern find_unsigned
 curve pattern badzero
}

graph gr/evol/openssl-lifeexpect.jgr {
 xaxis = date
 xlegend = "OpenSSL"
 yaxis = lifeexpect
 ylegend = "Life expectancy"
 legend = "off"
 project = OpenSSL

 curve pattern open
 curve pattern malloc
 curve pattern isnull
 curve pattern null_ref
 curve pattern notnull
 curve pattern unused
 curve pattern notand
 curve pattern find_unsigned
 curve pattern badzero
}

graph gr/evol/avgage.jgr {
 xaxis = date
 xlegend = "Linux"
 yaxis = avgage
 ylegend = "Average age"
 legend = "off"
 project = Linux

 curve pattern open
 curve pattern kmalloc7
 curve pattern isnull
 curve pattern null_ref
 curve pattern notnull
 curve pattern unused
 curve pattern notand
 curve pattern find_unsigned
 curve pattern badzero
}

graph gr/evol/linux-netincrease.jgr {
 xaxis = date
 xlegend = "Linux - Defects"
 yaxis = netincrease
 ylegend = "Number of defects"
 legend = "off"
 project = Linux

// curve pattern open
 curve pattern kmalloc7
 curve pattern isnull
 curve pattern null_ref
 curve pattern notnull
 curve pattern unused
// curve pattern notand
 curve pattern find_unsigned
}

graph gr/evol/linux-netincrease-unsigned.jgr {
 xaxis = date
 xlegend = "Linux - Net increase of defects"
 yaxis = netincrease
 ylegend = "Number of defects"
 legend = "off"
 project = Linux

 curve pattern find_unsigned
}

graph gr/evol/linux-birthfile.jgr {
 xaxis = date
 xlegend = "Linux - Born with a file"
 yaxis = birthfile
 ylegend = "Number of defects"
 legend = "off"
 project = Linux

 curve pattern open
 curve pattern kmalloc7
 curve pattern isnull
 curve pattern null_ref
 curve pattern notnull
 curve pattern unused
 curve pattern notand
 curve pattern find_unsigned
}

graph gr/evol/linux-deathfile.jgr {
 xaxis = date
 xlegend = "Linux - Removed with a file"
 yaxis = deathfile
 ylegend = "Number of defects"
 legend = "off"
 project = Linux

 curve pattern open
 curve pattern kmalloc7
 curve pattern isnull
 curve pattern null_ref
 curve pattern notnull
 curve pattern unused
 curve pattern notand
 curve pattern find_unsigned
}

graph gr/evol/wine.jgr {
 xaxis = date
 xlegend = "Wine"
 yaxis = count
 ylegend = "Number of defects"
 legend = "off"
 project = Wine

 curve pattern malloc
 curve pattern isnull
 curve pattern null_ref
 curve pattern notnull
 curve pattern unused
 curve pattern notand
 curve pattern find_unsigned
}

graph gr/evol/openssl.jgr {
 xaxis = date
 xlegend = "OpenSSL"
 yaxis = count
 ylegend = "Number of defects"
 legend = "off"
 project = OpenSSL

// curve pattern malloc
 curve pattern isnull
 curve pattern null_ref
 curve pattern notnull
 curve pattern unused
// curve pattern notand
 curve pattern find_unsigned
}

graph gr/evol/vlc.jgr {
 xaxis = date
 xlegend = "VLC"
 yaxis = count
 ylegend = "Number of defects"
 legend = "off"
 project = VLC

 curve pattern malloc
 curve pattern isnull
 curve pattern null_ref
 curve pattern notnull
 curve pattern unused
 curve pattern notand
// curve pattern find_unsigned
}

graph gr/size/code-size.jgr {
 xaxis = date
 xlegend = ""
 yaxis = size
 ylegend = "Code size\nin MLOC"
// ylegendfactor = M
 factor= 1000000 // MLOC
// legend = "defaults fontsize 6 x 800 y 3400000"
 legend = "defaults fontsize 6 x 800 y 3.4"

 curve project Linux
 curve project Wine
 curve project VLC
 curve project OpenSSL
}

graph gr/size/code-size-percentage.jgr {
 xaxis = date
 xlegend = ""
 yaxis = sizepct
 ylegend = "Percentage\nincrease"
 legend = "defaults fontsize 6 x 180 y 45"

 curve project Linux
 curve project Wine
 curve project VLC
 curve project OpenSSL
}

graph gr/evol/isnull-density.jgr {
 xaxis = date
 yaxis = density
 ylegend = "Density of defects"
 legend = "defaults fontsize 8 x 180 y 0.010"
 pattern = isnull
 factor = 1000 // 1K LOC
 //factor = 1000000 // 1M LOC

 curve project Linux
 curve project Wine
 curve project VLC
 curve project OpenSSL
}

graph gr/evol/badzero.jgr {
 xaxis = date
 yaxis = count
 xlegend = "badzero"
 ylegend = "Number of defects"
// legend = "defaults fontsize 8 x 1000 y 350"
 legend = "defaults fontsize 8 x 1000 y 500"
 pattern = badzero

 curve project Linux
 curve project Wine
 curve project VLC
 curve project OpenSSL
}

graph gr/evol/badzero-density.jgr {
 xaxis = date
 yaxis = density
 xlegend = "badzero"
 ylegend = "Density of defects\n(# by KLOC)"
 legend = "off"
 pattern = badzero
 factor = 1000

 curve project Linux
 curve project Wine
 curve project VLC
 curve project OpenSSL
}

graph gr/total/sum.jgr {
 xaxis = groups
 xlegend = ""
 yaxis = cumulcount
 ylegend = "Number of defects"

 project = Linux
 project = Wine
 project = VLC
 project = OpenSSL

 group pattern open
 group "(k)malloc" {
   curve project Linux   pattern kmalloc7
   curve project Wine    pattern  malloc
   curve project VLC     pattern  malloc
   curve project OpenSSL pattern  malloc
 }
 group pattern isnull
 group pattern null_ref
 group pattern notnull
 group pattern unused
 group pattern badzero
 group pattern notand
 group pattern find_unsigned
}

graph gr/total/avg.jgr {
 xaxis = groups
 xlegend = ""
 yaxis = avglifespan
 ylegend = "Lifespan in years"

 project = Linux
 project = Wine
 project = VLC
 project = OpenSSL

 group pattern open
 group "(k)malloc" {
   curve project Linux   pattern kmalloc7
   curve project Wine    pattern  malloc
   curve project VLC     pattern  malloc
   curve project OpenSSL pattern  malloc
 }
 group pattern isnull
 group pattern null_ref
 group pattern notnull
 group pattern unused
 group pattern badzero
 group pattern notand
 group pattern find_unsigned
}
