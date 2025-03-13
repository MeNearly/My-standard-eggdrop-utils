  ###########################################
  ## My simple standard tcl Library        ##
  ## v1.3 © 015-202x MeNearly@gmail.com    ##
  ## except uninstall and antiflood        ##
  ## which are © MenzAgitat                ##
  ##                                       ##
  ## Some utils                            ##
  ## Support of formatted eggdrop commands ##
  ##                                       ##
  ## DEPRECATED since eggdrop1.10/tcl9 :   ##
  ##    Easy UTF8 correction...            ##
  ## extended HTML entities parsing        ##
  ###########################################
  # md5 pour l'antiflood
  package require md5

  variable DEBUGMODE false
  # DEPRECATED
  variable UTF8_support false

  if { "$::tcl_version" >= "9.0" } {
    set UTF8_support true
  }

  variable stdlibversion "v1.3 ©2015-202x"
  variable user_agent "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:103.0) Gecko/20100101 Firefox/103.0"
  variable useragent $user_agent
  # 
  variable CTCP "\x01"

  variable bell "\x07"
  # 
  variable under "\x1f"
  # 
  variable ital "\x1d"
  # 
  variable strike "\x1e"
  # 
  variable reverse "\x16"
  # 
  variable bold "\x02"
  # 
  variable norm "\x0f"

  variable allCodes "$bell$under$ital$strike$reverse$bold$norm"

  variable pubbindz [::tcl::dict::create]
  variable protectedpubbindz [::tcl::dict::create]
  variable protectedpubbindz2 [::tcl::dict::create]
  variable msgbindz [::tcl::dict::create]
  variable protectedmsgbindz [::tcl::dict::create]
  variable msglinkedbindz [::tcl::dict::create]
  variable protectedmsglinkedbindz [::tcl::dict::create]
  variable protectedmsglinkedbindz2 [::tcl::dict::create]

  # before rehash, remove all binds (must be in a namespace !)
  if {[namespace current] != "::"} {
    bind evnt -|- prerehash [namespace current]::uninstall
  }
  # if you need more specific things, you should do this :
  # unbind evnt -|- prerehash [namespace current]::uninstall
  # bind evnt -|- prerehash [namespace current]::my_specific_uninstall

  proc varexists name {
    return [info exists [namespace current]::$name]
  }

  proc getvar name {
    if {![varexists $name]} {
      return 0
    }
    namespace upvar [namespace current] $name tmp
    return $tmp
  }

  proc setvar {name value} {
    namespace upvar [namespace current] $name tmp
    set tmp $value
    return $tmp
  }

  proc unsetvar name {
    unset [namespace current]::$name
  }

  proc incrvar {name {i 1}} {
    incr [namespace current]::$name $i
  }

  # mIRC colors
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

  # do it here, because it needs the getvar proc
  foreach colorname $colors {
    variable ${colorname}_back ",[getvar $colorname]"
  }


  # Below are procs to deal with commands containing some control codes (color, bold, etc)
  # set one of pubbinds, protectedpubbindz, etc. as ::tcl::dict { "command_name" "proc_name" ...}
  # Note that proc_name does not need [namespace current]:: !!
  # and then bind them with e.g.
  #   bind pubm - * [namespace current]::filterPubBindz

  proc filterPubBindz {nick user handle chan args} {
    return [core_filterPubBindz "pubbindz" $nick $user $handle $chan $args]
  }

  proc filterProtectedPubBindz {nick user handle chan args} {
    return [core_filterPubBindz "protectedpubbindz" $nick $user $handle $chan $args]
  }

  proc filterProtectedPubBindz2 {nick user handle chan args} {
    return [core_filterPubBindz "protectedpubbindz2" $nick $user $handle $chan $args]
  }

  proc filterMsgBindz {nick user handle args} {
    return [core_filterMsgBindz "msgbindz" $nick $user $handle $args]
  }

  proc filterProtectedMsgBindz {nick user handle args} {
    return [core_filterMsgBindz "protectedmsgbinz" $nick $user $handle $args]
  }

  proc filterMsgLinkedBindz {nick user handle args} {
    return [core_filterPubBindz "msglinkedbindz" $nick $user $handle $nick $args]
  }

  proc filterProtectedMsgLinkedBindz {nick user handle args} {
    return [core_filterPubBindz "protectedmsglinkedbindz" $nick $user $handle $nick $args]
  }

  proc filterProtectedMsgLinkedBindz2 {nick user handle args} {
    return [core_filterPubBindz "protectedmsglinkedbindz2" $nick $user $handle $nick $args]
  }

  proc argsList text {
    if {[string is list $text]} {
      set argslist [lindex $text 0]
    } else {
      set argslist [split $text]
    }
    return $argslist
  }

  proc core_filterPubBindz {bindzNames nick uhost handle chan txt} {
    set orig [argsList $txt]
    set args [argsList [stripcodes * $txt]]
    if {[catch {set cmd [lindex $args 0]}]} {
      set args [split $args]
      set orig [split $orig]
      set cmd [lindex $args 0]
    }
    set bindz [getvar $bindzNames]
    if {! [::tcl::dict::exists $bindz $cmd]} {
      return 1
    }
    set i [lsearch $orig $cmd]
    if {$i==-1} {
      set i 0
    }
    set orig [lreplace $orig 0 $i]
    set procz [::tcl::dict::get $bindz $cmd]
    if {$procz != ""} {
      if {[catch {set rez [$procz $nick $uhost $handle $chan $orig]} rc infos]} {
        notice_quick $nick [utf8 "[getvar color][getvar red][getvar bold]ERROR : [getvar bold][getvar ital][::tcl::dict::get $infos -errorinfo][getvar norm]"]
        if [getvar DEBUGMODE] {
          foreach line [split [::tcl::dict::get $infos -errorstack] "\n"] {
            notice_quick $nick [utf8 "[getvar color][getvar red][getvar bold]$line[getvar norm]"]
          }
        }
        return 1
      } else {
        return $rez
      }
    }
    return 1
  }

  proc core_filterMsgBindz {bindzNames nick user handle args} {
    set orig [argsList $args]
    set args [argsList [stripcodes * $args]]
    set cmd [lindex $args 0]
    set bindz [getvar $bindzNames]
    if {! [::tcl::dict::exists $bindz $cmd]} {
      return 1
    }
    set i [lsearch $orig $cmd]
    if {$i==-1} {
      set i 0
    }
    set orig [lreplace $orig 0 $i]

    set procz [::tcl::dict::get $bindz $cmd]
    if {$procz != ""} {
      if {[catch {set rez [$procz $nick $uhost $handle $chan $orig]} rc infos]} {
        notice_quick $nick [utf8 "[getvar color][getvar red][getvar bold]ERROR : [getvar bold][getvar ital][::tcl::dict::get $infos -errorinfo][getvar norm]"]
        if [getvar DEBUGMODE] {
          foreach line [split [::tcl::dict::get $infos -errorstack] "\n"] {
            notice_quick $nick [utf8 "[getvar color][getvar red][getvar bold]$line[getvar norm]"]
          }
        }
        return 1
      } else {
        return $rez
      }
    }
    return 1
  }
  ##############
  ## isChannel
  ##############

  proc isChannel str {
    set first [string range $str 0 0]
    if {$first == "#" || $first == "&"} {
      return 1
    }
    return 0
  }

  # DEPRECATED
  proc utf8 {str} {
    if {[getvar UTF8_support]} {
      return $str
    } else {
      return [encoding convertfrom utf-8 $str]
    }
  }

  # DEPRECATED
  proc utf8From {str} {
    if {[getvar UTF8_support]} {
      return $str
    } else {
      return [encoding convertto utf-8 $str]
    }
  }

  proc utf8_f {str} {
    return [encoding convertfrom utf-8 $str]
  }

  proc utf8From_f {str} {
    return [encoding convertto utf-8 $str]
  }

  proc unicode {str} {
    return [encoding convertfrom unicode $str]
  }

  proc unicodeFrom {str} {
    return [encoding convertto unicode $str]
  }

  proc notice {nick line} {
    gen_notice putserv "-normal" $nick $line
  }
  proc notice_help {nick line} {
    gen_notice puthelp "-normal" $nick $line
  }
  proc notice_quick {nick line} {
    gen_notice putquick "-normal" $nick $line
  }
  proc notice_now {nick line} {
    gen_notice putnow "" $nick $line
  }

  proc gen_notice {put_type arg nick line} {
    set i 0
    if { $line != "" } {
      if {[string length $line] > 450} {
        set text "//"
        while {$text != "" } {
          set splitted [split_line $line $i]
          set text [lindex $splitted 1]
          if {$text != ""} {
            if { $arg == "" } {
              $put_type "notice $nick :$text"
            } else {
              $put_type "notice $nick :$text" $arg
            }
          }
          set i [lindex $splitted 0]
        }
      } else {
        if { $arg == "" } {
          $put_type "notice $nick :$line"
        } else {
          $put_type "notice $nick :$line" $arg
        }
      }
    }
  }

  proc notice_to {nick oargs line} {
    set oargs [argsList $oargs]
    set to [lindex $oargs 0]
    if {$to == ""} {
      set to $nick
    }
    set i 0
    if { $line != "" } {
      if {[string length $line] > 400} {
        set text "//"
        while {$text != "" } {
          set splitted [split_line $line $i]
          set text [lindex $splitted 1]
          if {$text != ""} {
            putquick "notice $to :$text"
            if {[strlwr "$to"] ne [strlwr "$nick"]} {
              putquick "notice $nick :$text"
            }
          }
          set i [lindex $splitted 0]
        }
      } else {
        putquick "notice $to :$line"
        if {[strlwr "$to"] ne [strlwr "$nick"]} {
          putquick "notice $nick :$line"
        }
      }
    }
  }

  proc action {chan line} {
    gen_broadcast "putquick" $chan [utf8 "[getvar CTCP]ACTION $line[getvar CTCP]"]
  }

  proc broadcast {chan line} {
    gen_broadcast "putserv" $chan $line
  }

  proc broadcast_help {chan line} {
    gen_broadcast "puthelp" $chan $line
  }

  proc broadcast_quick {chan line} {
    gen_broadcast "putquick" $chan $line
  }

  proc broadcast_now {chan line} {
    gen_broadcast "putnow" $chan $line
  }

  proc message {nick line} {
    gen_broadcast "putserv" $nick $line
  }

  proc isCode char {
    return [expr [string first $char [getvar allCodes]] > -1];
  }

  proc stripAllCodes arg {
    set arg [string map { "\xa0" " "} $arg]
    return [stripcodes * $arg]
  }

  proc split_line {line start} {
    set end [expr $start + 399]
    if {$end > [string length $line]} {
      set tmp [string range $line $start end]
      if {[string trim $tmp]==""} {
        set tmp ""
      }
      return [list [string length $line] $tmp]
    }
    set tmp [string range $line $start $end]
    while {$tmp!="" && [string range $tmp end end] != " "} {
      incr end -1
      set tmp [string range $line $start $end]
    }
    return [list $end [string trim $tmp]]
  }

  proc gen_broadcast {put_type chan line} {
    set i 0
    set next false
    if { $line != "" } {
      if {[string length $line] > 400} {
        set text "//"
        while {$text != "" } {
          set splitted [string trim [split_line $line $i]]
          set i [lindex $splitted 0]
          set text [lindex $splitted 1]
          if {$text != ""} {
            if {[expr [string length $splitted] + [string length [string range $line $i end]]] < 400} {
              set text "$text[string range $line $i end]"
            }
            if {$next} {
              set text [strtrim $text]
            }
            $put_type "privmsg $chan :$text"
            set next true
          }
        }
      } else {
        $put_type "privmsg $chan :$line"
      }
    }
  }

  proc noAccent {str} {
    set str $str
    set str [string map [utf8 [list À A Â A Ä A Ã A Ç C É E È E Ê E Ë E Ï I Î I Ñ N Ö O Ô O Õ O Ù U Û U Ü U]] $str]
    set str [string map [utf8 [list à a â a ä a ã a ç c é e è e ê e ë e ï i î i ñ n ö o ô o õ o ù u û u ü u]] $str]
    return $str
  }

  proc littleCaps {str} {
    set rez [string map {a ᴀ b ʙ c ᴄ d ᴅ e ᴇ f ғ g ɢ h ʜ i ɪ j ᴊ k ᴋ l ʟ m ᴍ n ɴ o ᴏ p ᴘ q ǫ r ʀ s s t ᴛ u ᴜ v ᴠ w ᴡ x x y ʏ z ᴢ } [noAccent $str]]
    return $rez
  }

  proc ue_init {} {
     lappend d + { }
     for {set i 0} {$i < 256} {incr i} {
        set c [format %c $i]
        set x %[format %02x $i]
        if {![string match {[a-zA-Z0-9]} $c]} {
           lappend e $c $x
           lappend d $x $c
        }
     }
     set ::ue_map $e
     set ::ud_map $d
  }

  proc uencode {s} { string map $::ue_map $s }
  proc udecode {s} { string map $::ud_map $s }

  # this is quite complete
  proc htmlEntities str {
  # html 'normal' entities
    set tmp [::tcl::string::map {
      "&agrave;"    "à"    "&aacute;"    "á"    "&acirc;"      "â"
      "&atilde;"    "ã"    "&auml;"      "ä"    "&aring;"      "å"    "&aelig;"      "æ"
      "&ccedil;"    "ç"    "&egrave;"    "è"    "&eacute;"    "é"    "&ecirc;"      "ê"
      "&euml;"      "ë"    "&igrave;"    "ì"    "&iacute;"    "í"    "&icirc;"      "î"
      "&iuml;"      "ï"    "&eth;"        "ð"    "&ntilde;"    "ñ"    "&ograve;"    "ò"
      "&oacute;"    "ó"    "&ocirc;"      "ô"    "&otilde;"    "õ"    "&ouml;"      "ö"
      "&divide;"    "÷"    "&oslash;"    "ø"    "&ugrave;"    "ù"    "&uacute;"    "ú"
      "&ucirc;"      "û"    "&uuml;"      "ü"    "&yacute;"    "ý"    "&thorn;"      "þ"
      "&yuml;"      "ÿ"    "&quot;"      "\""  "&amp;"        "&"    "&euro;"      "€"
      "&oelig;"      "œ"    "&Yuml;"      "Ÿ"    "&iexcl;"      "¡"    "&lsquo;"      "'"
      "&cent;"      "¢"    "&pound;"      "£"    "&curren;"    "¤"    "&yen;"        "¥"
      "&brvbar;"    "¦"    "&brkbar;"    "¦"    "&sect;"      "§"    "&uml;"        "¨"
      "&die;"        "¨"    "&copy;"      "©"    "&ordf;"      "ª"    "&laquo;"      "«"
      "&not;"        "¬"    "&shy;"        "­-"  "&reg;"        "®"    "&macr;"      "¯"
      "&hibar;"      "¯"    "&deg;"        "°"    "&plusmn;"    "±"    "&sup2;"      "²"
      "&sup3;"      "³"    "&acute;"      "´"    "&micro;"      "µ"    "&para;"      "¶"
      "&middot;"    "·"    "&cedil;"      "¸"    "&sup1;"      "¹"    "&ordm;"      "º"
      "&raquo;"      "»"    "&frac14;"    "¼"    "&frac12;"    "½"    "&frac34;"    "¾"
      "&iquest;"    "¿"    "&Agrave;"    "À"    "&Aacute;"    "Á"    "&Acirc;"      "Â"
      "&Atilde;"    "Ã"    "&Auml;"      "Ä"    "&Aring;"      "Å"    "&AElig;"      "Æ"
      "&Ccedil;"    "Ç"    "&Egrave;"    "È"    "&Eacute;"    "É"    "&Ecirc;"      "Ê"
      "&Euml;"      "Ë"    "&Igrave;"    "Ì"    "&Iacute;"    "Í"    "&Icirc;"      "Î"
      "&Iuml;"      "Ï"    "&ETH;"        "Ð"    "&Dstrok;"    "Ð"    "&Ntilde;"    "Ñ"
      "&Ograve;"    "Ò"    "&Oacute;"    "Ó"    "&Ocirc;"      "Ô"    "&Otilde;"    "Õ"
      "&Ouml;"      "Ö"    "&times;"      "×"    "&Oslash;"    "Ø"    "&Ugrave;"    "Ù"
      "&Uacute;"    "Ú"    "&Ucirc;"      "Û"    "&Uuml;"      "Ü"    "&Yacute;"    "Ý"
      "&THORN;"      "Þ"    "&szlig;"      "ß"    "\r"          "\n"  "\t"          ""
      "&nbsp;"      " " "&apos;" "'" "&gt;" ">" "&lt;" "<"} $str]
# html decimal codes
    set re {&#([0-9]{2,3})}
    set decChars [regexp -all -inline -nocase $re $tmp]
    set mapL {}
    foreach {code value} $decChars {
      set c [format %c [scan $value %d]]
      lappend mapL "&#$value;"
      lappend mapL $c
    }
    set tmp [string map -nocase $mapL $tmp]

# html hexadecimal codes
    set re {&#x([a-f0-9]{2})}
    set hexChars [regexp -all -inline -nocase $re $tmp]
    set mapL {}
    foreach {code value} $hexChars {
      scan $value %x val
      set c [format %c $val]
      lappend mapL "&#x$value;"
      lappend mapL $c
    }
    set tmp [string map -nocase $mapL $tmp]

    return $tmp
  }

  proc urldecode str {
    # rewrite "+" back to space
    # protect \ from quoting another '\'
    set str [string map [list + { } "\\" "\\\\"] $str]

    # prepare to process all %-escapes
    regsub -all -- {%([A-Fa-f0-9][A-Fa-f0-9])} $str {\\u00\1} str

    # process \u unicode mapped chars
    return [subst -novar -nocommand $str]
  }

  proc html_entities_encode {data} {
    return [::tcl::string::map [lreverse [getvar entities]] $data]
  }

  proc html_hexdec_decode {data} {
    regsub -all {&#([[:digit:]]{1,5});} $data {[format %c [string trimleft "\1" "0"]]} data
    regsub -all {&#x([[:xdigit:]]{1,4});} $data {[format %c [scan "\1" %x]]} data
    return [subst $data]
  }

  proc duration int_time {
    set timeList [list]
    foreach div {86400 3600 60 1} mod {0 24 60 60} name {jour heure minute seconde} {
      set n [expr {$int_time / $div}]
      if {$mod > 0} {set n [expr {$n % $mod}]}
      if {$n > 0} {
        lappend timeList "$n ${name}[expr $n>1?{s}:{}]"
      }
    }
    return [join $timeList]
  }

  # usefull for some letter-games and some keyboards
  proc splitLinkedLetters {str} {
    set str [string map { æ ae Æ AE œ oe Œ OE} $str]
    return $str
  }

  proc linkLetters {str} {
    set str [string map { ae æ AE Æ oe œ OE Œ } $str]
    return $str
  }

  # a simplistic hardcoded permission mechanism
  proc protectedVerify {name nick uhost} {
#    putlog "Verifying for $name and $uhost/$nick"
    if {![info exists [namespace current]::owner] || ![info exists [namespace current]::allowed] || ![info exists [namespace current]::protected]} {
      return 1
    }
    if {[string match -nocase [getvar owner] $uhost]} { return 1}

    set is_protected [lsearch -nocase [getvar protected] $name]
    if {$is_protected == -1} {
      return 1
    } else {
      foreach mask [getvar allowed] {
        if {[string match -nocase $mask $uhost]} {
          return 1
        }
      }
    }
    notice $nick "[getvar ital][getvar color][getvar orange]Vous n'avez pas le droit d'utiliser cette commande"
    return 0
  }

  ## Own matchattr to avoid - flags 
  proc matchattr_ {hand flags chan} {
    if {[string first - $flags] != -1} {
      return 1
    }
    return [::matchattr $hand $flags $chan]
  }

  proc matchattr {hand flags chan} {
    return [matchattr_ $hand $flags $chan]
  }

  ## French ordnumber
  # Please note that the correct way to write e.g. the 3th in french is '3e', not '3ème'
  # but it is more convenient with screen reading to use the latter
  proc ordnumber num {
    switch $num {
      1 {
        set on "${num}er(e)"
      }
      2 {
        set on "${num}nd(e)"
      }
      default {
        set on "${num}ème"
      }
    }
    return $on
  }

  ##########################
  ## Inversion dict
  ##########################

  proc inverse_dict dico {
    ## PERMET D'INVERSER L'ASSOCIATION dans un dictionnaire
    ## n'est pas stocké, mais simplifie la visualisation ou recherche
    set tmp_dict [::tcl::dict::create]
    ::tcl::dict::for {key list1} $dico {
      foreach value $list1 {
        ::tcl::dict::lappend tmp_dict $value $key
      }
    }
    return $tmp_dict
  }

  ###########################################
  # Désinstallation/suppression du namespace
  # et suppression des [u]timers
  # (c) MenzAgitat amélioré ian (c) 2020
  ###########################################

  proc uninstall {args} {
    putlog "Désallocation des ressources de [getvar scriptFile]..."
    foreach binding [lsearch -inline -all -regexp [binds *[set tmpb [::tcl::string::range [namespace current] 2 end]]*] " \{?(::)?$tmpb"] {
      unbind [lindex $binding 0] [lindex $binding 1] [lindex $binding 2] [lindex $binding 4]
    }
    foreach running_utimer [utimers] {
      if { [::tcl::string::match "*[namespace current]::*" [lindex $running_utimer 1]] } { killutimer [lindex $running_utimer 2] }
    }
    foreach running_timer [timers] {
      if { [::tcl::string::match "*[namespace current]::*" [lindex $running_timer 1]] } { killtimer [lindex $running_timer 2] }
    }
    namespace delete [namespace current]
  }

  ###############################################################################
  ### Contrôle du flood 
  ### (c) MenzAgitat amélioré ian (c) 2020
  ### Note :
  ###   antiflood_msg = 0 : l'antiflood n'a pas encore été déclenché
  ###   antiflood_msg = 1 : message "antiflood toujours actif" déjà affiché
  ###   antiflood_msg = 2 : message "antiflood déclenché" déjà affiché
  ###############################################################################
# Activer le contrôle de flood ? (0 = non / 1 = oui)
  variable antiflood true

# Seuil de déclenchement de l'antiflood.
# Exemple : "4:180" = 4 commandes maximum en 180 secondes; les suivantes
# seront ignorées.
  variable flood_threshold "5:180"

# Intervalle de temps minimum entre l'affichage de 2 messages avertissant que
# l'antiflood a été déclenché (ne réglez pas cette valeur trop bas afin de ne
# pas être floodé par les messages avertissant de l'antiflood...)
  variable antiflood_msg_interval 20

  proc antiflood {cmd chan} {
    if {![varexists max_instances]} {
      set l_thres [split [getvar flood_threshold] ":"]
      setvar max_instances [lindex $l_thres 0]
      setvar instance_length [lindex $l_thres 1]
    }
    set cmd_hash [::md5::md5 $cmd]
    # l'antiflood est dans un statut neutre, on l'initialise
    if { ![::tcl::info::exists [namespace current]::instance($cmd_hash)] } {
      setvar instance($cmd_hash) 0
      setvar antiflood_msg($cmd_hash) 0
    }
    if { [getvar instance($cmd_hash)] >= [getvar max_instances] } {
      if { ![getvar antiflood_msg($cmd_hash)] } {
        setvar antiflood_msg($cmd_hash) 2
        broadcast $chan [utf8 "\x0304:::\x03 \x0314Contrôle de flood activé pour \x02$cmd\x02 : pas plus de [getvar max_instances] requête(s) toutes les [getvar instance_length] secondes.\x03"]
        if { [set msgresettimer [utimerexists "antiflood_msg_reset $cmd_hash"]] ne ""} {
          killutimer $msgresettimer
        }
        utimer [getvar antiflood_msg_interval] [list [namespace current]::antiflood_msg_reset $cmd_hash]
      } elseif { [getvar antiflood_msg($cmd_hash)] == 1 } {
        setvar antiflood_msg($cmd_hash) 2
        broadcast $chan [utf8 "\x0304:::\x03 \x0314Le contrôle de flood est toujours actif, merci de patienter.\x03"]
        if { [set msgresettimer [utimerexists "antiflood_msg_reset $cmd_hash"]] ne ""} {
          killutimer $msgresettimer
        }
        utimer [getvar antiflood_msg_interval] [list [namespace current]::antiflood_msg_reset $cmd_hash]
      }
      return "1"
    } else {
      setvar instance($cmd_hash) [expr [getvar instance($cmd_hash)] + 1]
      utimer [getvar instance_length] [list [namespace current]::antiflood_close_instance $cmd_hash]
      return "0"
    }
  }

  proc antiflood_close_instance {cmd_hash} {
    setvar instance($cmd_hash) [expr [getvar instance($cmd_hash)] - 1]
    # si le nombre d'instances retombe à 0, on efface les variables instance et
    # antiflood_msg afin de ne pas encombrer la mémoire inutilement
    if { [getvar instance($cmd_hash)] == 0 } {
      unsetvar instance($cmd_hash)
      unsetvar antiflood_msg($cmd_hash)
      # le nombre d'instances est retombé en dessous du seuil critique, on
      # réinitialise antiflood_msg
    } else {
      setvar antiflood_msg($cmd_hash) 0
      if { [set msgresettimer [utimerexists "[namespace current]::antiflood_msg_reset [set cmd_hash]"]] ne ""} {
        killutimer $msgresettimer
      }
    }
    return
  }

  proc antiflood_msg_reset {cmd_hash} {
    setvar antiflood_msg($cmd_hash) 1
    return
  }
#END ANTIFLOOD
  ue_init
