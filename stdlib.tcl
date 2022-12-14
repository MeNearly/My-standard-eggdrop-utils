  ##########################################
  ## My simple standard tcl Librairy      ##
  ## (c) 2015-2022 MeNearly@gmail.com     ##
  ## except uninstall and antiflood       ##
  ## which are (c) MenzAgitat             ##
  ##                                      ##
  ## Some utils                           ##
  ## permits support of formatted eggdrop ##
  ##   commands                           ##
  ## easy workaround use of utf8 for old  ##
  ##    versions of eggdrop ( < 1.8.x )   ##
  ##                                      ##
  ##########################################
  # md5 pour l'antiflood
  package require md5

  variable DEBUGMODE 0
  variable UTF8_support 1


  variable user_agent "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:103.0) Gecko/20100101 Firefox/103.0"

  # 
  variable CTCP "\u01"

  variable bell "\007"
  # 
  variable under "\u1f"
  # 
  variable ital "\u1d"
  # 
  variable strike "\u1e"
  # 
  variable reverse "\u16"
  # 
  variable bold "\u02"
  # 
  variable norm "\u0f"
  

  variable allCodes "$bell$under$ital$strike$reverse$bold$norm"

  variable pubbindz [::tcl::dict::create]
  variable protectedpubbindz [::tcl::dict::create]
  variable msgbindz [::tcl::dict::create]
  variable protectedmsgbindz [::tcl::dict::create]
  variable msglinkedbindz [::tcl::dict::create]
  variable protectedmsglinkedbindz [::tcl::dict::create]

  # before rehash, remove all binds
  bind evnt -|- prerehash [namespace current]::uninstall
  # if you need more specific things, you should do this :
  # unbind evnt -|- prerehash [namespace current]::uninstall
  # bind evnt -|- prerehash [namespace current]::my_specific_uninstall

  proc getvar {name} {
    if {![varexists $name]} {
      #putlog "Trying to get variable `$name`but it does not exist"
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

  proc varexists name {
    return [info exists [namespace current]::$name]
  }

  proc unsetvar name {
    unset [namespace current]::$name
  }

  # it needs var procs ...
  source "[file dirname [info script]]/colors.tcl"

  # Above are proc to deal with commands containing some control codes (color, bold, etc)
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

  proc stripAllCodes str {
    return [stripcodes abcgrui $str]
  }

  proc core_filterPubBindz {bindzNames nick uhost handle chan args} {
    set orig [argsList $args]
    set args [argsList [stripAllCodes $args]]
    set cmd [lindex $args 0]
    set bindz [getvar $bindzNames]

    if {! [::tcl::dict::exists $bindz $cmd]} {
      return 1
    }
    set orig [lreplace $orig 0 [lsearch $orig $cmd]]
    set procz [::tcl::dict::get $bindz $cmd]
    if {$procz != ""} {
      return [$procz $nick $uhost $handle $chan $orig]
    }
    return 1
  }

  proc core_filterMsgBindz {bindzNames nick user handle args} {
    set orig [argsList $args]
    set args [argsList [stripAllCodes $args]]
    set cmd [lindex $args 0]
    set bindz [getvar $bindzNames]
    if {! [::tcl::dict::exists $bindz $cmd]} {
      return 1
    }
    set orig [lreplace $orig 0 [lsearch $orig $cmd]]

    set procz [::tcl::dict::get $bindz $cmd]
    if {$procz != ""} {
      return [$procz $nick $user $handle $orig]
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

  proc utf8 {str} {
    if {[getvar UTF8_support]} {
      return $str
    } else {
      return [encoding convertfrom utf-8 $str]
    }
  }

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

  proc argsList argument {
    set argument [string map {\{ "" \} "" \" \\\"} $argument]
    if {![string is list $argument]} {
      set argument [split [lindex $argument 0]]
    }
    return $argument
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

  proc split_line {line start} {
    set end [expr $start + 399]
    set tmp [string range $line $start $end]
    while {$tmp!="" && [string range $tmp end end] != " "} {
      incr end -1
      set tmp [string range $line $start $end]
    }
    return [list $end [string trim $tmp]]
  }

  proc gen_broadcast {put_type chan line} {
    set i 0
    if { $line != "" } {
      if {[string length $line] > 400} {
          set text "//"
          while {$text != "" } {
            set splitted [split_line $line $i]
            set text [lindex $splitted 1]
            if {$text != ""} {
              $put_type "privmsg $chan :$text"
            }
            set i [lindex $splitted 0]
          }
      } else {
        $put_type "privmsg $chan :$line"
      }
    }
  }

  proc noAccent {str} {
    set str [utf8From $str]
    set str [string map { ?? A ?? A ?? A ?? A ?? C ?? E ?? E ?? E ?? E ?? I ?? I ?? N ?? O ?? O ?? O ?? U ?? U ?? U } $str]
    set str [string map { ?? a ?? a ?? a ?? a ?? c ?? e ?? e ?? e ?? e ?? i ?? i ?? n ?? o ?? o ?? o ?? u ?? u ?? u } $str]
    return $str
  }

  # this is obvioulsy incomplete !!
  proc htmlEntities str {
    return [::tcl::string::map {
      "&agrave;"    "??"    "&agrave;"    "??"    "&aacute;"    "??"    "&acirc;"      "??"
      "&atilde;"    "??"    "&auml;"      "??"    "&aring;"      "??"    "&aelig;"      "??"
      "&ccedil;"    "??"    "&egrave;"    "??"    "&eacute;"    "??"    "&ecirc;"      "??"
      "&euml;"      "??"    "&igrave;"    "??"    "&iacute;"    "??"    "&icirc;"      "??"
      "&iuml;"      "??"    "&eth;"        "??"    "&ntilde;"    "??"    "&ograve;"    "??"
      "&oacute;"    "??"    "&ocirc;"      "??"    "&otilde;"    "??"    "&ouml;"      "??"
      "&divide;"    "??"    "&oslash;"    "??"    "&ugrave;"    "??"    "&uacute;"    "??"
      "&ucirc;"      "??"    "&uuml;"      "??"    "&yacute;"    "??"    "&thorn;"      "??"
      "&yuml;"      "??"    "&quot;"      "\""  "&amp;"        "&"    "&euro;"      "???"
      "&oelig;"      "??"    "&Yuml;"      "??"    "&iexcl;"      "??"    "&lsquo;"      "'"
      "&cent;"      "??"    "&pound;"      "??"    "&curren;"    "??"    "&yen;"        "??"
      "&brvbar;"    "??"    "&brkbar;"    "??"    "&sect;"      "??"    "&uml;"        "??"
      "&die;"        "??"    "&copy;"      "??"    "&ordf;"      "??"    "&laquo;"      "??"
      "&not;"        "??"    "&shy;"        "??-"  "&reg;"        "??"    "&macr;"      "??"
      "&hibar;"      "??"    "&deg;"        "??"    "&plusmn;"    "??"    "&sup2;"      "??"
      "&sup3;"      "??"    "&acute;"      "??"    "&micro;"      "??"    "&para;"      "??"
      "&middot;"    "??"    "&cedil;"      "??"    "&sup1;"      "??"    "&ordm;"      "??"
      "&raquo;"      "??"    "&frac14;"    "??"    "&frac12;"    "??"    "&frac34;"    "??"
      "&iquest;"    "??"    "&Agrave;"    "??"    "&Aacute;"    "??"    "&Acirc;"      "??"
      "&Atilde;"    "??"    "&Auml;"      "??"    "&Aring;"      "??"    "&AElig;"      "??"
      "&Ccedil;"    "??"    "&Egrave;"    "??"    "&Eacute;"    "??"    "&Ecirc;"      "??"
      "&Euml;"      "??"    "&Igrave;"    "??"    "&Iacute;"    "??"    "&Icirc;"      "??"
      "&Iuml;"      "??"    "&ETH;"        "??"    "&Dstrok;"    "??"    "&Ntilde;"    "??"
      "&Ograve;"    "??"    "&Oacute;"    "??"    "&Ocirc;"      "??"    "&Otilde;"    "??"
      "&Ouml;"      "??"    "&times;"      "??"    "&Oslash;"    "??"    "&Ugrave;"    "??"
      "&Uacute;"    "??"    "&Ucirc;"      "??"    "&Uuml;"      "??"    "&Yacute;"    "??"
      "&THORN;"      "??"    "&szlig;"      "??"    "\r"          "\n"  "\t"          ""
      "&#039;"      "\'"  "&#39;"        "\'"  "&nbsp;"      " "    "&nbsp"        " "
      "&#34;"        "\'"  "&#38;"        "&"    "#91;"        "\("  "&#92;"        "/"
      "&#93;"        ")"    "&#123;"      "("    "&#125;"      ")"    "&#163;"      "??"
      "&#168;"      "??"    "&#169;"      "??"    "&#171;"      "??"    "&#173;"      "??"
      "&#174;"      "??"    "&#180;"      "??"    "&#183;"      "??"    "&#185;"      "??"
      "&#187;"      "??"    "&#188;"      "??"    "&#189;"      "??"    "&#190;"      "??"
      "&#192;"      "??"    "&#193;"      "??"    "&#194;"      "??"    "&#195;"      "??"
      "&#196;"      "??"    "&#197;"      "??"    "&#198;"      "??"    "&#199;"      "??"
      "&#200;"      "??"    "&#201;"      "??"    "&#202;"      "??"    "&#203;"      "??"
      "&#204;"      "??"    "&#205;"      "??"    "&#206;"      "??"    "&#207;"      "??"
      "&#208;"      "??"    "&#209;"      "??"    "&#210;"      "??"    "&#211;"      "??"
      "&#212;"      "??"    "&#213;"      "??"    "&#214;"      "??"    "&#215;"      "??"
      "&#216;"      "??"    "&#217;"      "??"    "&#218;"      "??"    "&#219;"      "??"
      "&#220;"      "??"    "&#221;"      "??"    "&#222;"      "??"    "&#223;"      "??"
      "&#224;"      "??"    "&#225;"      "??"    "&#226;"      "??"    "&#227;"      "??"
      "&#228;"      "??"    "&#229;"      "??"    "&#230;"      "??"    "&#231;"      "??"
      "&#232;"      "??"    "&#233;"      "??"    "&#234;"      "??"    "&#235;"      "??"
      "&#236;"      "??"    "&#237;"      "??"    "&#238;"      "??"    "&#239;"      "??"
      "&#240;"      "??"    "&#241;"      "??"    "&#242;"      "??"    "&#243;"      "??"
      "&#244;"      "??"    "&#245;"      "??"    "&#246;"      "??"    "&#247;"      "??"
      "&#248;"      "??"    "&#249;"      "??"    "&#250;"      "??"    "&#251;"      "??"
      "&#252;"      "??"    "&#253;"      "??"    "&#254;"      "??"    "&#9830;"      ""
      "&lt;"        "<"    "&gt;"        ">"    "&#47;"        "/"    "&#33;"        "!"
    } $data]
  }

  # usefull for some letter-games and some keyboards
  proc splitLinkedLetters {str} {
    set str [string map { ?? ae ?? AE ?? oe ?? OE} $str]
    return $str
  }

  proc linkLetters {str} {
    set str [string map { ae ?? AE ?? oe ?? OE ?? } $str]
    return $str
  }

  # a simplistic hardcoded permission mechanism
  proc protectedVerify {name nick uhost} {
#    putlog "Verifying for $name and $uhost/$nick"
    if {![info exists [namespace current]::owner] || ![info exists [namespace current]::allowed] || ![info exists [namespace current]::protected]} {
      return 1
    }
    if {[string match [getvar owner] $uhost]} { return 1}

    set is_protected [lsearch [getvar protected] $name]
    if {$is_protected == -1} {
      return 1
    } else {
      foreach mask [getvar allowed] {
        if {[string match $mask $uhost]} {
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
  # Please note that the correct way to write e.g. the 3th in french is '3e', not '3??me'
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
        set on "${num}??me"
      }
    }
    return $on
  }

  ###########################################
  # D??sinstallation/suppression du namespace
  # et suppression des [u]timers
  # (c) MenzAgitat am??lior?? ian (c) 2020
  ###########################################

  proc uninstall {args} {
    putlog "D??sallocation des ressources de [getvar scriptFile]..."
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
  ### Contr??le du flood 
  ### (c) MenzAgitat am??lior?? ian (c) 2020
  ### Note :
  ###   antiflood_msg = 0 : l'antiflood n'a pas encore ??t?? d??clench??
  ###   antiflood_msg = 1 : message "antiflood toujours actif" d??j?? affich??
  ###   antiflood_msg = 2 : message "antiflood d??clench??" d??j?? affich??
  ###############################################################################
# Activer le contr??le de flood ? (0 = non / 1 = oui)
  variable antiflood 1

# Seuil de d??clenchement de l'antiflood.
# Exemple : "4:180" = 4 commandes maximum en 180 secondes; les suivantes
# seront ignor??es.
  variable flood_threshold "5:180"

# Intervalle de temps minimum entre l'affichage de 2 messages avertissant que
# l'antiflood a ??t?? d??clench?? (ne r??glez pas cette valeur trop bas afin de ne
# pas ??tre flood?? par les messages avertissant de l'antiflood...)
  variable antiflood_msg_interval 20

  set l_thres [split [getvar flood_threshold] ":"]
  variable max_instances [lindex $l_thres 0]
  variable instance_length [lindex $l_thres 1]

  proc antiflood {cmd chan} {
    set cmd_hash [::md5::md5 $cmd]
    # l'antiflood est dans un statut neutre, on l'initialise
    if { ![::tcl::info::exists [namespace current]::instance($cmd_hash)] } {
      setvar instance($cmd_hash) 0
      setvar antiflood_msg($cmd_hash) 0
    }
    if { [getvar instance($cmd_hash)] >= [getvar max_instances] } {
      if { ![getvar antiflood_msg($cmd_hash)] } {
        setvar antiflood_msg($cmd_hash) 2
        broadcast $chan [utf8 "\00304:::\003 \00314Contr??le de flood activ?? pour \002$cmd\002 : pas plus de [getvar max_instances] requ??te(s) toutes les [getvar instance_length] secondes.\003"]
        if { [set msgresettimer [utimerexists "antiflood_msg_reset $cmd_hash"]] ne ""} {
          killutimer $msgresettimer
        }
        utimer [getvar antiflood_msg_interval] [list [namespace current]::antiflood_msg_reset $cmd_hash]
      } elseif { [getvar antiflood_msg($cmd_hash)] == 1 } {
        setvar antiflood_msg($cmd_hash) 2
        broadcast $chan [utf8 "\00304:::\003 \00314Le contr??le de flood est toujours actif, merci de patienter.\003"]
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
    # si le nombre d'instances retombe ?? 0, on efface les variables instance et
    # antiflood_msg afin de ne pas encombrer la m??moire inutilement
    if { [getvar instance($cmd_hash)] == 0 } {
      unsetvar instance($cmd_hash)
      unsetvar antiflood_msg($cmd_hash)
      # le nombre d'instances est retomb?? en dessous du seuil critique, on
      # r??initialise antiflood_msg
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

