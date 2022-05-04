;; {{{ Required libraries

;; If LuaRocks is installed, make sure that packages installed through it are
;; found (e.g. lgi). If LuaRocks is not installed, do nothing.
(pcall require "luarocks.loader")

(local gears (require "gears"))
(local awful (require "awful"
                      "awful.autofocus"))
(local wibox (require "wibox"))
(local beautiful (require "beautiful"))
(local naughty (require "naughty"))
(local lain (require "lain"))
;; (local menubar (require "menubar"))
(local freedesktop (require "freedesktop"))
(local hotkeys_popup (require "awful.hotkeys_popup"
                              "awful.hotkeys_popup.keys"))
(local mytable (or awful.util.table gears.table)) ;; -- 4.{0,1} compatibility
(local dpi (. (require "beautiful.xresources") "apply_dpi"))

;; }}}



;; {{{ Configure notifications and their style

(set naughty.config.padding (dpi 8))
(set naughty.config.spacing (dpi 8))
(set naughty.config.notify_callback
     (fn [args]
       (if (and args.title (args.title:match "%[%a+%] from %a+"))
           (do (set args.border_width (dpi 1.5))
               (set args.border_color "#ffb86c")
               (set args.timeout 0)
               args))))

;; }}}



;; {{{ Error handling

;; Check if awesome encountered an error during startup and fell back to
;; another config (This code will only ever execute for the fallback config)
(if awesome.startup_errors
    (naughty.notify {:preset naughty.config.presets.critical
                     :title "Oops, there were errors during startup!"
                     :text awesome.startup_errors}))

;; }}}



;; {{{ Autostart windowless processes

;; This function will run once every time Awesome is started
(fn run_once [cmd_arr]
  (each [_ cmd (ipairs cmd_arr)]
    (awful.spawn.with_shell (string.format "pgrep -u $USER -fx '%s' > /dev/null || (%s)" nil nil))))

;; Running Picom with `--experimental-backends` allows for Gaussian blur on
;; the background of windows, but introduces a lot of lag, at least on a GT 710.
(run_once ["picom --experimental-backends &"
           "unclutter --timeout 2 &"])

;; }}}



;; {{{

(local HOME "/home/seanld")
(local modkey "Mod4")
(local altkey "Mod1")
(local ctrlkey "Ctrl")
(local terminal "alacritty")
(local vi_focus false)
(local cycle_prev true)
(local editor (or (os.getenv "EDITOR") "nvim"))
(local browser "librewolf")

(set awful.util.terminal terminal)
(set awful.util.tagnames ["1" "2" "3" "4" "5"])
(set awful.layout.layouts [awful.layout.suit.tile
    awful.layout.suit.tile.left
    awful.layout.suit.tile.bottom
    awful.layout.suit.tile.top
    awful.layout.suit.floating
    ;;awful.layout.suit.fair
    ;;awful.layout.suit.fair.horizontal
    ;;awful.layout.suit.spiral
    ;;awful.layout.suit.spiral.dwindle
    ;;awful.layout.suit.max
    ;;awful.layout.suit.max.fullscreen
    ;;awful.layout.suit.magnifier
    ;;awful.layout.suit.corner.nw
    ;;awful.layout.suit.corner.ne
    ;;awful.layout.suit.corner.sw
    ;;awful.layout.suit.corner.se
    ;;lain.layout.cascade
    ;;lain.layout.cascade.tile
    ;;lain.layout.centerwork
    ;;lain.layout.centerwork.horizontal
    ;;lain.layout.termfair
    lain.layout.termfair.center])

(set lain.layout.termfair.nmaster 3)
(set lain.layout.termfair.ncol 1)
(set lain.layout.termfair.center.nmaster 3)
(set lain.layout.termfair.center.ncol 1)
(set lain.layout.cascade.tile.offset_x 2)
(set lain.layout.cascade.tile.offset_y 32)
(set lain.layout.cascade.tile.extra_padding 5)
(set lain.layout.cascade.tile.nmaster 5)
(set lain.layout.cascade.tile.ncol 2)

(set awful.util.taglist_buttons
     (mytable.join (awful.button [] 1 (fn [t]
                                        (t:view_only)))
                   (awful.button [modkey] 1 (fn [t]
                                              (if client.focus
                                                  (client.focus:move_to_tag t))))
                   (awful.button [] 3 awful.tag.viewtoggle)
                   (awful.button [modkey] 3 (fn [t]
                                              (if client.focus
                                                  (client.focus:toggle_tag t))))
                   (awful.button [] 4 (fn [t]
                                        (awful.tag.viewnext t.screen)))
                   (awful.button [] 5 (fn [t]
                                        (awful.tag.viewprev t.screen)))))

(set awful.util.tasklist_buttons
     (mytable.join (awful.button [] 1 (fn [c]
                                        (if (= c client.focus)
                                            (set c.minimized true)
                                            (c:emit_signal "request::activate"
                                                           "tasklist"
                                                           {:raise true}))))
                   (awful.button [] 3 (fn []
                                        (awful.menu.client_list {:theme {:width 250}})))
                   (awful.button [] 4 (fn []
                                        (awful.client.focus.byidx 1)))
                   (awful.button [] 5 (fn []
                                        (awful.client.focus.byidx -1)))))

(beautiful.init (string.format "%s/.config/awesome/theme/theme.lua"
                               (os.getenv "HOME")))

;; }}}



;; {{{ Menu

;; Create a launcher widget and a main menu
(local myawesomemenu [["Hotkeys" (fn []
                                   (hotkeys_popup.show_help nil (awful.screen.focused)))]
                      ["Manual" (string.format "%s -e man awesome" terminal)]
                      ["Edit config" (string.format "%s -e %s %s" terminal editor awesome.conffile)]
                      ["Restart" (awesome.restart)]
                      ["Quit" (awesome.quit)]])

(local mymainmenu (freedesktop.menu.build {:before [["Awesome" myawesomemenu beautiful.awesome_icon]]
                                           :after [["Open terminal" terminal]]}))

;; Hide menu when mouse leaves it
(mymainmenu.wibox:connect_signal "mouse::leave" (fn [] (mymainmenu:hide)))

;; }}}

(local pctl_ff_filter_cmd "playerctl -l | grep firefox")

;; {{{ Screen

;; Re-set wallpaper when a screen's geometry changes (e.g. resolution changes)
(screen.connect_signal "property::geometry" (fn [s]
                                              (if beautiful.wallpaper
                                                  (do
                                                    (var wallpaper beautiful.wallpaper)
                                                    (if (= (type wallpaper) "function")
                                                        (set wallpaper (wallpaper s)))
                                                    (gears.wallpaper.maximized wallpaper s true)))))

;; Create a wibox for each screen and add it
(awful.screen.connect_for_each_screen (fn [s]
                                        (beautiful.at_screen_connect s)))

;; }}}



;; {{{ Mouse bindings

(root.buttons (mytable.join (awful.button [] 3 (fn [] (mymainmenu:toggle)))
                            (awful.button [] 4 (awful.tag.viewnext))
                            (awful.button [] 5 (awful.tag.viewprev))))

;; }}}



;; {{{ Key bindings

(var globalkeys (mytable.join (awful.key [modkey]
                                         "n"
                                         (fn []
                                           (naughty.destroy_all_notifications))
                                         {:description "destroy all notifications"
                                          :group "hotkeys"})
                              (awful.key [modkey]
                                         "p"
                                         (fn []
                                           (awful.spawn.easy_async "gnome-screenshot -i"))
                                         {:description "take a screenshot"
                                          :group "hotkeys"})
                              (awful.key [modkey]
                                         "v"
                                         (fn []
                                           (awful.spawn.easy_async (.. HOME "/scripts/mullvad.sh")))
                                         {:description "toggle Mullvad VPN"
                                          :group "hotkeys"})

                              ;; Show help
                              (awful.key [modkey]
                                         "s"
                                         hotkeys_popup.show_help
                                         {:description "show help"
                                          :group "awesome"})

                              ;; Tag browsing
                              (awful.key [modkey altkey]
                                         "Left"
                                         awful.tag.viewprev
                                         {:description "view previous"
                                          :group "tag"})
                              (awful.key [modkey altkey]
                                         "Right"
                                         awful.tag.viewnext
                                         {:description "view next"
                                          :group "tag"})
                              (awful.key [modkey altkey]
                                         "Escape"
                                         awful.tag.history.restore
                                         {:description "go back"
                                          :group "tag"})

                              ;; Default client focus
                              (awful.key [altkey]
                                         "j"
                                         (awful.client.focus.byidx 1)
                                         {:description "focus next by index"
                                          :group "client"})
                              (awful.key [altkey]
                                         "k"
                                         (awful.client.focus.byidx -1)
                                         {:description "focus previous by index"
                                          :group "client"})

                              ;; Directional client focus
                              (awful.key [modkey]
                                         "Down"
                                         (awful.client.focus.global_bydirection "down")
                                         {:description "focus down"
                                          :group "client"})
                              (awful.key [modkey]
                                         "Up"
                                         (awful.client.focus.global_bydirection "up")
                                         {:description "focus up"
                                          :group "client"})
                              (awful.key [modkey]
                                         "Left"
                                         (awful.client.focus.global_bydirection "left")
                                         {:description "focus left"
                                          :group "client"})
                              (awful.key [modkey]
                                         "Right"
                                         (awful.client.focus.global_bydirection "right")
                                         {:description "focus right"
                                          :group "client"})

                              ;; Menu
                              (awful.key [modkey]
                                         "w"
                                         (fn []
                                           (mymainmenu:show))
                                         {:description "show main menu"
                                          :group "awesome"})

                              ;; Layout manipulation
                              (awful.key [modkey "Shift"]
                                         "Left"
                                         (awful.client.swap.byidx 1)
                                         {:description "swap with next client by index"
                                          :group "client"})
                              (awful.key [modkey "Shift"]
                                         "Right"
                                         (awful.client.swap.byidx 1)
                                         {:description "swap with previous client by index"
                                          :group "client"})

                              ;; Screen-hopping
                              (awful.key [modkey "Control"]
                                         "Right"
                                         (awful.screen.focus_relative 1)
                                         {:description "focus the next screen"
                                          :group "screen"})
                              (awful.key [modkey "Control"]
                                         "Left"
                                         (awful.screen.focus_relative -1)
                                         {:description "focus the previous screen"
                                          :group "screen"})

                              ;; Quick-jump to urgent window
                              (awful.key [modkey]
                                         "u"
                                         awful.client.urgent.jumpto
                                         {:description "jump to urgent client"
                                          :group "client"})

                              ;; Show/hide wibox
                              (awful.key [modkey]
                                         "b"
                                         (fn []
                                           (each [_ s (ipairs screen)]
                                             (set s.mywibox.visible
                                                  (not s.mywibox.visible))
                                             (if s.mybottomwibox
                                                 (set s.mybottomwibox.visible
                                                      (not s.mybottomwibox.visible)))))
                                         {:description "toggle wibox"
                                          :group "awesome"})

                              ;; On-the-fly useless gaps change
                              (awful.key [modkey "Control"]
                                         "="
                                         (lain.util.useless_gaps_resize 0.5)
                                         {:description "increment useless gaps"
                                          :group "tag"})
                              (awful.key [modkey "Control"]
                                         "-"
                                         (lain.util.useless_gaps_resize -0.5)
                                         {:description "decrement useless gaps"
                                          :group "tag"})

                              ;; Quicker useless gaps change
                              (awful.key [modkey "Control"]
                                         "="
                                         (lain.util.useless_gaps_resize 2)
                                         {:description "increment useless gaps, fast"
                                          :group "tag"})
                              (awful.key [modkey "Control"]
                                         "-"
                                         (lain.util.useless_gaps_resize -0.5)
                                         {:description "decrement useless gaps, fast"
                                          :group "tag"})

                              ;; Standard program
                              (awful.key [modkey]
                                         "t"
                                         (fn [terminal]
                                           (awful.spawn terminal))
                                         {:description "open a terminal"
                                          :group "launcher"})
                              (awful.key [modkey "Control"]
                                         "r"
                                         awesome.restart
                                         {:description "reload awesome"
                                          :group "awesome"})
                              (awful.key [modkey altkey]
                                         "q"
                                         awesome.quit
                                         {:description "quit awesome"
                                          :group "awesome"})
                              (awful.key [modkey]
                                         "e"
                                         (fn []
                                           (awful.spawn "emacs"))
                                         {:description "start emacs"
                                          :group "launcher"})

                              (awful.key [modkey "Control"]
                                         "Up"
                                         (fn []
                                           (awful.tag.incmwfact 0.02))
                                         {:description "increase master width factor"
                                          :group "layout"})
                              (awful.key [modkey "Control"]
                                         "Down"
                                         (fn []
                                           (awful.tag.incmwfact -0.02))
                                         {:description "decrease master width factor"
                                          :group "layout"})
                              (awful.key [modkey altkey]
                                         "h"
                                         (fn []
                                           (awful.tag.incnmaster 1 nil true))
                                         {:description "increase number of master clients"
                                          :group "layout"})
                              (awful.key [modkey altkey]
                                         "l"
                                         (fn []
                                           (awful.tag.incnmaster -1 nil true))
                                         {:description "decrease number of master clients"
                                          :group "layout"})
                              (awful.key [modkey "Control"]
                                         "h"
                                         (fn []
                                           (awful.tag.incncol 1 nil true))
                                         {:description "increase number of columns"
                                          :group "layout"})
                              (awful.key [modkey "Control"]
                                         "l"
                                         (fn []
                                           (awful.tag.incncol -1 nil true))
                                         {:description "decrease number of columns"
                                          :group "layout"})
                              (awful.key [modkey]
                                         "space"
                                         (fn []
                                           (awful.layout.inc 1))
                                         {:description "select next layout"
                                          :group "layout"})
                              (awful.key [modkey altkey]
                                         "space"
                                         (fn []
                                           (awful.layout.inc -1))
                                         {:description "select previous layout"
                                          :group "layout"})

                              ;; Un-minimize the last minimized client
                              (awful.key [modkey "Shift"]
                                         "a"
                                         (fn []
                                           (local c (awful.client.restore))
                                           (if c
                                               (c:emit_signal "request::activate"
                                                              "key.unminimize"
                                                              {:raise true})))
                                         {:description "restore minimized"
                                          :group "client"})

                              ;; ;; Dropdown application
                              ;; (awful.key [modkey]
                              ;;            "z"
                              ;;            (fn []
                              ;;              (. (awful.screen.focused) (quake:toggle)))
                              ;;            {:description "dropdown application"
                              ;;             :group "launcher"})

                              ;; Widgets popups
                              (awful.key [altkey]
                                         "c"
                                         (fn []
                                           (if beautiful.cal
                                               (beautiful.cal.show 7)))
                                         {:description "show calendar"
                                          :group "widgets"})
                              (awful.key [altkey]
                                         "h"
                                         (fn []
                                           (if beautiful.fs
                                               (beautiful.fs.show 7)))
                                         {:description "show filesystem"
                                          :group "widgets"})
                              (awful.key [altkey]
                                         "w"
                                         (fn []
                                           (if beautiful.weather
                                               (beautiful.weather.show 7)))
                                         {:description "show weather"
                                          :group "widgets"})

                              ;; Screen brightness
                              (awful.key []
                                         "XF86MonBrightnessUp"
                                         (fn []
                                           (os.execute "xbacklight -inc 10"))
                                         {:description "brightness +10%"
                                          :group "hotkeys"})
                              (awful.key []
                                         "XF86MonBrightnessDown"
                                         (fn []
                                           (os.execute "xbacklight -dec 10"))
                                         {:description "brightness -10%"
                                          :group "hotkeys"})

                              ;; Generic volume control
                              (awful.key [altkey]
                                         "m"
                                         (fn []
                                           (os.execute (string.format "amixer -q set %s toggle"
                                                                      (or beautiful.volume.togglechannel
                                                                          beautiful.volume.channel)))
                                           (beautiful.volume.update))
                                         {:description "toggle mute"
                                          :group "hotkeys"})
                              (awful.key [altkey "Control"]
                                         "m"
                                         (fn []
                                           (os.execute (string.format "amixer -q set %s 100%%"
                                                                      beautiful.volume.channel))
                                           (beautiful.volume.update))
                                         {:description "volume 100%"
                                          :group "hotkeys"})
                              (awful.key [altkey "Control"]
                                         "0"
                                         (fn []
                                           (os.execute (string.format "amixer -q set %s 0%%"
                                                                      beautiful.volume.channel))
                                           (beautiful.volume.update))
                                         {:description "volume 0%"
                                          :group "hotkeys"})

                              ;; Firefox-specific media control bindings
                              (awful.key []
                                         "XF86AudioPlay"
                                         (fn []
                                           (os.execute (.. "playerctl play-pause -p "
                                                           pctl_ff_filter_cmd
                                                           ")")))
                                         {:description "Firefox play/pause"
                                          :group "hotkeys"})
                              (awful.key []
                                         "XF86AudioNext"
                                         (fn []
                                           (os.execute (.. "playerctl next -p "
                                                           pctl_ff_filter_cmd
                                                           ")")))
                                         {:description "Firefox next track"
                                          :group "hotkeys"})
                              (awful.key []
                                         "XF86AudioPrev"
                                         (fn []
                                           (os.execute (.. "playerctl previous -p "
                                                           pctl_ff_filter_cmd
                                                           ")")))
                                         {:description "Firefox previous track"
                                          :group "hotkeys"})
                              (awful.key [ctrlkey]
                                         "XF86AudioNext"
                                         (fn []
                                           (os.execute (.. "playerctl position 5+ -p "
                                                           pctl_ff_filter_cmd
                                                           ")")))
                                         {:description "Firefox seek 5 seconds forward"
                                          :group "hotkeys"})
                              (awful.key [ctrlkey]
                                         "XF86AudioPrev"
                                         (fn []
                                           (os.execute (.. "playerctl position 5- -p "
                                                           pctl_ff_filter_cmd
                                                           ")")))
                                         {:description "Firefox seek 5 seconds backward"
                                          :group "hotkeys"})

                              ;; Spotify-specific media control bindings
                              (awful.key []
                                         "XF86AudioPlay"
                                         (fn []
                                           (os.execute "playerctl play-pause -p spotify"))
                                         {:description "Spotify play/pause"
                                          :group "hotkeys"})
                              (awful.key []
                                         "XF86AudioNext"
                                         (fn []
                                           (os.execute "playerctl next -p spotify"))
                                         {:description "Spotify next track"
                                          :group "hotkeys"})
                              (awful.key []
                                         "XF86AudioPrev"
                                         (fn []
                                           (os.execute "playerctl previous -p spotify"))
                                         {:description "Spotify previous track"
                                          :group "hotkeys"})
                              (awful.key [modkey ctrlkey]
                                         "XF86AudioNext"
                                         (fn []
                                           (os.execute "playerctl position 5+ -p spotify"))
                                         {:description "Spotify seek 5 seconds forward"
                                          :group "hotkeys"})
                              (awful.key [modkey ctrlkey]
                                         "XF86AudioPrev"
                                         (fn []
                                           (os.execute "playerctl position 5- -p spotify"))
                                         {:description "Spotify seek 5 seconds backward"
                                          :group "hotkeys"})

                              ;; MPD Control
                              (awful.key [altkey ctrlkey]
                                         "Up"
                                         (fn []
                                           (os.execute "mpc toggle"))
                                         {:description "mpc toggle"
                                          :group "widgets"})
                              (awful.key [altkey ctrlkey]
                                         "Down"
                                         (fn []
                                           (os.execute "mpc stop"))
                                         {:description "mpc stop"
                                          :group "widgets"})
                              (awful.key [altkey ctrlkey]
                                         "Left"
                                         (fn []
                                           (os.execute "mpc prev"))
                                         {:description "mpc prev"
                                          :group "widgets"})
                              (awful.key [altkey ctrlkey]
                                         "Right"
                                         (fn []
                                           (os.execute "mpc next"))
                                         {:description "mpc next"
                                          :group "widgets"})
                              (awful.key [altkey]
                                         "0"
                                         (fn []
                                           (local common {:text "MPD widget"
                                                          :position "top_middle"
                                                          :timeout 2})
                                           (if beautiful.mpd.timer.started
                                               (do
                                                 (beautiful.mpd.timer:stop)
                                                 (set common.text (.. common.text
                                                                      (lain.util.markup.bold "OFF"))))
                                               (beautiful.mpd.timer:start)
                                               (set common.text (.. common.text
                                                                    (lain.util.markup.bold "ON"))))
                                           (naughty.notify common))
                                         {:description "mpc on/off"
                                          :group "widgets"})

                              ;; User programs
                              (awful.key [modkey]
                                         "q"
                                         (fn []
                                           (awful.spawn browser))
                                         {:description "run browser"
                                          :group "launcher"})

                              ;; TODO convert:
                              ;; awful.key({ modkey }, "x", function () awful.screen.focused().mypromptbox:run() end,
                              ;;           {description = "run prompt", group = "launcher"}),
                              ;; (awful.key [modkey]
                              ;;            "x"
                              ;;            (fn []
                              ;;              ())
                              ;;            {:description "rofi run menu"
                              ;;             :group "launcher"})

                              ;; TODO finish converting the `:textbox' section.
                              ;; (awful.key [modkey]
                              ;;            "l"
                              ;;            (fn []
                              ;;              (awful.prompt.run {:prompt "Run Lua code:"
                              ;;                                 :textbox awful.screen.focused().mypromptbox.widget
                              ;;                                 :exe_callback awful.util.eval
                              ;;                                 :history_path (.. (awful.util.get_cache_dir) "/history_eval")}))
                              ;;            {:description "lua execute prompt"
                              ;;             :group "awesome"})

                              ;; Default
                              (awful.key [modkey]
                                         "i"
                                         (fn []
                                           (awful.spawn.easy_async_with_shell "echo \"/home/seanld/Pictures/$(ls ~/Pictures | rofi -dmenu)\" | sxiv -i"))
                                         {:description "view image from ~/Pictures"
                                          :group "launcher"})

                              ;; Rofi
                              (awful.key [modkey]
                                         "r"
                                         (fn []
                                           (os.execute "rofi -show run -show-icons -matching regex -font \"Iosevka Custom 14\""))
                                         {:description "rofi run menu"
                                          :group "launcher"})))

(local clientkeys (mytable.join (awful.key [modkey "Shift"]
                                           "="
                                           (fn [c]
                                             (set c.height (+ c.height 5)))
                                           {:description "increase client height"
                                            :group "client"})
                                (awful.key [altkey "Shift"]
                                           "m"
                                           lain.util.magnify_client
                                           {:description "magnify client"
                                            :group "client"})
                                (awful.key [modkey]
                                           "f"
                                           (fn [c]
                                             (set c.fullscreen (not c.fullscreen))
                                             (c:raise))
                                           {:description "toggle fullscreen"
                                            :group "client"})
                                (awful.key [modkey]
                                           "k"
                                           (fn [c]
                                             (c:kill))
                                           {:description "kill client"
                                            :group "client"})
                                (awful.key [modkey "Control"]
                                           "space"
                                           awful.client.floating.toggle
                                           {:description "toggle floating"
                                            :group "client"})
                                (awful.key [modkey "Control"]
                                           "Return"
                                           (fn [c]
                                             (c:swap (awful.client.getmaster)))
                                           {:description "move to master"
                                            :group "client"})
                                (awful.key [modkey "Shift"]
                                           "Tab"
                                           (fn [c]
                                             (c:move_to_screen))
                                           {:description "move to screen"
                                            :group "client"})
                                (awful.key [modkey "Shift"]
                                           "t"
                                           (fn [c]
                                             (set c.ontop (not c.ontop)))
                                           {:description "toggle keep on top"
                                            :group "client"})
                                (awful.key [modkey]
                                           "a"
                                           (fn [c]
                                             (set c.minimized true))
                                           {:description "minimize client"
                                            :group "client"})
                                (awful.key [modkey]
                                           "m"
                                           (fn [c]
                                             (set c.maximized (not c.maximized))
                                             (c:raise))
                                           {:description "(un)maximize"
                                            :group "client"})
                                (awful.key [modkey ctrlkey]
                                           "m"
                                           (fn [c]
                                             (set c.maximized_vertical (not c.maximized_vertical))
                                             (c:raise))
                                           {:description "(un)maximize vertically"
                                            :group "client"})
                                (awful.key [modkey]
                                           "m"
                                           (fn [c]
                                             (set c.maximized_horizontal (not c.maximized_horizontal))
                                             (c:raise))
                                           {:description "(un)maximize horizontally"
                                            :group "client"})))

;; Bind all key numbers to tags.
;; Be careful: we use keycodes to make it work on any keyboard layout.
;; This should map on the top row of your keyboard, usually 1 to 9.
(for [i 1 9]
  (set globalkeys (mytable.join globalkeys
                                (awful.key [modkey]
                                           (.. "#" (+ i 9))
                                           (fn []
                                             (local screen (awful.screen.focused))
                                             (local tag (. screen.tags i))
                                             (if tag
                                                 (tag:view_only)))
                                           {:description (.. "view tag #" i)
                                            :group "tag"})
                                (awful.key [modkey ctrlkey]
                                           (.. "#" (+ i 9))
                                           (fn []
                                             (local screen (awful.screen.focused))
                                             (local tag (. screen.tags i))
                                             (if tag
                                                 (awful.tag.viewtoggle tag)))
                                           {:description (.. "toggle tag #" i)
                                            :group "tag"})
                                (awful.key [modkey "Shift"]
                                           (.. "#" (+ i 9))
                                           (fn []
                                             (if client.focus
                                                 (do
                                                   (local tag (. client.focus.screen.tags i))
                                                   (if tag
                                                       (client.focus:move_to_tag tag)))))
                                           {:description (.. "move focused client to tag #" i)
                                            :group "tag"})
                                (awful.key [modkey ctrlkey "Shift"]
                                           (.. "#" (+ i 9))
                                           (fn []
                                             (if client.focus
                                                 (do
                                                   (local tag (. client.focus.screen.tags i))
                                                   (if tag
                                                       (client.focus:toggle_tag tag)))))
                                           {:description (.. "toggle focused client on tag #" i)
                                            :group "tag"}))))

(local clientbuttons (mytable.join (awful.button []
                                                 1
                                                 (fn [c]
                                                   (c:emit_signal "request::activate"
                                                                  "mouse_click"
                                                                  {:raise true})))
                                   (awful.button [modkey]
                                                 1
                                                 (fn [c]
                                                   (c:emit_signal "request::activate"
                                                                  "mouse_click"
                                                                  {:raise true})
                                                   (awful.mouse.client.move c)))
                                   (awful.button [modkey]
                                                 3
                                                 (fn [c]
                                                   (c:emit_signal "request::activate"
                                                                  "mouse_click"
                                                                  {:raise true})
                                                   (awful.mouse.client.resize c)))))

;; Set keys
(root.keys globalkeys)

;; }}}



;; {{{ Rules

;; Rules to apply to new clients (through the "manage" signal).
(set awful.rules.rules [{:rule {}
                         :properties {:border_width beautiful.border_width
                                      :border_color beautiful.border_color
                                      :focus awful.client.focus.filter
                                      :raise true
                                      :keys clientkeys
                                      :buttons clientbuttons
                                      :screen awful.screen.preferred
                                      :placement (+ awful.placement.no_overlap
                                                    awful.placement.no_offscreen)
                                      :size_hints_honor false}}

                        ;; Floating clients
                        {:rule_any {:instance ["DTA" ;; Firefox addon DownThemAll
                                               "copyq" ;; Includes session name in class
                                               "pinentry"]
                                    :class ["Arandr"
                                            "Blueman-manager"
                                            "Gpick"
                                            "Kruler"
                                            "MessageWin" ;; kalarm
                                            "Sxiv"
                                            "Tor Browser" ;; Needs a fixed window size to avoid fingerprinting by size.
                                            "Wpa_gui"
                                            "veromix"
                                            "xtightvncviewer"
                                            "Zathura"
                                            "org.pwmt.zathura"
                                            "Balena-etcher-electron.bin"] ;; Balena Etcher gives hardly any detail about windows :(
                                    :name ["Event Tester"]
                                    :role ["AlarmWindow"
                                           "ConfigManager"
                                           "pop-up"
                                           "GtkFileChooserDialog"]}
                         :properties {:floating true
                                      :placement awful.placement.centered}}

                        ;; Tag 5 clients
                        {:rule_any {:instance ["qemu"]}
                         :properties {:tag "5"
                                      :screen 2
                                      :focus true
                                      :floating false}}

                        ;; Tag 5 clients
                        {:rule_any {:instance ["qemu"]}
                         :properties {:tag "4"
                                      :screen 1
                                      :focus true
                                      :floating false}}])

;; }}}



;; {{{ Signals

;; Signal function to execute when a new client appears.
(client.connect_signal "manage"
                       (fn [c]
                         (if (or (= c.name nil)
                                 (= (string.sub c.name 1 11) "User Status"))
                             (set c.floating true))
                         (if (and awesome.startup
                                  (not c.size_hints.user_position)
                                  (not c.size_hints.program_position))
                             (awful.placement.no_offscreen c))))

;; Add a titlebar if titlebars_enabled is set to true in the rules.
(client.connect_signal "request::titlebars"
                       (fn [c]
                         (if beautiful.titlebar_fun
                             (beautiful.titlebar_fun c))
                         (local buttons (mytable.join (awful.button []
                                                                    1
                                                                    (fn []
                                                                      (c:emit_signal "request::activate"
                                                                                     "titlebar"
                                                                                     {:raise true})
                                                                      (awful.mouse.client.move c)))
                                                      (awful.button []
                                                                    3
                                                                    (fn []
                                                                      (c:emit_signal "request::activate"
                                                                                     "titlebar"
                                                                                     {:raise true})
                                                                      (awful.mouse.client.resize c)))))
                         (: (awful.titlebar c {:size 16})
                            "setup"
                            {{: (awful.titlebar.widget.iconwidget c) ;; Left
                              :buttons buttons
                              :layout wibox.layout.fixed.horizontal}
                             {{:align "center" ;; Middle
                               :widget (awful.titlebar.widget.titlewidget c)}
                              :buttons buttons
                              :layout wibox.layout.flex.horizontal}
                             {1 (awful.titlebar.widget.floatingbutton c) ;; Right
                              2 (awful.titlebar.widget.maximizedbutton c)
                              3 (awful.titlebar.widget.stickybutton c)
                              4 (awful.titlebar.widget.ontopbutton c)
                              5 (awful.titlebar.widget.closebutton c)
                              :layout (wibox.layout.fixed.horizontal)}
                             :layout wibox.layout.align.horizontal})))

;; }}}
