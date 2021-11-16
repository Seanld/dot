# $1: Shortcut title.
# $2: Shortcut command.
# $3: Shortcut keys. You may use any of "<Ctrl>" or "<Alt>" or "<Shift>" for example.
function set_shortcut {
	python3 gnome-shortcut.py $1 $2 $3;
}

set_shortcut "Launch Firefox" "firefox" "<Ctrl><Alt>F";
set_shortcut "Launch Alacritty" "alacritty" "<Ctrl><Alt>T";
set_shortcut "Launch Emacs" "emacs" "<Ctrl><Alt>S";
set_shortcut "Launch Calc-mode" "emacs -f full-calc" "<Ctrl><Alt>C";
