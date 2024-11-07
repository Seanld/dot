local wezterm = require 'wezterm'

local config = {
    color_scheme = 'Dracula',
    font = wezterm.font(
        'Iosevka Custom',
        { stretch = "ExtraCondensed" }
    ),
    font_size = 10.5,
    hide_tab_bar_if_only_one_tab = true,
    scrollback_lines = 10000,
    use_resize_increments = true,
}

local center_content = function(window, pane)
    local win_dim = window:get_dimensions()
    local tab_dim = pane:tab():get_size()
    local overrides = window:get_config_overrides() or {}
    local padding_left = (win_dim.pixel_width - tab_dim.pixel_width) / 2
    local padding_top = (win_dim.pixel_height - tab_dim.pixel_height) / 2
    local new_padding = {
        left = padding_left,
        right = 0,
        top = padding_top,
        bottom = 0,
    }
    if overrides.window_padding and new_padding.left == overrides.window_padding.left then
        return
    end
    overrides.window_padding = new_padding
    window:set_config_overrides(overrides)
end

wezterm.on('window-resized', center_content)
wezterm.on('window-config-reloaded', center_content)

return config