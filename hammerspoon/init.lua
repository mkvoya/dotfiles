-- Config for hammerspoon

masterkey = {"alt"}
slavekey = {"alt", "shift"}
hs.hotkey.bind(masterkey, "R", function()
    hs.reload()
end)
hs.alert.show("Config loaded")

-- hs.notify.new({title="Hammerspoon", informativeText="Config Reloaded"}):send()
--
-- some nethack moves
hs.hotkey.bind(masterkey, "K", function()
    local win = hs.window.focusedWindow()
    local f = win:frame()
    f.y = f.y - 10
    win:setFrame(f)
end)
hs.hotkey.bind(masterkey, "H", function()
    local win = hs.window.focusedWindow()
    local f = win:frame()
    f.x = f.x - 10
    win:setFrame(f)
end)
hs.hotkey.bind(masterkey, "L", function()
    local win = hs.window.focusedWindow()
    local f = win:frame()
    f.x = f.x + 10
    win:setFrame(f)
end)
hs.hotkey.bind(masterkey, "J", function()
    local win = hs.window.focusedWindow()
    local f = win:frame()
    f.y = f.y + 10
    win:setFrame(f)
end)
-- window resizing
hs.hotkey.bind(slavekey, "H", function()
    local win = hs.window.focusedWindow()
    local f = win:frame()
    local screen = win:screen()
    local max = screen:frame()

    f.x = max.x
    f.y = max.y
    f.w = max.w / 2
    f.h = max.h
    win:setFrame(f)
end)
hs.hotkey.bind(slavekey, "J", function()
    local win = hs.window.focusedWindow()
    local f = win:frame()
    local screen = win:screen()
    local max = screen:frame()

    f.x = max.x
    f.y = max.y + max.h / 2
    f.w = max.w
    f.h = max.h / 2
    win:setFrame(f)
end)
hs.hotkey.bind(slavekey, "K", function()
    local win = hs.window.focusedWindow()
    local f = win:frame()
    local screen = win:screen()
    local max = screen:frame()

    f.x = max.x
    f.y = max.y
    f.w = max.w
    f.h = max.h / 2
    win:setFrame(f)
end)
hs.hotkey.bind(slavekey, "L", function()
    local win = hs.window.focusedWindow()
    local f = win:frame()
    local screen = win:screen()
    local max = screen:frame()

    f.x = max.x + max.w / 2
    f.y = max.y
    f.w = max.w / 2
    f.h = max.h
    win:setFrame(f)
end)
hs.hotkey.bind(slavekey, "F", function()
    local win = hs.window.focusedWindow()
    local f = win:frame()
    local screen = win:screen()
    local max = screen:frame()

    f.x = max.x
    f.y = max.y
    f.w = max.w
    f.h = max.h
    win:setFrame(f)
end)
hs.hotkey.bind(slavekey, "R", function()
    local win = hs.window.focusedWindow()
    local f = win:frame()
    local screen = win:screen()
    local max = screen:frame()

    f.x = max.x + max.w / 6
    f.y = max.y + max.h / 6
    f.w = max.w * 2 / 3
    f.h = max.h * 2 / 3
    win:setFrame(f)
end)

hs.hotkey.bind(slavekey, "S", function()
    local win = hs.window.focusedWindow()
    local nextScreen = win:screen():next()
    win:moveToScreen(nextScreen)
end)
