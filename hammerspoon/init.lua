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
hs.hotkey.bind(slavekey, "N", function()
    local win = hs.window.focusedWindow()
    local f = win:frame()
    local screen = win:screen()
    local max = screen:frame()

    f.x = max.x + max.w * 5 / 6
    f.y = max.y + max.h / 12
    f.w = max.w / 6
    f.h = max.h / 3
    win:setFrame(f)
end)

hs.hotkey.bind(slavekey, "S", function()
    local win = hs.window.focusedWindow()
    local nextScreen = win:screen():next()
    win:moveToScreen(nextScreen)
end)

hs.hotkey.bind(masterkey, "space", function()
    hs.task.new("/usr/local/bin/emacsclient", nil, function()
        hs.alert.show("done")
    end, {
    "-n ",
    "-e ",
    "'(toggle-floating-frame)'"
    }):start()
end)

function encodeURI(str)
  if (str) then
    str = string.gsub(str, "([^%w _ %- . ~])",
                      function (c) return string.format ("%%%02X", string.byte(c)) end)
    str = string.gsub (str, " ", "%%20")
  end
  return str
end

hs.hotkey.bind(
  slavekey, "C",
  function ()
    local focusedElement = hs.uielement.focusedElement();

    if (focusedElement == nil or focusedElement:selectedText() == '' or focusedElement:selectedText() == nil) then
      hs.notify.new({title="Capture", informativeText="No selected text"}):send()
      return
    end

    local focusedWindow = window.focusedWindow()

    local url = string.format("org-protocol://capture?template=c&url=hammerspoon&title=%s&body=%s", escapeUri(focusedWindow:title()), escapeUri(focusedElement:selectedText()))

    hs.notify.new({title="Capture", informativeText=focusedWindow:title() .. "\n" .. focusedElement:selectedText()}):send()
    hs.execute(string.format("open '%s'", url))
end)
