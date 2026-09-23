-- Extra autostart processes.
-- o.launch_on_start("my-service")
o.launch_on_start("xremap /home/tj/.config/xremap/chrome.yml")
-- Polkit authentication agent. Without one, polkitd has no way to prompt, so
-- pkexec and every GUI action needing elevation fail silently instead of asking.
o.launch_on_start("hyprpolkitagent")
