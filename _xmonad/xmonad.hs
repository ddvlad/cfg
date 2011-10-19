-- based on
-- http://haskell.org/haskellwiki/Xmonad/Config_archive/John_Goerzen's_Configuration

import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.GridSelect
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.Tabbed
import XMonad.Layout.ToggleLayouts
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO
import Graphics.X11.ExtraTypes.XF86 -- for XK_Launch1

myManageHook = composeAll
    [ className =? "Gimp"     --> doFloat
    , isFullscreen --> doFullFloat

    , className =? "Firefox"  --> doShift "web"
    , stringProperty "WM_WINDOW_ROLE" =? "Mutt"     --> doShift "mail"
    , className =? "Xchat"    --> doShift "comm"
    , className =? "Pidgin"   --> doShift "comm"
    , className =? "Gvim"     --> doShift "vi"
    ]

modm = mod4Mask
myTerminal = "gnome-terminal"

myConfig = defaultConfig
        { manageHook = manageDocks <+> myManageHook
        , startupHook = setWMName "LG3D"
        , layoutHook = avoidStruts $ smartBorders $
            toggleLayouts Full $
            Tall 1 (5/100) (1/2) |||
            Mirror (Tall 1 (5/100) (1/2)) |||
            named "Tabbed" simpleTabbed
        , workspaces = [ "web", "mail", "vi", "term", "comm" ] ++
            map show [6..9]
        , modMask = modm
        , terminal = myTerminal
        , normalBorderColor = "#1e2320"
        , focusedBorderColor = "#dca3a3"
        } `additionalKeys`
        [ ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
        , ((0, xK_Print), spawn "scrot")
        -- Spawn terminal with win-enter, in addition to win-shift-enter
        , ((modm, xK_Return), spawn myTerminal)
        -- Toggle status bar and docks
        , ((modm, xK_b), sendMessage ToggleStruts)
        -- CycleWS
        , ((modm, xK_Right), nextWS)
        , ((modm, xK_Left), prevWS)
        , ((modm .|. shiftMask, xK_Right), shiftToNext >> nextWS)
        , ((modm .|. shiftMask, xK_Left), shiftToPrev >> prevWS)

        , ((modm, xK_g), goToSelected defaultGSConfig)
        -- Toggle full layout with others, for maximize effect
        , ((modm, xK_f), sendMessage ToggleLayout)
        -- ThinkVantage button on my X220
        -- TODO find a better use for this?
        , ((0, xF86XK_Launch1), spawn myTerminal)
        , ((modm, xK_p), spawn "dmenu_run")
        ]

main = do
    xmproc <- spawnPipe "/usr/bin/xmobar -x 0 /home/vlad/.xmobarrc"
    xmonad $ ewmh $ myConfig
          { logHook = dynamicLogWithPP $ xmobarPP
                { ppOutput = hPutStrLn xmproc
                , ppCurrent = xmobarColor "#60b48a" "" . wrap "[" "]"
                , ppUrgent = xmobarColor "#ff0000" "" . xmobarStrip
                -- The above line does not work.
                , ppHidden = xmobarColor "#60b48a" ""
                , ppHiddenNoWindows = xmobarColor "#705050" ""
                , ppTitle = xmobarColor "#60b48a" "" . shorten 100
                }
          }
-- vim: set ai:
