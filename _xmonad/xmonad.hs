-- based on
-- http://haskell.org/haskellwiki/Xmonad/Config_archive/John_Goerzen's_Configuration

import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO

myManageHook = composeAll
    [ className =? "Gimp"     --> doFloat
    ]

main = do
    xmproc <- spawnPipe "/usr/bin/xmobar /home/ddvlad/.xmobarrc"
    xmonad $ defaultConfig
        { manageHook = manageDocks <+> myManageHook
        , layoutHook = avoidStruts $ layoutHook defaultConfig
        , logHook = dynamicLogWithPP $ xmobarPP
                { ppOutput = hPutStrLn xmproc
                , ppTitle = xmobarColor "green" "" . shorten 50
                }
        , modMask = mod4Mask
        } `additionalKeys`
        [ ((mod4Mask .|. shiftMask, xK_z),
            spawn "gnome-screensaver-command --lock")
        , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
        , ((0, xK_Print), spawn "scrot")
        ]
