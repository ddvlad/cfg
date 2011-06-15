-- based on
-- http://haskell.org/haskellwiki/Xmonad/Config_archive/John_Goerzen's_Configuration

import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO

import XMonad.Actions.CycleWS
import XMonad.Actions.GridSelect

myManageHook = composeAll
    [ className =? "Gimp"     --> doFloat
    ]

modm = mod4Mask

main = do
    xmproc <- spawnPipe "/usr/bin/xmobar /home/ddvlad/.xmobarrc"
    xmonad $ defaultConfig
        { manageHook = manageDocks <+> myManageHook
        , layoutHook = avoidStruts $ layoutHook defaultConfig
        , logHook = dynamicLogWithPP $ xmobarPP
                { ppOutput = hPutStrLn xmproc
                , ppTitle = xmobarColor "green" "" . shorten 50
                }
        , modMask = modm
        } `additionalKeys`
        [ ((modm .|. shiftMask, xK_z),
            spawn "gnome-screensaver-command --lock")
        , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
        , ((0, xK_Print), spawn "scrot")
        -- CycleWS
        , ((modm, xK_Right), nextWS)
        , ((modm, xK_Left), prevWS)
        , ((modm .|. shiftMask, xK_Right), shiftToNext >> nextWS)
        , ((modm .|. shiftMask, xK_Left), shiftToPrev >> prevWS)
        , ((modm, xK_g), goToSelected defaultGSConfig)
        ]

-- vim: set ai:
