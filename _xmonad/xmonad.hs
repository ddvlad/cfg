--
-- xmonad example config file.
--
-- A template showing all available configuration hooks,
-- and how to override the defaults in your own xmonad.hs conf file.
--
-- Normally, you'd only override those defaults you care about.
--
 
import XMonad
import System.Exit
import System.IO
 
import qualified XMonad.StackSet as W
import qualified Data.Map        as M

import XMonad.Actions.CycleWS
import XMonad.Actions.CopyWindow
import XMonad.Actions.FindEmptyWorkspace
import XMonad.Actions.NoBorders
import qualified XMonad.Actions.Submap as SM
import qualified XMonad.Actions.Search as S
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import qualified XMonad.Prompt as P
import XMonad.Prompt.DirExec
import XMonad.Util.Run

-- Layouts
import XMonad.Layout.NoBorders
import XMonad.Layout.Tabbed
import XMonad.Layout.Combo
import XMonad.Layout.Named
import XMonad.Layout.WindowNavigation
import XMonad.Layout.PerWorkspace

import XMonad.Prompt.AppendFile
import XMonad.Hooks.SetWMName
 
-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal      = "gnome-terminal"
 
-- Width of the window border in pixels.
--
myBorderWidth   = 1
 
-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask       = mod4Mask
 
-- The mask for the numlock key. Numlock status is "masked" from the
-- current modifier status, so the keybindings will work with numlock on or
-- off. You may need to change this on some systems.
--
-- You can find the numlock modifier by running "xmodmap" and looking for a
-- modifier with Num_Lock bound to it:
--
-- > $ xmodmap | grep Num
-- > mod2        Num_Lock (0x4d)
--
-- Set numlockMask = 0 if you don't have a numlock key, or want to treat
-- numlock status separately.
--
myNumlockMask   = mod2Mask
 
-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
-- myWorkspaces    = ["1","2","3","4","5","6","7","8","9"]
myWorkspaces    = ["web", "mail", "irc/im"] ++ map show [4 .. 8 :: Int] ++ ["code"]
 
-- Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor  = "#000000"
myFocusedBorderColor = "#ff0000"
 
-- Default offset of drawable screen boundaries from each physical
-- screen. Anything non-zero here will leave a gap of that many pixels
-- on the given edge, on the that screen. A useful gap at top of screen
-- for a menu bar (e.g. 15)
--
-- An example, to set a top gap on monitor 1, and a gap on the bottom of
-- monitor 2, you'd use a list of geometries like so:
--
-- > defaultGaps = [(18,0,0,0),(0,18,0,0)] -- 2 gaps on 2 monitors
--
-- Fields are: top, bottom, left, right.
--
-- Gaps should be handled my ManageDocks now
myDefaultGaps   = [(0,0,0,0)]
 
------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
 
    -- launch a terminal
    [ ((modMask              , xK_Return), spawn $ XMonad.terminal conf)
 
    -- launch dmenu
    , ((modMask,               xK_p     ), spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\"")
 
    -- launch gmrun
    , ((modMask .|. shiftMask, xK_p     ), spawn "gmrun")
 
    -- close focused window 
    , ((modMask .|.  controlMask, xK_c     ), kill)
 
     -- Rotate through the available layout algorithms
    , ((modMask,               xK_space ), sendMessage NextLayout)
 
    --  Reset the layouts on the current workspace to default
    , ((modMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
 
    -- Resize viewed windows to the correct size
    , ((modMask,               xK_n     ), refresh)
 
    -- Move focus to the next window
    -- I never use this, so I'll make it do something else.
    --, ((modMask,               xK_Tab   ), windows W.focusDown)
 
    -- Move focus to the next window
    , ((modMask,               xK_j     ), windows W.focusDown)
 
    -- Move focus to the previous window
    , ((modMask,               xK_k     ), windows W.focusUp  )
 
    -- Move focus to the master window
    , ((modMask,               xK_m     ), windows W.focusMaster  )
 
    -- Swap the focused window and the master window
    , ((modMask .|. controlMask, xK_Return), windows W.swapMaster)
 
    -- Swap the focused window with the next window
    , ((modMask .|. controlMask, xK_j     ), windows W.swapDown  )
 
    -- Swap the focused window with the previous window
    , ((modMask .|. controlMask, xK_k     ), windows W.swapUp    )
 
    -- Shrink the master area
    , ((modMask,               xK_h     ), sendMessage Shrink)
 
    -- Expand the master area
    , ((modMask,               xK_l     ), sendMessage Expand)
 
    -- Push window back into tiling
    , ((modMask,               xK_t     ), withFocused $ windows . W.sink)
 
    -- Increment the number of windows in the master area
    , ((modMask              , xK_comma ), sendMessage (IncMasterN 1))
 
    -- Deincrement the number of windows in the master area
    , ((modMask              , xK_period), sendMessage (IncMasterN (-1)))
 
    -- toggle the status bar gap
     {-, ((modMask              , xK_b     ),
          modifyGap (\i n -> let x = (XMonad.defaultGaps conf ++ repeat (0,0,0,0)) !! i
                             in if n == x then (0,0,0,0) else x))
	^ Manual gaps have been removed in favor of ManageDocks -}
    , ((modMask		    , xK_b), sendMessage ToggleStruts)

    -- toggle window borders
    , ((modMask		    , xK_g	), withFocused toggleBorder)

    -- append a line to todo file. Uses Prompt.AppendFile
    -- , ((modMask, xK_n), appendFilePrompt P.defaultXPConfig "todo/todo_now")
 
    -- Quit xmonad
    , ((modMask .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))
 
    -- Restart xmonad
    -- This sucks, 'cause I randomly hit it when switching screens
    {-
    , ((modMask              , xK_q     ),
          broadcastMessage ReleaseResources >> restart "xmonad" True)
    -}
    ]
    ++
 
    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        --, (f, m) <- [(W.greedyView, 0), (W.shift, controlMask)]]
	, (f, m) <- [(W.greedyView, 0), (W.shift, controlMask), (copy, shiftMask)]]
    ++

    [((modMask             , xK_u     ), kill1)]
    ++

    -- 
    -- Added for CycleWS
    --
    [ ((modMask,                 xK_Right), nextWS)
    , ((modMask,                 xK_Left),  prevWS)
    , ((modMask .|. controlMask, xK_Right), shiftToNext >> nextWS)
    , ((modMask .|. controlMask, xK_Left),  shiftToPrev >> prevWS)
    , ((modMask, xK_Tab), toggleWS)]
    ++

    -- 
    -- DirExec, as WmiiActions is dead
    [ ((modMask, xK_a), dirExecPrompt (P.defaultXPConfig { P.font = "xft: DejaVu Sans:pixelsize=14" })
      spawn "/home/vlad/.xmonad/actions") ]
    ++
    
    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    -- Not using xinerama
    --
    [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

 
------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
 
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w))
 
    -- mod-button2, Raise the window to the top of the stack
    , ((modMask, button2), (\w -> focus w >> windows W.swapMaster))
 
    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modMask, button3), (\w -> focus w >> mouseResizeWindow w))
 
    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]
 
------------------------------------------------------------------------
-- Layouts:
 
-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
myLayout = avoidStruts $ windowNavigation $ smartBorders
		(tiled ||| mytab ||| Mirror tiled)
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = named "Tall" $ Tall nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1
 
     -- Default proportion of screen occupied by master pane
     ratio   = 7/10
 
     -- Percent of screen to increment by when resizing panes
     delta   = 5/100

     -- mytab is a plain tabbed layout with a custom font
     mytab = named "Tabbed" $ tabbed shrinkText
       (defaultTheme { fontName = "xft:DejaVu Sans:size=14" })
 

------------------------------------------------------------------------
-- Window rules:
 
-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook = composeAll
    [ className =? "MPlayer"        --> doFloat
    , className =? "Gimp"           --> doFloat
    , className =? "Firefox-bin"    --> doF (W.shift "web")
    , className =? "Firefox"    --> doF (W.shift "web") -- Firefox 3
    , className =? "Thunderbird-bin" --> doF (W.shift "mail")
    , className =? "Thunderbird" --> doF (W.shift "mail")
    , className =? "thunderbird" --> doF (W.shift "mail")
    , className =? "evolution" --> doF (W.shift "mail")
    , className =? "Evolution" --> doF (W.shift "mail")
    , className =? "claws-mail" --> doF (W.shift "mail")
    , className =? "Claws-mail" --> doF (W.shift "mail")
    , className =? "xchat"	    --> doF (W.shift "irc/im")
    , className =? "Xchat"	    --> doF (W.shift "irc/im")
    , title =? "Buddy List"    --> doF (W.shift "irc/im")
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "Do" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore ] <+> manageDocks

 
 
------------------------------------------------------------------------
-- Status bars and logging
 
-- Perform an arbitrary action on each internal state change or X event.
-- See the 'DynamicLog' extension for examples.
--
-- To emulate dwm's status bar
--
-- > logHook = dynamicLogDzen
--
myLogHook = ewmhDesktopsLogHook
 
------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.
 
-- Run xmonad with the settings you specify. No need to modify this.
--
-- main = xmonad defaults

my_dzen :: (XConfig (Choose Tall (Choose (Mirror Tall) Full)) -> IO ()) -> IO ()
my_dzen f = do
	h <- spawnPipe "dzen2 -ta l -tw 1440"
	f $ defaultConfig {
		logHook = dynamicLogWithPP defaultPP { 
			ppOutput = hPutStrLn h 
			, ppUrgent = ("*"++)
		}
	}

main = xmonad $ ewmh $ defaultConfig {
      -- simple stuff
        terminal           = myTerminal,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        numlockMask        = myNumlockMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,
        -- defaultGaps        = myDefaultGaps,
 
      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,
 
      -- hooks, layouts
        layoutHook         = myLayout,
        manageHook         = myManageHook,
	startupHook        = setWMName "LG3D"
        --logHook            = myLogHook
    }
 
-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will 
-- use the defaults defined in xmonad/XMonad/Config.hs
-- 
-- No need to modify this.
--
defaults = defaultConfig {
      -- simple stuff
        terminal           = myTerminal,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        numlockMask        = myNumlockMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,
        -- defaultGaps        = myDefaultGaps,
 
      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,
 
      -- hooks, layouts
        layoutHook         = myLayout,
        manageHook         = myManageHook,
        logHook            = myLogHook
    }
