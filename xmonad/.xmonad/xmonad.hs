import System.IO
import System.Exit
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Actions.CycleWS
-- import XMonad.Actions.DynamicWorkspaces
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spiral
import XMonad.Layout.Circle
import XMonad.Layout.CenteredMaster
import XMonad.Layout.Mosaic
import XMonad.Layout.Grid
import XMonad.Layout.Named
import XMonad.Layout.StackTile
import XMonad.Layout.Roledex
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.PerWorkspace
import XMonad.Prompt
import XMonad.Prompt.Shell
-- import XMonad.Prompt.Man
-- import XMonad.Prompt.Ssh

import qualified XMonad.StackSet as W
import qualified Data.Map        as M



modm = mod4Mask

main = do
    xmproc <- spawnPipe "xmobar"
    xmonad $ defaultConfig

        { manageHook  = myManageHook <+> manageDocks
        , terminal    = "urxvt"
        , focusedBorderColor = "#5d0017"
        , normalBorderColor  = "#1e1c10"
        , workspaces  = myWorkspaces
        , layoutHook  = myLayoutHook
        , logHook     = myLogHook xmproc
        , modMask     = modm
        , startupHook = setWMName "LG3D"
        } `additionalKeys` myKeys


myLayoutHook = avoidStruts $ toggleLayouts Full $
        onWorkspace "3:mail" (Grid) $
        smartBorders (
        named "Tall" tiled
    ||| named "Mirror Tall" (Mirror tiled)
    ||| Full
    ||| Grid
    ||| Circle
    ||| named "Center" (centerMaster Grid)
    ||| StackTile 1 delta (1/2)
    ||| Roledex
    ||| mosaic 2 [3,2]
    ||| spiral (6/7)
    )
  where
    tiled   = Tall 1 delta ratio
    ratio   = 0.5
    delta   = 0.01


myWorkspaces = ["1:web", "2:term", "3:mail", "4:vocabulary", "5:pdf", "6:skype", "7:media", "8:VirtualBox", "9:chrome"]


-- These classNames can be retrieved by executing the command
--      xprop | grep "WM_WINDOW_ROLE\|WM_CLASS"
-- in a terminal. (After that, click on the window to retrieve the information)
myManageHook = composeAll [ isFullscreen --> doF W.focusDown <+> doFullFloat
                          , className =? "Evolution"       --> doShift "3:mail"
                          , className =? "Firefox"         --> doShift "1:web"
                          , className =? "Iceweasel"       --> doShift "1:web"
                          , className =? "URxvt"           --> doShift "2:term"
                          , className =? "Video"           --> doShift "7:media"
                          , className =? "banshee"         --> doShift "7:media"
                          , className =? "Vlc"             --> doShift "7:media"
                          , className =? "Rhythmbox"       --> doShift "7:media"
                          , className =? "Evince"          --> doShift "5:pdf"
                          , className =? "Zathura"         --> doShift "5:pdf"
                          , className =? "Skype"           --> doShift "6:skype"
                          , className =? "Chromium-browser" --> doShift "9:chrome"
                          , className =? "chromium-browser" --> doShift "9:chrome"
                          , className =? "jmemorize-core-Main" --> doShift "4:vocabulary"
                          , className =? "VirtualBox"      --> doShift "8:VirtualBox"
                          , className =? "Gimp"            --> doFloat
                          ]

myLogHook h = dynamicLogWithPP $ xmobarPP
                                 { ppOutput = hPutStrLn h
                                 , ppTitle = xmobarColor "#cdcd57" "" . shorten 150
                                 , ppCurrent = xmobarColor "#cdcd57" "" -- . wrap "[" "]"
                                 , ppSep = " <fc=#3d3d07>|</fc> "
                                 }

myKeys = [ 
-- Never used these bindings; I think I can remove them.
--         ((modm,               xK_Right), nextWS)
--       , ((modm,               xK_Left),  prevWS)
--       , ((modm .|. shiftMask, xK_Right), shiftToNext)
--       , ((modm .|. shiftMask, xK_Left),  shiftToPrev)
--       , ((modm,               xK_Up),    nextScreen)
--       , ((modm,               xK_Down),  prevScreen)
         , ((modm .|. shiftMask, xK_Up),    shiftNextScreen)
         , ((modm .|. shiftMask, xK_Down),  shiftPrevScreen)
         , ((modm,               xK_z),     toggleWS)
         , ((modm .|. shiftMask, xK_l),     spawn "gnome-screensaver-command -l")
         , ((modm,               xK_f),     sendMessage ToggleLayout)
         --, ((modm,               xK_F1),    manPrompt defaultXPConfig)
         , ((modm,               xK_F2),    shellPrompt defaultXPConfig)
         --, ((modm,               xK_F3),    sshPrompt defaultXPConfig)
         -- To figure out what your key name is, use the command `xev`.
         -- The 0 below means that you do not have to press MOD-Key
         , ((0                     , 0x1008FF11), spawn "amixer set Master 2-")
         , ((0                     , 0x1008FF13), spawn "amixer set Master 2+")
         , ((0                     , 0x1008FF12), spawn "amixer set Master toggle")
         -- Make a screenshot from the currently active screen
         , ((controlMask, xK_Print), spawn "sleep 0.3; scrot -s")
         -- Make a screenshot just of the current screen
         , ((0, xK_Print), spawn "scrot")
         ]

