import Data.Char (toLower)
import Data.Monoid (mappend)
import qualified Data.Map        as M

import System.IO
import System.Exit
import XMonad
import qualified XMonad.StackSet as W

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName

import XMonad.Actions.CycleWS
import XMonad.Actions.GridSelect
import XMonad.Actions.Search
import qualified XMonad.Actions.Submap as SM -- For layered keybindings
import XMonad.Actions.UpdatePointer
import XMonad.Actions.Volume
import XMonad.Actions.WindowGo
-- import XMonad.Actions.DynamicWorkspaces

import XMonad.Layout.CenteredMaster
import XMonad.Layout.Circle
import XMonad.Layout.Grid
import XMonad.Layout.Mosaic
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.ResizableTile
-- import XMonad.Layout.Roledex -- From the documentation: "This is a completely pointless layout which acts like Microsoft's Flip 3D"
-- import XMonad.Layout.Spiral  -- "Show your friends what XMonad can do."
import XMonad.Layout.StackTile
import XMonad.Layout.ThreeColumns
import XMonad.Layout.ToggleLayouts

import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.Man
import XMonad.Prompt.Ssh
import XMonad.Prompt.Pass

import XMonad.Util.Dzen
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.Scratchpad
import XMonad.Util.XSelection

import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.CopyWindow(copy)

-- set modm to windows key (mod4Mask)
modm = mod4Mask

-- Set my favorite terminal
myTerminal :: String
myTerminal = "URxvt"

main = do
    xmproc <- spawnPipe "xmobar"
    xmonad $ docks defaultConfig
        { manageHook  = myManageHook <+> manageDocks
        , terminal    = map toLower myTerminal
        , focusedBorderColor = "#5d0017"
        , normalBorderColor  = "#1e1c10"
        , workspaces  = myWorkspaces
        , layoutHook  = myLayoutHook
        , logHook     = myLogHook xmproc -- Notify xmobar via the log hook
        , modMask     = modm
        , startupHook = setWMName "LG3D"
        } `additionalKeys` myKeys

searchFunc :: String -> String
searchFunc s = "https://de.pons.com/übersetzung/französisch-deutsch/" ++ (escape s)
pons = searchEngineF "pons" searchFunc
gesetze = searchEngine "gesetze" "https://dejure.org/cgi-bin/suche?Suchenach="

-- This is the list of search engines that are available;
-- see the keybinding for the search action below. Once pressed, one has to
-- specialize the search engine by simply pressing another button (as defined here).
-- The prompt only opens then.
searchEngineMap method = M.fromList $
    [ ((0, xK_g), method google)
    , ((0, xK_d), method duckduckgo)
    , ((0, xK_g), method gesetze)
    , ((0, xK_i), method imdb)
    , ((0, xK_o), method openstreetmap)
    , ((0, xK_p), method pons)
    , ((0, xK_w), method wikipedia)
    , ((0, xK_y), method youtube)
    ]

-- Check XMonad.Layout.SubLayouts if you want to nest layouts
myLayoutHook = avoidStruts $ toggleLayouts Full $ -- toggle to "Full" when meta-f was pressed
        onWorkspace "3:mail" (Grid) $
        smartBorders (
        named "Tall" tiled
    ||| ThreeCol 1 (3/100) (1/2) -- See doc: http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Layout-ThreeColumns.html. (3/100) = how much do we resize every time? (1/2) = master screen will occupy 1/2 of the screen. Make this negative to refer to slave screens.
    ||| ThreeColMid 1 (3/100) (1/2) -- See above. But master screen is in the middle
    ||| named "Mirror Tall" (Mirror tiled) -- Takes the tall layout and rotates it by 90°
    ||| Full
    ||| Grid -- Useful for >= 4 windows. Will arrange them all equally.
    {-||| Circle-}
    {-||| named "Center" (centerMaster Grid)-}
    {-||| StackTile 1 delta (1/2)-}
    ||| mosaic 2 [3,2]
 --   ||| spiral (6/7)
    )
  where
    tiled   = Tall 1 delta ratio -- 1 means "1 window in master area"
    ratio   = toRational (2/(1+sqrt(5)::Double))  -- golden cut
--    ratio   = 0.5
--    delta   = 0.01
    delta   = 0.03


myWorkspaces = ["1:web", "2:term", "3:mail", "4:vocabulary", "5:pdf", "6:diverse", "7:media", "8:VM", "9:chrome"]


-- These classNames can be retrieved by executing the command
--      xprop | grep "WM_WINDOW_ROLE\|WM_CLASS"
-- in a terminal. (After that, click on the window to retrieve the information)
myManageHook = (composeAll [ isFullscreen --> doF W.focusDown <+> doFullFloat
                          , className =? "Firefox"         --> doShift "1:web"
                          , className =? "Iceweasel"       --> doShift "1:web"
                          , className =? "Firefox-esr"       --> doShift "1:web"
                          , className =? myTerminal        --> doShift "2:term"
                          , className =? "Evolution"       --> doShift "3:mail"
                          , className =? "Skype"           --> doShift "6:skype"
                          , className =? "chromium"        --> doShift "9:chrome"
                          , className =? "jmemorize-core-Main" --> doShift "4:vocabulary"
                          , className =? "VirtualBox"      --> doShift "8:VirtualBox"
                          , className =? "Gimp"            --> doFloat
                          ])
                <+> (composeAll . concat $ [
                      [ className =? x --> doShift "7:media" | x <- mediaApps ]
                     ,[ className =? x --> doShift "5:pdf"   | x <- documentApps ]
                    ])
                    where 
                      mediaApps    = [ "Video", "banshee", "vlc", "Rhythmbox" ]
                      documentApps = [ "Zathura", "Evince" ]

myLogHook h = dynamicLogWithPP $ xmobarPP
                                -- See https://hackage.haskell.org/package/xmonad-contrib-0.16/docs/XMonad-Hooks-DynamicLog.html#t:PP for config
                                 { ppOutput = hPutStrLn h -- Write output to h
                                 , ppTitle = xmobarColor "#cdcd57" "" . shorten 150 -- window title format
                                 , ppCurrent = xmobarColor "#cdcd57" "" . wrap "[" "]" -- formats currently focused workspace
                                 , ppSep = " <fc=#3d3d07>|</fc> " -- Separates the sections: workspaces | layoutname | title
                                 }

myKeys = [ 
-- Never used these bindings; I think I can remove them.
         ((modm,               xK_Right), nextWS)
       , ((modm,               xK_Left),  prevWS)
       , ((modm .|. shiftMask, xK_Right), shiftToNext)
       , ((modm .|. shiftMask, xK_Left),  shiftToPrev)
--       , ((modm,               xK_Up),    nextScreen)
--       , ((modm,               xK_Down),  prevScreen)
         , ((modm .|. shiftMask, xK_Up),    shiftNextScreen)
         , ((modm .|. shiftMask, xK_Down),  shiftPrevScreen)
         , ((modm .|. shiftMask, xK_l),     spawn "dm-tool lock") -- Used to be gnome-screensaver-command -l, but that has been deprecated for a long time. Also, I'm using lightdm not gdm
         , ((modm,               xK_f),     sendMessage ToggleLayout)
         , ((modm,               xK_F4),    spawn "xcalib -invert -alter")
--  Used with a secondary screen
--       , ((modm,               xK_F5),    spawn "xrandr && xrandr --output DisplayPort-1 --off && xrandr --output DisplayPort-1 --mode 2560x1600 --left-of eDP --primary && xmodmap ~/.Xmodmap")
         , ((modm,               xK_F5),    spawn "xrandr && xrandr --output DisplayPort-1 --off && xrandr --output DisplayPort-1 --mode 3840x2160 --dpi 144 --left-of eDP --primary && xmodmap ~/.Xmodmap")
         , ((modm,               xK_F1),    manPrompt myXPConfig)
         , ((modm,               xK_F2),    shellPrompt myXPConfig) -- I use the shell prompt also as an application launcher
         , ((modm,               xK_F3),    sshPrompt myXPConfig)
         , ((modm,               xK_F9),    spawn "light -U 10" ) -- In percent
         , ((modm,               xK_F10),   spawn "light -A 10" ) -- In percent
         , ((modm,               xK_F11),   spawn "light -S 33" ) -- In percent
         , ((modm,               xK_p),    passPrompt myXPConfig)
         -- Increment the number of windows in the master area.
         , ((modm, xK_comma), sendMessage (IncMasterN 1))
         -- Decrement the number of windows in the master area.
         , ((modm, xK_period), sendMessage (IncMasterN (-1)))
         --, ((modm,               xK_F12),   spawn "/home/heinrich/bin/change-keymap.sh")
         -- To figure out what your key name is, use the command `xev`.
         -- The 0 below means that you do not have to press MOD-Key
         -- , ((0                     , 0x1008FF11), spawn "amixer set Master 2-")
         -- , ((0                     , 0x1008FF13), spawn "amixer set Master 2+")
         -- , ((0                     , 0x1008FF12), spawn "amixer set Master toggle")
         -- , ((modm                  , xK_Page_Down), spawn "amixer set Master 600-")
         -- , ((modm                  , xK_Page_Up), spawn "amixer set Master 600+") -- was until Debian 10 1.65+
         , ((modm                  , xK_Page_Down), lowerVolume 4 >>= myVolumeAlert)
         , ((modm                  , xK_Page_Up), raiseVolume 4 >>= myVolumeAlert) -- was until Debian 10 1.65+
         -- Make a screenshot by allowing the user to select what should be captured
         , ((controlMask, xK_Print), spawn "sleep 0.3; scrot -s")
         -- Make a screenshot just of the current screen
         , ((0, xK_Print), spawn "scrot")
         -- Toggles workspace
         , ((modm, xK_Tab), toggleWorkspace)

         --
         -- Search related settings
         --
         -- search prompt
         , ((modm,                 xK_s     ), SM.submap $ searchEngineMap $ promptSearch myXPConfig)
         -- search clipboard with specified search engine
         , ((modm .|. controlMask, xK_s   ), SM.submap $ searchEngineMap $ selectSearch)
         -- open clipboard contents as URL
         , ((modm .|. shiftMask,   xK_s     ), safePromptSelection "firefox")

         -- Shows/hides my terminal
         , ((modm, xK_c), scratchpadSpawnAction defaultConfig {terminal = map toLower myTerminal } )
         , ((modm .|. shiftMask, xK_a      ), addWorkspacePrompt myXPConfig)
         , ((modm .|. shiftMask, xK_r      ), removeEmptyWorkspace)
         , ((modm .|. shiftMask, xK_v      ), selectWorkspace myXPConfig)
         ]
    where
        toggleWorkspace = windows $ W.view =<< W.tag . head . filter 
                 ((\x -> x /= "NSP" && x /= "SP") . W.tag) . W.hidden

myVolumeAlert = dzenConfig centeredWindow . show . round
    where
    centeredWindow = onCurr (center 550 366)
      >=> XMonad.Util.Dzen.font "-*-helvetica-*-r-*-*-64-*-*-*-*-*-*-*"
      >=> addArgs ["-fg", "#d8c3c3"]
      >=> addArgs ["-bg", "#373737"]

-- Xmonad.Prompt config
myXPConfig :: XPConfig
myXPConfig = def
 { XMonad.Prompt.font        = "xft:Meslo LG L DZ Regular for Powerline:size=12" -- one of my favorite fonts. add :style=bold for bold
  , XMonad.Prompt.bgColor     = "black"
  --, fgColor     = "white"
  , fgHLight    = "white"
  , bgHLight    = "black"
  , borderColor = "#8B0000" -- some type of red.
  , promptBorderWidth = 0   -- I don't want a border, so I disabled it
  , position    = Bottom
  , height      = 45
  , historySize = 256
  , alwaysHighlight = False
  }
