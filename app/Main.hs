{-# LANGUAGE NamedFieldPuns #-}

-- -fprint-potential-instances
---------------------------------------------------------------------------
--                                                                       --
--     _|      _|  _|      _|                                      _|    --
--       _|  _|    _|_|  _|_|    _|_|    _|_|_|      _|_|_|    _|_|_|    --
--         _|      _|  _|  _|  _|    _|  _|    _|  _|    _|  _|    _|    --
--       _|  _|    _|      _|  _|    _|  _|    _|  _|    _|  _|    _|    --
--     _|      _|  _|      _|    _|_|    _|    _|    _|_|_|    _|_|_|    --
--                                                                       --
---------------------------------------------------------------------------
-- Imports.
import qualified Data.Map        as M()
import Data.Ratio()


import System.Directory


-- import XMonad
import XMonad.Hooks.DynamicLog

import XMonad

import XMonad.Hooks.ManageDocks


import XMonad.Util.EZConfig        -- append key/mouse bindings
import XMonad.Actions.CycleWS

import XMonad.Layout(Tall)
import XMonad.Layout.Grid

import XMonad.Layout.NoBorders

import XMonad.Actions.GridSelect

import XMonad.Actions.SpawnOn
import XMonad.Hooks.SetWMName

import qualified XMonad.StackSet as W
import XMonad.Hooks.ManageHelpers

import XMonad.Layout.Magnifier

import XMonad.Layout.AutoMaster

import XMonad.Util.Scratchpad

-- dwm swap
import XMonad.Actions.DwmPromote


import XMonad.Hooks.UrgencyHook
import XMonad.Util.NamedWindows
import XMonad.Util.Run

import XMonad.Hooks.WorkspaceHistory

import           XMonad.Util.NamedScratchpad                    -- summon/dismiss running app windows

import System.Environment
import System.File.Tree (getDirectory, copyTo_)

import XMonad.Util.Cursor -- configure cursor on startuphook

data LibNotifyUrgencyHook = LibNotifyUrgencyHook deriving (Read, Show)
instance UrgencyHook LibNotifyUrgencyHook where
    urgencyHook LibNotifyUrgencyHook w = do
        name     <- getName w
        Just idx <- fmap (W.findTag w) $ gets windowset
        safeSpawn "notify-send" [show name, "workspace " ++ idx]



myStartupHook :: X ()
myStartupHook = setWMName "LG3D"

                >> spawn "killall compton; compton -CG  --backend glx --vsync opengl-swc --paint-on-overlay"
                >> spawn "setxkbmap ca multi"
                >> spawn "xset s blank && xset s 180 && xset dpms 0 360 420" -- Set screen blank after 3 min, turn off after 6 min. and suspend after 7 min.
                -- >> createDir
                >> spawn "xrdb -merge ~/.Xresources" -- load .Xresources. I mainly want solarized dark as solarized theme.
                >> spawn "fcitx-autostart"
                >> spawn "nm-applet"
                >> spawn "xsetroot -xcf ~/.xmonad/res/icons/DMZ-Black/cursors/left_ptr 200"
                >> spawn "xrandr --dpi 221"
                >> spawn "xrandr --output DP-0 --scale 1x1"
                >> spawn "xinput --set-prop 'SynPS/2 Synaptics TouchPad' 'libinput Natural Scrolling Enabled' '1'"
                >> spawnHere "killall stalonetray; stalonetray"
                >> spawn "xset s 300 5"
                >> spawn "xss-lock -n /usr/lib/xsecurelock/dimmer -l -- env XSECURELOCK_SAVER=saver_mpv xsecurelock"
                >> spawn "xinput set-prop  'libinput Accel Speed' 1"
                >> spawn "sleep 1; systemctl --user daemon-reload & systemctl --user start emacs"
                -- TODO it don't work for now but fix it
                >> spawn "systemctl --user daemon-reload && sleep 1 systemctl --user start emacs"
                -- setuping icons using this guide : https://www.xaprb.com/blog/2006/04/24/beautiful-x11-cursors/
                >> catchIO (
                             getEnv "HOME"
                             >>= \homePath
                                 ->  withCurrentDirectory  (homePath ++ "/.xmonad/res/icons")
                                     $ createDir (homePath ++ "/.icons" )
                                     >>= copyDirectory (homePath ++ "/.xmonad/res/icons")
                           )


                -- >> setDefaultCursor xC_pirate
                -- todo it don't work for now but fix it
                -- >> spawn "killall blueman-applet && blueman-applet"

-- the main function.
main :: IO ()
main = do
      conf <- statusBar myBar myXmobarPP  toggleStrutsKey myConfig
      xmonad conf


myBar :: String
myBar = "xmobar"

-- Custom PP.
-- LogHook
myXmobarPP :: PP
myXmobarPP  = xmobarPP {
               ppCurrent = xmobarColor "#f8f8f8" "DodgerBlue4" . wrap " " " "
               , ppVisible = xmobarColor "#f8f8f8" "LightSkyBlue4" . wrap " " " "
               , ppUrgent  = xmobarColor "#f8f8f8" "red4" . wrap " " " " . xmobarStrip
               , ppLayout  = wrap "" "" . xmobarColor "DarkOrange" "" . wrap " [" "] "
               , ppTitle   = xmobarColor "#61ce3c" "" . shorten 50
               , ppSep     = ""
               , ppWsSep   = " "
               , ppSort    = fmap
                               (namedScratchpadFilterOutWorkspace.)
                               (ppSort defaultPP)

               }


toggleStrutsKey :: XConfig l -> (KeyMask, KeySym)
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)


altMask :: KeyMask
altMask = mod1Mask

myModMask :: KeyMask
myModMask = mod4Mask

myTerminal :: String
myTerminal = "export TERM=xterm-256color ; alacritty"

myWorkSpaces :: [String]
myWorkSpaces = ["1", "2", "3", "4:vm", "5:media", "6", "7", "8", "9"]


myManageHook = composeAll
    [ className =? "Chromium"       --> doShift "2:web"
    , className =? "Google-chrome"  --> doShift "2:web"
    , resource  =? "desktop_window" --> doIgnore
    , className =? "Galculator"     --> doFloat
    , className =? "Steam"          --> unfloat
    , className =? "Gimp"           --> doFloat
    , resource  =? "gpicview"       --> doFloat
    , className =? "MPlayer"        --> doFloat
    -- , className =? "VirtualBox"     --> doShift "4:vm"
    , className =? "VirtualBox"     --> doShift "4:vm"
    , className =? "virt-manager"     --> doShift "4:vm"
    , className =? "qemu-system-x86_64" --> doShift "6:vm"
    , className =? "Xchat"          --> doShift "5:media"
    , className =? "stalonetray"    --> doIgnore
    -- , className =? "Spotify"        --> doShift "5:media"
    -- , title =? "Spotify"        --> doShift "5:media"
    , isFullscreen --> (doF W.focusDown <+> doFullFloat)]
    <+> manageScratchPad
    <+> namedScratchpadManageHook myScratchpads
    where unfloat = ask >>= doF . W.sink


---- Scratchpad ----
-- src: https://github.com/altercation/es-etc/blob/master/xorg/.xmonad/xmonad.hs

toggleSP sp = namedScratchpadAction myScratchpads sp

myScratchpads =
    [
      NS "spotify"      "spotify" (className =? "Spotify") centWinVBig
    , NS "mixer"        "pavucontrol" (className =? "Pavucontrol") centWinBig

    ] where
        -- order of ratios: left-margin top-margin width height
        centWin     = (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3))
        centWinBig  = (customFloating $ W.RationalRect (1/8) (1/8) (3/4) (3/4))
        centWinVBig = (customFloating $ W.RationalRect (1/40) (1/20) (19/20) (9/10))
        centWinMax  = (customFloating $ W.RationalRect (0/1) (0/1) (1/1) (1/1))
        centWinThin = (customFloating $ W.RationalRect (1/30) (1/4) (28/30) (1/2))
        centSquare  = (customFloating $ W.RationalRect (1/3) (1/4) (1/3) (1/2))
        lowerThird  = (customFloating $ W.RationalRect (0) (2/3) (1) (1/3))


-- then define your scratchpad management separately:
manageScratchPad :: ManageHook
manageScratchPad = scratchpadManageHook (W.RationalRect l t w h)

  where
    h = 0.1     -- terminal height, 10%
    w = 1       -- terminal width, 100%
    t = 1 - h   -- distance from top edge, 90%
    l = 1 - w   -- distance from left edge, 0%

createDir :: String -> IO String
createDir dirToCreate = do
                 createDirectoryIfMissing True dirToCreate
                 return dirToCreate

copyDirectory :: FilePath -> FilePath -> IO ()
copyDirectory source target = getDirectory source >>= copyTo_ target


tiled :: Tall a
tiled = Tall 1 (3/100) (1/2)


myConfig =  def {
    modMask = myModMask
    , terminal           = myTerminal
    , focusFollowsMouse  = True
    , workspaces         = myWorkSpaces
    , layoutHook = smartBorders $
                   lessBorders OnlyScreenFloat  $
                   avoidStruts $
                 magnifiercz 1.3  (Mirror tiled)
                 -- ||| Mirror (Tall 1 (3/100) (1/2))
                 ||| magnifiercz 1.3 (Tall 1 (3/100) (1/2))
                 ||| Full
                 -- ||| magnifier (Tall 1 (3/100) (1/2))
                 -- ||| magnifiercz 1.1  (Mirror tiled)
                 ||| magnifiercz 1.3 (autoMaster 1 (1/100) Grid)
  , manageHook         = manageHook def <+> manageDocks  <+> myManageHook
  , normalBorderColor  = "#2a2b2f"
  , focusedBorderColor = "DarkOrange"
  , borderWidth        = 3
  , startupHook = myStartupHook
  , logHook = workspaceHistoryHook


--
  } `additionalKeysP`
        [ ("<XF86AudioRaiseVolume>", spawn "amixer -D pulse sset Master 5%+")   -- volume up
        , ("<XF86AudioLowerVolume>", spawn "amixer -D pulse sset Master 5%-") -- volume down
        , ("<XF86MonBrightnessUp>", spawn "xbacklight -inc 10")
        , ("<XF86MonBrightnessDown>", spawn "xbacklight -dec 10")
        , ("<XF86AudioMute>"       , spawn "amixer -D pulse sset Master toggle") -- mute
        , ( "<XF86AudioPlay>"
          , spawn $
            unwords
                [ "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify"
                , "/org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.PlayPause"
                ])
        , ( "<XF86AudioNext>"
          , spawn $
            unwords
                [ "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify"
                , "/org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Next"
                ])
        , ( "<XF86AudioPrevious>"
          , spawn $
            unwords
                [ "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify"
                , "/org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Previous"
                ])
        ]
   `additionalKeys`
        [ ((controlMask .|. altMask, xK_l), spawn "env XSECURELOCK_SAVER=saver_mpv xsecurelock || slock")                             -- lock screen
        , ((controlMask, xK_Print)        , spawn "sleep 0.2; cd Pictures/scrot; scrot -s")               -- screenshot
        , ((0, xK_Print)                  , spawn "cd Pictures/scrot; scrot")                             -- screenshot
        , ((mod4Mask, xK_p)               , spawn "rofi -dpi 200 -show run")                         -- dmenu9
        , ((altMask, xK_Shift_L)          , spawn "/home/mikefaille/bin/layout-switch.sh") -- chan9ge layout
        , ((mod4Mask, xK_Left  ), prevWS)
        , ((mod4Mask, xK_Right ), nextWS)

        , (( mod4Mask .|. controlMask .|. shiftMask, xK_space), rescreen)
        , ((mod4Mask, xK_s), spawnSelected def ["xterm","gmplayer","gvim"])
        , ((mod4Mask,               xK_Return), dwmpromote)

        , ((mod4Mask .|. controlMask              , xK_equal ), sendMessage MagnifyMore)
        , ((mod4Mask .|. controlMask              , xK_minus), sendMessage MagnifyLess)
        , ((mod4Mask .|. controlMask              , xK_o    ), sendMessage ToggleOff  )
        , ((mod4Mask .|. controlMask .|. shiftMask, xK_o    ), sendMessage ToggleOn   )
        , ((mod4Mask .|. controlMask              , xK_m    ), sendMessage Toggle     )

        , ((mod4Mask .|. controlMask               , xK_n    ),   scratchpadSpawnActionTerminal myTerminal   )
        , ((mod4Mask          , xK_s        ), toggleSP "spotify")
        , ((mod4Mask          , xK_m        ), toggleSP "mixer")


        ]
