import XMonad
import XMonad.Core

import XMonad.Util.EZConfig
import XMonad.Util.Run
import XMonad.Config.Gnome
import XMonad.Config.Desktop

import Graphics.X11.Xlib
import qualified Data.Map as M
import qualified XMonad.StackSet as W
import Data.Bits ((.|.))
import System.Exit
import System.IO
import Data.Monoid

import XMonad.Layout
import XMonad.Layout.ResizableTile
import XMonad.Layout.WindowNavigation
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Spiral
import XMonad.Layout.Tabbed
--import XMonad.Layout.SimpleFloat
--import XMonad.Layout.Grid
--import XMonad.Layout.Spacing
--import XMonad.Layout.NoBorders

import XMonad.Hooks.ManageDocks
--import XMonad.Hooks.EwmhDesktops
--import XMonad.Hooks.DynamicLog
--import XMonad.Hooks.FadeInactive
--import XMonad.Hooks.UrgencyHook
--import XMonad.Hooks.SetWMName

import XMonad.Actions.CycleWS
import XMonad.Actions.SwapWorkspaces
import XMonad.Actions.Submap
--import XMonad.Actions.DwmPromote


main = xmonad gnomeConfig
      { terminal = "gnome-terminal"
      , normalBorderColor  = myInactiveBorderColor
      , focusedBorderColor = myActiveBorderColor
      , layoutHook = myLayoutHook
      , modMask = mod4Mask
      , keys = myKeys
     }

-- Layouts
myLayoutHook = desktopLayoutModifiers
    ( ResizableTall 1 (3/100) (1/2) [] ||| ThreeCol 1 (3/100) (1/3) ||| spiral (1) ||| simpleTabbed )

-- Colors
myFgColor = "grey60"
myBgColor = "black"
myHighlightedFgColor = "white"

myActiveBorderColor = "red"
myInactiveBorderColor = "grey60"

myCurrentWsFgColor = "yellow"
myUrgentWsFgColor = "purple"

myLayoutFgColor = "yellow"

mySeparatorFgColor = "red"

myUrgencyHintFgColor = "purple"
myUrgencyHintBgColor = "yellow"

-- Keys
myKeys = \conf -> mkKeymap conf $
--Window Navigation
        [ ("M-<R>",             sendMessage $ Go R)
        , ("M-<L>",             sendMessage $ Go L)
        , ("M-<U>",             sendMessage $ Go U)
        , ("M-<D>",             sendMessage $ Go D)

        , ("M-j",               windows W.focusDown)
        , ("M-k",               windows W.focusUp)
        , ("M-m",               windows W.focusMaster)

--Window Movement
        , ("M-S-<R>",           sendMessage $ Swap R)
        , ("M-S-<L>",           sendMessage $ Swap L)
        , ("M-S-<U>",           sendMessage $ Swap U)
        , ("M-S-<D>",           sendMessage $ Swap D)

        , ("M-S-j",             windows W.swapDown)
        , ("M-S-k",             windows W.swapUp)

        , ("M-S-m",             windows W.swapMaster)
     -- , ("M-S-w",             dwmpromote)

--Drop floating window
        , ("M-d",               withFocused $ windows . W.sink)

--kill window
        , ("M-q",               kill)
        ]

--Shift workspaces
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
        ++
        [ (m ++ i, windows $ f j)
        | (i, j) <- zip (map show [1..9]) (XMonad.workspaces conf)
        , (m, f) <- [("M-", W.view), ("M-S-", W.shift)] --Shift wndw to ws
        ]

    -- mod-{h,l}, Switch to physical/Xinerama screens 1, 2
    -- mod-shift-{h,l}, Move client to screen 1, 2
        ++
        [ (m ++ key, screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip ["h", "l"] [0..]
        , (m, f) <- [("M-", W.view), ("M-S-", W.shift)]
        ]

--Layout Management
        ++
        [ ("M-<Space>",         sendMessage NextLayout)
        , ("M-S-<Space>",       setLayout $ XMonad.layoutHook conf)

        , ("M--",               sendMessage (IncMasterN (-1)))
        , ("M-=",               sendMessage (IncMasterN 1))

--Shrink/expand
        , ("M-S--",             sendMessage Shrink)
        , ("M-S-=",             sendMessage Expand)

        , ("M-b",               sendMessage ToggleStruts)
        ]


--Applications
        ++
        [ ("M-<Return>",        spawn $ XMonad.terminal conf)
        , ("M-S-<Return>",      spawn $ "dmenu_run -nb '#2C001E' -nf '#AEA79F'"
                                              ++ " -sb '#AEA79F' -sf '#2C001E'"
                                              ++ " -l 4 -m 1")
        ]

--XMonad system
        ++
        [ ("M-C-<Esc>",         spawn $ "xkill")
        , ("M-S-q",             io (exitWith ExitSuccess))
        , ("M-S-r",             spawn "xmonad --recompile; xmonad --restart")
        ]
