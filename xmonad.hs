import XMonad hiding ((|||))
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

import XMonad.Layout hiding ((|||))
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.ResizableTile
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Spiral
import XMonad.Layout.Tabbed
--import XMonad.Layout.SimpleFloat
--import XMonad.Layout.Grid
--import XMonad.Layout.Spacing
--import XMonad.Layout.NoBorders
import XMonad.Layout.WindowNavigation

import XMonad.Hooks.ManageDocks
--import XMonad.Hooks.EwmhDesktops
--import XMonad.Hooks.DynamicLog
--import XMonad.Hooks.FadeInactive
--import XMonad.Hooks.UrgencyHook
--import XMonad.Hooks.SetWMName

import XMonad.Actions.CycleWS
import XMonad.Actions.SwapWorkspaces
import XMonad.Actions.Submap


main = xmonad gnomeConfig
  { terminal            = "gnome-terminal"
  , modMask             = mod4Mask
  , focusFollowsMouse   = False
  , workspaces          = myWorkspaces
  , layoutHook          = myLayoutHook
  , manageHook          = myManageHook <+> manageHook defaultConfig
  , keys                = myKeys
  , normalBorderColor   = myInactiveBorderColor
  , focusedBorderColor  = myActiveBorderColor
  }


-- Workspaces
myWorkspaces = map show [1..9] ++ ["0"]


-- Layouts
-- windowNavigation for M-[hjkl] movement
-- desktopLayoutModifiers for proper bar/panel tiling
myLayoutHook = windowNavigation $ desktopLayoutModifiers
  ( two ||| Mirror two ||| ThreeCol 1 (3/100) (1/3) ||| spiral (1) ||| simpleTabbed )
    where two = ResizableTall 1 (3/100) (1/2) []


-- Management
myManageHook = composeAll
    [ className =? "Xmessage" --> doFloat
    , manageDocks
    ]


-- Keys
myKeys = \conf -> mkKeymap conf $
  --Window Navigation
  [ ("M-h",             sendMessage $ Go L)
  , ("M-j",             sendMessage $ Go D)
  , ("M-k",             sendMessage $ Go U)
  , ("M-l",             sendMessage $ Go R)
  , ("M-m",             windows W.focusMaster)

  --Window Movement
  , ("M-S-h",           sendMessage $ Swap L)
  , ("M-S-j",           sendMessage $ Swap D)
  , ("M-S-k",           sendMessage $ Swap U)
  , ("M-S-l",           sendMessage $ Swap R)
  , ("M-S-m",           windows W.swapMaster)

  --Drop floating window back into tiling
  , ("M-t",             withFocused $ windows . W.sink)

  --kill window
  , ("M-q",             kill)
  ]

  --Shift workspaces
  -- mod-[1..0], Switch to workspace N
  -- mod-shift-[1..0], Move client to workspace N
  ++
  [ (m ++ i, windows $ f j)
  | (i, j) <- zip (myWorkspaces) (XMonad.workspaces conf)
  , (m, f) <- [("M-", W.view), ("M-S-", W.shift)] --Shift wndw to ws
  ]

  --Shift monitors
  -- mod-{u,i,o}, Switch to physical/Xinerama screens 1, 2, 3
  -- mod-shift-{u,i,o}, Move client to screen 1, 2, 3
  ++
  [ (m ++ key, screenWorkspace sc >>= flip whenJust (windows . f))
  | (key, sc) <- zip ["u", "i", "o"] [0..]
  , (m, f) <- [("M-", W.view), ("M-S-", W.shift)]
  ]

  --Layout Management
  ++
  [ ("M-<Space>",         sendMessage NextLayout)
  , ("M-S-<Space>",       sendMessage FirstLayout)

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


