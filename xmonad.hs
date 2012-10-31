import XMonad hiding ((|||))

-- Tools to get Gnome integration
import XMonad.Config.Desktop
import XMonad.Config.Gnome
import XMonad.Hooks.ManageDocks

-- Utils
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig
import System.Exit
import System.IO

-- Imports for various layouts
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

import XMonad.Actions.CycleWS
import XMonad.Actions.Submap

-- TODO(aarongable):
-- Use LayoutCombinators to add jump-to-layout shortcuts
-- Use Submap for more intuitive and sometimes vimlike keys
-- Use CycleWS for moving between monitors

main = xmonad $ gnomeConfig
  { terminal            = "gnome-terminal"
  , modMask             = mod4Mask
  , focusFollowsMouse   = False
  , workspaces          = myWorkspaces
  , layoutHook          = myLayoutHook
  , manageHook          = myManageHook <+> manageHook gnomeConfig
  , keys                = myKeys
  , normalBorderColor   = myInactiveBorderColor
  , focusedBorderColor  = myActiveBorderColor
  }


-- Workspaces
myWorkspaces = map show [1..9] ++ ["0"]


-- Layouts
-- windowNavigation for M-[hjkl] movement
-- desktopLayoutModifiers for proper bar/panel tiling,
--    necessary because I override the default gnomeConfig layoutHook
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

  --Kill window
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
  -- mod-{y,o}, Switch to physical/Xinerama screens 1, 2
  -- mod-shift-{y,o}, Move client to screen 1, 2
  ++
  [ (m ++ key, screenWorkspace sc >>= flip whenJust (windows . f))
  | (key, sc) <- zip ["y", "o"] [0..]
  , (m, f) <- [("M-", W.view), ("M-S-", W.shift)]
  ]
  --Shift monitors
  -- mod-{u,i}, Switch to physical/Xinerama screens 1, 2
  -- mod-shift-{u,i}, Move client to screen 1, 2
  ++
  [ (m ++ key, screenWorkspace sc >>= flip whenJust (windows . f))
  | (key, sc) <- zip ["u", "i"] [0..]
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
                                  -- colors chosen to match ubuntu
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
myActiveBorderColor = "red"
myInactiveBorderColor = "black"
