import XMonad
import XMonad.Config.Desktop
import XMonad.Prompt
import XMonad.Prompt.RunOrRaise (runOrRaisePrompt)
import XMonad.Prompt.AppendFile (appendFilePrompt)
import XMonad.Actions.SpawnOn
import XMonad.Actions.CycleWS
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Util.Run
import XMonad.Util.Loggers
import XMonad.Util.SpawnOnce
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.IndependentScreens
import XMonad.Layout.ResizableTile
import XMonad.Layout.Accordion
import XMonad.Layout.Hidden
import XMonad.Layout.BorderResize
import XMonad.Layout.Spacing

import System.Exit (exitWith, ExitCode(..) )

import qualified XMonad.StackSet as W
import qualified Data.Map as M

toggleStrutsKey XConfig { XMonad.modMask = modMask } = (modMask, xK_b)

keys' conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((modMask,                    xK_F1       ), spawnOn "2:web" "chromium")
    , ((modMask,                    xK_F2       ), spawn "thunar")
    , ((modMask,                    xK_p        ), spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\"")
    , ((modMask .|. shiftMask,      xK_Return   ), spawn $ XMonad.terminal conf)
    , ((modMask .|. shiftMask,      xK_c        ), kill)
    , ((modMask .|. shiftMask,      xK_l        ), spawn "slock")
    -- Programs
    , ((0,                          xK_Print    ), spawn "deepin-screenshot -s ~/Screenshots/")
    , ((shiftMask,                  xK_Print    ), spawn "deepin-screenshot -f -s ~/Screenshots/")
    -- Media Keys
    , ((0,                          0x1008ff12  ), spawn "amixer -q sset Headphone toggle")
    , ((0,                          0x1008ff11  ), spawn "amixer -q sset Headphone 5%-")
    , ((0,                          0x1008ff13  ), spawn "amixer -q sset Headphone 5%+")
    , ((0,                          0x1008ff14  ), spawn "rhythmbox-client --play-pause")
    , ((0,                          0x1008ff17  ), spawn "rhythmbox-client --next")
    , ((0,                          0x1008ff16  ), spawn "rhythmbox-client --previous")

    -- layouts
    , ((modMask,                    xK_space    ), sendMessage NextLayout)
    , ((modMask .|. shiftMask,      xK_space    ), setLayout $ XMonad.layoutHook conf)
    , ((modMask,                    xK_b        ), sendMessage ToggleStruts)
    , ((modMask,                    xK_n        ), refresh)
    , ((modMask,                    xK_Tab      ), windows W.focusDown)
    , ((modMask,                    xK_j        ), windows W.focusDown)
    , ((modMask,                    xK_k        ), windows W.focusUp  )
    , ((modMask .|. shiftMask,      xK_j        ), windows W.swapDown)
    , ((modMask .|. shiftMask,      xK_k        ), windows W.swapUp)
    , ((modMask,                    xK_Return   ), windows W.swapMaster)
    , ((modMask,                    xK_t        ), withFocused $ windows . W.sink)
    , ((modMask,                    xK_h        ), sendMessage Shrink)
    , ((modMask,                    xK_l        ), sendMessage Expand)
    , ((modMask,                    xK_comma    ), sendMessage (IncMasterN 1))
    , ((modMask,                    xK_period   ), sendMessage (IncMasterN (-1)))
    , ((modMask,               xK_Right         ), sendMessage $ ExpandTowards R)
    , ((modMask,               xK_Left          ), sendMessage $ ExpandTowards L)
    , ((modMask,               xK_Down          ), sendMessage $ ExpandTowards D)
    , ((modMask,               xK_Up            ), sendMessage $ ExpandTowards U)
    , ((modMask .|. shiftMask , xK_Right        ), sendMessage $ ShrinkFrom R)
    , ((modMask .|. shiftMask , xK_Left         ), sendMessage $ ShrinkFrom L)
    , ((modMask .|. shiftMask , xK_Down         ), sendMessage $ ShrinkFrom D)
    , ((modMask .|. shiftMask , xK_Up           ), sendMessage $ ShrinkFrom U)
    , ((modMask,                    xK_r        ), sendMessage Rotate)
    , ((modMask,                    xK_s        ), sendMessage Swap)
    , ((modMask .|. shiftMask, xK_w ), spawn "wmctrl -a \"$(wmctrl -l | cut -f 5- -d ' ' | cut -c -20 | dmenu)\"")

    -- hidden windows
    , ((modMask .|. shiftMask,  xK_backslash), withFocused hideWindow)
    , ((modMask,                xK_backslash), popNewestHiddenWindow)

    -- workspaces
    {-
    , ((modMask .|. controlMask,   xK_Right     ), nextWS)
    , ((modMask .|. shiftMask,     xK_Right     ), shiftToNext)
    , ((modMask .|. controlMask,   xK_Left      ), prevWS)
    , ((modMask .|. shiftMask,     xK_Left      ), shiftToPrev)
    -}

    -- quit, or restart
    , ((modMask .|. shiftMask,      xK_q        ), io (exitWith ExitSuccess))
    , ((modMask,                    xK_q        ), spawn "killall conky dzen2 && xmonad --recompile && xmonad --restart")
    ]
    ++
    -- mod-[1..9] %! Switch to workspace N
    -- mod-shift-[1..9] %! Move client to workspace N
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    {-++

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]] -}

main = do
    {-secondaryStatus <- spawnPipe "xmobar -x 1 ~/.xmobarsecondaryrc"-}
    xmonad =<<
      statusBar "xmobar ~/.xmobarrc"
      xmobarPP
        { ppCurrent         = xmobarColor "#429942" "" . wrap "[" "]"
        , ppHiddenNoWindows = xmobarColor "#606060" ""
        , ppTitle           = xmobarColor "#00FF00" ""
        , ppSep             = " | "
        , ppOrder           = \(ws:_:t:_) -> [ws, t]
        }
      toggleStrutsKey
      desktopConfig
        { terminal   = "urxvt"
        , workspaces = [ "1:main", "2:web", "3:dev", "4:dev'", "5:ftp", "6:etc" ]
        , layoutHook = spacingRaw True (Border 4 0 0 4) True (Border 0 4 4 0) True $
            borderResize $
            hiddenWindows $
              emptyBSP ||| Full
        , manageHook = (composeAll . concat $
          [ [className =? c     --> doShift "3:dev"  |   c <- ["jetbrains-webstorm"] ]
          , [className =? c     --> doShift "4:dev'" |   c <- ["jetbrains-phpstorm"] ]
          , [className =? c     --> doCenterFloat    |   c <- ["Xmessage", "Thunar"] ]
          , [stringProperty "WM_WINDOW_ROLE" =? "pop-up"   --> doFloat ]
          ])
        , startupHook = do
            spawnOnOnce "2:web" "chromium"
            setWMName "LG3D"
        , keys = keys'
        , modMask = mod1Mask
        , normalBorderColor  = "#202020"
        , focusedBorderColor = "#183563"
        }
    where
      tall = ResizableTall 2 (1/10) 1 []
