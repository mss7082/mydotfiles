--
-- xmonad example config file.
--
-- A template showing all available configuration hooks,
-- and how to override the defaults in your own xmonad.hs conf file.
--
-- Normally, you'd only override those defaults you care about.
--

import XMonad
import Data.Monoid
import Data.Tree
import System.Exit
import XMonad.Util.SpawnOnce
import XMonad.Util.Run
import Graphics.X11.ExtraTypes.XF86


import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import Data.Maybe (fromJust)

-- Hooks 
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.WorkspaceHistory


-- Layouts modifiers
import XMonad.Layout.LayoutModifier
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))
import XMonad.Layout.Spacing
import XMonad.Layout.Renamed
import XMonad.Layout.NoBorders
import XMonad.Layout.WindowNavigation
import XMonad.Layout.SubLayouts
import XMonad.Layout.Simplest
import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))
import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))

-- Layouts
import XMonad.Layout.Spiral
import XMonad.Layout.Tabbed
import XMonad.Layout.SimplestFloat
import XMonad.Layout.GridVariants (Grid(Grid))
import XMonad.Layout.ThreeColumns
import XMonad.Layout.ResizableTile

-- Actions
import XMonad.Actions.MouseResize
import XMonad.Actions.TreeSelect


-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal      = "alacritty"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- Whether clicking on a window to focus also passes the click to the window
myClickJustFocuses :: Bool
myClickJustFocuses = False

-- Width of the window border in pixels.
--
myBorderWidth   = 2

--Makes setting the spacingRaw simpler to write. The spacingRaw module adds a configurable amount of space around windows.
mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

-- Below is a variation of the above except no borders are applied
-- if fewer than two windows. So a single window has no gaps.
mySpacing' :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing' i = spacingRaw True (Border i i i i) True (Border i i i i) True


-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask       = mod1Mask

-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
--myWorkspaces    = ["1","2","3","4","5","6","7","8","9"]
myWorkspaces :: Forest String
myWorkspaces = [ Node "Web"       -- for everyday activity's
                   [ Node "Browser" []   --  with 4 extra sub-workspaces, for even more activity's
                   , Node "Chat" []
                   , Node "Email" []
                   , Node "Ada" []
                   ]
               , Node "Dev" []
               , Node "Networking" [] -- for all your networking needs
               , Node "Misc" [] -- for all your networking needs
               ]

-- Tree Workspaces config 
myTreeConfig = TSConfig { ts_hidechildren = True
                           , ts_background   = 0xc0c0c0c0
                           , ts_font         = "xft:Sans-16"
                           , ts_node         = (0xff000000, 0xff50d0db)
                           , ts_nodealt      = (0xff000000, 0xff10b8d6)
                           , ts_highlight    = (0xffffffff, 0xffff0000)
                           , ts_extra        = 0xff000000
                           , ts_node_width   = 200
                           , ts_node_height  = 30
                           , ts_originX      = 0
                           , ts_originY      = 0
                           , ts_indent       = 80
                           , ts_navigate     = myTreeNavigation
                           }


-- Tree Workspace Navigation
myTreeNavigation = M.fromList
    [ ((0, xK_Escape), cancel)
    , ((0, xK_Return), select)
    , ((0, xK_space),  select)
    , ((0, xK_Up),     movePrev)
    , ((0, xK_Down),   moveNext)
    , ((0, xK_Left),   moveParent)
    , ((0, xK_Right),  moveChild)
    , ((0, xK_k),      movePrev)
    , ((0, xK_j),      moveNext)
    , ((0, xK_h),      moveParent)
    , ((0, xK_l),      moveChild)
    , ((0, xK_o),      moveHistBack)
    , ((0, xK_i),      moveHistForward)
    , ((0, xK_b),      moveTo ["Web", "Browser"])
    , ((0, xK_c),      moveTo ["Web", "Chat"])
    , ((0, xK_e),      moveTo ["Web", "Email"])
    , ((0, xK_a),      moveTo ["Web", "Ada"])
    , ((0, xK_n),      moveTo ["Networking"])
    , ((0, xK_d),      moveTo ["Dev"])
    , ((0, xK_m),      moveTo ["Misc"])
    ]

-- Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor  = "#dddddd"
myFocusedBorderColor = "#ff0000"

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    -- launch a terminal
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)

    -- launch dmenu
    , ((modm,               xK_p     ), spawn "dmenu_run")

    -- launch gmrun
    , ((modm .|. shiftMask, xK_p     ), spawn "gmrun")

    -- close focused window
    , ((modm .|. shiftMask, xK_c     ), kill)

     -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)

    -- Move focus to the next window
    , ((modm,               xK_Tab   ), windows W.focusDown)

    -- Move focus to the next window
    , ((modm,               xK_j     ), windows W.focusDown)

    -- Move focus to the previous window
    , ((modm,               xK_k     ), windows W.focusUp  )

    -- Move focus to the master window
    , ((modm,               xK_m     ), windows W.focusMaster  )

    -- Swap the focused window and the master window
    , ((modm,               xK_Return), windows W.swapMaster)

    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )

    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- Shrink the master area
    , ((modm,               xK_h     ), sendMessage Shrink)

    -- Expand the master area
    , ((modm,               xK_l     ), sendMessage Expand)

    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))

    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
    -- , ((modm              , xK_b     ), sendMessage ToggleStruts)

    -- Quit xmonad
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))

    -- Restart xmonad
    , ((modm              , xK_q     ), spawn "xmonad --recompile; xmonad --restart")

    -- Run xmessage with a summary of the default keybindings (useful for beginners)
    , ((modm .|. shiftMask, xK_slash ), spawn ("echo \"" ++ help ++ "\" | xmessage -file -"))
    ]
    ++

    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

    ++
    -- Enable Multmedia volume keys
    [((0, xF86XK_AudioRaiseVolume       ), spawn "pactl set-sink-volume 0 +1.5%")
    ,((0, xF86XK_AudioLowerVolume       ), spawn "pactl set-sink-volume 0 -1.5%")
    ,((0, xF86XK_AudioMute              ), spawn "pactl set-sink-mute 0 toggle")
    ]

    ++
    -- TreeSelect Workspaces
    [((modm, xK_f), treeselectWorkspace myTreeConfig myWorkspaces W.greedyView)
    ,((modm .|. shiftMask, xK_f), treeselectWorkspace myTreeConfig myWorkspaces W.shift)
    ]

    ++
    -- PrintScreen Binding
    --[((0, xK_Print), spawn "scrot -q 1 $HOME/pictures/screenshots/%Y-%m-%d-%H:%M:%S.png")]
    [((0, xK_Print), spawn "maim -s | xclip -selection clipboard -t image/png")]

    ++
    -- ResizableTall keys
    [((modm, xK_Left),  sendMessage MirrorExpand)
    ,((modm, xK_Up),    sendMessage MirrorExpand)
    ,((modm, xK_Right), sendMessage MirrorShrink)
    ,((modm, xK_Down),  sendMessage MirrorShrink)
    ]


------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

------------------------------------------------------------------------
-- Layouts:

-- Defining a bunch of layouts, many that I don't use.
-- limitWindows n sets maximum number of windows displayed for layout.
-- mySpacing n sets the gap size around the windows.
tall     = renamed [Replace "tall"]
           $ smartBorders
           $ windowNavigation
           $ subLayout [] (smartBorders Simplest)
           $ limitWindows 12
           $ mySpacing 8
           $ ResizableTall 1 (3/100) (1/2) []
spirals  = renamed [Replace "spirals"]
           $ smartBorders
           $ windowNavigation
           $ subLayout [] (smartBorders Simplest)
           $ mySpacing' 4
           $ spiral (6/7)
monocle  = renamed [Replace "monocle"]
           $ smartBorders
           $ windowNavigation
           $ subLayout [] (smartBorders Simplest)
           $ limitWindows 20 Full
grid     = renamed [Replace "grid"]
           $ smartBorders
           $ windowNavigation
           $ subLayout [] (smartBorders Simplest)
           $ limitWindows 12
           $ mySpacing 4
           $ mkToggle (single MIRROR)
           $ Grid (16/10)
threeCol = renamed [Replace "threeCol"]
           $ smartBorders
           $ windowNavigation
           $ subLayout [] (smartBorders Simplest)
           $ limitWindows 7
           $ ThreeCol 1 (3/100) (1/2)
floats   = renamed [Replace "floats"]
           $ smartBorders
           $ limitWindows 20 simplestFloat






-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--

myLayout = avoidStruts $ mouseResize $ windowArrange $ T.toggleLayouts floats
               $ mkToggle (NBFULL ?? NOBORDERS ?? EOT) myDefaultLayout
             where
               myDefaultLayout =     withBorder myBorderWidth tall
                                 ||| spirals
                                 ||| noBorders monocle
                                 ||| grid
                                 ||| threeCol
                                 ||| floats

--myLayout = avoidStruts (tiled ||| Mirror tiled ||| Full)
--  where
     -- default tiling algorithm partitions the screen into two panes
--     tiled   = Tall nmaster delta ratio

     -- The default number of windows in the master pane
--     nmaster = 1

     -- Default proportion of screen occupied by master pane
--     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
--     delta   = 3/100

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
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore ]

------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook = mempty

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
--myLogHook = return ()

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook = do
	spawnOnce "nitrogen --restore &"
	spawnOnce "picom &"

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--
main = do
  xmproc <- spawnPipe "xmobar -x 0 ~/.config/xmobar/xmobarrc"
  xmonad $ docks defaults

-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
--
-- No need to modify this.
--
defaults = def {
      -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        clickJustFocuses   = myClickJustFocuses,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = toWorkspaces myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,

      -- hooks, layouts
        layoutHook         = myLayout,
        manageHook         = myManageHook,
        handleEventHook    = myEventHook,
        --logHook            = myLogHook,
        logHook            = workspaceHistoryHook,
        startupHook        = myStartupHook
    }

-- | Finally, a copy of the default bindings in simple textual tabular format.
help :: String
help = unlines ["The default modifier key is 'alt'. Default keybindings:",
    "",
    "-- launching and killing programs",
    "mod-Shift-Enter  Launch xterminal",
    "mod-p            Launch dmenu",
    "mod-Shift-p      Launch gmrun",
    "mod-Shift-c      Close/kill the focused window",
    "mod-Space        Rotate through the available layout algorithms",
    "mod-Shift-Space  Reset the layouts on the current workSpace to default",
    "mod-n            Resize/refresh viewed windows to the correct size",
    "",
    "-- move focus up or down the window stack",
    "mod-Tab        Move focus to the next window",
    "mod-Shift-Tab  Move focus to the previous window",
    "mod-j          Move focus to the next window",
    "mod-k          Move focus to the previous window",
    "mod-m          Move focus to the master window",
    "",
    "-- modifying the window order",
    "mod-Return   Swap the focused window and the master window",
    "mod-Shift-j  Swap the focused window with the next window",
    "mod-Shift-k  Swap the focused window with the previous window",
    "",
    "-- resizing the master/slave ratio",
    "mod-h  Shrink the master area",
    "mod-l  Expand the master area",
    "",
    "-- floating layer support",
    "mod-t  Push window back into tiling; unfloat and re-tile it",
    "",
    "-- increase or decrease number of windows in the master area",
    "mod-comma  (mod-,)   Increment the number of windows in the master area",
    "mod-period (mod-.)   Deincrement the number of windows in the master area",
    "",
    "-- quit, or restart",
    "mod-Shift-q  Quit xmonad",
    "mod-q        Restart xmonad",
    "mod-[1..9]   Switch to workSpace N",
    "",
    "-- Workspaces & screens",
    "mod-Shift-[1..9]   Move client to workspace N",
    "mod-{w,e,r}        Switch to physical/Xinerama screens 1, 2, or 3",
    "mod-Shift-{w,e,r}  Move client to screen 1, 2, or 3",
    "",
    "-- Mouse bindings: default actions bound to mouse events",
    "mod-button1  Set the window to floating mode and move by dragging",
    "mod-button2  Raise the window to the top of the stack",
    "mod-button3  Set the window to floating mode and resize by dragging"]
