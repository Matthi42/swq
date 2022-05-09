module UI where

import Prelude

import Concur.React.DOM (El, el', el)
import React (ReactClass, unsafeCreateElement)
import React.DOM.Props (unsafeFromPropsArray)
import Concur.React.Props (unsafeMkProp)

import Data.Unit (Unit)
import Effect.Uncurried (EffectFn1)
import React (unsafeCreateLeafElement, ReactClass, ReactElement)
import React.SyntheticEvent (SyntheticAnimationEvent, SyntheticClipboardEvent, SyntheticCompositionEvent, SyntheticEvent, SyntheticFocusEvent, SyntheticKeyboardEvent, SyntheticMouseEvent, SyntheticTouchEvent, SyntheticTransitionEvent, SyntheticUIEvent, SyntheticWheelEvent)

foreign import classToolbar :: forall a. ReactClass a
foreign import classButton :: forall a. ReactClass a
foreign import classAppBar :: forall a. ReactClass a
-- foreign import classToolbar :: forall a. ReactClass a


button :: El
button = el' (unsafeCreateElement classButton <<< unsafeFromPropsArray)

appBar :: El
appBar = el' (unsafeCreateElement classAppBar <<< unsafeFromPropsArray)

-- toolbar :: El
-- toolbar = el' (unsafeCreateElement classToolbar <<< const {})
-- toolbar' :: forall a. Record a -> ReactElement
toolbar' = el' (unsafeCreateElement classToolbar <<< unsafeFromPropsArray)

-- toolbar'' :: forall a. IsTSEq (Record a) (OptionRecord (ToolbarPropsO (ToolbarPropsE ToolbarPropsM)) ToolbarPropsM) => Record a -> Array ReactElement -> ReactElement
-- toolbar'' = unsafeCreateElement classToolbar


position = unsafeMkProp "position"
