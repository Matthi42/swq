module Style where

import Prelude
import Concur.Core (Widget)
import Concur.Core.Props (Props(..))
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.MUI.DOM as MD
import Concur.React.Props as P
import Data.Array (elem, filter, intercalate, mapMaybe)
import Data.List (find)
import Data.Maybe (Maybe(..), fromJust)
import Partial.Unsafe (unsafePartial)
import React.DOM.Props (Props, unsafeMkProps) as RP
import React.SyntheticEvent (SyntheticEvent_)
import Simple.JSON (class ReadForeign)
import Unsafe.Coerce (unsafeCoerce)

kontaktCSS :: String
kontaktCSS =
  """
.header {
  display: flex
}
.header h4 {
  margin-right: auto;
  flex-grow: 1;
}
.header a { margin-left: 1rem; }

.content > * {
  margin-top: 2rem;
}
.input-edit > * {
  margin-top: 2rem;
}
.msg {
    display: flex;
    align-items: center;
    justify-content: center;
    padding: 1rem;
}
.edit-view {
    display: flex;
    justify-content: space-between
}
/* .edit-view > *:not(:last-child) { margin-right: 1rem; } */
.result-head * {
    font-weight: bold;
}
"""

formControl :: forall a. String -> String -> Widget HTML a -> Widget HTML a
formControl id label ctrl =
  MD.formControl [ P.unsafeMkProp "variant" "outlined" ]
    [ MD.inputLabel [ P.htmlFor id ] [ D.text label ]
    , ctrl
    ]

-- https://github.com/labordynamicsinstitute/metajelo-ui/blob/master/src/Metajelo/FormUtil.purs
select :: forall a. Eq a => Array (Props RP.Props a) -> a -> Array { l :: String, t :: a } -> Widget HTML a
select props selected opts =
  MD.select
    ([ (unsafeFind <<< P.unsafeTargetValue) <$> P.onChange, P.value findSelected ] <> props)
    $ map (\{ l } -> MD.menuItem [ P.value l ] [ D.text l ]) opts
  where
  unsafeFind a = (unsafePartial $ fromJust $ find (((==) a) <<< _.l) opts).t

  findSelected = (unsafePartial $ fromJust $ find (((==) selected) <<< _.t) opts).l

-- | A multi select widget
-- | props: Zusätzliche Props für das Dropdown
-- | selected: Ausgewählte Einträge
-- | opts: Verfügbare Optionen
multiSelect ::
  forall a.
  Eq a =>
  Show a =>
  ReadForeign a =>
  Array (Props RP.Props (Array a)) ->
  Array a ->
  Array { l :: String, t :: a } ->
  Widget HTML (Array a)
multiSelect props selected opts =
  MD.select
    ( [ P.multiple true
      , P.valueArray selectedLabels
      , renderValue $ intercalate " " selectedLabels
      , (mapMaybe unsafeFind <<< unsafeTargetArray) <$> P.onChange
      ]
        <> props
    )
    $ map
        ( \{ t, l } ->
            MD.menuItem [ P.key $ show t, P.value l ]
              [ MD.checkbox [ P.checked (isSelected t) ] []
              , MD.listItemText [ P.unsafeMkProp "primary" l ] []
              ]
        )
        opts
  where
  selectedLabels = map _.l $ filter (isSelected <<< _.t) opts

  isSelected a = elem a selected

  unsafeFind a = _.t <$> find (((==) a) <<< _.l) opts

unsafeTargetArray ::
  forall r.
  SyntheticEvent_ r ->
  Array String
unsafeTargetArray e = (unsafeCoerce e).target.value

renderValue :: forall a. String -> Props RP.Props a
renderValue val = PrimProp (RP.unsafeMkProps "renderValue" $ \_ -> val)

-- | A Text input that has a button attached
-- | Returns its contents on the user pressing enter, or clicking the button
-- | Inspired by <https://github.com/purescript-concur/purescript-concur-react/blob/v0.4.2/src/Concur/React/Widgets.purs#L30-L35>
textFieldWithSubmit ::
  String ->
  String ->
  -- String ->
  (forall a. Array (Props RP.Props a)) ->
  (forall a. Array (Props RP.Props a)) ->
  Widget HTML (Maybe String)
textFieldWithSubmit val butLabel inpProps butProps =
  D.div [ P.style { display: "flex" } ]
    [ MD.textField
        ( inpProps
            <> [ Nothing <$ P.onKeyEnter
              , Just <<< P.unsafeTargetValue <$> P.onChange
              , P.value val
              , P.style { flexGrow: "1" }
              , P.unsafeMkProp "variant" "outlined"
              ]
        )
        []
    , MD.button
        ( butProps
            <> [ Nothing <$ P.onClick
              , P.color "primary"
              , P.unsafeMkProp "variant" "contained"
              , P.style { marginLeft: "1rem", height: "56px" }
              ]
        )
        [ D.text butLabel ]
    ]
