module Inferno where

import Control.Monad.Eff (Eff)
import DOM (DOM)
import DOM.Node.Types (Element)
import Data.Function.Uncurried (Fn2, Fn5, runFn9, Fn9, Fn3)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Nullable (toNullable, Nullable)
import Data.StrMap (StrMap)
import Prelude (Unit, ($))
import Unsafe.Coerce (unsafeCoerce)


foreign import data Prop :: *
foreign import data Props :: *
foreign import data StaticVElement :: * -- Can we just use the object shape?
foreign import data BluePrint :: *
foreign import data BluePrint0 :: *
foreign import data BluePrint1 :: * -> *
foreign import data BluePrint2 :: * -> * -> *
foreign import data BluePrint3 :: * -> * -> * -> *
foreign import data BluePrint4 :: * -> * -> * -> * -> *
foreign import data OptElementF :: *
foreign import data INFERNO :: !

foreign import render ::
  forall eff. Fn2 INode Element (Eff (dom :: DOM, inferno :: INFERNO | eff) Unit)
foreign import prop :: forall a. String -> a -> Prop
foreign import props :: Array Prop -> Props
foreign import staticVElement :: Fn3 String Props (Array StaticVElement) StaticVElement
foreign import createOptBlueprint ::
  forall a b c d.
  Fn9
  StaticVElement
  (Nullable Int) (Nullable a)
  (Nullable Int) (Nullable b)
  (Nullable Int) (Nullable c)
  (Nullable Int) (Nullable d)
  BluePrint
  -- Je kan ook nog verder gaan door arrays door te geven maar nog niet geïmpleemnteerd

data ChildrenType a = ChildrenType Int
nonKeyed :: ChildrenType (Array INode) -- TODO should support all NodeTypes
nonKeyed = ChildrenType 1
keyed :: ChildrenType (StrMap INode) -- TODO should support all NodeTypes
keyed = ChildrenType 2
node :: ChildrenType INode --  TODO should support all NodeTypes
node = ChildrenType 3
text :: ChildrenType String
text = ChildrenType 4

foreign import data INode :: *

data DynamicValue = NoDynamicValue
                  | DynamicValueInt Int
                  | DynamicValueString String

-- | Int: volgnummer van ValueTypes
-- | DynamicValue: Ofwel volgnummer van ChildrenType ofwel naam van Prop
data Dynamic a = Dynamic Int DynamicValue
mkDynamic :: forall a. Int -> DynamicValue -> Dynamic a
mkDynamic i j = Dynamic i j

childrenD :: forall a. ChildrenType a -> Dynamic a
childrenD (ChildrenType j) = mkDynamic 1 (DynamicValueInt j)
classNameD :: Dynamic String
classNameD = mkDynamic 2 NoDynamicValue
styleD :: Dynamic String
styleD = mkDynamic 3 NoDynamicValue
dataD :: Dynamic String
dataD = mkDynamic 4 NoDynamicValue
refD :: Dynamic String -- ???
refD = mkDynamic 5 NoDynamicValue
spreadD :: Dynamic String -- JSX Spread attributes ???
spreadD = mkDynamic 6 NoDynamicValue
valueD :: Dynamic String
valueD = mkDynamic 7 NoDynamicValue
propD :: String -> Dynamic String
propD propName = mkDynamic 8 (DynamicValueString propName)


-- misschien een beetje stom maar ok
makeBP0 :: StaticVElement -> BluePrint0
makeBP0 static = toBP0 $ runFn9 createOptBlueprint static null null null null null null null null

makeBP1 :: forall a.
           StaticVElement
           -> Dynamic a
           -> BluePrint1 a
makeBP1 static (Dynamic i a) =
  toBP1 $ runFn9 createOptBlueprint static
    (toNullable $ Just i) (toSecondInfernoPart a)
    null null null null null null

makeBP2 :: forall a b.
           StaticVElement
           -> Dynamic a
           -> Dynamic b
           -> BluePrint2 a b
makeBP2 static (Dynamic i a) (Dynamic j b) =
  toBP2 $ runFn9 createOptBlueprint static
    (toNullable $ Just i) (toSecondInfernoPart a)
    (toNullable $ Just j) (toSecondInfernoPart b)
    null null null null

makeBP3 :: forall a b c.
           StaticVElement
           -> Dynamic a
           -> Dynamic b
           -> Dynamic c
           -> BluePrint3 a b c
makeBP3 static (Dynamic i a) (Dynamic j b) (Dynamic k c) =
  toBP3 $ runFn9 createOptBlueprint static
    (toNullable $ Just i) (toSecondInfernoPart a)
    (toNullable $ Just j) (toSecondInfernoPart b)
    (toNullable $ Just k) (toSecondInfernoPart c)
    null null

makeBP4 :: forall a b c d.
           StaticVElement
           -> Dynamic a
           -> Dynamic b
           -> Dynamic c
           -> Dynamic d
           -> BluePrint4 a b c d
makeBP4 static (Dynamic i a) (Dynamic j b) (Dynamic k c) (Dynamic l d) =
  toBP4 $ runFn9 createOptBlueprint static
    (toNullable $ Just i) (toSecondInfernoPart a)
    (toNullable $ Just j) (toSecondInfernoPart b)
    (toNullable $ Just k) (toSecondInfernoPart c)
    (toNullable $ Just l) (toSecondInfernoPart d)

null :: forall a. Nullable a
null = toNullable Nothing

toBP0 :: BluePrint -> BluePrint0
toBP0 = unsafeCoerce
toBP1 :: forall a. BluePrint -> BluePrint1 a
toBP1 = unsafeCoerce
toBP2 :: forall a b. BluePrint -> BluePrint2 a b
toBP2 = unsafeCoerce
toBP3 :: forall a b c. BluePrint -> BluePrint3 a b c
toBP3 = unsafeCoerce
toBP4 :: forall a b c d. BluePrint -> BluePrint4 a b c d
toBP4 = unsafeCoerce

runBP0 :: BluePrint0 -> INode
runBP0 bp =
  (toOptElement {bp: bp, dom: null, type: 2, v0: null, v1: null, v2: null, v3: null})
runBP1 :: forall a. BluePrint1 a -> a -> INode
runBP1 bp a =
  (toOptElement {bp: bp, dom: null, type: 2, v0: a,    v1: null, v2: null, v3: null})
runBP2 :: forall a b. BluePrint2 a b -> a -> b -> INode
runBP2 bp a b =
  (toOptElement {bp: bp, dom: null, type: 2, v0: a,    v1: b,    v2: null, v3: null})
runBP3 :: forall a b c. BluePrint3 a b c -> a -> b -> c -> INode
runBP3 bp a b c =
  (toOptElement {bp: bp, dom: null, type: 2, v0: a,    v1: b,    v2: c   , v3: null})
runBP4 :: forall a b c d. BluePrint4 a b c d -> a -> b -> c -> d -> INode
runBP4 bp a b c d =
  (toOptElement {bp: bp, dom: null, type: 2, v0: a,    v1: b,    v2: c   , v3: d   })

-- | CAREFUL, EXTREMELY UNSAFE FUNCTION!!!
toSecondInfernoPart :: forall a. DynamicValue -> Nullable a
toSecondInfernoPart NoDynamicValue = null
toSecondInfernoPart (DynamicValueInt j) = unsafeCoerce j
toSecondInfernoPart (DynamicValueString s) = unsafeCoerce s

-- | ANOTHER ONE
toOptElement ::
  forall a b c d e bp.
  {bp :: bp, dom :: Nullable e, type :: Int, v0 :: a, v1 :: b, v2 :: c, v3 :: d}
  -> INode
toOptElement = unsafeCoerce

-- | AND ANOTHER ONE
toTextNode :: String -> INode
toTextNode str = unsafeCoerce {dom: null, type: 3, text: str}

-- | AND ANOTHER ONE, TODO WTF IS FRAGMENT???
toFragment :: forall a. (ChildrenType a) -> a -> INode
toFragment (ChildrenType i) children =
  unsafeCoerce {dom: null, pointer: null, type: 4, children, childrenType: i}
