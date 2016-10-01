module Main where

import Prelude
import Control.Monad.Aff (launchAff, later', later)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToNonElementParentNode)
import DOM.HTML.Window (document)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (ElementId(ElementId))
import Data.Maybe (maybe)
import Data.Nullable (toMaybe)
import Inferno (INode, BluePrint1, toTextNode, runBP1, render, node, childrenD, prop, props, staticVElement, makeBP1)

helloDiv :: BluePrint1 INode
helloDiv = makeBP1
           (staticVElement
              "div"
              (props [prop "class" "hey"])
              [])
           (childrenD node)

main = do
  w <- window
  d <- document w
  let dn = htmlDocumentToNonElementParentNode d
  elem <- getElementById (ElementId "app") dn
  go elem "hey first"
  launchAff $ later' 2000 (liftEff $ go elem "hey after 2 seconds")
  log "Hello sailor!"

go elem str = maybe (log "Couldn't find app elem")
                    (render (runBP1 helloDiv (toTextNode str)))
                    (toMaybe elem)
