module Highlighter where
import Graphics.Vty.Attributes

data    HighlightElement = HighlightElement HighlightMode HighlightColor String
data    HighlightMode    = HighlightWord | HighlightRange
newtype HighlightColor   = HighlightColor String
