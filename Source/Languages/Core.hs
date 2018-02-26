
module Languages.Core where

-- import Data.Text

data Tagged tag content = Tagged { getTag :: tag, getContent :: content}
type Named = Tagged Text

mapTagged :: (a -> a) -> (b -> c) -> Tagged a b -> Tagged a c
mapTagged f g (Tagged a b) = Tagged (f a) (g b)

mapTag f = mapTagged f id
