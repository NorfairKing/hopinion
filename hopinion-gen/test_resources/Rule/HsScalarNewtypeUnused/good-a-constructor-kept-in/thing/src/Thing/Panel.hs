module Thing.Panel where

-- Width's module keeps its constructor, so nothing here can wrap a Word in
-- one: naming the type as the fix would be asking for something this field
-- cannot do, whatever its name suggests.
data Panel = Panel
  { panelLineWidth :: !Word
  }
