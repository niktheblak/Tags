module Taglib.ItemData where

import qualified Data.ByteString as BS

-- | Defines a generalized form of tag item data in which the data can be
-- textual or binary.
--
-- This distinction allows specific tag format implementations to create
-- item types that are not textual by nature. In other words, the framework
-- does not force tag item data formats to be textual; therefore tag items
-- that do not contain textual data will not be processed as contanining
-- textual data.
data ItemValue =
     -- | Constructor for textual item data type.
    Text String
    -- | Constructor for binary or unspecified item data type.
    -- The binary data is tag-format specific and it may not
    -- be possible to interpret the data without in-depth
    -- knowledge of the tag format.
    | Binary BS.ByteString

instance Show ItemValue where
    show (Text t) = t
    show (Binary b) = "[binary data " ++ show (BS.length b) ++ " bytes]"

-- | Class for tag items whose data can be represented with the 'ItemData'
-- type. This should include majority of tag item formats.
class HasItemValue a where
    -- | Gets the 'ItemData' representation of the tag item's data.
    --
    -- The 'ItemData' returned by this function may not represent the actual
    -- tag data bit-by-bit; in order to get the exact serialization data
    -- specified by the tag format, use the 'serializationData' function
    -- instead.
    itemValue :: a -> ItemValue

-- | Represents a generic tag item with a key and a value.
class (Show a) => TagItem a where
    -- | Gets the key of a tag item.
    key :: a -> String
    -- | Gets the 'ItemValue' representation of the data of a tag item.
    value :: a -> ItemValue

isTextual :: ItemValue -> Bool
isTextual (Text _) = True
isTextual _ = False
