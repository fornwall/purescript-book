module Test.MySolutions where

import Prelude

import Data.List (List(..), filter, head)
import Data.AddressBook (AddressBook, Entry)
import Data.Maybe (Maybe(..))


findEntryByStreet :: String -> AddressBook -> Maybe Entry
findEntryByStreet streetName = head <<< filter filterEntry
  where
  filterEntry :: Entry -> Boolean
  filterEntry entry = entry.address.street == streetName

