module Test.MySolutions where

import Prelude

import Data.List (filter, head, null, nubBy)
import Data.AddressBook (AddressBook, Entry)
import Data.Maybe (Maybe)


findEntryByStreet :: String -> AddressBook -> Maybe Entry
findEntryByStreet streetName = head <<< filter filterEntry
  where
  filterEntry :: Entry -> Boolean
  filterEntry entry = entry.address.street == streetName

isInBook :: String -> String -> AddressBook -> Boolean
isInBook firstName lastName = not null <<< filter filterEntry
  where
  filterEntry :: Entry -> Boolean
  filterEntry entry = entry.firstName == firstName && entry.lastName == lastName

removeDuplicates :: AddressBook -> AddressBook
removeDuplicates = nubBy nameEquals
  where
  nameEquals :: Entry -> Entry -> Boolean
  nameEquals e1 e2 = e1.firstName == e2.firstName && e2.lastName == e2.lastName