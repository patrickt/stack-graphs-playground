module Edge where

import Node (Node)
import Handle (Handle)

type Edge = {
  source :: Handle Node,
  sink :: Handle Node,
  precedence :: Int
}
