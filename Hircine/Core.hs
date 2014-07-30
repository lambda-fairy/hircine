module Hircine.Core (

    -- * Types
    Message,
    Msg(..),
    Origin(..),
    Command(..),
    Method(..),

    -- * Parsing and rendering
    parseMessage,
    renderMessage,

    -- * Utilities
    decode,
    encode,
    Bytes,

    -- * For testing
    testMessage

    ) where


import Hircine.Core.Types
import Hircine.Core.Parser
