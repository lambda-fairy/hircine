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
    renderCommand,
    showMessage,
    showCommand,

    -- * Utilities
    decodeIrc,

    ) where


import Hircine.Core.Types
import Hircine.Core.Parser
