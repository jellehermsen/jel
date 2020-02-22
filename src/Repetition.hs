{-
    This file is part of Jel.

    Jel is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    any later version.

    Jel is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Jel. If not, see <https://www.gnu.org/licenses/>.
-}

module Repetition (recordDot) where

import qualified UI.NCurses as Curses
import qualified Data.Text as Text

import Types
import qualified State

recordDot :: State.State -> Curses.Event -> [Action] -> State.State
recordDot state (Curses.EventCharacter c) [] =
    state {State.dotInput  = Text.concat [State.dotInput state, Text.singleton c]}

recordDot state (Curses.EventCharacter c) actions =
    case (State.mode state, filteredActions) of
        (InsertMode, [ActCommandMode]) -> saveDotRegister $ addDotInput state c
        (InsertMode, _) -> addDotInput state c
        (CommandMode, []) -> state {State.dotInput = ""}
        (CommandMode, [ActInsertMode]) -> addDotInput state c
        (CommandMode, [ActInsertNewLine, ActInsertMode]) -> addDotInput state c
        (CommandMode, _) -> saveDotRegister $ addDotInput state c
        (_, _) -> state
    where
        filteredActions = filter isChangeAction actions

recordDot state _ _ = state

isChangeAction :: Action -> Bool
isChangeAction (ActDeleteChar _) = True
isChangeAction (ActDeleteCharBefore _) = True
isChangeAction (ActDelete _ _) = True
isChangeAction (ActDeleteLine _) = True
isChangeAction (ActInsertChar _) = True
isChangeAction (ActInsertNewLine) = True
isChangeAction (ActJoinLine _) = True
isChangeAction (ActReplaceChar _ _) = True
isChangeAction (ActInsertMode) = True
isChangeAction (ActCommandMode) = True
isChangeAction _ = False

addDotInput :: State.State -> Char -> State.State
addDotInput state c = state {
    State.dotInput  = Text.concat [State.dotInput state, Text.singleton c]
}

saveDotRegister :: State.State -> State.State
saveDotRegister state = (State.setRegister state "dot" (State.dotInput state)) {
    State.dotInput = ""
}
