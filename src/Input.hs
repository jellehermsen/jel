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
    along with Jel.  If not, see <https://www.gnu.org/licenses/>.
-}

module Input where

import Data.Char (isPrint)
import qualified UI.NCurses as Curses
import Types

data PossibleMotion = CouldBeMotion | NoMotion | Motion

-- |Given the current mode, previous commands and a NCurses event, this
-- function generates either a new list of commands for the next time, or a list
-- of actions that can be processed
parseInput :: Mode -> [Command] -> Curses.Event -> Either [Command] [Action]

-- Numbers
parseInput CommandMode [] (Curses.EventCharacter '1') = Left [CmdAmount 1]
parseInput CommandMode [] (Curses.EventCharacter '2') = Left [CmdAmount 2]
parseInput CommandMode [] (Curses.EventCharacter '3') = Left [CmdAmount 3]
parseInput CommandMode [] (Curses.EventCharacter '4') = Left [CmdAmount 4]
parseInput CommandMode [] (Curses.EventCharacter '5') = Left [CmdAmount 5]
parseInput CommandMode [] (Curses.EventCharacter '6') = Left [CmdAmount 6]
parseInput CommandMode [] (Curses.EventCharacter '7') = Left [CmdAmount 7]
parseInput CommandMode [] (Curses.EventCharacter '8') = Left [CmdAmount 8]
parseInput CommandMode [] (Curses.EventCharacter '9') = Left [CmdAmount 9]
parseInput CommandMode [CmdAmount n] (Curses.EventCharacter '1') = Left [CmdAmount (n*10+1)]
parseInput CommandMode [CmdAmount n] (Curses.EventCharacter '2') = Left [CmdAmount (n*10+2)]
parseInput CommandMode [CmdAmount n] (Curses.EventCharacter '3') = Left [CmdAmount (n*10+3)]
parseInput CommandMode [CmdAmount n] (Curses.EventCharacter '4') = Left [CmdAmount (n*10+4)]
parseInput CommandMode [CmdAmount n] (Curses.EventCharacter '5') = Left [CmdAmount (n*10+5)]
parseInput CommandMode [CmdAmount n] (Curses.EventCharacter '6') = Left [CmdAmount (n*10+6)]
parseInput CommandMode [CmdAmount n] (Curses.EventCharacter '7') = Left [CmdAmount (n*10+7)]
parseInput CommandMode [CmdAmount n] (Curses.EventCharacter '8') = Left [CmdAmount (n*10+8)]
parseInput CommandMode [CmdAmount n] (Curses.EventCharacter '9') = Left [CmdAmount (n*10+9)]
parseInput CommandMode [CmdAmount n] (Curses.EventCharacter '0') = Left [CmdAmount (n*10)]

-- Movement
parseInput CommandMode [CmdAmount n] (Curses.EventCharacter 'j') = Right $ matchActions [CmdAmount n, CmdDown]
parseInput CommandMode [] (Curses.EventCharacter 'j') = Right $ matchActions [CmdDown]

parseInput CommandMode [CmdAmount n] (Curses.EventCharacter 'k') = Right $ matchActions [CmdAmount n, CmdUp]
parseInput CommandMode [] (Curses.EventCharacter 'k') = Right $ matchActions [CmdUp]

parseInput CommandMode [CmdAmount n] (Curses.EventCharacter 'h') = Right $ matchActions [CmdAmount n, CmdLeft]
parseInput CommandMode [] (Curses.EventCharacter 'h') = Right $ matchActions [CmdLeft]

parseInput CommandMode [CmdAmount n] (Curses.EventCharacter 'l') = Right $ matchActions [CmdAmount n, CmdRight]
parseInput CommandMode [] (Curses.EventCharacter 'l') = Right $ matchActions [CmdRight]

parseInput CommandMode [CmdAmount n] (Curses.EventCharacter '$') = Right $ matchActions [CmdAmount n, CmdEndOfLine]
parseInput CommandMode [] (Curses.EventCharacter '$') = Right $ matchActions [CmdEndOfLine]

parseInput CommandMode [] (Curses.EventCharacter '^') = Right $ matchActions [CmdFirstNoneWhiteSpace]

parseInput CommandMode [] (Curses.EventCharacter '0') = Right $ matchActions [CmdBeginningOfLine]

parseInput CommandMode [CmdAmount n] (Curses.EventCharacter '\EOT') = Right $ matchActions [CmdAmount n, CmdPageDown]
parseInput CommandMode [] (Curses.EventCharacter '\EOT') = Right $ matchActions [CmdPageDown]

parseInput CommandMode [CmdAmount n] (Curses.EventCharacter '\NAK') = Right $ matchActions [CmdAmount n, CmdPageUp]
parseInput CommandMode [] (Curses.EventCharacter '\NAK') = Right $ matchActions [CmdPageUp]

-- Find forward
parseInput CommandMode [CmdAmount n] (Curses.EventCharacter 'f') = Left $ [CmdAmount n, CmdFindForward]
parseInput CommandMode [] (Curses.EventCharacter 'f') = Left $ [CmdFindForward]
parseInput CommandMode [CmdAmount n, CmdFindForward] (Curses.EventCharacter c) = Right $ matchActions [CmdAmount n, CmdFindForward, CmdChar c]
parseInput CommandMode [CmdFindForward] (Curses.EventCharacter c) = Right $ matchActions [CmdFindForward, CmdChar c]

-- Find backward
parseInput CommandMode [CmdAmount n] (Curses.EventCharacter 'F') = Left $ [CmdAmount n, CmdFindBackward]
parseInput CommandMode [] (Curses.EventCharacter 'F') = Left $ [CmdFindBackward]
parseInput CommandMode [CmdAmount n, CmdFindBackward] (Curses.EventCharacter c) = Right $ matchActions [CmdAmount n, CmdFindBackward, CmdChar c]
parseInput CommandMode [CmdFindBackward] (Curses.EventCharacter c) = Right $ matchActions [CmdFindBackward, CmdChar c]

-- Delete character(s)
parseInput CommandMode [CmdAmount n] (Curses.EventCharacter 'x') = Right $ matchActions [CmdAmount n, CmdDeleteChar]
parseInput CommandMode [] (Curses.EventCharacter 'x') = Right $ matchActions [CmdDeleteChar]

parseInput CommandMode [CmdAmount n] (Curses.EventCharacter 'X') = Right $ matchActions [CmdAmount n, CmdDeleteCharBefore]
parseInput CommandMode [] (Curses.EventCharacter 'X') = Right $ matchActions [CmdDeleteCharBefore]

-- Open new line
parseInput CommandMode [CmdAmount n] (Curses.EventCharacter 'o') = Right $ matchActions [CmdAmount n, CmdOpenLine]
parseInput CommandMode [] (Curses.EventCharacter 'o') = Right $ matchActions [CmdOpenLine]

parseInput CommandMode [CmdAmount n] (Curses.EventCharacter 'O') = Right $ matchActions [CmdAmount n, CmdOpenLineBefore]
parseInput CommandMode [] (Curses.EventCharacter 'O') = Right $ matchActions [CmdOpenLineBefore]

-- Switch to insert after mode
parseInput CommandMode [CmdAmount _] (Curses.EventCharacter 'i') = Right $ matchActions [CmdInsertMode]
parseInput CommandMode [] (Curses.EventCharacter 'i') = Right $ matchActions [CmdInsertMode]

parseInput CommandMode [] (Curses.EventCharacter '\f') = Right $ matchActions [CmdRedrawScreen]

-- Switch to insert before mode
parseInput CommandMode [CmdAmount _] (Curses.EventCharacter 'I') = Right $ matchActions [CmdInsertModeBefore]
parseInput CommandMode [] (Curses.EventCharacter 'I') = Right $ matchActions [CmdInsertModeBefore]

-- Append
parseInput CommandMode [CmdAmount _] (Curses.EventCharacter 'a') = Right $ matchActions [CmdAppend]
parseInput CommandMode [] (Curses.EventCharacter 'a') = Right $ matchActions [CmdAppend]

-- Switch back to command mode
parseInput CommandMode _ (Curses.EventCharacter '\ESC') = Left []
parseInput InsertMode [] (Curses.EventCharacter '\ESC') = Right $ matchActions [CmdCommandMode]
parseInput ReplaceMode [] (Curses.EventCharacter '\ESC') = Right $ matchActions [CmdCommandMode]

-- Replace single character
parseInput CommandMode [] (Curses.EventCharacter 'r') = Left [CmdReplaceChar 1]
parseInput CommandMode [CmdAmount n] (Curses.EventCharacter 'r') = Left [CmdReplaceChar n]
parseInput CommandMode [CmdReplaceChar n] (Curses.EventCharacter c)
    | isPrint c = Right $ matchActions [CmdReplaceChar n, CmdChar c]
    | otherwise = Left []

-- Insert newline
parseInput InsertMode [] (Curses.EventCharacter '\n') = Right $ matchActions [CmdInsertNewLine]
parseInput ReplaceMode [] (Curses.EventCharacter '\n') = Right $ matchActions [CmdInsertNewLine]

-- Backspace
parseInput InsertMode [] (Curses.EventCharacter '\b') = Right $ matchActions [CmdBackspace]

-- Insert character
parseInput InsertMode [] (Curses.EventCharacter c)
    | isPrint c = Right $ matchActions [CmdInsertChar c]
    | otherwise = Left []

-- Replace character
parseInput ReplaceMode [] (Curses.EventCharacter c)
    | isPrint c = Right $ matchActions [CmdReplaceCharAndMove c]
    | otherwise = Left []

-- Join lines
parseInput CommandMode [] (Curses.EventCharacter 'J') = Right $ matchActions [CmdJoinLine]
parseInput CommandMode [CmdAmount n] (Curses.EventCharacter 'J') = Right $ matchActions [CmdAmount n, CmdJoinLine]

-- Undo
parseInput CommandMode [CmdAmount n] (Curses.EventCharacter 'u') = Right $ matchActions [CmdAmount n, CmdUndo]
parseInput CommandMode [] (Curses.EventCharacter 'u') = Right $ matchActions [CmdUndo]

-- Redo
parseInput CommandMode [CmdAmount n] (Curses.EventCharacter '\DC2') = Right $ matchActions [CmdAmount n, CmdRedo]
parseInput CommandMode [] (Curses.EventCharacter '\DC2') = Right $ matchActions [CmdRedo]

-- Goto line
parseInput CommandMode [] (Curses.EventCharacter 'G') = Right $ matchActions [CmdGotoLastLine]
parseInput CommandMode [] (Curses.EventCharacter 'g') = Left [CmdGotoLine]
parseInput CommandMode [CmdGotoLine] (Curses.EventCharacter 'g') = Right $ matchActions [CmdGotoFirstLine]
parseInput CommandMode [CmdAmount n] (Curses.EventCharacter 'g') = Left [CmdAmount n, CmdGotoLine]
parseInput CommandMode [CmdAmount n, CmdGotoLine] (Curses.EventCharacter 'g') = Right $ matchActions [CmdAmount n, CmdGotoLine]

parseInput CommandMode [] (Curses.EventCharacter 'w') = Right $ matchActions [CmdNextWord]
parseInput CommandMode [CmdAmount n] (Curses.EventCharacter 'w') = Right $ matchActions [CmdAmount n, CmdNextWord]

parseInput CommandMode [] (Curses.EventCharacter 'b') = Right $ matchActions [CmdPrevWord]
parseInput CommandMode [CmdAmount n] (Curses.EventCharacter 'b') = Right $ matchActions [CmdAmount n, CmdPrevWord]

parseInput CommandMode [] (Curses.EventCharacter 'B') = Right $ matchActions [CmdPrevWord]
parseInput CommandMode [CmdAmount n] (Curses.EventCharacter 'B') = Right $ matchActions [CmdAmount n, CmdPrevWord]

parseInput CommandMode [] (Curses.EventCharacter 'e') = Right $ matchActions [CmdNextWordEnding]
parseInput CommandMode [CmdAmount n] (Curses.EventCharacter 'e') = Right $ matchActions [CmdAmount n, CmdNextWordEnding]

parseInput CommandMode [] (Curses.EventCharacter 'E') = Right $ matchActions [CmdNextWordEnding]
parseInput CommandMode [CmdAmount n] (Curses.EventCharacter 'E') = Right $ matchActions [CmdAmount n, CmdNextWordEnding]

-- Paste
parseInput CommandMode [CmdAmount n] (Curses.EventCharacter 'p') = Right $ matchActions [CmdAmount n, CmdPasteAfter]
parseInput CommandMode [] (Curses.EventCharacter 'p') = Right $ matchActions [CmdPasteAfter]

parseInput CommandMode [CmdAmount n] (Curses.EventCharacter 'P') = Right $ matchActions [CmdAmount n, CmdPasteBefore]
parseInput CommandMode [] (Curses.EventCharacter 'P') = Right $ matchActions [CmdPasteBefore]

-- Repeat last modification (dot command)
parseInput CommandMode [CmdAmount n] (Curses.EventCharacter '.') = Right $ matchActions [CmdAmount n, CmdRepeat]
parseInput CommandMode [] (Curses.EventCharacter '.') = Right $ matchActions [CmdRepeat]

-- Replace Mode
parseInput CommandMode [] (Curses.EventCharacter 'R') = Right $ matchActions [CmdReplaceMode]

-- Delete sections
parseInput CommandMode [CmdAmount n] (Curses.EventCharacter 'd') = Left [CmdDelete n]
parseInput CommandMode [] (Curses.EventCharacter 'd') = Left [CmdDelete 1]
parseInput CommandMode [CmdDelete n] (Curses.EventCharacter 'd') = Right $ matchActions [CmdDeleteLine n]
parseInput CommandMode coms@((CmdDelete n):_) (eChar) = parseMotions coms (ActDelete n) eChar

-- Quit on ctrl-q
parseInput _ _ (Curses.EventCharacter '\DC1') = Right [ActQuit]
parseInput _ _ _ = Left []

-- |Tries to decide whether the tail of a given list of commands resembles a
-- motion. If it does it gives you the full action, the given partial action
-- completed with the motion actions. If it does not it fails with an
-- ActErrorMessage.
-- This function can be used with commands that can operate on motions like
-- CmdDelete, or CmdIndent.
parseMotions :: [Command] -> ([Action] -> Action) -> Curses.Event -> Either [Command] [Action]
parseMotions [] _ _ = Left []
parseMotions (command:xs) action eChar =
    case (isMotion possibleMotion) of
        NoMotion      -> Right $ [ActErrorMessage "Invalid motion"]
        CouldBeMotion -> Left $ (command:motionCommands)
        Motion        -> Right $ [ActFlagUndoPoint, action motionActions]
    where
        possibleMotion = parseInput CommandMode xs eChar
        motionActions = actions possibleMotion
        motionCommands = commands possibleMotion

-- Map a list of commands to a list of actions
matchActions :: [Command] -> [Action]
matchActions [CmdQuit] = [ActQuit]
matchActions [CmdAmount n, CmdDown] = [ActCursorDown n]
matchActions [CmdDown] = [ActCursorDown 1]

matchActions [CmdAmount n, CmdUp] = [ActCursorUp n]
matchActions [CmdUp] = [ActCursorUp 1]

matchActions [CmdAmount n, CmdLeft] = [ActCursorLeft n]
matchActions [CmdLeft] = [ActCursorLeft 1]

matchActions [CmdAmount n, CmdRight] = [ActCursorRight n]
matchActions [CmdRight] = [ActCursorRight 1]

matchActions [CmdAmount n, CmdEndOfLine] = [ActCursorDown (n - 1), ActEndOfLine]
matchActions [CmdEndOfLine] = [ActEndOfLine]

matchActions [CmdBeginningOfLine] = [ActBeginningOfLine]
matchActions [CmdFirstNoneWhiteSpace] = [ActFirstNoneWhiteSpace]

matchActions [CmdAmount n, CmdPageDown] = [ActPageDown n]
matchActions [CmdPageDown] = [ActPageDown 1]

matchActions [CmdAmount n, CmdPageUp] = [ActPageUp n]
matchActions [CmdPageUp] = [ActPageUp 1]
matchActions [CmdInsertMode] = [ActFlagUndoPoint, ActInsertMode]
matchActions [CmdInsertModeBefore] = [ActFirstNoneWhiteSpace, ActInsertMode]
matchActions [CmdReplaceMode] = [ActFlagUndoPoint, ActReplaceMode]
matchActions [CmdCommandMode] = [ActCommandMode]
matchActions [CmdInsertChar c] = [ActInsertChar c]
matchActions [CmdBackspace] = [ActDeleteCharBefore 1]
matchActions [CmdAmount n, CmdDeleteChar] = [ActFlagUndoPoint, ActDeleteChar n]
matchActions [CmdDeleteChar] = [ActFlagUndoPoint, ActDeleteChar 1]
matchActions [CmdAmount n, CmdDeleteCharBefore] = [ActFlagUndoPoint, ActDeleteCharBefore n]
matchActions [CmdDeleteCharBefore] = [ActFlagUndoPoint, ActDeleteCharBefore 1]
matchActions [CmdDeleteLine n] = [ActFlagUndoPoint, ActDeleteLine n]
matchActions [CmdInsertNewLine] = [ActInsertNewLine, ActCursorDown 1, ActFirstNoneWhiteSpace]
matchActions [CmdAmount _, CmdOpenLine] = matchActions [CmdOpenLine]
matchActions [CmdOpenLine] = [ActFlagUndoPoint, ActEndOfLine, ActAdvanceCursor, ActInsertNewLine, ActCursorDown 1, ActInsertMode]
matchActions [CmdAmount _, CmdOpenLineBefore] = matchActions [CmdOpenLineBefore]
matchActions [CmdOpenLineBefore] = [ActBeginningOfLine, ActInsertNewLine, ActInsertMode]
matchActions [CmdAmount n, CmdUndo] = [ActUndo n]
matchActions [CmdUndo] = [ActUndo 1]
matchActions [CmdAmount 1, CmdJoinLine] = [ActFlagUndoPoint, ActJoinLine 2]
matchActions [CmdAmount n, CmdJoinLine] = [ActFlagUndoPoint, ActJoinLine n]
matchActions [CmdJoinLine] = [ActFlagUndoPoint, ActJoinLine 2]
matchActions [CmdAmount n, CmdFindForward, CmdChar c] = [ActFindForward n c]
matchActions [CmdFindForward, CmdChar c] = [ActFindForward 1 c]
matchActions [CmdAmount n, CmdFindBackward, CmdChar c] = [ActFindBackward n c]
matchActions [CmdFindBackward, CmdChar c] = [ActFindBackward 1 c]

matchActions [CmdAmount n, CmdRedo] = [ActRedo n]
matchActions [CmdRedo] = [ActRedo 1]
matchActions [CmdRedrawScreen] = [ActRedrawScreen]
matchActions [CmdGotoLastLine] = [ActGotoLastLine, ActFirstNoneWhiteSpace]
matchActions [CmdAmount n, CmdGotoLine] = [ActGotoLine n, ActFirstNoneWhiteSpace]
matchActions [CmdGotoFirstLine] = [ActGotoLine 0, ActFirstNoneWhiteSpace]
matchActions [CmdAmount n, CmdNextWord] = [ActNextWord n]
matchActions [CmdNextWord] = [ActNextWord 1]
matchActions [CmdAmount n, CmdPrevWord] = [ActPrevWord n]
matchActions [CmdPrevWord] = [ActPrevWord 1]
matchActions [CmdAmount n, CmdNextWordEnding] = [ActNextWordEnding n]
matchActions [CmdNextWordEnding] = [ActNextWordEnding 1]

matchActions [CmdPasteAfter] = [ActFlagUndoPoint, ActPasteAfter 1]
matchActions [CmdAmount n, CmdPasteAfter] = [ActPasteAfter n]
matchActions [CmdPasteBefore] = [ActFlagUndoPoint, ActPasteBefore 1]
matchActions [CmdAmount n, CmdPasteBefore] = [ActPasteBefore n]

matchActions [CmdReplaceChar n, CmdChar c] = [ActFlagUndoPoint, ActReplaceChar n c]
matchActions [CmdReplaceCharAndMove c] = [ActReplaceCharAndMove c]

matchActions [CmdAmount n, CmdRepeat] = [ActRepeat n]
matchActions [CmdRepeat] = [ActRepeat 1]

matchActions [CmdAppend] = [ActFlagUndoPoint, ActAdvanceCursor, ActInsertMode]
matchActions _ = [ActIdle]

-- |Check whether the input resembles (a part of) a motion. This is used when
-- pairing deletes, yanks or changes with motions
isMotion :: Either [Command] [Action] -> PossibleMotion
isMotion (Left _) = CouldBeMotion
isMotion (Right [ActBeginningOfLine])     = Motion
isMotion (Right [ActCursorDown _])        = Motion
isMotion (Right ((ActCursorDown _):_))    = Motion
isMotion (Right [ActCursorLeft _])        = Motion
isMotion (Right [ActCursorRight _])       = Motion
isMotion (Right [ActCursorUp _])          = Motion
isMotion (Right (ActEndOfLine:_))         = Motion
isMotion (Right [ActFirstNoneWhiteSpace]) = Motion
isMotion (Right [ActFindForward _ _])     = Motion
isMotion (Right [ActFindBackward _ _])    = Motion
isMotion (Right [ActNextWord _])          = Motion
isMotion (Right [ActPrevWord _])          = Motion
isMotion (Right [ActNextWordEnding _])    = Motion
isMotion (Right (ActGotoLine _:_))        = Motion
isMotion (Right (ActGotoLastLine:_))      = Motion
isMotion _ = NoMotion

actions :: Either [Command] [Action] -> [Action]
actions (Left _) = []
actions (Right xs) = xs

commands :: Either [Command] [Action] -> [Command]
commands (Right _) = []
commands (Left xs) = xs
