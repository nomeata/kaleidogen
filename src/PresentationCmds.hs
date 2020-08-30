module PresentationCmds (Cmds, Cmd, Cmd'(..)) where

data Cmd' a
    = SummonAt a
    | MoveTo a
    | FadeOut a
    | Remove
    | Animate
type Cmd k a = (k, Cmd' a)
type Cmds k a = [Cmd k a]
