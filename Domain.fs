namespace SBash

module Domain =

    type Path = Path of string
    type Arguments = Arguments of string

    type Name = Name of string
    type LiteralEqual = char
    type Value = Value of string

    type ControlOperator =
        | Newline of char
        | Semicolon of char

    type Parameter = Parameter of Name * Value
    type Command = Command of Path * Arguments

    type Statement =
        | ParamStatement of Parameter
        | CommandStatement of Command

    type Program = Program of Statement list
