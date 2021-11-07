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

    type Statement =
        | Nothing
        | Parameter of Name * Value
        | Command of Path * Arguments

    type Program = Program of Statement list
