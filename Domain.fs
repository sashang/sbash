namespace SBash

module Domain =

    type Path = string
    type Arguments = string list

    type Name = string
    type LiteralEqual = char
    type Value = string

    type ControlOperator =
        | Newline of char
        | Semicolon of char

    type Statement =
        | Nothing
        | Parameter of Name * Value
        | Command of Path * Arguments

    type Program = Program of Statement list
