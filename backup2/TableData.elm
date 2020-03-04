module TableData exposing
    ( TableData
    , TableTitle
    )


type alias TableTitle =
    { title : String
    }


type alias TableData =
    { columns : List TableTitle
    , data : List (List String)
    , command : String
    }
