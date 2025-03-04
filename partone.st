Object subclass: MyFunctions [
    MyFunctions class >> chunkIt: n list: xs [
        | result |
        result := OrderedCollection new.
        1 to: xs size by: n do: [:i |
            result add: (xs copyFrom: i to: (i + n - 1 min: xs size)).
        ].
        ^ result asArray.
    ]

    MyFunctions class >> firstLoc: target list: xs [
        | index |
        index := 1.
        xs do: [:x |
            (x = target) ifTrue: [^ index].
            index := index + 1.
        ].
        ^ -1.
    ]
]

" Testing the functions with .printString to convert output to string "
Transcript show: (MyFunctions chunkIt: 2 list: #(1 2 3 4 5)) printString; cr.
Transcript show: (MyFunctions firstLoc: 3 list: #(1 2 3 4 3)) printString; cr.
