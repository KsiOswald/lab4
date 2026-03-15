open System

type BiTree =
    Node of int * BiTree * BiTree
    | Nil

let rec insert currentTree value =
    match currentTree with
    | Nil -> 
        Node(value,Nil,Nil)
    | Node(currentValue, leftNode, rightNode) -> 
        if value < currentValue then
            Node(currentValue, insert leftNode value, rightNode)
        else
            Node(currentValue,leftNode ,insert rightNode value)


let infix root left right = (right(); root(); left())

let travWithDepth strategy action tree =
    let rec walk curTree depth =
        match curTree with
        | Node (value, left, right) -> 
            strategy (fun () -> action value depth)
                     (fun () -> walk left (depth + 1))
                     (fun () -> walk right (depth + 1))
        | Nil -> ()

    walk tree 0

let spaces n = String.replicate n "  "

let printTree T = 
    travWithDepth infix (fun x dep -> 
        printfn "%s%A" (spaces dep) x) T


let rec treeFold currentTree = [
    match currentTree with
    | Nil -> ()
    | Node (num, left, right) -> 
        if (abs(num) % 2 = 0) then
            yield num
            yield! treeFold left 
            yield! treeFold right
        else
            yield! treeFold left
            yield! treeFold right
            
    ]


[<EntryPoint>]
let main args =
    let randList = [
        let rand = new Random()
        let countElement = rand.Next(10, 15)
        for _ in 1..countElement do
            let randNum = rand.Next(-500, 500)
            yield randNum
        ]
    printfn "Список случайных строк: %A" randList
    let tree = List.fold insert Nil randList
    printfn "Исходное дерево: "
    printTree tree
    let resultList = treeFold tree
    
    printf "Список четных элементов: %A" resultList 
    0