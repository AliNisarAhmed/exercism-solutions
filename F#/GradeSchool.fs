module GradeSchool

type School = Map<int, string list>

let empty : School = Map.empty

let addToOptionalList student =
    function
    | None -> Some [ student ]
    | Some list -> Some <| student :: list

let add (student: string) (grade: int) (school: School) : School =
    Map.change grade (addToOptionalList student) school


let roster (school: School) : string list =
    school
    |> Map.toList
    |> List.collect (snd >> List.sort)

let grade (number: int) (school: School) : string list =
    if Map.containsKey number school then
        Map.find number school
    else
        []
