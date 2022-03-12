type prize =
  | Car
  | Lose of int
  | House
  | Education
  | Nate

module Person = struct
  let prizes = []
  let money = 0
  let familial_status = "lonely"
  let name = "Alpine"
end

let current_prizes = Person.prizes
let current_money = Person.money
let current_family = Person.familial_status
let current_name = Person.name
