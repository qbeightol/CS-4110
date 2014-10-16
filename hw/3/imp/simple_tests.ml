

evalc (make_configuration (Skip))

evalc (make_configuration (Assign ("x", Int 5)))

evalc (make_configuration (Seq (Break, Assign ("x", Int 5))))

evalc (make_configuration (While (True, Seq (Break, Assign ("x", Int 5)))))


let test_01 = 
  Seq (Assign ("x", Int 1), 
  Seq (Assign ("y", Int 2), 
  Seq (While (True, 
          Seq (Assign ("x", Plus (Var "x", Int 1)),
          Seq (If (Less (Int 4, Var "x"), Break, Skip),
          Seq (Continue,
               Assign ("y", Plus (Var "y", Int 1)))))),
      Assign ("x", Times (Var "x", Int 2)))))