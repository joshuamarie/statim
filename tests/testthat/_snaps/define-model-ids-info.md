# print.model_id snapshot for x_by

    Code
      print(x_by(extra, group))
    Output
      -- Model Definition ------------------------------------------------------------ 
      
      Model ID : x_by 
      Args : extra | group 

# print.model_id snapshot for rel

    Code
      print(rel(speed, dist))
    Output
      -- Model Definition ------------------------------------------------------------ 
      
      Model ID : rel 
      Args : speed ; dist 

# print.model_id snapshot for pairwise

    Code
      print(pairwise(a, b, c))
    Output
      -- Model Definition ------------------------------------------------------------ 
      
      Model ID : pairwise 
      Args : a, b, c 

# print.def_model snapshot for x_by

    Code
      define_model(x_by(extra, group), sleep)
    Output
      
      -- Model Definition ------------------------------------------------------------ 
      
      Model ID : x_by 
      Args : extra | group 
      Other info:
          x_vars : 1 
          by_vars : 1 
      Variables :
          extra : <dbl [20]> 
          group : <fct [20]> 
      

# print.def_model snapshot for formula

    Code
      define_model(extra ~ group, sleep)
    Output
      
      -- Model Definition ------------------------------------------------------------ 
      
      Model ID : formula 
      Args : extra ~ group 
      Other info:
          left_var : 1 
          right_var : 1 
      Variables :
          extra : <dbl [20]> 
          group : <fct [20]> 
      

# print.def_model snapshot for pairwise

    Code
      define_model(pairwise(extra, ID), sleep)
    Output
      
      -- Model Definition ------------------------------------------------------------ 
      
      Model ID : pairwise 
      Args : extra, ID 
      Other info:
          direction : lt 
          n_pairs : 1 
      Variables :
          extra : <dbl [20]> 
          ID : <fct [20]> 
      

# print.test_lazy snapshot — default method

    Code
      prepare_test(define_model(sleep, x_by(extra, group)), TTEST)
    Output
      
      -- Model Definition ------------------------------------------------------------ 
      
      Model ID : x_by 
      Args : extra | group 
      
      -- Test Specification ---------------------------------------------------------- 
      
      Test : T-Test 
      Method : default
      

# print.test_lazy snapshot — via boot

    Code
      via(prepare_test(define_model(sleep, x_by(extra, group)), TTEST), "boot", n = 2000L)
    Output
      
      -- Model Definition ------------------------------------------------------------ 
      
      Model ID : x_by 
      Args : extra | group 
      
      -- Test Specification ---------------------------------------------------------- 
      
      Test : T-Test 
      Method : boot (n = 2000)
      

