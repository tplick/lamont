
let analyze_deal deal counter idx =
    print_deal deal;
    List.iter (fun depth ->
      (if depth <= 0 then
        let (value, variation), node_count = evaluate_deal_alpha (ref 0) deal depth
        in Printf.printf "alpha depth %d: value %d, nodes %d\n%!" depth value node_count;
           counter := !counter + node_count))
        [4; 8];
    let (value, variation), node_count = evaluate_deal_gamma_top (ref 0) deal 52 idx
    in (* Printf.printf "gamma depth %d: value %d, nodes %d\n%!" 52 value node_count; *)
       counter := !counter + node_count

let _ =
    let counter = ref 0 in
    (* for i = 1 to 20 do *)
    for i = 1 to 20 do
        Printf.printf "#%d\n" i;
        let d = new_deal ()
        in analyze_deal d counter i;
        Printf.printf "\n"
    done;
    Printf.printf "Saw %d nodes.\n" !counter

