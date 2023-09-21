
let analyze_deal deal =
    Printf.printf "\n";
    print_deal deal;
    List.iter (fun depth ->
      (if depth <= 8 then
        let (value, variation), node_count = evaluate_deal_alpha (ref 0) deal depth
        in Printf.printf "alpha depth %d: value %d, nodes %d\n%!" depth value node_count);
        let (value, variation), node_count = evaluate_deal_gamma_top (ref 0) deal depth
        in Printf.printf "gamma depth %d: value %d, nodes %d\n%!" depth value node_count)
        [4; 8; 12; 16; 20]

let _ =
    for i = 1 to 10 do
        let d = new_deal ()
        in analyze_deal d
    done

