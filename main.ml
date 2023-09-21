
let analyze_deal deal =
    Printf.printf "\n";
    print_deal deal;
    List.iter (fun depth ->
        let (value, variation), node_count = evaluate_deal_alpha (ref 0) deal depth
        in Printf.printf "alpha depth %d: value %d, nodes %d\n" depth value node_count;
        let (value, variation), node_count = evaluate_deal_beta (ref 0) deal depth
        in Printf.printf " beta depth %d: value %d, nodes %d\n" depth value node_count)
        [4; 8; 12]

let _ =
    for i = 1 to 10 do
        let d = new_deal ()
        in analyze_deal d
    done

