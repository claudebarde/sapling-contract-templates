
type storage = 8 sapling_state

type parameter = 8 sapling_transaction list

type return = operation list * storage

let main (tx_list, s : parameter * storage) : return =
    let (ops, new_state, difference) = 
        List.fold
            (
                fun (((ops, prev_state, budget), tx) : (operation list * storage * tez) * 8 sapling_transaction) -> 
                    match Tezos.sapling_verify_update tx prev_state with
                    | None -> failwith "INVALID_SAPLING_TX"
                    | Some (bound_data, (tx_balance, new_sapling_state)) -> (
                        let tx_balance_in_tez = 1mutez * abs tx_balance in
                        if tx_balance > 0
                        then
                            // transaction balance greater than zero = sends XTZ
                            (
                                match (Bytes.unpack bound_data: key_hash option) with
                                | None -> failwith "UNABLE_TO_UNPACK_RECIPIENT"
                                | Some (recipient_key_hash) ->
                                    let recipient = Tezos.implicit_account recipient_key_hash in
                                    let op = Tezos.transaction unit tx_balance_in_tez recipient in
                                    (op :: ops), new_sapling_state, budget
                            )            
                        else
                            // transaction balance less or equal to zero
                            (
                                match (budget - tx_balance_in_tez) with
                                | None -> failwith "INVALID_AMOUNT"
                                | Some (diff) -> (
                                    // checks the size of the bound data
                                    if Bytes.length bound_data <> 0n
                                    then failwith "UNEXPECTED_EMPTY_BOUND_DATA"
                                    else 
                                        ops, new_sapling_state, diff
                                )
                            )
                    )
            )
            tx_list
            (([]: operation list), s, Tezos.get_amount ())
    in
    if (difference <> 0mutez)
    then failwith "UNEXPECTED_REMAINDER"
    else ops, new_state