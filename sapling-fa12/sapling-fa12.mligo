type storage = {
    state: 8 sapling_state;
    fa1_2_contract: address;
}

type parameter = 8 sapling_transaction list

type fa1_2_parameter = 
[@layout:comb]
{
    from: address;
    [@annot to] to_: address;
    value: nat;
}

type return = operation list * storage

let transfer 
    (from: address) 
    (recipient: address) 
    (amount: int): operation option =
        match ((Tezos.get_entrypoint_opt "%transfer" from): fa1_2_parameter contract option) with
        | None -> (None: operation option)
        | Some contract -> 
            let param = 
                { from = from; to_ = recipient; value = (abs amount) }
            in 
            Some (Tezos.transaction param 0tez contract)

let main (tx_list, s : parameter * storage) : return =
    // contract must fail if an amount of tez is transferred
    if (Tezos.get_amount () > 0tez)
    then failwith "UNEXPECTED_XTZ_AMOUNT"
    else
        let (ops, new_state) = 
            List.fold
                (
                    fun (((ops, storage), tx) : (operation list * storage) * 8 sapling_transaction) -> 
                        match Tezos.sapling_verify_update tx storage.state with
                        | None -> failwith "INVALID_SAPLING_TX"
                        | Some (bound_data, (tx_balance, new_sapling_state)) -> (
                            // If the balance is strictly positive (i.e. unshielding), we send
                            // funds to the given address.
                            if tx_balance > 0
                            then
                                (
                                    match (Bytes.unpack bound_data: key_hash option) with
                                    | None -> failwith "UNABLE_TO_UNPACK_RECIPIENT"
                                    | Some (recipient_key_hash) ->
                                        let recipient = 
                                            recipient_key_hash 
                                            |> Tezos.implicit_account
                                            |> Tezos.address
                                        in
                                        (match transfer storage.fa1_2_contract recipient tx_balance with
                                        | None -> ops, { storage with state = new_sapling_state }
                                        | Some op -> (op :: ops), { storage with state = new_sapling_state })
                                        
                                ) 
                            else
                                // no implicit account is expected in the bound data
                                (
                                    match (Bytes.unpack bound_data: key_hash option) with
                                    | Some (_) -> failwith "UNEXPECTED_RECIPIENT"
                                    | None -> 
                                        if tx_balance < 0
                                        then
                                            // If the balance is negative, the contract receives the tokens (shielding)
                                            (match transfer (Tezos.get_sender ()) storage.fa1_2_contract tx_balance with
                                            | None -> ops, { storage with state = new_sapling_state }
                                            | Some op -> (op :: ops), { storage with state = new_sapling_state })
                                        else
                                            // If the balance is zero (Sapling transfer)
                                            ops, { storage with state = new_sapling_state }
                                )
                        )
                )
                tx_list
                (([]: operation list), s)
        in ops, new_state