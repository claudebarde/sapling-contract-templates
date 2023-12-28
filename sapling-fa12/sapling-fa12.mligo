type storage = {
    state: 8 sapling_state;
    fa1_2_contract: address;
}

type parameter = 8 sapling_transaction list

type fa1_2_parameter = 
{
    from: address;
    [@annot to] to_: address;
    value: nat;
}

type return = operation list * storage

[@entry]
let main (tx_list: parameter) (s: storage) : return =
    // contract must fail if an amount of tez is transferred
    if (Tezos.get_amount () > 0tez)
    then failwith "UNEXPECTED_XTZ_AMOUNT"
    else
        match ((Tezos.get_entrypoint_opt "%transfer" s.fa1_2_contract): fa1_2_parameter contract option) with
        | None -> failwith "INVALID_FA1_2_CONTRACT"
        | Some contract -> 
            let (ops, new_state) = 
                List.fold
                    (
                        fun (((ops, new_state), tx) : (operation list * 8 sapling_state) * 8 sapling_transaction) -> 
                            match Tezos.sapling_verify_update tx new_state with
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
                                            let param = 
                                            { 
                                                from = Tezos.get_self_address (); 
                                                to_ = recipient; 
                                                value = (abs tx_balance) 
                                            }
                                            in 
                                            ((Tezos.transaction param 0tez contract) :: ops), 
                                            new_sapling_state
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
                                                let param = 
                                                { 
                                                    from = Tezos.get_sender (); 
                                                    to_ = Tezos.get_self_address (); 
                                                    value = (abs tx_balance) 
                                                }
                                                in 
                                                ((Tezos.transaction param 0tez contract) :: ops), 
                                                new_sapling_state
                                            else
                                                // If the balance is zero (Sapling transfer)
                                                ops, new_sapling_state
                                    )
                            )
                    )
                    tx_list
                    (([]: operation list), s.state)
            in (ops, { s with state = new_state })