type storage = {
    state: 8 sapling_state;
    fa2_contract: address;
    token_id: nat;
}

type parameter = 8 sapling_transaction list

type fa2_transaction =
[@layout:comb]
{
    to_: address;
    token_id: nat;
    amount: nat;
}
type fa2_transfer = 
[@layout:comb]
{
    from_: address;
    txs: fa2_transaction list;
}
type fa2_parameter = fa2_transfer list

type return = operation list * storage

type loop_els = ((address * nat) list * nat * storage) * 8 sapling_transaction

[@entry]
let main (tx_list: parameter) (s: storage) : return =
    // contract must fail if an amount of tez is transferred
    if (Tezos.get_amount () > 0tez)
    then failwith "UNEXPECTED_XTZ_AMOUNT"
    else
        let (unshielding_reqs, shielding_amount, new_storage) = 
            List.fold
                (
                    fun (((unshielding_reqs, shielding_amount, storage), tx) : loop_els) -> 
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
                                        let data = (recipient, abs(tx_balance)) in
                                        (
                                            data :: unshielding_reqs,
                                            shielding_amount,
                                            { storage with state = new_sapling_state }
                                        )                                        
                                )            
                            else if tx_balance < 0
                            then
                                // If the balance is negative, the contract receives the tokens (shielding)
                                (
                                    match (Bytes.unpack bound_data: key_hash option) with
                                    | Some (_) -> failwith "UNEXPECTED_RECIPIENT"
                                    | None -> 
                                        (
                                            unshielding_reqs,
                                            (shielding_amount + abs(tx_balance)),
                                            { storage with state = new_sapling_state }
                                        )
                                )
                            else
                                // If the balance is zero (Sapling transfer)
                                (
                                    unshielding_reqs,
                                    shielding_amount,
                                    { storage with state = new_sapling_state }
                                )

                        )
                )
                tx_list
                (([]: (address * nat) list), 0n, s)
        in
        // creates the transfers
        let op: operation = 
            match ((Tezos.get_entrypoint_opt "%transfer" s.fa2_contract): fa2_parameter contract option) with
            | None -> failwith "%TRANFER_DOESNT_EXIST"
            | Some contract -> (
                // transfers to the contract
                let transfers_to: fa2_parameter =
                    [{
                        from_ = Tezos.get_sender () ;
                        txs = [{
                            to_ = Tezos.get_self_address () ;
                            token_id = s.token_id ;
                            amount = shielding_amount
                        }]
                    }]
                in
                // transfers from the contract
                let transfers_from: fa2_transaction list = 
                    List.map
                        (
                            fun (req: (address * nat)) ->
                                {
                                    to_ = req.0 ;
                                    amount = req.1 ;
                                    token_id = s.token_id ;
                                }
                        )
                        unshielding_reqs
                in 
                let transfers: fa2_parameter = 
                    { from_ = Tezos.get_self_address () ; txs = transfers_from } :: transfers_to
                in
                // forges the transaction
                Tezos.transaction transfers 0tez contract
            )
        in
        
        [op], new_storage