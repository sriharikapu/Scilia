contract Crowdfunding
 (owner     : address,
  max_block : uint,
  goal      : uint)
(* Mutable state description *)
{
  backers : address => uint = [];
  funded  : boolean = false;
}
(* Transition 1: Donating money *)
transition Donate
  (sender : address, value : uint, tag : string)
  (* Simple filter identifying this transition *)
  if tag == "donate" =>
  bs <- & backers;
  blk <- && block_number;
  let nxt_block = blk + 1 in
  if max_block <= nxt_block
  then send (<to -> sender, amount -> 0,
	      tag -> main,
	      msg -> "deadline_passed">, MT)
  else
    if not (contains(bs, sender))
    then let bs1 = put(sbs, ender, value) in
         backers := bs1;
         send (<to -> sender,
                amount -> 0,
	        tag -> "main",
	        msg -> "ok">, MT)
    else send (<to -> sender,
                amount -> 0,
	        tag -> "main",
	        msg -> "already_donated">, MT)
(* Transition 2: Sending the funds to the owner *)
transition GetFunds
  (sender : address, value : uint, tag : string)
  (* Only the owner can get the money back *)
  if (tag == "getfunds") && (sender == owner) =>
  blk <- && block_number;
  bal <- & balance;
  if max_block >= blk
  then if goal <= bal
       then funded := true;
            send (<to -> owner, amount -> bal,
                   tag -> "main", msg -> "funded">, MT)
       else send (<to -> owner, amount -> 0,
                   tag -> "main", msg -> "failed">, MT)
  else send (<to -> owner, amount -> 0, tag -> "main",
   	      msg -> "too_early_to_claim_funds">, MT)
(* Transition 3: Reclaim funds by a backer *)
transition Claim
  (sender : address, value : uint, tag : string)
  if tag == "claim" =>
  blk <- && block_number;
  if blk <= max_block
  then send (<to -> sender, amount -> 0, tag -> "main",
              msg -> "too_early_to_reclaim">, MT)
  else bs <- & backers;
       bal <- & balance;
       if (not (contains(bs, sender))) || funded ||
          goal <= bal
       then send (<to -> sender, amount -> 0,
                   tag -> "main",
	           msg -> "cannot_refund">, MT)
       else
       let v = get(bs, sender) in
       backers := remove(bs, sender);
       send (<to -> sender, amount -> v, tag -> "main",
              msg -> "here_is_your_money">, MT)
